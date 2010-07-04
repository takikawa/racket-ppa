#lang scheme/unit


(require (rename-in "../utils/utils.ss" [infer r:infer]))
(require syntax/kerncase
	 unstable/list
         mzlib/etc
         scheme/match
         "signatures.ss"
         "tc-structs.ss"
         (rep type-rep)
         (types utils convenience)
         (private parse-type type-annotation type-contract)
         (env type-env init-envs type-name-env type-alias-env lexical-env)
	 unstable/mutated-vars
         (utils tc-utils)
         "provide-handling.ss"
         "def-binding.ss"
         (for-template
          "internal-forms.ss"
          mzlib/contract
          scheme/base))

(import tc-expr^ check-subforms^)
(export typechecker^)

(define (tc-toplevel/pass1 form)
  ;(printf "form-top: ~a~n" form)
  ;; first, find the mutated variables:
  (find-mutated-vars form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                   define-typed-struct/exec-internal :-internal assert-predicate-internal
                                   require/typed-internal values)
      ;; forms that are handled in other ways
      [stx 
       (or (syntax-property form 'typechecker:ignore) 
           (syntax-property form 'typechecker:ignore-some))
       (list)]
      
      ;; type aliases have already been handled by an earlier pass
      [(define-values () (begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values)))
       (list)]

      ;; declare-refinement
      ;; FIXME - this sucks and should die
      [(define-values () (begin (quote-syntax (declare-refinement-internal pred)) (#%plain-app values)))
       (match (lookup-type/lexical #'pred)
              [(and t (Function: (list (arr: (list dom) (Values: (list (Result: rng _ _))) #f #f '()))))
               (let ([new-t (make-pred-ty (list dom)
                                          rng
                                          (make-Refinement dom #'pred (syntax-local-certifier)))])
                 (register-type #'pred new-t))
               (list)]
              [t (tc-error "cannot declare refinement for non-predicate ~a" t)])]
      
      ;; require/typed
      [(define-values () (begin (quote-syntax (require/typed-internal nm ty)) (#%plain-app values)))
       (let ([t (parse-type #'ty)])
         (register-type #'nm t)
         (list (make-def-binding #'nm t)))]
      
      [(define-values () (begin (quote-syntax (require/typed-internal nm ty #:struct-maker parent)) (#%plain-app values)))
       (let* ([t (parse-type #'ty)]
              [flds (Struct-flds (lookup-type-name (Name-id t)))]
              [mk-ty (flds #f . ->* . t)])
         (register-type #'nm mk-ty)
         (list (make-def-binding #'nm mk-ty)))]
      
      ;; define-typed-struct
      [(define-values () (begin (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...))) (#%plain-app values)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]
      [(define-values () (begin (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...) #:mutable)) (#%plain-app values)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #:mutable #t)]
      [(define-values () (begin (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...) #:maker m #:constructor-return t)) 
                                (#%plain-app values)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #:maker #'m #:constructor-return #'t)]
      [(define-values () (begin (quote-syntax (define-typed-struct-internal nm ([fld : ty] ...) #:type-only))
                                (#%plain-app values)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #:type-only #t)]
      ;; define-typed-struct w/ polymorphism
      [(define-values () (begin (quote-syntax (define-typed-struct-internal (vars ...) nm ([fld : ty] ...))) (#%plain-app values)))
       (tc/poly-struct (syntax->list #'(vars ...)) #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)))]    
      
      ;; executable structs - this is a big hack
      [(define-values () (begin (quote-syntax (define-typed-struct/exec-internal nm ([fld : ty] ...) proc-ty)) (#%plain-app values)))
       (tc/struct #'nm (syntax->list #'(fld ...)) (syntax->list #'(ty ...)) #'proc-ty)]
      
      ;; predicate assertion - needed for define-type b/c or doesn't work
      [(define-values () (begin (quote-syntax (assert-predicate-internal ty pred)) (#%plain-app values)))
       (register-type #'pred (make-pred-ty (parse-type #'ty)))]
      
      ;; top-level type annotation
      [(define-values () (begin (quote-syntax (:-internal id ty)) (#%plain-app values)))
       (identifier? #'id)
       (register-type/undefined #'id (parse-type #'ty))]
            
      
      ;; values definitions
      [(define-values (var ...) expr)
       (let* ([vars (syntax->list #'(var ...))])
         (cond
           ;; if all the variables have types, we stick them into the environment
           [(andmap (lambda (s) (syntax-property s 'type-label)) vars)        
            (let ([ts (map get-type vars)])
              (for-each register-type vars ts)
              (map make-def-binding vars ts))]
           ;; if this already had an annotation, we just construct the binding reps
           [(andmap (lambda (s) (lookup-type s (lambda () #f))) vars)
            (for-each finish-register-type vars)
            (map (lambda (s) (make-def-binding s (lookup-type s))) vars)]
           ;; special case to infer types for top level defines - should handle the multiple values case here
           [(and (= 1 (length vars)) 
                 (with-handlers ([exn:fail? (lambda _ #f)])
                   (save-errors!)
                   (begin0 (tc-expr #'expr)
                           (restore-errors!))))
            => (match-lambda 
                 [(tc-result1: t)
                  (register-type (car vars) t)
                  (list (make-def-binding (car vars) t))]
                 [t (int-err "~a is not a tc-result" t)])]
           [else
            (tc-error "Untyped definition : ~a" (map syntax-e vars))]))]
      
      ;; to handle the top-level, we have to recur into begins
      [(begin . rest)
       (apply append (filter list? (map tc-toplevel/pass1 (syntax->list #'rest))))]
      
      ;; define-syntaxes just get noted
      [(define-syntaxes (var ...) . rest)
       (andmap identifier? (syntax->list #'(var ...)))
       (map make-def-stx-binding (syntax->list #'(var ...)))]
      
      ;; otherwise, do nothing in this pass
      ;; handles expressions, provides, requires, etc and whatnot
      [_ (list)])))





;; typecheck the expressions of a module-top-level form
;; no side-effects
;; syntax -> void
(define (tc-toplevel/pass2 form)
  (parameterize ([current-orig-stx form])
    (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal define-type-internal 
                                   require/typed-internal values)
      ;; these forms we have been instructed to ignore
      [stx 
       (syntax-property form 'typechecker:ignore)
       (void)]
      
      ;; this is a form that we mostly ignore, but we check some interior parts
      [stx 
       (syntax-property form 'typechecker:ignore-some)
       (check-subforms/ignore form)]
      
      ;; these forms should always be ignored
      [(#%require . _) (void)]
      [(#%provide . _) (void)]
      [(define-syntaxes . _) (void)]
      [(define-values-for-syntax . _) (void)]
      
      ;; FIXME - we no longer need these special cases
      ;; these forms are handled in pass1
      [(define-values () (begin (quote-syntax (require/typed-internal . rest)) (#%plain-app values)))
       (void)]
      [(define-values () (begin (quote-syntax (define-type-alias-internal . rest)) (#%plain-app values)))
       (void)]
      [(define-values () (begin (quote-syntax (define-typed-struct-internal . rest)) (#%plain-app values)))
       (void)]          
      
      ;; definitions just need to typecheck their bodies
      [(define-values (var ...) expr)
       (begin (let* ([vars (syntax->list #'(var ...))]
                     [ts (map lookup-type vars)])
                (tc-expr/check #'expr (ret ts)))
              (void))]
      
      ;; to handle the top-level, we have to recur into begins
      [(begin) (void)]
      [(begin . rest)
       (let loop ([l (syntax->list #'rest)])
         (if (null? (cdr l))
             (tc-toplevel/pass2 (car l))
             (begin (tc-toplevel/pass2 (car l))
                    (loop (cdr l)))))]
      
      ;; otherwise, the form was just an expression
      [_ (tc-expr form)])))



;; new implementation of type-check
(define-syntax-rule (internal-syntax-pred nm)
  (lambda (form)
    (kernel-syntax-case* form #f 
      (nm values)
      [(define-values () (begin (quote-syntax (nm . rest)) (#%plain-app values)))
       #t]
      [_ #f])))

(define (parse-def x)
  (kernel-syntax-case x #f
    [(define-values (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))

(define (parse-syntax-def x)
  (kernel-syntax-case x #f
    [(define-syntaxes (nm ...) . rest) (syntax->list #'(nm ...))]
    [_ #f]))


(define (add-type-name! names)
  (for-each register-type-name names))

(define (parse-type-alias form)
  (kernel-syntax-case* form #f 
    (define-type-alias-internal values)
    [(define-values () (begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values)))
     (values #'nm #'ty)]
    [_ (int-err "not define-type-alias")]))

(define (type-check forms0)
  (begin-with-definitions
    (define forms (syntax->list forms0))
    (define-values (type-aliases struct-defs stx-defs0 val-defs0 provs reqs)
      (filter-multiple 
       forms
       (internal-syntax-pred define-type-alias-internal)
       (lambda (e) (or ((internal-syntax-pred define-typed-struct-internal) e)
                       ((internal-syntax-pred define-typed-struct/exec-internal) e)))
       parse-syntax-def
       parse-def 
       provide?
       define/fixup-contract?))
    (for-each (compose register-type-alias parse-type-alias) type-aliases)   
    ;; add the struct names to the type table
    (for-each (compose add-type-name! names-of-struct) struct-defs)
    ;; resolve all the type aliases, and error if there are cycles
    (resolve-type-aliases parse-type)
    ;; do pass 1, and collect the defintions
    (define defs (apply append (filter list? (map tc-toplevel/pass1 forms))))
    ;; separate the definitions into structures we'll handle for provides    
    (define stx-defs (filter def-stx-binding? defs))
    (define val-defs (filter def-binding? defs))
    ;; typecheck the expressions and the rhss of defintions
    (for-each tc-toplevel/pass2 forms)
    ;; check that declarations correspond to definitions
    (check-all-registered-types)
    ;; report delayed errors
    (report-all-errors)
    ;; compute the new provides
    (with-syntax
        ([((new-provs ...) ...) (map (generate-prov stx-defs val-defs) provs)])
      #`(begin
           #,(env-init-code)
           #,(tname-env-init-code)
           #,(talias-env-init-code)
           (begin new-provs ... ...)))))

;; typecheck a top-level form
;; used only from #%top-interaction
;; syntax -> void
(define (tc-toplevel-form form)
  (tc-toplevel/pass1 form)  
  (begin0 (tc-toplevel/pass2 form)
          (report-all-errors)))

