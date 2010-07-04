#lang scheme/unit


(require (rename-in "../utils/utils.ss" [private r:private]))
(require syntax/kerncase
         scheme/match
         "signatures.ss"
         (r:private type-utils type-effect-convenience union subtype 
		    parse-type type-annotation stxclass-util)
         (rep type-rep effect-rep)
         (utils tc-utils)
         (env lexical-env)
         (only-in (env type-environments) lookup current-tvars extend-env)
         scheme/private/class-internal
         (except-in stxclass id)
         (only-in srfi/1 split-at))

(require (for-template scheme/base scheme/private/class-internal))

(import tc-if^ tc-lambda^ tc-app^ tc-let^ check-subforms^)
(export tc-expr^)


;; return the type of a literal value
;; scheme-value -> type
(define (tc-literal v-stx)
  (syntax-parse v-stx 
    [i:boolean (-val #'i.datum)]
    [i:identifier (-val #'i.datum)]
    [i:exact-integer -Integer]
    [i:number N]
    [i:str -String]
    [i:char -Char]
    [i:keyword (-val #'i.datum)]
    [i:bytes -Bytes]
    [i:byte-pregexp -Byte-PRegexp]
    [i:byte-regexp -Byte-Regexp]
    [i:regexp -Regexp]
    [(i ...) 
     (-Tuple (map tc-literal (syntax->list #'(i ...))))]
    [i #:declare i (3d vector?)
     (make-Vector (apply Un (map tc-literal (vector->list #'i.datum))))]
    [_ Univ]))


;; do-inst : syntax type -> type
(define (do-inst stx ty)
  (define inst (syntax-property stx 'type-inst))
  (define (split-last l)
    (let-values ([(all-but last-list) (split-at l (sub1 (length l)))])
      (values all-but (car last-list))))
  (define (in-improper-stx stx)
    (let loop ([l stx])
      (match l
        [#f null]
        [(cons a b) (cons a (loop b))]
        [e (list e)])))
  (for/fold ([ty ty])
    ([inst (in-improper-stx inst)])
    (cond [(not inst) ty]
          [(not (or (Poly? ty) (PolyDots? ty)))
           (tc-error/expr #:return (Un) "Cannot instantiate non-polymorphic type ~a" ty)]
          
          [(and (Poly? ty)
                (not (= (length (syntax->list inst)) (Poly-n ty))))
           (tc-error/expr #:return (Un)
                          "Wrong number of type arguments to polymorphic type ~a:~nexpected: ~a~ngot: ~a"
                          ty (Poly-n ty) (length (syntax->list inst)))]
          [(and (PolyDots? ty) (not (>= (length (syntax->list inst)) (sub1 (PolyDots-n ty)))))
           ;; we can provide 0 arguments for the ... var
           (tc-error/expr #:return (Un)
                          "Wrong number of type arguments to polymorphic type ~a:~nexpected at least: ~a~ngot: ~a"
                          ty (sub1 (PolyDots-n ty)) (length (syntax->list inst)))]
          [(PolyDots? ty)
           ;; In this case, we need to check the last thing.  If it's a dotted var, then we need to
           ;; use instantiate-poly-dotted, otherwise we do the normal thing.
           (let-values ([(all-but-last last-stx) (split-last (syntax->list inst))])
             (match (syntax-e last-stx)
               [(cons last-ty-stx (? identifier? last-id-stx))
                (unless (Dotted? (lookup (current-tvars) (syntax-e last-id-stx) (lambda _ #f)))
                  (tc-error/stx last-id-stx "~a is not a type variable bound with ..." (syntax-e last-id-stx)))
                (if (= (length all-but-last) (sub1 (PolyDots-n ty)))
                    (let* ([last-id (syntax-e last-id-stx)]
                           [last-ty
                            (parameterize ([current-tvars (extend-env (list last-id)
                                                                      (list (make-DottedBoth (make-F last-id)))
                                                                      (current-tvars))])
                              (parse-type last-ty-stx))])
                      (instantiate-poly-dotted ty (map parse-type all-but-last) last-ty last-id))
                    (tc-error/expr #:return (Un) "Wrong number of fixed type arguments to polymorphic type ~a:~nexpected: ~a~ngot: ~a"
                                   ty (sub1 (PolyDots-n ty)) (length all-but-last)))]
               [_
                (instantiate-poly ty (map parse-type (syntax->list inst)))]))]
          [else
           (instantiate-poly ty (map parse-type (syntax->list inst)))])))

;; typecheck an identifier
;; the identifier has variable effect
;; tc-id : identifier -> tc-result
(define (tc-id id)
  (let* ([ty (lookup-type/lexical id)])
    (ret ty (list (make-Var-True-Effect id)) (list (make-Var-False-Effect id)))))

;; typecheck an expression, but throw away the effect
;; tc-expr/t : Expr -> Type
(define (tc-expr/t e) (match (tc-expr e)
                        [(tc-result: t) t]
			[t (int-err "tc-expr returned ~a, not a tc-result, for ~a" t (syntax->datum e))]))

(define (tc-expr/check/t e t)
  (match (tc-expr/check e t)
    [(tc-result: t) t]))

;; check-below : (/\ (Result Type -> Result)
;;                   (Type Type -> Type))
(define (check-below tr1 expected)
  (match* (tr1 expected)
    [((tc-result: t1 te1 ee1) t2)
     (unless (subtype t1 t2)
       (tc-error/expr "Expected ~a, but got ~a" t2 t1))
     (ret expected)]
    [(t1 t2)
     (unless (subtype t1 t2)
       (tc-error/expr "Expected ~a, but got ~a" t2 t1))
     expected]))

(define (tc-expr/check form expected)
  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a~n" (syntax-object->datum form))
    ;; the argument must be syntax
    (unless (syntax? form) 
      (int-err "bad form input to tc-expr: ~a" form))
    (let (;; a local version of ret that does the checking
          [ret 
           (lambda args
             (define te (apply ret args))
             (check-below te expected)
             (ret expected))])
      (kernel-syntax-case* form #f 
        (letrec-syntaxes+values find-method/who) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
        [stx
         (syntax-property form 'typechecker:with-handlers)
         (check-subforms/with-handlers/check form expected)]
        [stx 
         (syntax-property form 'typechecker:ignore-some)
         (let ([ty (check-subforms/ignore form)])
           (unless ty
             (int-err "internal error: ignore-some"))
           (check-below ty expected))]
        ;; data
        [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
        [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
        [(quote val)  (ret (tc-literal #'val))]
        ;; syntax
        [(quote-syntax datum) (ret (-Syntax (tc-literal #'datum)))]
        ;; mutation!
        [(set! id val)
         (match-let* ([(tc-result: id-t) (tc-expr #'id)]
                      [(tc-result: val-t) (tc-expr #'val)])
           (unless (subtype val-t id-t)
             (tc-error/expr "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
           (ret -Void))]
        ;; top-level variable reference - occurs at top level
        [(#%top . id) (check-below (tc-id #'id) expected)]
        ;; weird
        [(#%variable-reference . _)
         (tc-error/expr #:return (ret expected) "#%variable-reference is not supported by Typed Scheme")]
        ;; identifiers
        [x (identifier? #'x) 
           (check-below (tc-id #'x) expected)]
        ;; w-c-m
        [(with-continuation-mark e1 e2 e3)
         (begin (tc-expr/check #'e1 Univ)
                (tc-expr/check #'e2 Univ)
                (tc-expr/check #'e3 expected))]  
        ;; application        
        [(#%plain-app . _) (tc/app/check form expected)]
        ;; #%expression
        [(#%expression e) (tc-expr/check #'e expected)]
        ;; syntax
        ;; for now, we ignore the rhs of macros
        [(letrec-syntaxes+values stxs vals . body)
         (tc-expr/check (syntax/loc form (letrec-values vals . body)) expected)]
        ;; begin
        [(begin e . es) (tc-exprs/check (syntax->list #'(e . es)) expected)]
        [(begin0 e . es)
         (begin (tc-exprs/check (syntax->list #'es) Univ)
                (tc-expr/check #'e expected))]          
        ;; if
        [(if tst thn els) (tc/if-twoarm/check #'tst #'thn #'els expected)]
        ;; lambda
        [(#%plain-lambda formals . body)
         (tc/lambda/check form #'(formals) #'(body) expected)]        
        [(case-lambda [formals . body] ...)
         (tc/lambda/check form #'(formals ...) #'(body ...) expected)] 
        ;; send
        [(let-values (((_) meth))
           (let-values (((_ _) (#%plain-app find-method/who _ rcvr _)))
             (#%plain-app _ _ args ...)))
         (tc/send #'rcvr #'meth #'(args ...) expected)]
        ;; let
        [(let-values ([(name ...) expr] ...) . body)
         (tc/let-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
        [(letrec-values ([(name ...) expr] ...) . body)
         (tc/letrec-values/check #'((name ...) ...) #'(expr ...) #'body form expected)]
        ;; other
        [_ (tc-error/expr #:return (ret expected) "cannot typecheck unknown form : ~a~n" (syntax->datum form))]
        ))))

;; type check form in the current type environment
;; if there is a type error in form, or if it has the wrong annotation, error
;; otherwise, produce the type of form
;; syntax[expr] -> type
(define (tc-expr form)
  ;; do the actual typechecking of form
  ;; internal-tc-expr : syntax -> Type    
  (define (internal-tc-expr form)
    (kernel-syntax-case* form #f 
      (letrec-syntaxes+values #%datum #%app lambda find-method/who) ;; letrec-syntaxes+values is not in kernel-syntax-case literals
      ;; 
      [stx
       (syntax-property form 'typechecker:with-handlers)
       (let ([ty (check-subforms/with-handlers form)])
         (unless ty
           (int-err "internal error: with-handlers"))
         ty)]
      [stx 
       (syntax-property form 'typechecker:ignore-some)
       (let ([ty (check-subforms/ignore form)])
         (unless ty
           (int-err "internal error: ignore-some"))
         ty)]
      
      ;; data
      [(quote #f) (ret (-val #f) (list (make-False-Effect)) (list (make-False-Effect)))]
      [(quote #t) (ret (-val #t) (list (make-True-Effect)) (list (make-True-Effect)))]
      
      [(quote val)  (ret (tc-literal #'val))]
      ;; syntax
      [(quote-syntax datum) (ret (-Syntax (tc-literal #'datum)))]
      ;; w-c-m
      [(with-continuation-mark e1 e2 e3)
       (begin (tc-expr/check #'e1 Univ)
              (tc-expr/check #'e2 Univ)
              (tc-expr #'e3))]
      ;; lambda
      [(#%plain-lambda formals . body)
       (tc/lambda form #'(formals) #'(body))]        
      [(case-lambda [formals . body] ...)
       (tc/lambda form #'(formals ...) #'(body ...))]  
      ;; send
      [(let-values (((_) meth))
         (let-values (((_ _) (#%plain-app find-method/who _ rcvr _)))
           (#%plain-app _ _ args ...)))
       (tc/send #'rcvr #'meth #'(args ...))]
      ;; let
      [(let-values ([(name ...) expr] ...) . body)
       (tc/let-values #'((name ...) ...) #'(expr ...) #'body form)]
      [(letrec-values ([(name ...) expr] ...) . body)
       (tc/letrec-values #'((name ...) ...) #'(expr ...) #'body form)]        
      ;; mutation!
      [(set! id val)
       (match-let* ([(tc-result: id-t) (tc-expr #'id)]
                    [(tc-result: val-t) (tc-expr #'val)])
         (unless (subtype val-t id-t)
           (tc-error/expr "Mutation only allowed with compatible types:~n~a is not a subtype of ~a" val-t id-t))
         (ret -Void))]        
      ;; top-level variable reference - occurs at top level
      [(#%top . id) (tc-id #'id)]
      ;; #%expression
      [(#%expression e) (tc-expr #'e)]
      ;; weird
      [(#%variable-reference . _)
       (tc-error/expr #:return (ret (Un)) "#%variable-reference is not supported by Typed Scheme")]
      ;; identifiers
      [x (identifier? #'x) (tc-id #'x)]                 
      ;; application        
      [(#%plain-app . _) (tc/app form)]
      ;; if
      [(if tst body) (tc/if-twoarm #'tst #'body #'(#%app void))]
      [(if tst thn els) (tc/if-twoarm #'tst #'thn #'els)]                          

      
      
      ;; syntax
      ;; for now, we ignore the rhs of macros
      [(letrec-syntaxes+values stxs vals . body)
       (tc-expr (syntax/loc form (letrec-values vals . body)))]
      
      ;; begin
      [(begin e . es) (tc-exprs (syntax->list #'(e . es)))]
      [(begin0 e . es)
       (begin (tc-exprs (syntax->list #'es))
              (tc-expr #'e))]
      ;; other
      [_ (tc-error/expr #:return (ret (Un)) "cannot typecheck unknown form : ~a~n" (syntax->datum form))]))
  
  (parameterize ([current-orig-stx form])
    ;(printf "form: ~a~n" (syntax->datum form))
    ;; the argument must be syntax
    (unless (syntax? form) 
      (int-err "bad form input to tc-expr: ~a" form))
    ;; typecheck form
    (let ([ty (cond [(type-ascription form) => (lambda (ann)
                                                 (tc-expr/check form ann))]
                    [else (internal-tc-expr form)])])
      (match ty
        [(tc-result: t eff1 eff2)
         (let ([ty* (do-inst form t)])
           (ret ty* eff1 eff2))]))))

(define (tc/send rcvr method args [expected #f])
  (match (tc-expr rcvr)
    [(tc-result: (Instance: (and c (Class: _ _ methods))))
     (match (tc-expr method)
       [(tc-result: (Value: (? symbol? s)))
        (let* ([ftype (cond [(assq s methods) => cadr]
                            [else (tc-error/expr "send: method ~a not understood by class ~a" s c)])]
               [ret-ty (tc/funapp rcvr args (ret ftype) (map tc-expr (syntax->list args)) expected)])
          (if expected
              (begin (check-below ret-ty expected) (ret expected))
              ret-ty))]
       [(tc-result: t) (int-err "non-symbol methods not supported by Typed Scheme: ~a" t)])]
    [(tc-result: t) (tc-error/expr #:return (or expected (ret (Un))) "send: expected a class instance, got ~a" t)]))

;; type-check a list of exprs, producing the type of the last one.
;; if the list is empty, the type is Void.
;; list[syntax[expr]] -> tc-result
(define (tc-exprs exprs)
  (cond [(null? exprs) (ret -Void)]
        [(null? (cdr exprs)) (tc-expr (car exprs))]
        [else (tc-expr/check (car exprs) Univ)
              (tc-exprs (cdr exprs))]))

(define (tc-exprs/check exprs expected)
  (cond [(null? exprs) (check-below (ret -Void) expected)]
        [(null? (cdr exprs)) (tc-expr/check (car exprs) expected)]
        [else (tc-expr/check (car exprs) Univ)
              (tc-exprs/check (cdr exprs) expected)]))
