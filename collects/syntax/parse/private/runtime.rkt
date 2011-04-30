#lang racket/base
(require racket/list
         racket/stxparam
         "runtime-progress.rkt"
         "runtime-failure.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/kerncase
                     racket/private/sc
                     racket/syntax
                     "rep-data.rkt"
                     "rep-attrs.rkt"))

(provide (all-from-out "runtime-progress.rkt")
         (all-from-out "runtime-failure.rkt")

         this-syntax
         this-context-syntax

         stx-list-take
         stx-list-drop/cx

         let-attributes
         let-attributes*
         let/unpack
         defattrs/unpack
         attribute
         attribute-binding
         check-list^depth)

;; == Syntax Parameters

;; this-syntax
;; Bound to syntax being matched inside of syntax class
(define-syntax-parameter this-syntax
  (lambda (stx)
    (wrong-syntax stx "used out of context: not within a syntax class")))

;; this-context-syntax
;; Bound to (expression that extracts) context syntax (bottom frame in progress)
(define-syntax-parameter this-context-syntax
  (lambda (stx)
    (wrong-syntax stx "used out of context: not within a syntax class")))

;; == with ==

(provide with)

(define-syntax (with stx)
  (syntax-case stx ()
    [(with ([stxparam expr] ...) . body)
     (with-syntax ([(var ...) (generate-temporaries #'(stxparam ...))])
       (syntax/loc stx
         (let ([var expr] ...)
           (syntax-parameterize ((stxparam (make-rename-transformer (quote-syntax var)))
                                 ...)
             . body))))]))

;; == Control information ==

(provide fail-handler
         cut-prompt
         wrap-user-code

         fail
         try)

(define-syntax-parameter fail-handler
  (lambda (stx)
    (wrong-syntax stx "internal error: used out of context")))
(define-syntax-parameter cut-prompt
  (lambda (stx)
    (wrong-syntax stx "internal error: used out of context")))

(define-syntax-rule (wrap-user-code e)
  (with ([fail-handler #f]
         [cut-prompt #t])
    e))

(define-syntax-rule (fail fs)
  (fail-handler fs))

(define-syntax (try stx)
  (syntax-case stx ()
    [(try e0 e ...)
     (with-syntax ([(re ...) (reverse (syntax->list #'(e ...)))])
       (with-syntax ([(fh ...) (generate-temporaries #'(re ...))])
         (with-syntax ([(next-fh ...) (drop-right (syntax->list #'(fail-handler fh ...)) 1)]
                       [(last-fh) (take-right (syntax->list #'(fail-handler fh ...)) 1)])
           #'(let* ([fh (lambda (fs1)
                          (with ([fail-handler
                                  (lambda (fs2)
                                    (next-fh (cons fs1 fs2)))])
                            re))]
                    ...)
               (with ([fail-handler last-fh])
                 e0)))))]))

;; -----

(require syntax/stx)
(define (stx-list-take stx n)
  (datum->syntax #f
                 (let loop ([stx stx] [n n])
                   (if (zero? n)
                       null
                       (cons (stx-car stx)
                             (loop (stx-cdr stx) (sub1 n)))))))

;; stx-list-drop/cx : stxish stx nat -> (values stxish stx)
(define (stx-list-drop/cx x cx n)
  (let loop ([x x] [cx cx] [n n])
    (if (zero? n)
        (values x
                (if (syntax? x) x cx))
        (loop (stx-cdr x)
              (if (syntax? x) x cx)
              (sub1 n)))))

;; == Attributes

(begin-for-syntax
 (define-struct attribute-mapping (var name depth syntax?)
   #:omit-define-syntaxes
   #:property prop:procedure
   (lambda (self stx)
     (if (attribute-mapping-syntax? self)
         #`(#%expression #,(attribute-mapping-var self))
         #`(let ([value #,(attribute-mapping-var self)])
             (if (check-syntax '#,(attribute-mapping-depth self) value)
                 value
                 (raise-syntax-error
                  #f
                  (format "attribute is bound to non-syntax value: ~e" value)
                  (quote-syntax #,(or (let loop ([p (syntax-property stx 'disappeared-use)])
                                        (cond [(identifier? p) p]
                                              [(pair? p) (or (loop (car p)) (loop (cdr p)))]
                                              [else #f]))
                                      (attribute-mapping-name self))))))))))

;; check-syntax : nat any -> boolean
;; Returns #t if value is a (listof^depth syntax)
(define (check-syntax depth value)
  (if (zero? depth)
      (syntax? value)
      (and (list? value)
           (for/and ([part (in-list value)])
             (check-syntax (sub1 depth) part)))))

(define-for-syntax (parse-attr x)
  (syntax-case x ()
    [#s(attr name depth syntax?) #'(name depth syntax?)]))

(define-syntax (let-attributes stx)
  (syntax-case stx ()
    [(let-attributes ([a value] ...) . body)
     (with-syntax ([((name depth syntax?) ...)
                    (map parse-attr (syntax->list #'(a ...)))])
       (with-syntax ([(vtmp ...) (generate-temporaries #'(name ...))]
                     [(stmp ...) (generate-temporaries #'(name ...))])
         #'(letrec-syntaxes+values
               ([(stmp) (make-attribute-mapping (quote-syntax vtmp)
                                                'name 'depth 'syntax?)] ...)
               ([(vtmp) value] ...)
             (letrec-syntaxes+values
                 ([(name) (make-syntax-mapping 'depth (quote-syntax stmp))] ...)
                 ()
               . body))))]))

;; (let-attributes* (([id num] ...) (expr ...)) expr) : expr
;; Special case: empty attrs need not match number of value exprs.
(define-syntax let-attributes*
  (syntax-rules ()
    [(la* (() _) . body)
     (let () . body)]
    [(la* ((a ...) (val ...)) . body)
     (let-attributes ([a val] ...) . body)]))

;; (let/unpack (([id num] ...) expr) expr) : expr
;; Special case: empty attrs need not match packed length
(define-syntax (let/unpack stx)
  (syntax-case stx ()
    [(let/unpack (() packed) body)
     #'body]
    [(let/unpack ((a ...) packed) body)
     (with-syntax ([(tmp ...) (generate-temporaries #'(a ...))])
       #'(let-values ([(tmp ...) (apply values packed)])
           (let-attributes ([a tmp] ...) body)))]))

(define-syntax (defattrs/unpack stx)
  (syntax-case stx ()
    [(defattrs (a ...) packed)
     (with-syntax ([((name depth syntax?) ...)
                    (map parse-attr (syntax->list #'(a ...)))])
       (with-syntax ([(vtmp ...) (generate-temporaries #'(name ...))]
                     [(stmp ...) (generate-temporaries #'(name ...))])
         #'(begin (define-values (vtmp ...) (apply values packed))
                  (define-syntax stmp
                    (make-attribute-mapping (quote-syntax vtmp)
                                            'name 'depth 'syntax?))
                  ...
                  (define-syntax name (make-syntax-mapping 'depth (quote-syntax stmp)))
                  ...)))]))

(define-syntax (attribute stx)
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(attribute name)
       (identifier? #'name)
       (let ([mapping (syntax-local-value #'name (lambda () #f))])
         (unless (syntax-pattern-variable? mapping)
           (wrong-syntax #'name "not bound as a pattern variable"))
         (let ([var (syntax-mapping-valvar mapping)])
           (let ([attr (syntax-local-value var (lambda () #f))])
             (unless (attribute-mapping? attr)
               (wrong-syntax #'name "not bound as an attribute"))
             (syntax-property (attribute-mapping-var attr)
                              'disappeared-use
                              #'name))))])))

;; (attribute-binding id)
;; mostly for debugging/testing
(define-syntax (attribute-binding stx)
  (syntax-case stx ()
    [(attribute-bound? name)
     (identifier? #'name)
     (let ([value (syntax-local-value #'name (lambda () #f))])
       (if (syntax-pattern-variable? value)
           (let ([value (syntax-local-value (syntax-mapping-valvar value) (lambda () #f))])
             (if (attribute-mapping? value)
                 #`(quote #,(make-attr (attribute-mapping-name value)
                                       (attribute-mapping-depth value)
                                       (attribute-mapping-syntax? value)))
                 #'(quote #f)))
           #'(quote #f)))]))

;; (check-list^depth attr expr)
(define-syntax (check-list^depth stx)
  (syntax-case stx ()
    [(_ a expr)
     (with-syntax ([#s(attr name depth syntax?) #'a])
       (quasisyntax/loc #'expr
         (check-list^depth* 'name 'depth expr)))]))

(define (check-list^depth* aname n0 v0)
  (define (loop n v)
    (when (positive? n)
      (unless (list? v)
        (raise-type-error aname (format "lists nested ~s deep" n0) v))
      (for ([x (in-list v)]) (loop (sub1 n) x))))
  (loop n0 v0)
  v0)


;; ====

(provide check-literal
         free-identifier=?/phases)

;; check-literal : id phase-level stx -> void
;; FIXME: change to normal 'error', if src gets stripped away
(define (check-literal id phase ctx)
  (unless (identifier-binding id phase)
    (raise-syntax-error #f
                        (format "literal is unbound in phase ~s" phase)
                        ctx id)))

;; free-identifier=?/phases : id phase-level id phase-level -> boolean
;; Determines whether x has the same binding at phase-level phase-x
;; that y has at phase-level y.
;; At least one of the identifiers MUST have a binding (module or lexical)
(define (free-identifier=?/phases x phase-x y phase-y)
  (let ([bx (identifier-binding x phase-x)]
        [by (identifier-binding y phase-y)])
    (cond [(and (list? bx) (list? by))
           (let ([modx (module-path-index-resolve (first bx))]
                 [namex (second bx)]
                 [phasex (fifth bx)]
                 [mody (module-path-index-resolve (first by))]
                 [namey (second by)]
                 [phasey (fifth by)])
             (and (eq? modx mody) ;; resolved-module-paths are interned
                  (eq? namex namey)
                  (equal? phasex phasey)))]
          [else
           ;; Module is only way to get phase-shift; if not module-bound names,
           ;; then only identifiers at same phase can refer to same binding.
           (and (equal? phase-x phase-y)
                (free-identifier=? x y phase-x))])))

;; ----

(provide begin-for-syntax/once)

;; (begin-for-syntax/once expr/phase1 ...)
;; evaluates in pass 2 of module/intdefs expansion
(define-syntax (begin-for-syntax/once stx)
  (syntax-case stx ()
    [(bfs/o e ...)
     (cond [(list? (syntax-local-context))
            #`(define-values ()
                (begin (begin-for-syntax/once e ...)
                       (values)))]
           [else
            #'(let-syntax ([m (lambda _ (begin e ...) #'(void))])
                (m))])]))

;; ====

(provide no-shadow)

(begin-for-syntax
 (define (check-shadow def)
   (syntax-case def ()
     [(_def (x ...) . _)
      (parameterize ((current-syntax-context def))
        (for ([x (in-list (syntax->list #'(x ...)))])
          (let ([v (syntax-local-value x (lambda _ #f))])
            (when (syntax-pattern-variable? v)
              (wrong-syntax
               x
               ;; FIXME: customize "~do pattern" vs "#:do block" as appropriate
               "definition in ~~do pattern must not shadow attribute binding")))))])))

(define-syntax (no-shadow stx)
  (syntax-case stx ()
    [(no-shadow e)
     (let ([ee (local-expand #'e (syntax-local-context)
                             (kernel-form-identifier-list))])
       (syntax-case ee (begin define-values define-syntaxes)
         [(begin d ...)
          #'(begin (no-shadow d) ...)]
         [(define-values . _)
          (begin (check-shadow ee)
                 ee)]
         [(define-syntaxes . _)
          (begin (check-shadow ee)
                 ee)]
         [_
          ee]))]))

;; ----

(provide curried-stxclass-parser
         app-argu)

(define-syntax (curried-stxclass-parser stx)
  (syntax-case stx ()
    [(_ class argu)
     (with-syntax ([#s(arguments (parg ...) (kw ...) _) #'argu])
       (let ([sc (get-stxclass/check-arity #'class #'class
                                           (length (syntax->list #'(parg ...)))
                                           (syntax->datum #'(kw ...)))])
         (with-syntax ([parser (stxclass-parser sc)])
           #'(lambda (x cx pr es fh cp success)
               (app-argu parser x cx pr es fh cp success argu)))))]))

(define-syntax (app-argu stx)
  (syntax-case stx ()
    [(aa proc extra-parg ... #s(arguments (parg ...) (kw ...) (kwarg ...)))
     #|
     Use keyword-apply directly?
        #'(keyword-apply proc '(kw ...) (list kwarg ...) parg ... null)
     If so, create separate no-keyword clause.
     |#
     ;; For now, let #%app handle it.
     (with-syntax ([((kw-part ...) ...) #'((kw kwarg) ...)])
       #'(proc kw-part ... ... extra-parg ... parg ...))]))
