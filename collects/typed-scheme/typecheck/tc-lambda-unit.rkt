#lang scheme/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt"
         "tc-metafunctions.rkt"
         "tc-subst.rkt" "check-below.rkt"
         mzlib/trace
         scheme/list
         racket/syntax unstable/struct syntax/stx
         (rename-in scheme/contract [-> -->] [->* -->*] [one-of/c -one-of/c])
         (except-in (rep type-rep) make-arr)
         (rename-in (types convenience utils union)
                    [make-arr* make-arr])
         (private type-annotation)
         (types abbrev utils)
	 (env type-env-structs lexical-env tvar-env index-env)
	 (utils tc-utils)

         racket/match)
(require (for-template scheme/base "internal-forms.rkt"))

(import tc-expr^)
(export tc-lambda^)

(define-struct/cond-contract lam-result ([args (listof (list/c identifier? Type/c))]
                                         [kws (listof (list/c keyword? identifier? Type/c boolean?))]
                                         [rest (or/c #f (list/c identifier? Type/c))]
                                         [drest (or/c #f (cons/c identifier? (cons/c Type/c symbol?)))]
                                         [body tc-results?])
  #:transparent)

(define (lam-result->type lr)
  (match lr
    [(struct lam-result ((list (list arg-ids arg-tys) ...) (list (list kw kw-id kw-ty req?) ...) rest drest body))
     (let ([arg-names (append arg-ids
                              (if rest (list (car rest)) null)
                              (if drest (list (car drest)) null)
                              kw-id)])
       (make-arr
        arg-tys
        (abstract-results body arg-names)
        #:kws (map make-Keyword kw kw-ty req?)
        #:rest (if rest (second rest) #f)
        #:drest (if drest (cdr drest) #f)))]
    [_ (int-err "not a lam-result")]))

(define (expected-str tys-len rest-ty drest arg-len rest)
  (format "Expected function with ~a argument~a~a, but got function with ~a argument~a~a"
          tys-len
          (if (= tys-len 1) "" "s")
          (if (or rest-ty
                  drest)
              " and a rest arg"
              "")
          arg-len
          (if (= arg-len 1) "" "s")
          (if rest " and a rest arg" "")))

;; listof[id] option[id] block listof[type] option[type] option[(cons type var)] tc-result -> lam-result
(define/cond-contract (check-clause arg-list rest body arg-tys rest-ty drest ret-ty)
     ((listof identifier?)
      (or/c #f identifier?) syntax? (listof Type/c) (or/c #f Type/c) (or/c #f (cons/c Type/c symbol?)) tc-results?
      . --> .
      lam-result?)
  (let* ([arg-len (length arg-list)]
         [tys-len (length arg-tys)]
         [arg-types (if (andmap type-annotation arg-list)
                        (get-types arg-list #:default Univ)
                        (cond
                          [(= arg-len tys-len) arg-tys]
                          [(< arg-len tys-len) (take arg-tys arg-len)]
                          [(> arg-len tys-len) (append arg-tys
                                                       (map (lambda _ (or rest-ty (Un)))
                                                            (drop arg-list tys-len)))]))])
    (define (check-body)
      (with-lexical-env/extend
       arg-list arg-types
       (make-lam-result (for/list ([al arg-list] [at arg-types] [a-ty arg-tys]) (list al at)) null
                        (and rest-ty (list (or rest (generate-temporary)) rest-ty))
                        ;; make up a fake name if none exists, this is an error case anyway
                        (and drest (cons (or rest (generate-temporary)) drest))
                        (tc-exprs/check (syntax->list body) ret-ty))))
    (when (or (not (= arg-len tys-len))
              (and (or rest-ty drest) (not rest)))
      (tc-error/delayed (expected-str tys-len rest-ty drest arg-len rest)))
    (cond
      [(not rest)
       (check-body)]
      [drest
       (with-lexical-env/extend
        (list rest) (list (make-ListDots (car drest) (cdr drest)))
        (check-body))]
      [(dotted? rest)
       =>
       (lambda (b)
         (let ([dty (get-type rest #:default Univ)])
           (with-lexical-env/extend
            (list rest) (list (make-ListDots dty b))
            (check-body))))]
      [else
       (let ([rest-type (cond
                          [rest-ty rest-ty]
                          [(type-annotation rest) (get-type rest #:default Univ)]
                          [(< arg-len tys-len) (list-ref arg-tys arg-len)]
                          [else (Un)])])
         (with-lexical-env/extend
          (list rest) (list (-lst rest-type))
          (check-body)))])))

;; typecheck a single lambda, with argument list and body
;; drest-ty and drest-bound are both false or not false
;; syntax-list[id] block listof[type] tc-result option[type] option[(cons type var)] -> lam-result
(define (tc/lambda-clause/check args body arg-tys ret-ty rest-ty drest)
    (syntax-case args ()
      [(args* ...)
       (check-clause (syntax->list #'(args* ...)) #f body arg-tys rest-ty drest ret-ty)]
      [(args* ... . rest)
       (check-clause (syntax->list #'(args* ...)) #'rest body arg-tys rest-ty drest ret-ty)]))

;; syntax-list[id] block -> lam-result
(define (tc/lambda-clause args body)
  (syntax-case args ()
    [(args ...)
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (get-types arg-list #:default Univ)])
       (with-lexical-env/extend
        arg-list arg-types
        (make lam-result
              (map list arg-list arg-types)
              null
              #f
              #f
              (tc-exprs (syntax->list body)))))]
    [(args ... . rest)
     (let* ([arg-list (syntax->list #'(args ...))]
            [arg-types (get-types arg-list #:default Univ)])
       (cond
         [(dotted? #'rest)
          =>
          (lambda (bound)
            (unless (bound-index? bound)
              (if (bound-tvar? bound)
                  (tc-error "Bound on ... type (~a) is not an appropriate type variable" bound)
                  (tc-error/stx #'rest "Bound on ... type (~a) was not in scope" bound)))
            (let ([rest-type (extend-tvars (list bound)
                               (get-type #'rest #:default Univ))])
              (with-lexical-env/extend
               (cons #'rest arg-list)
               (cons (make-ListDots rest-type bound) arg-types)
               (make-lam-result
                (map list arg-list arg-types)
                null
                #f
                (cons #'rest (cons rest-type bound))
                (tc-exprs (syntax->list body))))))]
         [else
          (let ([rest-type (get-type #'rest #:default Univ)])
            (with-lexical-env/extend
             (cons #'rest arg-list)
             (cons (make-Listof rest-type) arg-types)
             (make-lam-result
                   (map list arg-list arg-types)
                   null
                   (list #'rest rest-type)
                   #f
                   (tc-exprs (syntax->list body)))))]))]))

(define (formals->list l)
  (let loop ([l (syntax-e l)])
    (cond [(stx-pair? l) (cons (stx-car l) (loop (stx-cdr l)))]
          [(pair? l) (cons (car l) (loop (cdr l)))]
          [else null])))

;; tc/mono-lambda : syntax-list syntax-list (or/c #f tc-results) -> (listof lam-result)
;; typecheck a sequence of case-lambda clauses
(define (tc/mono-lambda formals bodies expected)
  (define (syntax-len s)
    (cond [(syntax->list s) => length]
          [else (let loop ([s s])
                  (cond
                    [(pair? s)
                     (+ 1 (loop (cdr s)))]
                    [(pair? (syntax-e s))
                     (+ 1 (loop (cdr (syntax-e s))))]
                    [else 1]))]))
  (define (formals->list s)
    (let loop ([s s])
      (cond
        [(pair? s)
         (cons (car s) (loop (cdr s)))]
        [(null? s) s]
        [(pair? (syntax-e s))
         (cons (stx-car s) (loop (cdr (syntax-e s))))]
        [(null? (syntax-e s)) null]
        [else (list s)])))
  (define (go formals bodies formals* bodies* nums-seen)
    (cond
      [(null? formals)
       (map tc/lambda-clause (reverse formals*) (reverse bodies*))]
      [(memv (syntax-len (car formals)) nums-seen)
       ;; we check this clause, but it doesn't contribute to the overall type
       (tc/lambda-clause (car formals) (car bodies))
       (go (cdr formals) (cdr bodies) formals* bodies* nums-seen)]
      [else
       (go (cdr formals) (cdr bodies)
           (cons (car formals) formals*)
           (cons (car bodies) bodies*)
           (cons (syntax-len (car formals)) nums-seen))]))
  (cond
    ;; special case for not-case-lambda
    [(and expected
          (= 1 (length (syntax->list formals))))
     (let loop ([expected expected])
       (match expected
         [(tc-result1: (and t (Mu: _ _))) (loop (ret (unfold t)))]
         [(tc-result1: (Function: (list (arr: argss rets rests drests '()) ...)))
          (let ([fmls (car (syntax->list formals))])
            (for/list ([args argss] [ret rets] [rest rests] [drest drests])
              (tc/lambda-clause/check fmls (car (syntax->list bodies))
                                      args (values->tc-results ret (formals->list fmls)) rest drest)))]
         [_ (go (syntax->list formals) (syntax->list bodies) null null null)]))]
    ;; otherwise
    [else (go (syntax->list formals) (syntax->list bodies) null null null)]))

(define (tc/mono-lambda/type formals bodies expected)
  (define t (make-Function (map lam-result->type (tc/mono-lambda formals bodies expected))))
  (if expected
      (and (check-below (ret t true-filter) expected) t)
      t))

;; tc/plambda syntax syntax-list syntax-list type -> Poly
;; formals and bodies must by syntax-lists
(define/cond-contract (tc/plambda form formals bodies expected)
  (syntax? syntax? syntax? (or/c tc-results? #f) . --> . Type/c)
  (define/cond-contract (maybe-loop form formals bodies expected)
    (syntax? syntax? syntax? tc-results? . --> . Type/c)
    (match expected
      [(tc-result1: (Function: _)) (tc/mono-lambda/type formals bodies expected)]
      [(tc-result1: (or (Poly: _ _) (PolyDots: _ _)))
       (tc/plambda form formals bodies expected)]
      [(tc-result1: (Error:)) (tc/mono-lambda/type formals bodies #f)]
      [(tc-result1: (and v (Values: _))) (maybe-loop form formals bodies (values->tc-results v #f))]
      [_ (int-err "expected not an appropriate tc-result: ~a" expected)]))
  (match expected
    [(tc-result1: (and t (Poly-names: ns expected*)))
     (let* ([tvars (let ([p (syntax-property form 'typechecker:plambda)])
                     (when (and (pair? p) (eq? '... (car (last p))))
                       (tc-error
                        "Expected a polymorphic function without ..., but given function had ..."))
                     (or (and p (map syntax-e (syntax->list p)))
                         ns))]
            [ty (extend-tvars tvars
                  (maybe-loop form formals bodies (ret expected*)))])
       ;(printf "plambda: ~a ~a ~a \n" literal-tvars new-tvars ty)
       t)]
    [(tc-result1: (and t (PolyDots-names: (list ns ... dvar) expected*)))
     (let-values
         ([(tvars dotted)
           (let ([p (syntax-property form 'typechecker:plambda)])
             (if p
                 (match (map syntax-e (syntax->list p))
                   [(list var ... dvar '...)
                    (values var dvar)]
                   [_ (tc-error "Expected a polymorphic function with ..., but given function had no ...")])
                 (values ns dvar)))])
       ;; check the body for side effect
       (extend-indexes dotted
         (extend-tvars tvars
           (maybe-loop form formals bodies (ret expected*))))
       t)]
    [#f
     (match (map syntax-e (syntax->list (syntax-property form 'typechecker:plambda)))
       [(list tvars ... dotted-var '...)
        (let* ([ty (extend-indexes dotted-var
                     (extend-tvars tvars
                       (tc/mono-lambda/type formals bodies #f)))])
          (make-PolyDots (append tvars (list dotted-var)) ty))]
       [tvars
        (let* ([ty (extend-tvars tvars
                     (tc/mono-lambda/type formals bodies #f))])
          ;(printf "plambda: ~a ~a ~a \n" literal-tvars new-tvars ty)
          (make-Poly tvars ty))])]
    [(tc-result1: t)
     (unless (check-below (tc/plambda form formals bodies #f) t)
       (tc-error/expr #:return expected
                      "Expected a value of type ~a, but got a polymorphic function." t))
     t]
    [_ (int-err "not a good expected value: ~a" expected)]))

;; typecheck a sequence of case-lambda clauses, which is possibly polymorphic
;; tc/lambda/internal syntax syntax-list syntax-list option[type] -> tc-result
(define (tc/lambda/internal form formals bodies expected)
  (if (or (syntax-property form 'typechecker:plambda)
          (match expected
            [(tc-result1: t) (or (Poly? t) (PolyDots? t))]
            [_ #f]))
      (ret (tc/plambda form formals bodies expected) true-filter)
      (ret (tc/mono-lambda/type formals bodies expected) true-filter)))

;; tc/lambda : syntax syntax-list syntax-list -> tc-result
(define (tc/lambda form formals bodies)
  (tc/lambda/internal form formals bodies #f))

;; tc/lambda/check : syntax syntax-list syntax-list Type -> tc-result
(define (tc/lambda/check form formals bodies expected)
  (tc/lambda/internal form formals bodies expected))

;; form : a syntax object for error reporting
;; formals : the formal arguments to the loop
;; body : a block containing the body of the loop
;; name : the name of the loop
;; args : the types of the actual arguments to the loop
;; ret : the expected return type of the whole expression
(define (tc/rec-lambda/check form formals body name args return)
  (with-lexical-env/extend
   (syntax->list formals) args
   (let* ([r (tc-results->values return)]
          [t (make-arr args r)]
          [ft (make-Function (list t))])
     (with-lexical-env/extend
      (list name) (list ft)
      (begin (tc-exprs/check (syntax->list body) return) (ret ft))))))

;(trace tc/mono-lambda)


