#lang scheme/base

(require scheme/contract/private/guts
         scheme/contract/private/opt
         "contract-arr-checks.ss")
(require (for-syntax scheme/base)
         (for-syntax scheme/contract/private/opt-guts)
         (for-syntax scheme/contract/private/helpers)
         (for-syntax "contract-arr-obj-helpers.ss")
         (for-syntax syntax/stx)
         (for-syntax syntax/name))

(provide ->
         ->d
         ->*
         ->d*
         ->r
         ->pp
         ->pp-rest
         case->
         opt->
         opt->*
         unconstrained-domain->)

(define-syntax (unconstrained-domain-> stx)
  (syntax-case stx ()
    [(_ rngs ...)
     (with-syntax ([(rngs-x ...) (generate-temporaries #'(rngs ...))]
                   [(proj-x ...) (generate-temporaries #'(rngs ...))]
                   [(p-app-x ...) (generate-temporaries #'(rngs ...))]
                   [(res-x ...) (generate-temporaries #'(rngs ...))])
       #'(let ([rngs-x (coerce-contract 'unconstrained-domain-> rngs)] ...)
           (let ([proj-x ((proj-get rngs-x) rngs-x)] ...)
             (make-proj-contract
              (build-compound-type-name 'unconstrained-domain-> ((name-get rngs-x) rngs-x) ...)
              (λ (pos-blame neg-blame src-info orig-str positive-position?)
                (let ([p-app-x (proj-x pos-blame neg-blame src-info orig-str positive-position?)] ...)
                  (λ (val)
                    (if (procedure? val)
                        (λ args
                          (let-values ([(res-x ...) (apply val args)])
                            (values (p-app-x res-x) ...)))
                        (raise-contract-error val
                                              src-info
                                              pos-blame
                                              orig-str
                                              "expected a procedure")))))
              procedure?))))]))

(define (build--> name doms doms-rest rngs kwds quoted-kwds rng-any? func)
  (let ([doms/c (map (λ (dom) (coerce-contract name dom)) doms)]
        [rngs/c (map (λ (rng) (coerce-contract name rng)) rngs)]
        [kwds/c (map (λ (kwd) (coerce-contract name kwd)) kwds)]
        [doms-rest/c (and doms-rest (coerce-contract name doms-rest))])
    (make--> rng-any? doms/c doms-rest/c rngs/c kwds/c quoted-kwds func)))
;; rng-any? : boolean
;; doms : (listof contract)
;; dom-rest : (or/c false/c contract)
;; rngs : (listof contract) -- may be ignored by the wrapper function in the case of any
;; kwds : (listof contract)
;; quoted-keywords : (listof keyword) -- must be sorted by keyword<
;; func : the wrapper function maker. It accepts a procedure for
;;        checking the first-order properties and the contracts
;;        and it produces a wrapper-making function.
(define-struct -> (rng-any? doms dom-rest rngs kwds quoted-kwds func)
  #:omit-define-syntaxes
  #:property proj-prop
  (λ (ctc) 
    (let* ([doms/c (map (λ (x) ((proj-get x) x)) 
                        (if (->-dom-rest ctc)
                            (append (->-doms ctc) (list (->-dom-rest ctc)))
                            (->-doms ctc)))]
           [rngs/c (map (λ (x) ((proj-get x) x)) (->-rngs ctc))]
           [kwds/c (map (λ (x) ((proj-get x) x)) (->-kwds ctc))]
           [mandatory-keywords (->-quoted-kwds ctc)]
           [func (->-func ctc)]
           [dom-length (length (->-doms ctc))]
           [has-rest? (and (->-dom-rest ctc) #t)])
      (lambda (pos-blame neg-blame src-info orig-str positive-position?)
        (let ([partial-doms (map (λ (dom) (dom neg-blame pos-blame src-info orig-str (not positive-position?)))
                                 doms/c)]
              [partial-ranges (map (λ (rng) (rng pos-blame neg-blame src-info orig-str positive-position?))
                                   rngs/c)]
              [partial-kwds (map (λ (kwd) (kwd neg-blame pos-blame src-info orig-str (not positive-position?)))
                                 kwds/c)])
          (apply func
                 (λ (val)
                   (if has-rest?
                       (check-procedure/more val dom-length '() mandatory-keywords src-info pos-blame orig-str)
                       (check-procedure val dom-length 0 '() mandatory-keywords src-info pos-blame orig-str)))
                 (append partial-doms partial-ranges partial-kwds))))))
   
  #:property name-prop
  (λ (ctc) (single-arrow-name-maker 
            (->-doms ctc)
            (->-dom-rest ctc)
            (->-kwds ctc)
            (->-quoted-kwds ctc)
            (->-rng-any? ctc)
            (->-rngs ctc)))
  #:property first-order-prop
  (λ (ctc)
    (let ([l (length (->-doms ctc))])
      (if (->-dom-rest ctc)
          (λ (x)
            (and (procedure? x) 
                 (procedure-accepts-and-more? x l)))
          (λ (x)
            (and (procedure? x) 
                 (procedure-arity-includes? x l)
                 (no-mandatory-keywords? x))))))
  #:property stronger-prop
  (λ (this that)
    (and (->? that)
         (= (length (->-doms that))
            (length (->-doms this)))
         (andmap contract-stronger?
                 (->-doms that)
                 (->-doms this))
         (= (length (->-rngs that))
            (length (->-rngs this)))
         (andmap contract-stronger?
                 (->-rngs this) 
                 (->-rngs that)))))

(define (single-arrow-name-maker doms/c doms-rest kwds/c kwds rng-any? rngs)
  (cond
    [doms-rest
     (build-compound-type-name 
      '->*
      (apply build-compound-type-name (append doms/c (apply append (map list kwds kwds/c))))
      doms-rest
      (cond
        [rng-any? 'any]
        [else (apply build-compound-type-name rngs)]))]
    [else
     (let ([rng-name
            (cond
              [rng-any? 'any]
              [(null? rngs) '(values)]
              [(null? (cdr rngs)) (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply build-compound-type-name 
              '->
              (append doms/c
                      (apply append (map list kwds kwds/c))
                      (list rng-name))))]))

(define-for-syntax (sort-keywords stx kwd/ctc-pairs)
  (define (insert x lst)
    (cond
      [(null? lst) (list x)]
      [else
       (let ([fst-kwd (syntax-e (car (car lst)))])
         #;(printf "comparing ~s to ~s\n" (car x) fst-kwd)
         (cond
           [(equal? (syntax-e (car x)) fst-kwd)
            (raise-syntax-error #f 
                                "duplicate keyword"
                                stx
                                (car x))]
           [(keyword<? (syntax-e (car x)) fst-kwd)
            (cons x lst)]
           [else (cons (car lst) (insert x (cdr lst)))]))]))
  
  (let loop ([pairs (map syntax->list kwd/ctc-pairs)])
    (cond
      [(null? pairs) null]
      [else (insert (car pairs) (loop (cdr pairs)))])))

(define-for-syntax (split-doms stx name raw-doms)
  (let loop ([raw-doms raw-doms]
             [doms '()]
             [kwd-doms '()])
    (syntax-case raw-doms ()
      [() (list (reverse doms)
                (sort-keywords stx kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (not (keyword? (syntax-e #'arg))))
       (loop #'rest
             doms
             (cons #'(kwd arg) kwd-doms))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (keyword? (syntax-e #'arg)))
       (raise-syntax-error name
                           "expected a keyword followed by a contract"
                           stx
                           #'kwd)]
      [(kwd)
       (keyword? (syntax-e #'kwd))
       (raise-syntax-error name
                           "expected a keyword to be followed by a contract"
                           stx
                           #'kwd)]
      [(x . rest)
       (loop #'rest (cons #'x doms) kwd-doms)])))

(define-for-syntax (->-helper stx)
  (syntax-case stx ()
    [(-> raw-doms ... last-one)
     (with-syntax ([((doms ...) ((dom-kwd dom-kwd-ctc) ...)) (split-doms stx '-> #'(raw-doms ...))])
       (with-syntax ([(dom-kwd-arg ...) (generate-temporaries (syntax (dom-kwd ...)))]
                     [(dom-kwd-ctc-id ...) (generate-temporaries (syntax (dom-kwd ...)))])
         (with-syntax ([(keyword-call/ctc ...) (apply append (map syntax->list (syntax->list #'((dom-kwd (dom-kwd-ctc-id dom-kwd-arg)) ...))))]
                       [(keyword-formal-parameters ...) (apply append (map syntax->list (syntax->list #'((dom-kwd dom-kwd-arg) ...))))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))])
           (syntax-case* #'last-one (-> any values) module-or-top-identifier=?
             [any
              (with-syntax ([(ignored) (generate-temporaries (syntax (rng)))])
                (values (syntax (dom-ctc ...))
                        (syntax (ignored))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (any/c))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) (val (dom-ctc args) ... keyword-call/ctc ...)))
                        #t))]
             [(values rngs ...)
              (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                            [(rng-ctc ...) (generate-temporaries (syntax (rngs ...)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc ...))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rngs ...))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) 
                                 (let-values ([(rng-x ...) (val (dom-ctc args) ... keyword-call/ctc ...)])
                                   (values (rng-ctc rng-x) ...))))
                        #f))]
             [rng
              (with-syntax ([(rng-ctc) (generate-temporaries (syntax (rng)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rng))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((args ... keyword-formal-parameters ...) (rng-ctc (val (dom-ctc args) ... keyword-call/ctc ...))))
                        #f))]))))]))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->/proc/main stx)
  (let-values ([(dom-names rng-names kwd-names dom-ctcs rng-ctcs kwd-ctcs kwds inner-args/body use-any?) (->-helper stx)])
    (with-syntax ([(args body) inner-args/body])
      (with-syntax ([(dom-names ...) dom-names]
                    [(rng-names ...) rng-names]
                    [(kwd-names ...) kwd-names]
                    [(dom-ctcs ...) dom-ctcs]
                    [(rng-ctcs ...) rng-ctcs]
                    [(kwd-ctcs ...) kwd-ctcs]
                    [(kwds ...) kwds]
                    [inner-lambda 
                     (add-name-prop
                      (syntax-local-infer-name stx)
                      (syntax (lambda args body)))]
                    [use-any? use-any?])
        (with-syntax ([outer-lambda
                       (syntax
                        (lambda (chk dom-names ... rng-names ... kwd-names ...)
                          (lambda (val)
                            (chk val)
                            inner-lambda)))])
          (values
           (syntax (build--> '->
                             (list dom-ctcs ...)
                             #f
                             (list rng-ctcs ...)
                             (list kwd-ctcs ...)
                             '(kwds ...)
                             use-any?
                             outer-lambda))
           inner-args/body
           (syntax (dom-names ... rng-names ...))))))))
  
(define-syntax (-> stx) 
  (let-values ([(stx _1 _2) (->/proc/main stx)])
    stx))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->*/proc/main stx)
  (syntax-case* stx (->* any) module-or-top-identifier=?
    [(->* (doms ...) any)
     (->/proc/main (syntax (-> doms ... any)))]
    [(->* (doms ...) (rngs ...))
     (->/proc/main (syntax (-> doms ... (values rngs ...))))]
    [(->* (raw-doms ...) rst rng)
     (with-syntax ([((doms ...) ((dom-kwd dom-kwd-ctc) ...)) (split-doms stx '-> #'(raw-doms ...))])
       (with-syntax ([(dom-kwd-arg ...) (generate-temporaries (syntax (dom-kwd ...)))]
                     [(dom-kwd-ctc-id ...) (generate-temporaries (syntax (dom-kwd ...)))])
         (with-syntax ([(keyword-formal-parameters ...) (apply append (map syntax->list (syntax->list #'((dom-kwd dom-kwd-arg) ...))))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))])
           (with-syntax ([(dom-x ...) (generate-temporaries (syntax (doms ...)))]
                         [(args ...) (generate-temporaries (syntax (doms ...)))]
                         [(rst-x) (generate-temporaries (syntax (rst)))]
                         [(rest-arg) (generate-temporaries (syntax (rst)))])
             (syntax-case #'rng (any)
               [(rngs ...)
                (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                              [(rng-args ...) (generate-temporaries (syntax (rngs ...)))])
                  
                  (let ([inner-args/body 
                         #`((args ... keyword-formal-parameters ... . rest-arg)
                            (let-values ([(rng-args ...)
                                          #,(if (null? (syntax-e #'(dom-kwd ...)))
                                                #'(apply val (dom-x args) ... (rst-x rest-arg))
                                                #'(keyword-apply val 
                                                                 '(dom-kwd ...)
                                                                 (list (dom-kwd-ctc-id dom-kwd-arg) ...) 
                                                                 (dom-x args) ...
                                                                 (rst-x rest-arg)))])
                              (values (rng-x rng-args) ...)))])
                    (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                                  (add-name-prop
                                                   (syntax-local-infer-name stx)
                                                   (syntax (lambda args body))))])
                      (with-syntax ([outer-lambda 
                                     (syntax
                                      (lambda (chk dom-x ... rst-x rng-x ... dom-kwd-ctc-id ...)
                                        (lambda (val)
                                          (chk val)
                                          inner-lambda)))])
                        (values (syntax (build--> '->*
                                                  (list doms ...)
                                                  rst
                                                  (list rngs ...)
                                                  (list dom-kwd-ctc ...)
                                                  '(dom-kwd ...)
                                                  #f 
                                                  outer-lambda))
                                inner-args/body
                                (syntax (dom-x ... rst-x rng-x ...)))))))]
               [any
                (let ([inner-args/body 
                       #`((args ... keyword-formal-parameters ... . rest-arg)
                          #,(if (null? (syntax-e #'(dom-kwd ...)))
                                #'(apply val (dom-x args) ... (rst-x rest-arg))
                                #'(keyword-apply val
                                                 '(dom-kwd ...) 
                                                 (list (dom-kwd-ctc-id dom-kwd-arg) ...)
                                                 (dom-x args) ...
                                                 (rst-x rest-arg))))])
                  (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                                (add-name-prop
                                                 (syntax-local-infer-name stx)
                                                 (syntax (lambda args body))))])
                    (with-syntax ([outer-lambda 
                                   (syntax
                                    (lambda (chk dom-x ... rst-x ignored dom-kwd-ctc-id ...)
                                      (lambda (val)
                                        (chk val)
                                        inner-lambda)))])
                      (values (syntax (build--> '->*
                                                (list doms ...)
                                                rst
                                                (list any/c)
                                                (list dom-kwd-ctc ...)
                                                '(dom-kwd ...)
                                                #t
                                                outer-lambda))
                              inner-args/body
                              (syntax (dom-x ... rst-x))))))])))))]))

(define-syntax (->* stx) 
  (let-values ([(stx _1 _2) (->*/proc/main stx)])
    stx))

(define-for-syntax (select/h stx err-name ctxt-stx)
  (syntax-case stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
    [(-> . args) ->/h]
    [(->* . args) ->*/h]
    [(->d . args) ->d/h]
    [(->d* . args) ->d*/h]
    [(->r . args) ->r/h]
    [(->pp . args) ->pp/h]
    [(->pp-rest . args) ->pp-rest/h]
    [(xxx . args) (raise-syntax-error err-name "unknown arrow constructor" ctxt-stx (syntax xxx))]
    [_ (raise-syntax-error err-name "malformed arrow clause" ctxt-stx stx)]))

(define-syntax (->d stx) (make-/proc #f ->d/h stx))
(define-syntax (->d* stx) (make-/proc #f ->d*/h stx))
(define-syntax (->r stx) (make-/proc #f ->r/h stx))
(define-syntax (->pp stx) (make-/proc #f ->pp/h stx))
(define-syntax (->pp-rest stx) (make-/proc #f ->pp-rest/h stx))
(define-syntax (case-> stx) (make-case->/proc #f stx stx select/h))
(define-syntax (opt-> stx) (make-opt->/proc #f stx select/h #'case-> #'->))
(define-syntax (opt->* stx) (make-opt->*/proc #f stx stx select/h #'case-> #'->))

;;
;; arrow opter
;;
(define/opter (-> opt/i opt/info stx)
  (define (opt/arrow-ctc doms rngs)
    (let*-values ([(dom-vars rng-vars) (values (generate-temporaries doms)
                                               (generate-temporaries rngs))]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                      (opt/i (opt/info-swap-blame opt/info) (car doms))])
                          (loop (cdr vars)
                                (cdr doms)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-doms)
                                (append lifts-doms lift)
                                (append superlifts-doms superlift)
                                (append partials-doms partial)
                                (append this-stronger-ribs stronger-ribs)))]))]
                  [(next-rngs lifts-rngs superlifts-rngs partials-rngs stronger-ribs-rng)
                   (let loop ([vars rng-vars]
                              [rngs rngs]
                              [next-rngs null]
                              [lifts-rngs null]
                              [superlifts-rngs null]
                              [partials-rngs null]
                              [stronger-ribs null])
                     (cond
                       [(null? rngs) (values (reverse next-rngs)
                                             lifts-rngs
                                             superlifts-rngs
                                             partials-rngs
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                      (opt/i opt/info (car rngs))])
                          (loop (cdr vars)
                                (cdr rngs)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-rngs)
                                (append lifts-rngs lift)
                                (append superlifts-rngs superlift)
                                (append partials-rngs partial)
                                (append this-stronger-ribs stronger-ribs)))]))])
      (values
       (with-syntax ((pos (opt/info-pos opt/info))
                     (src-info (opt/info-src-info opt/info))
                     (orig-str (opt/info-orig-str opt/info))
                     ((dom-arg ...) dom-vars)
                     ((rng-arg ...) rng-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars))
                     ((next-rng ...) next-rngs))
         (syntax (begin
                   (check-procedure val dom-len 0 '() '() #| keywords |# src-info pos orig-str)
                   (λ (dom-arg ...)
                     (let-values ([(rng-arg ...) (val next-dom ...)])
                       (values next-rng ...))))))
       (append lifts-doms lifts-rngs)
       (append superlifts-doms superlifts-rngs)
       (append partials-doms partials-rngs)
       #f
       #f
       (append stronger-ribs-dom stronger-ribs-rng))))
  
  (define (opt/arrow-any-ctc doms)
    (let*-values ([(dom-vars) (generate-temporaries doms)]
                  [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                   (let loop ([vars dom-vars]
                              [doms doms]
                              [next-doms null]
                              [lifts-doms null]
                              [superlifts-doms null]
                              [partials-doms null]
                              [stronger-ribs null])
                     (cond
                       [(null? doms) (values (reverse next-doms)
                                             lifts-doms
                                             superlifts-doms
                                             partials-doms
                                             stronger-ribs)]
                       [else
                        (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                      (opt/i (opt/info-swap-blame opt/info) (car doms))])
                          (loop (cdr vars)
                                (cdr doms)
                                (cons (with-syntax ((next next)
                                                    (car-vars (car vars)))
                                        (syntax (let ((val car-vars)) next)))
                                      next-doms)
                                (append lifts-doms lift)
                                (append superlifts-doms superlift)
                                (append partials-doms partial)
                                (append this-stronger-ribs stronger-ribs)))]))])
      (values
       (with-syntax ((pos (opt/info-pos opt/info))
                     (src-info (opt/info-src-info opt/info))
                     (orig-str (opt/info-orig-str opt/info))
                     ((dom-arg ...) dom-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars)))
         (syntax (begin
                   (check-procedure val dom-len 0 '() '() #|keywords|# src-info pos orig-str)
                   (λ (dom-arg ...)
                     (val next-dom ...)))))
       lifts-doms
       superlifts-doms
       partials-doms
       #f
       #f
       stronger-ribs-dom)))
  
  (syntax-case* stx (-> values any) module-or-top-identifier=?
    [(-> dom ... (values rng ...))
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (syntax->list (syntax (rng ...)))))]
    [(-> dom ... any)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-any-ctc (syntax->list (syntax (dom ...)))))]
    [(-> dom ... rng)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (list #'rng)))]))
