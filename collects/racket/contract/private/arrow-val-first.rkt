#lang racket/base

#|

TODO: find the places where the functions are constructed and called for plus1 and chaperone.
(to be able to add the pre/post conditions as arguments instead of inlining them into the wrappers)

plus1 call:  build->*-plus-one-acceptor
plus1 arg list construction: build-plus-one-arity-function/real

|#

(require (for-syntax racket/base
                     "application-arity-checking.rkt"
                     "arr-util.rkt")
         "kwd-info-struct.rkt"
         "blame.rkt"
         "misc.rkt"
         "prop.rkt"
         "guts.rkt"
         "generate.rkt"
         "arrow-common.rkt"
         "arrow-higher-order.rkt"
         "arrow-collapsible.rkt"
         "collapsible-common.rkt"
         "list.rkt"
         racket/stxparam)

(provide (rename-out [->/c ->]) ->*
         (for-syntax ->-internal ->*-internal) ; for ->m and ->*m
         base->? base->-name base->-rngs base->-doms
         dynamic->*
         arity-checking-wrapper
         (for-syntax ->-arity-check-only->?
                     ->*-arity-check-only->?
                     ->-valid-app-shapes
                     ->*-valid-app-shapes)
         (rename-out [-predicate/c predicate/c])
         build->*-plus-one-acceptor)

(begin-for-syntax
  (struct parsed->* (man-dom       ;; syntax?[(id ...)]
                     man-dom-kwds  ;; syntax?[((kwd id) ..)]
                     opt-dom       ;; syntax?[(id ...)]
                     opt-dom-kwds  ;; syntax?[((kwd id) ..)]
                     rest-ctc      ;; (or/c #f syntax?[id])
                     pre           ;; (or/c #f syntax?[id])
                     pre/desc      ;; (or/c #f syntax?[id])
                     rng-ctcs      ;; (or/c #f syntax?[(id ...)])
                     post          ;; (or/c #f syntax?[id])
                     post/desc     ;; (or/c #f syntax?[id])
                     lets)         ;; syntax?[([id expr] ...)]
    #:prefab))

(define-for-syntax (->-arity-check-only->? stx)
  (syntax-case stx (any any/c)
    [(_ any/c ... any) (- (length (syntax->list stx)) 2)]
    [_ #f]))

(define-for-syntax (->*-arity-check-only->? stx)
  (syntax-case stx (any any/c)
    [(_ (any/c ...) any) (length (syntax->list (cadr (syntax->list stx))))]
    [(_ (any/c ...) () any) (length (syntax->list (cadr (syntax->list stx))))]
    [_ #f]))

; Like call-with-values, but only for a receiver that is a case-lambda with a special pattern.
; It saves the results in temporal variables to avoid creating closures. 
(define-syntax (call-with-values/check-range stx)
  (syntax-protect
    (syntax-case stx (case-lambda)
      [(_
        thunk
        (case-lambda
          [(res-x ...) success ...]
          [failed/args failure ...]))
       (and (identifier? #'failed/args)
            (andmap identifier? (syntax-e #'(res-x ...))))
       (quasisyntax/loc
        stx
        (let ()
          (define-values (failed/args res-x ...)
          (call-with-values
            thunk
            (case-lambda
              [(res-x ...)
               (values #f res-x ...)]
              [failed/args
               (values failed/args #,@(map (λ (x) #'#f) 
                                           (syntax->list #'(res-x ...))))])))
          (cond
            [failed/args failure ...]
            [else success ...])))])))

(define-for-syntax popular-keys
  ;; the most popular contract shapes as of January 2016 from
  ;; the main distribution package; plus some that TR generates
  ;; for plot-gui-lib as of October 2017; as of July 2019, using
  ;; these popular keys appears to save about 10% of the disk
  ;; space taken by .zo files during the main-distribution build
  `((() 0 () () #f 1)
    (() 0 () () #f #f)
    ((#f) 0 () () #f 1)
    ((#f) 1 () () #f 1)
    ((#f) 0 () () #f #f)
    ((#f #f) 0 () () #f 1)
    ((#f #f) 0 () () #f #f)
    ((#f #f #f) 0 () () #f 1)
    ((#f #f #f) 0 () () #f #f)
    ((#f #f #f #f) 0 () () #f 1)
    ((#f #f #f #f) 0 () () #f #f)
    ((#f #f #f #f #f) 0 () () #f 1)
    ((#f #f #f #f #f) 0 () () #f #f)
    ((#f #f #f #f #f #f) 0 () () #f 1)
    ((#f #f #f #f #f #f) 0 () () #f #f)
    ((#f #f #f #f #f #f #f) 0 () () #f 1)
    ((#f #f #f #f #f #f #f #f) 0 () () #f 1)
    ;; 9 argument doesn't seem to show up
    ((#f #f #f #f #f #f #f #f #f #f) 0 () () #f 1)

    ;; multiple results or optional arguments below
    ((#f) 2 () () #f 1)
    ((#f) 3 () () #f 1)
    ((#f) 4 () () #f 1)
    ((#f) 0 () () #f 2)
    ((#f) 0 () () #f 4)
    ((#f) 0 () () #f 6)
    ((#f #f) 1 () () #f 1)
    ((#f #f) 2 () () #f 1)
    ((#f #f) 3 () () #f 1)
    ((#f #f) 3 () () #f 4)
    ((#f #f) 5 () () #f 1)
    ((#f #f #f) 0 () () #f 2)
    ((#f #f #f) 0 () () #f 4)
    ((#f #f #f) 1 () () #f 1)
    ((#f #f #f) 4 () () #f 1)
    ((#f #f #f #f) 1 () () #f 1)
    ((#f #f #f #f) 3 () () #f 1)
    ((#f #f #f #f #f) 1 () () #f 1)
    ((#f #f #f #f #f #f) 2 () () #f 1)
    ((#f #f #f #f #f #f #f) 1 () () #f 1)
    ((#f #f #f #f #f #f #f #f) 3 () () #f 1)))

(define-syntax (generate-popular-key-ids stx)
  (syntax-case stx ()
    [(_ popular-key-ids)
     #`(define-for-syntax popular-key-ids
         (list #,@(map (λ (x y) #`(list (quote-syntax #,x) (quote-syntax #,y)))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-plus-one-key-id))
                       (generate-temporaries (for/list ([e (in-list popular-keys)])
                                               'popular-chaperone-key-id)))))]))
(generate-popular-key-ids popular-key-ids)

(define-for-syntax (argument-details->popular-keys-table-entry/info
                    pre-regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc
                    method?)
  (define regular-args
    (if method?
        (cons #'any/c pre-regular-args) ; add `this` argument
        pre-regular-args))
  (define regular-args/no-any/c
    (for/list ([stx (in-list regular-args)])
      (syntax-case stx (any/c)
        [any/c #f]
        [else stx])))
  (define key
    (and (not pre) (not pre/desc)
         (not post) (not post/desc)
         (list (map not regular-args/no-any/c)
               (length optional-args)
               (map syntax-e mandatory-kwds)
               (map syntax-e optional-kwds)
               (and rest #t)
               (and rngs (if (syntax? rngs)
                             (length (syntax->list rngs))
                             (length rngs))))))
  (define entry-in-table (and key (member key popular-keys)))
  (define index (and entry-in-table
                     (- (length popular-keys) (length entry-in-table))))
  (values (and index (list-ref popular-key-ids index))
          regular-args/no-any/c
          regular-args))

(define-for-syntax (build-code-for-chaperone-constructor
                    a-parsed->*
                    method?)

  (define pre-regular-args (parsed->*-man-dom a-parsed->*))
  (define optional-args (parsed->*-opt-dom a-parsed->*))
  (define mandatory-kwds
    (with-syntax ([((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...)
                   (parsed->*-man-dom-kwds a-parsed->*)])
      (syntax->list #'(mandatory-dom-kwd ...))))
  (define optional-kwds
    (with-syntax ([((optional-dom-kwd optional-dom-kwd-ctc) ...)
                   (parsed->*-opt-dom-kwds a-parsed->*)])
      (syntax->list #'(optional-dom-kwd ...))))
  (define rest (parsed->*-rest-ctc a-parsed->*))
  (define rngs (parsed->*-rng-ctcs a-parsed->*))
  (define pre (parsed->*-pre a-parsed->*))
  (define pre/desc (parsed->*-pre/desc a-parsed->*))
  (define post (parsed->*-post a-parsed->*))
  (define post/desc (parsed->*-post/desc a-parsed->*))

  (define-values (ids regular-args/no-any/c regular-args)
    (argument-details->popular-keys-table-entry/info pre-regular-args
                                                     optional-args
                                                     mandatory-kwds
                                                     optional-kwds
                                                     pre pre/desc
                                                     rest
                                                     rngs
                                                     post post/desc
                                                     method?))
  (cond
    [ids (list-ref ids 1)]
    [else
     (build-chaperone-constructor/real
      regular-args/no-any/c
      optional-args
      mandatory-kwds
      optional-kwds
      pre pre/desc
      rest
      rngs
      post post/desc
      method?)]))

(define-for-syntax (build-code-for-plus-one-arity-function
                    a-parsed->*
                    method?)

  (define pre-regular-args (parsed->*-man-dom a-parsed->*))
  (define optional-args (parsed->*-opt-dom a-parsed->*))
  (define mandatory-kwds
    (with-syntax ([((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...)
                   (parsed->*-man-dom-kwds a-parsed->*)])
      (syntax->list #'(mandatory-dom-kwd ...))))
  (define optional-kwds
    (with-syntax ([((optional-dom-kwd optional-dom-kwd-ctc) ...)
                   (parsed->*-opt-dom-kwds a-parsed->*)])
      (syntax->list #'(optional-dom-kwd ...))))
  (define rest (parsed->*-rest-ctc a-parsed->*))
  (define rngs (parsed->*-rng-ctcs a-parsed->*))

  (define pre (parsed->*-pre a-parsed->*))
  (define pre/desc (parsed->*-pre/desc a-parsed->*))
  (define post (parsed->*-post a-parsed->*))
  (define post/desc (parsed->*-post/desc a-parsed->*))

  (define-values (ids regular-args/no-any/c regular-args)
    (argument-details->popular-keys-table-entry/info pre-regular-args
                                                     optional-args
                                                     mandatory-kwds
                                                     optional-kwds
                                                     pre pre/desc
                                                     rest
                                                     rngs
                                                     post post/desc
                                                     method?))
  (cond
    [ids (list-ref ids 0)]
    [else
     (build-plus-one-arity-function/real
      regular-args
      optional-args
      mandatory-kwds
      optional-kwds
      pre pre/desc
      rest
      rngs
      post post/desc
      method?)]))

(define-syntax (build-populars stx)
  (syntax-case stx ()
    [(_ popular-chaperone-key-table)
     #`(begin
         #,@(for/list ([ids (in-list popular-key-ids)]
                       [key (in-list popular-keys)])
              (define plus-one-id (list-ref ids 0))
              (define chaperone-id (list-ref ids 1))
              (define-values (regular-arg-any/c-or-not?s
                              optional-arg-count
                              mandatory-kwds
                              optional-kwds
                              rest
                              rngs)
                (apply values key))
              (define mans (for/list ([is-any/c? (in-list regular-arg-any/c-or-not?s)]
                                      [x (in-naturals)])
                             (string->symbol (format "man~a" x))))
              (define mans/no-any/c
                (for/list ([is-any/c? (in-list regular-arg-any/c-or-not?s)]
                           [man-var (in-list mans)])
                  (if is-any/c? #f man-var)))
              (define opts (for/list ([x (in-range optional-arg-count)])
                             (string->symbol (format "opt~a" x))))
              (define rng-vars (and rngs (for/list ([x (in-range rngs)])
                                           (string->symbol (format "rng~a" x)))))
              #`(begin
                  (define #,(syntax-local-introduce plus-one-id)
                    #,(build-plus-one-arity-function/real
                       mans opts
                       mandatory-kwds
                       optional-kwds
                       #f #f
                       rest
                       rng-vars
                       #f #f #f))
                  (define #,(syntax-local-introduce chaperone-id)
                    #,(build-chaperone-constructor/real
                       mans/no-any/c opts
                       mandatory-kwds
                       optional-kwds
                       #f #f
                       rest
                       rng-vars
                       #f #f #f))))
         (define popular-chaperone-key-table
           (make-hash
            (list #,@(for/list ([id (in-list popular-key-ids)]
                                [key (in-list popular-keys)])
                       #`(cons '#,key #,(list-ref id 1)))))))]))

(define-for-syntax (build-plus-one-arity-function/real
                    regular-args
                    optional-args
                    mandatory-kwds
                    optional-kwds
                    pre pre/desc
                    rest
                    rngs
                    post post/desc
                    method?)
  (with-syntax ([(regb ...) (generate-temporaries regular-args)]
                [(optb ...) (generate-temporaries optional-args)]
                [(kb ...) (generate-temporaries mandatory-kwds)]
                [(okb ...) (generate-temporaries optional-kwds)]
                [(rb ...) (generate-temporaries (or rngs '()))]
                [(arg-x ...) (generate-temporaries regular-args)]
                [(res-x ...) (generate-temporaries (or rngs '()))]
                [(kwd-arg-x ...) (generate-temporaries mandatory-kwds)])

    (define base-arg-expressions (reverse (syntax->list #'((regb arg-x neg-party) ...))))
    (define normal-arg-vars (generate-temporaries #'(arg-x ...)))
    (define base-arg-vars normal-arg-vars)

    (with-syntax ([(formal-kwd-args ...)
                   (apply append (map list mandatory-kwds (syntax->list #'(kwd-arg-x ...))))]
                  [(kwd-arg-exps ...)
                   (apply
                    append
                    (map (λ (kwd kwd-arg-x kb)
                           (set! base-arg-expressions
                                 (cons #`(#,kb #,kwd-arg-x neg-party)
                                       base-arg-expressions))
                           (set! base-arg-vars (cons (car (generate-temporaries (list kwd-arg-x)))
                                                     base-arg-vars))
                           (list kwd (car base-arg-vars)))
                         mandatory-kwds
                         (syntax->list #'(kwd-arg-x ...))
                         (syntax->list #'(kb ...))))]
                  [(letrec-bound-id) (generate-temporaries '(f))])
      
      (with-syntax ([(wrapper-args ...) #'(neg-party arg-x ... formal-kwd-args ...)]
                    [(the-call ...) #`(f #,@(reverse normal-arg-vars) kwd-arg-exps ...)]
                    [(pre-check ...)
                     (cond
                       [pre 
                        (list #`(check-pre-cond #,pre blame+neg-party f))]
                       [pre/desc
                        (list #`(check-pre-cond/desc #,pre/desc blame+neg-party f))]
                       [else (list)])]
                    [(post-check ...)
                     (cond
                       [post
                        (list #`(check-post-cond #,post blame+neg-party f))]
                       [post/desc
                        (list #`(check-post-cond/desc #,post/desc blame+neg-party f))]
                       [else (list)])]
                    [(restb) (generate-temporaries '(rest-args))])
        (define (make-body-proc range-checking?)
          (cond
            [(or (and (null? optional-args)
                      (null? optional-kwds))
                 (and (null? mandatory-kwds)
                      (null? optional-kwds)))
             (define case-lambda-clauses
               (let loop ([optional-args (reverse optional-args)]
                          [ob (reverse (syntax->list #'(optb ...)))]
                          [first? #t])
                 (define args-expressions base-arg-expressions)
                 (define args-vars base-arg-vars)
                 (define no-rest-call
                   #`(the-call ...
                      #,@(for/list ([ob (in-list (reverse ob))]
                                    [optional-arg (in-list (reverse optional-args))])
                           (set! args-expressions
                                 (cons #`(#,ob #,optional-arg neg-party)
                                       args-expressions))
                           (set! args-vars
                                 (cons (car (generate-temporaries (list optional-arg)))
                                       args-vars))
                           (car args-vars))))
                 (define full-call
                   (cond
                     [(and first? rest)
                      (set! args-expressions (cons #'(restb rest-arg neg-party) args-expressions))
                      (set! args-vars (cons (car (generate-temporaries '(rest-args-arrow-contract)))
                                            args-vars))
                      #`(apply #,@no-rest-call #,(car args-vars))]
                     [else
                      no-rest-call]))
                 (define the-args #`(wrapper-args ... 
                                     #,@(reverse optional-args)
                                     #,@(if (and first? rest)
                                            #'rest-arg
                                            '())))
                 (define let-values-clause
                   #`[#,(reverse args-vars)
                      (with-contract-continuation-mark
                       blame+neg-party
                       (values #,@(reverse args-expressions)))])
                 
                 (define the-clause
                   (if rngs
                       #`[#,the-args
                          (let ([blame+neg-party (cons blame neg-party)])
                            pre-check ...
                            #,
                            (cond
                              [range-checking?
                               #`(call-with-values/check-range
                                  (λ () (let-values (#,let-values-clause)
                                          #,full-call))
                                  (case-lambda
                                    [(res-x ...)
                                     (with-contract-continuation-mark
                                      blame+neg-party
                                      post-check ...
                                      (values
                                       (rb res-x neg-party)
                                       ...))]
                                    [args
                                     (with-contract-continuation-mark
                                      blame+neg-party
                                      (wrong-number-of-results-blame
                                       blame neg-party f
                                       args
                                       #,(length (syntax->list #'(res-x ...)))))]))]
                              [else
                               #`(begin
                                  (define-values (res-x ...)
                                    (let-values (#,let-values-clause)
                                      #,full-call))
                                  (with-contract-continuation-mark
                                   blame+neg-party
                                   (begin
                                     post-check ...
                                     (values
                                      (rb res-x neg-party)
                                      ...))))]))]
                       #`[#,the-args
                          (let ([blame+neg-party (cons blame neg-party)])
                            pre-check ...
                            (let-values (#,let-values-clause)
                              #,full-call))]))
                 (cons the-clause
                       (cond
                         [(null? optional-args) '()]
                         [else (loop (cdr optional-args)
                                     (cdr ob)
                                     #f)]))))
             (cond
               [(null? (cdr case-lambda-clauses))
                ;; need to specialize this case because
                ;; there might be keyword arguments here
                #`(λ #,@(car case-lambda-clauses))]
               [else
                ;; (but there won't here)
                #`(case-lambda #,@case-lambda-clauses)])]
            [else
             #`(make-checking-proc f blame
                                   #,(if pre pre #'#f)
                                   #,(if pre/desc pre/desc #'#f)
                                   '(#,@mandatory-kwds) (list kb ...)
                                   '(#,@optional-kwds) (list okb ...)
                                   #,(length regular-args) (list regb ... optb ...)
                                   #,(if rest #'restb #'#f)
                                   #,(if post post #'#f)
                                   #,(if post/desc post/desc #'#f)
                                   #,(if rngs #'(list rb ...) #'#f)
                                   #,method?)]))
        (define number-of-rngs (and rngs (with-syntax ([rngs rngs]) (length (syntax->list #'rngs)))))
        #`(λ (f)
            (λ (blame regb ... optb ... kb ... okb ...
                      #,@(if pre (list pre) '())
                      #,@(if pre/desc (list pre/desc) '())
                      #,@(if rest (list #'restb) '())
                      rb ...
                      #,@(if post (list post) '())
                      #,@(if post/desc (list post/desc) '()))
              (procedure-specialize
               #,(if rngs
                     #`(if (equal? #,number-of-rngs (procedure-result-arity f))
                           #,(make-body-proc #f)
                           #,(make-body-proc #t))
                     (make-body-proc #t)))))))))

(define (build->*-plus-one-acceptor plus-one-arity-wrapper-maker
                                    blame
                                    ->stct)
  (define-values (partial-doms
                  partial-rests
                  man-then-opt-partial-kwds
                  partial-ranges
                  c-c-doms
                  maybe-c-c-ranges)
    (build-subcontract-late-negs blame
                                 (base->-doms ->stct)
                                 (base->-rest ->stct)
                                 (base->-rngs ->stct)
                                 (base->-kwd-infos ->stct)
                                 #f))
  (define plus-one-constructor-args
    (append partial-doms
            man-then-opt-partial-kwds
            partial-rests
            (if (base->-pre-thunk ->stct)
                (list (base->-pre-thunk ->stct))
                '())
            partial-ranges
            (if (base->-post-thunk ->stct)
                (list (base->-post-thunk ->stct))
                '())))
  (apply plus-one-arity-wrapper-maker
         blame
         plus-one-constructor-args))

(define (make-checking-proc f blame pre pre/desc
                            original-mandatory-kwds kbs
                            original-optional-kwds okbs
                            minimum-arg-count rbs rest-ctc
                            post post/desc rngs
                            method?)
  (make-keyword-procedure
   (λ (actual-kwds actual-kwd-args neg-party . regular-args)
     (check-arg-count minimum-arg-count (length rbs) regular-args f blame neg-party rest-ctc method?)
     (check-keywords original-mandatory-kwds original-optional-kwds actual-kwds f blame neg-party)
     (define (mk-call)
       (keyword-apply
        f
        actual-kwds
        (let loop ([kwds actual-kwds]
                   [kwd-args actual-kwd-args]
                   [mandatory-kwds original-mandatory-kwds]
                   [optional-kwds original-optional-kwds]
                   [kbs kbs]
                   [okbs okbs])
          (cond
            [(null? kwd-args) '()]
            [else
             (define kwd (car kwds))
             (define kwd-arg (car kwd-args))
             (cond
               [(and (pair? mandatory-kwds)
                     (equal? (car mandatory-kwds) kwd))
                (cons ((car kbs) kwd-arg neg-party)
                      (loop (cdr kwds) 
                            (cdr kwd-args)
                            (cdr mandatory-kwds)
                            optional-kwds
                            (cdr kbs)
                            okbs))]
               [(and (pair? optional-kwds)
                     (equal? (car optional-kwds) kwd))
                (cons ((car okbs) kwd-arg neg-party)
                      (loop (cdr kwds) 
                            (cdr kwd-args)
                            mandatory-kwds
                            (cdr optional-kwds)
                            kbs
                            (cdr okbs)))]
               [(pair? optional-kwds)
                (loop kwds kwd-args mandatory-kwds (cdr optional-kwds) kbs (cdr okbs))]
               [else
                (error 'arrow-val-first.rkt
                       (string-append
                        "internal error:\n  f ~s\n  actual-kwds ~s"
                        "\n  mandatory-kwds ~s\n  optional-kwds ~s\n  neg-party ~s")
                       f actual-kwds original-mandatory-kwds original-optional-kwds neg-party)])]))
        (let loop ([regular-args regular-args]
                   [rbs rbs])
          (cond
            [(null? regular-args) '()]
            [(null? rbs) (rest-ctc regular-args neg-party)]
            [else
             (cons ((car rbs) (car regular-args) neg-party)
                   (loop (cdr regular-args) (cdr rbs)))]))))
     (define blame+neg-party (cons blame neg-party))
     (when pre (check-pre-cond pre blame+neg-party f))
     (when pre/desc (check-pre-cond/desc pre blame+neg-party f))
     (cond
       [rngs
        (define results (call-with-values mk-call list))
        (define rng-len (length rngs))
        (unless (= (length results) rng-len)
          (bad-number-of-results (blame-add-missing-party blame neg-party)
                                 f rng-len results))
        (when post (check-post-cond post blame+neg-party f))
        (when post/desc (check-post-cond post/desc blame+neg-party f))
        (apply
         values
         (for/list ([result (in-list results)]
                    [rng (in-list rngs)])
           (rng result neg-party)))]
       [else
        (mk-call)]))))

(build-populars popular-chaperone-key-table)
(define (lookup-popular-chaperone-key regular-arg-any/c-or-not?s
                                      optional-arg-count
                                      mandatory-kwds
                                      optional-kwds
                                      rest
                                      rngs)
  (define key (list regular-arg-any/c-or-not?s
                    optional-arg-count
                    mandatory-kwds
                    optional-kwds
                    rest
                    rngs))
  (hash-ref popular-chaperone-key-table key #f))

(define (check-arg-count minimum-arg-count len-rbs regular-args val blame neg-party rest-ctc method?)
  (define actual-count (length regular-args))
  (define adjust (if method? sub1 values))
  (cond
    [(< actual-count minimum-arg-count)
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= len-rbs minimum-arg-count)
                            ""
                            "at least ")
                        (adjust minimum-arg-count))]
    [(and (not rest-ctc) (< len-rbs actual-count))
     (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                        '(expected: "~a~a arguments")
                        (if (= len-rbs minimum-arg-count)
                            ""
                            "at most ")
                        (adjust len-rbs))]))

(define (check-keywords mandatory-kwds optional-kwds kwds val blame neg-party)
  (let loop ([mandatory-kwds mandatory-kwds]
             [optional-kwds optional-kwds]
             [kwds kwds])
    (cond
      [(null? kwds) 
       (unless (null? mandatory-kwds)
         (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                            '(expected: "keyword argument ~a")
                            (car mandatory-kwds)))]
      [else
       (define kwd (car kwds))
       (cond
         [(and (null? optional-kwds) (null? mandatory-kwds))
          (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                             '(expected: "no keyword argument ~a")
                             kwd)]
         [(and (pair? mandatory-kwds)
               (or (null? optional-kwds)
                   (keyword<? (car mandatory-kwds) (car optional-kwds))))
          (define man-kwd (car mandatory-kwds))
          (cond
            [(equal? kwd man-kwd)
             (loop (cdr mandatory-kwds) optional-kwds (cdr kwds))]
            [(keyword<? kwd man-kwd) 
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "no keyword argument ~a")
                                kwd)]
            [(keyword<? man-kwd kwd) 
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "keyword argument ~a")
                                man-kwd)])]
         [(and (pair? optional-kwds)
               (or (null? mandatory-kwds)
                   (keyword<? (car optional-kwds) (car mandatory-kwds))))
          (define opt-kwd (car optional-kwds))
          (cond
            [(equal? kwd opt-kwd)
             (loop mandatory-kwds (cdr optional-kwds) (cdr kwds))]
            [(keyword<? kwd opt-kwd)
             (raise-blame-error (blame-swap blame) #:missing-party neg-party val
                                '(expected: "no keyword argument ~a")
                                kwd)]
            [(keyword<? opt-kwd kwd)
             (loop mandatory-kwds (cdr optional-kwds) kwds)])])])))

(define-for-syntax (parse-> stx this->)
  (syntax-case stx ()
    [(_ args ... rng)
     (let ()
       (define-values (regular-args kwds kwd-args let-bindings ellipsis-info)
         (parse-arrow-args stx (syntax->list #'(args ...)) this->))
       (define (add-pos-obligations stxes)
         (for/list ([stx (in-list stxes)])
           (syntax-property stx 'racket/contract:positive-position this->)))
       (define rngs
         (syntax-case #'rng (any values)
           [any #f]
           [(values rng ...) (add-pos-obligations (syntax->list #'(rng ...)))]
           [rng (add-pos-obligations (list #'rng))]))
       (values regular-args kwds kwd-args let-bindings ellipsis-info rngs))]))

(define-for-syntax (parse-arrow-args stx args this->)
  (let loop ([args args]
             [regular-args '()]
             [kwds '()]
             [kwd-args '()]
             [let-bindings '()]
             [ellipsis #f])
    (cond
      [(null? args) 
       (define sorted
         (sort (map cons kwds kwd-args)
               keyword<?
               #:key (compose syntax-e car)))
       (when (pair? sorted)
         (for ([pr1 (in-list sorted)]
               [pr2 (in-list (cdr sorted))])
           (when (equal? (syntax-e (car pr1)) (syntax-e (car pr2)))
             (raise-syntax-error #f "duplicate keyword" stx
                                 (car pr1)
                                 (list (car pr2))))))
       (values (reverse regular-args)
               (map car sorted)
               (map cdr sorted)
               (reverse let-bindings)
               #f)]
      [else
       (cond
         [(and (identifier? (car args)) (free-identifier=? (car args) #'(... ...)))
          (when ellipsis
            (raise-syntax-error '-> "expected at most one ellipsis"
                                stx (car args) ellipsis))
          (when (null? regular-args)
            (raise-syntax-error '->
                                "expected the ellipsis to follow a contract"
                                stx
                                (car args)))
          (for ([arg (in-list (cdr args))])
            (when (keyword? (syntax-e arg))
              (raise-syntax-error '->
                                  "keywords are not allowed after the ellipsis"
                                  stx arg)))
          (define sorted
            (sort (map cons kwds kwd-args)
                  keyword<?
                  #:key (compose syntax-e car)))
          (define arg-xes (generate-temporaries (cdr args)))
          (values (reverse (cdr regular-args))
                  (map car sorted)
                  (map cdr sorted)
                  (append (reverse let-bindings)
                          (for/list ([arg-exp (cdr args)]
                                     [arg-x (in-list arg-xes)])
                            #`[#,arg-x #,(syntax-property
                                          (syntax-property arg-exp 'inferred-name (void))
                                          'racket/contract:negative-position
                                          this->)]))
                  (cons (car regular-args) arg-xes))]
         [(keyword? (syntax-e (car args)))
          (when (null? (cdr args))
            (raise-syntax-error '-> 
                                "expected a contract to follow the keyword (plus the range)"
                                stx
                                (car args)))
          (when (and (identifier? (cadr args))
                     (free-identifier=? (cadr args) #'(... ...)))
            (raise-syntax-error '->
                                "expected a contract to follow a keyword, not an ellipsis"
                                stx
                                (car args)))
          (with-syntax ([(arg-x) (generate-temporaries (list (car args)))])
            (loop (cddr args)
                  regular-args
                  (cons (car args) kwds)
                  (cons #'arg-x kwd-args)
                  (cons #`[arg-x #,(syntax-property (syntax-property
                                                     (cadr args)
                                                     'inferred-name (void))
                                                    'racket/contract:negative-position 
                                                    this->)]
                        let-bindings)
                  ellipsis))]
         [else
          (with-syntax ([(arg-x) (generate-temporaries (list (car args)))])
            (loop (cdr args)
                  (cons #'arg-x regular-args)
                  kwds
                  kwd-args
                  (cons #`[arg-x #,(syntax-property (syntax-property
                                                     (car args)
                                                     'inferred-name (void))
                                                    'racket/contract:negative-position 
                                                    this->)]
                        let-bindings)
                  ellipsis))])])))

(define-for-syntax (->-valid-app-shapes stx)
  (syntax-case stx ()
    [(_ args ... rng)
     (let ()
       (define-values (regular-args kwds kwd-args let-bindings ellipsis-info rngs)
         (parse-> stx (gensym 'this->)))
       (define arg-count (length regular-args))
       (define app-shapes
         (valid-app-shapes (if ellipsis-info
                               (+ arg-count (length ellipsis-info) -1)
                               (list arg-count))
                           (map syntax->datum kwds)
                           '()))
       (values app-shapes
               (build-code-for-plus-one-arity-function
                (with-syntax ([(kwds ...) kwds]
                              [(kwd-args ...) kwd-args])
                  (parsed->* regular-args #'((kwds kwd-args) ...)
                             '() '()
                             (and ellipsis-info #t)
                             #f #f rngs #f #f '()))
                #f)))]))

(define-syntax (->/c stx)
  (syntax-case stx ()
    [(_ . args)
     (->-internal (syntax/loc stx (-> . args)) #|method?|# #f)]))

(define-for-syntax (->-internal stx method?)
  (syntax-case stx ()
    [(_ args ... rng)
     (let ()
       (define this-> (gensym 'this->))
       (define-values (regular-args kwds kwd-args let-bindings ellipsis-info rngs)
         (parse-> stx this->))
       (define chaperone-constructor
         (build-code-for-chaperone-constructor
          (with-syntax ([(kwds ...) kwds]
                        [(kwd-args ...) kwd-args])
            (parsed->* regular-args #'((kwds kwd-args) ...)
                       '() '()
                       (and ellipsis-info #t) #f #f rngs #f #f
                       '()))
          method?))
       (syntax-property
        #`(let #,let-bindings
            #,(cond
                [(and (not method?)
                      (null? kwd-args)
                      (not ellipsis-info))
                 (define rng-count (and rngs (length rngs)))
                 (define doms-count (length regular-args))
                 (cond
                   [(and (equal? rng-count 1) (= doms-count 0))
                    (quasisyntax/loc stx
                      (build-nullary-very-simple-->
                       #,(car rngs)
                       #,chaperone-constructor))]
                   [(and (equal? rng-count 1) (= doms-count 1))
                    (quasisyntax/loc stx
                      (build-unary-very-simple-->
                       #,(car regular-args)
                       #,(car rngs)
                       #,chaperone-constructor))]
                   [else
                    (quasisyntax/loc stx
                      (build-very-simple-->
                       (list #,@regular-args)
                       #,(if rngs
                             #`(list #,@rngs)
                             #'#f)
                       #,chaperone-constructor))])]
                [else
                 (quasisyntax/loc stx
                   (build-simple-->
                    (list #,@regular-args)
                    '(#,@kwds)
                    (list #,@kwd-args)
                    #,(if rngs
                          #`(list #,@rngs)
                          #'#f)
                    #,chaperone-constructor
                    #,(if ellipsis-info
                          #`(ellipsis-rest-arg #,(length regular-args) #,@ellipsis-info)
                          #'#f)
                    #,method?))]))
        'racket/contract:contract
        (vector this->
                ;; the -> in the original input to this guy
                (list (car (syntax-e stx)))
                '())))]))

;; not quite the same as split-doms in arr-util.rkt, but similar idea.
(define-for-syntax (:split-doms stx name raw-doms this->*)
  (let loop ([raw-doms raw-doms]
             [doms '()]
             [kwd-doms '()]
             [let-bindings '()])
    (syntax-case raw-doms ()
      [() (list (reverse doms)
                (sort-keywords stx kwd-doms)
                (reverse let-bindings))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (not (keyword? (syntax-e #'arg))))
       (with-syntax ([(x) (generate-temporaries #'(kwd))])
         (loop #'rest
               doms
               (cons #'(kwd x) kwd-doms)
               (cons #`[x #,(syntax-property (syntax-property #'arg 'inferred-name (void))
                                             'racket/contract:negative-position 
                                             this->*)]
                     let-bindings)))]
      [(kwd arg . rest)
       (and (keyword? (syntax-e #'kwd))
            (keyword? (syntax-e #'arg)))
       (raise-syntax-error name
                           "cannot have two keywords in a row"
                           stx
                           #'kwd)]
      [(kwd)
       (keyword? (syntax-e #'kwd))
       (raise-syntax-error name
                           "cannot have a keyword at the end"
                           stx
                           #'kwd)]
      [(x . rest)
       (with-syntax ([(t) (generate-temporaries #'(x))])
         (loop #'rest 
               (cons #'t doms) 
               kwd-doms
               (cons #`[t #,(syntax-property (syntax-property #'x 'inferred-name (void))
                                             'racket/contract:negative-position 
                                             this->*)]
                     let-bindings)))])))

(define-for-syntax (parse->* stx this->*)
  (syntax-case stx ()
    [(_ (raw-mandatory-dom ...) . other)
     (let ()
       (define-values (raw-optional-doms rest-ctc pre pre/desc rng-ctcs post post/desc
                                         additional-lets)
         (parse-leftover->* stx #'other))
       (with-syntax ([(man-dom
                       man-dom-kwds
                       (man-lets ...))
                      (:split-doms stx '->* #'(raw-mandatory-dom ...) this->*)]
                     [(opt-dom
                       opt-dom-kwds
                       (opt-lets ...))
                      (:split-doms stx '->* raw-optional-doms this->*)]
                     [(additional-lets ...) additional-lets])
         ;; call sort-keywords for the duplicate variable check
         (sort-keywords stx (append (syntax->list #'man-dom-kwds) (syntax->list #'opt-dom-kwds)))
         (parsed->* (syntax->list #'man-dom)
                    #'man-dom-kwds
                    (syntax->list #'opt-dom)
                    #'opt-dom-kwds
                    rest-ctc pre pre/desc rng-ctcs post post/desc
                    #'(man-lets ... opt-lets ... additional-lets ...))))]))

;; -> (values raw-optional-doms rest-ctc pre rng-ctc post)
;; rest-ctc (or/c #f syntax) -- #f means no rest contract, syntax is the contract
;; rng-ctc (or/c #f syntax) -- #f means `any', syntax is a sequence of result values
(define-for-syntax (parse-leftover->* stx leftover)
  (define additional-lets '())
  (let*-values ([(raw-optional-doms leftover)
                 (syntax-case leftover ()
                   [(kwd . rst)
                    (keyword? (syntax-e #'kwd))
                    (values #'() leftover)]
                   [(rng #:post . rst)
                    (values #'() leftover)]
                   [(rng #:post/desc . rst)
                    (values #'() leftover)]
                   [(rng)
                    (values #'() leftover)]
                   [((raw-optional-dom ...) . leftover)
                    (values #'(raw-optional-dom ...) #'leftover)]
                   [_ 
                    (values #'() leftover)])]
                [(rst leftover)
                 (syntax-case leftover ()
                   [(#:rest rest-expr one-thing another-thing)
                    (and (not (keyword? #'one-thing))
                         (not (keyword? #'another-thing)))
                    (raise-syntax-error
                     #f
                     (string-append
                      "expected the #:rest keyword to be followed only by the range"
                      " (possibly with pre- and post-conditions)")
                     stx #'another-thing)]
                   [(#:rest rest-expr . leftover)
                    (with-syntax ([(rest-x) (generate-temporaries #'(rest-expr))])
                      (set! additional-lets (cons #'[rest-x rest-expr] additional-lets))
                      (values #'rest-x #'leftover))]
                   [_ (values #f leftover)])]
                [(pre pre/desc leftover)
                 (syntax-case leftover ()
                   [(#:pre pre-expr . leftover)
                    (with-syntax ([(pre-x) (generate-temporaries #'(pre-expr))])
                      (set! additional-lets (cons #`[pre-x (λ () pre-expr)] additional-lets))
                      (values #'pre-x #f #'leftover))]
                   [(#:pre/desc pre-expr . leftover)
                    (with-syntax ([(pre-x) (generate-temporaries #'(pre-expr))])
                      (set! additional-lets (cons #`[pre-x (λ () pre-expr)] additional-lets))
                      (values #f #'pre-x #'leftover))]
                   [_ (values #f #f leftover)])]
                [(rng leftover)
                 (syntax-case leftover (any values)
                   [(any) (values #f #'())]
                   [(any . more) (raise-syntax-error #f "expected nothing to follow any" stx #'any)]
                   [((values ctc ...) . leftover)
                    (values #'(ctc ...) #'leftover)]
                   [(rng . leftover)
                    (begin
                      (when (keyword? (syntax-e #'rng))
                        (raise-syntax-error #f "expected a range contract" stx #'rng))
                      (values #'(rng) #'leftover))]
                   [_
                    (raise-syntax-error #f "expected a range contract" stx leftover)])]
                [(post post/desc leftover)
                 (syntax-case leftover ()
                   [(#:post post-expr . leftover)
                    (with-syntax ([(post-x) (generate-temporaries #'(post-expr))])
                      (set! additional-lets (cons #`[post-x (λ () post-expr)] additional-lets))
                      (values #'post-x #f #'leftover))]
                   [(#:post/desc post-expr . leftover)
                    (with-syntax ([(post-x) (generate-temporaries #'(post-expr))])
                      (set! additional-lets (cons #`[post-x (λ () post-expr)] additional-lets))
                      (values #f #'post-x #'leftover))]
                   [else
                    (values #f #f leftover)])])
    (syntax-case leftover ()
      [() (values raw-optional-doms rst pre pre/desc rng post post/desc
                  (reverse additional-lets))]
      [(x . y) (raise-syntax-error #f "expected the contract to end, but found an extra sub-piece"
                                   stx #'x)])))

(define-for-syntax (->*-valid-app-shapes stx)
  (define this->* (gensym 'this->*))
  (define a-parsed->* (parse->* stx this->*))
  (with-syntax ([((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...)
                 (parsed->*-man-dom-kwds a-parsed->*)]
                [((optional-dom-kwd optional-dom-kwd-ctc) ...)
                 (parsed->*-opt-dom-kwds a-parsed->*)])
    (values (valid-app-shapes-from-man/opts (length (parsed->*-man-dom a-parsed->*))
                                            (length (parsed->*-opt-dom a-parsed->*))
                                            (parsed->*-rest-ctc a-parsed->*)
                                            (syntax->datum #'(mandatory-dom-kwd ...))
                                            (syntax->datum #'(optional-dom-kwd ...)))
            (build-code-for-plus-one-arity-function
             a-parsed->*
             #f))))

(define-syntax (->* stx)
  (syntax-case stx ()
    [(_ . args)
     (->*-internal (syntax/loc stx (->* . args)) #|method?|# #f)]))

(define-for-syntax (->*-internal stx method?)
  (define this->* (gensym 'this->*))
  (define a-parsed->* (parse->* stx this->*))
  (with-syntax ([((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...) (parsed->*-man-dom-kwds a-parsed->*)]
                [((optional-dom-kwd optional-dom-kwd-ctc) ...) (parsed->*-opt-dom-kwds a-parsed->*)]
                [(let-bindings ...) (parsed->*-lets a-parsed->*)])
    (define pre (parsed->*-pre a-parsed->*))
    (define pre/desc (parsed->*-pre/desc a-parsed->*))
    (define post (parsed->*-post a-parsed->*))
    (define post/desc (parsed->*-post/desc a-parsed->*))
    (define rest-ctc (parsed->*-rest-ctc a-parsed->*))
    (define rng-ctcs (parsed->*-rng-ctcs a-parsed->*))
    (define chaperone-constructor (build-code-for-chaperone-constructor a-parsed->* method?))
    (syntax-property
     #`(let (let-bindings ...)
         (build--> '->*
                   (list #,@(parsed->*-man-dom a-parsed->*))
                   (list #,@(parsed->*-opt-dom a-parsed->*))
                   '(mandatory-dom-kwd ...)
                   (list mandatory-dom-kwd-ctc ...)
                   '(optional-dom-kwd ...)
                   (list optional-dom-kwd-ctc ...)
                   #,rest-ctc
                   #,(cond [pre #''pre] [pre/desc #''pre/desc] [else #'#f])
                   #,(or pre pre/desc #'#f)
                   #,(if rng-ctcs
                         #`(list #,@(for/list ([rng-ctc (in-list (syntax->list rng-ctcs))])
                                      (syntax-property rng-ctc
                                                       'racket/contract:positive-position
                                                       this->*)))
                         #'#f)
                   #,(cond [post #''post] [post/desc #''post/desc] [else #'#f])
                   #,(or post post/desc #'#f)
                   #,chaperone-constructor
                   #,method?))

     'racket/contract:contract
     (vector this->*
             ;; the -> in the original input to this guy
             (list (car (syntax-e stx)))
             '()))))

(define (wrong-number-of-results-blame blame neg-party val reses expected-values)
  (define length-reses (length reses))
  (raise-blame-error 
   blame #:missing-party neg-party val
   '("received ~a value~a" expected: "~a value~a")
   length-reses
   (if (= 1 length-reses) "" "s")
   expected-values
   (if (= 1 expected-values) "" "s")))

(define (build-nullary-very-simple--> _rng
                                      chaperone-constructor)
  (define rng (coerce-contract '-> _rng))
  (cond
    [(and (flat-contract? rng)
          (eq? void? (flat-contract-predicate rng)))
     ->void-contract]
    [(chaperone-contract? rng)
     (make--> 0
              '() '() #f #f #f
              (list rng) #f #f
              chaperone-constructor
              #f)]
    [else
     (make-impersonator-> 0 '() '() #f #f #f
                          (list rng) #f #f
                          chaperone-constructor
                          #f)]))

(define (build-unary-very-simple--> _dom _rng
                                    chaperone-constructor)
  (define dom (coerce-contract '-> _dom))
  (define rng (coerce-contract '-> _rng))
  (cond
    [(and (any/c? dom)
          (flat-contract? rng)
          (eq? boolean? (flat-contract-predicate rng)))
     any/c->boolean-contract]
    [(and (chaperone-contract? dom)
          (chaperone-contract? rng))
     (make--> 1
              (list dom) '() #f #f #f
              (list rng) #f #f
              chaperone-constructor
              #f)]
    [else
     (make-impersonator-> 1
                          (list dom) '() #f #f #f
                          (list rng) #f #f
                          chaperone-constructor
                          #f)]))

;; INVARIANT: this is not called when `build-unary-very-simple-->`
;; or `build-nullary-very-simple-->` could have been
(define (build-very-simple--> raw-regular-doms raw-rngs
                              chaperone-constructor)
  (define regular-doms
    (for/list ([dom (in-list raw-regular-doms)])
      (coerce-contract '-> dom)))
  (define rngs
    (and raw-rngs
         (for/list ([rng (in-list raw-rngs)])
           (coerce-contract '-> rng))))
  (cond
    [(and (andmap chaperone-contract? regular-doms)
          (andmap chaperone-contract? (or rngs '())))
     (make--> (length raw-regular-doms)
              regular-doms '() #f #f #f
              rngs #f #f
              chaperone-constructor
              #f)]
    [else
     (make-impersonator-> (length raw-regular-doms)
                          regular-doms '() #f #f #f
                          rngs #f #f
                          chaperone-constructor
                          #f)]))

(define (build-simple--> raw-regular-doms
                         mandatory-kwds mandatory-raw-kwd-doms
                         raw-rngs
                         chaperone-constructor
                         raw-rest-ctc
                         method?)
  (build--> '->
            raw-regular-doms '() 
            mandatory-kwds mandatory-raw-kwd-doms
            '() '()
            raw-rest-ctc
            #f #f raw-rngs #f #f
            chaperone-constructor
            method?))

(define (build--> who 
                  pre-raw-regular-doms raw-optional-doms 
                  mandatory-kwds mandatory-raw-kwd-doms
                  optional-kwds optional-raw-kwd-doms
                  raw-rest-ctc
                  pre-cond pre-cond-thunk
                  raw-rngs
                  post-cond post-cond-thunk
                  chaperone-constructor
                  method?)
  (define raw-regular-doms
    (if method?
        (cons any/c pre-raw-regular-doms) ; `this` argument
        pre-raw-regular-doms))
  (define regular-doms
    (for/list ([dom (in-list (append raw-regular-doms raw-optional-doms))])
      (coerce-contract who dom)))
  (define mandatory-kwd-infos 
    (for/list ([kwd (in-list mandatory-kwds)]
               [dom (in-list mandatory-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #t)))
  (define optional-kwd-infos 
    (for/list ([kwd (in-list optional-kwds)]
               [dom (in-list optional-raw-kwd-doms)])
      (kwd-info kwd (coerce-contract who dom) #f)))
  (define kwd-infos (sort (append optional-kwd-infos mandatory-kwd-infos)
                          keyword<?
                          #:key kwd-info-kwd))
  (define rest-ctc (and raw-rest-ctc (coerce-contract who raw-rest-ctc)))
  (define rngs
    (and raw-rngs
         (for/list ([rng (in-list raw-rngs)])
           (coerce-contract who rng))))
  (cond
    [(and (null? regular-doms)
          (null? kwd-infos)
          (not rest-ctc)
          (not pre-cond)
          (not post-cond)
          (pair? rngs)
          (null? (cdr rngs))
          (flat-contract? (car rngs))
          (eq? void? (flat-contract-predicate (car rngs))))
     ->void-contract]
    [(and (pair? regular-doms)
          (null? (cdr regular-doms))
          (any/c? (car regular-doms))
          (null? kwd-infos)
          (not rest-ctc)
          (not pre-cond)
          (not post-cond)
          (pair? rngs)
          (null? (cdr rngs))
          (flat-contract? (car rngs))
          (eq? boolean? (flat-contract-predicate (car rngs))))
     any/c->boolean-contract]
    [(and (andmap chaperone-contract? regular-doms)
          (andmap (λ (x) (chaperone-contract? (kwd-info-ctc x))) kwd-infos)
          (andmap chaperone-contract? (or rngs '())))
     (make--> (length raw-regular-doms)
              regular-doms kwd-infos rest-ctc
              pre-cond pre-cond-thunk
              rngs post-cond post-cond-thunk
              chaperone-constructor
              method?)]
    [else
     (make-impersonator-> (length raw-regular-doms)
                          regular-doms kwd-infos rest-ctc
                          pre-cond pre-cond-thunk
                          rngs post-cond post-cond-thunk
                          chaperone-constructor
                          method?)]))

(define (dynamic->* #:mandatory-domain-contracts [mandatory-domain-contracts '()]
                    #:optional-domain-contracts [optional-domain-contracts '()]
                    #:mandatory-keywords [unsorted-mandatory-keywords '()]
                    #:mandatory-keyword-contracts [unsorted-mandatory-keyword-contracts '()]
                    #:optional-keywords [unsorted-optional-keywords '()]
                    #:optional-keyword-contracts [unsorted-optional-keyword-contracts '()]
                    #:rest-contract [rest-contract #f]
                    #:range-contracts range-contracts)
  
  ;; leave these out for now
  (define pre-cond #f)
  (define post-cond #f)
  (define pre-cond-thunk #f)
  (define post-cond-thunk #f)
  
  (define-syntax-rule (check-list e) (check-list/proc e 'e))
  (define (check-list/proc e name)
    (unless (list? e)
      (raise-argument-error 
       'dynamic->*
       (format "list? in the #:~a argument" name)
       e)))
  (define (check-list/kwds e name)
    (unless (andmap keyword? e)
      (raise-argument-error 
       'dynamic->*
       (format "(listof keyword?) in the #:~a argument" name)
       e)))
  (define (check-same-length l1 l2 name)
    (unless (= (length l1) (length l2))
      (error 'dynamic->*
             (string-append
              "expected the length of the #:~a-keywords argument"
              " to be the same as the length of the #:~a-keyword-contracts argument")
             name name)))
  (check-list mandatory-domain-contracts)
  (check-list optional-domain-contracts)
  (check-list unsorted-mandatory-keywords)
  (check-list/kwds unsorted-mandatory-keywords 'mandatory-keywords)
  (check-list unsorted-mandatory-keyword-contracts)
  (check-same-length unsorted-mandatory-keywords unsorted-mandatory-keyword-contracts 'mandatory)
  (check-list unsorted-optional-keywords)
  (check-list/kwds unsorted-optional-keywords 'optional-keywords)
  (check-list unsorted-optional-keyword-contracts)
  (check-same-length unsorted-optional-keywords unsorted-optional-keyword-contracts 'optional)
  (unless (or (not range-contracts)
              (list? range-contracts))
    (raise-argument-error 'dynamic->*
                          "(or/c (listof contract?) #f) in the #:range-contracts argument"
                          range-contracts))
  
  (define (sort-kwds unsorted-keywords unsorted-keyword-contracts)
    (define sorted
      (sort (map cons unsorted-keywords unsorted-keyword-contracts)
            keyword<?
            #:key car))
    (values (map car sorted) (map cdr sorted)))
  (define-values (mandatory-keywords mandatory-keyword-contracts)
    (sort-kwds unsorted-mandatory-keywords unsorted-mandatory-keyword-contracts))
  (define-values (optional-keywords optional-keyword-contracts)
    (sort-kwds unsorted-optional-keywords unsorted-optional-keyword-contracts))
  
  (define-syntax-rule 
    (define-next next args)
    (define (next n) 
      (let loop ([n n][_args args])
        (cond
          [(zero? n) (set! args _args) '()]
          [(null? _args) (error 'plug-one-arity-function-dynamic->* "internal error")]
          [else (cons (car _args) (loop (- n 1) (cdr _args)))]))))
  
  (define min-arity (length mandatory-domain-contracts))
  (define optionals (length optional-domain-contracts))
  (define rng-len (and range-contracts (length range-contracts)))
  (define max-arity (if rest-contract #f (+ min-arity optionals)))

  (define build-chaperone-constructor
    (or (lookup-popular-chaperone-key (for/list ([i (in-range min-arity)]) #f)
                                      optionals
                                      mandatory-keywords
                                      optional-keywords
                                      (and rest-contract #t)
                                      rng-len)
        (λ (blame f neg-party blame-party-info is-impersonator? rng-ctc-x . args)
          (define-next next args)
          (define mandatory-dom-projs (next min-arity))
          (define optional-dom-projs (next optionals))
          (define rest-proj (if rest-contract
                                (car (next 1))
                                #f))
          (define mandatory-dom-kwd-projs (next (length mandatory-keyword-contracts)))
          (define optional-dom-kwd-projs (next (length optional-keyword-contracts)))
          (define rng-projs (and rng-len (next rng-len)))
          (define mandatory+optional-dom-projs (append mandatory-dom-projs optional-dom-projs))
          (define kwd-table
            (make-hash
             (for/list ([kwd (in-list (append mandatory-keywords optional-keywords))]
                        [kwd-proj (in-list (append mandatory-dom-kwd-projs optional-dom-kwd-projs))])
               (cons kwd kwd-proj))))
          (define blame+neg-party (cons blame neg-party))
          
          (define interposition-proc
            (make-keyword-procedure
             (λ (kwds kwd-args . args)
               
               (check-arg-count min-arity max-arity args f blame neg-party rest-contract #f)
               (check-keywords mandatory-keywords optional-keywords kwds f blame neg-party)
               
               (define kwd-results
                 (for/list ([kwd (in-list kwds)]
                            [kwd-arg (in-list kwd-args)])
                   ((hash-ref kwd-table kwd) kwd-arg neg-party)))
               (define regular-arg-results
                 (let loop ([args args]
                            [projs mandatory+optional-dom-projs])
                   (cond
                     [(and (null? projs) (null? args)) '()]
                     [(null? projs)
                      (rest-proj args neg-party)]
                     [(null? args) (error 'cant-happen::dynamic->*)]
                     [else (cons ((car projs) (car args) neg-party)
                                 (loop (cdr args) (cdr projs)))])))
               (define (result-checker . results)
                 (unless (= rng-len (length results))
                   (bad-number-of-results (blame-add-missing-party blame neg-party)
                                                f rng-len results))
                 (apply 
                  values
                  (for/list ([res (in-list results)]
                             [neg-party-proj (in-list rng-projs)])
                    (neg-party-proj res neg-party))))
               (define args-dealt-with
                 (if (null? kwds)
                     regular-arg-results
                     (cons kwd-results regular-arg-results)))
               (apply
                values
                (if range-contracts
                    (cons result-checker args-dealt-with)
                    args-dealt-with)))))
          
          (values (arity-checking-wrapper f blame neg-party blame+neg-party
                                          interposition-proc interposition-proc
                                          #f interposition-proc interposition-proc #f #f #f
                                          min-arity max-arity
                                          mandatory-keywords optional-keywords
                                          #f ; not a method contract
                                          is-impersonator?)
                  #f))))
  
  (build--> 'dynamic->*
            mandatory-domain-contracts optional-domain-contracts 
            mandatory-keywords mandatory-keyword-contracts
            optional-keywords optional-keyword-contracts
            rest-contract
            pre-cond pre-cond-thunk range-contracts post-cond post-cond-thunk
            build-chaperone-constructor
            #f)) ; not a method contract

(define (->-generate ctc)
  (cond
    [(and (equal? (length (base->-doms ctc))
                  (base->-min-arity ctc))
          (not (base->-rest ctc)))
     ;; only handle the case with no optional args and no rest args
     (define dom-ctcs (base->-doms ctc))
     (define doms-l (length dom-ctcs))
     (λ (fuel)
       (define dom-exers '())
       (define addl-available dom-ctcs)
       (for ([c (in-list (base->-doms ctc))])
         (define-values (exer ctcs) ((contract-struct-exercise c) fuel))
         (set! dom-exers (cons exer dom-exers))
         (set! addl-available (append ctcs addl-available)))
       (set! dom-exers (reverse dom-exers))
       (define rngs-gens 
         (if (base->-rngs ctc)
             (with-definitely-available-contracts
              addl-available
              (λ ()
                (for/list ([c (in-list (base->-rngs ctc))])
                  (contract-random-generate/choose c fuel))))
             '()))
       (cond
         [(for/and ([rng-gen (in-list rngs-gens)])
            rng-gen)
          (define env (contract-random-generate-get-current-environment))
          (λ ()
            (procedure-reduce-arity
             (λ args
               ; stash the arguments for use by other generators
               (for ([ctc (in-list dom-ctcs)]
                     [arg (in-list args)])
                 (contract-random-generate-stash env ctc arg))
               ; exercise the arguments
               (for ([arg (in-list args)]
                     [dom-exer (in-list dom-exers)])
                 (dom-exer arg))
               ; compute the results 
               (define results
                 (for/list ([rng-gen (in-list rngs-gens)])
                   (rng-gen)))
               ; return the results
               (apply values results))
             doms-l))]
         [else #f]))]
    [else (λ (fuel) #f)]))

(define (->-exercise ctc)
  (define rng-ctcs (base->-rngs ctc))
  (define dom-ctcs (for/list ([doms (in-list (base->-doms ctc))]
                              [i (in-range (base->-min-arity ctc))])
                     doms))
  (define dom-kwd-infos (for/list ([dom-kwd (in-list (base->-kwd-infos ctc))]
                                   #:when (kwd-info-mandatory? dom-kwd))
                          dom-kwd))
  (define dom-kwds (map kwd-info-kwd dom-kwd-infos))
  (cond
    [(not (base->-rest ctc))
     (λ (fuel)
       (define gens 
         (for/list ([dom-ctc (in-list dom-ctcs)])
           (contract-random-generate/choose dom-ctc fuel)))
       (define kwd-gens
         (for/list ([kwd-info (in-list dom-kwd-infos)])
           (contract-random-generate/choose (kwd-info-ctc kwd-info) fuel)))
       (define rng-exers
         (and rng-ctcs
              (for/list ([rng-ctc (in-list rng-ctcs)])
                (define-values (exer ctcs)
                  ((contract-struct-exercise rng-ctc) fuel))
                exer)))
       (define env (contract-random-generate-get-current-environment))
       (cond
         [(and (andmap values gens)
               (andmap values kwd-gens))
          (values 
           (λ (f)
             (call-with-values
              (λ ()
                (keyword-apply 
                 f
                 dom-kwds
                 (for/list ([kwd-gen (in-list kwd-gens)])
                   (kwd-gen))
                 (for/list ([gen (in-list gens)])
                   (gen))))
              (λ results 
                (when rng-ctcs
                  (for ([res-ctc (in-list rng-ctcs)]
                        [result (in-list results)])
                    (contract-random-generate-stash env res-ctc result))
                  (for ([exer (in-list rng-exers)]
                        [result (in-list results)])
                    (exer result))))))
           (or rng-ctcs '()))]
         [else
          (values void '())]))]
    [else
     (λ (fuel) (values void '()))]))

;; print-as-method-if-method?: Usually, whether an `->` is printed as `->m` is
;; determined by whether the contract has an implicit `any/c` for the `this`
;; argument.
;; Unfortunately, this is not always the case. `object-contract` creates
;; contracts that *look* like function contracts (i.e. print as `->`), but act
;; like method contracts. Therefore, `object-contract` printing needs to
;; override our behavior.
;; That was probably not good design, but we're stuck with it.
(define ((base->-name print-as-method-if-method?) ctc)
  (cond
    [(predicate/c? ctc) 'predicate/c]
    [else
     (define method? (base->-method? ctc))
     (define arr (if (and method? print-as-method-if-method?) '->m '->))
     (define rngs (base->-rngs ctc))
     (define rng-sexp
       (cond
         [(not rngs) 'any]
         [(= 1 (length rngs))
          (contract-name (car rngs))]
         [else
          `(values ,@(map contract-name rngs))]))
     (cond
       [(and (andmap kwd-info-mandatory? (base->-kwd-infos ctc))
             (= (base->-min-arity ctc)
                (length (base->-doms ctc)))
             (or (not (base->-rest ctc))
                 (ellipsis-rest-arg-ctc? (base->-rest ctc)))
             (not (base->-pre? ctc))
             (not (base->-post? ctc)))
        (define kwd-args
          (apply
           append
           (for/list ([kwd-info (in-list (base->-kwd-infos ctc))])
             (list (kwd-info-kwd kwd-info)
                   (contract-name (kwd-info-ctc kwd-info))))))
        (define doms ((if method? cdr values) (map contract-name (base->-doms ctc))))
        (cond
          [(ellipsis-rest-arg-ctc? (base->-rest ctc))
           `(,arr ,@doms
                  ,@kwd-args
                  ,(contract-name (*list-ctc-prefix (base->-rest ctc)))
                  ...
                  ,@(for/list ([ctc (in-list (*list-ctc-suffix (base->-rest ctc)))])
                      (contract-name ctc))
                  ,rng-sexp)]
          [else
           `(,arr ,@doms
                  ,@kwd-args
                  ,rng-sexp)])]
       [else
        (define (take l n) (reverse (list-tail (reverse l) (- (length l) n))))
        (define mandatory-args
          `(,@(map contract-name
                   ((if method? cdr values) (take (base->-doms ctc) (base->-min-arity ctc))))
            ,@(apply
               append
               (for/list ([kwd-info (base->-kwd-infos ctc)]
                          #:when (kwd-info-mandatory? kwd-info))
                 (list (kwd-info-kwd kwd-info) 
                       (contract-name (kwd-info-ctc kwd-info)))))))
        
        (define optional-args
          `(,@(map contract-name (list-tail (base->-doms ctc) (base->-min-arity ctc)))
            ,@(apply
               append
               (for/list ([kwd-info (base->-kwd-infos ctc)]
                          #:when (not (kwd-info-mandatory? kwd-info)))
                 (list (kwd-info-kwd kwd-info) 
                       (contract-name (kwd-info-ctc kwd-info)))))))
        (define arr* (if (and method? print-as-method-if-method?) '->*m '->*))
        `(,arr* ,mandatory-args
                ,@(if (null? optional-args)
                      '()
                      (list optional-args))
                ,@(if (base->-rest ctc)
                      (list '#:rest (contract-name (base->-rest ctc)))
                      (list))
                ,@(case (base->-pre? ctc)
                    [(pre)      (list '#:pre '...)]
                    [(pre/desc) (list '#:pre/desc '...)]
                    [(#f)       (list)])
                ,rng-sexp
                ,@(case (base->-post? ctc)
                    [(post)      (list '#:post '...)]
                    [(post/desc) (list '#:post/desc '...)]
                    [(#f)        (list)]))])]))

(define ((->-first-order ctc) x)
  (define l (base->-min-arity ctc))
  (define man-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:when (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (define opt-kwds (for/list ([kwd-info (base->-kwd-infos ctc)]
                              #:unless (kwd-info-mandatory? kwd-info))
                     (kwd-info-kwd kwd-info)))
  (and (procedure? x) 
       (if (base->-rest ctc)
           (procedure-accepts-and-more? x l)
           (procedure-arity-includes? x l #t))
       (keywords-match man-kwds opt-kwds x)
       #t))

(define (make-property is-impersonator?)
  (define build-X-property
    (if is-impersonator? build-contract-property build-chaperone-contract-property))
  (define val-first-proj
    (λ (->stct)
      (maybe-warn-about-val-first ->stct)
      (->-proj is-impersonator? ->stct
               (base->-min-arity ->stct)
               (base->-doms ->stct)
               (base->-kwd-infos ->stct)
               (base->-rest ->stct)
               (base->-pre? ->stct)
               (base->-pre-thunk ->stct)
               (base->-rngs ->stct)
               (base->-post? ->stct)
               (base->-post-thunk ->stct)
               (base->-chaperone-constructor ->stct)
               (base->-method? ->stct)
               #f)))
  (define collapsible-late-neg-proj
    (λ (->stct)
      (->-proj is-impersonator? ->stct
               (base->-min-arity ->stct)
               (base->-doms ->stct)
               (base->-kwd-infos ->stct)
               (base->-rest ->stct)
               (base->-pre? ->stct)
               (base->-pre-thunk ->stct)
               (base->-rngs ->stct)
               (base->-post? ->stct)
               (base->-post-thunk ->stct)
               (base->-chaperone-constructor ->stct)
               (base->-method? ->stct)
               #t)))
  (build-X-property
   #:trusted trust-me
   #:name (base->-name #|print-as-method-if-method|# #t)
   #:first-order ->-first-order
   #:projection
   (λ (this)
     (define cthis (val-first-proj this))
     (λ (blame)
       (define cblame (cthis blame))
       (λ (val)
         ((cblame val) #f))))
   #:stronger ->-stronger
   #:equivalent ->-equivalent
   #:generate ->-generate
   #:exercise ->-exercise
   #:val-first-projection val-first-proj
   #:collapsible-late-neg-projection collapsible-late-neg-proj))

(define (->-stronger this that)
  (and (base->? that)
       (= (length (base->-doms that))
          (length (base->-doms this)))
       (= (base->-min-arity this) (base->-min-arity that))
       (andmap contract-struct-stronger? (base->-doms that) (base->-doms this))
       (= (length (base->-kwd-infos this))
          (length (base->-kwd-infos that)))
       (for/and ([this-kwd-info (base->-kwd-infos this)]
                 [that-kwd-info (base->-kwd-infos that)])
         (and (equal? (kwd-info-kwd this-kwd-info)
                      (kwd-info-kwd that-kwd-info))
              (contract-struct-stronger? (kwd-info-ctc that-kwd-info)
                                         (kwd-info-ctc this-kwd-info))))
       (if (base->-rngs this)
           (and (base->-rngs that)
                (andmap contract-struct-stronger? (base->-rngs this) (base->-rngs that)))
           (not (base->-rngs that)))
       (not (base->-pre? this))
       (not (base->-pre? that))
       (not (base->-post? this))
       (not (base->-post? that))))

(define (->-equivalent this that)
  (and (base->? that)
       (= (length (base->-doms that))
          (length (base->-doms this)))
       (= (base->-min-arity this) (base->-min-arity that))
       (andmap contract-struct-equivalent? (base->-doms that) (base->-doms this))
       (= (length (base->-kwd-infos this))
          (length (base->-kwd-infos that)))
       (for/and ([this-kwd-info (base->-kwd-infos this)]
                 [that-kwd-info (base->-kwd-infos that)])
         (and (equal? (kwd-info-kwd this-kwd-info)
                      (kwd-info-kwd that-kwd-info))
              (contract-struct-equivalent? (kwd-info-ctc that-kwd-info)
                                           (kwd-info-ctc this-kwd-info))))
       (if (base->-rngs this)
           (and (base->-rngs that)
                (andmap contract-struct-equivalent? (base->-rngs this) (base->-rngs that)))
           (not (base->-rngs that)))
       (not (base->-pre? this))
       (not (base->-pre? that))
       (not (base->-post? this))
       (not (base->-post? that))))

(define-struct (-> base->) ()
  #:property prop:chaperone-contract (make-property #f))

(define-struct (predicate/c base->) ()
  #:property prop:chaperone-contract (make-property #f))

(define-struct (impersonator-> base->) ()
  #:property prop:contract (make-property #t))

(define ->void-contract
  (let-syntax ([get-chaperone-constructor
                (λ (_)
                  (define desired-key '(() 0 () () #f 1))
                  (define expected-index 0)
                  (unless (equal? desired-key (list-ref popular-keys expected-index))
                    (error '->void-contract "expected the 0th key to be ~s" desired-key))
                  (define ids (list-ref popular-key-ids expected-index))
                  (list-ref ids 1))])
    (make--> 0 '() '() #f #f #f
             (list (coerce-contract 'whatever void?))
             #f #f
             (get-chaperone-constructor)
             #f))) ; not a method contract

(define (mk-any/c->boolean-contract constructor)
  (define (check-result blame neg-party rng)
    (if (boolean? rng)
        rng
        (raise-blame-error blame #:missing-party neg-party rng
                           '(expected: "boolean?" given: "~e")
                           rng)))
  (define (rng-checker f blame neg-party)
    (case-lambda
      [(rng)
       (check-result blame neg-party rng)]
      [args
       (wrong-number-of-results-blame blame neg-party f args 1)]))
  (constructor 1 (list any/c) '() #f #f #f
               (list (coerce-contract 'whatever boolean?))
               #f #f
               (λ (blame f neg-party
                         _ignored-blame-party-info
                         _ignored-is-impersonator?
                         _ignored-rng-ctcs
                         _ignored-dom-contract
                         _ignored-rng-contract)
                 (unless (procedure? f)
                   (raise-blame-error
                    blame #:missing-party neg-party f
                    '(expected: "a procedure" given: "~e")
                    f))
                 (unless (procedure-arity-includes? f 1)
                   (raise-blame-error
                    blame #:missing-party neg-party f
                    '(expected: "a procedure that accepts 1 non-keyword argument"
                                given: "~e")
                    f))
                 (values (cond
                           [(and (struct-predicate-procedure? f)
                                 (not (impersonator? f)))
                            #f]
                           [(and (equal? (procedure-arity f) 1)
                                 (let-values ([(required mandatory) (procedure-keywords f)])
                                   (and (null? required)
                                        (null? mandatory))))
                            (λ (arg)
                              (values (rng-checker f blame neg-party) arg))]
                           [(procedure-arity-includes? f 1)
                            (make-keyword-procedure
                             (λ (kwds kwd-args . other)
                               (unless (null? kwds)
                                 (raise-no-keywords-arg blame #:missing-party neg-party f kwds))
                               (unless (= 1 (length other))
                                 (raise-wrong-number-of-args-error
                                  #:missing-party neg-party
                                  blame f (length other) 1 1
                                  #f)) ; not a method contract
                               (values (rng-checker f blame neg-party) (car other))))])
                         #f))
               #f)) ; not a method contract

(define -predicate/c (mk-any/c->boolean-contract predicate/c))
(define any/c->boolean-contract (mk-any/c->boolean-contract make-->))
