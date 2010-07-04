(module kw '#%kernel
  (#%require "define.ss"
             "small-scheme.ss"
             "more-scheme.ss"
             (for-syntax '#%kernel
                         "stx.ss"
                         "small-scheme.ss"
                         "stxcase-scheme.ss"
                         "name.ss"
                         "norm-define.ss"
                         "qqstx.ss"
                         "sort.ss"))

  (#%provide new-lambda new-λ
             new-define
             new-app
             (rename *make-keyword-procedure make-keyword-procedure)
             keyword-apply
             procedure-keywords
             procedure-reduce-keyword-arity
             new-prop:procedure)
  
  ;; ----------------------------------------

  (-define-struct keyword-procedure (proc required allowed))
  (define-values (struct:keyword-method make-km keyword-method? km-ref km-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      0 0 #f))

  (define (generate-arity-string proc)
    (let-values ([(req allowed) (procedure-keywords proc)]
                 [(a) (procedure-arity proc)]
                 [(keywords-desc)
                  (lambda (opt req)
                    (format "~a with keyword~a~a"
                            (if (null? (cdr req))
                                (format "an ~aargument" opt)
                                (format "~aarguments" opt))
                            (if (null? (cdr req))
                                ""
                                "s")
                            (case (length req)
                              [(1) (format " ~a" (car req))]
                              [(2) (format " ~a and ~a" (car req) (cadr req))]
                              [else
                               (let loop ([req req])
                                 (if (null? (cdr req))
                                     (format " and ~a" (car req))
                                     (format " ~a,~a" (car req)
                                             (loop (cdr req)))))])))]
                 [(method-adjust)
                  (lambda (a)
                    (if (or (okm? proc)
                            (keyword-method? proc))
                        (if (zero? a) 0 (sub1 a))
                        a))])

      (string-append
       (cond
        [(number? a) 
         (let ([a (method-adjust a)])
           (format "~a argument~a" a (if (= a 1) "" "s")))]
        [(arity-at-least? a)
         (let ([a (method-adjust (arity-at-least-value a))])
           (format "at least ~a argument~a" a (if (= a 1) "" "s")))]
        [else
         "a different number of arguments"])
       (if (null? req)
           ""
           (format " plus ~a" (keywords-desc "" req)))
       (if allowed
           (let ([others (let loop ([req req][allowed allowed])
                           (cond
                            [(null? req) allowed]
                            [(eq? (car req) (car allowed))
                             (loop (cdr req) (cdr allowed))]
                            [else
                             (cons (car allowed) (loop req (cdr allowed)))]))])
             (if (null? others)
                 ""
                 (format " plus ~a"
                         (keywords-desc "optional " others))))
           " plus arbitrary keyword arguments"))))

  ;; Constructor for a procedure with only optional keywords.
  ;; The `procedure' property dispatches to a procedure in the 
  ;; struct (which has exactly the right arity).
  (define-values (struct:okp make-optional-keyword-procedure okp? okp-ref okp-set!)
    (make-struct-type 'procedure
                      struct:keyword-procedure
                      1 0 #f
                      (list (cons prop:arity-string generate-arity-string))
                      (current-inspector) 0))

  ;; A ``method'' (for arity reporting)
  (define-values (struct:okm make-optional-keyword-method okm? okm-ref okm-set!)
    (make-struct-type 'procedure
                      struct:okp
                      0 0 #f))
  
  ;; Constructor generator for a procedure with a required keyword.
  ;; (This is used with lift-expression, so that the same constructor
  ;;  is used for each evaluation of a keyword lambda.)
  ;; The `procedure' property is a per-type method that has exactly
  ;;  the right arity, and that sends all arguments to `missing-kw'.
  (define (make-required name fail-proc method?)
    (let-values ([(s: mk ? -ref -set!)
                  (make-struct-type (string->symbol (format "procedure:~a" name))
                                    (if method?
                                        struct:keyword-method
                                        struct:keyword-procedure)
                                    0 0 #f
                                    (list (cons prop:arity-string generate-arity-string))
                                    (current-inspector) fail-proc)])
      mk))

  ;; Allows keyword application to see into a "method"-style procedure attribute:
  (define-values (new-prop:procedure new-procedure? new-procedure-ref)
    (make-struct-type-property 'procedure #f
                               (list (cons prop:procedure values))))
  
  ;; ----------------------------------------

  (define *make-keyword-procedure
    (letrec ([make-keyword-procedure
              (case-lambda 
                [(proc) (make-keyword-procedure 
                         proc
                         (lambda args
                           (apply proc null null args)))]
                [(proc plain-proc)
                 (make-optional-keyword-procedure
                  proc
                  null
                  #f
                  plain-proc)])])
      make-keyword-procedure))
                         
  (define (keyword-apply proc kws kw-vals . normal-argss)
    (let ([type-error
           (lambda (what which)
             (apply raise-type-error
                    'keyword-apply
                    what
                    which
                    proc
                    kws
                    kw-vals
                    normal-argss))])
      (unless (procedure? proc)
        (type-error "procedure" 0))
      (let loop ([ks kws])
        (cond
          [(null? ks) (void)]
          [(or (not (pair? ks))
               (not (keyword? (car ks))))
           (type-error "list of keywords" 1)]
          [(null? (cdr ks)) (void)]
          [(or (not (pair? (cdr ks)))
               (not (keyword? (cadr ks))))
           (loop (cdr ks))]
          [(keyword<? (car ks) (cadr ks))
           (loop (cdr ks))]
          [else (type-error "sorted list of keywords" 1)]))
      (unless (list? kw-vals)
        (type-error "list" 2))
      (unless (= (length kws) (length kw-vals))
        (raise-mismatch-error
         'keyword-apply
         (format
          "keyword list: ~e; does not match the length of the value list: "
          kws)
         kw-vals))
      (let ([normal-args
             (let loop ([normal-argss normal-argss][pos 3])
               (if (null? (cdr normal-argss))
                   (let ([l (car normal-argss)])
                     (if (list? l)
                         l
                         (type-error "list" pos)))
                   (cons (car normal-argss)
                         (loop (cdr normal-argss) (add1 pos)))))])
        (if (null? kws)
            (apply proc normal-args)
            (apply
             (keyword-procedure-extract kws (+ 2 (length normal-args)) proc)
             kws
             kw-vals
             normal-args)))))

  (define (procedure-keywords p)
    (cond
     [(keyword-procedure? p)
      (values (keyword-procedure-required p)
              (keyword-procedure-allowed p))]
     [(procedure? p)
      (let ([p2 (procedure-extract-target p)])
        (if p2
            (procedure-keywords p2)
            (if (new-procedure? p)
                (let ([v (new-procedure-ref p)])
                  (if (procedure? v)
                      (procedure-keywords v)
                      (values null null)))
                (values null null))))]
     [else (raise-type-error 'procedure-keywords
                             "procedure"
                             p)]))

  ;; ----------------------------------------
  ;; `lambda' with optional and keyword arguments
  
  (define-for-syntax (simple-args? args)
    (cond
      [(identifier? args) #t]
      [(pair? args) (and (identifier? (car args))
                         (simple-args? (cdr args)))]
      [(syntax? args) (simple-args? (syntax-e args))]
      [(null? args) #t]
      [else #f]))
  
  ;; Helper to parse the argument list.
  ;; The result is syntax:
  ;;    ((plain-id ...)           ; non-potional, non-keyword args
  ;;     (opt-id ...)             ; optional, non-keyword args
  ;;     ([id opt-expr kind] ...) ; all args, kind is one of: #:plain, #:opt, #:kw
  ;;     ([kw kw-id req?] ...)    ; kw args
  ;;     (req-kw ...)             ; required keywords (could be extracted from previous)
  ;;     rest)                    ; either () or (rest-id)
  (define-for-syntax (parse-formals stx args)
    (let* ([kw-ht (make-hasheq)]
           [check-kw (lambda (kw)
                       (when (hash-ref kw-ht (syntax-e kw) #f)
                         (raise-syntax-error
                          #f
                          "duplicate keyword for argument"
                          stx
                          kw))
                       (hash-set! kw-ht (syntax-e kw) #t))])
      (let loop ([args args] [needs-default? #f])
        (syntax-case args ()
          [id
           (identifier? (syntax id))
           #'(() () () () () (id))]
          [()
           #'(() () () () () ())]
          [(id . rest)
           (identifier? (syntax id))
           (begin
             (when needs-default?
               (raise-syntax-error
                #f "default-value expression missing" stx (syntax id)))
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #f)])
               #'((id . plain) opt-ids ([id #f #:plain] . opts) kws need-kw rest)))]
          [([id default] . rest)
           (identifier? (syntax id))
           (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #t)])
             #'(plain (id . opt-ids) ([id default #:opt] . opts) kws need-kw rest))]
          [(kw id . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id #f #:kw-req] . opts) ([kw id #t] . kws) (kw . need-kw) rest)))]
          [(kw [id default] . rest)
           (and (identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id default #:kw-opt] . opts) ([kw id #f] . kws) need-kw rest)))]
          [(kw)
           (keyword? (syntax-e #'kw))
           (begin
             (check-kw #'kw)
             (raise-syntax-error
              #f
              "missing argument identifier after keyword"
              stx
              #'kw))]
          [(kw bad . rest)
           (keyword? (syntax-e #'kw))
           (raise-syntax-error
            #f
            "after keyword, not an identifier or identifier with default"
            stx
            (syntax bad))]
          [(bad . rest)
           (raise-syntax-error
            #f
            "not an identifier, identifier with default, or keyword"
            stx
            (syntax bad))]
          [else
           (raise-syntax-error
            #f "bad argument sequence" stx (syntax args))]))))
  
  ;; The new `lambda' form:
  (define-syntaxes (new-lambda new-λ)
    (let ([new-lambda
           (lambda (stx)
             (if (eq? (syntax-local-context) 'expression)
                 (syntax-case stx ()
                   [(_ args body1 body ...)
                    (if (simple-args? #'args)
                        ;; Use plain old `lambda':
                        (syntax/loc stx
                          (lambda args body1 body ...))
                        ;; Handle keyword or optional arguments:
                        (with-syntax ([((plain-id ...)
                                        (opt-id ...)
                                        ([id opt-expr kind] ...)
                                        ([kw kw-id kw-req] ...)
                                        need-kw
                                        rest)
                                       (parse-formals stx #'args)])
                          (let ([dup-id (check-duplicate-identifier (syntax->list #'(id ... . rest)))])
                            (when dup-id
                              (raise-syntax-error
                               #f
                               "duplicate argument identifier"
                               stx
                               dup-id)))
                          (let* ([kws (syntax->list #'(kw ...))]
                                 [opts (syntax->list #'(opt-id ...))]
                                 [ids (syntax->list #'(id ...))]
                                 [plain-ids (syntax->list #'(plain-id ...))]
                                 [kw-reqs (syntax->list #'(kw-req ...))]
                                 [kw-args (generate-temporaries kws)]     ; to hold supplied value
                                 [kw-arg?s (generate-temporaries kws)]    ; to indicated whether it was supplied
                                 [opt-args (generate-temporaries opts)]   ; supplied value
                                 [opt-arg?s (generate-temporaries opts)]  ; whether supplied
                                 [needed-kws (sort (syntax->list #'need-kw)
                                                   (lambda (a b) (keyword<? (syntax-e a) (syntax-e b))))]
                                 [sorted-kws (sort (map list kws kw-args kw-arg?s kw-reqs)
                                                   (lambda (a b) (keyword<? (syntax-e (car a))
                                                                            (syntax-e (car b)))))]
                                 [method? (syntax-property stx 'method-arity-error)])
                            (with-syntax ([(kw-arg ...) kw-args]
                                          [(kw-arg? ...) (let loop ([kw-arg?s kw-arg?s]
                                                                    [kw-reqs kw-reqs])
                                                           (cond
                                                            [(null? kw-arg?s) null]
                                                            [(not (syntax-e (car kw-reqs)))
                                                             (cons (car kw-arg?s) (loop (cdr kw-arg?s) (cdr kw-reqs)))]
                                                            [else (loop (cdr kw-arg?s) (cdr kw-reqs))]))]
                                          [kws-sorted sorted-kws]
                                          [(opt-arg ...) opt-args]
                                          [(opt-arg? ...) opt-arg?s]
                                          [(new-plain-id  ...) (generate-temporaries #'(plain-id ...))]
                                          [new-rest (if (null? (syntax-e #'rest))
                                                        '()
                                                        '(new-rest))]
                                          [(rest-id) (if (null? (syntax-e #'rest))
                                                         '(())
                                                         #'rest)]
                                          [rest-empty (if (null? (syntax-e #'rest))
                                                          '()
                                                          '(null))]
                                          [fail-rest (if (null? (syntax-e #'rest))
                                                         '(null)
                                                         #'rest)]
                                          [make-okp (if method?
                                                        #'make-optional-keyword-method
                                                        #'make-optional-keyword-procedure)]
                                          [method? method?])
                              
                              (let ([with-core 
                                     (lambda (result)
                                       ;; body of procedure, where all keyword and optional
                                       ;; argments come in as a pair of arguments (value and
                                       ;; whether the value is valid):
                                       (syntax-property
                                        (quasisyntax/loc stx
                                          ;; We need to push the certificates way down, so that
                                          ;; the `class' macro (for example) can pull it apart
                                          ;; enough to insert an additional argument.
                                          (let #,(syntax-property
                                                  #`(#,(syntax-property
                                                        #`[core 
                                                           #,(syntax-property
                                                              #`(lambda #,(syntax-property
                                                                           #`(given-kws given-args
                                                                                        new-plain-id ... 
                                                                                        opt-arg ...
                                                                                        opt-arg? ...
                                                                                        . new-rest)
                                                                           'certify-mode
                                                                           'transparent)
                                                                  ;; sort out the arguments into the user-supplied bindings,
                                                                  ;; evaluating default-values expressions as needed:
                                                                  (let-kws given-kws given-args kws-sorted
                                                                           (let-maybe ([id opt-expr kind] ... . rest)
                                                                                      (kw-arg ...) (kw-arg? ...)
                                                                                      (opt-arg ...) (opt-arg? ...)
                                                                                      (new-plain-id ... . new-rest)
                                                                                      ;; the original body, finally:
                                                                                      body1 body ...)))
                                                              'certify-mode
                                                              'transparent)]
                                                        'certify-mode
                                                        'transparent))
                                                  'certify-mode
                                                  'transparent)
                                            ;; entry points use `core':
                                            #,result))
                                        'certify-mode
                                        'transparent))]
                                    [mk-no-kws
                                     (lambda ()
                                       ;; entry point without keywords:
                                       (syntax/loc stx
                                         (opt-cases (core null null) ([opt-id opt-arg opt-arg?] ...) (plain-id ...) 
                                                    () (rest-empty rest-id . rest)
                                                    ())))]
                                    [mk-with-kws
                                     (lambda ()
                                       ;; entry point with keywords:
                                       (if (and (null? opts)
                                                (null? #'new-rest))
                                           #'core
                                           (syntax/loc stx
                                             (opt-cases (core) ([opt-id opt-arg opt-arg?] ...) (given-kws given-args plain-id ...) 
                                                        () (rest-empty rest-id . rest)
                                                        ()))))]
                                    [mk-kw-arity-stub
                                     (lambda ()
                                       ;; struct-type entry point for no keywords when a keyword is required
                                       (syntax/loc stx
                                         (fail-opt-cases (missing-kw) (opt-id ...) (self plain-id ...) 
                                                         () (rest-id . fail-rest)
                                                         ())))])
                                (cond
                                 [(null? kws)
                                  ;; just the no-kw part
                                  (with-core (mk-no-kws))]
                                 [(null? needed-kws)
                                  ;; both parts dispatch to core
                                  (with-core
                                   (with-syntax ([kws (map car sorted-kws)]
                                                 [no-kws (let ([p (mk-no-kws)]
                                                               [n (syntax-local-infer-name stx)])
                                                           (if n
                                                               #`(let ([#,n #,p]) #,n)
                                                               p))]
                                                 [with-kws (mk-with-kws)])
                                     (syntax/loc stx
                                       (make-okp
                                        with-kws
                                        null
                                        'kws
                                        no-kws))))]
                                 [else
                                  ;; just the keywords part dispatches to core,
                                  ;; and the other part dispatches to failure
                                  (with-core
                                   (with-syntax ([kws (map car sorted-kws)]
                                                 [needed-kws needed-kws]
                                                 [no-kws (mk-no-kws)]
                                                 [with-kws (mk-with-kws)]
                                                 [mk-id (with-syntax ([n (syntax-local-infer-name stx)]
                                                                      [call-fail (mk-kw-arity-stub)])
                                                          (syntax-local-lift-expression
                                                           #'(make-required 'n call-fail method?)))])
                                     (syntax/loc stx
                                       (mk-id
                                        with-kws
                                        'needed-kws
                                        'kws))))]))))))])
                 #`(#%expression #,stx)))])
      (values new-lambda new-lambda)))
  
  (define (missing-kw proc . args)
    (apply
     (keyword-procedure-extract null 0 proc)
     null
     null
     args))
  
  ;; ----------------------------------------

  ;; Helper macro:
  ;; Steps through the list bound to `kw-args', extracting
  ;; the available values. For each keyword, this binds one
  ;; id to say whether the value is present, and one id
  ;; to the actual value (if present); if the keyword isn't
  ;; available, then the corresponding `req' is applied, which
  ;; should signal an error if the keyword is required.
  (define-syntax let-kws
    (syntax-rules ()
      [(_ kws kw-args () . body)
       (begin . body)]
      [(_ kws kw-args ([kw arg arg? #f] . rest) . body)
       (let ([arg? (and (pair? kws)
                        (eq? 'kw (car kws)))])
         (let ([arg (if arg? (car kw-args) (void))]
               [kws (if arg? (cdr kws) kws)]
               [kw-args (if arg? (cdr kw-args) kw-args)])
           (let-kws kws kw-args rest . body)))]
      [(_ kws kw-args ([kw arg arg? #t] . rest) . body)
       (let ([arg (car kw-args)]
             [kws (cdr kws)]
             [kw-args (cdr kw-args)])
         (let-kws kws kw-args rest . body))]))

  ;; Used for `req' when the keyword argument is optional:
  (define-syntax missing-ok
    (syntax-rules ()
      [(_ x y) #f]))
  
  ;; ----------------------------------------

  ;; Helper macro:
  ;; Builds up a `case-lambda' to handle the arities
  ;; possible due to optional arguments. Each clause
  ;; jumps directory to `core', where each optional
  ;; argument is split into two: a boolean argument that
  ;; indicates whether it was supplied, and an argument 
  ;; for the value (if supplied).
  (define-syntax opt-cases 
    (syntax-rules ()
      [(_ (core ...) () (base ...) () (rest-empty rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (case-lambda
         [(base ... . rest-id)
          (core ... base ... . rest)])]
      [(_ (core ...) ([opt-id opt-arg opt-arg?]) (base ...) (done-id ...) (rest-empty rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (case-lambda
         [(base ...) (core ... base ... (a-false done-id) ... #f (a-false done-id) ... #f . rest-empty)]
         [(base ... done-id ... opt-arg . rest-id)
          (core ... base ... done-id ... opt-arg (a-true done-id) ... #t . rest)]
         . clauses)]
      [(_ (core ...) ([opt-id opt-arg opt-arg?] more ...) (base ...) (done-id ...) (rest-empty rest-id . rest) clauses)
       ;; Handle just one optional argument, add it to the "done" sequence,
       ;; and continue generating clauses for the remaining optional arguments.
       (opt-cases (core ...) (more ...) (base ...) (done-id ... opt-id) (rest-empty rest-id . rest)
                  ([(base ... done-id ... opt-arg)
                    (core ... base ... 
                          done-id ... opt-arg (a-false more) ... 
                          (a-true done-id) ... #t (a-false more) ... . rest-empty)]
                   . clauses))]))

  ;; Helper macro:
  ;; Similar to opt-cases, but just pass all arguments along to `fail'.
  (define-syntax fail-opt-cases
    (syntax-rules ()
      [(_ (fail ...) () (base ...) () (rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (case-lambda
         [(base ... . rest-id)
          (apply fail ... base ... . rest)])]
      [(_ (fail ...) (opt-id) (base ...) (done ...) (rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (case-lambda
         [(base ...) (fail ... base ...)]
         [(base ... done ... opt-id . rest-id) (apply fail ... base ... done ... opt-id . rest)]
         . clauses)]
      [(_ (fail ...) (opt-id more ...) (base ...) (done ...) (rest-id . rest) clauses)
       ;; Handle just one more optional argument:
       (fail-opt-cases (fail ...) (more ...) (base ...) (done ... opt-id) (rest-id . rest)
                       ([(base ... done ... opt-arg)
                         (fail ... base ... done ... opt-arg)]
                        . clauses))]))
  
  ;; Helper macros:
  (define-syntax (a-false stx) #'#f)
  (define-syntax (a-true stx) #'#t)
  
  ;; ----------------------------------------

  ;; Helper macro:
  ;; Walks through all arguments in order, shifting supplied
  ;; optional values into the user-supplied binding, and
  ;; evaluating default-value expressions when the optional
  ;; value is not available. The binding order here is
  ;; consistent with the original order of the arguments
  ;; (where, e.g., an optional keyword argument might
  ;; precede a required argument, so the required argument
  ;; cannot be used to compute the default).
  (define-syntax let-maybe
    (syntax-rules (required)
      [(_ () () () () () () . body)
       (let () . body)]
      [(_ ([id ignore #:plain] . more) kw-args kw-arg?s opt-args opt-arg?s (req-id . req-ids) . body)
       (let ([id req-id])
         (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:opt] . more)  kw-args kw-arg?s (opt-arg . opt-args) (opt-arg? . opt-arg?s) req-ids . body)
       (let ([id (if opt-arg?
                     opt-arg
                     expr)])
         (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:kw-req] . more)  (kw-arg . kw-args) kw-arg?s opt-args opt-arg?s req-ids . body)
       (let ([id kw-arg])
         (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ ([id expr #:kw-opt] . more)  (kw-arg . kw-args) (kw-arg? . kw-arg?s) opt-args opt-arg?s req-ids . body)
       (let ([id (if kw-arg?
                     kw-arg
                     expr)])
         (let-maybe more kw-args kw-arg?s opt-args opt-arg?s req-ids . body))]
      [(_ (id) () () () () (req-id) . body)
       (let ([id req-id]) . body)]))

  ;; ----------------------------------------
  ;; `define' with keyword arguments
  
  (define-syntax (new-define stx)
    (let-values ([(id rhs)
                  (normalize-definition stx #'new-lambda #t #t)])
      (quasisyntax/loc stx
        (define #,id #,rhs))))
  
  ;; ----------------------------------------
  ;; `#%app' with keyword arguments
  
  (define-syntax (new-app stx)
    (let ([l (syntax->list stx)])
      (if (not (and l
                    (pair? (cdr l))
                    (not (keyword? (syntax-e (cadr l))))
                    (ormap (lambda (x) (keyword? (syntax-e x)))
                           l)))
          ;; simple or erroneous app:
          (if (identifier? stx)
              (raise-syntax-error
               #f
               "illegal use"
               stx)
              (if (and (pair? l)
                       (null? (cdr l)))
                  (raise-syntax-error
                   #f
                   "missing procedure expression; probably originally (), which is an illegal empty application"
                   stx)
                  (quasisyntax/loc stx
                    (#%app . #,(cdr (syntax-e stx))))))
          ;; keyword app (maybe)
          (let ([exprs
                 (let ([kw-ht (make-hasheq)])
                   (let loop ([l (cddr l)])
                     (cond
                       [(null? l) null]
                       [(keyword? (syntax-e (car l)))
                        (when (hash-ref kw-ht (syntax-e (car l)) #f)
                          (raise-syntax-error
                           'application
                           "duplicate keyword in application"
                           stx
                           (car l)))
                        (hash-set! kw-ht (syntax-e (car l)) #t)        
                        (cond
                          [(null? (cdr l))
                           (raise-syntax-error
                            'application
                            "missing argument expression after keyword"
                            stx
                            (car l))]
                          [(keyword? (cadr l))
                           (raise-syntax-error
                            'application
                            "keyword in expression possition (immediately after another keyword)"
                            stx
                            (cadr l))]
                          [else
                           (cons (cadr l)
                                 (loop (cddr l)))])]
                       [else
                        (cons (car l) (loop (cdr l)))])))])
            (let ([ids (cons (or (syntax-local-infer-name stx)
                                 'procedure)
                             (generate-temporaries exprs))])
              (let loop ([l (cdr l)]
                         [ids ids]
                         [bind-accum null]
                         [arg-accum null]
                         [kw-pairs null])
                (cond
                  [(null? l)
                   (let* ([args (reverse arg-accum)]
                          [sorted-kws (sort kw-pairs 
                                            (lambda (a b)
                                              (keyword<? (syntax-e (car a))
                                                         (syntax-e (car b)))))]
                          [lifted (syntax-local-lift-expression
                                   #`(list #,@(map (lambda (p) #`(quote #,(car p))) 
                                                   sorted-kws)))]
                          [cnt (+ 1 (length args))])
                     (quasisyntax/loc stx
                       (let #,(reverse bind-accum)
                         ((keyword-procedure-extract #,lifted #,cnt #,(car args))
                          #,lifted
                          (list #,@(map cdr sorted-kws))
                          . #,(cdr args)))))]
                  [(keyword? (syntax-e (car l)))
                   (loop (cddr l)
                         (cdr ids)
                         (cons (list (car ids) (cadr l)) bind-accum)
                         arg-accum
                         (cons (cons (car l) (car ids))
                               kw-pairs))]
                  [else (loop (cdr l)
                              (cdr ids)
                              (cons (list (car ids) (car l)) bind-accum)
                              (cons (car ids) arg-accum)
                              kw-pairs)])))))))
  
  ;; Checks given kws against expected. Result is
  ;; (values missing-kw extra-kw), where both are #f if
  ;; the arguments are ok.
  (define (check-kw-args p kws)
    (let loop ([kws kws]
               [required (keyword-procedure-required p)]
               [allowed (keyword-procedure-allowed p)])
      (cond
        [(null? kws)
         (if (null? required)
             (values #f #f)
             (values (car required) #f))]
        [(and (pair? required)
              (eq? (car required) (car kws)))
         (loop (cdr kws) (cdr required) (and allowed (cdr allowed)))]
        [(not allowed) ; => all keywords are allowed
         (loop (cdr kws) required #f)]
        [(pair? allowed)
         (if (eq? (car allowed) (car kws))
             (loop (cdr kws) required (cdr allowed))
             (loop kws required (cdr allowed)))]
        [else (values #f (car kws))])))

  ;; Extracts the procedure using the keyword-argument protocol.
  ;; If `p' doesn't accept keywords, make up a procedure that
  ;; reports an error.
  (define (keyword-procedure-extract/method kws n p method-n)
    (if (and (keyword-procedure? p)
             (procedure-arity-includes? (keyword-procedure-proc p) n)
             (let-values ([(missing-kw extra-kw) (check-kw-args p kws)])
               (and (not missing-kw) (not extra-kw))))
        ;; Ok:
        (keyword-procedure-proc p)
        ;; Not ok, so far:
        (let ([p2 (if (keyword-procedure? p)
                      #f
                      (if (procedure? p)
                          (or (procedure-extract-target p)
                              (and (new-procedure? p)
                                   'method))
                          #f))])
          (if p2
              ;; Maybe the target is ok:
              (if (eq? p2 'method)
                  ;; Build wrapper method:
                  (let ([p3 (keyword-procedure-extract/method kws (add1 n)
                                                              (new-procedure-ref p)
                                                              (add1 method-n))])
                    (lambda (kws kw-args . args)
                      (apply p3 kws kw-args (cons p args))))
                  ;; Recur:
                  (keyword-procedure-extract/method kws n p2 method-n))
              ;; Not ok, period:
              (lambda (kws kw-args . args)
                (let-values ([(missing-kw extra-kw)
                              (if (keyword-procedure? p)
                                  (check-kw-args p kws)
                                  (values #f (car kws)))]
                             [(n) (let ([method-n (+ method-n
                                                     (if (or (keyword-method? p)
                                                             (okm? p))
                                                         1
                                                         0))])
                                    (if (n . >= . method-n)
                                        (- n method-n)
                                        n))])
                  (let ([args-str
                         (if (and (null? args)
                                  (null? kws))
                             "no arguments supplied"
                             ;; Hack to format arguments:
                             (with-handlers ([exn:fail?
                                              (lambda (exn)
                                                (format "arguments were: ~a"
                                                        (cadr (regexp-match 
                                                               #rx"other arguments were: (.*)$"
                                                               (exn-message exn)))))])
                               (apply raise-type-error 'x "x" 0 'x
                                      (append args
                                              (apply append (map list kws kw-args))))))])
                    (raise
                     (make-exn:fail:contract
                      (if extra-kw
                          (if (keyword-procedure? p)
                              (format 
                               (string-append
                                "procedure application: procedure: ~e;"
                                " does not expect an argument with keyword ~a; ~a")
                               p
                               extra-kw
                               args-str)
                              (format 
                               (string-append
                                "procedure application: expected a procedure that"
                                " accepts keyword arguments, given ~e; ~a")
                               p
                               args-str))
                          (if missing-kw
                              (format 
                               (string-append
                                "procedure application: procedure: ~e; requires"
                                " an argument with keyword ~a, not supplied; ~a")
                               p
                               missing-kw
                               args-str)
                              (format 
                               (string-append
                                "procedure application: no case matching ~a non-keyword"
                                " argument~a for: ~e; ~a")
                               (- n 2)
                               (if (= 1 (- n 2)) "" "s")
                               p
                               args-str)))
                      (current-continuation-marks))))))))))
  (define (keyword-procedure-extract kws n p)
    (keyword-procedure-extract/method kws n p 0))

  ;; setting procedure arity
  (define (procedure-reduce-keyword-arity proc arity req-kw allowed-kw)
    (let ([plain-proc (procedure-reduce-arity (if (okp? proc)
                                                  (okp-ref proc 0) 
                                                  proc)
                                              arity)])
      (define (sorted? kws)
        (let loop ([kws kws])
          (cond
           [(null? kws) #t]
           [(null? (cdr kws)) #t]
           [(keyword<? (car kws) (cadr kws)) (loop (cdr kws))]
           [else #f])))
      (define (subset? a b)
        (cond
         [(null? a) #t]
         [(null? b) #f]
         [(eq? (car a) (car b)) (subset? (cdr a) (cdr b))]
         [(keyword<? (car a) (car b)) #f]
         [else (subset? a (cdr b))]))

      (unless (and (list? req-kw) (andmap keyword? req-kw)
                   (sorted? req-kw))
        (raise-type-error 'procedure-reduce-keyword-arity "sorted list of keywords" 
                          2 proc arity req-kw allowed-kw))
      (when allowed-kw
        (unless (and (list? allowed-kw) (andmap keyword? allowed-kw)
                     (sorted? allowed-kw))
          (raise-type-error 'procedure-reduce-keyword-arity "sorted list of keywords or #f" 
                            2 proc arity req-kw allowed-kw))
        (unless (subset? req-kw allowed-kw)
          (raise-mismatch-error 'procedure-reduce-keyword-arity 
                                "allowed-keyword list does not include all required keywords: "
                                allowed-kw)))
      (let ([old-req (if (keyword-procedure? proc)
                         (keyword-procedure-required proc)
                         null)]
            [old-allowed (if (keyword-procedure? proc)
                             (keyword-procedure-allowed proc)
                             null)])
        (unless (subset? old-req req-kw)
          (raise-mismatch-error 'procedure-reduce-keyword-arity
                                "cannot reduce required keyword set: "
                                old-req))
        (when old-allowed
          (unless (subset? req-kw old-allowed)
            (raise-mismatch-error 'procedure-reduce-keyword-arity
                                  "cannot require keywords not in original allowed set: "
                                  old-allowed))
          (unless (or (not allowed-kw)
                      (subset? allowed-kw old-allowed))
            (raise-mismatch-error 'procedure-reduce-keyword-arity
                                  "cannot allow keywords not in original allowed set: "
                                  old-allowed))))
      (make-optional-keyword-procedure
       (procedure-reduce-arity (keyword-procedure-proc proc)
                               (let loop ([a arity])
                                 (cond
                                  [(integer? a) (+ a 2)]
                                  [(arity-at-least? a)
                                   (make-arity-at-least (+ (arity-at-least-value a) 2))]
                                  [else
                                   (map loop a)])))
       req-kw
       allowed-kw
       plain-proc))))
