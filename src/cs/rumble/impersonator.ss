
(define-record impersonator (val next props))
(define-record chaperone impersonator ())

(define (impersonator-ephemeron i)
  (if (impersonator? i)
      (make-ephemeron (impersonator-val i) i)
      ;; This is a useless ephemeron, but we create one for consistency
      ;; with the case that we have an impersonator:
      (make-ephemeron i i)))

(define (strip-impersonator v)
  (if (impersonator? v)
      (impersonator-val v)
      v))

(define (raise-chaperone-error who what e e2)
  (raise-arguments-error
   who
   (string-append "non-chaperone result; received a" (if (equal? what "argument") "n" "") " " what
                  " that is not a chaperone of the original " what)
   "original" e
   "received" e2))

(define (hash-ref2 ht key1 key2 default)
  (let ([ht/val (intmap-ref ht key1 #f)])
    (if (and key2 ht/val)
        (intmap-ref ht/val key2 #f)
        ht/val)))

(define (hash-set2 ht key1 key2 val)
  (intmap-set ht key1 (if key2
                          (intmap-set (intmap-ref ht key1 empty-hasheq) key2 val)
                          val)))

(define (impersonate-ref acc rtd pos orig record-name field-name)
  (#%$app/no-inline do-impersonate-ref acc rtd pos orig record-name field-name))

(define (do-impersonate-ref acc rtd pos orig record-name field-name)
  (impersonate-struct-or-property-ref acc rtd rtd pos orig record-name field-name))

;; `val/acc` is an accessor if `rtd`, a value otherwise;
;; `key2/pos` is a pos if `rtd`
(define (impersonate-struct-or-property-ref val/acc rtd key1 key2/pos orig record-name field-name)
  (cond
   [(and (impersonator? orig)
         (or (not rtd)
             (record? (impersonator-val orig) rtd)))
    (let loop ([v orig])
      (cond
       [(and rtd
             (struct-undefined-chaperone? v))
        ;; Must be the only wrapper left
        (let ([abs-pos (fx+ key2/pos (struct-type-parent-total*-count rtd))])
          (let ([r (unsafe-struct*-ref (impersonator-val v) abs-pos)])
            (when (eq? r unsafe-undefined)
              (raise-unsafe-undefined 'struct-ref "undefined" "use" val/acc (impersonator-val v) abs-pos))
            r))]
       [(or (struct-impersonator? v)
            (struct-chaperone? v))
        (let ([wrapper (hash-ref2 (struct-impersonator/chaperone-procs v) key1 key2/pos #f)])
          (cond
           [wrapper
            (let* ([r (cond
                       [(pair? wrapper)
                        (|#%app| (car wrapper) (impersonator-next v))]
                       [else
                        (loop (impersonator-next v))])]
                   [new-r (cond
                           [(pair? wrapper)
                            (|#%app| (cdr wrapper) orig r)]
                           [else (|#%app| wrapper orig r)])])
              (when (struct-chaperone? v)
                (unless (chaperone-of? new-r r)
                  (raise-chaperone-error 'struct-ref "value" r new-r)))
              new-r)]
           [else
            (loop (impersonator-next v))]))]
       [(impersonator? v)
        (loop (impersonator-next v))]
       [else
        (cond
         [rtd
          (let ([abs-pos (fx+ key2/pos (struct-type-parent-total*-count rtd))])
            (unsafe-struct*-ref v abs-pos))]
         [else val/acc])]))]
   [else
    (raise-argument-error (string->symbol
                           (string-append (symbol->string (or record-name 'struct))
                                          "-"
                                          (symbol->string (or field-name 'field))))
                          (string-append (symbol->string (or record-name 'struct)) "?")
                          orig)]))

(define (impersonate-set! set rtd pos abs-pos orig a record-name field-name)
  (#%$app/no-inline do-impersonate-set! set rtd pos abs-pos orig a record-name field-name))

(define (struct-mutator-pos->key2 pos) (fx- -1 pos))

(define (do-impersonate-set! set rtd pos abs-pos orig a record-name field-name)
  (cond
   [(and (impersonator? orig)
         (record? (impersonator-val orig) rtd))
    (let ([key1 rtd]
          [key2 (struct-mutator-pos->key2 pos)])
      (let loop ([v orig] [a a])
        (cond
         [(struct-undefined-chaperone? v)
          ;; Must be the only wrapper left
          (let ([v (impersonator-val v)])
            (when (eq? (unsafe-struct*-ref v abs-pos) unsafe-undefined)
              (unless (eq? (continuation-mark-set-first #f prop:chaperone-unsafe-undefined)
                           unsafe-undefined)
                (raise-unsafe-undefined 'struct-set! "assignment disallowed" "assign" set v abs-pos)))
            (unsafe-struct*-set! v abs-pos a))]
         [(or (struct-impersonator? v)
              (struct-chaperone? v))
          (let ([wrapper (hash-ref2 (struct-impersonator/chaperone-procs v) key1 key2 #f)])
            (cond
             [wrapper
              (let ([new-a (cond
                            [(pair? wrapper)
                             (|#%app| (cdr wrapper) orig a)]
                            [else (wrapper orig a)])])
                (when (struct-chaperone? v)
                  (unless (chaperone-of? new-a a)
                    (raise-chaperone-error 'struct-set! "value" a new-a)))
                (cond
                 [(pair? wrapper)
                  (|#%app| (car wrapper) (impersonator-next v) new-a)]
                 [else
                  (loop (impersonator-next v) new-a)]))]
             [else
              (loop (impersonator-next v) a)]))]
         [(impersonator? v)
          (loop (impersonator-next v) a)]
         [else
          ;; Equivalent to `(set v a)`:
          (unsafe-struct*-set! v abs-pos a)])))]
   [else
    (raise-argument-error (string->symbol
                           (string-append "set-"
                                          (symbol->string (or record-name 'struct))
                                          "-"
                                          (if field-name
                                              (symbol->string field-name)
                                              (string-append "field" (number->string pos)))
                                          "!"))
                          (string-append (symbol->string (or record-name 'struct)) "?")
                          orig)]))

(define (impersonate-struct-info orig)
  (let loop ([v orig])
    (cond
     [(struct-chaperone? v)
      (let ([wrapper (hash-ref (struct-impersonator/chaperone-procs v) struct-info #f)])
        (cond
         [wrapper
          (let-values ([(rtd skipped?) (loop (impersonator-next v))])
            (cond
             [(not rtd) (values #f skipped?)]
             [else
              (call-with-values (lambda () (wrapper rtd skipped?))
                (case-lambda
                 [(new-rtd new-skipped?)
                  (unless (chaperone-of? new-rtd rtd)
                    (raise-chaperone-error 'struct-info "value" rtd new-rtd))
                  (unless (chaperone-of? new-skipped? skipped?)
                    (raise-chaperone-error 'struct-info "value" skipped? new-skipped?))
                  (values new-rtd new-skipped?)]
                 [args (raise-impersonator-result-arity-error 'struct-info orig 2 args)]))]))]
         [else
          (loop (impersonator-next v))]))]
     [(impersonator? v)
      (loop (impersonator-next v))]
     [else (struct-info v)])))

(define (raise-impersonator-result-arity-error who orig n args)
  (raise
   (|#%app|
    exn:fail:contract:arity
    (string-append
     (symbol->string who) ": arity mismatch;\n"
     " received wrong number of values from a chaperone's replacement procedure\n"
     "  expected: " (number->string n) "\n"
     "  received: " (number->string (length args)) "\n"
     "  chaperone: " (error-value->string orig)))))

;; ----------------------------------------

(define-record struct-type-chaperone chaperone (struct-info make-constructor guard))

(define/who (chaperone-struct-type rtd struct-info-proc make-constructor-proc guard-proc . props)
  (check who struct-type? rtd)
  (check who (procedure-arity-includes/c 8) struct-info-proc)
  (check who (procedure-arity-includes/c 1) make-constructor-proc)
  (check who procedure? guard-proc)
  (make-struct-type-chaperone
   (strip-impersonator rtd)
   rtd
   (add-impersonator-properties who
                                props
                                (if (impersonator? rtd)
                                    (impersonator-props rtd)
                                    empty-hasheq))
   struct-info-proc
   make-constructor-proc
   guard-proc))

(define (chaperone-constructor rtd ctr)
  (let loop ([rtd rtd])
    (cond
     [(struct-type-chaperone? rtd)
      (let* ([ctr (loop (impersonator-next rtd))]
             [new-ctr ((struct-type-chaperone-make-constructor rtd) ctr)])
        (unless (chaperone-of? new-ctr ctr)
          (raise-chaperone-error 'struct-type-make-constructor "value" ctr new-ctr))
        new-ctr)]
     [(impersonator? rtd)
      (loop (impersonator-next rtd))]
     [else ctr])))

(define (chaperone-struct-type-info orig-rtd get-results)
  (apply
   values
   (let loop ([rtd orig-rtd])
     (cond
      [(struct-type-chaperone? rtd)
       (let ([results (loop (impersonator-next rtd))])
         (let-values ([new-results (apply (struct-type-chaperone-struct-info rtd) results)])
           (cond
            [(= (length results) (length new-results))
             (for-each (lambda (r new-r)
                         (unless (chaperone-of? new-r r)
                           (raise-chaperone-error 'struct-type-info "value" r new-r)))
                       results
                       new-results)
             new-results]
            [else
             (raise-impersonator-result-arity-error 'struct-type-info orig-rtd (length results) new-results)])))]
      [(impersonator? rtd)
       (loop (impersonator-next rtd))]
      [else (call-with-values get-results list)]))))

;; ----------------------------------------

(define-record-type (impersonator-property create-impersonator-property impersonator-property?)
  (fields name))

(define-record-type (impersonator-property-accessor-procedure
                     make-impersonator-property-accessor-procedure
                     raw:impersonator-property-accessor-procedure?)
  (fields proc name))

(define/who (make-impersonator-property name)
  (check who symbol? name)
  (let ([p (create-impersonator-property name)]
        [predicate-name (string->symbol (format "~a?" name))]
        [accessor-name (string->symbol (format "~a-accessor" name))])
    (letrec ([predicate
              (lambda (v)
                (if (impersonator? v)
                    (not (eq? none (hash-ref (impersonator-props v) p none)))
                    (let ([iv (extract-impersonator-of predicate-name v)])
                      (and iv
                           (predicate iv)))))]
             [accessor
              (case-lambda
               [(v default)
                (let ([fail (lambda ()
                              (cond
                               [(eq? default none)
                                (raise-argument-error accessor-name
                                                      (format "~a?" name)
                                                      v)]
                               [(procedure? default)
                                (default)]
                               [else default]))])
                (if (impersonator? v)
                    (let ([pv (hash-ref (impersonator-props v) p none)])
                      (if (eq? none pv)
                          (fail)
                          pv))
                    (let ([iv (extract-impersonator-of accessor-name v)])
                      (if iv
                          (accessor iv default)
                          (fail)))))]
               [(v) (accessor v none)])])
      (values p
              (make-named-procedure predicate predicate-name)
              (make-impersonator-property-accessor-procedure accessor accessor-name)))))

(define (impersonator-property-accessor-procedure? v)
  (or (raw:impersonator-property-accessor-procedure? v)
      (and (impersonator? v) (raw:impersonator-property-accessor-procedure? (impersonator-val v)))))

;; ----------------------------------------

(define-record props-impersonator impersonator ())
(define-record props-chaperone chaperone ())

;; Applicable variants:
(define-record props-procedure-impersonator props-impersonator (arity-mask))
(define-record props-procedure-chaperone props-chaperone (arity-mask))
;; Incomplete-arity variants:
(define-record props-procedure~-impersonator props-procedure-impersonator ())
(define-record props-procedure~-chaperone props-procedure-chaperone ())

(define (add-impersonator-properties who props base-props)
  (let loop ([props props] [base-props base-props])
    (cond
     [(null? props)
      base-props]
     [(impersonator-property? (car props))
      (when (null? (cdr props))
        (raise-arguments-error who "missing value argument after an imperonsonator-property argument"
                               "impersonator property" (car props)))
      (loop (cddr props) (hash-set base-props (car props) (cadr props)))]
     [else
      (raise-argument-error who "impersonator-property?" (car props))])))

(define (rewrap-props-impersonator orig new)
  (let ([val (strip-impersonator new)]
        [props (impersonator-props orig)])
    (cond
     [(props-procedure~-impersonator? orig)
      (make-props-procedure~-impersonator val new props (props-procedure-impersonator-arity-mask orig))]
     [(props-procedure-impersonator? orig)
      (make-props-procedure-impersonator val new props (props-procedure-impersonator-arity-mask orig))]
     [(props-procedure~-chaperone? orig)
      (make-props-procedure~-chaperone val new props (props-procedure-chaperone-arity-mask orig))]
     [(props-procedure-chaperone? orig)
      (make-props-procedure-chaperone val new props (props-procedure-chaperone-arity-mask orig))]
     [(props-chaperone? orig)
      (make-props-chaperone  val new props)]
     [(props-impersonator? orig)
      (make-props-impersonator val new props)]
     [else
      (raise-arguments-error 'rewrap-props-impersonator "internal error: unknown impersonator variant")])))

;; ----------------------------------------

(define-record struct-impersonator impersonator (procs)) ; hash of proc -> (cons orig-orig wrapper-proc)
(define-record struct-chaperone chaperone (procs))

(define (struct-impersonator/chaperone-procs i)
  (if (struct-impersonator? i)
      (struct-impersonator-procs i)
      (struct-chaperone-procs i)))

(define-record procedure-struct-impersonator struct-impersonator (arity-mask))
(define-record procedure-struct-chaperone struct-chaperone (arity-mask))
(define-record procedure~-struct-impersonator procedure-struct-impersonator ())
(define-record procedure~-struct-chaperone procedure-struct-chaperone ())

(define (impersonate-struct v . args)
  (do-impersonate-struct 'impersonate-struct #f v args))

(define (chaperone-struct v . args)
  (do-impersonate-struct 'chaperone-struct #t v args))

(define (do-impersonate-struct who as-chaperone? v args)
  (cond
   [(null? args) v]
   [else
    (let* ([st (if (struct-type? (car args))
                   (car args)
                   #f)]
           [orig-args (if st (cdr args) args)]
           [val (strip-impersonator v)]
           [orig-iprops (if (impersonator? v) (impersonator-props v) empty-hasheq)])
      (unless (or (not st) (record? val (strip-impersonator st)))
        (raise-arguments-error who "given value is not an instance of the given structure type"
                               "struct type" st
                               "value" v))
      (let loop ([first? (not st)]
                 [args orig-args]
                 [props empty-hasheq]
                 [saw-props empty-hasheq]
                 [witnessed? (and st #t)]
                 [iprops orig-iprops])
        (let ([get-proc
               (lambda (what args arity orig-proc key1 key2 key-applies? now-witnessed?)
                 (unless key-applies?
                   (raise-arguments-error who
                                          "operation does not apply to given value"
                                          "operation kind" (make-unquoted-printing-string what)
                                          "operation procedure" orig-proc
                                          "value" v))
                 (when (hash-ref2 saw-props key1 key2 #f)
                   (raise-arguments-error who
                                          "given operation accesses the same value as a previous operation argument"
                                          "operation kind" (make-unquoted-printing-string what)
                                          "operation procedure" orig-proc))
                 (when (null? (cdr args))
                   (raise-arguments-error who
                                          "missing redirection procedure after operation"
                                          "operation kind" (make-unquoted-printing-string what)
                                          "operation procedure" orig-proc))
                 (let ([proc (cadr args)])
                   (when proc
                     (unless (unsafe-procedure-and-arity-includes? proc arity)
                       (raise-arguments-error who
                                              "operation's redirection procedure does not match the expected arity"
                                              "given" proc
                                              "expected" (make-unquoted-printing-string
                                                          (string-append
                                                           "(or/c #f (procedure-arity-includes/c " (number->string arity) "))"))
                                              "operation kind" (make-unquoted-printing-string what)
                                              "operation procedure" orig-proc)))
                   (when (and as-chaperone?
                              (and (impersonator? orig-proc)
                                   (not (chaperone? orig-proc))))
                     (raise-arguments-error who
                                            "impersonated operation cannot be used to create a chaperone"
                                            "operation" orig-proc))
                   (let ([new-args (cddr args)])
                     (loop #f
                           new-args
                           (if proc
                               (hash-set2 props key1 key2
                                          (if (impersonator? orig-proc)
                                              (cons orig-proc ; save original accessor, in case it's impersonated
                                                    proc)     ; the interposition proc
                                              proc))
                               props)
                           (if (null? new-args) saw-props (hash-set2 saw-props key1 key2 #t))
                           (or witnessed? now-witnessed?)
                           iprops))))])
          (cond
           [(null? args)
            (unless as-chaperone?
              (check-accessors-paired-with-mutators who orig-args v))
            (unless witnessed?
              (raise-arguments-error who
                                     (string-append "cannot "
                                                    (if as-chaperone? "chaperone" "impersonate")
                                                    " value as a structure without a witness")
                                     "explanation" (string-append
                                                    "a structure type, accessor, or mutator acts as a witness\n"
                                                    "   that the given value's representation can be chaperoned or impersonated")
                                     "given value" v))
            (when (and (authentic? v)
                       (not (authentic-override? v)))
              (raise-arguments-error who
                                     (string-append "cannot "
                                                    (if as-chaperone? "chaperone" "impersonate")
                                                    " instance of an authentic structure type")
                                     "given value" v))
            (cond
             [(eq? props empty-hasheq)
              ;; No structure operations chaperoned, so either unchanged or
              ;; a properties-only impersonator
              (cond
               [(eq? iprops orig-iprops)
                v]
               [else
                ;; Same six cases as below, but for a propery-only impersonator
                (if (procedure? val)
                    (if (procedure-incomplete-arity? val)
                        (if as-chaperone?
                            (make-props-procedure~-chaperone val v iprops (procedure-arity-mask v))
                            (make-props-procedure~-impersonator val v iprops (procedure-arity-mask v)))
                        (if as-chaperone?
                            (make-props-procedure-chaperone val v iprops (procedure-arity-mask v))
                            (make-props-procedure-impersonator val v iprops (procedure-arity-mask v))))
                    (if as-chaperone?
                        (make-props-chaperone val v iprops)
                        (make-props-impersonator val v iprops)))])]
             [(procedure? val)
              ;; Wrap as a procedure-struct impersonator
              (if (procedure-incomplete-arity? val)
                  (if as-chaperone?
                      (make-procedure~-struct-chaperone val v iprops props (procedure-arity-mask v))
                      (make-procedure~-struct-impersonator val v iprops props (procedure-arity-mask v)))
                  (if as-chaperone?
                      (make-procedure-struct-chaperone val v iprops props (procedure-arity-mask v))
                      (make-procedure-struct-impersonator val v iprops props (procedure-arity-mask v))))]
             [else
              ;; Wrap as a plain old struct impersonator
              (if as-chaperone?
                  (make-struct-chaperone val v iprops props)
                  (make-struct-impersonator val v iprops props))])]
           [(impersonator-property? (car args))
            (loop #f
                  '()
                  props
                  saw-props
                  witnessed?
                  (add-impersonator-properties who args iprops))]
           [(struct-accessor-procedure? (car args))
            (let* ([orig-proc (car args)]
                   [key-proc (strip-impersonator orig-proc)]
                   [rtd+pos (struct-accessor-procedure-rtd+pos key-proc)])
              (unless (or as-chaperone?
                          (struct-type-field-mutable? (car rtd+pos) (cdr rtd+pos)))
                (raise-arguments-error who
                                       "cannot replace operation for an immutable field"
                                       "operation kind" (make-unquoted-printing-string "property accessor")
                                       "operation procedure" (car args)))
              (get-proc "accessor" args 2
                        orig-proc (car rtd+pos) (cdr rtd+pos)
                        (record? val (car rtd+pos))
                        #t))]
           [(struct-mutator-procedure? (car args))
            (let* ([orig-proc (car args)]
                   [key-proc (strip-impersonator orig-proc)]
                   [pos+rtd (struct-mutator-procedure-pos+rtd key-proc)])
              (get-proc "mutator" args 2
                        orig-proc (cdr pos+rtd) (struct-mutator-pos->key2 (car pos+rtd))
                        (record? val (cdr pos+rtd))
                        #t))]
           [(struct-type-property-accessor-procedure? (car args))
            (let* ([orig-proc (car args)]
                   [key-proc (strip-impersonator orig-proc)])
              (unless (or as-chaperone?
                          (struct-type-property-accessor-procedure-can-impersonate? key-proc))
                (raise-arguments-error who
                                       "operation cannot be impersonated"
                                       "operation kind" (make-unquoted-printing-string "property accessor")
                                       "operation procedure" orig-proc))
              (get-proc "property accessor" args 2
                        orig-proc key-proc #f
                        ((struct-type-property-accessor-procedure-pred key-proc) val)
                        #t))]
           [(and as-chaperone?
                 (equal? struct-info (car args)))
            (get-proc "struct-info procedure" args 2
                      struct-info struct-info #f
                      #t
                      #f)]
           [else
            (raise-argument-error who
                                  (string-append
                                   "(or/c "
                                   (if first? "struct-type?\n      " "")
                                   "struct-accessor-procedure?"
                                   "\n      struct-mutator-procedure?"
                                   "\n      struct-type-property-accessor-procedure?"
                                   (if as-chaperone? "\n      struct-info" "")
                                   ")")
                                  (car args))]))))]))

(define (check-accessors-paired-with-mutators who args v)
  (let ([mutator-reps
         (let loop ([args args])
           (cond
            [(null? args) empty-hash]
            [(struct-mutator-procedure? (car args))
             (hash-set (loop (cddr args))
                       (let ([pos+rtd (struct-mutator-procedure-pos+rtd (strip-impersonator (car args)))])
                         (cons (cdr pos+rtd) (car pos+rtd)))
                       #t)]
            [else
             (loop (cddr args))]))])
    (let loop ([args args])
      (cond
       [(null? args) empty-hash]
       [(struct-accessor-procedure? (car args))
        (let ([rtd+pos (struct-accessor-procedure-rtd+pos (strip-impersonator (car args)))])
          (unless (or (struct-type-immediate-transparent? (car rtd+pos))
                      (hash-ref mutator-reps rtd+pos #f))
            (raise-arguments-error who
                                   "accessor redirection for a non-transparent field requires a mutator redirection"
                                   "explanation" "a mutator redirection acts as a witness that access is allowed"
                                   "accessor" (car args)
                                   "value to impersonate" v)))
        (loop (cddr args))]
       [else
        (loop (cddr args))]))))

;; ----------------------------------------

(define-record struct-undefined-chaperone chaperone ())
(define-record procedure-struct-undefined-chaperone chaperone ())
(define-record procedure~-struct-undefined-chaperone procedure-struct-undefined-chaperone ())

(define-values (prop:chaperone-unsafe-undefined chaperone-unsafe-undefined? chaperone-unsafe-undefined-ref)
  (make-struct-type-property 'chaperone-unsafe-undefined
                             (lambda (v info)
                               (check 'guard-for-prop:chaperone-unsafe-undefined
                                      (lambda (v) (and (list? v) (andmap symbol? v)))
                                      :contract "(listof symbol?)"
                                      v)
                               v)))

(define (chaperone-struct-unsafe-undefined v)
  (cond
   [(not (record? v))
    v]
   [else
    ((if (procedure? v)
         (if (incomplete-arity? v)
             make-procedure~-struct-undefined-chaperone
             make-procedure-struct-undefined-chaperone)
         make-struct-undefined-chaperone)
     (strip-impersonator v)
     v
     (if (impersonator? v)
         (impersonator-props v)
         empty-hasheq))]))

(define (raise-unsafe-undefined who short-msg what orig-proc v pos)
  (let* ([names (if (chaperone-unsafe-undefined? v)
                    (chaperone-unsafe-undefined-ref v)
                    '())]
         [len (length names)])
  (cond
   [(< pos len)
    (let ([n (list-ref names (- len pos 1))])
      (raise
       (|#%app|
        exn:fail:contract:variable
        (format "~a: ~a;\n cannot ~a field before initialization"
                n short-msg what)
        (current-continuation-marks)
        n)))]
   [else
    (raise
     (|#%app|
      exn:fail:contract
      (format "~a: ~a;\n cannot ~as field before initialization"
              (object-name orig-proc) short-msg what)
      (current-continuation-marks)))])))

;; ----------------------------------------

(define-values (prop:impersonator-of impersonator-of-redirect? impersonator-of-ref)
  (make-struct-type-property 'impersonator-of
                             (lambda (v info)
                               (check 'guard-for-prop:impersonator-of (procedure-arity-includes/c 1) v)
                               ;; Add a tag to track origin of the `prop:impersonator-of` value
                               (cons (box 'impersonator-of) v))))

(define (extract-impersonator-of who a)
  (and (impersonator-of-redirect? a)
       (let* ([tag+ref (impersonator-of-ref a)]
              [a2 (|#%app| (cdr tag+ref) a)])
         (cond
          [(not a2)
           ;; `prop:impersonator-of` function can report #f to mean
           ;; "not an impersonator, after all"
           #f]
          [else
           (let ([different
                  (lambda (what)
                    (raise-arguments-error who (format (string-append "impersonator-of property procedure returned a"
                                                                      " value with a different `~a` source")
                                                       what)
                                           "original value" a
                                           "returned value" a2))])
             (unless (and (impersonator-of-redirect? a2)
                          (eq? (car tag+ref)
                               (car (impersonator-of-ref a2))))
               (different 'prop:impersonator-of))
             (unless (record-equal-procedure a (strip-impersonator a2))
               (different 'prop:equal+hash))
             a2)]))))

;; ----------------------------------------

(define (set-impersonator-applicables!)
  (let ([add (lambda (rtd)
               (struct-property-set! prop:procedure rtd impersonate-apply)  
               (struct-property-set! prop:procedure-arity rtd 3))])
    (add (record-type-descriptor props-procedure-impersonator))
    (add (record-type-descriptor props-procedure-chaperone))
    (add (record-type-descriptor props-procedure~-impersonator))
    (add (record-type-descriptor props-procedure~-chaperone)))

  (struct-property-set! prop:procedure
                        (record-type-descriptor impersonator-property-accessor-procedure)
                        0)
  (struct-property-set! prop:object-name
                        (record-type-descriptor impersonator-property-accessor-procedure)
                        1))

(define (set-impersonator-hash!)
  (let ([struct-impersonator-hash-code
         (lambda (c hash-code)
           ((record-hash-procedure (impersonator-val c))
            c
            hash-code))])
    (let ([add (lambda (rtd)
                 (record-type-hash-procedure rtd struct-impersonator-hash-code))])
      (add (record-type-descriptor struct-impersonator))
      (add (record-type-descriptor struct-chaperone))
      (add (record-type-descriptor procedure-struct-impersonator))
      (add (record-type-descriptor procedure-struct-chaperone)))
    (let ([add (lambda (rtd)
                 (record-type-hash-procedure rtd
                                             (lambda (c hash-code)
                                               (cond
                                                [(record? (impersonator-val c))
                                                 (struct-impersonator-hash-code c hash-code)]
                                                [else
                                                 (hash-code (impersonator-next c))]))))])
      (add (record-type-descriptor props-impersonator))
      (add (record-type-descriptor props-chaperone))
      (add (record-type-descriptor props-procedure-impersonator))
      (add (record-type-descriptor props-procedure-chaperone)))))
