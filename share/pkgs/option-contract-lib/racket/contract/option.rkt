#lang racket/base
(require racket/list
         racket/contract/base
         racket/contract/combinator
         (for-syntax racket/base))

(provide (rename-out [_option/c option/c])
         #;transfer-option exercise-option waive-option tweak-option transfer/c
         has-option? has-option-with-contract?
         invariant/c)


(require syntax/location 
         (for-syntax racket/provide-transform)
         (for-syntax racket/list)
         (for-syntax racket/struct-info))

(define-for-syntax (get-struct-info id who)
  (cond
    [(and (identifier? id)
          (struct-info? (syntax-local-value id (λ () #f))))
     (let ([si (extract-struct-info (syntax-local-value id))])  
       (with-syntax ([id id]
                     [pred (third si)] 
                     [(accessor ...) (fourth si)]
                     [(mutator ...) (fifth si)])
         #'(list pred (list accessor ...) (list mutator ...) 'id)))]
    [else
     (raise-syntax-error who "expected a struct identifier" id)]))


(define-for-syntax (convert-args args this-one who) 
  (let loop ([args args]
             [new-args null])
    (cond
      [(null? args) (reverse new-args)]
      [(keyword? (syntax-e (car args)))
       (if (null? (cdr args))
           (reverse (cons (car args) new-args))
           (cond [(eq? (syntax-e (car args)) '#:struct)
                  (loop (cddr args)
                        (list* (get-struct-info (cadr args) who) (car args) new-args))]
                 [else
                  (loop (cddr args)
                        (list* (cadr args) (car args) new-args))]))]
      [else
       (loop (cdr args)
             (cons (syntax-property
                    (car args)
                    'racket/contract:positive-position
                    this-one)
                   new-args))])))

(define (same-type v s-info)
  (let ([pred (first s-info)])
    (and pred (pred v))))

(define (no-mutators? s-info)
  (if (symbol? s-info) 
      #t
      (andmap boolean? (third s-info))))

;; proj is a late-neg-projection (so it accepts a value and the missing party)
;; but the blame record always has the neg-party filled in (in order to
;; make the blame party manipulations easier to sort out). So we always
;; just pass #f as the missing party argument to the proj
(struct info (val late-neg-proj blame with))

(define-values (impersonator-prop:proxy proxy? proxy-info) 
  (make-impersonator-property 'proxy))

(define (build-wrap-proc val)
  (let-values ([(arity) (procedure-arity val)]
               [(rkeys akeys) (procedure-keywords val)])
    (cond 
      [(and akeys (empty? akeys))
       values]
      [else
       (make-keyword-procedure
        (λ (kwds kwd-args . other-args)
          (apply values kwd-args other-args))
        (λ args
          (apply values args)))]))) 

(define (build-proxy with ctc val proj blame)
  (let* ([proxy-info (info val proj blame with)]
         [ival
          (cond [(procedure? val)
                 (chaperone-procedure
                  val
                  (build-wrap-proc val)
                  impersonator-prop:contracted ctc
                  impersonator-prop:proxy proxy-info)]
                [(vector? val)
                 (chaperone-vector
                  val
                  (λ (v i val) val)
                  (λ (v i val) val)
                  impersonator-prop:contracted ctc
                  impersonator-prop:proxy proxy-info)]
                [(hash? val)
                 (chaperone-hash
                  val
                  (λ (h k) (values k (λ (h k v) v)))
                  (λ (h k v) (values k v))
                  (λ (h k) k)
                  (λ (h k) k)
                  impersonator-prop:contracted ctc
                  impersonator-prop:proxy proxy-info)]
                [else 
                 (chaperone-struct
                  val
                  (first (second (option-structid ctc)))
                  (λ (v f) f)
                  impersonator-prop:contracted ctc
                  impersonator-prop:proxy proxy-info)])])
    (cond [with ((proj blame) ival #f)]
          [else ival])))


(define (run-tester tester val orig-proj blame neg-party here)
  (define indy-blame (blame-replace-negative blame here))
  (unless (tester ((orig-proj indy-blame) val neg-party))
    (raise-blame-error 
     (blame-add-context indy-blame (format "option contract tester ~e of" tester))
     #:missing-party neg-party
     val
     "")))


(define (optionc-name c)
  (apply build-compound-type-name 'option/c 
         (contract-name (option-orig-ctc c))
         (append 
          (if (option-with c)
              (list '#:with-contract #t)
              null)
          (if (eq? (option-tester c) 'dont-care)
              null
              (list '#:tester (option-tester c)))
          (if (eq? (option-flat c) #f)
              null
              (list '#:flat? #t))
          (if (eq? (option-immutable c) 'dont-care)
              null
              (list '#:immutable (option-immutable c)))
          (if (eq? (option-invariant c) 'dont-care)
              null
              (list '#:invariant (option-invariant c)))
          (if (eq? (option-structid c) 'none)
              null
              (list '#:struct (fourth (option-structid c)))))))

(define (check-option structid invariant flat immutable val blame neg-party)
    (when (and (eq? invariant 'dont-care)
               (or  (not (eq? immutable 'dont-care))
                    (not (eq? flat #f))))
      (raise-blame-error 
       blame #:missing-party neg-party
       val
       '(expected "an invariant keyword argument (based on presence of other keyword arguments)")))
    (unless (or (and (procedure? val) (not (parameter? val)) (eq? structid 'none))
                (and (vector? val) (eq? structid 'none))
                (and (hash? val) (eq? structid 'none))
                (and (not (eq? structid 'none)) (same-type val structid)))
      (if (eq? structid 'none)
          (raise-blame-error blame val '(expected "a procedure or a vector or a hash" given: "~e")
                             val)  
          (raise-blame-error blame val '(expected "a struct of type ~a" given: "~e")
                             (fourth structid) val))))



(define (build-orig-proj c inv flat immutable structid here)
  (cond [(eq? inv 'dont-care) c]
        [else
         (invariantc c inv #:struct structid #:flat? flat #:immutable immutable here)]))

(struct option (orig-ctc with tester invariant  flat immutable structid here)
  #:property prop:contract
  (build-contract-property
   #:name 
   optionc-name
   #:first-order
   (λ (ctc)
     (λ (val)
       ((contract-first-order (option-orig-ctc ctc)) val)))
   #:late-neg-projection
   (λ (ctc)
     (let*  ([with (option-with ctc)]
                 [tester (option-tester ctc)]
                 [invariant (option-invariant ctc)]
                 [flat (option-flat ctc)]
                 [immutable (option-immutable ctc)]
                 [structid (option-structid ctc)]
                 [here (option-here ctc)]
                 [orig-ctc (option-orig-ctc ctc)]
                 [orig-proj (contract-late-neg-projection (option-orig-ctc ctc))]
                 [exec-ctc-proj (contract-late-neg-projection
                                 (build-orig-proj orig-ctc invariant flat immutable structid here))])
       (λ (blame)
         (define option-blame (blame-add-context blame "the option of"))
         (λ (val neg-party)
           (check-option structid invariant flat immutable val blame neg-party)
           (unless (symbol? tester)
             (run-tester tester val orig-proj blame neg-party here))
           (build-proxy with ctc val exec-ctc-proj
                        (blame-add-missing-party option-blame neg-party))))))))

(define (option/c ctc
                  #:with-contract [with #f]
                  #:tester [tester 'dont-care]
                  #:invariant [invariant 'dont-care] 
                  #:flat? [flat #f] 
                  #:immutable [immutable 'dont-care]
                  #:struct [structid 'none]
                  here)
  (option ctc with tester invariant flat immutable structid here))



(define-syntax (_option/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx option/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list stx) null))]
    [(optionc arg ...) 
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'option-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one 'option/c)])
         (syntax-property
          (syntax/loc stx
            (option/c new-arg ... (quote-module-name)))
          'racket/contract:contract
          (vector this-one (list #'optionc) null))))]))


(struct transferc ()
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection
   (λ (ctc)
     (λ (blame)
       (λ (val neg-party)
         (define w/missing-blame (blame-add-missing-party blame neg-party))
         (define pos-blame (blame-positive w/missing-blame))
         (define neg-blame (blame-negative w/missing-blame))
         (cond
           [(proxy? val)
            (define info (proxy-info val))
            (build-proxy
             (info-with info)
             (value-contract val)
             (info-val info)
             (info-late-neg-proj info)
             (blame-update (info-blame info) pos-blame neg-blame))]
           [else val]))))))

(define/final-prop transfer/c (transferc))

(define (has-option? val)
  (and (has-contract? val) 
       (option? (value-contract val))))

(define (has-option-with-contract? val)
  (and (has-contract? val) 
       (option? (value-contract val))
       (info-with (proxy-info val))))

(define (tweak-option val)
  (cond [(proxy? val)
         (let ((info (proxy-info val)))
           (build-proxy
            (not (info-with info))
            (value-contract val)
            (info-val info)
            (info-late-neg-proj info)
            (info-blame info)))]
        [else val]))

(define (exercise-option val)
  (cond [(proxy? val)
         (let ([info (proxy-info val)])
           (((info-late-neg-proj info)
             (info-blame info))
            (info-val info)
            #f))]
        [else val]))

(define (waive-option val)
  (cond [(proxy? val) (info-val (proxy-info val))]
        [else val]))



;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                   ;            
;      ;                                                 ;                                         ;             
;      ;                                                 ;                           ;             ;             
;                                                                                    ;            ;              
;    ;;;      ;; ;;;   ;;;   ;;;    ;;;;    ;;;  ;;    ;;;        ;;;;    ;; ;;;   ;;;;;;;        ;       ;;; ;  
;      ;       ;;   ;   ;     ;         ;     ;;;        ;            ;    ;;   ;    ;            ;      ;   ;;  
;      ;       ;    ;   ;    ;          ;     ;          ;            ;    ;    ;    ;           ;      ;     ;  
;      ;       ;    ;    ;   ;     ;;;;;;     ;          ;       ;;;;;;    ;    ;    ;           ;      ;        
;      ;       ;    ;     ; ;     ;     ;     ;          ;      ;     ;    ;    ;    ;          ;       ;        
;      ;       ;    ;     ; ;     ;    ;;     ;          ;      ;    ;;    ;    ;    ;    ;     ;        ;    ;  
;   ;;;;;;;   ;;;  ;;;    ;;;      ;;;; ;;  ;;;;;;    ;;;;;;;    ;;;; ;;  ;;;  ;;;    ;;;;     ;          ;;;;   
;                                                                                              ;                 
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                                                                                                                



(define-struct invariant-info (ctc invariant structid flat immutable here))




(define (invariantc-name c)
  (let ([immutable (invariant-info-immutable c)]
        [flat (invariant-info-flat c)]
        [invariant (invariant-info-invariant c)]
        [structid  (invariant-info-structid c)])
    (apply build-compound-type-name 'invariant/c 
           (contract-name (invariant-info-ctc c))
           invariant
           (append
            (if (eq? structid 'none)
                null
                (list '#:struct (fourth structid)))
            (if flat
                (list '#:flat? #t)
                null)
            (if (eq? immutable 'dont-care)
                null
                (list '#:immutable immutable))))))

(define (check-invariant c return-boolean?)
  (let ([orig-ctc (invariant-info-ctc c)]
        [immutable (invariant-info-immutable c)]
        [invariant (invariant-info-invariant c)]
        [structid (invariant-info-structid c)])
    (λ (val first-order? blame neg-party)
      (cond
        [(or (and (vector? val) (eq? structid 'none))
             (and (hash? val) (eq? structid 'none))
             (and (not (eq? structid 'none)) (same-type val structid)))
         (define so-far
           (cond
             [(eq? immutable #t)
              (if (or (immutable? val) (and (not (symbol? structid)) (no-mutators? structid)))
                  #t
                  (if return-boolean?
                      #f
                      (raise-blame-error blame #:missing-party neg-party
                                         val '(expected "immutable data" given: "~e") val)))]
             [(eq? immutable #f)
              (if (or (immutable? val) 
                      (and (not (symbol? structid)) (no-mutators? structid)))
                  (if return-boolean?
                      #f
                      (raise-blame-error blame #:missing-party neg-party
                                         val '(expected "mutable data" given: "~e") val))
                  #t)]
             [else #t]))
         (cond
           [so-far
            (cond
              [first-order?
               (if (contract-first-order-passes? orig-ctc val)
                   #t
                   (if return-boolean?
                       #f
                       (let ([kind (cond [(vector? val) "vector"]
                                         [(hash? val) "hash"]
                                         [else "struct"])])                            
                         (raise-blame-error blame #:missing-party neg-party
                                            val '(expected: "~s that satisfies ~s" given: "~e") 
                                            kind
                                            (contract-name orig-ctc)
                                            val))))]
              [else #t])]
           [else #f])]
        [else
         (if return-boolean?
             #f
             (if (eq? structid 'none)
                 (raise-blame-error
                  blame #:missing-party neg-party
                  val '(expected "a vector or a hash" given: "~e") val)  
                 (raise-blame-error
                  blame #:missing-party neg-party
                  val '(expected "a struct of type ~a" given: "~e") (fourth structid) val)))]))))

(define (invariantc-first-order ctc)
  (let ([check (check-invariant ctc #t)])
    (λ (val)
      (check val #t #f #f))))

(define-struct (immutable-invariantc invariant-info) ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name invariantc-name
   #:first-order invariantc-first-order
   #:late-neg-projection 
   (λ (ctc)
     (define invariant (invariant-info-invariant ctc))
     (define orig-proj (contract-late-neg-projection (invariant-info-ctc ctc)))
     (define check (check-invariant ctc #f))
     (λ (blame)
       (define indy-blame (blame-replace-negative blame (invariant-info-here ctc)))
       (define orig-val-acceptor (orig-proj blame))
       (λ (val neg-party)
         (check val #f blame neg-party)
         (unless (invariant ((orig-proj indy-blame) val neg-party))
           (let ([kind (cond [(vector? val) 'vector]
                             [(hash? val) 'hash]
                             [else 'struct])])  
             (raise-blame-error 
              blame #:missing-party neg-party
              val
              (format "expected ~s that satisfies ~s given: ~e" kind invariant val))))
         (orig-val-acceptor val neg-party))))))



(define (build-inv-proxy ctc val inv late-neg-proj blame indy-blame neg-party impersonate?)
  (define (run-invariant kind blame) 
    (unless (inv ((late-neg-proj indy-blame) val neg-party))
      (raise-blame-error 
       blame #:missing-party neg-party
       val
       (format "expected ~s that satisfies ~s given: ~e" kind inv val))))
  (define blame+neg-party (cons blame neg-party))
  (cond [(vector? val)
         (let ([vector-wrapper 
                (λ (wrapper ) 
                  (wrapper
                   ((late-neg-proj blame) val neg-party)
                   (λ (vec i v)
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'vector blame))
                     v)
                   (λ (vec i v)
                     (vector-set! vec i v)
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'vector (blame-swap blame)))
                     v)
                   impersonator-prop:contracted ctc))])
           (if impersonate? 
               (vector-wrapper impersonate-vector) 
               (vector-wrapper chaperone-vector)))]
        [(hash? val)
         (let ([hash-wrapper
                (λ (wrapper) 
                  (wrapper
                   ((late-neg-proj blame) val neg-party)
                   (λ (h k)
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'hash blame))
                     (values k (λ (h k v) v)))
                   (λ (h k v) 
                     (if (immutable? h) (hash-set h k v) (hash-set! h k v))
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'hash (blame-swap blame)))
                     (values k v))
                   (λ (h k) 
                     (if (immutable? h) (hash-remove h k) (hash-remove! h k))
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'hash (blame-swap blame)))
                     k)
                   (λ (h k)
                     (with-contract-continuation-mark
                      blame+neg-party
                      (run-invariant 'hash blame))
                     k)
                   impersonator-prop:contracted ctc))])
           (if impersonate? 
               (hash-wrapper impersonate-hash) 
               (hash-wrapper chaperone-hash)))]
        [else
         (let* ([s-info (invariant-info-structid ctc)]
                [base-val ((late-neg-proj blame) val neg-party)]
                [a-wrap (λ (v f)
                          (with-contract-continuation-mark
                           blame+neg-party
                           (run-invariant 'struct blame))
                          f)]
                [m-wrap (λ (m)
                          (λ (v f) 
                            (m base-val f)
                            (with-contract-continuation-mark
                             blame+neg-party
                             (run-invariant 'struct (blame-swap blame)))
                            f))]
                [wrapped-accessors (foldr (λ (first rest) 
                                            (if (procedure? first)
                                                (list* first a-wrap rest)
                                                rest))
                                          '()
                                          (second s-info))]
                [wrapped-mutators (foldr (λ (first rest) 
                                           (if (procedure? first)
                                               (list* first (m-wrap first) rest)
                                               rest))
                                         '()
                                         (third s-info))]
                [struct-wrapper
                 (λ (wrapper) 
                   (apply
                    wrapper
                    base-val
                    (append wrapped-accessors wrapped-mutators (list impersonator-prop:contracted ctc))))])
           (if impersonate? 
               (struct-wrapper impersonate-struct) 
               (struct-wrapper chaperone-struct)))]))

(define (invariantc-ho-late-neg-projection impersonate?)
  (λ (ctc)
    (let ([orig-ctc (invariant-info-ctc ctc)]
          [immutable (invariant-info-immutable ctc)]
          [check (check-invariant ctc #f)]
          [invariant (invariant-info-invariant ctc)]
          [here (invariant-info-here ctc)])
      (λ (blame)
        (define indy-blame (blame-replace-negative blame here))
        (define proj (contract-late-neg-projection orig-ctc))
        (define val-acceptor ((contract-late-neg-projection orig-ctc) indy-blame))
        (λ (val neg-party)
          (with-contract-continuation-mark
           (cons blame neg-party)
           (check val #f blame neg-party)
           (unless (invariant (val-acceptor val neg-party))
             (let ([kind (cond [(vector? val) 'vector]
                               [(hash? val) 'hash]
                               [else 'struct])])  
               (raise-blame-error 
                blame #:missing-party neg-party
                val
                (format "expected ~s that satisfies ~s given: ~e" kind invariant val))))
           (build-inv-proxy ctc val invariant proj blame indy-blame neg-party impersonate?)))))))


(define-struct (chaperone-invariantc invariant-info) ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name invariantc-name
   #:first-order invariantc-first-order
   #:late-neg-projection (invariantc-ho-late-neg-projection #f)))

(define-struct (impersonator-invariantc invariant-info) ()
  #:property prop:contract
  (build-contract-property
   #:name invariantc-name
   #:first-order invariantc-first-order
   #:late-neg-projection (invariantc-ho-late-neg-projection #t)))

(define-syntax (invariant/c stx)
  (syntax-case stx ()
    [x
     (identifier? #'x)
     (syntax-property
      (syntax/loc stx invariant/c)
      'racket/contract:contract
      (vector (gensym 'ctc) (list stx) null))]
    [(invc arg ...)
     (let ([args (syntax->list #'(arg ...))]
           [this-one (gensym 'invariant-ctc)])
       (with-syntax ([(new-arg ...) (convert-args args this-one 'invariant/c)])
         (syntax-property
          (syntax/loc stx
            (invariantc new-arg ... (quote-module-name)))
          'racket/contract:contract
          (vector this-one (list #'invc) null))))]))

(define (invariantc c inv #:struct [structid 'none] #:flat? [flat #f] #:immutable [immutable 'dont-care] here)
  (cond
    [(or flat
         (and (eq? immutable #t)
              (flat-contract? c)))
     (make-immutable-invariantc c inv structid flat immutable here)]
    [(chaperone-contract? c)
     (make-chaperone-invariantc c inv structid flat immutable here)]
    [else
     (make-impersonator-invariantc c inv structid flat immutable here)]))
