(module generic-methods '#%kernel

  (#%require (for-syntax '#%kernel "small-scheme.rkt" "define.rkt"
                         "stx.rkt" "stxcase-scheme.rkt")
             "define.rkt" "../stxparam.rkt")

  (#%provide define/generic
             generic-property
             generic-methods
             generic-method-table
             (for-syntax generic-info?
                         make-generic-info
                         generic-info-property
                         generic-info-predicate
                         generic-info-accessor
                         generic-info-methods
                         find-generic-method-index))

  (begin-for-syntax

    (define-values (struct:generic-info
                    make-generic-info
                    generic-info?
                    generic-info-get
                    generic-info-set!)
      (make-struct-type 'generic-info #f 4 0))

    (define-values (generic-info-property
                    generic-info-predicate
                    generic-info-accessor
                    generic-info-methods)
      (values (make-struct-field-accessor generic-info-get 0 'property)
              (make-struct-field-accessor generic-info-get 1 'predicate)
              (make-struct-field-accessor generic-info-get 2 'accessor)
              (make-struct-field-accessor generic-info-get 3 'methods)))

    (define (check-identifier! name ctx stx)
      (unless (identifier? stx)
        (raise-syntax-error name "expected an identifier" ctx stx)))

    (define (get-info name ctx stx)
      (check-identifier! name ctx stx)
      (define info (syntax-local-value stx (lambda () #f)))
      (unless (generic-info? info)
        (raise-syntax-error name "bad generic interface name" ctx stx))
      info)

    (define (unimplemented-transformer un stx)
      (define name (unimplemented-method un))
      (raise-syntax-error name "method not implemented" stx))

    (define-values (struct:unimplemented
                    make-unimplemented
                    unimplemented?
                    unimplemented-get
                    unimplemented-set!)
      (make-struct-type 'unimplemented
                        #f
                        1
                        0
                        #f
                        (list (cons prop:set!-transformer
                                    unimplemented-transformer))))

    (define unimplemented-method
      (make-struct-field-accessor unimplemented-get 0 'method))

    (define (find-generic-method who ctx gen-id delta gen-info method-id proc)

      (unless (syntax? ctx)
        (raise-argument-error who "syntax?" ctx))
      (unless (identifier? gen-id)
        (raise-argument-error who "identifier?" gen-id))
      (unless (and (procedure? delta)
                   (procedure-arity-includes? delta 1))
        (raise-argument-error who "(syntax? . -> . syntax?)" delta))
      (unless (generic-info? gen-info)
        (raise-argument-error who "generic-info?" gen-info))
      (unless (identifier? method-id)
        (raise-argument-error who "identifier?" method-id))
      (unless (and (procedure? proc)
                   (procedure-arity-includes? proc 2))
        (raise-argument-error
         who
         "(exact-nonnegative-integer? identifier? . -> . any)"
         proc))

      (define-values (originals indices)
        (let loop ([original-ids (generic-info-methods gen-info)]
                   [index 0]
                   [rev-originals '()]
                   [rev-indices '()])
          (cond
            [(null? original-ids)
             (values (reverse rev-originals)
                     (reverse rev-indices))]
            [else
             (define original-id (car original-ids))
             (define context-id (syntax-local-get-shadower (delta original-id)))
             (cond
               [(free-identifier=? context-id method-id)
                (loop (cdr original-ids)
                      (add1 index)
                      (cons original-id rev-originals)
                      (cons index rev-indices))]
               [else
                (loop (cdr original-ids)
                      (add1 index)
                      rev-originals
                      rev-indices)])])))

      (when (null? originals)
        (raise-syntax-error
         #f
         (format "~.s is not a method of generic interface ~.s"
                 (syntax-e method-id)
                 (syntax-e gen-id))
         ctx
         method-id))
      (unless (null? (cdr originals))
        (raise-syntax-error
         #f
         (format "multiple methods match ~.s in generic interface ~.s: ~.s"
                 (syntax-e method-id)
                 (syntax-e gen-id)
                 (map syntax-e originals))
         ctx
         method-id))
      (proc (car indices) (car originals)))

    (define (find-generic-method-index ctx gen-id delta gen-info method-id)
      (find-generic-method 'find-generic-method-index
                           ctx gen-id delta gen-info method-id
                           (lambda (index original) index)))

    (define (find-generic-method-original ctx gen-id delta gen-info method-id)
      (find-generic-method 'find-generic-method-index
                           ctx gen-id delta gen-info method-id
                           (lambda (index original) original))))

  (define-syntax-parameter generic-method-context #f)

  (define-syntax (implementation stx)
    (syntax-case stx ()
      [(_ method)
       (let ([val (syntax-local-value #'method (lambda () #f))])
         (cond
           [(unimplemented? val) #'(quote #f)]
           [else #'method]))]))

  (define-syntax (generic-property stx)
    (syntax-case stx ()
      [(_ gen)
       (generic-info-property (get-info 'generic-property stx #'gen))]))

  (define-syntax (generic-methods stx)
    (syntax-case stx ()
      [(_ gen def ...)
       (let ()
         (define info (get-info 'generic-methods stx #'gen))
         (define delta (syntax-local-make-delta-introducer #'gen))
         (define methods (map delta (generic-info-methods info)))
         (with-syntax ([(method ...) methods])
           (syntax/loc stx
             (syntax-parameterize ([generic-method-context #'gen])
               (letrec-syntaxes+values
                   ([(method) (make-unimplemented 'method)] ...)
                   ()
                 def ...
                 (values (implementation method) ...))))))]))

  (define-syntax (generic-method-table stx)
    (syntax-case stx ()
      [(_ gen def ...)
       #'(call-with-values (lambda () (generic-methods gen def ...)) vector)]))

  (define-syntax (define/generic stx)
    (define gen-id (syntax-parameter-value #'generic-method-context))
    (define gen-val
      (and (identifier? gen-id)
           (syntax-local-value gen-id (lambda () #f))))
    (unless (generic-info? gen-val)
      (raise-syntax-error 'define/generic "only allowed inside methods" stx))
    (syntax-case stx ()
      [(_ bind ref)
       (let ()
         (unless (identifier? #'bind)
           (raise-syntax-error 'define/generic "expected an identifier" #'bind))
         (unless (identifier? #'ref)
           (raise-syntax-error 'define/generic "expected an identifier" #'ref))
         (define delta (syntax-local-make-delta-introducer gen-id))
         (define methods (generic-info-methods gen-val))
         (define method-id
           (find-generic-method-original stx gen-id delta gen-val #'ref))
         (with-syntax ([method method-id])
           #'(define bind method)))])))
