#lang racket/base
(require (for-syntax racket/base syntax/parse
                     racket/sequence))

(provide (rename-out [-#%module-begin #%module-begin])
         #%datum
         define
         #%top-interaction)

(define-syntax (-#%module-begin stx)
  (define-splicing-syntax-class lax-spaces
    #:attributes ([lax-spaces? 0])
    (pattern #:lax-spaces
             #:with lax-spaces? #t)
    (pattern (~seq)
             #:with lax-spaces? #f))
  (syntax-parse stx
    [(_ ls:lax-spaces stuff ...)
     #'(#%plain-module-begin
        (provide string-constants)
        (define string-constants (make-hash))
        (string-constant-item ls.lax-spaces? string-constants stuff) ...)]))

(begin-for-syntax
  (define-syntax-class str-or-id-as-str
    #:attributes
    [(str 0)]
    (pattern str:str)
    (pattern x:id
             #:fail-unless (compile-time-string? (syntax-local-value #'x (λ () #f)))
             "identifier must be bound by #:define"
             #:with str (compile-time-string-str (syntax-local-value #'x)))))

(define-syntax (string-constant-item stx)
  (syntax-parse stx
    [(_ _ the-hash (#:define x:id s:str))
     #'(define-syntax x (compile-time-string 's))]
    [(_ _ the-hash (x:id str:str-or-id-as-str))
     #'(add-sc the-hash 'x str)]
    [(_ #t the-hash (x:id this-str:str-or-id-as-str next-str:str-or-id-as-str ...))
     #'(add-sc the-hash 'x this-str next-str ...)]
    [(_ #f the-hash (x:id this-str:str-or-id-as-str next-str:str-or-id-as-str ...))
     (define expln
       (string-append
        "  multi-line string constants must be broken on spaces"
        " and the space must start at the beginning of the"
        " (non-first) string constant"))
     (define (check-string-start str-stx)
       (unless (regexp-match #rx"^ " (syntax-e str-stx))
         (raise-syntax-error 'string-constant-lang
                             (string-append
                              "expected a string that begins with a space\n"
                              expln)
                             stx
                             str-stx)))
     (define (check-string-end str-stx)
       (when (regexp-match #rx" $" (syntax-e str-stx))
         (raise-syntax-error 'string-constant-lang
                             (string-append
                              "expected a string that does not end with a space\n"
                              expln)
                             stx
                             str-stx)))
     (check-string-end #'this-str.str)
     (for ([a-next-str (in-list (syntax->list #'(next-str.str ...)))])
       (check-string-start a-next-str)
       (check-string-end a-next-str))
     #'(add-sc the-hash 'x this-str next-str ...)]))

(begin-for-syntax
 (struct compile-time-string (str)
   #:property prop:procedure
   (λ (this stx)
     (syntax-parse stx
       [x:id #`'#,(compile-time-string-str this)]))))

(define (add-sc string-constants name . strs)
  (hash-set! string-constants name (apply string-append strs)))
