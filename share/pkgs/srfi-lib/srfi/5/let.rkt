#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))
(provide s:let)

(define-syntax (s:let stx)
  (define-syntax-class loopid
    #:description "loop identifier"
    (pattern :id))
  (define-syntax-class binding-pair
    #:description "binding pair"
    (pattern [name:id arg:expr]))
  (define-syntax-class rest-binding
    #:description "\"rest\" binding"
    (pattern [rest-name:id rest-arg:expr ...]))
  (define-splicing-syntax-class let-style-bindings
    #:description "let-style bindings"
    #:attributes (loop* [name 1] [arg 1] rest-name [rest-arg 1])
    ;; in let-style bindings, rest-binding only allowed w/
    ;; at least one binding-pair
    (pattern (~seq (~optional loop*:loopid)
                   (:binding-pair ...))
             #:with (rest-arg ...) #'()
             #:attr rest-name #f)
    (pattern (~seq (~optional loop*:loopid)
                   (:binding-pair ...+ . :rest-binding))))
  (define-splicing-syntax-class define-style-bindings
    #:description "define-style bindings"
    #:attributes (loop* [name 1] [arg 1] rest-name [rest-arg 1])
    (pattern (~seq (loop*:loopid :binding-pair ...))
             #:with (rest-arg ...) #'()
             #:attr rest-name #f)
    (pattern (~seq (loop*:loopid
                    :binding-pair ...
                    . :rest-binding))))
  (define-splicing-syntax-class bindings
    #:description #f
    #:attributes (loop* [name 1] [arg 1] rest-name [rest-arg 1])
    (pattern :let-style-bindings)
    (pattern :define-style-bindings)) 
  (syntax-parse stx
    [(_ () body:expr ...+)
     #'(let () body ...)]
    [(_ :bindings
        body:expr ...+)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(name ... (~? rest-name))))
     "duplicate variable name"
     #:with loop (or (attribute loop*) #'tmp-loop)
     #'(letrec ([loop (λ (~? (name ... . rest-name)
                             (name ...))
                        body ...)])
         (loop arg ...  rest-arg ...))]))



