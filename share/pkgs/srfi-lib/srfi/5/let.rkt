#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))
(provide s:let)

(define-syntax (s:let stx)
  (define-syntax-class loopid
    #:description "loop identifier"
    #:opaque
    (pattern :id))
  (define-syntax-class binding-pair
    #:description "binding pair"
    (pattern [name:id arg:expr]))
  (define-splicing-syntax-class maybe-rest-binding
    #:description #f
    #:no-delimit-cut
    (pattern (~describe "\"rest\" binding"
                        (~seq (~describe #:opaque "\"rest\" identifier"
                                         rest-name:id)
                              ~!
                              (~describe #:opaque "\"rest\" init expression"
                                         rest-arg:expr)
                              ...)))
    (pattern (~seq)
             #:attr rest-name #f
             #:with (rest-arg ...) #'()))
  (define-splicing-syntax-class bindings
    #:description "bindings"
    #:attributes (loop* [name 1] [arg 1] rest-name [rest-arg 1])
    (pattern
     (~describe
      "\"named let\"-style bindings"
      (~seq loop*:loopid
            ~!
            (~describe
             "parenthesized sequence of binding pairs with optional \"rest\" binding"
             (:binding-pair ... :maybe-rest-binding)))))
    (pattern
     (~describe
      "\"define\"-style bindings"
      (~seq (loop*:loopid ~! :binding-pair ... :maybe-rest-binding))))
    (pattern
     (~describe
      "\"let\"-style bindings with no loop identifier"
      (~seq
       (~or* (~describe #:opaque "empty sequence of binding pairs"
                        (~and () (:binding-pair ... :maybe-rest-binding)))
             (~describe
              "parenthesized sequence of one or more binding pairs with optional \"rest\" binding"
              (:binding-pair ...+ :maybe-rest-binding)))))
     #:attr loop* #f))
  (syntax-parse stx
    [(_ :bindings
        (~describe "body forms" (~seq body:expr ...+)))
     ;; it is NOT an error for loop* to be shaddowed:
     ;; see https://srfi-email.schemers.org/srfi-5/msg/18712014/
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(name ... (~? rest-name))))
     "duplicate variable name"
     #:with loop (or (attribute loop*) #'anonymous-srfi-5-let)
     #`(letrec ([loop #,(syntax-property
                         (syntax/loc stx
                          (λ (~? (name ... . rest-name)
                                 (name ...))
                            body ...))
                         'inferred-name (or (attribute loop*) (void)))])
         (loop arg ...  rest-arg ...))]))
