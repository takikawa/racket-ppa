#lang scheme/base
(require (for-syntax "closure.ss")
         (for-syntax scheme/base)
         (for-template scheme/base))
(provide define-closure)

(define-syntax (define-closure stx)
  (syntax-case stx ()
    [(_ tag formals (free-vars ...) body)
     (let-values ([(make-CLOSURE closure-definitions)
                   (make-closure-definition-syntax
                    #'tag
                    (syntax->list #'(free-vars ...))
                    #`(lambda formals body))])
       #`(begin #,@closure-definitions))]))
