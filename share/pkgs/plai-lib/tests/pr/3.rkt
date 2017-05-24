#lang racket/base
(require racket/sandbox)

(module+ test
  (require rackunit)
  (define v
    (with-handlers ([exn:fail? (λ (x) (exn-message x))])
      (make-module-evaluator
       `(module example racket/base
          (require plai/datatype)

          (define-type Foo
            [bar])

          (Foo 3)))))
  
  (check-regexp-match #rx"Foo: Illegal use of type name outside type-case." v))
