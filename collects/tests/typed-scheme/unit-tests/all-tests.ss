#lang scheme/base

(require 
 "test-utils.ss"
 "planet-requires.ss"
 "typecheck-tests.ss" ;;fail
 "subtype-tests.ss" ;; pass
 "type-equal-tests.ss" ;; pass
 "remove-intersect-tests.ss" ;; pass
 "parse-type-tests.ss" ;; pass
 "type-annotation-test.ss" ;; pass
 "module-tests.ss" ;; pass
 "subst-tests.ss" ;; pass
 "infer-tests.ss" ;; pass
 "contract-tests.ss"
 )

(require (r:infer infer infer-dummy)
         (schemeunit))

(provide unit-tests)

(infer-param infer)

(define unit-tests
  (apply
   test-suite 
   "Unit Tests"
   (for/list ([f (list
                  typecheck-tests
                  subtype-tests
                  type-equal-tests
                  restrict-tests
                  remove-tests
                  overlap-tests
                  parse-type-tests
                  type-annotation-tests
                  module-tests
                  fv-tests
                  contract-tests)])
     (f))))



(define-go (lambda () unit-tests))

;(go/gui)


