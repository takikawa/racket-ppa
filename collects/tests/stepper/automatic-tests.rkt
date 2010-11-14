#lang scheme

(require "through-tests.ss" 
         "test-engine.ss")

(let ((outer-namespace (current-namespace)))
  (parameterize ([display-only-errors #t]
                 ;; display-only-errors is insufficient, because the evals
                 ;; actually cause output.  So we just eat stdout.
                 [current-output-port (open-output-string)]
		 [current-namespace (make-base-namespace)])
    ;; make sure the tests' print-convert sees the teaching languages' properties
    #;(namespace-attach-module outer-namespace 'mzlib/pconvert-prop (current-namespace))
    (namespace-require 'test-engine/racket-tests)
    (if (run-all-tests-except '(bad-and bad-cons check-error begin-let-bug prims qq-splice time set! local-set! lazy1 lazy2 lazy3
                                        local-struct/i local-struct/ilam))
	(exit 0)
	(exit 1))))
