#lang scheme/base

(require (file "test.ss")
         (file "check-info.ss")
         (file "format.ss"))

(provide format-tests)

(define format-tests
  (test-suite
   "All tests for format"
   
   (test-case
    "display-check-info-stack"
    (let ([p (open-output-string)])
      (parameterize ([current-output-port p])
        (check string=?
               (begin (display-check-info-stack
                       (list (make-check-name "foo")
                             (make-check-actual 1)
                             (make-check-expected 2)))
                      (get-output-string p))
               "name:       \"foo\"\nactual:     1\nexpected:   2\n\n"))))
   ))
