#lang racket
(require racket/runtime-path
         rackunit rackunit/text-ui
         typed-scheme/optimizer/logging
         unstable/logging)

(provide optimization-tests missed-optimization-tests
         test-opt test-missed-optimization test-file?
         generate-log tests-dir missed-optimizations-dir)

(define (generate-log name dir)
  ;; some tests require other tests, so some fiddling is required
  (with-output-to-string
    (lambda ()
      (with-intercepted-logging ; catch opt logs
       (lambda (l)
         (when (eq? (vector-ref l 2) ; look only for optimizer messages
                    optimization-log-key)
           (displayln (vector-ref l 1)))) ; print log message
       (lambda ()
         (parameterize
             ([current-namespace (make-base-empty-namespace)]
              [current-load-relative-directory dir])
           (dynamic-require
            (build-path (current-load-relative-directory) name)
            #f)))
       #:level 'warning))))

;; we log optimizations and compare to an expected log to make sure that all
;; the optimizations we expected did indeed happen
(define (compare-logs name dir)
  (test-suite "Log Comparison"
              (check-equal?
               ;; ugly, but otherwise rackunit spews the entire logs to
               ;; stderr, and they can be quite long
               #t
               (equal?
                ;; actual log
                (with-input-from-string
                    (string-append "(" (generate-log name dir) ")")
                  read)
                ;; expected log
                (with-input-from-file (build-path dir name)
                  (lambda () ; from the test file
                    (read-line) ; skip the #;
                    (read)))))))


(define-runtime-path tests-dir                "./tests")
(define-runtime-path missed-optimizations-dir "./missed-optimizations")

;; these two return lists of tests to be run for that category of tests
(define (test-opt name)
  (list (compare-logs name tests-dir)))
(define (test-missed-optimization name)
  (list (compare-logs name missed-optimizations-dir)))

(define (test-file? name)
  (and (regexp-match ".*rkt$" name)
       ;; skip emacs temp unsaved file backups
       (not (regexp-match "^\\.#" name))))

;; proc returns the list of tests to be run on each file
(define (mk-suite suite-name dir proc)
  (make-test-suite
   suite-name
   (for/list ([name (directory-list dir)]
              #:when (test-file? name))
     (make-test-suite
      (path->string name)
      (proc name)))))

(define optimization-tests
  (mk-suite "Optimization Tests" tests-dir test-opt))
(define missed-optimization-tests
  (mk-suite "Missed Optimization Tests" missed-optimizations-dir test-missed-optimization))
