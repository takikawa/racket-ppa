#lang scheme/base

(require scheme/class
         "test-info.scm")

(define test-display-textual%
  (class* object% ()

    (init-field (current-rep #f))

    (define test-info #f)
    (define/pubment (install-info t)
      (set! test-info t)
      (inner (void) install-info t))

    (define/public (display-results)
      (insert-test-results test-info))

    (define/pubment (insert-test-results test-info)
      (let* ([style (send test-info test-style)]
             [total-tests (send test-info tests-run)]
             [failed-tests (send test-info tests-failed)]
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]
             [test-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond [(zero? total-tests) zero-message]
                              [(= 1 total-tests) "Ran 1 test.\n"]
                              [else (format "Ran ~a tests.\n" total-tests)]))
                (when (> total-tests 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-tests) (= 1 total-tests))
                             "Test passed!\n\n"]
                            [(zero? failed-tests) "All tests passed!\n\n"]
                            [(= failed-tests total-tests) "0 tests passed.\n"]
                            [else "~a of the ~a tests failed.\n\n"]))))]
             [check-outcomes
              (lambda (zero-message)
                (printf "~a"
                        (cond
                          [(zero? total-checks) zero-message]
                          [(= 1 total-checks) "Ran 1 check.\n"]
                          [else (format "Ran ~a checks.\n" total-checks)]))
                (when (> total-checks 0)
                  (printf "~a"
                          (cond
                            [(and (zero? failed-checks) (= 1 total-checks))
                             "Check passed!\n\n"]
                            [(zero? failed-checks) "All checks passed!\n\n"]
                            [(= failed-checks total-checks) "0 checks passed.\n"]
                            [else (format "~a of the ~a checks failed.\n\n"
                                          failed-checks total-checks)]))))])
        (case style
          [(test-require)
           (test-outcomes "This program must be tested!\n")
           (check-outcomes "This program is unchecked!\n")]
          [(check-require)
           (check-outcomes "This program is unchecked!\n")]
          [(test-basic)
           (test-outcomes "")
           (check-outcomes "")]
          [else (check-outcomes "")])

        (unless (and (zero? total-checks) (zero? total-tests))
          (inner (display-check-failures (send test-info failed-checks)
                                         test-info)
                 insert-test-results test-info))))

    (define/public (display-check-failures checks test-info)
      (for ([failed-check (reverse checks)])
        (printf "~a" "\t")
        (make-link (failed-check-msg failed-check)
                   (failed-check-src failed-check))
        (printf "~a" "\n")))

    (define/public (report-success) (void))
    
    (define/public (next-line) (printf "~a" "\n\t"))

    ;; make-link: (listof (U string snip%)) src -> void
    (define (make-link msg dest)
      (for-each printf msg)
      (printf (format-src dest)))

    (define (format-src src)
      (let ([src-file car]
            [src-line cadr]
            [src-col caddr])
        (string-append
         (cond [(symbol? (src-file src)) " At "]
               [(path? (src-file src))
                (string-append " In " (path->string (src-file src)) " at ")]
               [else " At "])
         "line " (cond [(src-line src) => number->string]
                       [else "(unknown)"])
         " column " (cond [(src-col src) => number->string]
                          [else "(unknown)"]))))

      (super-instantiate ())))

(define test-engine%
  (class* object% ()
    (field [test-info #f]
           [test-display #f])

    (define display-class test-display-textual%)
    (define display-rep #f)
    (define display-event-space #f)

    (super-instantiate ())

    (define/public (refine-display-class d) (set! display-class d))
    (define/public (info-class) test-info-base%)

    (define/public (add-analysis a) (send test-info add-analysis a))

    (define/public (setup-info style)
      (set! test-info (make-object (info-class) style)))
    (define/pubment (setup-display cur-rep event-space)
      (set! test-display (make-object display-class cur-rep))
      (set! display-rep cur-rep)
      (set! display-event-space event-space)
      (inner (void) setup-display cur-rep event-space))

    (define/pubment (run)
      (when (test-execute)
        (unless test-info (setup-info 'check-base))
        (inner (void) run)))
    (define/public (summarize-results port)
      (cond
        [(test-execute)
         (unless test-display (setup-display #f #f))
         (let ([result (send test-info summarize-results)])
           (send test-display install-info test-info)
           (case result
             [(no-tests) (display-untested port)]
             [(all-passed) (display-success port display-event-space
                                            (+ (send test-info tests-run)
                                               (send test-info checks-run)))]
             [(mixed-results)
              (display-results display-rep display-event-space)]))]
        [else
         (fprintf port "Tests disabled.\n")]))

    (define/private (display-success port event count)
      (when event
        (parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event])
          ((dynamic-require 'scheme/gui 'queue-callback)
            (lambda () (send test-display report-success)))))
      (unless (test-silence)
        (fprintf port "~a test~a passed!\n"
                 (case count
                   [(0) "Zero"]
                   [(1) "The only"]
                   [(2) "Both"]
                   [else (format "All ~a" count)])
                 (if (= count 1) "" "s"))))
    (define/public (display-untested port)
      (unless (test-silence)
        (fprintf port "This program should be tested.~n")))
    (define/public (display-results rep event-space)
      (cond
        [(and rep event-space)
         (parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event-space])
           ((dynamic-require 'scheme/gui 'queue-callback)
            (lambda () (send rep display-test-results test-display))))]
        [event-space 
         (parameterize ([(dynamic-require 'scheme/gui 'current-eventspace) event-space])
           ((dynamic-require 'scheme/gui 'queue-callback) (lambda () (send test-display display-results))))]
        [else (send test-display display-results)]))

    (define/pubment (initialize-test test)
      (inner (void) initialize-test test))

    (define/pubment (run-test test)
      (inner (void) run-test test))

    (define/pubment (run-testcase testcase)
      (inner (void) run-testcase testcase))))

(define test-format (make-parameter (lambda (v) (format "~a" v))))
(define test-execute (make-parameter #t))
(define error-handler (make-parameter (lambda (e) (e))))
(define test-silence (make-parameter #f))

(provide test-engine% test-display-textual% test-format error-handler test-execute test-silence)
