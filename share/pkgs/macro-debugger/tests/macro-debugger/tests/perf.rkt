#lang racket/base
(require racket/list
         racket/path
         macro-debugger/model/debug
         "collects.rkt")
(provide perf-libs
         perf-lib)

(define verbose (make-parameter #t))
(define (vprintf fmt . args) (when (verbose) (apply eprintf fmt args)))

(define time-info-handler (make-parameter void))
(define-syntax-rule (with-log-time [label ...] . body)
  (let-values ([(vs cpu-msec real-msec gc-msec) (time-apply (lambda () . body) null)])
    ((time-info-handler) (list label ...) cpu-msec real-msec gc-msec)
    (apply values vs)))

(define (perf-libs mods #:reductions? [reductions? #f] #:hiding? [hiding? #f])
  (for ([m mods]) (perf-lib m #:reductions? reductions? #:hiding? hiding?)))

(define (perf-lib m #:reductions? [reductions? #f] #:hiding? [hiding? #f])
  (vprintf "tracing ~s ... " m)
  (define deriv (with-log-time [m 'deriv] (trace-module m)))
  (when reductions?
    (parameterize ((macro-policy hide-none-policy))
      (vprintf "stepping ... ")
      (with-log-time [m 'reductions] (reductions+ deriv))))
  (when (and reductions? hiding?)
    (parameterize ((macro-policy standard-policy))
      (vprintf "hiding ... ")
      (with-log-time [m 'hiding-standard]
        (reductions+ deriv))))
  (vprintf "ok\n"))

;; ----

(define (the-time-info-handler label cpu-msec real-msec gc-msec)
  (printf "~s\n" (list label cpu-msec real-msec gc-msec))
  (flush-output (current-output-port)))

(define modules-for-test
  (trace-modules '(racket/main typed/racket framework)))

(module+ test
  (module config info (define timeout 1200))
  (void))

(module+ main
  (require rackunit/text-ui)
  (parameterize ((verbose #t)
                 (time-info-handler the-time-info-handler))
    (perf-libs modules-for-test #:reductions? #t #:hiding? #t)))
