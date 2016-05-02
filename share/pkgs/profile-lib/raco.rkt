#lang racket/base

(require racket/cmdline
         raco/command-name
         errortrace/errortrace-lib
         "main.rkt" "raco-utils.rkt")

;; raco profile
;; profile the main submodule (if there is one), or the top-level module

(define delay #f)
(define iterations #f)
(define threads? #f)
(define use-errortrace? #f)
(define file
  (command-line #:program (short-program+command-name)
                #:once-each
                [("--delay") n
                 "Sampling rate (seconds)"
                 (let ([n* (string->number n)])
                   (unless (real? n*)
                     (raise-argument-error 'raco-profile "real?" n*))
                   (set! delay n*))]
                [("--repeat") n
                 "Number of iterations"
                 (let ([n* (string->number n)])
                   (unless (integer? n*)
                     (raise-argument-error 'raco-profile "integer?" n*))
                   (set! iterations n*))]
                [("--all-threads")
                 "Profile all threads"
                 (set! threads? #t)]
                [("--use-errortrace")
                 "Use errortrace mode"
                 (set! use-errortrace? #t)]
                #:args (filename)
                filename))

(parameterize ([current-compile
                (if use-errortrace?
                    (make-errortrace-compile-handler)
                    (current-compile))])
  (define (t)
    (collect-garbage)
    (collect-garbage)
    (collect-garbage)
    (dynamic-require (module-to-profile file) #f))
  (cond [(and delay iterations)
         (profile-thunk t
                        #:delay delay
                        #:repeat iterations
                        #:threads threads?
                        #:use-errortrace? use-errortrace?)]
        [delay
          (profile-thunk t
                         #:delay delay
                         #:threads threads?
                         #:use-errortrace? use-errortrace?)]
        [iterations
         (profile-thunk t
                        #:repeat iterations
                        #:threads threads?
                        #:use-errortrace? use-errortrace?)]
        [else
         (profile-thunk t
                        #:threads threads?
                        #:use-errortrace? use-errortrace?)]))

(module test racket/base) ; don't run for testing
