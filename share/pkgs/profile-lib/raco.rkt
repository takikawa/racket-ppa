#lang racket/base

(require racket/cmdline
         raco/command-name
         errortrace/errortrace-lib
         "main.rkt" "raco-utils.rkt"
         (prefix-in text: "render-text.rkt"))

;; raco profile
;; profile the main submodule (if there is one), or the top-level module

(define delay #f)
(define iterations #f)
(define threads? #f)
(define use-errortrace? #f)
(define order 'topological)
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
                #:once-any
                [("--topological")
                 "Order functions topologically (the default)"
                 (set! order 'topological)]
                [("--self")
                 "Order functions by self time"
                 (set! order 'self)]
                [("--total")
                 "Order functions by total time"
                 (set! order 'total)]
                #:args (filename)
                filename))

(define (t)
  ;; use a fresh namespace every time, to play nice with --repeat
  ;; otherwise, the 2nd+ `dynamic-require`s are no-ops
  (parameterize* ([current-namespace       (make-base-empty-namespace)]
                  ;; need to run `make-errortrace-compile-handler` in the
                  ;; proper namespace
                  [current-compile         (if use-errortrace?
                                               (make-errortrace-compile-handler)
                                               (current-compile))]
                  [use-compiled-file-paths (if use-errortrace?
                                               (cons (build-path "compiled" "errortrace")
                                                     (use-compiled-file-paths))
                                               (use-compiled-file-paths))])
    (dynamic-require (module-to-profile file) #f)))

(collect-garbage)
(collect-garbage)
(collect-garbage)
(cond [(and delay iterations)
       (profile-thunk t
                      #:delay delay
                      #:repeat iterations
                      #:order order
                      #:threads threads?
                      #:use-errortrace? use-errortrace?)]
      [delay
        (profile-thunk t
                       #:delay delay
                       #:order order
                       #:threads threads?
                       #:use-errortrace? use-errortrace?)]
      [iterations
       (profile-thunk t
                      #:repeat iterations
                      #:order order
                      #:threads threads?
                      #:use-errortrace? use-errortrace?)]
      [else
       (profile-thunk t
                      #:order order
                      #:threads threads?
                      #:use-errortrace? use-errortrace?)])

(module test racket/base) ; don't run for testing
