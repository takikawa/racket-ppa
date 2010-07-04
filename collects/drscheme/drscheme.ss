#lang scheme/base

(require "private/key.ss")

(define debugging? (getenv "PLTDRDEBUG"))

(define install-cm? (and (not debugging?)
                         (getenv "PLTDRCM")))

(define cm-trace? (or (equal? (getenv "PLTDRCM") "trace")
                      (equal? (getenv "PLTDRDEBUG") "trace")))


;; the flush is only here to ensure that the output is 
;; appears when running in cygwin under windows.
(define (flprintf fmt . args)
  (apply printf fmt args)
  (flush-output))

(when debugging?
  (flprintf "PLTDRDEBUG: installing CM to load/create errortrace zos\n")
  (let-values ([(zo-compile
                 make-compilation-manager-load/use-compiled-handler
                 manager-trace-handler)
                (parameterize ([current-namespace (make-base-empty-namespace)]
                               [use-compiled-file-paths '()])
                  (values
                   (dynamic-require 'errortrace/zo-compile 'zo-compile)
                   (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)
                   (dynamic-require 'compiler/cm 'manager-trace-handler)))])
    (current-compile zo-compile)
    (use-compiled-file-paths (list (build-path "compiled" "errortrace")))
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
    (error-display-handler (dynamic-require 'errortrace/errortrace-lib
                                            'errortrace-error-display-handler))
    (when cm-trace?
      (flprintf "PLTDRDEBUG: enabling CM tracing\n")
      (manager-trace-handler
       (λ (x) (display "1: ") (display x) (newline) (flush-output))))))

(when install-cm?
  (flprintf "PLTDRCM: installing compilation manager\n")
  (let-values ([(make-compilation-manager-load/use-compiled-handler
                 manager-trace-handler)
                (parameterize ([current-namespace (make-base-empty-namespace)])
                  (values
                   (dynamic-require 'compiler/cm 'make-compilation-manager-load/use-compiled-handler)
                   (dynamic-require 'compiler/cm 'manager-trace-handler)))])
    (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
    (when cm-trace?
      (flprintf "PLTDRCM: enabling CM tracing\n")
      (manager-trace-handler
       (λ (x) (display "1: ") (display x) (newline) (flush-output))))))

(dynamic-require 'drscheme/private/drscheme-normal #f)
