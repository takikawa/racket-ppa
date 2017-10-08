#lang racket/base
(require '#%unsafe
         '#%flfxnum
         '#%extfl
         "../private/kw.rkt")

(provide (except-out (all-from-out '#%unsafe)
                     unsafe-undefined
                     check-not-unsafe-undefined
                     check-not-unsafe-undefined/assign
                     prop:chaperone-unsafe-undefined
                     chaperone-struct-unsafe-undefined
                     unsafe-chaperone-procedure
                     unsafe-impersonate-procedure
                     unsafe-start-atomic unsafe-end-atomic
                     unsafe-start-breakable-atomic unsafe-end-breakable-atomic
                     unsafe-in-atomic?
                     unsafe-thread-at-root
                     unsafe-make-custodian-at-root
                     unsafe-custodian-register
                     unsafe-custodian-unregister
                     unsafe-register-process-global
                     unsafe-set-on-atomic-timeout!
                     unsafe-abort-current-continuation/no-wind
                     unsafe-call-with-composable-continuation/no-wind)
         (rename-out [new:unsafe-impersonate-procedure unsafe-impersonate-procedure]
                     [new:unsafe-chaperone-procedure unsafe-chaperone-procedure])
         (prefix-out unsafe-
                     (combine-out flsin flcos fltan
                                  flasin flacos flatan
                                  fltruncate flround flfloor flceiling
                                  flexp fllog flexpt

                                  extflsin extflcos extfltan
                                  extflasin extflacos extflatan
                                  extfltruncate extflround extflfloor extflceiling
                                  extflexp extfllog extflexpt)))
