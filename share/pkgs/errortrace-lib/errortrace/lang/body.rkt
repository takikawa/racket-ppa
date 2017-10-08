#lang racket/base
(require (for-syntax racket/base
                     syntax/strip-context
                     "../errortrace-lib.rkt"
                     "../stacktrace.rkt"
                     "../private/utils.rkt"))

(provide (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ lang . body)
     (let ([e (errortrace-annotate #`(module . #,(strip-context #`(n lang . body))))])
       (syntax-case e ()
         [(mod nm lang (mb . body)) 
          #`(#%plain-module-begin 
             (require (only-in lang)
                      (only-in errortrace)) ; to set the display handler
             . body)]))]))
