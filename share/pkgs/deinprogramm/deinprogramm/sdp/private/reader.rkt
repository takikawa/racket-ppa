#lang racket/base
(require (rename-in syntax/module-reader
                    [#%module-begin #%reader-module-begin]))
(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket/base)
                     #%module-begin))

(define-syntax-rule (module-begin lang level)
  (#%reader-module-begin
   lang

   #:read (wrap-reader read level)
   #:read-syntax (wrap-reader read-syntax '())
   #:info (make-info level)))

(define (wrap-reader read-proc level)
  (lambda args
    (parameterize ([read-decimal-as-inexact #f]
                   [read-accept-dot (eq? level 'advanced)]
                   [read-accept-quasiquote (eq? level 'advanced)]
                   [read-accept-graph (eq? level 'advanced)])
      (apply read-proc args))))

(define ((make-info level) key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (list (dynamic-require 'stepper/drracket-button 'stepper-drracket-button))]

    [(drscheme:opt-out-toolbar-buttons)
     ;; opt-out of all of the extra buttons b/c 
     ;; we don't want anything to confuse in the teaching languages.
     #f]
    
    [(drracket:show-big-defs/ints-labels) #t]
    
    [else (use-default key default)]))
