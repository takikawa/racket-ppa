#lang racket/base

(require (for-syntax racket/base syntax/parse/pre syntax/stx))

(define-syntax (#%module-begin stx)
  (syntax-parse stx #:literals (require)
    [(mb (require . args) ... [nm:id ty] ...)
     #'(#%plain-module-begin
        (begin
          (define-syntax (nm stx)
            (raise-syntax-error
             'type-check
             (format "type name used out of context\n  type: ~a\n  in: ~a"
                     (syntax->datum (if (stx-pair? stx) (stx-car stx) stx))
                     (syntax->datum stx))
             stx
             (and (stx-pair? stx) (stx-car stx))))
          ...
          (provide nm) ...
          (begin-for-syntax
            (module* initialize #f
              (require
               (only-in typed-racket/env/init-envs initialize-type-name-env))
              (require . args) ...
              (provide initialize-type-names)
              (define (initialize-type-names)
                (initialize-type-name-env
                 (list (list #'nm ty) ...)))))))]))

(provide #%module-begin require
         (all-from-out racket/base)
         (for-syntax (all-from-out racket/base)))
