#lang scheme/base

(require scheme/unit
         (for-syntax scheme/base
                     mzlib/private/unit-compiletime
                     mzlib/private/unit-syntax))

(provide (rename-out [module-begin #%module-begin])
         (except-out (all-from-out scheme/base) #%module-begin)
         (all-from-out scheme/unit)
         (for-syntax (all-from-out scheme/base)))

(define-for-syntax (make-name s)
  (string->symbol
   (string-append (regexp-replace "-sig$" (symbol->string s) "")
                  "^")))

;; Recognizes scheme require forms.
(define-for-syntax split-scheme-requires
  (split-requires* (list #'require #'#%require)))

(define-syntax (module-begin stx)
  (parameterize ((error-syntax stx))
    (with-syntax ((name (make-name (syntax-property stx 'enclosing-module-name))))
      (syntax-case stx ()
        ((_ . x)
         (with-syntax ((((reqs ...) . (body ...))
                        (split-scheme-requires (checked-syntax->list #'x))))
           (datum->syntax
            stx
            (syntax-e #'(#%module-begin
                         reqs ...
                         (provide name)
                         (define-signature name (body ...))))
            stx)))))))
