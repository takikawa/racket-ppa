#lang racket/base
(require macro-debugger/model/debug)
;; Testing facilities for macro debugger

(provide trace/ns
         trace/t
         trace/k

         testing-namespace

         hide-all-policy
         hide-none-policy

         T-policy
         Tm-policy

         stx/hide-none
         stx/hide-all
         stx/hide-standard
         stx/hide-T
         stx/hide-Tm)

(define (trace/t expr)
  (trace/ns expr #f))

(define (trace/k expr)
  (trace/ns expr #t))

;; Use just 'expand', not 'expand/compile-time-evals',
;; for test backward compatibility
;; FIXME: add tests that use 'expand/compile-time-evals'
(define (trace/ns expr kernel?)
  (parameterize ((current-namespace (choose-namespace kernel?)))
    (trace expr expand)))

(define (choose-namespace kernel?)
  (if kernel? kernel-namespace testing-namespace))

(define helper-module
  '(module helper racket/base
     (require tests/macro-debugger/helper/helper)
     (provide (all-from-out tests/macro-debugger/helper/helper))))

(define kernel-namespace (make-base-empty-namespace))
(parameterize ((current-namespace kernel-namespace))
  (namespace-require ''#%kernel)
  (eval '(#%require (for-syntax '#%kernel)))
  (eval helper-module)
  (eval '(define-syntaxes (id)
           (lambda (stx)
             (cadr (syntax->list stx)))))
  (eval '(define-syntaxes (Tid)
           (lambda (stx)
             (cadr (syntax->list stx)))))
  (eval '(define-syntaxes (Tlist)
           (lambda (stx)
             (datum->syntax (quote-syntax here)
                            (list (quote-syntax list)
                                  (cadr (syntax->list stx)))))))
  (eval '(define-syntaxes (wrong)
           (lambda (stx)
             (raise-syntax-error #f "wrong" stx)))))

(define testing-namespace (make-base-namespace))
(parameterize ((current-namespace testing-namespace))
  (eval '(require scheme/base))
  (eval '(require (for-syntax scheme/base)))
  (eval helper-module)
  (eval '(require 'helper)))

;; Specialized macro hiding tests
(define (stx/hide-policy d policy)
  (define-values (_steps _binders _uses stx _exn)
    (parameterize ((macro-policy policy))
      (reductions+ d)))
  stx)

(define (stx/hide-none d)
  (stx/hide-policy d hide-none-policy))
(define (stx/hide-all d)
  (stx/hide-policy d hide-all-policy))
(define (stx/hide-standard d)
  (stx/hide-policy d standard-policy))

(define (stx/hide-T d)
  (stx/hide-policy d T-policy))
(define (stx/hide-Tm d)
  (stx/hide-policy d Tm-policy))

;; T hiding policy
;; ALL macros & primitives are hidden
;; EXCEPT those starting with T (Tlist and Tlet)
(define (T-policy id)
  (or (memq (syntax-e id) '())
      (regexp-match #rx"^T" (symbol->string (syntax-e id)))))

;; Tm hiding policy
;; ALL MACROS & primitive tags are hidden
;; EXCEPT those starting with T (Tlist and Tlet)
;; EXCEPT module (=> #%module-begin gets tagged)
(define (Tm-policy id)
  (or (memq (syntax-e id) '(module))
      (regexp-match #rx"^T" (symbol->string (syntax-e id)))))
