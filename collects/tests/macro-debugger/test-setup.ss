
#lang scheme/base
(require macro-debugger/model/debug)
;; Testing facilities for macro debugger

(provide trace/ns
         trace/t
         trace/k
         hide-all-policy
         hide-none-policy
         simple-policy

         stx/hide-none
         stx/hide-all
         stx/hide-standard
         stx/hide-simple)

(define (trace/t expr)
  (trace/ns expr #f))

(define (trace/k expr)
  (trace/ns expr #t))

(define (trace/ns expr kernel?)
  (parameterize ((current-namespace (choose-namespace kernel?)))
    (trace expr)))

(define (choose-namespace kernel?)
  (if kernel? kernel-namespace testing-namespace))

(define helper-module
  '(module helper scheme/base
     (require (for-syntax scheme/base))
     (provide Tid
              Tlist
              Tlet
              Tleid
              Tlift
              myor
              the-current-output-port
              wrong
              pre-id
              id
              leid
              lift)
     (define-syntax (id stx)
       (syntax-case stx ()
         [(id x) #'x]))
     (define-syntax (pre-id stx)
       (syntax-case stx ()
         [(pre-id x) #'(id x)]))
     (define-syntax (leid stx)
       (syntax-case stx ()
         [(leid e)
          (with-syntax ([ee (local-expand #'e 'expression null)])
            #`(#%expression ee))]))
     (define-syntax (lift stx)
       (syntax-case stx ()
         [(lift e)
          (with-syntax ([v (syntax-local-lift-expression #'e)])
            #'(#%expression v))]))
     (define-syntax wrong
       (lambda (stx)
         (raise-syntax-error #f "macro blows up here!" stx)))
     (define-syntax Tid
       (syntax-rules ()
         [(Tid e) e]))
     (define-syntax Tlist
       (syntax-rules ()
         [(Tlist e) (list e)]))
     (define-syntax Tlet
       (syntax-rules ()
         [(Tlet x e b) ((lambda (x) b) e)]))
     (define-syntax (Tleid stx)
       (syntax-case stx ()
         [(Tleid e)
          (with-syntax ([ee (local-expand #'e 'expression null)])
            #`(#%expression ee))]))
     (define-syntax (Tlift stx)
       (syntax-case stx ()
         [(Tlift e)
          (with-syntax ([v (syntax-local-lift-expression #'e)])
            #'(#%expression v))]))
     (define-syntax myor
       (syntax-rules ()
         [(myor x)
          x]
         [(myor x y ...)
          (let ((t x))
            (if t t (myor y ...)))]))
     (define-syntax the-current-output-port
       (make-set!-transformer 
        (syntax-rules (set!)
          [(set! the-current-output-port op)
           (#%plain-app current-output-port op)])))))

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
  (define-values (_steps _uses stx _exn)
    (parameterize ((macro-policy policy))
      (reductions+ d)))
  stx)

(define (stx/hide-none d)
  (stx/hide-policy d hide-none-policy))
(define (stx/hide-all d)
  (stx/hide-policy d hide-all-policy))
(define (stx/hide-simple d)
  (stx/hide-policy d simple-policy))
(define (stx/hide-standard d)
  (stx/hide-policy d standard-policy))
#|
(define (hide/standard d) (hide/policy d standard-policy))
(define (hide/all d) (hide/policy d hide-all-policy))
(define (hide/null d) (hide/policy d hide-none-policy))
(define (hide/except d syms)
  (hide/policy d (lambda (id) (memq (syntax-e id) syms))))
(define (hide/simple d) (hide/policy d simple-policy))
|#

;; Simple hiding policy
;; ALL MACROS & primitive tags are hidden
;; EXCEPT Tlist and Tlet (and #%module-begin)
(define (simple-policy id)
  (or (memq (syntax-e id) '())
      (regexp-match #rx"^T" (symbol->string (syntax-e id)))))
