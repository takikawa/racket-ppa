#lang racket/base
(require racket/class
         "interfaces.rkt")
(provide new-macro-scopes-partition
         new-all-scopes-partition
         partition-choices
         identifier=-choices)

(define (new-macro-scopes-partition)
  (new macro-scopes-partition%))

(define (new-all-scopes-partition)
  (new scopes-partition%))

;; scopes-partition%
(define scopes-partition%
  (class* object% (partition<%>)

    ;; simplified : hash[(listof nat) => nat]
    (define simplified (make-hash))

    ;; next-number : nat
    (define next-number 0)

    (define/public (get-partition stx)
      (let ([marks (get-scopes stx)])
        (or (hash-ref simplified marks #f)
            (let ([n next-number])
              (hash-set! simplified marks n)
              (set! next-number (add1 n))
              n))))

    (define/public (get-scopes stx)
      (get-all-scopes stx))

    (define/public (same-partition? a b)
      (= (get-partition a) (get-partition b)))

    (define/public (count)
      next-number)

    (get-partition (datum->syntax #f 'nowhere))
    (super-new)))

;; macro-scopes-partition%
(define macro-scopes-partition%
  (class scopes-partition%
    (super-new)
    (define/override (get-scopes stx)
      (get-macro-scopes stx))))

(define (get-macro-scopes stx)
  (define ctx (hash-ref (syntax-debug-info stx) 'context null))
  (for/list ([scope (in-list ctx)]
             #:when (memq (vector-ref scope 1) '(macro)))
    (vector-ref scope 0)))

(define (get-all-scopes stx)
  (define ctx (hash-ref (syntax-debug-info stx) 'context null))
  (for/list ([scope (in-list ctx)])
    (vector-ref scope 0)))

;; ==== Partition choices ====

(define partition-choices
  (make-parameter
   `(("By macro scopes" . ,new-macro-scopes-partition)
     ("By all scopes" . ,new-all-scopes-partition))))

;; ==== Identifier relations ====

(define identifier=-choices
  (make-parameter
   `(("<nothing>" . #f)
     ("bound-identifier=?"  . ,bound-identifier=?)
     ("free-identifier=?" . ,free-identifier=?))))
