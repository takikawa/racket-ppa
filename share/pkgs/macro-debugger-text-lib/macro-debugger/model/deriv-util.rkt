#lang racket/base
(require (for-syntax racket/base racket/struct-info)
         racket/match
         "deriv.rkt")

(provide make

         Wrap
         
         ok-node?
         interrupted-node?

         wderiv-e1
         wderiv-e2
         wlderiv-es1
         wlderiv-es2
         wbderiv-es1
         wbderiv-es2

         wderivlist-es2)

;; Wrap matcher
;; Matches unwrapped, interrupted wrapped, or error wrapped
(define-match-expander Wrap
  (lambda (stx)
    (syntax-case stx ()
      [(Wrap S (var ...))
       (syntax/loc stx (struct S (var ...)))])))

;; ----

(define (check sym pred type x)
  (unless (pred x)
    (raise-type-error sym type x)))

(define (ok-node? x)
  (check 'ok-node? node? "node" x)
  (and (node-z2 x) #t))
(define (interrupted-node? x)
  (check 'interrupted-node? node? "node" x)
  (not (node-z2 x)))


(define (wderiv-e1 x)
  (check 'wderiv-e1 deriv? "deriv" x)
  (node-z1 x))
(define (wderiv-e2 x)
  (check 'wderiv-e2 deriv? "deriv" x)
  (node-z2 x))

(define (wlderiv-es1 x)
  (check 'wlderiv-es1 lderiv? "lderiv" x)
  (node-z1 x))
(define (wlderiv-es2 x)
  (check 'wlderiv-es2 lderiv? "lderiv" x)
  (node-z2 x))

(define (wbderiv-es1 x)
  (check 'wbderiv-es1 bderiv? "bderiv" x)
  (node-z1 x))
(define (wbderiv-es2 x)
  (check 'wbderiv-es2 bderiv? "bderiv" x))

;; wderivlist-es2 : (list-of WDeriv) -> (list-of Stx)/#f
(define (wderivlist-es2 xs)
  (let ([es2 (map wderiv-e2 xs)])
    (and (andmap syntax? es2) es2)))

;; get-struct-info : identifier stx -> struct-info-list
(define-for-syntax (get-struct-info id ctx)
  (define (bad-struct-name x)
    (raise-syntax-error #f "expected struct name" ctx x))
  (unless (identifier? id)
    (bad-struct-name id))
  (let ([value (syntax-local-value id (lambda () #f))])
    (unless (struct-info? value)
      (bad-struct-name id))
    (extract-struct-info value)))

;; (make struct-name field-expr ...)
;; Checks that correct number of fields given.
(define-syntax (make stx)
  (syntax-case stx ()
    [(make S expr ...)
     (let ()
       (define info (get-struct-info #'S stx))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(expr ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s, got ~s)"
                    (syntax-e #'S)
                    num-slots
                    num-provided)
            stx)))
       (with-syntax ([constructor constructor])
         (syntax-property #'(constructor expr ...)
                          'disappeared-use
                          #'S)))]))
