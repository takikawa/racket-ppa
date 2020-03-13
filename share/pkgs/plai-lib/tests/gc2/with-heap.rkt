#lang plai/gc2/collector

(require syntax/macro-testing rackunit)

(define (init-allocator)
  (error 'not-implemented!))

(define (gc:alloc-flat f)
  (error 'not-implemented!))
(define (gc:flat? addr)
  (error 'not-implemented!))
(define (gc:deref addr)
  (error 'not-implemented!))

(define (gc:cons f r)
  (error 'not-implemented!))
(define (gc:cons? addr)
  (error 'not-implemented!))
(define (gc:first addr)
  (error 'not-implemented!))
(define (gc:rest addr)
  (error 'not-implemented!))
(define (gc:set-first! addr new)
  (error 'not-implemented!))
(define (gc:set-rest! addr new)
  (error 'not-implemented!))

(define (gc:closure code-ptr free-vars)
  (error 'not-implemented!))
(define (gc:closure? addr)
  (error 'not-implemented!))
(define (gc:closure-code-ptr addr)
  (error 'not-implemented!))
(define (gc:closure-env-ref addr i)
  (error 'not-implemented!))

(check-exn #rx"expected more terms"
           (lambda () (convert-compile-time-error (with-heap (vector 'x)))))
