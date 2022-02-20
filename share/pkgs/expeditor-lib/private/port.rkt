#lang racket/base

(provide open-input-string/count
         input-port-position)

(define (open-input-string/count s)
  (define p (open-input-string s))
  (port-count-lines! p)
  p)

(define (input-port-position s)
  (define-values (lin col pos) (port-next-location s))
  (sub1 pos))
