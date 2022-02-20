#lang racket/base

(provide assert*)

(define-syntax assert*
  (syntax-rules ()
    [(_ expr ...)
     (begin (assert expr) ...)]))

(define (assert expr)
  (unless expr
    (error "assertion failed: ~s" 'expr)))
