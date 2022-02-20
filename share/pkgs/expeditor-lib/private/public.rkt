#lang racket/base

(provide define-public)

(define-syntax-rule (define-public (id ...) def ...)
  (define-values (id ...)
    (let ()
      def ...
      (values id ...))))

