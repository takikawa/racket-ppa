#lang racket

(require "grammar.ss"
         redex/reduction-semantics)

(provide (all-defined-out))

(define-language any)

(define-metafunction any
  [(count-up number)
   ,(build-list (term number) (λ (x) x))])

(define-metafunction any
  concat : (any ...) ... -> (any ...)
  [(concat any ...) ,(apply append (term (any ...)))])