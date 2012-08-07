#lang racket/base

(define to-require null)
(define (add-mod! m)
  (set! to-require (cons m to-require)))

(define (do-requires [ns (current-namespace)])
  (parameterize ([current-namespace ns])
    (for ([m (in-list to-require)]
          #:when m)
      (dynamic-require `(submod ,m #%type-decl) #f))))

(provide add-mod! do-requires)