#lang s-exp syntax/module-reader

typed/scheme/base

#:read r:read
#:read-syntax r:read-syntax
#:info make-info
#:language-info make-language-info

(define (make-info key default use-default)
  (case key
    [else (use-default key default)]))

(define make-language-info
  `#(typed-racket/language-info get-info ()))


(require (prefix-in r: typed-racket/typed-reader))
