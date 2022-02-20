#lang racket/base

(provide (all-defined-out))

(define default-color -1)

(define black-color 0)
(define red-color 1)
(define green-color 2)
(define yellow-color 3)
(define blue-color 4)
(define magenta-color 5)
(define cyan-color 6)
(define light-gray-color 7)
(define dark-gray-color 8)
(define light-red-color 9)
(define light-green-color 10)
(define light-yellow-color 11)
(define light-blue-color 12)
(define light-magenta-color 13)
(define light-cyan-color 14)
(define white-color 15)

(define error-color light-red-color)
(define paren-color red-color)
(define literal-color green-color)
(define identifier-color light-blue-color)
(define comment-color yellow-color)

(define (expeditor-set-syntax-color! category color)
  (define c
    (case color
      [(black) black-color]
      [(red) red-color]
      [(green) green-color]
      [(yellow) yellow-color]
      [(blue) blue-color]
      [(magenta) magenta-color]
      [(cyan) cyan-color]
      [(light-gray) light-gray-color]
      [(dark-gray) dark-gray-color]
      [(light-red) light-red-color]
      [(light-green) light-green-color]
      [(light-yellow) light-yellow-color]
      [(light-blue) light-blue-color]
      [(light-magenta) light-magenta-color]
      [(light-cyan) light-cyan-color]
      [(white) white-color]
      [else default-color]))
  (case category
    [(error) (set! error-color c)]
    [(paren) (set! paren-color c)]
    [(literal) (set! literal-color c)]
    [(identifier) (set! identifier-color c)]
    [(comment) (set! comment-color c)]))
