#;
(
TR opt: float-fun.rkt 12:2 (+ x 1.0) -- binary float
)

#lang typed/racket
#:optimize


(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
