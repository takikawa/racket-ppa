#;
(
TR opt: float-complex-mult.rkt 13:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-mult.rkt 13:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-mult.rkt 13:21 3.0+6.0i -- unboxed literal
TR opt: float-complex-mult.rkt 13:0 (* 1.0+2.0i 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
-66.0-12.0i
)

#lang typed/scheme
#:optimize

(* 1.0+2.0i 2.0+4.0i 3.0+6.0i)
