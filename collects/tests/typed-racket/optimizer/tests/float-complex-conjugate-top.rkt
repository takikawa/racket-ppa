#;
(
TR opt: float-complex-conjugate-top.rkt 13:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate-top.rkt 13:23 2.0+4.0i -- unboxed literal
TR opt: float-complex-conjugate-top.rkt 13:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-conjugate-top.rkt 13:0 (conjugate (+ 1.0+2.0i 2.0+4.0i)) -- unboxed unary float complex
3.0-6.0i
)

#lang typed/scheme
#:optimize

(conjugate (+ 1.0+2.0i 2.0+4.0i))
