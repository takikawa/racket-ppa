#;
(
TR opt: float-complex.rkt 16:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 16:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 16:0 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex.rkt 17:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 17:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 17:0 (- 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
3.0+6.0i
-1.0-2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0+4.0i)
(- 1.0+2.0i 2.0+4.0i)
