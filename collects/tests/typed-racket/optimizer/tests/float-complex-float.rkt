#;
(
TR opt: float-complex-float.rkt 28:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 28:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 28:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 28:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 29:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 29:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 29:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 30:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 30:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 30:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 30:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 31:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 31:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 31:21 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 31:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
