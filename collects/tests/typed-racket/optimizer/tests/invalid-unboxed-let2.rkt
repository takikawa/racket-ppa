#;
(
TR opt: invalid-unboxed-let2.rkt 21:33 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 21:42 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 21:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 21:55 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 21:64 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 21:52 (+ 3.0+6.0i 4.0+8.0i) -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 22:5 t1 -- unbox float-complex
TR opt: invalid-unboxed-let2.rkt 22:8 t2 -- unbox float-complex
TR opt: invalid-unboxed-let2.rkt 22:2 (+ t1 t2) -- unboxed binary float complex
10.0+20.0i
)

#lang typed/scheme
#:optimize



;; unboxing of let bindings does not currently work with multiple values
(let-values (((t1 t2) (values (+ 1.0+2.0i 2.0+4.0i) (+ 3.0+6.0i 4.0+8.0i))))
  (+ t1 t2))
