#;
(
TR opt: invalid-unboxed-let2.rkt 24:30 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: invalid-unboxed-let2.rkt 24:31 + -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 24:33 1.0+2.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 24:42 2.0+4.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 24:52 (+ 3.0+6.0i 4.0+8.0i) -- unboxed float complex
TR opt: invalid-unboxed-let2.rkt 24:53 + -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 24:55 3.0+6.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 24:64 4.0+8.0i -- unboxed literal
TR opt: invalid-unboxed-let2.rkt 25:2 (+ t1 t2) -- unboxed float complex
TR opt: invalid-unboxed-let2.rkt 25:3 + -- unboxed binary float complex
TR opt: invalid-unboxed-let2.rkt 25:5 t1 -- unbox float-complex
TR opt: invalid-unboxed-let2.rkt 25:8 t2 -- unbox float-complex
10.0+20.0i
)

#lang typed/scheme
#:optimize



;; unboxing of let bindings does not currently work with multiple values
(let-values (((t1 t2) (values (+ 1.0+2.0i 2.0+4.0i) (+ 3.0+6.0i 4.0+8.0i))))
  (+ t1 t2))
