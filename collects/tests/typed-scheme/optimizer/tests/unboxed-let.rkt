#;
(
TR opt: unboxed-let.rkt 29:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 29:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 29:0 (let* ((t1 (+ 1.0+2.0i 2.0+4.0i)) (t2 (- t1 3.0+6.0i)) (t3 4.0+8.0i)) (+ t2 t3)) -- unboxed let bindings
TR opt: unboxed-let.rkt 29:12 + -- unboxed binary float complex
TR opt: unboxed-let.rkt 29:14 1.0+2.0i -- unboxed literal
TR opt: unboxed-let.rkt 29:23 2.0+4.0i -- unboxed literal
TR opt: unboxed-let.rkt 30:11 (- t1 3.0+6.0i) -- unboxed float complex
TR opt: unboxed-let.rkt 30:12 - -- unboxed binary float complex
TR opt: unboxed-let.rkt 30:14 t1 -- leave var unboxed
TR opt: unboxed-let.rkt 30:14 t1 -- unbox float-complex
TR opt: unboxed-let.rkt 30:17 3.0+6.0i -- unboxed literal
TR opt: unboxed-let.rkt 31:11 4.0+8.0i -- unboxed literal
TR opt: unboxed-let.rkt 32:2 (+ t2 t3) -- unboxed float complex
TR opt: unboxed-let.rkt 32:3 + -- unboxed binary float complex
TR opt: unboxed-let.rkt 32:5 t2 -- leave var unboxed
TR opt: unboxed-let.rkt 32:5 t2 -- unbox float-complex
TR opt: unboxed-let.rkt 32:8 t3 -- leave var unboxed
TR opt: unboxed-let.rkt 32:8 t3 -- unbox float-complex
4.0+8.0i
)

#lang typed/scheme
#:optimize



(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
