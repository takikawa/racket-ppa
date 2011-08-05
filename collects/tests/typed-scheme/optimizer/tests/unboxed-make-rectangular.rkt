#;
(
TR opt: unboxed-make-rectangular.rkt 27:0 (let ((x (make-rectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 27:10 make-rectangular -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 28:2 (+ x 2.0+4.0i) -- unboxed float complex
TR opt: unboxed-make-rectangular.rkt 28:3 + -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 28:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 28:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 28:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 29:0 (let ((x (unsafe-make-flrectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 29:10 unsafe-make-flrectangular -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 30:2 (+ x 2.0+4.0i) -- unboxed float complex
TR opt: unboxed-make-rectangular.rkt 30:3 + -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 30:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 30:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 30:7 2.0+4.0i -- unboxed literal
3.0+6.0i
3.0+6.0i
)

#lang typed/scheme
#:optimize


(require racket/unsafe/ops)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
