#;
(
TR missed opt: invalid-float-promotion.rkt 12:0 (/ (ann 1 Integer) 2.0) -- all args float-arg-expr, result not Float -- caused by: 12:8 1
TR info: invalid-float-promotion.rkt 12:0 (/ (ann 1 Integer) 2.0) -- exact real arith
TR missed opt: invalid-float-promotion.rkt 13:0 (* (ann 2/3 Exact-Rational) 3.0) -- all args float-arg-expr, result not Float -- caused by: 13:8 2/3
TR info: invalid-float-promotion.rkt 13:0 (* (ann 2/3 Exact-Rational) 3.0) -- exact real arith
0.5 2.0
)
#lang typed/scheme
#:optimize
;; the ann are necessary, since (* PosReal Float) -> Float (exact 0 is not in PosReal)
(/ (ann 1 Integer) 2.0) ; result is not a float, can't optimize
(* (ann 2/3 Exact-Rational) 3.0)
