#;
(
TR missed opt: real-in-float-expr.rkt 17:0 (* (ann 3 Real) 2.3) -- all args float-arg-expr, result not Float -- caused by: 17:8 3
TR info: real-in-float-expr.rkt 17:0 (* (ann 3 Real) 2.3) -- exact real arith
TR opt: real-in-float-expr.rkt 22:0 (* 2 3) -- fixnum bounded expr
TR opt: real-in-float-expr.rkt 23:0 (+ 2 3) -- fixnum bounded expr
TR info: real-in-float-expr.rkt 24:0 (+ 2/3 3/4) -- exact real arith
TR missed opt: real-in-float-expr.rkt 26:0 (* (ann 2 Natural) 2.0) -- all args float-arg-expr, result not Float -- caused by: 26:8 2
TR info: real-in-float-expr.rkt 26:0 (* (ann 2 Natural) 2.0) -- exact real arith
6.8999999999999995 6 5 6
5
17/12
4.0
)
#lang typed/racket

(* (ann 3 Real) ; with type Real, typechecker must assume it could be exact 0
   2.3)

(* (ann 2 Integer) (ann 3 Integer)) ; but these have nothing to do with floats, should not be logged
(+ (ann 2 Integer) (ann 3 Integer))
(* 2 3)
(+ 2 3)
(+ 2/3 3/4)

(* (ann 2 Natural) 2.0) ; close calls that result in Nonnegative-Real and co (i.e. not directly Real) should be reported too
