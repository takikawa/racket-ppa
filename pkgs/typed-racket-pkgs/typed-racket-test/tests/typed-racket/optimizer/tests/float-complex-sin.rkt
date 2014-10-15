#;#;
#<<END
TR info: float-complex-sin.rkt 2:13 (sin (* t 6.28)) -- possible exact real arith
TR info: float-complex-sin.rkt 2:18 (* t 6.28) -- possible exact real arith
TR missed opt: float-complex-sin.rkt 2:13 (sin (* t 6.28)) -- all args float-arg-expr, result not Float -- caused by: 2:18 (* t 6.28)
TR missed opt: float-complex-sin.rkt 2:18 (* t 6.28) -- all args float-arg-expr, result not Float -- caused by: 2:21 t
TR opt: float-complex-sin.rkt 2:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-sin.rkt 2:13 (sin (* t 6.28)) -- non float complex in complex ops
TR opt: float-complex-sin.rkt 2:30 0.0+0.0i -- unboxed literal
END
#<<END
-0.0031853017931379904+0.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
