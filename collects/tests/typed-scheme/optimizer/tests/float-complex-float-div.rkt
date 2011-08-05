#;
(
TR opt: float-complex-float-div.rkt 47:52 real-part -- unboxed float complex
TR opt: float-complex-float-div.rkt 47:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 48:52 imag-part -- unboxed float complex
TR opt: float-complex-float-div.rkt 48:62 x -- unbox float-complex
TR opt: float-complex-float-div.rkt 50:6 (/ 1.0 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-float-div.rkt 50:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 50:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 50:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 51:6 (/ 1.0+2.0i 2.0) -- unboxed float complex
TR opt: float-complex-float-div.rkt 51:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 51:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 51:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 52:6 (/ 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float-div.rkt 52:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 52:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 52:13 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 52:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 53:6 (/ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float-div.rkt 53:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 53:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 53:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 53:22 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 54:6 (/ 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex
TR opt: float-complex-float-div.rkt 54:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 54:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 54:18 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 54:27 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 55:6 (/ 1.0+2.0i 2.0 3.0) -- unboxed float complex
TR opt: float-complex-float-div.rkt 55:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 55:9 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-div.rkt 55:18 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 55:22 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 56:6 (/ 1.0 2.0 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float-div.rkt 56:7 / -- unboxed binary float complex
TR opt: float-complex-float-div.rkt 56:9 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 56:13 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-div.rkt 56:17 3.0+6.0i -- unboxed literal
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")
)

#lang typed/scheme
#:optimize

(map (lambda: ((x : Float-Complex))
              (string-append (real->decimal-string (real-part x) 10)
                             (real->decimal-string (imag-part x) 10)))
     (list
      (/ 1.0 2.0+4.0i)
      (/ 1.0+2.0i 2.0)
      (/ 1.0 2.0+4.0i 3.0+6.0i)
      (/ 1.0+2.0i 2.0 3.0+6.0i)
      (/ 1.0+2.0i 2.0+4.0i 3.0)
      (/ 1.0+2.0i 2.0 3.0)
      (/ 1.0 2.0 3.0+6.0i)))
