#;
(
unboxed-letrec.rkt line 31 col 5 - x - unbox float-complex
unboxed-letrec.rkt line 31 col 7 - y - unbox float-complex
unboxed-letrec.rkt line 31 col 3 - + - unboxed binary float complex
unboxed-letrec.rkt line 31 col 2 - (#%app + x y) - unboxed float complex
unboxed-letrec.rkt line 31 col 5 - x - unbox float-complex
unboxed-letrec.rkt line 31 col 7 - y - unbox float-complex
unboxed-letrec.rkt line 31 col 3 - + - unboxed binary float complex
unboxed-letrec.rkt line 31 col 2 - (#%app + x y) - unboxed float complex
unboxed-letrec.rkt line 29 col 31 - 1.0+2.0i - unboxed literal
unboxed-letrec.rkt line 30 col 34 - 2.0+4.0i - unboxed literal
unboxed-letrec.rkt line 30 col 43 - 3.0+6.0i - unboxed literal
unboxed-letrec.rkt line 30 col 32 - + - unboxed binary float complex
unboxed-letrec.rkt line 28 col 0 - (letrec-values (((f) (lambda (x) (#%app f x))) ((x) (quote 1.0+2.0i)) ((y) (#%app + (quote 2.0+4.0i) (quote 3.0+6.0i)))) (#%app + x y)) - unboxed let bindings
unboxed-letrec.rkt line 31 col 5 - x - leave var unboxed
unboxed-letrec.rkt line 31 col 7 - y - leave var unboxed
unboxed-letrec.rkt line 31 col 3 - + - unboxed binary float complex
unboxed-letrec.rkt line 31 col 2 - (#%app + x y) - unboxed float complex
6.0+12.0i
)

#lang typed/scheme
#:optimize



(letrec: ((f : (Any -> Any) (lambda: ((x : Any)) (f x)))
          (x : Float-Complex   1.0+2.0i)
          (y : Float-Complex   (+ 2.0+4.0i 3.0+6.0i)))
  (+ x y))
