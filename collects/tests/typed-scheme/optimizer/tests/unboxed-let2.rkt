#;
(
unboxed-let2.rkt line 32 col 5 - t1 - unbox float-complex
unboxed-let2.rkt line 32 col 8 - t2 - unbox float-complex
unboxed-let2.rkt line 32 col 3 - + - unboxed binary float complex
unboxed-let2.rkt line 32 col 2 - (#%app + t1 t2) - unboxed float complex
unboxed-let2.rkt line 32 col 5 - t1 - unbox float-complex
unboxed-let2.rkt line 32 col 8 - t2 - unbox float-complex
unboxed-let2.rkt line 32 col 3 - + - unboxed binary float complex
unboxed-let2.rkt line 32 col 2 - (#%app + t1 t2) - unboxed float complex
unboxed-let2.rkt line 30 col 13 - 1.0+2.0i - unboxed literal
unboxed-let2.rkt line 30 col 22 - 2.0+4.0i - unboxed literal
unboxed-let2.rkt line 30 col 11 - + - unboxed binary float complex
unboxed-let2.rkt line 31 col 13 - 3.0+6.0i - unboxed literal
unboxed-let2.rkt line 31 col 22 - 4.0+8.0i - unboxed literal
unboxed-let2.rkt line 31 col 11 - + - unboxed binary float complex
unboxed-let2.rkt line 30 col 0 - (let-values (((t1) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i))) ((t2) (#%app + (quote 3.0+6.0i) (quote 4.0+8.0i)))) (#%app + t1 t2)) - unboxed let bindings
unboxed-let2.rkt line 32 col 5 - t1 - leave var unboxed
unboxed-let2.rkt line 32 col 8 - t2 - leave var unboxed
unboxed-let2.rkt line 32 col 3 - + - unboxed binary float complex
unboxed-let2.rkt line 32 col 2 - (#%app + t1 t2) - unboxed float complex
10.0+20.0i
)

#lang typed/scheme
#:optimize



(let ((t1 (+ 1.0+2.0i 2.0+4.0i))
      (t2 (+ 3.0+6.0i 4.0+8.0i)))
  (+ t1 t2))
