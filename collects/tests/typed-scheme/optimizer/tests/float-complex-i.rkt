#;
(
float-complex-i.rkt line 15 col 3 - 1.0+2.0i - unboxed literal
float-complex-i.rkt line 15 col 15 - 0+1.0i - unboxed literal
float-complex-i.rkt line 15 col 21 - 2.0+4.0i - unboxed literal
float-complex-i.rkt line 15 col 13 - * - unboxed binary float complex
float-complex-i.rkt line 15 col 1 - + - unboxed binary float complex
float-complex-i.rkt line 15 col 0 - (#%app + (quote 1.0+2.0i) (#%app * (quote 0+1.0i) (quote 2.0+4.0i))) - unboxed float complex
-3.0+4.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
