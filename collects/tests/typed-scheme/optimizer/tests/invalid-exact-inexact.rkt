#;
(
TR opt: invalid-exact-inexact.rkt 9:1 exact->inexact -- float to float
1.0
)

#lang typed/scheme
#:optimize
(exact->inexact 1.0) ; not an integer, can't optimize
