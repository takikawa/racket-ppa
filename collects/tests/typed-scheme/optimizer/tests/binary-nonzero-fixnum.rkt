#;
(
TR opt: binary-nonzero-fixnum.rkt 11:1 modulo -- binary nonzero fixnum
TR opt: binary-nonzero-fixnum.rkt 11:9 vector-length -- vector-length
1
)

#lang typed/scheme
#:optimize

(modulo (vector-length '#(1 2 3)) 2)
