#;
(
TR opt: known-vector-length.rkt 11:1 + -- fixnum bounded expr
TR opt: known-vector-length.rkt 11:6 vector-length -- known-length vector-length
4
)

#lang typed/scheme
#:optimize

(+ 2 (vector-length (ann (vector 1 2) (Vector Integer Integer))))
