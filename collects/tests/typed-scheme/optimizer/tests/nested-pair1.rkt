#;
(
TR opt: nested-pair1.rkt 11:1 car -- pair
TR opt: nested-pair1.rkt 11:6 cdr -- pair
2
)

#lang typed/scheme
#:optimize

(car (cdr '(1 2)))
