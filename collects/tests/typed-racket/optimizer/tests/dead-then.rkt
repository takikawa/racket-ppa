#;
(
TR info: dead-then.rkt 12:1 display -- hidden parameter
TR opt: dead-then.rkt 13:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 14:13 (+ 4.0 5.0) -- binary float
TR info: dead-then.rkt 15:1 display -- hidden parameter
TR opt: dead-then.rkt 16:13 (+ 2.0 3.0) -- dead then branch
TR opt: dead-then.rkt 17:13 (+ 4.0 5.0) -- binary float
9.09.0)
#lang typed/scheme
#:optimize
(display (if (number? "eh")
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
(display (if #f
             (+ 2.0 3.0)
             (+ 4.0 5.0)))
