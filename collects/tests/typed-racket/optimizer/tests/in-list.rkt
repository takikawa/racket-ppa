#;
(
TR opt: in-list.rkt 6:0 #%module-begin -- in-list
123)

#lang typed/scheme
#:optimize

(for: ((i : Natural '(1 2 3)))
      (display i))
