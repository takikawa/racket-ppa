#lang info

(define scribblings '(("macro-debugger.scrbl" () (tool-library))))

(define raco-commands
  '(("macro-stepper"
     (submod macro-debugger/stepper main)
     "explore expansion steps"
     #f)))
