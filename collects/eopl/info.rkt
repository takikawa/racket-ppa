#lang setup/infotab

(require string-constants)

(define scribblings '(("eopl.scrbl" () (teaching -20))))

(define textbook-pls
  (list (list '("eopl-small.png" "eopl")
              "Essentials of Programming Languages"
              (string-constant teaching-languages)
              "Essentials of Programming Languages (3rd ed.)")))
