;;;
;;; COMPREHENSIONS
;;;

(module comprehension-struct "mzscheme2.rkt"
  (provide comprehension comprehension? comprehension-loop make-comprehension
           comprehension-payload)
  
  (define-struct comprehension
    (name loop payload)))
