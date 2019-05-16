(module DMdA-vanilla-reader mzscheme
  (require deinprogramm/DMdA/private/DMdA-reader)
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "DMdA-vanilla.rkt" "deinprogramm")))
  (define -read (make-read '(lib "DMdA-vanilla.rkt" "deinprogramm"))))
