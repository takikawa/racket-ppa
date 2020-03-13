(module sdp-vanilla-reader mzscheme
  (require deinprogramm/sdp/private/sdp-reader)
  (provide (rename -read-syntax read-syntax)
           (rename -read read))
  (define -read-syntax (make-read-syntax '(lib "vanilla.rkt" "deinprogramm" "sdp")))
  (define -read (make-read '(lib "vanilla.rkt" "deinprogramm" "sdp"))))
