#;#;
#<<END
TR opt: drop-pure-pred.rkt 17:6 (list 2) -- dead else branch
END
#<<END
'(1)

END

#lang typed/scheme
#:optimize

(let ()
  (define x 7)
  (if (exact-integer? x)
      (list 1)
      (list 2)))

