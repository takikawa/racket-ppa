#;#;
#<<END
TR info: invalid-binary-nonzero-fixnum.rkt 11:3 display -- hidden parameter
END
""

#lang typed/scheme
#:optimize
(: f ( -> Void))
(define (f) ; in a function, to prevent evaluation
  (display (quotient 4 0))) ; 2 fixnums, but the second is 0, cannot optimize
