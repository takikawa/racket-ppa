#;#;
#<<END
TR info: in-range.rkt 2:3 display -- hidden parameter
TR opt: in-range.rkt 1:9 4 -- in-range
END
"0123"
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(for ([i 4])
  (display i))
