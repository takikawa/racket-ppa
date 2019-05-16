#lang scheme/base
(require deinprogramm/sdp/private/sdp-reader)
(provide (rename-out (-read-syntax read-syntax))
         (rename-out (-read read)))
(define -read-syntax (make-read-syntax '(lib "beginner.rkt" "deinprogramm" "sdp")))
(define -read (make-read '(lib "beginner.rkt" "deinprogramm" "sdp")))
