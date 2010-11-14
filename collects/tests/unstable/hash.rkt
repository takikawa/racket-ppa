#lang racket

(require rackunit rackunit/text-ui unstable/hash "helpers.rkt")

(run-tests
 (test-suite "hash.ss"
   (test-suite "hash-ref/check"
     (test-ok (check-equal? (hash-ref/check #hash([1 . one] [2 . two]) 1)
                            'one))
     (test-bad (hash-ref/check #hash([1 . one] [2 . two]) 3)))
   (test-suite "hash-ref/identity"
     (test-ok (check-equal? (hash-ref/identity #hash([1 . one] [2 . two]) 1)
                            'one))
     (test-ok (check-equal? (hash-ref/identity #hash([1 . one] [2 . two]) 3)
                            3)))
   (test-suite "hash-ref/default"
     (test-ok (check-equal? (hash-ref/default #hash([1 . one] [2 . two]) 1 '?)
                            'one))
     (test-ok (check-equal? (hash-ref/default #hash([1 . one] [2 . two]) 3 '?)
                            '?)))
   (test-suite "hash-ref/failure"
     (test-ok (define x 7)
              (define (f) (set! x (+ x 1)) x)
              (check-equal? (hash-ref/failure #hash([1 . one] [2 . two]) 1 f)
                            'one)
              (check-equal? x 7)
              (check-equal? (hash-ref/failure #hash([1 . one] [2 . two]) 3 f)
                            8)
              (check-equal? x 8)))
   (test-suite "hash-union"
     (test-ok (hash-union #hash([1 . one] [2 . two])
                          #hash([3 . three] [4 . four]))
              #hash([4 . four] [3 . three] [1 . one] [2 . two])))
   (test-suite "hash-union!"
     (test-ok (define h (make-hash))
              (hash-union! h #hash([1 . one] [2 . two]))
              (hash-union! h #hash([3 . three] [4 . four]))
              (check-equal? (hash-copy
                             #hash([1 . one] [2 . two] [3 . three] [4 . four]))
                            h)))))

