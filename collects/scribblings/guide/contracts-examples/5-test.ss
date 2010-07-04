#lang scheme
(require schemeunit/test schemeunit/text-ui "5.ss")

(define s (put (put (initialize (flat-contract integer?) =) 2) 1))

(run-tests
 (test-suite
  "queue"
  (test-true
   "empty"
   (is-empty? (initialize (flat-contract integer?) =)))
  (test-true "put" (queue? s))
  (test-equal? "count" 2 (count s))
  (test-true "put exn"
             (with-handlers ([exn:fail:contract? (lambda _ #t)])
               (put (initialize (flat-contract integer?)) 'a)
               #f))
  (test-true "remove" (queue? (rem s)))
  (test-equal? "head" 2 (head s))))
