#lang racket/base

(provide all-record-tests)

(require rackunit
	 deinprogramm/sdp/record
	 deinprogramm/signature/signature-syntax
	 (only-in deinprogramm/signature/signature signature?)
	 racket/match)

(define any (signature any %any))
(define rational (signature (predicate rational?)))
(define string (signature (predicate string?)))

(define-record pare
  kons pare?
  (kar any)
  (kdr any))

(define-record paire
  koins
  (kair any)
  (kdir any))

(define-record chocolate-cookie
  make-chocolate-cookie chocolate-cookie?
  (chocolate-cookie-chocolate rational)
  (chocolate-cookie-cookie    rational))

(define-record (ppare a)
  pkons pkons?
  (pkar a)
  (pkdr any))

(define-record nullary
  make-nullary nullary?)

(define all-record-tests
  (test-suite
   "Tests for DeinProgramm records."

   (test-case
    "basics"
    (define p1 (kons 1 2))
    (define p2 (kons 3 4))

    (check-true (pare? p1))
    (check-true (pare? p2))

    (check-false (pare? 5))
    (check-false (pare? (make-chocolate-cookie 1 2)))

    (check-equal? (kar p1) 1)
    (check-equal? (kdr p1) 2)
    (check-equal? (kar p2) 3)
    (check-equal? (kdr p2) 4))

   (test-case
    "no predicate"

    (define p1 (koins 1 2))
    (define p2 (koins 3 4))

    (check-equal? (kair p1) 1)
    (check-equal? (kdir p1) 2)
    (check-equal? (kair p2) 3)
    (check-equal? (kdir p2) 4)

    (check-true (signature? paire)))

   (test-case
    "matching"
    (define p (kons 1 2))
    (define c (make-chocolate-cookie 3 4))

    (define t
      (lambda (r)
	(match r
	  ((kons a b) (list 'kons a b))
	  ((make-chocolate-cookie ch ck) (list 'make-chocolate-cookie ch ck)))))

    (check-equal? (t p) '(kons 1 2))
    (check-equal? (t c) '(make-chocolate-cookie 3 4)))

   (test-case
    "parametric"
    (define p (pkons 1 2))

    (check-equal? (pkar p) 1)
    (check-equal? (pkdr p) 2))))
