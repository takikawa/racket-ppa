#lang racket/base

(provide all-record-tests)

(require rackunit
	 deinprogramm/define-record-procedures
	 racket/match)

(define-record-procedures pare
  kons pare?
  (kar kdr))

(define-record-procedures chocolate-cookie
  make-chocolate-cookie chocolate-cookie?
  (chocolate-cookie-chocolate chocolate-cookie-cookie))

(define-record-procedures-2 mpare
  mkons mpare?
  ((mkar set-mkar!) mkdr))

(define-record-procedures-parametric ppare pkons-of
  pkons pkons?
  (pkar pkdr))

(define-record-procedures-parametric-2 pmpare pmkons-of
  pmkons pmkons?
  ((pmkar set-pmkar!) pmkdr))
 
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
    "-2"
    (define p (mkons 1 2))

    (check-equal? (mkar p) 1)
    (check-equal? (mkdr p) 2)

    (set-mkar! p 5)

    (check-equal? (mkar p) 5))

   (test-case
    "-parametric"
    (define p (pkons 1 2))

    (check-equal? (pkar p) 1)
    (check-equal? (pkdr p) 2))

   (test-case
    "-parametric-2"
    (define p (pmkons 1 2))

    (check-equal? (pmkar p) 1)
    (check-equal? (pmkdr p) 2)

    (set-pmkar! p 5)

    (check-equal? (pmkar p) 5))))
