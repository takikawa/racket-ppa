#lang racket/base

(provide all-match-tests)

(require rackunit
	 deinprogramm/define-record-procedures
	 (only-in deinprogramm/DMdA match empty make-pair))

(define-record-procedures pare
  kons pare?
  (kar kdr))

(define-record-procedures bare
  gons bare?
  (gar gdr))

(define-record-procedures nullary
  make-nullary nullary?
  ())

(define all-match-tests
  (test-suite
   "Tests for DeinProgramm match form."

   (test-case
    "literals"
    (define foo
      (lambda (x)
	(match x
	  (#t 'true)
	  (#f 'false)
	  ('() 'nil)
	  ('(foo bar) 'foobar)
	  ("foo" 'foo)
	  ("bar" 'bar)
	  (5 'five)
	  (2 'two))))

    (check-equal? (foo #t) 'true)
    (check-equal? (foo #f) 'false)
    (check-equal? (foo '()) 'nil)
    (check-equal? (foo '(foo bar)) 'foobar)
    (check-equal? (foo "foo") 'foo)
    (check-equal? (foo "bar") 'bar)
    (check-equal? (foo 5) 'five)
    (check-equal? (foo 2) 'two))


   (test-case
    "variables"
    (define foo
      (lambda (x)
	(match x
	  (#t  'true)
	  (foo (list 'foo foo)))))
    (check-equal? (foo #t) 'true)
    (check-equal? (foo "foo") '(foo "foo")))
      
   (test-case
    "lists"
    (define foo
      (lambda (x)
	(match x
	  (empty 'empty)
	  ((make-pair 'foo empty) 'fooempty)
	  ((list 'foo 'bar) 'listfoobar)
	  ((list 'bar 'foo) 'listbarfoo)
	  ((list a b c) (list 'list a b c))
	  ((make-pair 5 b) (list 'make-pair5 b))
	  ((make-pair a (make-pair b c)) (list 'make-pair a b c))
	  ((make-pair a b) (list 'make-pair a b))
	  (x (list 'x x)))))

    (check-equal? (foo empty) 'empty)
    (check-equal? (foo "empty") '(x "empty"))
    (check-equal? (foo (list 1 2 3)) '(list 1 2 3))
    (check-equal? (foo (make-pair 'foo empty)) 'fooempty)
    (check-equal? (foo (make-pair 1 empty)) '(make-pair 1 ()))
    (check-equal? (foo (make-pair 5 empty)) '(make-pair5 ()))
    (check-equal? (foo (list 1 2)) '(make-pair 1 2 ()))
    (check-equal? (match empty ((list) 'bingo)) 'bingo)
    (check-equal? (match (list 1) ((list) 'bingo) (foo foo)) (list 1))
    (check-equal? (foo (list 'foo 'bar)) 'listfoobar)
    (check-equal? (foo (list 'bar 'foo)) 'listbarfoo))

   (test-case
    "anything"
    (check-equal? (match 5 (_ 7)) 7)
    (check-equal? (match '(1 2) (_ 7)) 7)
    (check-equal? (match #f (_ 7)) 7)
    (check-equal? (let ((_ 5)) (match #f (_ _))) 5)
    (check-equal? (match #f
		    ((kons _ _) 7)
		    (_ 5))
		  5)
    (check-equal? (match (kons 1 2)
		    ((kons _ _) 7)
		    (_ 5))
		  7))

   (test-case
    "records"
    (define foo
      (lambda (x)
	(match x
	  ((make-pair foo empty) 'pairfoo)
	  ((make-nullary) 'nullary)
	  ((kons a b) (list 'kons a b))
	  ((gons a b) (list 'gons a b)))))
      
    (check-equal? (foo (make-pair foo empty)) 'pairfoo)
    (check-equal? (foo (make-nullary)) 'nullary)
    (check-equal? (foo (kons 1 2)) '(kons 1 2))
    (check-equal? (foo (gons 1 2)) '(gons 1 2)))))
