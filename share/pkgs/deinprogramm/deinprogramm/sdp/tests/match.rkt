#lang racket/base

(provide all-match-tests)

(require rackunit
	 deinprogramm/sdp/record
	 deinprogramm/signature/signature-syntax
	 (only-in deinprogramm/sdp/private/primitives match empty cons))

(define any (signature any %any))

(define-record pare
  kons pare?
  (kar any)
  (kdr any))

(define-record bare
  gons bare?
  (gar any)
  (gdr any))

(define-record nullary
  make-nullary nullary?)

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
	  ((cons 'foo empty) 'fooempty)
	  ((list 'foo 'bar) 'listfoobar)
	  ((list 'bar 'foo) 'listbarfoo)
	  ((list a b c) (list 'list a b c))
	  ((cons 5 b) (list 'cons5 b))
	  ((cons a (cons b c)) (list 'cons a b c))
	  ((cons a b) (list 'cons a b))
	  (x (list 'x x)))))

    (check-equal? (foo empty) 'empty)
    (check-equal? (foo "empty") '(x "empty"))
    (check-equal? (foo (list 1 2 3)) '(list 1 2 3))
    (check-equal? (foo (cons 'foo empty)) 'fooempty)
    (check-equal? (foo (cons 1 empty)) '(cons 1 ()))
    (check-equal? (foo (cons 5 empty)) '(cons5 ()))
    (check-equal? (foo (list 1 2)) '(cons 1 2 ()))
    (check-equal? (match empty ((list) 'bingo)) 'bingo)
    (check-equal? (match (list 1) ((list) 'bingo) (foo foo)) (list 1))
    (check-equal? (foo (list 'foo 'bar)) 'listfoobar)
    (check-equal? (foo (list 'bar 'foo)) 'listbarfoo))

   (test-case
    "anything"
    (check-equal? (match 5 (_ 7)) 7)
    (check-equal? (match 5 (... 7)) 7)
    (check-equal? (match '(1 2) (_ 7)) 7)
    (check-equal? (match '(1 2) (... 7)) 7)
    (check-equal? (match #f (_ 7)) 7)
    (check-equal? (match #f (... 7)) 7)
    (check-equal? (let ((_ 5)) (match #f (_ _))) 5)
    (check-equal? (match #f
		    ((kons _ _) 7)
		    (_ 5))
		  5)
    (check-equal? (match #f
		    ((kons ... ...) 7)
		    (... 5))
		  5)
    (check-equal? (match (kons 1 2)
		    ((kons _ _) 7)
		    (_ 5))
		  7)
    (check-equal? (match (kons 1 2)
		    ((kons ... ...) 7)
		    (... 5))
		  7))
   
   (test-case
    "records"
    (define foo
      (lambda (x)
	(match x
	  ((cons foo empty) 'pairfoo)
	  ((make-nullary) 'nullary)
	  ((kons a b) (list 'kons a b))
	  ((gons a b) (list 'gons a b)))))

    (check-equal? (foo (cons foo empty)) 'pairfoo)
    (check-equal? (foo (make-nullary)) 'nullary)
    (check-equal? (foo (kons 1 2)) '(kons 1 2))
    (check-equal? (foo (gons 1 2)) '(gons 1 2)))))
