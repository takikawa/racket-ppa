#lang scheme/base

(provide all-contract-tests)

(require schemeunit
	 deinprogramm/contract/contract
	 deinprogramm/contract/contract-syntax)

(require scheme/promise)

(define integer (make-predicate-contract 'integer integer? 'integer-marker))
(define boolean (make-predicate-contract 'boolean boolean? 'boolean-marker))
(define %a (make-type-variable-contract 'a 'a-marker))
(define %b (make-type-variable-contract 'b 'b-marker))

(define-syntax say-no
  (syntax-rules ()
    ((say-no ?body ...)
     (let/ec exit
       (call-with-contract-violation-proc
	(lambda (obj contract message blame)
	  (exit 'no))
	(lambda ()
	  ?body ...))))))

(define-syntax failed-contract
  (syntax-rules ()
    ((say-no ?body ...)
     (let/ec exit
       (call-with-contract-violation-proc
	(lambda (obj contract message blame)
	  (exit contract))
	(lambda ()
	  ?body ...))))))

(define contract-tests
  (test-suite
   "Tests for contract combinators"

   (test-case
    "flat"
    (check-equal? (say-no (apply-contract integer 5)) 5)
    (check-equal? (say-no (apply-contract integer "foo")) 'no))

   (test-case
    "list"
    (define integer-list (make-list-contract 'integer-list integer #f))
    (check-equal? (say-no (apply-contract integer-list '(1 2 3)))
		  '(1 2 3))
    (check-equal? (say-no (apply-contract integer-list '#f))
		  'no)
    (check-eq? (failed-contract (apply-contract integer-list '(1 #f 3)))
	       integer))

   (test-case
    "list-cached"
    (define integer-list (make-list-contract 'integer-list integer #f))
    (define boolean-list (make-list-contract 'integer-list boolean #f))
    (define l '(1 2 3))
    (define foo "foo")
    (define no '(1 #f 3))
    (define no2 '(1 #f 3))
    (define integer-list->bool (make-procedure-contract 'integer-list->bool (list integer-list) boolean 'int->bool-marker))

    (check-equal? (say-no (apply-contract integer-list l))
		  '(1 2 3))
    (check-equal? (say-no (apply-contract integer-list l))
		  '(1 2 3))
    (check-equal? (say-no (apply-contract boolean-list l))
		  'no)
    (check-equal? (say-no (apply-contract integer-list foo))
		  'no)
    (check-equal? (say-no (apply-contract integer-list foo))
		  'no)
    (check-eq? (failed-contract (apply-contract integer-list no))
	       integer)
    (check-eq? (failed-contract (apply-contract integer-list no))
	       integer)

    (let ((proc (say-no (apply-contract integer-list->bool (lambda (l) (even? (car l)))))))
      (check-equal? (say-no (proc no)) 'no)
      (check-equal? (say-no (proc no)) 'no)
      (check-equal? (say-no (proc no2)) 'no)
      (check-equal? (say-no (proc no2)) 'no))
    )
 
   (test-case
    "mixed"
    (define int-or-bool (make-mixed-contract 'int-or-bool
					     (list integer
						   boolean)
					     'int-or-bool-marker))
    (check-equal? (say-no (apply-contract int-or-bool #f))
		  #f)
    (check-equal? (say-no (apply-contract int-or-bool 17))
		  17)
    (check-equal? (say-no (apply-contract int-or-bool "foo"))
		  'no))

   (test-case
    "combined"
    (define octet (make-combined-contract
		   'octet
		   (list
		    integer
		    (make-predicate-contract '<256
					     (delay (lambda (x)
						      (< x 256)))
					     '<256-marker)
		    (make-predicate-contract 'non-negative
					     (delay (lambda (x)
						      (>= x 0)))
					     'non-negative-marker))
		   'octet-marker))
    (check-equal? (say-no (apply-contract octet #f))
		  'no)
    (check-equal? (say-no (apply-contract octet 17))
		  17)
    (check-equal? (say-no (apply-contract octet 0))
		  0)
    (check-equal? (say-no (apply-contract octet -1))
		  'no)
    (check-equal? (say-no (apply-contract octet 255))
		  255)
    (check-equal? (say-no (apply-contract octet 256))
		  'no)
    (check-equal? (say-no (apply-contract octet "foo"))
		  'no))

   (test-case
    "case"
    (define foo-or-bar (make-case-contract 'foo-or-bar '("foo" "bar") 'foo-or-bar-marker))
    (check-equal? (say-no (apply-contract foo-or-bar #f))
		  'no)
    (check-equal? (say-no (apply-contract foo-or-bar "foo"))
		    "foo")
    (check-equal? (say-no (apply-contract foo-or-bar "bar"))
		  "bar"))

   (test-case
    "procedure"
    (define int->bool (make-procedure-contract 'int->bool (list integer) boolean 'int->bool-marker))
    (check-equal? (say-no (apply-contract int->bool #f))
		  'no)
    (check-equal? (say-no (apply-contract int->bool (lambda () "foo")))
		  'no)
    (check-equal? (say-no (apply-contract int->bool (lambda (x y) "foo")))
		  'no)
    (let ((proc (say-no (apply-contract int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-contract int->bool (lambda (x) (+ x 1))))))
      (check-equal? (say-no (proc 12)) 'no)))

   (test-case
    "type variable - simple"
    (check-equal? (say-no (apply-contract %a #f)) #f)
    (check-equal? (say-no (apply-contract %a 15)) 15))
   
   (test-case
    "type variable - list"
    (define a-list (make-list-contract 'a-list %a #f))
    (check-equal? (say-no (apply-contract a-list '(1 2 3)))
		  '(1 2 3))
    (check-equal? (say-no (apply-contract a-list '#f))
		  'no)
    (check-equal? (say-no (apply-contract a-list '(#f "foo" 5)))
		  '(#f "foo" 5)))

   (test-case
    "apply-contract/blame"
    (define int->bool (make-procedure-contract 'int->bool (list integer) boolean 'int->bool-marker))
    (let ((proc (say-no (apply-contract/blame int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-contract/blame int->bool (lambda (x) x)))))
      (call-with-contract-violation-proc
       (lambda (obj contract message blame)
	 (check-true (syntax? blame)))
       (lambda ()
	 (proc 5)))))
   ))

(define contract-syntax-tests
  (test-suite
   "Tests for contract syntax"

   (test-case
    "predicate"
    (define integer (contract (predicate integer?)))
    (check-equal? (say-no (apply-contract integer 5)) 5)
    (check-equal? (say-no (apply-contract integer "foo")) 'no))

   (test-case
    "list"
    (check-equal? (say-no (apply-contract (contract x (list %a)) 5)) 'no)
    (check-equal? (say-no (apply-contract (contract x (list %a)) '(1 2 3))) '(1 2 3))
    (check-equal? (say-no (apply-contract (contract x (list (predicate integer?))) '(1 2 3))) '(1 2 3))
    (check-equal? (say-no (apply-contract (contract x (list (predicate integer?))) '(1 #f 3))) 'no))

   (test-case
    "mixed"
    (define int-or-bool (contract (mixed integer boolean)))
    (check-equal? (say-no (apply-contract int-or-bool #f))
		  #f)
    (check-equal? (say-no (apply-contract int-or-bool 17))
		  17)
    (check-equal? (say-no (apply-contract int-or-bool "foo"))
		  'no))

   (test-case
    "combined"
    (define octet (contract (combined integer
				      (predicate (lambda (x)
						   (< x 256)))
				      (predicate (lambda (x)
						   (>= x 0))))))
    (check-equal? (say-no (apply-contract octet #f))
		  'no)
    (check-equal? (say-no (apply-contract octet 17))
		  17)
    (check-equal? (say-no (apply-contract octet 0))
		  0)
    (check-equal? (say-no (apply-contract octet -1))
		  'no)
    (check-equal? (say-no (apply-contract octet 255))
		  255)
    (check-equal? (say-no (apply-contract octet 256))
		  'no)
    (check-equal? (say-no (apply-contract octet "foo"))
		  'no))

   (test-case
    "procedure"
    (define int->bool (contract int->bool ((predicate integer?) -> (predicate boolean?))))
    (check-equal? (say-no (apply-contract int->bool #f))
		  'no)
    (check-equal? (say-no (apply-contract int->bool (lambda () "foo")))
		  'no)
    (check-equal? (say-no (apply-contract int->bool (lambda (x y) "foo")))
		  'no)
    (let ((proc (say-no (apply-contract int->bool (lambda (x) (odd? x))))))
      (check-pred procedure? proc)
      (check-equal? (proc 15) #t)
      (check-equal? (proc 16) #f)
      (check-equal? (say-no (proc "foo")) 'no))
    (let ((proc (say-no (apply-contract int->bool (lambda (x) (+ x 1))))))
      (check-equal? (say-no (proc 12)) 'no))) 

))


(define all-contract-tests
  (test-suite
   "all-contract-tests"
   contract-tests
   contract-syntax-tests))
