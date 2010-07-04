
(load-relative "loadtest.ss")

(Section 'basic)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test '() 'null null)
(test '() 'null ())

(let ([f (lambda () #&7)])
  (test #t eq? (f) (f)))

;; test that all symbol characters are supported.
'(+ - ... !.. $.+ %.- &.! *.: /:. :+. <-. =. >. ?. ~. _. ^.)

(define disjoint-type-functions
  (list boolean? char? null? number? pair? procedure? string? symbol? vector?))
(define type-examples
  (list
   #t #f #\a '() 9739 '(test) record-error "test" "" 'test '#() '#(a b c) ))
(define i 1)
(for-each (lambda (x) (display (make-string i #\ ))
		  (set! i (+ 3 i))
		  (write x)
		  (newline))
	  disjoint-type-functions)
(define type-matrix
  (map (lambda (x)
	 (let ((t (map (lambda (f) (f x)) disjoint-type-functions)))
	   (write t)
	   (write x)
	   (newline)
	   t))
       type-examples))

(test #f not #t)
(test #f not 3)
(test #f not (list 3))
(test #t not #f)
(test #f not '())
(test #f not (list))
(test #f not 'nil)
(arity-test not 1 1)

(test #t boolean? #f)
(test #t boolean? #t)
(test #f boolean? 0)
(test #f boolean? '())
(arity-test boolean? 1 1)

(test #t eqv? 'a 'a)
(test #f eqv? 'a 'b)
(test #t eqv? 2 2)
(test #f eqv? 2 2.0)
(test #t eqv? '() '())
(test #t eqv? '10000 '10000)
(test #t eqv? 10000000000000000000 10000000000000000000)
(test #f eqv? 10000000000000000000 10000000000000000001)
(test #f eqv? 10000000000000000000 20000000000000000000)
(test #f eqv? (cons 1 2) (cons 1 2))
(test #f eqv? (lambda () 1) (lambda () 2))
(test #f eqv? #f 'nil)
(let ((p (lambda (x) x)))
  (test #t eqv? p p))
(define gen-counter
 (lambda ()
   (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))
(let ((g (gen-counter))) (test #t eqv? g g))
(test #f eqv? (gen-counter) (gen-counter))
(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
	 (g (lambda () (if (eqv? f g) 'g 'both))))
  (test #f eqv? f g))

(test #t eq? 'a 'a)
(test #f eq? (list 'a) (list 'a))
(test #t eq? '() '())
(test #t eq? car car)
(let ((x '(a))) (test #t eq? x x))
(let ((x '#())) (test #t eq? x x))
(let ((x (lambda (x) x))) (test #t eq? x x))

(test #t equal? 'a 'a)
(test #t equal? '("a") '("a"))
(test #t equal? '(a) '(a))
(test #t equal? '(a (b) c) '(a (b) c))
(test #t equal? '("a" ("b") "c") '("a" ("b") "c"))
(test #t equal? "abc" "abc")
(test #t equal? 2 2)
(test #t equal? (make-vector 5 'a) (make-vector 5 'a))
(test #t equal? (box "a") (box "a"))
(test #f equal? "" (string #\null))

(test #f equal? 'a "a")
(test #f equal? 'a 'b)
(test #f equal? '(a) '(b))
(test #f equal? '(a (b) d) '(a (b) c))
(test #f equal? '(a (b) c) '(d (b) c))
(test #f equal? '(a (b) c) '(a (d) c))
(test #f equal? "abc" "abcd")
(test #f equal? "abcd" "abc")
(test #f equal? 2 3)
(test #f equal? 2.0 2)
(test #f equal? (make-vector 5 'b) (make-vector 5 'a))
(test #f equal? (box "a") (box "b"))

(arity-test eq? 2 2)
(arity-test eqv? 2 2)
(arity-test equal? 2 2)

(test '(a b c d e) 'dot '(a . (b . (c . (d . (e . ()))))))
(define x (list 'a 'b 'c))
(define y x)
(and list? (test #t list? y))
(set-cdr! x 4)
(test '(a . 4) 'set-cdr! x)
(test #t eqv? x y)
(test '(a b c . d) 'dot '(a . (b . (c . d))))
(test #f list? y)
(let ((x (list 'a))) (set-cdr! x x) (test #f list? x))
(arity-test list? 1 1)

(test #t pair? '(a . b))
(test #t pair? '(a . 1))
(test #t pair? '(a b c))
(test #f pair? '())
(test #f pair? '#(a b))
(arity-test pair? 1 1)

(test '(a) cons 'a '())
(test '((a) b c d) cons '(a) '(b c d))
(test '("a" b c) cons "a" '(b c))
(test '(a . 3) cons 'a 3)
(test '((a b) . c) cons '(a b) 'c)
(arity-test cons 2 2) 

(test 'a car '(a b c))
(test '(a) car '((a) b c d))
(test 1 car '(1 . 2))
(arity-test car 1 1)
(err/rt-test (car 1))

(test '(b c d) cdr '((a) b c d))
(test 2 cdr '(1 . 2))
(arity-test cdr 1 1)
(err/rt-test (cdr 1))

(test '(a 7 c) list 'a (+ 3 4) 'c)
(test '() list)

(test 3 length '(a b c))
(test 3 length '(a (b) (c d e)))
(test 0 length '())
(arity-test length 1 1)
(err/rt-test (length 1))
(err/rt-test (length '(1 . 2)))
(err/rt-test (length "a"))
; (err/rt-test (length (quote #0=(1 . #0#))))
(err/rt-test (let ([p (cons 1 1)]) (set-cdr! p p) (length p)))
(define x (cons 4 0))
(set-cdr! x x) 
(err/rt-test (length x))

(define l '(1 2 3))
(set-cdr! l 5)
(test '(1 . 5) 'set-cdr! l)
(set-car! l 0)
(test '(0 . 5) 'set-car! l)
(arity-test set-car! 2 2)
(arity-test set-cdr! 2 2)
(err/rt-test (set-car! 4 4))
(err/rt-test (set-cdr! 4 4))

(define (box-tests box unbox box? set-box! set-box!-name unbox-name)
  (define b (box 5))
  (test 5 unbox b)
  (when set-box!
	(set-box! b 6)
	(test 6 unbox b))
  (test #t box? b)
  (test #f box? 5)
  (arity-test box 1 1)
  (arity-test unbox 1 1)
  (arity-test box? 1 1)
  (when set-box!
	(arity-test set-box! 2 2))
  (err/rt-test (unbox 8))
  (when set-box!
    (err/rt-test (set-box! 8 8))))
(box-tests box unbox box? set-box! 'set-box! 'unbox)
(box-tests make-weak-box weak-box-value weak-box? #f #f 'weak-box-value)

(test '(x y) append '(x) '(y))
(test '(a b c d) append '(a) '(b c d))
(test '(a (b) (c)) append '(a (b)) '((c)))
(test '() append)
(test '(a b c . d) append '(a b) '(c . d))
(test 'a append '() 'a)
(test 1 append 1)
(test '(1 . 2) append '(1) 2)
(test '(1 . 2) append '(1) 2)
(err/rt-test (append '(1 2 . 3) 1))
(err/rt-test (append '(1 2 3) 1 '(4 5 6)))
(test '(x y) append! '(x) '(y))
(test '(a b c d) append! '(a) '(b c d))
(test '(a (b) (c)) append! '(a (b)) '((c)))
(test '() append!)
(test '(a b c . d) append! '(a b) '(c . d))
(test 'a append! '() 'a)
(test 1 append! 1)
(err/rt-test (append! '(1 2 . 3) 1))
(err/rt-test (append! '(1 2 3) 1 '(4 5 6)))
(err/rt-test (append! (cons-immutable 1 null) '(4 5 6)))

(define l '(1 2))
(define l2 '(3 4 . 7))
(define l3 (append l l2))
(test '(1 2 3 4 . 7) 'append l3)
(set-car! l2 5)
(test '(1 2 5 4 . 7) 'append l3)
(set-car! l3 0)
(test '(0 2 5 4 . 7) 'append l3)
(test '(1 2) 'append l)

(let* ([l '(1 2)]
       [l2 '(3 4 . 7)]
       [l3 (append! l l2)])
  (test '(1 2 3 4 . 7) 'append! l3)
  (set-car! l2 5)
  (test '(1 2 5 4 . 7) 'append! l3)
  (set-car! l3 0)
  (test '(0 2 5 4 . 7) 'append! l3)
  (test '(0 2 5 4 . 7) 'append! l))

(test '(c b a) reverse '(a b c))
(test '((e (f)) d (b c) a) reverse '(a (b c) d (e (f))))
(arity-test reverse 1 1)
(err/rt-test (reverse 1))
(err/rt-test (reverse '(1 . 1)))

(define l '(a b c))
(test '(c b a) reverse! l)
(test '(a) 'reverse! l)
(test '((e (f)) d (b c) a) reverse! '(a (b c) d (e (f))))
(arity-test reverse! 1 1)
(err/rt-test (reverse! 1))
(err/rt-test (reverse! '(1 . 1)))

(test 'c list-ref '(a b c d) 2)
(test 'c list-ref '(a b c . d) 2)
(arity-test list-ref 2 2)
(err/rt-test (list-ref 1 1) exn:application:mismatch?)
(err/rt-test (list-ref '(a b . c) 2) exn:application:mismatch?)
(err/rt-test (list-ref '(1 2 3) 2.0))
(err/rt-test (list-ref '(1) '(1)))
(err/rt-test (list-ref '(1) 1) exn:application:mismatch?)
(err/rt-test (list-ref '() 0) exn:application:mismatch?)
(err/rt-test (list-ref '() 0) exn:application:mismatch?)
(err/rt-test (list-ref '(1) -1))
(err/rt-test (list-ref '(1) 2000000000000) exn:application:mismatch?)

(test '(c d) list-tail '(a b c d) 2)
(test '(a b c d) list-tail '(a b c d) 0)
(test '(b c . d) list-tail '(a b c . d) 1)
(test 1 list-tail 1 0)
(arity-test list-tail 2 2)
(err/rt-test (list-tail 1 1) exn:application:mismatch?)
(err/rt-test (list-tail '(1 2 3) 2.0))
(err/rt-test (list-tail '(1) '(1)))
(err/rt-test (list-tail '(1) -1))
(err/rt-test (list-tail '(1) 2) exn:application:mismatch?)
(err/rt-test (list-tail '(1 2 . 3) 3) exn:application:mismatch?)

(define (test-mem memq memq-name)
  (test '(a b c) memq 'a '(a b c))
  (test '(b c) memq 'b '(a b c))
  (test '(b . c) memq 'b '(a b . c))
  (test '#f memq 'a '(b c d))

  (arity-test memq 2 2)
  (err/rt-test (memq 'a 1) exn:application:mismatch?)
  (err/rt-test (memq 'a '(1 . 2)) exn:application:mismatch?))

(test-mem memq 'memq)
(test-mem memv 'memv)
(test-mem member 'member)

(test #f memq "apple" '("apple"))
(test #f memv "apple" '("apple"))
(test '("apple") member "apple" '("apple"))

; (test #f memq 1/2 '(1/2)) ; rationals are immutable and we may want to optimize
(test '(1/2) memv 1/2 '(1/2))
(test '(1/2) member 1/2 '(1/2))

(test '((1 2)) member '(1 2) '(1 2 (1 2)))

(define (test-ass assq assq-name)
  (define e '((a 1) (b 2) (c 3)))
  (test '(a 1) assq 'a e)
  (test '(b 2) assq 'b e)
  (test #f assq 'd e)
  (test '(a 1) assq 'a '((x 0) (a 1) b 2))
  (test '(a 1) assq 'a '((x 0) (a 1) . 0))
  (arity-test assq 2 2)

  (err/rt-test (assq 1 1) exn:application:mismatch?)
  (err/rt-test (assq 1 '(1 2))  exn:application:mismatch?)
  (err/rt-test (assq 1 '((0) . 2)) exn:application:mismatch?))

(test-ass assq 'assq)
(test-ass assv 'assv)
(test-ass assoc 'assoc)

(test #f assq '(a) '(((a)) ((b)) ((c))))
(test #f assv '(a) '(((a)) ((b)) ((c))))
(test '((b) 1) assoc '(b) '(((a)) ((b) 1) ((c))))

; (test #f assq '1/2 '(((a)) (1/2) ((c)))) ; rationals are immutable and we may want to optimize
(test '(1/2) assv '1/2 '(((a)) (1/2) ((c))))
(test '(1/2) assoc '1/2 '(((a)) (1/2) ((c))))

(test #f immutable? (cons 1 null))
(test #f immutable? (list 1))
(test #f immutable? (list 1 2))
(test #f immutable? (list* 1 null))
(test #f immutable? (list* 1 2 null))
(test #f immutable? 1)
(test #f immutable? #(1 2 3))
(test #f immutable? #())
(test #f immutable? (string-copy "hi"))

(test #t immutable? (cons-immutable 1 null))
(test #t immutable? (list-immutable 1))
(test #t immutable? (list-immutable 1 2))
(test #t immutable? (list*-immutable 1 null))
(test #t immutable? (list*-immutable 1 2 null))
(test #t immutable? "hi")
(test #t immutable? (string->immutable-string "hi"))
(test #t immutable? (string->immutable-string (string-copy "hi")))

(test #t immutable? (make-immutable-hash-table null))
(test #t immutable? (make-immutable-hash-table '((a . b))))
(test #t immutable? (make-immutable-hash-table '((a . b)) 'equal))
(test #f immutable? (make-hash-table))
(test #f immutable? (make-hash-table 'equal))
(test #f immutable? (make-hash-table 'weak))
(test #f immutable? (make-hash-table 'weak 'equal))

(test #t symbol? 'foo)
(test #t symbol? (car '(a b)))
(test #f symbol? "bar")
(test #t symbol? 'nil)
(test #f symbol? '())
(test #f symbol? #f)
;;; But first, what case are symbols in?  Determine the standard case:
#ci(parameterize ([read-case-sensitive #f])
     (define char-standard-case char-upcase)
     (if (string=? (symbol->string 'A) "a")
	 (set! char-standard-case char-downcase))
     (test #t 'standard-case
	   (string=? (symbol->string 'a) (symbol->string 'A)))
     (test #t 'standard-case
	   (or (string=? (symbol->string 'a) "A")
	       (string=? (symbol->string 'A) "a")))
     (let ()
       (define (str-copy s)
	 (let ((v (make-string (string-length s))))
	   (do ((i (- (string-length v) 1) (- i 1)))
	       ((< i 0) v)
	     (string-set! v i (string-ref s i)))))
       (define (string-standard-case s)
	 (set! s (str-copy s))
	 (do ((i 0 (+ 1 i))
	      (sl (string-length s)))
	     ((>= i sl) s)
	   (string-set! s i (char-standard-case (string-ref s i)))))
       (test (string-standard-case "flying-fish") symbol->string 'flying-fish)
       (test (string-standard-case "martin") symbol->string 'Martin)
       (test "Malvina" symbol->string (string->symbol "Malvina"))
       (test #t 'standard-case (eq? 'a 'A))))

(define x (string #\a #\b))
(define y (string->symbol x))
(string-set! x 0 #\c)
(test "cb" 'string-set! x)
(test "ab" symbol->string y)
(test y string->symbol "ab")

#ci(test #t eq? 'mISSISSIppi 'mississippi)
#ci(test #f 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
#cs(test #t 'string->symbol (eq? 'bitBlt (string->symbol "bitBlt")))
(test 'JollyWog string->symbol (symbol->string 'JollyWog))
#ci(test 'JollyWog string->symbol (symbol->string 'JollyWog))

(arity-test symbol? 1 1)

(define (char-tests)
  (test #t eqv? '#\  #\Space)
  (test #t eqv? #\space '#\Space)
  (test #t char? #\a)
  (test #t char? #\()
  (test #t char? #\ )
  (test #t char? '#\newline)
  (arity-test char? 1 1)

  (test #f char=? #\A #\B)
  (test #f char=? #\A #\A #\B)
  (test #f char=? #\A #\B #\A)
  (test #f char=? #\a #\b)
  (test #f char=? #\9 #\0)
  (test #t char=? #\A #\A)
  (test #t char=? #\A #\A #\A)
  (test #t char=? #\370 #\370)
  (test #f char=? #\371 #\370)
  (test #f char=? #\370 #\371)
  (arity-test char=? 2 -1)
  (err/rt-test (char=? #\a 1)) 
  (err/rt-test (char=? #\a #\b 1)) 
  (err/rt-test (char=? 1 #\a))

  (test #t char<? #\A #\B)
  (test #t char<? #\A #\B #\C)
  (test #f char<? #\A #\B #\A)
  (test #f char<? #\A #\A #\C)
  (test #t char<? #\a #\b)
  (test #f char<? #\9 #\0)
  (test #f char<? #\A #\A)
  (test #f char<? #\370 #\370)
  (test #f char<? #\371 #\370)
  (test #t char<? #\370 #\371)
  (arity-test char<? 2 -1)
  (err/rt-test (char<? #\a 1)) 
  (err/rt-test (char<? #\a #\a 1)) 
  (err/rt-test (char<? 1 #\a))

  (test #f char>? #\A #\B)
  (test #t char>? #\B #\A)
  (test #f char>? #\A #\B #\C)
  (test #f char>? #\B #\A #\C)
  (test #t char>? #\C #\B #\A)
  (test #f char>? #\a #\b)
  (test #t char>? #\9 #\0)
  (test #f char>? #\A #\A)
  (test #f char>? #\370 #\370)
  (test #t char>? #\371 #\370)
  (test #f char>? #\370 #\371)
  (arity-test char>? 2 -1)
  (err/rt-test (char>? #\a 1)) 
  (err/rt-test (char>? #\a #\a 1)) 
  (err/rt-test (char>? 1 #\a))

  (test #t char<=? #\A #\B)
  (test #t char<=? #\A #\B #\C)
  (test #t char<=? #\A #\A #\C)
  (test #f char<=? #\A #\B #\A)
  (test #f char<=? #\B #\A #\C)
  (test #t char<=? #\a #\b)
  (test #f char<=? #\9 #\0)
  (test #t char<=? #\A #\A)
  (test #t char<=? #\370 #\370)
  (test #f char<=? #\371 #\370)
  (test #t char<=? #\370 #\371)
  (arity-test char<=? 2 -1)
  (err/rt-test (char<=? #\a 1)) 
  (err/rt-test (char<=? #\b #\a 1)) 
  (err/rt-test (char<=? 1 #\a))

  (test #f char>=? #\A #\B)
  (test #f char>=? #\a #\b)
  (test #t char>=? #\9 #\0)
  (test #t char>=? #\A #\A)
  (test #t char>=? #\370 #\370)
  (test #t char>=? #\371 #\370)
  (test #f char>=? #\370 #\371)
  (arity-test char>=? 2 -1)
  (err/rt-test (char>=? #\a 1)) 
  (err/rt-test (char>=? #\a #\b 1)) 
  (err/rt-test (char>=? 1 #\a))

  (test #f char-ci=? #\A #\B)
  (test #f char-ci=? #\A #\A #\B)
  (test #f char-ci=? #\a #\B)
  (test #f char-ci=? #\A #\b)
  (test #f char-ci=? #\a #\b)
  (test #f char-ci=? #\9 #\0)
  (test #t char-ci=? #\A #\A)
  (test #t char-ci=? #\A #\a)
  (test #t char-ci=? #\A #\a #\A)
  (test #t char-ci=? #\370 #\370)
  (test #f char-ci=? #\371 #\370)
  (test #f char-ci=? #\370 #\371)
  (arity-test char-ci=? 2 -1)
  (err/rt-test (char-ci=? #\a 1)) 
  (err/rt-test (char-ci=? #\a #\b 1)) 
  (err/rt-test (char-ci=? 1 #\a))

  (test #t char-ci<? #\A #\B)
  (test #t char-ci<? #\A #\B #\C)
  (test #t char-ci<? #\a #\B)
  (test #t char-ci<? #\A #\b)
  (test #t char-ci<? #\A #\b #\C)
  (test #t char-ci<? #\a #\b)
  (test #f char-ci<? #\9 #\0)
  (test #f char-ci<? #\A #\A)
  (test #f char-ci<? #\A #\a)
  (test #f char-ci<? #\A #\b #\B)
  (test #f char-ci<? #\370 #\370)
  (test #f char-ci<? #\371 #\370)
  (test #t char-ci<? #\370 #\371)
  (arity-test char-ci<? 2 -1)
  (err/rt-test (char-ci<? #\a 1)) 
  (err/rt-test (char-ci<? #\b #\a 1)) 
  (err/rt-test (char-ci<? 1 #\a))

  (test #f char-ci>? #\A #\B)
  (test #f char-ci>? #\B #\A #\C)
  (test #t char-ci>? #\C #\B #\A)
  (test #f char-ci>? #\a #\B)
  (test #f char-ci>? #\A #\b)
  (test #f char-ci>? #\a #\b)
  (test #t char-ci>? #\C #\b #\A)
  (test #t char-ci>? #\9 #\0)
  (test #f char-ci>? #\A #\A)
  (test #f char-ci>? #\A #\a)
  (test #f char-ci>? #\370 #\370)
  (test #t char-ci>? #\371 #\370)
  (test #f char-ci>? #\370 #\371)
  (arity-test char-ci>? 2 -1)
  (err/rt-test (char-ci>? #\a 1)) 
  (err/rt-test (char-ci>? #\a #\b 1)) 
  (err/rt-test (char-ci>? 1 #\a))

  (test #t char-ci<=? #\A #\B)
  (test #t char-ci<=? #\a #\B)
  (test #t char-ci<=? #\a #\B #\C)
  (test #f char-ci<=? #\a #\b #\A)
  (test #t char-ci<=? #\A #\b)
  (test #t char-ci<=? #\a #\b)
  (test #f char-ci<=? #\9 #\0)
  (test #t char-ci<=? #\A #\A)
  (test #t char-ci<=? #\A #\a)
  (test #t char-ci<=? #\370 #\370)
  (test #f char-ci<=? #\371 #\370)
  (test #t char-ci<=? #\370 #\371)
  (arity-test char-ci<=? 2 -1)
  (err/rt-test (char-ci<=? #\a 1)) 
  (err/rt-test (char-ci<=? #\b #\a 1)) 
  (err/rt-test (char-ci<=? 1 #\a))

  (test #f char-ci>=? #\A #\B)
  (test #f char-ci>=? #\B #\A #\C)
  (test #t char-ci>=? #\B #\B #\A)
  (test #f char-ci>=? #\a #\B)
  (test #f char-ci>=? #\A #\b)
  (test #f char-ci>=? #\a #\b)
  (test #t char-ci>=? #\9 #\0)
  (test #t char-ci>=? #\A #\A)
  (test #t char-ci>=? #\A #\a)
  (test #t char-ci>=? #\370 #\370)
  (test #t char-ci>=? #\371 #\370)
  (test #f char-ci>=? #\370 #\371)
  (arity-test char-ci>=? 2 -1)
  (err/rt-test (char-ci>=? #\a 1)) 
  (err/rt-test (char-ci>=? #\a #\b 1)) 
  (err/rt-test (char-ci>=? 1 #\a)))

(char-tests)      

(define (ascii-range start end)
  (let ([s (or (and (number? start) start) (char->integer start))]
	[e (or (and (number? end) end) (char->integer end))])
    (let loop ([n e][l (list (integer->char e))])
      (if (= n s)
	  l
	  (let ([n (sub1 n)])
	    (loop n (cons (integer->char n) l)))))))

(define uppers (ascii-range #\A #\Z))
(define lowers (ascii-range #\a #\z))


(define alphas (append uppers lowers))
(define digits (ascii-range #\0 #\9))
(define whites (list #\newline #\return #\space #\page #\tab #\vtab))

(define (test-all is-a? name members)
  (let loop ([n 0])
    (unless (= n 128)
      (let ([c (integer->char n)])
	(test (and (memq c members) #t) `(,is-a? (integer->char ,n)) (is-a? c))
	(loop (add1 n)))))
  (arity-test is-a? 1 1)
  (err/rt-test (is-a? 1)))

(test-all char-alphabetic? 'char-alphabetic? alphas) 
(test-all char-numeric? 'char-numeric? digits) 
(test-all char-whitespace? 'char-whitespace? whites) 
(test-all char-upper-case? 'char-upper-case? uppers) 
(test-all char-lower-case? 'char-lower-case? lowers) 

(let loop ([n 0])
  (unless (= n 512)
     (test n 'integer->char (char->integer (integer->char n)))
     (loop (add1 n))))

(test 0 char->integer #\nul)
(test 10 char->integer #\newline)
(test 13 char->integer #\return)
(test 9 char->integer #\tab)
(test 8 char->integer #\backspace)
(test 12 char->integer #\page)
(test 32 char->integer #\space)
(test 127 char->integer #\rubout)
(test #\null 'null #\nul)
(test #\newline 'linefeed #\linefeed)

(test #\. integer->char (char->integer #\.))
(test #\A integer->char (char->integer #\A))
(test #\a integer->char (char->integer #\a))
(test #\371 integer->char (char->integer #\371))
(test #\U12345 integer->char (char->integer #\U12345))
(arity-test integer->char 1 1)
(arity-test char->integer 1 1)
(err/rt-test (integer->char 5.0))
(err/rt-test (integer->char 'a))
(err/rt-test (integer->char -1))
(err/rt-test (integer->char (expt 2 32)))
(err/rt-test (integer->char 10000000000000000))
(err/rt-test (char->integer 5))

(define (test-up/down case case-name members memassoc)
  (let loop ([n 0])
    (unless (= n 128)
      (let ([c (integer->char n)])
	(if (memq c members)
	    (test (cdr (assq c memassoc)) case c)
	    (test c case c)))
      (loop (add1 n))))
  (arity-test case 1 1)
  (err/rt-test (case 2)))

(test-up/down char-upcase 'char-upcase lowers (map cons lowers uppers))
(test-up/down char-downcase 'char-downcase uppers (map cons uppers lowers))

(test #t string? "The word \"recursion\\\" has many meanings.")
(test #t string? "")
(arity-test string? 1 1)
(test 3 'make-string (string-length (make-string 3)))
(test "" make-string 0)
(arity-test make-string 1 2)
(err/rt-test (make-string "hello"))
(err/rt-test (make-string 5 "hello"))
(err/rt-test (make-string 5.0 #\b))
(err/rt-test (make-string 5.2 #\a))
(err/rt-test (make-string -5 #\f))
(err/rt-test (make-string 500000000000000 #\f) exn:fail:out-of-memory?) ;; bignum on 32-bit machines
(err/rt-test (make-string 50000000000000000000 #\f) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines


(define f (make-string 3 #\*))
(test "?**" 'string-set! (begin (string-set! f 0 #\?) f))
(arity-test string-set! 3 3)
(err/rt-test (string-set! "hello" 0 #\a)) ; immutable string constant
(define hello-string (string-copy "hello"))
(err/rt-test (string-set! hello-string 'a #\a))
(err/rt-test (string-set! 'hello 4 #\a))
(err/rt-test (string-set! hello-string 4 'a))
(err/rt-test (string-set! hello-string 4.0 'a))
(err/rt-test (string-set! hello-string 5 #\a) exn:application:mismatch?)
(err/rt-test (string-set! hello-string -1 #\a))
(err/rt-test (string-set! hello-string (expt 2 100) #\a) exn:application:mismatch?)
(test "abc" string #\a #\b #\c)
(test "" string)
(err/rt-test (string #\a 1))
(err/rt-test (string 1 #\a))
(err/rt-test (string 1))
(test 3 string-length "abc")
(test 0 string-length "")
(arity-test string-length 1 1)
(err/rt-test (string-length 'apple))
(test #\a string-ref "abc" 0)
(test #\c string-ref "abc" 2)
(arity-test string-ref 2 2)
(err/rt-test (string-ref 'apple 4))
(err/rt-test (string-ref "apple" 4.0))
(err/rt-test (string-ref "apple" '(4)))
(err/rt-test (string-ref "apple" 5) exn:application:mismatch?)
(err/rt-test (string-ref "" 0) exn:application:mismatch?)
(err/rt-test (string-ref "" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (string-ref "apple" -1))
(test "" substring "ab" 0 0)
(test "" substring "ab" 1 1)
(test "" substring "ab" 2 2)
(test "a" substring "ab" 0 1)
(test "b" substring "ab" 1 2)
(test "ab" substring "ab" 0 2)
(test "ab" substring "ab" 0)
(test "b" substring "ab" 1)
(test "" substring "ab" 2)
(test (string #\a #\nul #\b) substring (string #\- #\a #\nul #\b #\*) 1 4)
(arity-test substring 2 3)
(err/rt-test (substring 'hello 2 3))
(err/rt-test (substring "hello" "2" 3))
(err/rt-test (substring "hello" 2.0 3))
(err/rt-test (substring "hello" 2 3.0))
(err/rt-test (substring "hello" 2 "3"))
(err/rt-test (substring "hello" 2 7) exn:application:mismatch?)
(err/rt-test (substring "hello" -2 3))
(err/rt-test (substring "hello" 4 3) exn:application:mismatch?)
(err/rt-test (substring "hello" (expt 2 100) 3) exn:application:mismatch?)
(err/rt-test (substring "hello" (expt 2 100) 5) exn:application:mismatch?)
(err/rt-test (substring "hello" 3 (expt 2 100)) exn:application:mismatch?)
(test "foobar" string-append "foo" "bar")
(test "foo" string-append "foo")
(test "foo" string-append "foo" "")
(test "foogoo" string-append "foo" "" "goo")
(test "foo" string-append "" "foo")
(test "" string-append)
(test (string #\a #\nul #\b #\c #\nul #\d) 
      string-append (string #\a #\nul #\b) (string #\c #\nul #\d))
(err/rt-test (string-append 1))
(err/rt-test (string-append "hello" 1))
(err/rt-test (string-append "hello" 1 "done"))
(test "" make-string 0)
(define s (string-copy "hello"))
(define s2 (string-copy s))
(test "hello" 'string-copy s2)
(string-set! s 2 #\x)
(test "hello" 'string-copy s2)
(test (string #\a #\nul #\b) string-copy (string #\a #\nul #\b))
(string-fill! s #\x)
(test "xxxxx" 'string-fill! s)
(arity-test string-copy 1 1)
(arity-test string-fill! 2 2)
(err/rt-test (string-copy 'blah))
(err/rt-test (string-fill! 'sym #\1))
(err/rt-test (string-fill! "static" #\1))
(err/rt-test (string-fill! (string-copy "oops") 5))

(let ([s (make-string 10 #\x)])
  (test (void) string-copy! s 0 "hello")
  (test "helloxxxxx" values s)
  (test (void) string-copy! s 3 "hello")
  (test "helhelloxx" values s)
  (err/rt-test (string-copy! s 6 "hello") exn:application:mismatch?)
  (test (void) string-copy! s 5 "hello" 3)
  (test "helhelooxx" values s)
  (test (void) string-copy! s 5 "hello" 3)
  (test "helhelooxx" values s)
  (test (void) string-copy! s 0 "hello" 3 4)
  (test "lelhelooxx" values s)
  (test (void) string-copy! s 1 "hello" 3 5)
  (test "llohelooxx" values s)
  (err/rt-test (string-copy! s 1 "hello" 3 6) exn:application:mismatch?))

(arity-test string-copy! 3 5)
(let ([s (string-copy x)])
  (err/rt-test (string-copy! "x" 0 "x"))
  (err/rt-test (string-copy! s "x" "x"))
  (err/rt-test (string-copy! 0 0 "x"))
  (err/rt-test (string-copy! s 0 "x" -1))
  (err/rt-test (string-copy! s 0 "x" 1 0) exn:application:mismatch?)
  (err/rt-test (string-copy! s 2 "x" 0 1) exn:application:mismatch?))

(test "Hello, and how are you?" string->immutable-string "Hello, and how are you?")
(arity-test string->immutable-string 1 1)
(err/rt-test (string->immutable-string 'hello))

(define ax (string #\a #\nul #\370 #\x))
(define abigx (string #\a #\nul #\370 #\X))
(define ax2 (string #\a #\nul #\370 #\x))
(define ay (string #\a #\nul #\371 #\x))

(define (string-tests)
  (test #t string=? "" "")
  (test #f string<? "" "")
  (test #f string>? "" "")
  (test #t string<=? "" "")
  (test #t string>=? "" "")
  (test #t string-ci=? "" "")
  (test #f string-ci<? "" "")
  (test #f string-ci>? "" "")
  (test #t string-ci<=? "" "")
  (test #t string-ci>=? "" "")

  (test #f string=? "A" "B")
  (test #f string=? "a" "b")
  (test #f string=? "9" "0")
  (test #t string=? "A" "A")
  (test #f string=? "A" "AB")
  (test #t string=? ax ax2)
  (test #f string=? ax abigx)
  (test #f string=? ax ay)
  (test #f string=? ay ax)

  (test #t string<? "A" "B")
  (test #t string<? "a" "b")
  (test #f string<? "9" "0")
  (test #f string<? "A" "A")
  (test #t string<? "A" "AB")
  (test #f string<? "AB" "A")
  (test #f string<? ax ax2)
  (test #t string<? ax ay)
  (test #f string<? ay ax)

  (test #f string>? "A" "B")
  (test #f string>? "a" "b")
  (test #t string>? "9" "0")
  (test #f string>? "A" "A")
  (test #f string>? "A" "AB")
  (test #t string>? "AB" "A")
  (test #f string>? ax ax2)
  (test #f string>? ax ay)
  (test #t string>? ay ax)

  (test #t string<=? "A" "B")
  (test #t string<=? "a" "b")
  (test #f string<=? "9" "0")
  (test #t string<=? "A" "A")
  (test #t string<=? "A" "AB")
  (test #f string<=? "AB" "A")
  (test #t string<=? ax ax2)
  (test #t string<=? ax ay)
  (test #f string<=? ay ax)

  (test #f string>=? "A" "B")
  (test #f string>=? "a" "b")
  (test #t string>=? "9" "0")
  (test #t string>=? "A" "A")
  (test #f string>=? "A" "AB")
  (test #t string>=? "AB" "A")
  (test #t string>=? ax ax2)
  (test #f string>=? ax ay)
  (test #t string>=? ay ax)

  (test #f string-ci=? "A" "B")
  (test #f string-ci=? "a" "B")
  (test #f string-ci=? "A" "b")
  (test #f string-ci=? "a" "b")
  (test #f string-ci=? "9" "0")
  (test #t string-ci=? "A" "A")
  (test #t string-ci=? "A" "a")
  (test #f string-ci=? "A" "AB")
  (test #t string-ci=? ax ax2)
  (test #t string-ci=? ax abigx)
  (test #f string-ci=? ax ay)
  (test #f string-ci=? ay ax)
  (test #f string-ci=? abigx ay)
  (test #f string-ci=? ay abigx)

  (test #t string-ci<? "A" "B")
  (test #t string-ci<? "a" "B")
  (test #t string-ci<? "A" "b")
  (test #t string-ci<? "a" "b")
  (test #f string-ci<? "9" "0")
  (test #f string-ci<? "A" "A")
  (test #f string-ci<? "A" "a")
  (test #t string-ci<? "A" "AB")
  (test #f string-ci<? "AB" "A")
  (test #f string-ci<? ax ax2)
  (test #f string-ci<? ax abigx)
  (test #t string-ci<? ax ay)
  (test #f string-ci<? ay ax)
  (test #t string-ci<? abigx ay)
  (test #f string-ci<? ay abigx)

  (test #f string-ci>? "A" "B")
  (test #f string-ci>? "a" "B")
  (test #f string-ci>? "A" "b")
  (test #f string-ci>? "a" "b")
  (test #t string-ci>? "9" "0")
  (test #f string-ci>? "A" "A")
  (test #f string-ci>? "A" "a")
  (test #f string-ci>? "A" "AB")
  (test #t string-ci>? "AB" "A")
  (test #f string-ci>? ax ax2)
  (test #f string-ci>? ax abigx)
  (test #f string-ci>? ax ay)
  (test #t string-ci>? ay ax)
  (test #f string-ci>? abigx ay)
  (test #t string-ci>? ay abigx)

  (test #t string-ci<=? "A" "B")
  (test #t string-ci<=? "a" "B")
  (test #t string-ci<=? "A" "b")
  (test #t string-ci<=? "a" "b")
  (test #f string-ci<=? "9" "0")
  (test #t string-ci<=? "A" "A")
  (test #t string-ci<=? "A" "a")
  (test #t string-ci<=? "A" "AB")
  (test #f string-ci<=? "AB" "A")
  (test #t string-ci<=? ax ax2)
  (test #t string-ci<=? ax abigx)
  (test #t string-ci<=? ax ay)
  (test #f string-ci<=? ay ax)
  (test #t string-ci<=? abigx ay)
  (test #f string-ci<=? ay abigx)

  (test #f string-ci>=? "A" "B")
  (test #f string-ci>=? "a" "B")
  (test #f string-ci>=? "A" "b")
  (test #f string-ci>=? "a" "b")
  (test #t string-ci>=? "9" "0")
  (test #t string-ci>=? "A" "A")
  (test #t string-ci>=? "A" "a")
  (test #f string-ci>=? "A" "AB")
  (test #t string-ci>=? "AB" "A")
  (test #t string-ci>=? ax ax2)
  (test #t string-ci>=? ax abigx)
  (test #f string-ci>=? ax ay)
  (test #t string-ci>=? ay ax)
  (test #f string-ci>=? abigx ay)
  (test #t string-ci>=? ay abigx))

(string-tests)

(map (lambda (pred)
       (arity-test pred 2 -1)
       (err/rt-test (pred "a" 1))
       (err/rt-test (pred "a" "b" 5))
       (err/rt-test (pred 1 "a")))
     (list string=? 
	   string>? 
	   string<? 
	   string>=? 
	   string<=? 
	   string-ci=? 
	   string-ci>? 
	   string-ci<? 
	   string-ci>=? 
	   string-ci<=?
	   string-locale=? 
	   string-locale>? 
	   string-locale<? 
	   string-locale-ci=? 
	   string-locale-ci>? 
	   string-locale-ci<?))


(test #t byte? 10)
(test #t byte? 0)
(test #t byte? 255)
(test #f byte? 256)
(test #f byte? -1)
(test #f byte? (expt 2 40))
(test #f byte? (expt 2 100))
(test #f byte? #\newline)

(test #t bytes? #"The word \"recursion\\\" has many meanings.")
(test #t bytes? #"")
(arity-test bytes? 1 1)
(test 3 'make-bytes (bytes-length (make-bytes 3)))
(test #"" make-bytes 0)
(arity-test make-bytes 1 2)
(err/rt-test (make-bytes #"hello"))
(err/rt-test (make-bytes 5 #"hello"))
(err/rt-test (make-bytes 5.0 98))
(err/rt-test (make-bytes 5.2 97))
(err/rt-test (make-bytes -5 98))
(err/rt-test (make-bytes 50000000000000000000 #\f))
(err/rt-test (make-bytes 500000000000000 45) exn:fail:out-of-memory?) ;; bignum on 32-bit machines
(err/rt-test (make-bytes 50000000000000000000 45) exn:fail:out-of-memory?)  ;; bignum on 64-bit machines


(define f (make-bytes 3 (char->integer #\*)))
(test #"?**" 'bytes-set! (begin (bytes-set! f 0 (char->integer #\?)) f))
(arity-test bytes-set! 3 3)
(err/rt-test (bytes-set! #"hello" 0 #\a)) ; immutable bytes constant
(define hello-bytes (bytes-copy #"hello"))
(err/rt-test (bytes-set! hello-bytes 'a 97))
(err/rt-test (bytes-set! 'hello 4 97))
(err/rt-test (bytes-set! hello-bytes 4 'a))
(err/rt-test (bytes-set! hello-bytes 4.0 'a))
(err/rt-test (bytes-set! hello-bytes 5 97) exn:application:mismatch?)
(err/rt-test (bytes-set! hello-bytes -1 97))
(err/rt-test (bytes-set! hello-bytes (expt 2 100) 97) exn:application:mismatch?)
(test #"abc" bytes 97 98 99)
(test #"" bytes)
(err/rt-test (bytes #\a 1))
(err/rt-test (bytes 1 #\a))
(err/rt-test (bytes #\1))
(test 3 bytes-length #"abc")
(test 0 bytes-length #"")
(arity-test bytes-length 1 1)
(err/rt-test (bytes-length 'apple))
(test 97 bytes-ref #"abc" 0)
(test 99 bytes-ref #"abc" 2)
(arity-test bytes-ref 2 2)
(err/rt-test (bytes-ref 'apple 4))
(err/rt-test (bytes-ref #"apple" 4.0))
(err/rt-test (bytes-ref #"apple" '(4)))
(err/rt-test (bytes-ref #"apple" 5) exn:application:mismatch?)
(err/rt-test (bytes-ref #"" 0) exn:application:mismatch?)
(err/rt-test (bytes-ref #"" (expt 2 100)) exn:application:mismatch?)
(err/rt-test (bytes-ref #"apple" -1))
(test #"" subbytes #"ab" 0 0)
(test #"" subbytes #"ab" 1 1)
(test #"" subbytes #"ab" 2 2)
(test #"a" subbytes #"ab" 0 1)
(test #"b" subbytes #"ab" 1 2)
(test #"ab" subbytes #"ab" 0 2)
(test #"ab" subbytes #"ab" 0)
(test #"b" subbytes #"ab" 1)
(test #"" subbytes #"ab" 2)
(test (bytes 97 0 98) subbytes (bytes 32 97 0 98 45) 1 4)
(arity-test subbytes 2 3)
(err/rt-test (subbytes 'hello 2 3))
(err/rt-test (subbytes #"hello" #"2" 3))
(err/rt-test (subbytes #"hello" 2.0 3))
(err/rt-test (subbytes #"hello" 2 3.0))
(err/rt-test (subbytes #"hello" 2 #"3"))
(err/rt-test (subbytes #"hello" 2 7) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" -2 3))
(err/rt-test (subbytes #"hello" 4 3) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" (expt 2 100) 3) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" (expt 2 100) 5) exn:application:mismatch?)
(err/rt-test (subbytes #"hello" 3 (expt 2 100)) exn:application:mismatch?)
(test #"foobar" bytes-append #"foo" #"bar")
(test #"foo" bytes-append #"foo")
(test #"foo" bytes-append #"foo" #"")
(test #"foogoo" bytes-append #"foo" #"" #"goo")
(test #"foo" bytes-append #"" #"foo")
(test #"" bytes-append)
(test (bytes 97 0 98 99 0 100) 
      bytes-append (bytes 97 0 98) (bytes 99 0 100))
(err/rt-test (bytes-append 1))
(err/rt-test (bytes-append #"hello" 1))
(err/rt-test (bytes-append #"hello" 1 #"done"))
(test #"" make-bytes 0)
(define s (bytes-copy #"hello"))
(define s2 (bytes-copy s))
(test #"hello" 'bytes-copy s2)
(bytes-set! s 2 (char->integer #\x))
(test #"hello" 'bytes-copy s2)
(test (bytes 97 0 98) bytes-copy (bytes 97 0 98))
(bytes-fill! s (char->integer #\x))
(test #"xxxxx" 'bytes-fill! s)
(arity-test bytes-copy 1 1)
(arity-test bytes-fill! 2 2)
(err/rt-test (bytes-copy 'blah))
(err/rt-test (bytes-fill! 'sym 1))
(err/rt-test (bytes-fill! #"static" 1))
(err/rt-test (bytes-fill! (bytes-copy #"oops") #\5))

(define r (regexp "(-[0-9]*)+"))
(test '("-12--345" "-345") regexp-match r "a-12--345b")
(test '((1 . 9) (5 . 9)) regexp-match-positions r "a-12--345b")
(test '("--345" "-345") regexp-match r "a-12--345b" 2)
(test '("--34" "-34") regexp-match r "a-12--345b" 2 8)
(test '((4 . 9) (5 . 9)) regexp-match-positions r "a-12--345b" 2)
(test '((4 . 8) (5 . 8)) regexp-match-positions r "a-12--345b" 2 8)
(test '("a-b") regexp-match "a[-c]b" "a-b")
(test '("a-b") regexp-match "a[c-]b" "a-b")
(test #f regexp-match "x+" "12345")
(test "su casa" regexp-replace "mi" "mi casa" "su")
(define r2 (regexp "([Mm])i ([a-zA-Z]*)"))
(define insert "\\1y \\2")
(test "My Casa" regexp-replace r2 "Mi Casa" insert)
(test "my cerveza Mi Mi Mi" regexp-replace r2 "mi cerveza Mi Mi Mi" insert)
(test "my cerveza My Mi Mi" regexp-replace* r2 "mi cerveza Mi Mi Mi" insert)
(test "bbb" regexp-replace* "a" "aaa" "b")

;; Test weird port offsets:
(define (test-weird-offset regexp-match regexp-match-positions)
  (test #f regexp-match "e" (open-input-string ""))
  (test #f regexp-match "e" (open-input-string "") (expt 2 100))
  (test #f regexp-match "e" (open-input-string "") (expt 2 100) (expt 2 101))
  (test '((3 . 4)) regexp-match-positions "e" (open-input-string "eaae") 2 (expt 2 101)))
(test-weird-offset regexp-match regexp-match-positions)
(test-weird-offset regexp-match-peek regexp-match-peek-positions)

;; Check greedy and non-greedy operators:
(define (do-the-tests prefix suffix start end)
  (define input (format "~a~a~a" prefix "<tag1 b=5> <tag2 bb=7>" suffix))
  (define (check-greedy-stuff mk-input regexp-match regexp-match-positions)
    (define (testre s-answer p-answer pattern)
      (let ([p-answer (if (and p-answer start)
			  (list (cons (+ start (caar p-answer))
				      (+ start (cdar p-answer))))
			  p-answer)])
	(cond
	 [end
	  (test s-answer regexp-match pattern (mk-input) start (+ end (string-length input)))
	  (test p-answer regexp-match-positions pattern (mk-input) start (+ end (string-length input)))]
	 [start
	  (test s-answer regexp-match pattern (mk-input) start)
	  (test p-answer regexp-match-positions pattern (mk-input) start)]
	 [else
	  (test s-answer regexp-match pattern (mk-input))
	  (test p-answer regexp-match-positions pattern (mk-input))])))
    (define strs
      (if (string? (mk-input))
	  list
	  (lambda l (map string->bytes/utf-8 l))))
    
    (testre (strs "<tag1 b=5> <tag2 bb=7>") '((0 . 22)) "<.*>")
    (testre (strs "<tag1 b=5>") '((0 . 10)) "<.*?>")
    (testre (strs "<tag1 b=5> <tag2 bb=7>") '((0 . 22)) "<.*?>$")
    (testre (strs "") '((0 . 0)) "b*")
    (testre (strs "<tag") '((0 . 4)) "^<[tag]*")
    (testre (strs "<tag") '((0 . 4)) "<[tag]*")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]*1")
    (testre (strs "") '((0 . 0)) "b*?")
    (testre (strs "<") '((0 . 1)) "<[tag]*?")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]*?1")
    (testre (strs "b") '((6 . 7)) "b+?")
    (testre #f #f "^b+?")
    (testre (strs "<t") '((0 . 2)) "<[tag]+?")
    (testre (strs "<tag1") '((0 . 5)) "<[tag]+?1")
    (testre (strs "") '((0 . 0)) "b??")
    (testre (strs "") '((0 . 0)) "[tag]??")
    (testre (strs "g1") '((3 . 5)) "[tag]??1")
    (testre (strs "ag") '((2 . 4)) "[a-m]+"))
  
  (check-greedy-stuff (lambda () input) regexp-match regexp-match-positions)
  (check-greedy-stuff (lambda () (open-input-string input)) regexp-match regexp-match-positions)
  (let ([p (open-input-string input)])
    (check-greedy-stuff (lambda () p) regexp-match-peek regexp-match-peek-positions))
  (let ([mk (lambda ()
	      (let-values ([(r w) (make-pipe)])
		(thread (lambda ()
			  (let loop ([s 0])
			    (let ([e (min (+ s 1)
					  (string-length input))])
			      (display (substring input s e) w)
			      (sleep)
			      (unless (= e s)
				(loop e))))
			  (close-output-port w)))
		r))])
    (check-greedy-stuff mk regexp-match regexp-match-positions)
    (let ([p (mk)])
      (check-greedy-stuff (lambda () p) regexp-match-peek regexp-match-peek-positions))))

(do-the-tests "" "" #f #f)
(do-the-tests "" "" 0 #f)
(do-the-tests "" "" 0 0)
(do-the-tests "ack" "" 3 #f)
(do-the-tests "ack" "" 3 0)
(do-the-tests "ack" "hmm" 3 -3)

(test '((10002 . 10003)) regexp-match-positions "a" (open-input-string (format "~abbac" (make-string 10000 #\x))))

;; Test regexp with null chars:
(let* ([s (string #\a #\b #\nul #\c)]
       [3s (string-append s s s)])
  (test #f regexp-match (string #\nul) "no nulls")
  (test (list s) regexp-match s s)
  (test (list 3s s) regexp-match (format "(~a)*" s) 3s)
  (test (list (string #\b #\nul #\c)) regexp-match (string #\[ #\nul #\b #\] #\* #\c) s)
  (test (list (string #\a #\b #\nul)) regexp-match (string #\a #\[ #\b #\nul #\] #\+) s)
  (test "hihihi" regexp-replace* (string #\nul) (string #\nul #\nul #\nul) "hi"))
(test (string #\- #\nul #\+ #\- #\nul #\+ #\- #\nul #\+)
      regexp-replace* "a" "aaa" (string #\- #\nul #\+))

(test "xpple" regexp-replace #rx"a" "apple" "x")
(test #"xpple" regexp-replace #rx#"a" "apple" "x")
(test #"xpple" regexp-replace #rx"a" #"apple" "x")
(test #"xpple" regexp-replace #rx#"a" #"apple" "x")
(err/rt-test (regexp-replace #rx"a" "apple" #"x"))

(test "pAPple" regexp-replace #rx"a(.)" "apple" (lambda (a b) (string-append b (string-upcase a))))
(test #"p.ap.ple" regexp-replace #rx#"a(.)" "apple" (lambda (a b) (bytes-append b #"." a #".")))
(test #"p.ap.ple" regexp-replace #rx"a(.)" #"apple" (lambda (a b) (bytes-append b #"." a #".")))
(test #"p.ap.ple" regexp-replace #rx#"a(.)" #"apple" (lambda (a b) (bytes-append b #"." a #".")))
(err/rt-test (regexp-replace #rx#"a(.)" #"apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx#"a(.)" "apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx"a(.)" #"apple" (lambda (a b) "string")))
(err/rt-test (regexp-replace #rx"a(.)" "apple" (lambda (a b) #"bytes")))

;; Check extremely many subexpressions:
(for-each
 (lambda (mx)
   (let* ([v (make-vector mx null)]
	  [open (make-vector mx #t)])
     (let loop ([n 0][m 0][s null])
       (cond
	[(and (= n mx) (zero? m))
	 (let* ([s (list->string (reverse! s))]
		[plain (regexp-replace* "[()]" s "")])
	   (test (cons plain (map list->string (map reverse! (vector->list v)))) regexp-match s plain))]
	[(or (= n mx) (< (random 10) 3))
	 (if (and (positive? m)
		  (< (random 10) 7))
	     (begin
	       (let loop ([p 0][m (sub1 m)])
		 (if (vector-ref open p)
		     (if (zero? m)
			 (vector-set! open p #f)
			 (loop (add1 p) (sub1 m)))
		     (loop (add1 p) m)))
	       (loop n (sub1 m) (cons #\) s)))

	     (let ([c (integer->char (+ (char->integer #\a) (random 26)))])
	       (let loop ([p 0])
		 (unless (= p n)
		   (when (vector-ref open p)
		     (vector-set! v p (cons c (vector-ref v p))))
		   (loop (add1 p))))
	       (loop n m (cons c s))))]
	[else
	 (loop (add1 n) (add1 m) (cons #\( s))]))))
 '(1 10 100 500))

(define (test-bad-re-args who)
  (err/rt-test (who 'e "hello"))
  (err/rt-test (who "e" 'hello))
  (err/rt-test (who "e" "hello" -1 5))
  (err/rt-test (who "e" "hello" (- (expt 2 100)) 5))
  (err/rt-test (who "e" (open-input-string "") (- (expt 2 100)) 5))
  (err/rt-test (who "e" "hello" 1 (- (expt 2 100))))
  (err/rt-test (who "e" (open-input-string "") 1 (- (expt 2 100))))
  (err/rt-test (who "e" "hello" 1 +inf.0))
  (err/rt-test (who "e" "" 0 1) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" 3 2) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" 3 12) exn:application:mismatch?)
  (err/rt-test (who "e" "hello" (expt 2 100) 5)  exn:application:mismatch?)
  (err/rt-test (who "e" (open-input-string "") (expt 2 100) 5)  exn:application:mismatch?)
  (err/rt-test (who "e" (open-input-string "") (expt 2 100) (sub1 (expt 2 100))) exn:application:mismatch?))
(test-bad-re-args regexp-match)
(test-bad-re-args regexp-match-positions)

;; Test non-capturing parens
(test '("1aaa2" "a") regexp-match #rx"1(a)*2" "01aaa23")
(test '("1aaa2") regexp-match #rx"1(?:a)*2" "01aaa23")
(test '("1akakak2" "ak") regexp-match #rx"1(ak)*2" "01akakak23")
(test '("1akakak2") regexp-match #rx"1(?:ak)*2" "01akakak23")
(test '("1akakkakkkk2" "akkkk") regexp-match #rx"1(ak*)*2" "01akakkakkkk23")
(test '("1akakkakkkk2") regexp-match #rx"1(?:ak*)*2" "01akakkakkkk23")
(test '("01akakkakkkk23" "1akakkakkkk2" "1" "a" "k" "2") 
      regexp-match #rx"(?:0)(((?:1))(?:(a)(?:(k))*)*((?:2)))(?:3)" "_01akakkakkkk23_")

(test '((1 . 10) (7 . 9)) regexp-match-positions #rx"1(ak*)*2" "01akakkak23")
(test '((1 . 10)) regexp-match-positions #rx"1(?:ak*)*2" "01akakkak23")

;; Regexps that shouldn't work:
(err/rt-test (regexp "[a--b]") exn:fail?)
(err/rt-test (regexp "[a-b-c]") exn:fail?)

;; A good test of unicode-friendly ".":
(test '("load-extension: couldn't open \\\" (%s)\"") 
      regexp-match 
      (regexp "^(?:[^\\\"]|\\\\.)*\"") "load-extension: couldn't open \\\" (%s)\"")

;; Test bounded byte consumption on failure:
(let ([is (open-input-string "barfoo")]) 
  (test '(#f #\f) list (regexp-match "^foo" is 0 3) (read-char is)))
(let ([is (open-input-string "barfoo")]) 
  (test '(#f #\f) list (regexp-match "foo" is 0 3) (read-char is)))

(arity-test regexp 1 1)
(arity-test regexp? 1 1)
(arity-test regexp-match 2 5)
(arity-test regexp-match-positions 2 5)
(arity-test regexp-match-peek 2 5)
(arity-test regexp-match-peek-positions 2 5)
(arity-test regexp-replace 3 3)
(arity-test regexp-replace* 3 3)

(test #t vector? '#(0 (2 2 2 2) "Anna"))
(test #t vector? '#())
(arity-test vector? 1 1)
(test '#(a b c) vector 'a 'b 'c)
(test '#() vector)
(test 3 vector-length '#(0 (2 2 2 2) "Anna"))
(test 0 vector-length '#())
(arity-test vector-length 1 1)
(err/rt-test (vector-length "apple"))
(test 8 vector-ref '#(1 1 2 3 5 8 13 21) 5)
(arity-test vector-ref 2 2)
(err/rt-test (vector-ref "apple" 3))
(err/rt-test (vector-ref #(4 5 6) 3) exn:application:mismatch?)
(err/rt-test (vector-ref #() 0) exn:application:mismatch?)
(err/rt-test (vector-ref #() (expt 2 100)) exn:application:mismatch?)
(err/rt-test (vector-ref #(4 5 6) -1))
(err/rt-test (vector-ref #(4 5 6) 2.0))
(err/rt-test (vector-ref #(4 5 6) "2"))
(test '#(0 ("Sue" "Sue") "Anna") 'vector-set
	(let ((vec (vector 0 '(2 2 2 2) "Anna")))
	  (vector-set! vec 1 '("Sue" "Sue"))
	  vec))
(test '#(hi hi) make-vector 2 'hi)
(test '#() make-vector 0)
(test '#() make-vector 0 'a)
(test 2048 vector-length (make-vector 2048 'a))
(arity-test make-vector 1 2)
(err/rt-test (make-vector "a" 'a))
(err/rt-test (make-vector 1.0 'a))
(err/rt-test (make-vector 10.2 'a))
(err/rt-test (make-vector -1 'a))
(err/rt-test (make-vector 1000000000000000000000 'a) exn:fail:out-of-memory?)
(arity-test vector-set! 3 3)
(err/rt-test (vector-set! #() 0 'x) exn:application:mismatch?)
(err/rt-test (vector-set! #(1 2 3) -1 'x))
(err/rt-test (vector-set! #(1 2 3) 3 'x) exn:application:mismatch?)
(err/rt-test (vector-set! #(1 2 3) (expt 2 100) 'x) exn:application:mismatch?)
(err/rt-test (vector-set! '(1 2 3) 2 'x))
(err/rt-test (vector-set! #(1 2 3) "2" 'x))
(define v (quote #(1 2 3)))
(vector-fill! v 0)
(test (quote #(0 0 0)) 'vector-fill! v)
(arity-test vector-fill! 2 2)
(err/rt-test (vector-fill! '(1 2 3) 0))

(test #t procedure? car)
(test #f procedure? 'car)
(test #t procedure? (lambda (x) (* x x)))
(test #f procedure? '(lambda (x) (* x x)))
(test #t call-with-current-continuation procedure?)
(test #t call-with-escape-continuation procedure?)
(test #t procedure? (case-lambda ((x) x) ((x y) (+ x y))))
(arity-test procedure? 1 1)

(test 7 apply + (list 3 4))
(test 7 apply (lambda (a b) (+ a b)) (list 3 4))
(test 17 apply + 10 (list 3 4))
(test '() apply list '())
(define compose (lambda (f g) (lambda args (f (apply g args)))))
(test 30 (compose sqrt *) 12 75)
(err/rt-test (apply) exn:application:arity?)
(err/rt-test (apply (lambda x x)) exn:application:arity?)
(err/rt-test (apply (lambda x x) 1))
(err/rt-test (apply (lambda x x) 1 2))
(err/rt-test (apply (lambda x x) 1 '(2 . 3)))

(test '(b e h) map cadr '((a b) (d e) (g h)))
(test '(5 7 9) map + '(1 2 3) '(4 5 6))
(test '#(0 1 4 9 16) 'for-each
	(let ((v (make-vector 5)))
		(for-each (lambda (i) (vector-set! v i (* i i)))
			'(0 1 2 3 4))
		v))

(err/rt-test (let ([l (list 1 2 3)])
	       (for-each (lambda (x) (set-cdr! (cdr l) 1)) l))
	     exn:application:mismatch?)


(define (map-tests map)
  (let ([size? exn:application:mismatch?]
	[non-list? type?])
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '1))
    (err/rt-test (map (lambda (x y) (+ x y)) '2 '(1 2)))
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '(1 2 3)) size?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 3) '(1 2)) size?)
    (err/rt-test (map (lambda (x) (+ x)) '(1 2 . 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 . 3) '(1 2)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 . 3) '(1 2 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2) '(1 2 . 3)) non-list?)
    (err/rt-test (map (lambda (x y) (+ x y)) '(1 2 3) '(1 2 . 3)) non-list?)
    (err/rt-test (map) exn:application:arity?)
    (err/rt-test (map (lambda (x y) (+ x y))) exn:application:arity?)
    (err/rt-test (map (lambda () 10) null) exn:application:mismatch?)
    (err/rt-test (map (case-lambda [() 9] [(x y) 10]) '(1 2 3)) exn:application:mismatch?)
    (err/rt-test (map (lambda (x) 10) '(1 2) '(3 4)) exn:application:mismatch?)))
(map-tests map)
(map-tests for-each)
(map-tests andmap)
(map-tests ormap)

(test-values (list (void)) (lambda () (for-each (lambda (x) (values 1 2)) '(1 2))))
(err/rt-test (map (lambda (x) (values 1 2)) '(1 2)) arity?)

(test #t andmap add1 null)
(test #f ormap add1 null)
(test #f andmap positive? '(1 -2 3))
(test #t ormap positive? '(1 -2 3))
(test #f andmap negative? '(1 -2 3))
(test #t ormap negative? '(1 -2 3))
(test 4 andmap add1 '(1 2 3))
(test 2 ormap add1 '(1 2 3))

(err/rt-test (ormap (lambda (x) (values 1 2)) '(1 2)) arity?)
(err/rt-test (andmap (lambda (x) (values 1 2)) '(1 2)) arity?)

(test-values '(1 2) (lambda () (ormap (lambda (x) (values 1 2)) '(1))))
(test-values '(1 2) (lambda () (andmap (lambda (x) (values 1 2)) '(1))))

(test -3 call-with-current-continuation
		(lambda (exit)
		 (for-each (lambda (x) (if (negative? x) (exit x)))
		 	'(54 0 37 -3 245 19))
		#t))
(define list-length
 (lambda (obj)
  (call-with-current-continuation
   (lambda (return)
    (letrec ((r (lambda (obj) (cond ((null? obj) 0)
				((pair? obj) (+ (r (cdr obj)) 1))
				(else (return #f))))))
	(r obj))))))
(test 4 list-length '(1 2 3 4))
(test #f list-length '(a b . c))
(test '() map cadr '())

;;; This tests full conformance of call-with-current-continuation.  It
;;; is a separate test because some schemes do not support call/cc
;;; other than escape procedures.  I am indebted to
;;; raja@copper.ucs.indiana.edu (Raja Sooriamurthi) for fixing this
;;; code.  The function leaf-eq? compares the leaves of 2 arbitrary
;;; trees constructed of conses.  
(define (next-leaf-generator obj eot)
  (letrec ((return #f)
	   (cont (lambda (x)
		   (recurx obj)
		   (set! cont (lambda (x) (return eot)))
		   (cont #f)))
	   (recurx (lambda (obj)
		      (if (pair? obj)
			  (for-each recurx obj)
			  (call-with-current-continuation
			   (lambda (c)
			     (set! cont c)
			     (return obj)))))))
    (lambda () (call-with-current-continuation
		(lambda (ret) (set! return ret) (cont #f))))))
(define (leaf-eq? x y)
  (let* ((eot (list 'eot))
	 (xf (next-leaf-generator x eot))
	 (yf (next-leaf-generator y eot)))
    (letrec ((loop (lambda (x y)
		     (cond ((not (eq? x y)) #f)
			   ((eq? eot x) #t)
			   (else (loop (xf) (yf)))))))
      (loop (xf) (yf)))))
(define (test-cont)
  (newline)
  (display ";testing continuations; ")
  (test #t leaf-eq? '(a (b (c))) '((a) b c))
  (test #f leaf-eq? '(a (b (c))) '((a) b c d))
  '(report-errs))

(define (test-cc-values test-call/cc)
  (test '(a b c)
	call-with-values
	(lambda ()
	  (test-call/cc
	   (lambda (k)
	     (dynamic-wind
	      void
	      (lambda ()
		(k 'a 'b 'c))
	      (lambda ()
		(values 1 2))))))
	list)

  (test 1 dynamic-wind
	(lambda () (test-call/cc void))
	(lambda () 1)
	(lambda () (test-call/cc void)))

  ; Try devious jumping with pre- and post-thunks:
  (test 2 test-call/cc
	(lambda (exit)
	  (dynamic-wind
	   (lambda () (exit 2))
	   void
	   void)))
  (test 3 test-call/cc
	(lambda (exit)
	  (dynamic-wind
	   void
	   void
	   (lambda () (exit 3)))))

  (let ([rv
	 (lambda (get-v)
	   (let ([x 0])
	     (test-call/cc
	      (lambda (exit)
		(dynamic-wind
		 void
		 (lambda () (exit))
		 (lambda () (set! x (get-v))))))
	     x))]
	[r56
	 (lambda ()
	   (let ([x 0]
		 [y 1]
		 [c1 #f])
	     (dynamic-wind
	      (lambda () (set! x (add1 x)))
	      (lambda () 
		(let/cc k (set! c1 k))
		(if (>= x 5)
		    (set! c1 #f)))
	      (lambda () (set! y (add1 y))))
	     (when c1 (c1))
	     (list x y)))]
	[rx.y
	 (lambda (get-x get-y)
	   (let ([c1 #f]
		 [x 0]
		 [y 0])
	     (let ([v
		    (dynamic-wind
		     (lambda () (set! y x))
		     (lambda () (let/cc k (set! c1 k)))
		     (lambda () 
		       (set! x (get-x))
		       (when c1
			 ((begin0
			   c1
			   (set! c1 #f))
			  (get-y)))))])
	       (cons y v))))]
	[rv2
	 (lambda (get-v)
	   (let ([c1 #f]
		 [give-up #f])
	     (test-call/cc
	      (lambda (exit)
		(dynamic-wind
		 (lambda () (when give-up (give-up (get-v))))
		 (lambda () (let/cc k (set! c1 k)))
		 (lambda () (set! give-up exit) (c1)))))))]
	[r10-11-12
	 (lambda ()
	   (let ([c2 #f]
		 [x 10]
		 [y 11])
	     (let ([v (dynamic-wind
		       (lambda () (set! y (add1 y)))
		       (lambda () (begin0 x (set! x (add1 x))))
		       (lambda () (let/cc k (set! c2 k))))])
	       (when c2 ((begin0
			  c2
			  (set! c2 #f))))
	       (list v x y))))]
	[r13.14
	 (lambda ()
	   (let ([c0 #f]
		 [x 11]
		 [y 12])
	     (dynamic-wind
	      (lambda () (let/cc k (set! c0 k)))
	      (lambda () (set! x (add1 x)))
	      (lambda () 
		(set! y (add1 y))
		(when c0 ((begin0
			   c0
			   (set! c0 #f))))))
	     (cons x y)))]
	[ra-b-a-b
	 (lambda (get-a get-b)
	   (let ([l null])
	     (let ((k-in (test-call/cc (lambda (k1)
					 (dynamic-wind
					  (lambda () (set! l (append l (list (get-a)))))
					  (lambda ()
					    (call/cc (lambda (k2) (k1 k2))))
					  (lambda ()  (set! l (append l (list (get-b))))))))))
	       (k-in (lambda (v) l)))))])

  (test 4 rv (lambda () 4))
  (test '(5 6) r56)

  (test '(7 . 8) rx.y (lambda () 7) (lambda () 8))

  (test 9 rv2 (lambda () 9))

  (test '(10 11 12) r10-11-12)

  (test '(13 . 14) r13.14)

  ; !!! fixed in 50:
  (test '(enter exit enter exit)
	ra-b-a-b (lambda () 'enter) (lambda () 'exit))

  (test '((13 . 14) (10 11 12) (13 . 14) (10 11 12))
	ra-b-a-b r13.14 r10-11-12)
  (test '((10 11 12) (13 . 14) (10 11 12) (13 . 14))
	ra-b-a-b r10-11-12 r13.14)

  (test '((enter exit enter exit)
	  (exit enter exit enter)
	  (enter exit enter exit)
	  (exit enter exit enter))
	ra-b-a-b 
	(lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit)))
	(lambda () (ra-b-a-b (lambda () 'exit) (lambda () 'enter))))

  (test '(enter exit enter exit)
	rv (lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit))))
  (test '(enter exit enter exit)
	rv2 (lambda () (ra-b-a-b (lambda () 'enter) (lambda () 'exit))))

  (test '(10 11 12) rv r10-11-12)
  (test '(10 11 12) rv2 r10-11-12)

  (test '(13 . 14) rv r13.14)
  (test '(13 . 14) rv2 r13.14)

  (test 12 'dw/ec (test-call/cc
		   (lambda (k0)
		     (test-call/cc
		      (lambda (k1)
			(test-call/cc
			 (lambda (k2)
			   (dynamic-wind
			    void
			    (lambda () (k1 6))
			    (lambda () (k2 12))))))))))

  ;; !!! fixed in 53 (for call/ec)
  (test 13 'dw/ec (test-call/cc
		   (lambda (k0)
		     (test-call/cc
		      (lambda (k1)
			(test-call/cc
			 (lambda (k2)
			   (dynamic-wind
			    void
			    (lambda () (k1 6))
			    (lambda () (k2 12)))))
			(k0 13))))))

  ))
	      

(test-cc-values call/cc)
(test-cc-values call/ec)

(test 'ok
      'ec-cc-exn-combo
      (with-handlers ([void (lambda (x) 'ok)])
	(define f
	  (let ([k #f])
	    (lambda (n)
	      (case n
		[(0) (let/ec r (r (set! k (let/cc k k))))]
		[(1) (k)]))))
	(f 0)
	(f 1)))

(test '(1 2 3 4 1 2 3 4) 'dyn-wind-pre/post-order
      (let ([x null]
	    [go-back #f])
	(dynamic-wind
	 (lambda () (set! x (cons 4 x)))
	 (lambda () (dynamic-wind
		     (lambda () (set! x (cons 3 x)))
		     (lambda () (set! go-back (let/cc k k)))
		     (lambda () (set! x (cons 2 x)))))
	 (lambda () (set! x (cons 1 x))))
	(if (procedure? go-back)
	    (go-back 1)
	    x)))

(test '(5 . 5) 'suspended-cont-escape
      (let ([retry #f])
	(let ([v (let/ec exit
		   (dynamic-wind
		    void
		    (lambda () (exit 5))
		    (lambda ()
		      (let/ec inner-escape
			(set! retry (let/cc k k))
			(inner-escape 12)
			10))))])
	  (if (procedure? retry)
	      (retry 10)
	      (cons v v)))))

(test '(here) 'escape-interrupt-full-jump-up
      (let ([b #f]
	    [v null])
	(define (f g)
	  (dynamic-wind
	   void
	   g
	   (lambda () 
	     (set! v (cons 'here v))
	     (b 10))))
	
	(let/ec big
	  (set! b big)
	  (let/cc ok
	    (f (lambda ()
		 (ok #f)))))
	
	v))

;; Check interaction of map and call/cc
(let ()
  (define (try n m)
    (let ([retries (make-vector n)]
	  [copy #f]
	  [special -1]
	  [in (let loop ([i n])
		(if (= i 0)
		    null
		    (cons (- n i) (loop (sub1 i)))))])
      (let ([v (apply
		map 
		(lambda (a . rest)
		  (+ (let/cc k (vector-set! retries a k) 1)
		     a))
		(let loop ([m m])
		  (if (zero? m)
		      null
		      (cons in (loop (sub1 m))))))])
	(test (map (lambda (i)
		     (if (= i special)
			 (+ i 2)
			 (add1 i)))
		   in)
	      `(map/cc ,n ,m)
	      v))
      (if copy
	  (when (pair? copy)
	    (set! special (add1 special))
	    ((begin0 (car copy) (set! copy (cdr copy)))
	     2))
	  (begin
	    (set! copy (vector->list retries))
	    ((vector-ref retries (random n)) 1)))))
  (try 3 1)
  (try 10 1)
  (try 3 2)
  (try 10 2)
  (try 5 3)
  (try 3 5)
  (try 10 5))

(arity-test call/cc 1 2)
(arity-test call/ec 1 1)
(err/rt-test (call/cc 4))
(err/rt-test (call/cc (lambda () 0)))
(err/rt-test (call/ec 4))
(err/rt-test (call/ec (lambda () 0)))

(test #t primitive? car)
(test #f primitive? leaf-eq?)
(arity-test primitive? 1 1)

(test 1 procedure-arity procedure-arity)
(test 2 procedure-arity cons)
(test (make-arity-at-least 2) procedure-arity >)
(test (list 0 1) procedure-arity current-output-port)
(test (list 1 3 (make-arity-at-least 5))
      procedure-arity (case-lambda [(x) 0] [(x y z) 1] [(x y z w u . rest) 2]))
(test (make-arity-at-least 0) procedure-arity (lambda x 1))
(test (list 0 (make-arity-at-least 0)) procedure-arity (case-lambda 
							[() 10]
							[x 1]))
(test (make-arity-at-least 0) procedure-arity (lambda x x))
(arity-test procedure-arity 1 1)

(test #t procedure-arity-includes? cons 2)
(test #f procedure-arity-includes? cons 0)
(test #f procedure-arity-includes? cons 3)
(test #t procedure-arity-includes? list 3)
(test #t procedure-arity-includes? list 3000)
(test #t procedure-arity-includes? (lambda () 0) 0)
(test #f procedure-arity-includes? (lambda () 0) 1)
(test #f procedure-arity-includes? cons 10000000000000000000000000000)
(test #t procedure-arity-includes? list 10000000000000000000000000000)
(test #t procedure-arity-includes? (lambda x x) 10000000000000000000000000000)

(err/rt-test (procedure-arity-includes? cons -1))
(err/rt-test (procedure-arity-includes? cons 1.0))
(err/rt-test (procedure-arity-includes? 'cons 1))

(arity-test procedure-arity-includes? 2 2)

(newline)
(display ";testing scheme 4 functions; ")
(test '(#\P #\space #\l) string->list "P l")
(test '() string->list "")
(test "1\\\"" list->string '(#\1 #\\ #\"))
(test "" list->string '())
(arity-test list->string 1 1)
(arity-test string->list 1 1)
(err/rt-test (string->list 'hello))
(err/rt-test (list->string 'hello))
(err/rt-test (list->string '(#\h . #\e)))
(err/rt-test (list->string '(#\h 1 #\e)))
(test '(dah dah didah) vector->list '#(dah dah didah))
(test '() vector->list '#())
(test '#(dididit dah) list->vector '(dididit dah))
(test '#() list->vector '())
(arity-test list->vector 1 1)
(arity-test vector->list 1 1)
(err/rt-test (vector->list 'hello))
(err/rt-test (list->vector 'hello))
(err/rt-test (list->vector '(#\h . #\e)))

(test-cont)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables

(arity-test make-hash-table 0 2)
(err/rt-test (make-hash-table 'a))
(err/rt-test (make-hash-table 'weak 'weak) exn:application:mismatch?)
(err/rt-test (make-hash-table 'equal 'equal) exn:application:mismatch?)

(let ()
  (define-struct ax (b c)) ; opaque
  (define-struct a (b c) (make-inspector))

  (define save (let ([x null])
                 (case-lambda 
                  [() x]
                  [(a) (set! x (cons a x)) a])))
	 
  (define an-ax (make-ax 1 2))

  (let ([check-hash-tables
	 (lambda (maybe-weak reorder?)
	   (let ([h1 (apply make-hash-table maybe-weak)]
		 [l (list 1 2 3)])
	     (test #t eq? (eq-hash-code l) (eq-hash-code l))
	     (test #t eq? (equal-hash-code l) (equal-hash-code l))
	     (test #t eq? (equal-hash-code l) (equal-hash-code (list 1 2 3)))
	     (hash-table-put! h1 l 'ok)
	     (test 'ok hash-table-get h1 l)
	     (test 'nope hash-table-get h1 (list 1 2 3) (lambda () 'nope))
	     (test '(((1 2 3) . ok)) hash-table-map h1 (lambda (k v) (cons k v)))
	     (hash-table-remove! h1 l)
	     (test 'nope hash-table-get h1 l (lambda () 'nope)))
	   
	   (let ([h1 (apply make-hash-table 'equal maybe-weak)]
		 [l (list 1 2 3)]
		 [v (vector 5 6 7)]
		 [a (make-a 1 (make-a 2 3))]
		 [b (box (list 1 2 3))])

	     (test 0 hash-table-count h1)

	     ;; Fill in table. Use `puts1' and `puts2' so we can
	     ;; vary the order of additions.
	     (let ([puts1 (lambda ()
			    (hash-table-put! h1 (save l) 'list)
			    (hash-table-put! h1 (save "Hello World!") 'string)
			    (hash-table-put! h1 (save 123456789123456789123456789) 'bignum)
			    (hash-table-put! h1 (save 3.45) 'flonum)
			    (hash-table-put! h1 (save 3/45) 'rational)
			    (hash-table-put! h1 (save 3+45i) 'complex))]
		   [puts2 (lambda ()
			    (hash-table-put! h1 (save (list 5 7)) 'another-list)
			    (hash-table-put! h1 (save 3+0.0i) 'izi-complex)
			    (hash-table-put! h1 (save v) 'vector)
			    (hash-table-put! h1 (save a) 'struct)
			    (hash-table-put! h1 (save an-ax) 'structx)
			    (hash-table-put! h1 (save b) 'box))])
	       (if reorder?
		   (begin 
		     (puts2) 
		     (test 6 hash-table-count h1)
		     (puts1))
		   (begin 
		     (puts1) 
		     (test 6 hash-table-count h1)
		     (puts2))))

             (when reorder?
               ;; Add 1000 things and take them back out in an effort to 
               ;; trigger GCs that somehow affect hashing:
               (let loop ([i 0.0])
                 (unless (= i 1000.0)
                   (hash-table-put! h1 i #t)
                   (loop (add1 i))
                   (hash-table-remove! h1 i))))

	     (test 12 hash-table-count h1)
	     (test 'list hash-table-get h1 l)
	     (test 'list hash-table-get h1 (list 1 2 3))
	     (test 'another-list hash-table-get h1 (list 5 7))
	     (test 'string hash-table-get h1 "Hello World!")
	     (test 'bignum hash-table-get h1 123456789123456789123456789)
	     (test 'flonum hash-table-get h1 3.45)
	     (test 'rational hash-table-get h1 3/45)
	     (test 'complex hash-table-get h1 3+45i)
	     (test 'izi-complex hash-table-get h1 3+0.0i)
	     (test 'vector hash-table-get h1 v)
	     (test 'vector hash-table-get h1 #(5 6 7))
	     (test 'struct hash-table-get h1 a)
	     (test 'struct hash-table-get h1 (make-a 1 (make-a 2 3)))
	     (test 'structx hash-table-get h1 an-ax)
	     (test #f hash-table-get h1 (make-ax 1 2) (lambda () #f))
	     (test 'box hash-table-get h1 b)
	     (test 'box hash-table-get h1 #&(1 2 3))
	     (test #t
		   andmap
		   (lambda (i)
		     (and (member i 
				  (hash-table-map h1 (lambda (k v) (cons k v))))
			  #t))
		   `(((1 2 3) . list)
		     ((5 7) . another-list)
		     ("Hello World!" . string)
		     (123456789123456789123456789 . bignum)
		     (3.45 . flonum)
		     (3/45 . rational)
		     (3+45i . complex)
		     (3+0.0i . izi-complex)
		     (#(5 6 7) . vector)
		     (,(make-a 1 (make-a 2 3)) . struct)
		     (,an-ax . structx)
		     (#&(1 2 3) . box)))
	     (hash-table-remove! h1 (list 1 2 3))
	     (test 11 hash-table-count h1)
	     (test 'not-there hash-table-get h1 l (lambda () 'not-there))
	     (let ([c 0])
	       (hash-table-for-each h1 (lambda (k v) (set! c (add1 c))))
	       (test 11 'count c))
	     ;; return the hash table:
	     h1))])

    (let ([check-tables-equal
	   (lambda (mode t1 t2)
	     (test #t equal? t1 t2)
	     (test (equal-hash-code t1) equal-hash-code t2)
	     (let ([meta-ht (make-hash-table 'equal)])
	       (hash-table-put! meta-ht t1 mode)
	       (test mode hash-table-get meta-ht t2 (lambda () #f)))
             (test (hash-table-count t1) hash-table-count t2))])

      (check-tables-equal 'the-norm-table
                          (check-hash-tables null #f)
			  (check-hash-tables null #t))
      (check-tables-equal 'the-weak-table
                          (check-hash-tables (list 'weak) #f)
			  (check-hash-tables (list 'weak) #t)))

    (save))) ; prevents gcing of the ht-registered values

(test #f hash-table? 5)
(test #t hash-table? (make-hash-table))
(test #f hash-table? 5 'equal)
(test #f hash-table? (make-hash-table) 'equal)
(test #t hash-table? (make-hash-table 'equal) 'equal)
(test #f hash-table? (make-hash-table 'weak) 'equal)
(test #t hash-table? (make-hash-table 'weak 'equal) 'equal)
(test #f hash-table? 5 'weak)
(test #f hash-table? (make-hash-table) 'weak)
(test #f hash-table? (make-hash-table 'equal) 'weak)
(test #t hash-table? (make-hash-table 'weak) 'weak)
(test #t hash-table? (make-hash-table 'weak 'equal) 'weak)
(test #f hash-table? 5 'weak 'equal)
(test #f hash-table? (make-hash-table) 'weak 'equal)
(test #f hash-table? (make-hash-table 'equal) 'weak 'equal)
(test #f hash-table? (make-hash-table 'weak) 'weak 'equal)
(test #t hash-table? (make-hash-table 'weak 'equal) 'weak 'equal)

;; Double check that table are equal after deletions
(let ([test-del-eq
       (lambda (flags)
	 (let ([ht1 (apply make-hash-table flags)]
	       [ht2 (apply make-hash-table flags)])
	   (test #t equal? ht1 ht2)
	   (hash-table-put! ht1 'apple 1)
	   (hash-table-put! ht2 'apple 1)
	   (test #t equal? ht1 ht2)
	   (hash-table-put! ht2 'banana 2)
	   (test #f equal? ht1 ht2)
	   (hash-table-remove! ht2 'banana)
	   (test #t equal? ht1 ht2)))])
  (test-del-eq null)
  (test-del-eq '(equal))
  (test-del-eq '(weak)))

(err/rt-test (hash-table-count 0))
(err/rt-test (hash-table-put! 1 2 3))
(err/rt-test (hash-table-get 1 2))
(err/rt-test (hash-table-remove! 1 2))
(err/rt-test (hash-table-get (make-hash-table) 2) exn:application:mismatch?)
(err/rt-test (hash-table? 5 5))
(err/rt-test (hash-table? 5 'equal 5))
(err/rt-test (hash-table? (make-hash-table) 'equal 'equal) exn:application:mismatch?)
(err/rt-test (hash-table? (make-hash-table) 'weak 'weak) exn:application:mismatch?)

(define im-t (make-immutable-hash-table null))
(test #t hash-table? im-t)
(test #f hash-table? im-t 'equal)
(test null hash-table-map im-t cons)
(err/rt-test (hash-table-put! im-t 1 2))
(test #f hash-table-get im-t 5 (lambda () #f))
(define im-t (make-immutable-hash-table '((1 . 2))))
(test '((1 . 2)) hash-table-map im-t cons)
(test 2 hash-table-get im-t 1)
(define im-t (make-immutable-hash-table '(("hello" . 2))))
(test 'none hash-table-get im-t "hello" (lambda () 'none))
(define im-t (make-immutable-hash-table '(("hello" . 2)) 'equal))
(test 2 hash-table-get im-t "hello" (lambda () 'none))
(test #t hash-table? im-t 'equal)

(err/rt-test (hash-table-put! im-t 1 2))
(err/rt-test (hash-table-remove! im-t 1))
(err/rt-test (make-immutable-hash-table '(1)))
(err/rt-test (make-immutable-hash-table '((1 . 2) . 2)))
(err/rt-test (make-immutable-hash-table '((1 . 2) 3)))
(define cyclic-alist '#0=((1 . 2) . #0#))
(err/rt-test (make-immutable-hash-table cyclic-alist))
(err/rt-test (make-immutable-hash-table '((1 . 2)) 'weak))

(test 2 hash-table-get (hash-table-copy #hasheq((1 . 2))) 1)
(test (void) hash-table-put! (hash-table-copy #hasheq((1 . 2))) 3 4)

(arity-test make-hash-table 0 2)
(arity-test make-immutable-hash-table 1 2)
(arity-test hash-table-count 1 1)
(arity-test hash-table-get 2 3)
(arity-test hash-table-put! 3 3)
(arity-test hash-table-remove! 2 2)
(arity-test hash-table-map 2 2)
(arity-test hash-table-for-each 2 2)
(arity-test hash-table? 1 3)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(test #t string? (version))
(test #t string? (banner))
(test #t symbol? (system-type))
(test (system-type) system-type 'os)
(test #t string? (system-type 'machine))
(test #t symbol? (system-type 'link))
(test #t relative-path? (system-library-subpath))

(test #t 'cmdline (let ([v (current-command-line-arguments)])
		    (and (vector? v)
			 (andmap string? (vector->list v)))))
(err/rt-test (current-command-line-arguments '("a")))
(err/rt-test (current-command-line-arguments #("a" 1)))

(arity-test version 0 0)
(arity-test banner 0 0)
(arity-test system-type 0 1)
(arity-test system-library-subpath 0 1)
(arity-test current-command-line-arguments 0 1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; procedure-closure-contents-eq?

(for-each
 (lambda (jit?)
   (parameterize ([eval-jit-enabled jit?])
     (let ([f #f])
       (set! f (eval '(lambda (x) (lambda () x))))
       ((f 'c)) ; forced JIT compilation
       (test #t procedure-closure-contents-eq? (f 'a) (f 'a))
       (test #f procedure-closure-contents-eq? (f 'a) (f 'b))
       (set! f (eval '(case-lambda
		       [(x) (lambda () 12)]
		       [(x y) (lambda () (list x y))])))
       ((f 'c)) ; forces JIT compilation
       ((f 'c 'd)) ; forces JIT compilation
       (test #t procedure-closure-contents-eq? (f 'a) (f 'a))
       (test #t procedure-closure-contents-eq? (f 'a 'b) (f 'a 'b))
       (test #f procedure-closure-contents-eq? (f 'a 'b) (f 'c 'b)))))
 '(#t #f))
(test #t procedure-closure-contents-eq? add1 add1)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)

"last item in file"
