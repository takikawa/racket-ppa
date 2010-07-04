
(load-relative "loadtest.ss")

(Section 'unsafe)

(require '#%unsafe)

(let ()
  (define (test-tri result proc x y z 
                    #:pre [pre void] 
                    #:post [post (lambda (x) x)] 
                    #:literal-ok? [lit-ok? #t])
    (pre)
    (test result (compose post (eval proc)) x y z)
    (pre)
    (test result (compose post (eval `(lambda (x y z) (,proc x y z)))) x y z)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y z) (,proc ,x y z)))) y z))
    (pre)
    (test result (compose post (eval `(lambda (x z) (,proc x ,y z)))) x z)
    (pre)
    (test result (compose post (eval `(lambda (x y) (,proc x y ,z)))) x y)
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x ,y ,z)))) x)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y) (,proc ,x y ,z)))) y)
      (pre)
      (test result (compose post (eval `(lambda (z) (,proc ,x ,y z)))) z)))
  (define (test-bin result proc x y 
                    #:pre [pre void] 
                    #:post [post (lambda (x) x)]
                    #:literal-ok? [lit-ok? #t])
    (pre)
    (test result (compose post (eval proc)) x y)
    (pre)
    (test result (compose post (eval `(lambda (x y) (,proc x y)))) x y)
    (when lit-ok?
      (pre)
      (test result (compose post (eval `(lambda (y) (,proc ,x y)))) y))
    (pre)
    (test result (compose post (eval `(lambda (x) (,proc x ,y)))) x))
  (define (test-un result proc x)
    (test result (eval proc) x)
    (test result (eval `(lambda (x) (,proc x))) x))

  (test-bin 3 'unsafe-fx+ 1 2)
  (test-bin -1 'unsafe-fx+ 1 -2)

  (test-bin 8 'unsafe-fx- 10 2)
  (test-bin 3 'unsafe-fx- 1 -2)

  (test-bin 20 'unsafe-fx* 10 2)
  (test-bin -20 'unsafe-fx* 10 -2)
  
  (test-bin 3 'unsafe-fxquotient 17 5)
  (test-bin -3 'unsafe-fxquotient 17 -5)

  (test-bin 2 'unsafe-fxremainder 17 5)
  (test-bin 2 'unsafe-fxremainder 17 -5)

  (test-bin 3.4 'unsafe-fl+ 1.4 2.0)
  (test-bin -1.1 'unsafe-fl+ 1.0 -2.1)
  (test-bin +inf.0 'unsafe-fl+ 1.0 +inf.0)
  (test-bin -inf.0 'unsafe-fl+ 1.0 -inf.0)
  (test-bin +nan.0 'unsafe-fl+ +nan.0 -inf.0)

  (test-bin #f unsafe-fx= 1 2)
  (test-bin #t unsafe-fx= 2 2)
  (test-bin #f unsafe-fx= 2 1)

  (test-bin #t unsafe-fx< 1 2)
  (test-bin #f unsafe-fx< 2 2)
  (test-bin #f unsafe-fx< 2 1)

  (test-bin #f unsafe-fx> 1 2)
  (test-bin #f unsafe-fx> 2 2)
  (test-bin #t unsafe-fx> 2 1)

  (test-bin #t unsafe-fx<= 1 2)
  (test-bin #t unsafe-fx<= 2 2)
  (test-bin #f unsafe-fx<= 2 1)

  (test-bin #f unsafe-fx>= 1 2)
  (test-bin #t unsafe-fx>= 2 2)
  (test-bin #t unsafe-fx>= 2 1)

  (test-bin 7.9 'unsafe-fl- 10.0 2.1)
  (test-bin 3.7 'unsafe-fl- 1.0 -2.7)

  (test-bin 20.02 'unsafe-fl* 10.01 2.0)
  (test-bin -20.02 'unsafe-fl* 10.01 -2.0)
  
  (test-bin (exact->inexact 17/5) 'unsafe-fl/ 17.0 5.0)
  (test-bin +inf.0 'unsafe-fl/ 17.0 0.0)
  (test-bin -inf.0 'unsafe-fl/ -17.0 0.0)

  (test-bin 3 'unsafe-fxand 7 3)
  (test-bin 2 'unsafe-fxand 6 3)
  (test-bin 3 'unsafe-fxand -1 3)

  (test-bin 7 'unsafe-fxior 7 3)
  (test-bin 7 'unsafe-fxior 6 3)
  (test-bin -1 'unsafe-fxior -1 3)

  (test-bin 4 'unsafe-fxxor 7 3)
  (test-bin 5 'unsafe-fxxor 6 3)
  (test-bin -4 'unsafe-fxxor -1 3)

  (test-un -1 'unsafe-fxnot 0)
  (test-un -4 'unsafe-fxnot 3)

  (test-bin 32 'unsafe-fxlshift 2 4)
  (test-bin 32 'unsafe-fxlshift 8 2)
  (test-bin 8 'unsafe-fxlshift 8 0)

  (test-bin 2 'unsafe-fxrshift 32 4)
  (test-bin 8 'unsafe-fxrshift 32 2)
  (test-bin 8 'unsafe-fxrshift 8 0)

  (test-un 5 'unsafe-car (cons 5 9))
  (test-un 9 'unsafe-cdr (cons 5 9))
  (test-un 15 'unsafe-mcar (mcons 15 19))
  (test-un 19 'unsafe-mcdr (mcons 15 19))
  (let ([v (mcons 3 7)])
    (test-bin 8 'unsafe-set-mcar! v 8 
              #:pre (lambda () (set-mcar! v 0)) 
              #:post (lambda (x) (mcar v))
              #:literal-ok? #f)
    (test-bin 9 'unsafe-set-mcdr! v 9 
              #:pre (lambda () (set-mcdr! v 0)) 
              #:post (lambda (x) (mcdr v))
              #:literal-ok? #f))

  (test-bin 5 'unsafe-vector-ref #(1 5 7) 1)
  (test-un 3 'unsafe-vector-length #(1 5 7))
  (let ([v (vector 0 3 7)])
    (test-tri 5 'unsafe-vector-set! v 2 5 
              #:pre (lambda () (vector-set! v 2 0)) 
              #:post (lambda (x) (vector-ref v 2))
              #:literal-ok? #f))

  (let ()
    (define-struct posn (x [y #:mutable] z))
    (test-bin 'a unsafe-struct-ref (make-posn 'a 'b 'c) 0 #:literal-ok? #f)
    (test-bin 'b unsafe-struct-ref (make-posn 'a 'b 'c) 1 #:literal-ok? #f)
    (let ([p (make-posn 100 200 300)])
      (test-tri 500 'unsafe-struct-set! p 1 500
                #:pre (lambda () (set-posn-y! p 0)) 
                #:post (lambda (x) (posn-y p))
                #:literal-ok? #f)))

  (void))

(report-errs)
