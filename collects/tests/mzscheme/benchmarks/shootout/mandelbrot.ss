;; ---------------------------------------------------------------------
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla

;; Note: as of version 350, this benchmark spends much of
;;  its time GCing; it runs 2 times as fast in mzscheme3m.

;; The version that uses complex number is a little
;; more elegant, but slower:
;;  (define (mandelbrot iterations x y n ci)
;;    (let ((c (+ (- (/ (* 2.0 x) n) 1.5) 
;;                (* ci 0.0+1.0i))))
;;      (let loop ((i 0) (z 0.0+0.0i))
;;	(cond
;;	 [(> i iterations) 1]
;;	 [(> (magnitude z) 2.0) 0]
;;	 [else (loop (add1 i) (+ (* z z) c))]))))
 

(module mandelbrot mzscheme

  ;; -------------------------------
  
  (define +limit-sqr+ 4.0)

  (define +iterations+ 50)

  ;; -------------------------------
  
  (define (mandelbrot iterations x y n ci)
    (let ((cr (- (/ (* 2.0 x) n) 1.5)))
      (let loop ((i 0) (zr 0.0) (zi 0.0))
	(if (> i iterations)
	    1
	    (let ((zrq (* zr zr)) 
		  (ziq (* zi zi)))
	      (cond
	       ((> (+ zrq ziq) +limit-sqr+) 0)
	       (else (loop (add1 i) (+ (- zrq ziq) cr) (+ (* 2.0 (* zr zi)) ci)))))))))
  
  ;; -------------------------------
  
  (define (main args)
    (let ((n (string->number (vector-ref args 0)))
	  (out (current-output-port)))

      (fprintf out "P4\n~a ~a\n" n n)

      (let loop-y ((y 0))

	(when (< y n)
	   
	  (let ([ci (- (/ (* 2.0 y) n) 1.0)])
	    
	    (let loop-x ((x 0) (bitnum 0) (byteacc 0))

	      (if (< x n)
		  (let ([bitnum (add1 bitnum)]
			[byteacc (+ (arithmetic-shift byteacc 1) 
				    (mandelbrot +iterations+ x y n ci))])

		    (cond
		     ((= bitnum 8)
		      (write-byte byteacc out)
		      (loop-x (add1 x) 0 0))
		     
		     [else (loop-x (add1 x) bitnum byteacc)]))

		  (begin
		    (when (positive? bitnum)
		      (write-byte (arithmetic-shift byteacc (- 8 (bitwise-and n #x7))) out))

		    (loop-y (add1 y))))))))))

  ;; -------------------------------
  
  (main (current-command-line-arguments)))
