;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/

;; Translated directly from the C# version, which was:
;;   contributed by Isaac Gouy

#lang scheme/base
(require scheme/cmdline
         scheme/unsafe/ops)

(define (Approximate n)
  (let ([u (make-vector n 1.0)]
        [v (make-vector n 0.0)])
    ;; 20 steps of the power method
    (for ([i (in-range 10)])
      (MultiplyAtAv n u v)
      (MultiplyAtAv n v u))
    
    ;; B=AtA         A multiplied by A transposed
    ;; v.Bv /(v.v)   eigenvalue of v
    (let loop ([i 0][vBv 0.0][vv 0.0])
      (if (= i n)
          (sqrt (unsafe-fl/ vBv vv))
          (let ([vi (unsafe-vector-ref v i)])
            (loop (unsafe-fx+ 1 i)
                  (unsafe-fl+ vBv (unsafe-fl* (unsafe-vector-ref u i) vi))
                  (unsafe-fl+ vv (unsafe-fl* vi vi))))))))

;; return element i,j of infinite matrix A
(define (A i j)
  (let ([n (unsafe-fx+ i (unsafe-fx+ j 1))]
        [i+j (unsafe-fx+ i j)]
        [i+1 (unsafe-fx+ i 1)])
    (unsafe-fl/ 1.0 
                (unsafe-fl+ 
                 (unsafe-fl* (unsafe-fx->fl i+j)
                             (unsafe-fl/ (unsafe-fx->fl n)
                                         2.0))
                 (unsafe-fx->fl i+1)))))

;; multiply vector v by matrix A
(define (MultiplyAv n v Av)
  (for ([i (in-range n)])
    (unsafe-vector-set! Av i 
                        (for/fold ([r 0.0])
                            ([j (in-range n)])
                          (unsafe-fl+ r (unsafe-fl* (A i j) (unsafe-vector-ref v j)))))))

;; multiply vector v by matrix A transposed
(define (MultiplyAtv n v Atv)
  (for ([i (in-range n)])
    (vector-set! Atv i
                 (for/fold ([r 0.0])
                     ([j (in-range n)])
                   (unsafe-fl+ r (unsafe-fl* (A j i) (unsafe-vector-ref v j)))))))

;; multiply vector v by matrix A and then by matrix A transposed 
(define (MultiplyAtAv n v AtAv)
  (let ([u (make-vector n 0.0)])
    (MultiplyAv n v u)
    (MultiplyAtv n u AtAv)))

(printf "~a\n"
        (real->decimal-string
         (Approximate (command-line #:args (n) (string->number n)))
         9))

