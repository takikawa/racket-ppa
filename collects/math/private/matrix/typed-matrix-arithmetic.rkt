#lang typed/racket/base

(require racket/list
         "matrix-types.rkt"
         "utils.rkt"
         (except-in "untyped-matrix-arithmetic.rkt" matrix-map)
         "../array/array-struct.rkt"
         "../array/array-fold.rkt"
         "../array/utils.rkt")

(provide matrix-map
         matrix=
         matrix*
         matrix+
         matrix-
         matrix-scale
         matrix-sum)

(: matrix-map
   (All (R A B T ...)
        (case-> ((A -> R) (Matrix A) -> (Matrix R))
                ((A B T ... T -> R) (Matrix A) (Matrix B) (Matrix T) ... T -> (Matrix R)))))
(define matrix-map
  (case-lambda:
    [([f : (A -> R)] [arr : (Matrix A)])
     (inline-matrix-map f arr)]
    [([f : (A B -> R)] [arr0 : (Matrix A)] [arr1 : (Matrix B)])
     (inline-matrix-map f arr0 arr1)]
    [([f : (A B T ... T -> R)] [arr0 : (Matrix A)] [arr1 : (Matrix B)] . [arrs : (Matrix T) ... T])
     (define-values (m n) (apply matrix-shapes 'matrix-map arr0 arr1 arrs))
     (define g0 (unsafe-array-proc arr0))
     (define g1 (unsafe-array-proc arr1))
     (define gs (map unsafe-array-proc arrs))
     (unsafe-build-array
      ((inst vector Index) m n)
      (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                  (map (λ: ([g : (Indexes -> T)]) (g js)) gs))))]))

(: matrix=? ((Matrix Number) (Matrix Number) -> Boolean))
(define (matrix=? arr0 arr1)
  (define-values (m0 n0) (matrix-shape arr0))
  (define-values (m1 n1) (matrix-shape arr1))
  (and (= m0 m1)
       (= n0 n1)
       (let ([proc0  (unsafe-array-proc arr0)]
             [proc1  (unsafe-array-proc arr1)])
         (array-all-and (unsafe-build-array
                         ((inst vector Index) m0 n0)
                         (λ: ([js : Indexes])
                           (= (proc0 js) (proc1 js))))))))

(: matrix= (case-> ((Matrix Number) (Matrix Number) -> Boolean)
                   ((Matrix Number) (Matrix Number) (Matrix Number) (Matrix Number) * -> Boolean)))
(define matrix=
  (case-lambda:
    [([arr0 : (Matrix Number)] [arr1 : (Matrix Number)])  (matrix=? arr0 arr1)]
    [([arr0 : (Matrix Number)] [arr1 : (Matrix Number)] . [arrs : (Matrix Number) *])
     (and (matrix=? arr0 arr1)
          (let: loop : Boolean ([arr1 : (Matrix Number)  arr1]
                                [arrs : (Listof (Matrix Number))  arrs])
            (cond [(empty? arrs)  #t]
                  [else  (and (matrix=? arr1 (first arrs))
                              (loop (first arrs) (rest arrs)))])))]))

(: matrix* (case-> ((Matrix Real) (Matrix Real) * -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) * -> (Matrix Number))))
(define (matrix* a . as)
  (let loop ([a a] [as as])
    (cond [(empty? as)  a]
          [else  (loop (inline-matrix* a (first as)) (rest as))])))

(: matrix+ (case-> ((Matrix Real) (Matrix Real) * -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) * -> (Matrix Number))))
(define (matrix+ a . as)
  (let loop ([a a] [as as])
    (cond [(empty? as)  a]
          [else  (loop (inline-matrix+ a (first as)) (rest as))])))

(: matrix- (case-> ((Matrix Real) (Matrix Real) * -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) * -> (Matrix Number))))
(define (matrix- a . as)
  (cond [(empty? as)  (inline-matrix- a)]
        [else
         (let loop ([a a] [as as])
           (cond [(empty? as)  a]
                 [else  (loop (inline-matrix- a (first as)) (rest as))]))]))

(: matrix-scale (case-> ((Matrix Real) Real -> (Matrix Real))
                        ((Matrix Number) Number -> (Matrix Number))))
(define (matrix-scale a x) (inline-matrix-scale a x))

(: matrix-sum (case-> ((Listof (Matrix Real)) -> (Matrix Real))
                      ((Listof (Matrix Number)) -> (Matrix Number))))
(define (matrix-sum lst)
  (cond [(empty? lst)  (raise-argument-error 'matrix-sum "nonempty List" lst)]
        [else  (apply matrix+ lst)]))
