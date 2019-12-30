#lang racket

(require racket/contract)

(provide (contract-out
          [shuffle-list
           (-> list? natural-number/c list?)]
          ))

(define (shuffle-list l c)
  (if (zero? c)
      l
      (let-values ([(a b)
                    (let ([half (floor (/ (length l) 2))])
                      (values
                       (let loop ([l l][n half])
                         (if (zero? n)
                             null
                             (cons (car l) (loop (cdr l) (sub1 n)))))
                       (list-tail l half)))])
        (shuffle-list
         (let loop ([a a][b b][l null])
           (cond
             [(null? a) (append (reverse b) l)]
             [(null? b) (append (reverse a) l)]
             [(zero? (random 2))
              (loop (cdr a) b (cons (car a) l))]
             [else
              (loop a (cdr b) (cons (car b) l))]))
         (sub1 c)))))
