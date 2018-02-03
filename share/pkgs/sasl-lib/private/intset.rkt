#lang racket/base
(require (for-syntax racket/base syntax/parse racket/match))
(provide intset
         char-predicate
         int-in-set?
         char-in-set?)

(define-syntax (intset stx)
  (define-syntax-class range
    (pattern [lo:nat hi:nat])
    (pattern lo:nat #:with hi #'lo))
  (syntax-parse stx
    [(_ r:range ...)
     (define ranges0 (syntax->datum #'((r.lo r.hi) ...)))
     (define (compress ranges)
       (match ranges
         [(list* (list lo1 hi1) (list lo2 hi2) more)
          (cond [(= (add1 hi1) lo2)
                 (compress (list* (list lo1 hi2) more))]
                [else
                 (cons (car ranges) (compress (cdr ranges)))])]
         [else ranges]))
     #`(quote #,(list->vector (apply append (compress ranges0))))]))

(define ((char-predicate . sets) c)
  (for/or ([s (in-list sets)]) (char-in-set? c s)))

(define (char-in-set? c is) (int-in-set? (char->integer c) is))

;; An IntSet is (Vector {lo hi} ...)
;; Intepretation: (lambda (x) (or (<= lo x hi) ...))

(define (int-in-set? seek is)
  ;; (eprintf "seeking ~s\n" seek)
  (define (value k) (vector-ref is k))
  (define (loop lo hi)
    ;; (eprintf "  loop ~s, ~s\n" lo hi)
    (and (< lo hi) (loop* lo hi)))
  (define (loop* lo hi)
    ;; INV: (<= (value lo) seek (value hi))
    ;; INV: (even? lo) and (odd? hi)
    (define midlo (* 2 (quotient (+ lo hi 1) 4)))
    (define midhi (add1 midlo))
    (cond [(< seek (value midlo))
           (loop lo (sub1 midlo))]
          [(< (value midhi) seek)
           (loop (add1 midhi) hi)]
          ;; (value midlo) <= seek <= (value midhi)
          [else #t]))
  (let ([last (sub1 (vector-length is))])
    (cond [(<= (value 0) seek (value last))
           (loop 0 last)]
          [else #f])))
