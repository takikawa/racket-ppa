#lang racket/base
(require racket/match
         syntax/stx
         "stx-util.rkt")
(provide path-add-ref
         path-add-tail
         path-add-car
         path-add-cdr
         path-get
         path-get-until
         path-replace
         path-replacer
         path-cut-prefix
         path-prefix?)

;; A Path is a (list-of PathSeg), where the PathSegs are listed outermost to innermost.
;; For example: (path-get #'((a b) (c d)) '(car 1)) = #'(b), not #'(c d).

;; Paths have non-canonical forms: for example '(1 2 car) = '(3 car).

;; A PathSeg is one of
;; - 'car            -- represents nth car
;; - PositiveInteger -- -n represents nth tail (n cdrs, n>0)
(define (path-add-ref n p) (path-add-tail n (cons 'car p)))
(define (path-add-tail n p)
  (if (zero? n)
      p
      (match p
        [(cons (? exact-positive-integer? t) p*) (cons (+ n t) p*)]
        [_ (cons n p)])))
(define (path-add-car p) (cons 'car p))
(define (path-add-cdr p) (path-add-tail 1 p))

;; path-get : Stx Path -> Stx
(define (path-get stx path)
  (define (bad) (error 'path-get "out of bounds: ~s, ~e" path stx))
  (let loop ([stx stx] [path path])
    (match path
      ['() stx]
      [(cons 'car path)
       (if (stx-pair? stx) (loop (stxd-car stx) path) (bad))]
      [(cons (? exact-positive-integer? n) path)
       (loop (for/fold ([stx stx]) ([_i (in-range n)])
               (if (stx-pair? stx) (stxd-cdr stx) (bad)))
             path)])))

;; path-get-until : Stx Path (Stx -> Boolean) -> (values Stx Path)
(define (path-get-until stx path stop?)
  (define (bad) (error 'path-get-while "out of bounds: ~s, ~e" path stx))
  (define (loop stx path)
    (if (stop? stx) (values stx path) (okloop stx path)))
  (define (okloop stx path)
    (match path
      ['() (values stx null)]
      [(cons 'car path)
       (if (stx-pair? stx) (loop (stxd-car stx) path) (bad))]
      [(cons (? exact-positive-integer? n) path)
       (let tailloop ([stx stx] [n n])
         (cond [(zero? n) (loop stx path)]
               [(stop? stx) (values stx (path-add-tail n path))]
               [(stx-pair? stx) (tailloop (stxd-cdr stx) (sub1 n))]
               [else (bad)]))]))
  (loop stx path))

;; path-replace : Stx Path Stx [Boolean] -> Stx
(define (path-replace stx path x #:resyntax? resyntax?)
  (define (bad) (error 'path-replace "out of bounds: ~s, ~e" path stx))
  (let loop ([stx stx] [path path])
    (match path
      ['() x]
      [(cons 'car path)
       (unless (stx-pair? stx) (bad))
       (stx-replcar stx (loop (stxd-car stx) path) resyntax?)]
      [(cons (? exact-positive-integer? n) path)
       (let tailloop ([stx stx] [n n])
         (cond [(zero? n) (loop stx path)]
               [(not (stx-pair? stx)) (bad)]
               [else (stx-replcdr stx (tailloop (stxd-cdr stx) (sub1 n)) resyntax?)]))])))

;; stx-replcar : Stx Stx -> Stx
(define (stx-replcar stx x resyntax?)
  (cond [(pair? stx)
         (cons x (cdr stx))]
        [(and (syntax? stx) (pair? (syntax-e stx)))
         (let ([dstx (stx-disarm stx)])
           (let ([result (cons x (cdr (syntax-e dstx)))])
             (if resyntax? (resyntax result stx dstx) result)))]
        [else (raise-type-error 'stx-replcar "stx-pair" stx)]))

;; stx-replcdr : Stx Stx -> Stx
(define (stx-replcdr stx x resyntax?)
  (cond [(pair? stx)
         (cons (car stx) x)]
        [(and (syntax? stx) (pair? (syntax-e stx)))
         (let ([dstx (stx-disarm stx)])
           (let ([result (cons (car (syntax-e dstx)) x)])
             (if resyntax? (resyntax result stx dstx) result)))]
        [else (raise-type-error 'stx-replcdr "stx-pair" stx)]))

(define ((path-replacer stx path #:resyntax? resyntax?) s)
  (path-replace stx path s #:resyntax? resyntax?))

;; path-cut-prefix : Path Path -> Path/#f
;; Removes a as a prefix of b (or returns #f if a is not a prefix of b).
(define (path-cut-prefix p1 p2)
  (let loop ([p1 p1] [p2 p2])
    (match* [p1 p2]
      [['() p2] p2]
      [[(cons 'car p1b) (cons 'car p2b)]
       (loop p1b p2b)]
      [[(cons (? exact-positive-integer? n1) p1b) (cons (? exact-positive-integer? n2) p2b)]
       (cond [(= n1 n2) (loop p1b p2b)]
             [(< n1 n2) (loop p1b (cons (- n2 n1) p2b))]
             [(> n1 n2) (loop (cons (- n1 n2) p1b) p2b)])]
      [[_ _] #f])))

;; path-prefix? : Path Path -> Boolean
(define (path-prefix? a b) (and (path-cut-prefix a b) #t))
