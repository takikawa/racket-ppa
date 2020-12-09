#lang racket/base
(require (for-syntax racket/base syntax/transformer)
         racket/match
         syntax/stx
         "stx-util.rkt")
(provide sexpr-path?
         empty-path
         path-add-ref
         path-add-car
         path-add-cdr
         path-add-cdrs
         path-get
         path-get-until
         path-replace
         path-replacer
         path-append
         path-reverse
         path-cut-prefix
         path-prefix?)

(define sexpr-path? list?) ;; note: does not check well-formed

;; A Path is a (list-of PathSeg), where the PathSegs are listed outermost to innermost.
;; For example: (path-get #'((a b) (c d)) '(car 1)) = #'(b), not #'(c d).

;; Paths have non-canonical forms: for example '(1 2 car) = '(3 car).

;; A PathSeg is one of
;; - 'car            -- represents car
;; - PositiveInteger -- represents nth tail (n cdrs, n>0)
(define (path-add-ref n p) (path-add-tail n (cons 'car p)))
(define (path-add-tail n p)
  (if (zero? n)
      p
      (match p
        [(cons (? exact-positive-integer? t) p*) (cons (+ n t) p*)]
        [_ (cons n p)])))
(define (-path-add-car p) (cons 'car p))
(define (path-add-cdr p) (path-add-tail 1 p))
(define (-path-add-cdrs n p) (path-add-tail n p))
(define (-empty-path) null)

(define-match-expander empty-path
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'(quote ())]))
  (make-variable-like-transformer #'-empty-path))

(define-match-expander path-add-car
  (lambda (stx)
    (syntax-case stx ()
      [(_ p) #'(cons 'car p)]))
  (make-variable-like-transformer #'-path-add-car))

(define-match-expander path-add-cdrs
  (lambda (stx)
    (syntax-case stx ()
      [(_ n p) #'(cons (? exact-positive-integer? n) p)]))
  (make-variable-like-transformer #'-path-add-cdrs))

;; ----

;; path-get : Stx Path -> Stx
(define (path-get stx path)
  (define (bad) (error 'path-get "out of bounds: ~s, ~e" path stx))
  (let loop ([stx stx] [path path])
    (match path
      [(empty-path) stx]
      [(path-add-car path)
       (if (stx-pair? stx) (loop (stxd-car stx) path) (bad))]
      [(path-add-cdrs (? exact-positive-integer? n) path)
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
      [(empty-path) (values stx null)]
      [(path-add-car path)
       (if (stx-pair? stx) (loop (stxd-car stx) path) (bad))]
      [(path-add-cdrs (? exact-positive-integer? n) path)
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
      [(empty-path) x]
      [(path-add-car path)
       (unless (stx-pair? stx) (bad))
       (stx-replcar stx (loop (stxd-car stx) path) resyntax?)]
      [(path-add-cdrs (? exact-positive-integer? n) path)
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
      [[(empty-path) p2] p2]
      [[(path-add-car p1b) (path-add-car p2b)]
       (loop p1b p2b)]
      [[(path-add-cdrs n1 p1b) (path-add-cdrs n2 p2b)]
       (cond [(= n1 n2) (loop p1b p2b)]
             [(< n1 n2) (loop p1b (path-add-cdrs (- n2 n1) p2b))]
             [(> n1 n2) (loop (path-add-cdrs (- n1 n2) p1b) p2b)])]
      [[_ _] #f])))

;; path-prefix? : Path Path -> Boolean
(define (path-prefix? a b) (and (path-cut-prefix a b) #t))

;; path-append : Path Path -> Path
(define (path-append a b) (append a b))

;; path-reverse : Path -> Path
(define (path-reverse p) (reverse p))
