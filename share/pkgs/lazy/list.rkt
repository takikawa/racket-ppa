#lang lazy/base

(require
  (prefix-in ! racket/list)
  (except-in
   racket/list
   first second third fourth fifth sixth seventh eighth
   rest last-pair
   make-list
   take drop split-at takef dropf splitf-at
   take-right drop-right split-at-right takef-right dropf-right splitf-at-right
   add-between
   append*
   flatten
   remove-duplicates
   filter-map count partition
   range
   append-map
   filter-not
   argmin argmax))

(provide (all-from-out racket/list)
         last-pair
         take drop split-at takef dropf splitf-at
         take-right drop-right split-at-right takef-right dropf-right splitf-at-right
         add-between
         append*
         flatten
         remove-duplicates
         filter-map count partition
         range
         append-map
         filter-not
         argmin argmax)

;; lazy versions of exports from racket/list
;; ---------------------------------------------------------------------------------------------------

(define (last-pair p)
  (let ([p (! p)])
    (unless (pair? p)
      (raise-argument-error 'last-pair "pair?" p))
    (let loop ([p p])
      (define next (! (cdr p)))
      (if (pair? next) (loop next) p))))

(define (make-list n v)
  (let ([n (! n)])
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error 'make-list "exact-nonnegative-integer?" 0 n v))
    (let loop ([n n]
               [acc '()])
      (if (zero? n) acc
          (loop (sub1 n) (cons v acc))))))

(define (take l n)
  (let ([n (! n)])
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error 'take "exact-nonnegative-integer?" 1 l n))
    (let loop ([n n] [l l])
      (if (zero? n)
          '()
          (let ([l (! l)])
            (cond [(null? l)
                   ;; it would be fine to force the whole list (since we now
                   ;; know it's finite), but doing so means keeping a reference
                   ;; to its head, which can lead to memory leaks.
                   (raise-arguments-error 'take
                                          "index is too large for input list"
                                          "index" n)]
                  [(pair? l) (cons (car l) (loop (sub1 n) (! (cdr l))))]
                  [else (raise-argument-error 'take "list?" l)]))))))

(define (split-at l n)
  (let ([n (! n)])
    (unless (exact-nonnegative-integer? n)
      (raise-argument-error 'split-at "exact-nonnegative-integer?" 1 l n))
    (let loop ([n n] [l l])
      (if (zero? n)
          (values '() l)
          (let ([l (! l)])
            (cond [(null? l)
                   ;; see comment for `take`
                   (raise-arguments-error 'split-at
                                          "index is too large for input list"
                                          "index" n)]
                  [(pair? l)
                   (define-values (a b) (loop (sub1 n) (! (cdr l))))
                   (values (cons (car l) a) b)]
                  [else (raise-argument-error 'split-at "list?" l)]))))))

(define (drop lst pos)
  (list-tail lst pos))

(define (takef lst pred)
  (let ([pred (! pred)])
    (unless (procedure? pred)
      (raise-argument-error 'takef "procedure?" 1 lst pred))
    (let loop ([lst (! lst)])
      (cond
        [(and (pair? lst) (! (pred (car lst))))
         (cons (car lst) (loop (cdr lst)))]
        [else '()]))))

(define (dropf lst pred)
  (let ([pred (! pred)])
    (unless (procedure? pred)
      (raise-argument-error 'takef "procedure?" 1 lst pred))
    (let loop ([lst (! lst)])
      (cond
        [(and (pair? lst) (! (pred (car lst))))
         (loop (cdr lst))]
        [else lst]))))

(define (splitf-at lst pred)
  (let ([pred (! pred)])
    (unless (procedure? pred)
      (raise-argument-error 'takef "procedure?" 1 lst pred))
    (let loop ([lst (! lst)])
      (cond
        [(and (pair? lst) (! (pred (car lst))))
         (define-values (a b) (loop (cdr lst)))
         (values (cons (car lst) a) b)]
        [else (values '() lst)]))))

(define (take-right l n)
  (drop l (- (improper-length l) n)))

(define (drop-right l n)
  (take l (- (improper-length l) n)))

(define (split-at-right l n)
  (split-at l (- (improper-length l) n)))

(define (takef-right l pred)
  (improper-reverse (takef (improper-reverse l) pred)))

(define (dropf-right l pred)
  (improper-reverse (dropf (improper-reverse l) pred)))

(define (splitf-at-right l pred)
  (improper-reverse (splitf-at (improper-reverse l) pred)))

;; keyword arguments currently do not work to due Lazy Racket limitations
(define (add-between lst v
                     #:before-first [before-first '()]
                     #:before-last [before-last v]
                     #:after-last [after-last '()]
                     #:splice? [splice? #f])
  (define middle
    (let ([lst (!list lst)])
      (cons (car lst)
            (let loop ([lst (cdr lst)])
              (cond
                [(null? lst) '()]
                [else ((if splice? append list*)
                       (if (null? (cdr lst)) before-last v)
                       (if splice? (list (car lst)) (car lst))
                       (loop (cdr lst)))])))))
  (if splice?
      (append before-first middle after-last)
      middle))

(define (append* . args)
  (define-values (head tail) (split-at-right args 1))
  (apply append (append head (apply append tail))))

(define (flatten v)
  (let ([v (! v)])
    (cond
      [(pair? v) (append (flatten (car v)) (flatten (cdr v)))]
      [(null? v) '()]
      [else (list v)])))

(define (remove-duplicates lst [same? equal?] #:key [extract-key (λ (x) x)])
  (let loop ([lst (!list lst)])
    (if (null? lst) '()
        (cons (car lst) (loop (remove* (list (car lst)) lst
                                       (λ (a b) (same? (extract-key a) (extract-key b)))))))))

(define (filter-map proc lst . lsts)
  (let loop ([lsts (cons lst lsts)])
    (cond
      [(null? (! (car lsts))) '()]
      [else
       (define result (apply proc (map car lsts)))
       (if result
           (cons result (loop (map cdr lsts)))
           (loop (map cdr lsts)))])))

(define (count proc lst . lsts)
  (let loop ([lsts (cons lst lsts)]
             [acc 0])
    (cond
      [(null? (! (car lsts))) acc]
      [else
       (define result (apply proc (map car lsts)))
       (loop (map cdr lsts) (if result (add1 acc) acc))])))

(define (partition pred lst)
  (let loop ([lst (!list lst)])
    (cond
      [(null? lst) (values '() '())]
      [else
       (define-values (a b) (loop (cdr lst)))
       (if (pred (car lst))
           (values (cons (car lst) a) b)
           (values a (cons (car lst) b)))])))

(define range
  (case-lambda
    [(end) (range 0 end)]
    [(start end) (range start end 1)]
    [(start end step)
     (let loop ([n start])
       (cond
         [(if (positive? step)
              (n . >= . end)
              (n . <= . end)) '()]
         [else
          (cons n (loop (+ n step)))]))]))

(define (append-map proc lst . lsts)
  (append* (apply map proc lst lsts)))

(define (filter-not pred lst)
  (filter (λ (x) (not (pred x))) lst))

(define (argmin proc lst)
  (!argmin proc (!list lst)))

(define (argmax proc lst)
  (!argmax proc (!list lst)))

;; internal utility functions
;; ---------------------------------------------------------------------------------------------------

(define (improper-length lst)
  (let loop ([n 0] [lst (! lst)])
    (cond
      [(pair? lst) (loop (add1 n) (! (cdr lst)))]
      [else n])))

(define (improper-reverse lst)
  (let loop ([lst (! lst)]
             [acc '()])
    (cond
      [(pair? lst) (loop (! (cdr lst)) (cons (car lst) acc))]
      [else acc])))
