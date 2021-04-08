
;; Make `list?` an amoritized constant-time operation, which is
;; possible because `set-cdr!` is not exposed. Make it constant-time
;; by caching the result for an N-item list at the N/2-tail pair,
;; where a second request will cache at the N/4-tail pair, etc.
;; Detect cycles using the same `slow` tortoise that is used for
;; caching.
;;
;; To reduce the overhead of checking the hash table, only
;; start using it after the first `CHECK-AFTER-LEN` pairs.
;; Then, check only after `CHECK-SKIP-N` pairs --- and record
;; a sequence of `CHECK-SKIP-N`+1 results so one will hit when
;; checking every `CHECK-SKIP-N` pairs.

(define (list? v) (list-assuming-immutable? v))

(define (append-n l n l2)
  (cond
   [(fx= 0 n) l2]
   [else (cons (car l) (append-n (cdr l) (fx1- n) l2))]))
  
(define (unsafe-cons-list a d)
  ;; Can assume that `d` is a list
  (cons a d))

(define (list-pair? v)
  (and (pair? v)
       (list? v)))

;; ----------------------------------------

;; Early parts of the Racket stack use a built-in `map`, etc., which
;; we need to work with applicable structs for the JIT-style
;; compatilation approach

(define-syntax-rule (define-list-proc |#%name| name base combine)
  (define |#%name|
    (case-lambda
     [(f l)
      (if (list? l)
          (let loop ([l l])
            (if (null? l)
                base
                (let ([r (cdr l)])
                  (combine (|#%app| f (car l)) (loop r)))))
          (raise-argument-error 'name "list?" l))]
     [(f l1 l2)
      (cond
       [(not (list? l1))
        (raise-argument-error 'name "list?" l1)]
       [(not (list? l2))
        (raise-argument-error 'name "list?" l2)]
       [(not (= (length l1) (length l2)))
        (raise-arguments-error 'name "list lengths do not match"
                               "first list" l1
                               "second list" l2)]
       [else
        (let loop ([l1 l1] [l2 l2])
          (if (null? l1)
              base
              (let ([r1 (cdr l1)]
                    [r2 (cdr l2)])
                (combine (|#%app| f (car l1) (car l2))
                         (loop r1 r2)))))])]
     [(f l1 . ls)
      (when (not (list? l1))
        (raise-argument-error 'name "list?" l1))
      (let ([len (length l1)])
        (let loop ([ls ls])
          (unless (null? ls) 
            (let ([l (car ls)])
              (unless (list? l)
                (raise-argument-error 'name "list?" l))
              (unless (= (length l) len)
                (raise-arguments-error 'name "list lengths do not match"
                                       "first list" l1
                                       "other list" l)))
            (loop (cdr ls)))))
      (let loop ([l1 l1] [ls ls])
        (if (null? l1)
            base
            (let ([r1 (cdr l1)]
                  [r (map cdr ls)])
              (combine (apply f (car l1) (map car ls))
                       (loop r1 r)))))])))

(define-list-proc |#%map| map '() cons) 
(define-list-proc |#%for-each| for-each (void) begin)
(define-list-proc |#%andmap| andmap #t and)
(define-list-proc |#%ormap| andmap #f or)
