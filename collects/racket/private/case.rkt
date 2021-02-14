;;----------------------------------------------------------------------
;; case: based on Clinger, "Rapid Case Dispatch in Scheme"
;;       [http://scheme2006.cs.uchicago.edu/07-clinger.pdf]

(module case '#%kernel
  (#%require '#%paramz '#%unsafe "qq-and-or.rkt" "cond.rkt" "define.rkt" "fixnum.rkt"
             (for-syntax '#%kernel "define-et-al.rkt" "qq-and-or.rkt" "cond.rkt"
                         "stxcase-scheme.rkt"
                         "qqstx.rkt" "define.rkt" "sort.rkt" "fixnum.rkt"
                         "stx.rkt"))
  (#%provide case)

  (define-syntax (case stx)
    (syntax-case stx ()
      ;; Empty case
      [(_ v)
       (syntax-protect
        (syntax/loc stx (#%expression (begin v (void)))))]
      
      ;; Else-only case
      [(_ v [maybe-else e es ...])
       (and (identifier? #'maybe-else) (free-identifier=? #'else #'maybe-else))
       (syntax-property
        (syntax-protect
         (syntax/loc stx (#%expression (begin v (let-values () e es ...)))))
        'disappeared-use
        (list (syntax-local-introduce #'maybe-else)))]
      
      ;; If we have a syntactically correct form without an 'else' clause,
      ;; add the default 'else' and try again.
      [(self v [(k ...) e1 e2 ...] ...)
       (syntax-protect
        (syntax/loc stx (self v [(k ...) e1 e2 ...] ... [else (void)])))]
      
      ;; The general case
      [(_ v [(k ...) e1 e2 ...] ... [maybe-else x1 x2 ...])
       (and (identifier? #'maybe-else) (free-identifier=? #'else #'maybe-else))
       (syntax-property
        (syntax-protect
         (if (< (length (syntax-e #'(k ... ...))) *sequential-threshold*)
             (syntax/loc stx (let ([tmp v])
                               (case/sequential tmp [(k ...) e1 e2 ...] ... [else x1 x2 ...])))
             (syntax/loc stx (let ([tmp v])
                               (case/dispatch   tmp [(k ...) e1 e2 ...] ... [else x1 x2 ...])))))
        'disappeared-use
        (list (syntax-local-introduce #'maybe-else)))]
      
      ;; Error cases
      [(_ v clause ...)
       (let loop ([clauses (syntax->list #'(clause ...))])
         (unless (null? clauses)
           (let ([clause (car clauses)])
             (syntax-case clause ()
               [((_ ...) _ _ ...)
                (loop (cdr clauses))]
               [((_ ...) . _)
                (syntax-case clause ()
                  [(_)
                   (raise-syntax-error
                    #f
                    "bad syntax (missing expression after datum sequence)"
                    stx
                    clause)]
                  [(_ . _)
                   (raise-syntax-error
                    #f
                    "bad syntax (illegal use of `.' in clause)"
                    stx
                    clause)]
                  [_
                   (raise-syntax-error 
                    #f
                    "bad syntax (ill-formed clause)"
                    stx
                    clause)])]
               [(bad . _)
                (raise-syntax-error 
                 #f
                 ;; If #'bad is an identifier, report its binding in the error message.
                 ;; This helps resolving the syntax error when `else' is shadowed somewhere
                 (if (not (symbol? (syntax-e (syntax bad))))
                     "bad syntax (not a datum sequence)"
                     (string-append
                      "bad syntax (not a datum sequence)\n"
                      "  expected: a datum sequence or the binding 'else' from racket/base\n"
                      "  given: "
                      (let ([binding (identifier-binding (syntax bad))])
                        (cond
                          [(not binding) "an unbound identifier"]
                          [(eq? binding 'lexical) "a locally bound identifier"]
                          [else
                           (let*-values ([(src) (car binding)]
                                         [(mpath base) (module-path-index-split src)])
                             (cond
                               [(not mpath)
                                "an identifier bound by the current module"]
                               [else
                                (format "an identifier required from the module ~a"
                                        (resolved-module-path-name
                                         (module-path-index-resolve src)))]))]))))
                 stx
                 (syntax bad))]
               [_
                (raise-syntax-error 
                 #f
                 "bad syntax (ill-formed clause)"
                 stx
                 (syntax bad))]))))]
      [(_ . v)
       (not (null? (syntax-e (syntax v))))
       (raise-syntax-error 
        #f
        "bad syntax (illegal use of `.')"
        stx)]))
  
  ;; Sequential case: 
  ;; Turn the expression into a sequence of if-then-else.
  (define-syntax (case/sequential stx)
    (syntax-case stx (else)
      [(_ v [(k ...) es ...] arms ... [else xs ...])
       (syntax-protect
        #'(if (case/sequential-test v (k ...))
              (let-values () es ...)
              (case/sequential v arms ... [else xs ...])))]
      [(_ v [(k ...) es ...] [else xs ...])
       (syntax-protect
        #'(if (case/sequential-test v (k ...))
              (let-values () es ...)
              (let-values () xs ...)))]
      [(_ v [else xs ...])
       (syntax-protect
        #'(let-values () xs ...))]))
  
  (define-syntax (case/sequential-test stx)
    (syntax-protect
     (syntax-case stx ()
       [(_ v ())         #'#f]
       [(_ v (k))        #`(equal? v 'k)]
       [(_ v (k ks ...)) #`(if (equal? v 'k)
                               #t
                               (case/sequential-test v (ks ...)))])))
  
  ;; Triple-dispatch case:
  ;; (1) From the type of the value to a type-specific mechanism for
  ;; (2) mapping the value to the index of the consequent we need. Then,
  ;; (3) from the index, perform a binary search to find the consequent code.
  ;; Note: the else clause is given index 0.
  (define-syntax (case/dispatch stx)
    (syntax-case stx (else)
      [(_ v [(k ...) es ...] ... [else xs ...])
       (syntax-protect
        #`(let ([index
                 #,(let* ([ks  (partition-constants #'((k ...) ...))]
                          [exp #'0]
                          [exp (if (null? (consts-other ks))
                                   exp
                                   (dispatch-other #'v (consts-other ks) exp))]
                          [exp (if (null? (consts-char ks))
                                   exp
                                   #`(if (char? v)
                                         #,(dispatch-char #'v (consts-char ks))
                                         #,exp))]
                          [exp (if (null? (consts-symbol ks))
                                   exp
                                   #`(if #,(test-for-symbol #'v (consts-symbol ks))
                                         #,(dispatch-symbol #'v (consts-symbol ks) #'0)
                                         #,exp))]
                          [exp (if (null? (consts-fixnum ks))
                                   exp
                                   #`(if #,(if (null? (consts-other ks))
                                               ;; can check for fixnum
                                               #'(fixnum? v)
                                               ;; maybe could-be-fixnum numbers are in `others`
                                               #'(fixnum-for-every-system? v))
                                         #,(dispatch-fixnum #'v (consts-fixnum ks))
                                         #,exp))])
                     exp)])
            #,(index-binary-search #'index #'([xs ...] [es ...] ...))))]))

  (begin-for-syntax
    (define *sequential-threshold* 12)
    (define *hash-threshold*       10)
    
    (define nothing (gensym))
    
    (define interval-lo    car)
    (define interval-hi    cadr)
    (define interval-index caddr)
    
    (define (partition-constants stx)
      (define h (make-hash))
      
      (define (duplicate? x)
        (not (eq? (hash-ref h x nothing) nothing)))
      
      (define (add xs x idx)
        (hash-set! h x idx)
        (cons (cons x idx) xs))
      
      (let loop ([f '()] [s '()] [c '()] [o '()] [idx 1] [xs (syntax->list stx)])
        (cond [(null? xs)
               (list (cons 'fixnum f)
                     (cons 'symbol s)
                     (cons 'char   c)
                     (cons 'other  o))]
              [else (let inner ([f f] [s s] [c c] [o o] [ys (syntax->list (car xs))])
                      (cond [(null? ys) (loop f s c o (add1 idx) (cdr xs))]
                            [else
                             (let ([y (syntax->datum (car ys))])
                               (cond [(duplicate? y) (inner f s c o (cdr ys))]
                                     [(fixnum-for-every-system? y) (inner (add f y idx) s c o (cdr ys))]
                                     [(symbol? y)    (inner f (add s y idx) c o (cdr ys))]
                                     [(keyword? y)   (inner f (add s y idx) c o (cdr ys))]
                                     [(char? y)      (inner f s (add c y idx) o (cdr ys))]
                                     [else           (inner f s c (add o y idx) (cdr ys))]))]))])))
    
    (define (consts-fixnum ks) (cdr (assq 'fixnum ks)))
    (define (consts-symbol ks) (cdr (assq 'symbol ks)))
    (define (consts-char   ks) (cdr (assq 'char   ks)))
    (define (consts-other  ks) (cdr (assq 'other  ks)))
    
    ;; Character dispatch is fixnum dispatch.
    (define (dispatch-char tmp-stx char-alist)
      #`(let ([codepoint (char->integer #,tmp-stx)])
          #,(dispatch-fixnum #'codepoint
                             (map (λ (x)
                                    (cons (char->integer (car x))
                                          (cdr x)))
                                  char-alist))))
    
    ;; Symbol and "other" dispatch is either sequential or
    ;; hash-table-based, depending on how many constants we
    ;; have. Assume that `alist' does not map anything to `#f'.
    (define (dispatch-hashable tmp-stx alist make-hashX else-exp)
      (if (< (length alist) *hash-threshold*)
          #`(case/sequential #,tmp-stx 
                             #,@(map (λ (x)
                                       #`[(#,(car x)) #,(cdr x)])
                                     alist)
                             [else #,else-exp])
          (let ([tbl (make-hashX alist)])
            (if (literal-expression? else-exp)
                #`(hash-ref #,tbl #,tmp-stx (lambda () #,else-exp))
                #`(or (hash-ref #,tbl #,tmp-stx (lambda () #f))
                      #,else-exp)))))

    (define (dispatch-symbol tmp-stx symbol-alist else-exp)
      (dispatch-hashable tmp-stx symbol-alist make-immutable-hasheq else-exp))

    (define (dispatch-other tmp-stx other-alist else-exp)
      (dispatch-hashable tmp-stx other-alist make-immutable-hash else-exp))

    (define (test-for-symbol tmp-stx alist)
      (define (contains? pred)
        (ormap (lambda (p) (pred (car p))) alist))
      (if (contains? symbol?)
          (if (contains? keyword?)
              #`(or (symbol? #,tmp-stx) (keyword? #,tmp-stx))
              #`(symbol? #,tmp-stx))
          #`(keyword? #,tmp-stx)))

    (define (literal-expression? else-exp)
      (define v (syntax-e else-exp))
      (or (boolean? v) (number? v)))
    
    ;; Fixnum dispatch is either table lookup or binary search.
    (define (dispatch-fixnum tmp-stx fixnum-alist)
      (define (go intervals lo hi lo-bound hi-bound)
        (define len (length intervals))
        
        (cond [(or (>= lo-bound hi)
                   (<= hi-bound lo))
               #'0]
              [(and (> len 1)
                    (< (- hi lo) (* len 5)))
               (fixnum-table-lookup  intervals lo hi lo-bound hi-bound)]
              [else
               (fixnum-binary-search intervals lo hi lo-bound hi-bound)]))
      
      (define (fixnum-table-lookup intervals lo hi lo-bound hi-bound)
        (define index-lists
          (map (λ (int)
                 (vector->list
                  (make-vector (- (interval-hi int)
                                  (interval-lo int))
                               (interval-index int))))
               intervals))
        
        #`(let ([tbl #,(list->vector (apply append index-lists))])
            #,(bounded-expr tmp-stx lo hi lo-bound hi-bound
                            #`(unsafe-vector*-ref tbl (unsafe-fx- #,tmp-stx #,lo)))))
      
      (define (fixnum-binary-search intervals lo hi lo-bound hi-bound)
        (cond [(null? (cdr intervals))
               #`#,(interval-index (car intervals))]
              [else
               (define-values (lo-ints hi-ints) (split-intervals intervals))
               (define-values (lo-lo lo-hi) (lo+hi lo-ints))
               (define-values (hi-lo hi-hi) (lo+hi hi-ints))

               #`(if (unsafe-fx< #,tmp-stx #,hi-lo)
                     #,(go lo-ints lo-lo lo-hi lo-bound hi-lo)
                     #,(go hi-ints hi-lo hi-hi hi-lo hi-bound))]))

      (define (split-intervals intervals)
        (define n (quotient (length intervals) 2))
        (let loop ([n n] [lo '()] [hi intervals])
          (cond [(zero? n) (values (reverse lo) hi)]
                [else (loop (sub1 n) (cons (car hi) lo) (cdr hi))])))
      
      (define (lo+hi intervals)
        (values (interval-lo (car intervals))
                (interval-hi (car (reverse intervals)))))
      
      (define intervals (alist->intervals fixnum-alist))
      (define-values (lo hi) (lo+hi intervals))
      
      #`(if (and (unsafe-fx>= #,tmp-stx #,lo)
                 (unsafe-fx<  #,tmp-stx #,hi))
            #,(go intervals lo hi lo hi)
            0))
    
    ;; Once we have the index of the consequent we want, perform
    ;; a binary search to find it.
    (define (index-binary-search index-stx leg-stx)
      (define legs (list->vector (syntax->list leg-stx)))

      (define (go min max)
        (cond [(= min max)
               #`(let-values () #,@(vector-ref legs min))]
              [(= max (add1 min))
               #`(if (unsafe-fx< #,index-stx #,max)
                     (let-values () #,@(vector-ref legs min))
                     (let-values () #,@(vector-ref legs max)))]
              [else
               (let ([mid (quotient (+ min max) 2)])
                 #`(if (unsafe-fx< #,index-stx #,mid)
                       #,(go min (sub1 mid))
                       #,(go mid max)))]))
      
      (go 0 (sub1 (vector-length legs))))
    
    (define (bounded-expr tmp-stx lo hi lo-bound hi-bound exp-stx)
      (cond [(and (<= hi-bound hi)
                  (>= lo-bound lo))
             exp-stx]
            [(<= hi-bound hi)
             #`(if (unsafe-fx>= #,tmp-stx #,lo) exp-stx 0)]
            [(>= lo-bound lo)
             #`(if (unsafe-fx<  #,tmp-stx #,hi) exp-stx 0)]
            [else
             #`(if (and (unsafe-fx>= #,tmp-stx #,lo)
                        (unsafe-fx<  #,tmp-stx #,hi))
                   exp-stx
                   0)]))
    
    (define (alist->intervals alist)
      (let loop ([xs (sort alist < car)] [start-idx #f] [end-idx #f] [cur-val #f] [res '()])
        (cond [(null? xs)
               (if start-idx
                   (reverse (cons (list start-idx end-idx cur-val) res))
                   '())]
              [else
               (let* ([x (car xs)]
                      [k (car x)]
                      [v (cdr x)])
                 (cond [(not start-idx)
                        (loop (cdr xs) k (add1 k) v res)]
                       [(and (= end-idx k) (= cur-val v))
                        (loop (cdr xs) start-idx (add1 end-idx) cur-val res)]
                       [(= end-idx k)
                        (let ([interval (list start-idx end-idx cur-val)])
                          (loop (cdr xs) k (add1 k) v (cons interval res)))]
                       [else
                        ;; insert an interval in the gap for the default
                        (let ([int1 (list start-idx end-idx cur-val)]
                              [int2 (list end-idx k 0)])
                          (loop (cdr xs) k (add1 k) v (cons int2 (cons int1 res))))]))])))))
