#lang scheme

(require "test-util.ss"
         "reduction-semantics.ss"
         "matcher.ss"
         "term.ss"
         "rg.ss"
         "keyword-macros.ss"
         "error.ss")

(reset-count)

;; to-table : hash-table -> assoc
;; extracts the hash-table's mapping in a deterministic way
(define (to-table ht)
  (sort (hash-map ht cons)
        (λ (x y) (string<=? (format "~a" (car x)) (format "~a" (car y))))))

(let ()
  (define-language lc
    (e x (e e) (λ (x) e))
    (x variable))
  (let ([bc (find-base-cases lc)])
    (test (to-table (base-cases-non-cross bc))
          '((e . (1 2 2)) (x . (0))))
    (test (to-table (base-cases-cross bc))
          '((e-e . (0 2 2 1)) (x-e . (1 2 2 2 2)) (x-x . (0))))))

(let ()
  (define-language lang
    (e (e e)))
  (let ([bc (find-base-cases lang)])
    (test (to-table (base-cases-non-cross bc)) '((e . (inf))))
    (test (to-table (base-cases-cross bc)) '((e-e . (0 inf inf))))))

(let ()
  (define-language lang
    (a 1 2 3)
    (b a (a_1 b_!_1)))
  (let ([bc (find-base-cases lang)])
    (test (to-table (base-cases-non-cross bc))
          '((a . (0 0 0)) (b . (1 2))))
    (test (to-table (base-cases-cross bc))
          '((a-a . (0)) (a-b . (1)) (b-b . (0))))))

(let ()
  (define-language lc
    (e (e e ...)
       (+ e e)
       x
       v)
    (v (λ (x) e)
       number)
    (x variable))
  (let ([bc (find-base-cases lc)])
    (test (to-table (base-cases-non-cross bc)) 
          '((e . (2 2 1 1)) (v . (2 0)) (x . (0))))
    (test (to-table (base-cases-cross bc))
          '((e-e . (0 2 2 2 2 2)) (e-v . (1)) (v-e . (2 2 2 2 1)) (v-v . (0 2))
                                  (x-e . (2 2 2 2 1 3)) (x-v . (2 2)) (x-x . (0))))))

(let ()
  (define-language L
    (x (variable-prefix x)
       (variable-except y))
    (y y))
  (test (hash-ref (base-cases-non-cross (find-base-cases L)) 'x)
        '(0 0)))

(let ()
  (define-language lang
    (e number x y)
    (x variable)
    (y y))
  (test (min-prods (car (compiled-lang-lang lang))
                   (base-cases-non-cross (find-base-cases lang)))
        (list (car (nt-rhs (car (compiled-lang-lang lang)))))))

(define (make-random . nums)
  (let ([nums (box nums)])
    (λ ([m +inf.0])
      (cond [(null? (unbox nums)) (error 'make-random "out of numbers")]
            [(>= (car (unbox nums)) m) (error 'make-random "number too large")]
            [else (begin0 (car (unbox nums)) (set-box! nums (cdr (unbox nums))))]))))

(test (pick-from-list '(a b c) (make-random 1)) 'b)

(test (pick-number 24 (make-random 1/5)) 3)
(test (pick-number 224 (make-random 0 0 1/5)) -5)
(test (pick-number 524 (make-random 0 0 1 1/5 1/5)) 3/4)
(test (pick-number 1624 (make-random 0 0 0 .5 1 .5)) 3.0)
(test (pick-number 2624 (make-random 0 0 0 0 1 1 1/5 1/5 2 .5 0 .5))
      (make-rectangular 7/8 -3.0))

(test (pick-natural 224 (make-random 1/5)) 5)
(test (pick-integer 900 (make-random 0 0 1/5)) -7)
(test (pick-real 9000 (make-random 0 0 0 .5 1 1/8)) 11.0)

(let* ([lits '("bcd" "cbd")])
  (test (pick-char 0 (make-random 0 0)) #\A)
  (test (pick-char 0 (make-random 2 1)) #\c)
  (test (pick-char 1000 (make-random 1 25 0)) #\Z)
  (test (pick-char 1000 (make-random 0 65)) #\a)
  (test (pick-char 1500 (make-random 0 1 65)) #\a)
  (test (pick-char 1500 (make-random 0 0 3)) #\⇒)
  (test (pick-char 2500 (make-random 0 0 1 3)) #\⇒)
  (test (pick-char 2500 (make-random 0 0 0 1)) (integer->char #x4E01))
  (test (pick-char 1000 (make-random 0 (- (char->integer #\_) #x20))) #\`)
  (test (random-string lits 3 0 (make-random 0 1)) "cbd")
  (test (random-string lits 3 0 (make-random 1 0 1 1 1 2 1)) "abc")
  (test (pick-string lits 0 (make-random .5 1 0 1 1 1 2 1)) "abc")
  (test (pick-var lits 0 (make-random .01 1 0 1 1 1 2 1)) 'abc))

(let ()
  (define-language L
    (a 5 (x a))
    (b 4))
  (test (pick-nt 'a #f L 1 'dontcare)
        (nt-rhs (car (compiled-lang-lang L))))
  (test (pick-nt 'a #f L preferred-production-threshold 'dontcare (make-random 1))
        (nt-rhs (car (compiled-lang-lang L))))
  (let ([pref (car (nt-rhs (car (compiled-lang-lang L))))])
    (test (pick-nt 'a #f L preferred-production-threshold
                   (make-pref-prods 'dont-care
                                    (make-immutable-hash `((a ,pref))))
                   (make-random 0))
          (list pref)))
  (test (pick-nt 'b #f L preferred-production-threshold #f)
        (nt-rhs (cadr (compiled-lang-lang L)))))

(define-syntax raised-exn-msg
  (syntax-rules ()
    [(_ expr) (raised-exn-msg exn:fail? expr)]
    [(_ exn? expr)
     (with-handlers ([exn? exn-message])
       (begin
         expr
         (let ()
           (define-struct exn-not-raised ())
           (make-exn-not-raised))))]))

(define (patterns . selectors) 
  (map (λ (selector) 
         (λ (name cross? lang size pref-prods)
           (list (selector (nt-rhs (nt-by-name lang name cross?))))))
       selectors))

(define (iterator name items)
  (let ([bi (box items)])
    (λ () 
      (if (null? (unbox bi))
          (error name "empty")
          (begin0 (car (unbox bi)) (set-box! bi (cdr (unbox bi))))))))

(let ([iter (iterator 'test-iterator '(a b))])
  (test (iter) 'a)
  (test (iter) 'b)
  (test (raised-exn-msg (iter)) #rx"empty"))

(define (decisions #:var [var pick-var] 
                   #:nt [nt pick-nt]
                   #:str [str pick-string]
                   #:num [num pick-number]
                   #:nat [nat pick-natural]
                   #:int [int pick-integer]
                   #:real [real pick-real]
                   #:any [any pick-any]
                   #:seq [seq pick-sequence-length]
                   #:pref [pref pick-preferred-productions])
  (define-syntax decision
    (syntax-rules ()
      [(_ d) (if (procedure? d) (λ () d) (iterator (quote d) d))]))
  (unit (import) (export decisions^)
        (define next-variable-decision (decision var))
        (define next-non-terminal-decision (decision nt))
        (define next-number-decision (decision num))
        (define next-natural-decision (decision nat))
        (define next-integer-decision (decision int))
        (define next-real-decision (decision real))
        (define next-string-decision (decision str))
        (define next-any-decision (decision any))
        (define next-sequence-decision (decision seq))
        (define next-pref-prods-decision (decision pref))))

(define-syntax generate-term/decisions
  (syntax-rules ()
    [(_ lang pat size attempt decisions)
     (parameterize ([generation-decisions decisions])
       (generate-term lang pat size #:attempt attempt))]))

(let ()
  (define-language lc
    (e (e e) x (λ (x) e))
    (x (variable-except λ)))
  
  ;; Generate (λ (x) x)
  (test 
   (generate-term/decisions 
    lc e 1 0
    (decisions #:var (list (λ _ 'x) (λ _'x))
               #:nt (patterns third first first first)))
   '(λ (x) x))
  
  ;; Generate pattern that's not a non-terminal
  (test 
   (generate-term/decisions 
    lc (x x x_1 x_1) 1 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y)))) 
   '(x x y y))
  
  ; After choosing (e e), size decremented forces each e to x.
  (test
   (generate-term/decisions 
    lc e 1 0
    (decisions #:nt (patterns first)
               #:var (list (λ _ 'x) (λ _ 'y))))
   '(x y)))

;; variable-except pattern
(let ()
  (define-language var
    (e (variable-except x y)))
  (test
   (generate-term/decisions
    var e 2 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x) (λ _ 'z))))
   'z))

(let ()
  (define-language L
    (n natural)
    (i integer)
    (r real))
  (test (let ([n (generate-term L n 0 #:attempt 10000)])
          (and (integer? n)
               (exact? n)
               (not (negative? n))))
        #t)
  (test (generate-term/decisions L n 0 1 (decisions #:nat (λ (_) 42))) 42)
  (test (let ([i (generate-term L i 0 #:attempt 10000)])
          (and (integer? i) (exact? i)))
        #t)
  (test (generate-term/decisions L i 0 1 (decisions #:int (λ (_) -42))) -42)
  (test (real? (generate-term L r 0 #:attempt 10000)) #t)
  (test (generate-term/decisions L r 0 1 (decisions #:real (λ (_) 4.2))) 4.2))

(let ()
  (define-language lang
    (a (number number ... "foo" ... "bar" #t ...))
    (b (number_1 ..._!_1 number_1 ..._1))
    (c (variable_1 ..._1 number_2 ..._1))
    (d (z_1 ... z_2 ..._!_1 (z_1 z_2) ...))
    (e (n_1 ..._!_1 n_2 ..._!_1 (n_1 n_2) ..._3))
    (f (n_1 ..._1 n_2 ..._2 n_2 ..._1))
    (g (z_1 ..._!_1 z_2 ... (z_1 z_2) ...))
    (n number)
    (z 4))
  (test
   (generate-term/decisions 
    lang a 2 0
    (decisions #:num (build-list 3 (λ (n) (λ (_) n)))
               #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 1))))
   `(0 1 2 "foo" "foo" "foo" "bar" #t))
  (test (generate-term/decisions lang b 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate-term/decisions lang c 5 0 (decisions #:seq (list (λ (_) 0))))
        null)
  (test (generate-term/decisions lang d 5 0 (decisions #:seq (list (λ (_) 2))))
        '(4 4 4 4 (4 4) (4 4)))
  (test (raised-exn-msg exn:fail:redex:generation-failure? (generate-term lang e 5 #:retries 42)) 
        #rx"generate-term: unable to generate pattern e in 42")
  (test (generate-term/decisions lang f 5 0 (decisions #:seq (list (λ (_) 0)))) null)
  (test (generate-term/decisions 
         lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
         (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 2) (λ (_) 3) (λ (_) 4)
                                (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 1) (λ (_) 3))))
        '((0 0 0) (0 0 0 0) (1 1 1)))
  (test (generate-term/decisions 
         lang ((0 ..._!_1) ... (1 ..._!_1) ...) 5 0
         (decisions #:seq (list (λ (_) 2) (λ (_) 3) (λ (_) 4) (λ (_) 2) (λ (_) 3) (λ (_) 5))))
        '((0 0 0) (0 0 0 0) (1 1 1) (1 1 1 1 1))))

(let ()
  (define-language lang (e (variable-prefix pf)))
  (test 
   (generate-term/decisions
    lang e 5 0
    (decisions #:var (list (λ _ 'x))))
   'pfx))

(let ()
  (define-language lang (x variable literal))
  (test ((is-nt? lang) 'x) #t)
  (test ((is-nt? lang) 'y) #f))

(let ()
  (define-language lang
    (e number (e_1 e_2 e e_1 e_2)))
  (test
   (generate-term/decisions
    lang e 5 0
    (decisions #:nt (patterns second first first first)
               #:num (list (λ _ 2) (λ _ 3) (λ _ 4))))
   '(2 3 4 2 3)))

(let ()
  (define-language lang
    (a (number_!_1 number_!_2 number_!_1))
    (b (c_!_1 c_!_1 c_!_1))
    (c 1 2))
  (test
   (generate-term/decisions
    lang a 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (generate-term/decisions
    lang (number_!_1 number_!_2 number_!_1) 5 0
    (decisions #:num (list (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 1) (λ _ 2))))
   '(1 1 2))
  (test
   (raised-exn-msg exn:fail:redex:generation-failure? (generate-term lang b 5000))
   #rx"unable"))

(let ()
  (define-language lang
    (e string)
    (f foo bar))
  (test
   (let/ec k 
     (generate-term/decisions 
      lang e 5 0 
      (decisions #:str (list (λ (l a) (k (sort l string<=?)))))))
   '("bar" "foo")))

(let ()
  (define-language lang
    (a 43)
    (b (side-condition a_1 (odd? (term a_1))))
    (c (side-condition a_1 (even? (term a_1))))
    (e (side-condition (x_1 x_!_2 x_!_2) (not (eq? (term x_1) 'x))))
    (x variable))
  (test (generate-term lang b 5) 43)
  (test (generate-term lang (side-condition a (odd? (term a))) 5) 43)
  (test (raised-exn-msg exn:fail:redex:generation-failure? (generate-term lang c 5))
        #px"unable to generate pattern \\(side-condition a\\_1 #<syntax:.*\\/rg-test\\.ss:\\d+:\\d+>\\)")
  (test (let/ec k
          (generate-term lang (number_1 (side-condition 7 (k (term number_1)))) 5))
        'number_1)
  
  (test ; mismatch patterns work with side-condition failure/retry
   (generate-term/decisions
    lang e 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'y) (λ _ 'y) (λ _ 'x) (λ _ 'y))))
   '(y x y))
  (test ; generate compiles side-conditions in pattern
   (generate-term/decisions 
    lang (side-condition x_1 (not (eq? (term x_1) 'x))) 5 0
    (decisions #:var (list (λ _ 'x) (λ _ 'y))))
   'y))

(let ()
  (define-language lang
    (a (name x b))
    (b 4)
    (c (side-condition (name x d) (zero? (term x))))
    (d 2 1 0)
    (e ((side-condition (name d_1 d) (zero? (term d_1))) d_1)))
  (test (generate-term lang a 5) 4)
  (test (generate-term lang c 5) 0)
  (test (generate-term lang e 5) '(0 0)))

(let ()
  (define-language lang
    (a number (+ a a))
    (A hole (+ a A) (+ A a))
    (C hole)
    (e ((in-hole (in-hole f (number_1 hole)) number_1) number_1)) 
    (f (in-hole C (number_1 hole)))
    (g (in-hole (side-condition (hole number_1) (zero? (term number_1))) number_2))
    (h ((in-hole i number_1) number_1))
    (i (number_1 (in-hole j (number_1 hole))))
    (j (in-hole (hole number_1) (number_1 hole)))
    (x variable)
    (y variable))
  
  (test 
   (generate-term/decisions 
    lang (in-hole A number ) 5 0
    (decisions 
     #:nt (patterns second second first first third first second first first)
     #:num (build-list 5 (λ (x) (λ (_) x)))))
   '(+ (+ 1 2) (+ 0 (+ 3 4))))
  
  (test (generate-term lang (in-hole (in-hole (1 hole) hole) 5) 5) '(1 5))
  (test (generate-term lang (hole 4) 5) (term (hole 4)))
  (test (generate-term/decisions 
         lang (variable_1 (in-hole C variable_1)) 5 0
         (decisions #:var (list (λ _ 'x) (λ _ 'y) (λ _ 'x))))
        '(x x))
  (test (generate-term/decisions 
         lang (variable_!_1 (in-hole C variable_!_1)) 5 0
         (decisions #:var (list (λ _ 'x) (λ _ 'x) (λ _ 'x) (λ _ 'y))))
        '(x y))
  (test (generate-term/decisions lang e 5 0 (decisions #:num (list (λ _ 1) (λ _ 2))))
        '((2 (1 1)) 1))
  (test (generate-term/decisions lang g 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 1) (λ _ 0))))
        '(1 0))
  (test (generate-term/decisions lang h 5 0 (decisions #:num (list (λ _ 1) (λ _ 2) (λ _ 3))))
        '((2 ((3 (2 1)) 3)) 1)))

(let ()
  (define-language lc
      (e (e e) (+ e e) x v)
      (v (λ (x) e) number)
      (x variable-not-otherwise-mentioned))
  (test (generate-term/decisions lc x 5 0 (decisions #:var (list (λ _ 'λ) (λ _ '+) (λ _ 'x))))
        'x))

(let ()
  (define-language four 
    (e 4)
    (f 5))
  (define-language empty)
  
  ;; `any' pattern
  (let ([four (prepare-lang four)]
        [sexp (prepare-lang sexp)])
    (test (call-with-values (λ () (pick-any four sexp (make-random 0 1))) list)
          (list four 'f))
    (test (call-with-values (λ () (pick-any four sexp (make-random 1))) list)
          (list sexp 'sexp)))
  (test (generate-term/decisions 
         four any 5 0 (decisions #:any (list (λ (lang sexp) (values lang 'e))))) 4)
  (test (generate-term/decisions
         four any 5 0 
         (decisions #:any (list (λ (lang sexp) (values sexp 'sexp)))
                    #:nt (patterns fifth second second second)
                    #:seq (list (λ _ 3))
                    #:str (list (λ _ "foo") (λ _ "bar") (λ _ "baz"))))
        '("foo" "bar" "baz"))
  (test (generate-term/decisions
         empty any 5 0 (decisions #:nt (patterns first)
                                  #:var (list (λ _ 'x))))
        'x))

;; `hide-hole' pattern
(let ()
  (define-language lang
    (e (hide-hole (in-hole ((hide-hole hole) hole) 1))))
  (test (generate-term lang e 5) (term (hole 1))))

(define (output-error-port thunk)
  (let ([port (open-output-string)])
    (parameterize ([current-error-port port])
      (thunk))
    (get-output-string port)))

;; `cross' pattern
(let ()
  (define-language lang
    (e x (e e) v)
    (v (λ (x) e))
    (x variable-not-otherwise-mentioned))
  (define-extended-language name-collision lang (e-e 47))
  
  (test (generate-term/decisions
         lang (cross e) 3 0 
         (decisions #:nt (patterns fourth first first second first first first)
                    #:var (list (λ _ 'x) (λ _ 'y))))
        (term (λ (x) (hole y))))
  
  (test (generate-term/decisions name-collision (cross e) 3 0
                                 (decisions #:nt (patterns first)))
        (term hole))
  (test (generate-term/decisions name-collision e-e 3 0
                                 (decisions #:nt (patterns first)))
        47)
  
  (test (hash-ref (base-cases-non-cross (find-base-cases name-collision)) 'e-e)
        '(0)))

(let ()
  (define-language L
    (a ((a ...) ...)))
  (test (generate-term/decisions
         L (cross a) 3 0 
         (decisions #:nt (patterns second first)
                    #:seq (list (λ _ 0) (λ _ 0) (λ _ 0) (λ _ 0))))
        (term ((hole)))))

;; generation failures increase size and attempt
(let ()
  (define-language L
    (a d b)
    (b d c)
    (c e)
    
    (x variable))
  (test
   (generate-term/decisions
    L (side-condition a (eq? (term a) 'e)) 0 0
    ; It isn't possible for `a' to generate 'y until size is 2.
    ; When size is 0, the generator has no choice but the 'x production.
    ; When size is 1, the generator has a choice for `a' but not for `b'.
    ; Supply enough first-production choices to cover the size 1 attempts
    ; followed by the choices that produce 'y on the first size 2 attempt.
    (decisions 
     #:nt (apply patterns 
                 (append (build-list (* default-retries proportion-at-size)
                                     (λ (_) first))
                         (list second second first)))))
   'e)
  
  (test
   (generate-term/decisions
    L (side-condition x (number? (term x))) 0 0
    (decisions #:var (λ (lang-lits attempt)
                       (if (>= attempt retry-threshold) 0 'x))))
   0)
  
  (let ([attempts null]
        [start (sub1 retry-threshold)]
        [finish (+ retry-threshold post-threshold-incr)])
    (generate-term/decisions
     L (side-condition x (number? (term x))) 0 start
     (decisions #:var (λ (lang-lits attempt)
                        (set! attempts (cons attempt attempts))
                        (if (= attempt finish) 0 'x))))
    (test attempts (list finish retry-threshold start))))

;; output : (-> (-> void) string)
(define (output thunk)
  (let ([p (open-output-string)])
    (parameterize ([current-output-port p])
      (unless (void? (thunk))
        (error 'output "expected void result")))
    (begin0
      (get-output-string p)
      (close-output-port p))))

;; preferred productions
(let ([make-pick-nt (λ opt (λ req (apply pick-nt (append req opt))))])
  (define-language L
    (e (+ e e) (* e e) 7))
  (define-language M (e 0) (e-e 1))
  
  (let ([pats (λ (L) (nt-rhs (car (compiled-lang-lang L))))])
    (test 
     (generate-term/decisions
      L e 2 preferred-production-threshold
      (decisions #:pref (list (λ (L) (make-pref-prods
                                      'dont-care
                                      (make-immutable-hash `((e ,(car (pats L))))))))
                 #:nt (make-pick-nt (make-random 0 0 0))))
     '(+ (+ 7 7) (+ 7 7)))
    (test
     (generate-term/decisions
      L any 2 preferred-production-threshold
      (decisions #:nt (patterns first)
                 #:var (list (λ _ 'x))
                 #:any (list (λ (lang sexp) (values sexp 'sexp)))))
     'x)
    (test
     (generate-term/decisions
      L any 2 preferred-production-threshold
      (decisions #:pref (list (λ (L) (make-pref-prods
                                      'dont-care
                                      (make-immutable-hash `((e ,(car (pats L))))))))
                 #:nt (make-pick-nt (make-random 0 0 0))
                 #:any (list (λ (lang sexp) (values lang 'e)))))
     '(+ (+ 7 7) (+ 7 7)))
    (test
     (generate-term/decisions
      M (cross e) 2 preferred-production-threshold
      (decisions #:nt (make-pick-nt (make-random) (λ (att rand) #t))))
     (term hole))
    (test
     (generate-term/decisions
      M e-e 2 preferred-production-threshold
      (decisions #:nt (make-pick-nt (make-random) (λ (att rand) #t))))
     1)
    
    (test
     (let ([generated null])
       (output
        (λ ()
          (check-reduction-relation
           (reduction-relation L (--> e e))
           (λ (t) (set! generated (cons t generated)))
           #:decisions (decisions #:nt (make-pick-nt (make-random)
                                                     (λ (att rand) #t))
                                  #:pref (list (λ (_) 'dontcare)
                                               (λ (_) 'dontcare)
                                               (λ (_) 'dontcare)
                                               ; size 0 terms prior to this attempt
                                               (λ (L) (make-pref-prods
                                                       'dont-care
                                                       (make-immutable-hash `((e ,(car (pats L)))))))
                                               (λ (L) (make-pref-prods
                                                       'dont-care
                                                       (make-immutable-hash `((e ,(cadr (pats L)))))))))
           #:attempts 5)))
       generated)
     '((* 7 7) (+ 7 7) 7 7 7))))

;; redex-check
(let ()
  (define-language lang
    (d 5)
    (e e 4)
    (n number))
  (test (output (λ () (redex-check lang d #f)))
        #rx"redex-check: .*:.*\ncounterexample found after 1 attempt:\n5\n")
  (test (output (λ () (redex-check lang d #t))) 
        #rx"redex-check: .*:.*\nno counterexamples in 1000 attempts\n")
  (let-syntax ([noloc (λ (stx)
                        (syntax-case stx ()
                          [(_ e) (datum->syntax stx (syntax->datum #'e) #f)]))])
    (test (output (λ () (noloc (redex-check lang d #t)))) 
          "redex-check: no counterexamples in 1000 attempts\n"))
  (test (output (λ () (redex-check lang (d e) (and (eq? (term d) 5) (eq? (term e) 4)) #:attempts 2)))
        #rx"no counterexamples")
  (test (output (λ () (redex-check lang (d ...) (zero? (modulo (foldl + 0 (term (d ...))) 5)) #:attempts 2)))
        #rx"no counterexamples")
  (test (output (λ () (redex-check lang (d e) #f)))
        #rx"counterexample found after 1 attempt:\n\\(5 4\\)\n")
  (let* ([p (open-output-string)]
         [m (parameterize ([current-output-port p])
              (with-handlers ([exn:fail? exn-message])
                (redex-check lang d (error 'pred-raised))
                'no-exn-raised))])
    (test m "error: pred-raised")
    (test (get-output-string p) #rx"checking 5 raises.*\n$")
    (close-output-port p))

  (test (output
         (λ ()
           (redex-check lang n (eq? 42 (term n)) 
                        #:attempts 1
                        #:source (reduction-relation 
                                  lang 
                                  (--> 42 dontcare)
                                  (--> 0 dontcare z)))))
        #rx"counterexample found after 1 attempt with z:\n0\n")
  
  (let ([generated null])
    (test (output
           (λ ()
             (redex-check lang n (set! generated (cons (term n) generated)) 
                          #:attempts 5
                          #:source (reduction-relation 
                                    lang 
                                    (--> 1 dontcare)
                                    (--> 2 dontcare)))))
          #rx"no counterexamples.*with each clause")
    (test generated '(2 2 1 1)))
  
  (let ()
    (define-metafunction lang
      [(mf 42) dontcare]
      [(mf 0) dontcare])
    (test (output
           (λ ()
             (redex-check lang (n) (eq? 42 (term n)) 
                          #:attempts 1
                          #:source mf)))
          #px"counterexample found after 1 attempt with clause at .*:\\d+:\\d+:\n\\(0\\)\n"))
  
  (let ()
    (define-metafunction lang
      [(f) 
       dontcare
       (side-condition #f)])
    (test (raised-exn-msg
           exn:fail:redex:generation-failure?
           (redex-check lang any #t 
                        #:attempts 1
                        #:source f))
          #px"unable to generate LHS of clause at .*:\\d+:\\d+"))
  
  (let ()
    (define-metafunction lang
      [(mf d e) dontcare])
    (test (output
           (λ ()
             (redex-check lang (number_1 number_2) 
                          (and (= (term number_1) 5)
                               (= (term number_2) 4)) 
                          #:attempts 1
                          #:source mf)))
          #rx"no counterexamples"))
  
  (test (raised-exn-msg 
         exn:fail:redex?
         (redex-check lang n #t #:source (reduction-relation lang (--> x 1))))
        #rx"x does not match n")
  (test (raised-exn-msg
         exn:fail:redex:generation-failure?
         (redex-check lang (side-condition any #f) #t #:retries 42 #:attempts 1))
        #rx"^redex-check: unable .* in 42")
  (let-syntax ([test-gen-fail
                (syntax-rules ()
                  [(_ rhs expected)
                   (test 
                    (raised-exn-msg
                     exn:fail:redex:generation-failure?
                     (redex-check lang any #t 
                                  #:source (reduction-relation 
                                            lang
                                            rhs)
                                  #:retries 42
                                  #:attempts 1))
                    expected)])])
    (test-gen-fail 
     (--> (side-condition any #f) any)
     #px"^redex-check: unable to generate LHS of clause at .*:\\d+:\\d+ in 42")
    
    (test-gen-fail
     (--> (side-condition any #f) any impossible)
     #rx"^redex-check: unable to generate LHS of impossible in 42")))

;; check-metafunction-contract
(let ()
  (define-language empty)
  (define-metafunction empty
    f : (side-condition number_1 (odd? (term number_1))) -> number
    [(f 1) 1]
    [(f 3) 'NaN])
  
  (define-metafunction empty
    g : number ... -> (any ...)
    [(g number_1 ... 1 number_2 ...) (number_1 ...)])
  
  (define-metafunction empty
    h : number -> number
    [(h any) any])
  
  (define-metafunction empty
    [(i any ...) (any ...)])
  
  (define-metafunction empty
    j : (side-condition any #f) -> any
    [(j any ...) (any ...)])
  
  ;; Dom(f) < Ctc(f)
  (test (output 
         (λ () 
           (parameterize ([generation-decisions 
                           (decisions #:num (list (λ _ 2) (λ _ 5)))])
             (check-metafunction-contract f))))
        #rx"check-metafunction-contract:.*counterexample found after 1 attempt:\n\\(5\\)\n")
  ;; Rng(f) > Codom(f)
  (test (output
         (λ () 
           (parameterize ([generation-decisions
                           (decisions #:num (list (λ _ 3)))])
             (check-metafunction-contract f))))
        #rx"counterexample found after 1 attempt:\n\\(3\\)\n")
  ;; LHS matches multiple ways
  (test (output
         (λ () 
           (parameterize ([generation-decisions
                           (decisions #:num (list (λ _ 1) (λ _ 1))
                                      #:seq (list (λ _ 2)))])
             (check-metafunction-contract g))))
        #rx"counterexample found after 1 attempt:\n\\(1 1\\)\n")
  ;; OK -- generated from Dom(h)
  (test (output (λ () (check-metafunction-contract h))) #rx"no counterexamples")
  ;; OK -- generated from pattern (any ...)
  (test (output (λ () (check-metafunction-contract i #:attempts 5))) #rx"no counterexamples")
  
  ;; Unable to generate domain
  (test (raised-exn-msg
         exn:fail:redex:generation-failure?
         (check-metafunction-contract j #:attempts 1 #:retries 42))
        #rx"^check-metafunction-contract: unable .* in 42"))

;; check-reduction-relation
(let ()
  (define-language L
    (e (+ e ...) number)
    (E (+ number ... E* e ...))
    (E* hole E*)
    (n 4))
  
  (let ([generated null]
        [R (reduction-relation
            L
            (==> (+ number ...) whatever)
            (--> (side-condition number (even? (term number))) whatever)
            with
            [(--> (in-hole E a) whatever)
             (==> a b)])])
    (test (begin
            (output
             (λ ()
               (check-reduction-relation 
                R (λ (term) (set! generated (cons term generated)))
                #:decisions (decisions #:seq (list (λ _ 0) (λ _ 0) (λ _ 0))
                                       #:num (list (λ _ 1) (λ _ 1) (λ _ 0)))
                #:attempts 1)))
            generated)
          (reverse '((+ (+)) 0))))
  
  (let ([S (reduction-relation L (--> 1 2 name) (--> 3 4))])
    (test (output (λ () (check-reduction-relation S (λ (x) #t) #:attempts 1)))
          #rx"check-reduction-relation:.*no counterexamples")
    (test (output 
           (λ () (check-reduction-relation S (λ (x) #f))))
          #rx"counterexample found after 1 attempt with name:\n1\n")
    (test (output 
           (λ () (check-reduction-relation S (curry eq? 1))))
          #px"counterexample found after 1 attempt with clause at .*:\\d+:\\d+:\n3\n"))
  
  (test (output 
         (λ () (check-reduction-relation (reduction-relation L (--> 1 2) (--> 3 4 name)) (curry eq? 1))))
        #px"counterexample found after 1 attempt with name:\n3\n")
  
  (let ([T (reduction-relation
            L
            (==> number number
                 (where any_num number)
                 (side-condition (eq? (term any_num) 4))
                 (where any_numb any_num)
                 (side-condition (eq? (term any_numb) 4)))
            with
            [(--> (9 a) b)
             (==> a b)])])
    (test (output
           (λ ()
             (check-reduction-relation 
              T (curry equal? '(9 4)) 
              #:attempts 1
              #:decisions (decisions #:num (build-list 5 (λ (x) (λ _ x)))))))
          #rx"no counterexamples"))
  
  (let ([U (reduction-relation L (--> (side-condition any #f) any))])
    (test (raised-exn-msg
           exn:fail:redex:generation-failure?
           (check-reduction-relation U (λ (_) #t)))
          #rx"^check-reduction-relation: unable")))

; check-metafunction
(let ()
  (define-language empty)
  
  (define-metafunction empty
    [(m 1) whatever]
    [(m 2) whatever])
  (define-metafunction empty
    [(n (side-condition any #f)) any])
  
  (let ([generated null])
    (test (begin
            (output 
             (λ ()
               (check-metafunction m (λ (t) (set! generated (cons t generated))) #:attempts 1)))
            generated) 
          (reverse '((1) (2)))))
  
  (test
   (let/ec k
     (define-language L (n 2))
     (define-metafunction L
       [(f n)
        n
        (where number_2 ,(add1 (term n)))
        (where number_3 ,(add1 (term number_2)))
        (side-condition (k (term number_3)))]
       [(f any) 0])
     (check-metafunction f (λ (_) #t)))
   4)
  
  (test (output (λ () (check-metafunction m (λ (_) #t)))) #rx"no counterexamples")
  (test (output (λ () (check-metafunction m (curry eq? 1))))
        #px"check-metafunction:.*counterexample found after 1 attempt with clause at .*:\\d+:\\d+")
  (test (raised-exn-msg
          exn:fail:contract?
          (check-metafunction m (λ (_) #t) #:attempts 'NaN))
        #rx"check-metafunction: expected")
  (test (raised-exn-msg
         exn:fail:redex:generation-failure?
         (check-metafunction n (λ (_) #t) #:retries 42))
        #rx"check-metafunction: unable .* in 42"))

;; custom generators
(let ()
  (define-language L
    (x variable))
  
  (test
   (generate-term 
    L x_1 0
    #:custom (λ (pat sz i-h acc env att rec def)
               (match pat
                 ['x (values 'x env)]
                 [_ (def acc)])))
   'x)
  (test
   (let/ec k
     (equal?
      (generate-term 
       L (x x) 0
       #:custom (let ([once? #f])
                  (λ (pat sz i-h acc env att rec def)
                    (match pat
                      ['x (if once?
                              (k #f)
                              (begin
                                (set! once? #t)
                                (values 'x env)))]
                      [_ (def acc)]))))
      '(x x)))
   #t)
  
  (test
   (hash-ref
    (let/ec k
      (generate-term 
       L (x (x)) 0
       #:custom (λ (pat sz i-h acc env att rec def)
                  (match pat
                    [(struct binder ('x))
                     (values 'y (hash-set env pat 'y))]
                    [(list (struct binder ('x))) (k env)]
                    [_ (def acc)]))))
    (make-binder 'x))
   'y)
  
  (test
   (generate-term
    L (in-hole hole 7) 0
    #:custom (λ (pat sz i-h acc env att rec def)
               (match pat
                 [`(in-hole hole 7)
                  (rec 'hole #:contractum 7)]
                 [_ (def acc)])))
   7)
  
  (test
   (let/ec k
     (generate-term 
      L any 10
      #:attempt 42
      #:custom (λ (pat sz i-h acc env att rec def) (k (list sz att)))))
   '(10 42))
  
  (test
   (let/ec k
     (generate-term 
      L x 10
      #:custom (λ (pat sz i-h acc env att rec def) 
                 (match pat
                   ['x (rec 7 #:size 0)]
                   [7 (k sz)]
                   [_ (def att)]))))
   0)
  
  (test
   (generate-term
    L (q 7) 0
    #:custom (λ (pat sz i-h acc env att rec def) 
               (match pat
                 ['q (rec '(7 7) #:acc 8)]
                 [7 (values (or acc 7) env)]
                 [_ (def att)])))
   '((8 8) 7)))

;; parse/unparse-pattern
(let-syntax ([test-match (syntax-rules () [(_ p x) (test (match x [p #t] [_ #f]) #t)])])
      (define-language lang (x variable))
      (let ([pattern '((x_1 number) ... 3)])
        (test-match (list 
                     (struct ellipsis 
                             ('... 
                              (list (struct binder ('x_1)) (struct binder ('number)))
                              _
                              (list (struct binder ('number)) (struct binder ('x_1)))))
                     3)
                    (parse-pattern pattern lang 'top-level))
        (test (unparse-pattern (parse-pattern pattern lang 'top-level)) pattern))
      (let ([pattern '((x_1 ..._1 x_2) ..._!_1)])
        (test-match (struct ellipsis 
                            ((struct mismatch (i_1 '..._!_1)) 
                             (list 
                              (struct ellipsis 
                                      ('..._1 
                                       (struct binder ('x_1))
                                       (struct class ('..._1))
                                       (list (struct binder ('x_1)))))
                              (struct binder ('x_2)))
                             _ 
                             (list (struct binder ('x_2)) '..._1 (struct class ('..._1)) (struct binder ('x_1)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (let ([pattern '((name x_1 x_!_2) ...)])
        (test-match (struct ellipsis 
                            ('... `(name x_1 ,(struct mismatch (i_2 'x_!_2))) _ 
                                  (list (struct binder ('x_1)) (struct mismatch (i_2 'x_!_2)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (let ([pattern '((x ...) ..._1)])
        (test-match (struct ellipsis 
                            ('..._1
                             (list 
                              (struct ellipsis 
                                      ('...
                                       (struct binder ('x))
                                       (struct class (c_1))
                                       (list (struct binder ('x))))))
                             _
                             (list (struct class (c_1)) (struct binder ('x)))))
                    (car (parse-pattern pattern lang 'top-level)))
        (test (unparse-pattern (parse-pattern pattern lang 'top-level)) pattern))
      (let ([pattern '((variable_1 ..._!_1) ...)])
        (test-match (struct ellipsis 
                            ('...
                             (list 
                              (struct ellipsis 
                                      ((struct mismatch (i_1 '..._!_1))
                                       (struct binder ('variable_1))
                                       (struct class (c_1))
                                       (list (struct binder ('variable_1))))))
                             _
                             (list (struct class (c_1)) (struct mismatch (i_1 '..._!_1)) (struct binder ('variable_1)))))
                    (car (parse-pattern pattern lang 'grammar)))
        (test (unparse-pattern (parse-pattern pattern lang 'grammar)) pattern))
      (test (parse-pattern '(cross x) lang 'grammar) '(cross x-x))
      (test (parse-pattern '(cross x) lang 'cross) '(cross x))
      (test (parse-pattern 'x lang 'grammar) 'x)
      (test (parse-pattern 'variable lang 'grammar) 'variable))

(let ()
  (define-language lang (x variable))
  (define-syntax test-class-reassignments
    (syntax-rules ()
      [(_ pattern expected)
       (test (to-table (class-reassignments (parse-pattern pattern lang 'top-level)))
             expected)]))
  
  (test-class-reassignments 
   '(x_1 ..._1 x_2 ..._2 x_2 ..._1)
   '((..._2 . ..._1)))
  (test-class-reassignments
   '((x_1 ..._1 x_1 ..._2) (x_2 ..._1 x_2 ..._2) x_3 ..._2)
   '((..._1 . ..._2) (..._2 . ..._2)))
  (test-class-reassignments
   '(x_1 ..._1 x ..._2 x_1 ..._2)
   '((..._1 . ..._2)))
  (test-class-reassignments
   '(x_1 ..._1 x_2 ..._2 (x_1 x_2) ..._3)
   '((..._1 . ..._3) (..._2 . ..._3)))
  (test-class-reassignments
   '((x_1 ..._1) ..._2 x_2 ..._3 (x_1 ..._4 x_2) ..._5)
   '((..._1 . ..._4) (..._2 . ..._5) (..._3 . ..._5)))
  (test-class-reassignments
   '((x_1 ..._1) ..._2 (x_1 ..._3) ..._4 (x_1 ..._5) ..._6)
   '((..._1 . ..._5) (..._2 . ..._6) (..._3 . ..._5) (..._4 . ..._6)))
  (test-class-reassignments
   '(x_1 ..._1 x_1 ..._2 x_2 ..._1 x_2 ..._4 x_2 ..._3)
   '((..._1 . ..._3) (..._2 . ..._3) (..._4 . ..._3)))
  (test 
   (hash-map 
    (class-reassignments (parse-pattern '(x_1 ... x_1 ..._!_1 x_1 ..._1) lang 'top-level)) 
    (λ (_ cls) cls))
   '(..._1 ..._1))
  (test-class-reassignments
   '((3 ..._1) ..._2 (4 ..._1) ..._3)
   '((..._2 . ..._3)))
  (test-class-reassignments
   '(x ..._1 x ..._2 variable ..._2 variable ..._3 variable_1 ..._3 variable_1 ..._4)
   '((..._1 . ..._4) (..._2 . ..._4) (..._3 . ..._4))))

(print-tests-passed 'rg-test.ss)
