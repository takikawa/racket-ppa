#lang racket/base
(require racket/private/set
         racket/private/custom-write
         racket/stream
         racket/serialize
         racket/pretty
         racket/sequence
         racket/unsafe/ops
         (only-in racket/syntax format-symbol)
         (only-in racket/generic exn:fail:support)
         (for-syntax racket/base racket/syntax syntax/for-body))

(provide set seteq seteqv
         weak-set weak-seteq weak-seteqv
         mutable-set mutable-seteq mutable-seteqv
         list->set list->seteq list->seteqv
         list->weak-set list->weak-seteq list->weak-seteqv
         list->mutable-set list->mutable-seteq list->mutable-seteqv
         set-eq? set-eqv? set-equal?
         set-weak? set-mutable? set?
         for/set for/seteq for/seteqv
         for*/set for*/seteq for*/seteqv
         for/weak-set for/weak-seteq for/weak-seteqv
         for*/weak-set for*/weak-seteq for*/weak-seteqv
         for/mutable-set for/mutable-seteq for/mutable-seteqv
         for*/mutable-set for*/mutable-seteq for*/mutable-seteqv

         define-custom-set-types
         make-custom-set-types
         make-custom-set
         make-weak-custom-set
         make-mutable-custom-set

         in-immutable-set
         in-mutable-set
         in-weak-set
         
         chaperone-hash-set
         impersonate-hash-set)

(define (custom-set-empty? s)
  (dprintf "custom-set-empty?\n")
  (hash-empty? (custom-set-table s)))

(define (custom-set-member? s x)
  (dprintf "custom-set-member?\n")
  (set-check-elem 'set-member? s x)
  (hash-ref (custom-set-table s)
            (set-wrap-elem s x)
            #f))

(define (custom-set-count s)
  (dprintf "custom-set-count\n")
  (hash-count (custom-set-table s)))

(define (custom-set=? s1 s2)
  (dprintf "custom-set=?\n")
  (unless (generic-set? s2)
    (raise-argument-error 'set=? "generic-set?" 1 s1 s2))
  (set-check-compatible 'set=? s1 s2)
  (define table1 (custom-set-table s1))
  (define table2 (custom-set-table s2))
  (and (for/and ([k (in-hash-keys table1)])
         (hash-ref table2 k #f))
       (for/and ([k (in-hash-keys table2)])
         (hash-ref table1 k #f))))

(define (custom-subset? s1 s2)
  (dprintf "custom-subset?\n")
  (unless (generic-set? s2)
    (raise-argument-error 'subset? "generic-set?" 1 s1 s2))
  (set-check-compatible 'subset? s1 s2)
  (define table1 (custom-set-table s1))
  (define table2 (custom-set-table s2))
  (for/and ([k (in-hash-keys table1)])
    (hash-ref table2 k #f)))

(define (custom-proper-subset? s1 s2)
  (dprintf "custom-proper-subset?\n")
  (unless (generic-set? s2)
    (raise-argument-error 'proper-subset? "generic-set?" 1 s1 s2))
  (set-check-compatible 'proper-subset? s1 s2)
  (define table1 (custom-set-table s1))
  (define table2 (custom-set-table s2))
  (and (for/and ([k (in-hash-keys table1)])
         (hash-ref table2 k #f))
       (for/or ([k (in-hash-keys table2)])
         (not (hash-ref table1 k #f)))))

(define (custom-set-map s f)
  (dprintf "custom-set-map\n")
  (for/fold ([xs '()]) ([k (in-hash-keys (custom-set-table s))])
    (cons (f (set-unwrap-key s k)) xs)))

(define (custom-set-for-each s f)
  (dprintf "custom-set-for-each\n")
  (for ([k (in-hash-keys (custom-set-table s))])
    (f (set-unwrap-key s k))))

(define (custom-set-copy s)
  (dprintf "custom-set-copy\n")
  (update-custom-set-table s (hash-copy (custom-set-table s))))

(define (custom-set->list s)
  (dprintf "custom-set->list\n")
  (for/fold ([xs '()]) ([k (in-hash-keys (custom-set-table s))])
    (cons (set-unwrap-key s k) xs)))

(define (custom-set->stream s)
  (dprintf "custom-set->stream\n")
  (sequence->stream (custom-in-set s)))

(define (custom-set-first s)
  (dprintf "custom-set-first\n")
  (define table (custom-set-table s))
  (define i (hash-iterate-first table))
  (unless i
    (raise-argument-error 'set-first "(and/c generic-set? (not/c set-empty?))" s))
  (set-unwrap-key s (hash-iterate-key table i)))

(define (custom-set-rest s)
  (dprintf "custom-set-rest\n")
  (define table (custom-set-table s))
  (define i (hash-iterate-first table))
  (unless i
    (raise-argument-error 'set-rest "(and/c generic-set? (not/c set-empty?))" s))
  (update-custom-set-table s (hash-remove table (hash-iterate-key table i))))

(define (custom-set-add s x)
  (dprintf "custom-set-add\n")
  (set-check-elem 'set-add s x)
  (update-custom-set-table
   s
   (hash-set (custom-set-table s) (set-wrap-elem s x) #t)))

(define (custom-set-remove s x)
  (dprintf "custom-set-remove\n")
  (set-check-elem 'set-remove s x)
  (update-custom-set-table
   s
   (hash-remove (custom-set-table s) (set-wrap-elem s x))))

(define (custom-set-copy-clear s)
  (dprintf "custom-set-copy-clear\n")
  (update-custom-set-table s (hash-copy-clear (custom-set-table s))))

(define (custom-set-clear s)
  (dprintf "custom-set-clear\n")
  (update-custom-set-table s (hash-clear (custom-set-table s))))

(define (choose-immutable who better? set0 sets)
  (for/fold ([largest set0]) ([s (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s)
      (apply raise-argument-error who "generic-set?" i set0 sets))
    (set-check-compatible who set0 s)
    (if (and (immutable? (custom-set-table s))
             (better? (hash-count (custom-set-table s))
                      (hash-count (custom-set-table largest))))
        s
        largest)))

(define (choose-largest-immutable who set0 sets)
  (choose-immutable who > set0 sets))

(define (choose-smallest-immutable who set0 sets)
  (choose-immutable who < set0 sets))

(define (custom-set-union s . sets)
  (dprintf "custom-set-union\n")
  (define largest-immutable
    (choose-largest-immutable 'set-union s sets))
  (update-custom-set-table
   s
   (for/fold
       ([table (custom-set-table largest-immutable)])
       ([s2 (in-list (cons s sets))]
        #:unless (eq? s2 largest-immutable))
     (for/fold ([table table]) ([x (in-hash-keys (custom-set-table s2))])
       (hash-set table x #t)))))

(define (custom-set-symmetric-difference s . sets)
  (dprintf "custom-set-symmetric-difference\n")
  (define largest-immutable
    (choose-largest-immutable 'set-symmetric-difference s sets))
  (update-custom-set-table
   s
   (for/fold
       ([table (custom-set-table largest-immutable)])
       ([s2 (in-list (remove largest-immutable (cons s sets)))])
     (for/fold ([table table]) ([x (in-hash-keys (custom-set-table s2))])
       (if (hash-ref table x #f)
           (hash-remove table x)
           (hash-set table x #t))))))

(define (custom-set-intersect s . sets)
  (dprintf "custom-set-intersect\n")
  (define smallest-immutable
    (choose-smallest-immutable 'set-intersect s sets))
  (define all-sets (cons s sets))
  (define (keep? k)
    (for/and ([s2 (in-list all-sets)]
              #:unless (eq? s2 smallest-immutable))
      (hash-ref (custom-set-table s2) k #f)))
  (define smallest-table (custom-set-table smallest-immutable))
  (update-custom-set-table
   s
   (for/fold
       ([table smallest-table])
       ([k (in-hash-keys smallest-table)]
        #:unless (keep? k))
     (hash-remove table k))))

(define (custom-set-subtract s . sets)
  (dprintf "custom-set-subtract\n")
  (for ([s2 (in-list sets)] [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-subtract "generic-set?" i s sets))
    (set-check-compatible 'set-subtract s s2))
  (define (remove? k)
    (for/or ([s2 (in-list sets)])
      (hash-ref (custom-set-table s2) k #f)))
  (define initial-table (custom-set-table s))
  (update-custom-set-table
   s
   (for/fold
       ([table initial-table])
       ([k (in-hash-keys initial-table)]
        #:when (remove? k))
     (hash-remove table k))))

(define (custom-set-add! s x)
  (dprintf "custom-set-add!\n")
  (set-check-elem 'set-add! s x)
  (hash-set! (custom-set-table s) (set-wrap-elem s x) #t))

(define (custom-set-remove! s x)
  (dprintf "custom-set-remove!\n")
  (set-check-elem 'set-remove! s x)
  (hash-remove! (custom-set-table s) (set-wrap-elem s x)))

(define (custom-set-clear! s)
  (dprintf "custom-set-clear!\n")
  (hash-clear! (custom-set-table s)))

(define (custom-set-union! s . sets)
  (dprintf "custom-set-union!\n")
  (define table (custom-set-table s))
  (for ([s2 (in-list sets)]
        [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-union! "generic-set?" i s sets))
    (set-check-compatible 'set-union! s s2)
    (for ([x (in-hash-keys (custom-set-table s2))])
      (hash-set! table x #t))))

(define (custom-set-symmetric-difference! s . sets)
  (dprintf "custom-set-symmetric-difference!\n")
  (define table (custom-set-table s))
  (for ([s2 (in-list sets)]
        [i (in-naturals 1)])
    (unless (generic-set? s2)
      (apply raise-argument-error 'set-symmetric-difference! "generic-set?" i s sets))
    (set-check-compatible 'set-symmetric-difference! s s2)
    (for ([x (in-hash-keys (custom-set-table s2))])
      (if (hash-ref table x #f)
          (hash-remove! table x)
          (hash-set! table x #t)))))

(define (custom-set-intersect! s . sets)
  (dprintf "custom-set-intersect!\n")
  (define tables
    (for/list ([s2 (in-list sets)] [i (in-naturals 1)])
      (unless (generic-set? s2)
        (apply raise-argument-error 'set-intersect! "generic-set?" i s sets))
      (set-check-compatible 'set-intersect! s s2)
      (custom-set-table s2)))
  (define (keep? k)
    (for/and ([table (in-list tables)])
      (hash-ref table k #f)))
  (define table (custom-set-table s))
  (define to-remove
    (for/list ([k (in-hash-keys table)]
               #:unless (keep? k))
      k))
  (for ([k (in-list to-remove)])
    (hash-remove! table k)))

(define (custom-set-subtract! s . sets)
  (dprintf "custom-set-subtract!\n")
  (define tables
    (for/list ([s2 (in-list sets)] [i (in-naturals 1)])
      (unless (generic-set? s2)
        (apply raise-argument-error 'set-subtract! "generic-set?" i s sets))
      (set-check-compatible 'set-subtract! s s2)
      (custom-set-table s2)))
  (define (remove? k)
    (for/or ([table (in-list tables)])
      (hash-ref table k #f)))
  (define table (custom-set-table s))
  (define to-remove
    (for/list ([k (in-hash-keys table)]
               #:when (remove? k))
      k))
  (for ([k (in-list to-remove)])
     (hash-remove! table k)))

(define (set-wrap-elem d x)
  (define spec (custom-set-spec d))
  (wrap-elem spec x))

(define (wrap-elem spec x)
  (cond
    [spec
     (define wrap (custom-spec-wrap spec))
     (define intern (custom-spec-intern spec))
     (ephemeron-value
      (hash-ref! intern x
                 (lambda ()
                   (make-ephemeron x (wrap x)))))]
    [else x]))

(define (set-unwrap-key d k)
  (define spec (custom-set-spec d))
  (unwrap-key spec k))

(define (unwrap-key spec k)
  (if spec (custom-elem-contents k) k))

(define (set-check-elem who d x)
  (define spec (custom-set-spec d))
  (check-elem who spec x))

(define (check-elem who spec x)
  (when spec
    (define elem? (custom-spec-elem? spec))
    (unless (elem? x)
      (raise-argument-error who (format "~a" elem?) x))))

(define (update-custom-set-table s table)
  (cond
    [(immutable? table) (immutable-custom-set (custom-set-spec s) table)]
    [(hash-weak? table) (weak-custom-set (custom-set-spec s) table)]
    [else (mutable-custom-set (custom-set-spec s) table)]))

(define (chaperone-hash-set s inject-proc add-proc shrink-proc extract-proc . clear-proc+props)
  (define-values (clear-proc equal-key-proc prop-args)
    (check-chap/imp-args #f s inject-proc add-proc shrink-proc extract-proc clear-proc+props))
  (define (check-it who original new)
    (unless (chaperone-of? new original)
      (error 'chaperone-hash-set
             "~s did not return a chaperone of ~e, got ~e"
             who original new))
    new)

  (add-impersonator-properties
   (if inject-proc
       (chap-or-imp-hash-set s
                             chaperone-hash
                             (λ (ele) (check-it 'in-proc ele (inject-proc s ele)))
                             (λ (ele) (check-it 'add-proc ele (add-proc s ele)))
                             (λ (ele) (check-it 'shrink-proc ele (shrink-proc s ele)))
                             (λ (ele) (check-it 'extract-proc ele (extract-proc s ele)))
                             (and clear-proc (λ () (clear-proc s)))
                             (λ (ele) (equal-key-proc s ele)))
       s)
   prop-args))

(define (impersonate-hash-set s inject-proc add-proc shrink-proc extract-proc . clear-proc+props)
  (define-values (clear-proc equal-key-proc prop-args)
    (check-chap/imp-args #t s inject-proc add-proc shrink-proc extract-proc clear-proc+props))
  (add-impersonator-properties
   (if inject-proc
       (chap-or-imp-hash-set s
                             impersonate-hash
                             (λ (ele) (inject-proc s ele))
                             (λ (ele) (add-proc s ele))
                             (λ (ele) (shrink-proc s ele))
                             (λ (ele) (extract-proc s ele))
                             (and clear-proc (λ () (clear-proc s)))
                             (λ (ele) (equal-key-proc s ele)))
       s)
   prop-args))

(define (chap-or-imp-hash-set s c-or-i-hash
                              inject-proc add-proc shrink-proc extract-proc
                              clear-proc equal-key-proc)
  (update-custom-set-table
   s
   (cond
     [(custom-set-spec s)
      (define rewrap (custom-spec-wrap (custom-set-spec s)))
      (c-or-i-hash
       (custom-set-table s)
       (λ (hash key) (values (rewrap (inject-proc (custom-elem-contents key)))
                             (λ (hash key val) val)))
       (λ (hash key val) (values (rewrap (add-proc (custom-elem-contents key))) val))
       (λ (hash key) (rewrap (shrink-proc (custom-elem-contents key))))
       (λ (hash key) (rewrap (extract-proc (custom-elem-contents key))))
       (λ (hash) (clear-proc))
       (λ (hash key) (rewrap (equal-key-proc (custom-elem-contents key)))))]
     [else
      (c-or-i-hash
       (custom-set-table s)
       (λ (hash key) (values (inject-proc key) (λ (hash key val) val)))
       (λ (hash key val) (values (add-proc key) val))
       (λ (hash key) (shrink-proc key))
       (λ (hash key) (extract-proc key))
       (λ (hash) (clear-proc))
       (λ (hash key) (equal-key-proc key)))])))

(define (add-impersonator-properties without-props prop-args)
  (cond
    [(null? prop-args) without-props]
    [(immutable-custom-set? without-props)
     (apply chaperone-struct without-props struct:immutable-custom-set prop-args)]
    [(weak-custom-set? without-props)
     (apply chaperone-struct without-props struct:weak-custom-set prop-args)]
    [else
     (apply chaperone-struct without-props struct:mutable-custom-set prop-args)]))

(define (check-chap/imp-args impersonate? s
                             inject-proc add-proc shrink-proc extract-proc
                             clear-proc+equal-key-proc+props)
  (define who (if impersonate? 'impersonate-hash-set 'chaperone-hash-set))
  (unless (if impersonate?
              (or (set-mutable? s) (set-weak? s))
              (or (set? s) (set-mutable? s) (set-weak? s)))
    (apply raise-argument-error
           who
           (format "~s"
                   (if impersonate?
                       '(or/c set-mutable? set-weak?)
                       '(or/c set? set-mutable? set-weak?)))
           0 s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (unless (or (not inject-proc)
              (and (procedure? inject-proc)
                   (procedure-arity-includes? inject-proc 2)))
    (apply raise-argument-error
           who
           "(or/c #f (procedure-arity-includes/c 2))"
           1 s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (unless (or (not add-proc)
              (and (procedure? add-proc)
                   (procedure-arity-includes? add-proc 2)))
    (apply raise-argument-error
           who
           "(or/c #f (procedure-arity-includes/c 2))"
           2 s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (unless (or (not shrink-proc)
              (and (procedure? shrink-proc)
                   (procedure-arity-includes? shrink-proc 2)))
    (apply raise-argument-error
           who
           "(or/c #f (procedure-arity-includes/c 2))"
           3 s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (unless (or (not extract-proc)
              (and (procedure? extract-proc)
                   (procedure-arity-includes? extract-proc 2)))
    (apply raise-argument-error
           who
           "(or/c #f (procedure-arity-includes/c 2))"
           4 s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (unless (null? clear-proc+equal-key-proc+props)
    (unless (or (not (car clear-proc+equal-key-proc+props))
                (and (procedure? (car clear-proc+equal-key-proc+props))
                     (procedure-arity-includes? (car clear-proc+equal-key-proc+props) 1))
                (impersonator-property? (car clear-proc+equal-key-proc+props)))
      (apply raise-argument-error
             who
             (format "~s" `(or/c #f
                                 (procedure-arity-includes/c 1)
                                 impersonator-property?))
             5
             s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props)))
  (define-values (num-supplied-procs clear-proc equal-key-proc args)
    (cond
      [(null? clear-proc+equal-key-proc+props) (values 0 #f #f '())]
      [(impersonator-property? (car clear-proc+equal-key-proc+props))
       (values 0 #f #f clear-proc+equal-key-proc+props)]
      [else
       (define clear-proc (car clear-proc+equal-key-proc+props))
       (define equal-key-proc+props (cdr clear-proc+equal-key-proc+props))
       (cond
         [(null? equal-key-proc+props) (values 1 clear-proc #f '())]
         [(impersonator-property? (car equal-key-proc+props))
          (values 1 clear-proc #f equal-key-proc+props)]
         [else
          (values 2 clear-proc (car equal-key-proc+props) (cdr equal-key-proc+props))])]))
  (unless (or (not equal-key-proc)
              (and (procedure? equal-key-proc)
                   (procedure-arity-includes? equal-key-proc 2)))
    (apply raise-argument-error
           who
           "(or/c #f (procedure-arity-includes/c 1))"
           (+ 4 num-supplied-procs)
           s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props))
  (for ([ele (in-list args)]
        [i (in-naturals)]
        #:when (even? i))
    (unless (impersonator-property? ele)
      (apply raise-argument-error
             who
             "impersonator-property?"
             (+ i num-supplied-procs 4)
             s inject-proc add-proc shrink-proc extract-proc clear-proc+equal-key-proc+props)))
  (when (or (not inject-proc) (not add-proc) (not shrink-proc) (not extract-proc))
    (unless (and (not inject-proc) (not add-proc) (not shrink-proc) (not extract-proc)
                 (not equal-key-proc) (not clear-proc))
      (raise-arguments-error who
                             (string-append
                              "if one of inject-proc, add-proc, shrink-proc"
                              " or extract-proc is #f, they must all be and the"
                              " equal-key-proc and clear-proc must also be")
                             "inject-proc" inject-proc
                             "add-proc" add-proc
                             "shrink-proc" shrink-proc
                             "extract-proc" extract-proc
                             "equal-key-proc" equal-key-proc
                             "clear-proc" clear-proc)))
  (unless inject-proc
    (when (null? args)
      (raise-arguments-error
       who
       (string-append
        "when inject-proc, add-proc, shrink-proc, and extract-proc are #f,"
        " at least one property must be supplied"))))
  (values clear-proc
          (or equal-key-proc (λ (s e) e))
          args))
  
(define (set-check-compatible name s1 s2)
  (define spec (custom-set-spec s1))
  (unless (and (custom-set? s2)
               (eq? (custom-set-spec s2) spec)
               (or spec
                   (hash-compatible? (custom-set-table s1)
                                     (custom-set-table s2))))
    (raise-arguments-error
     name
     "set arguments have incompatible equivalence predicates"
     "first set" s1
     "incompatible set" s2)))

(define (hash-compatible? x y)
  (cond
    [(hash-equal? x) (hash-equal? y)]
    [(hash-eqv? x) (hash-eqv? y)]
    [(hash-eq? x) (hash-eq? y)]))

(define (write-custom-set s port mode)
  (cond [(custom-set-spec s)
         (define table (custom-set-table s))
         (define key-str
           (cond [(immutable? table) ""]
                 [(hash-weak? table) "weak-"]
                 [else "mutable-"]))
         (fprintf port "#<~acustom-set>" key-str)]
        [else (write-hash-set s port mode)]))

(define write-hash-set
  (make-constructor-style-printer
   (lambda (s)
     (define table (custom-set-table s))
     (define key-str
       (cond [(immutable? table) ""]
             [(hash-weak? table) "weak-"]
             [else "mutable-"]))
     (cond [(custom-set-spec s)
            (string-append key-str "custom-set")]
           [else
            (define cmp-str
              (cond [(hash-equal? table) "set"]
                    [(hash-eqv? table) "seteqv"]
                    [(hash-eq? table) "seteq"]))
            (string-append key-str cmp-str)]))
   (lambda (s) (hash-keys (custom-set-table s)))))

(define (custom-in-set s)
  (define keys (in-hash-keys (custom-set-table s)))
  (if (custom-set-spec s)
      (sequence-map custom-elem-contents keys)
      keys))

(define (custom-in-set/checked s)
  (unless (custom-set? s)
    (raise (exn:fail:contract (format "not a hash set: ~a" s)
             (current-continuation-marks))))
  (custom-in-set s))

(define (set-immutable? s) (set? s))
;; creates an new id with the given id and format str
(define-for-syntax (mk-id id fmt-str)
  (datum->syntax id (string->symbol (format fmt-str (syntax->datum id)))))

;; raise-custom-set-exn : Any Symbol -> Exn
;; Raises exception reporting that `s` is not a custom-set of type `expected-set-type`
(define (raise-custom-set-exn s expected-set-type)
  (raise
   (exn:fail:contract
    (if (custom-set? s)
        (format "wrong kind of hash set, expected ~a, got: ~a\n" expected-set-type s)
        (format "not a hash set: ~a" s))
    (current-continuation-marks))))

(define-syntax (define-in-set-sequence-syntax stx)
  (syntax-case stx (set-type:)
    [(_ set-type: SETTYPE)
     (with-syntax
      ([IN-SET-NAME (mk-id #'SETTYPE "in-~a-set")]
       [-first (mk-id #'SETTYPE "unsafe-~a-hash-iterate-first")]
       [-next (mk-id #'SETTYPE "unsafe-~a-hash-iterate-next")]
       [-get (mk-id #'SETTYPE "unsafe-~a-hash-iterate-key")]
       [-test? (mk-id #'SETTYPE "set-~a?")])
      #'(define-sequence-syntax IN-SET-NAME
          (lambda () #'custom-in-set/checked)
          (lambda (stx)
            (syntax-case stx ()
              [[(id) (_ set-expr)]
               (for-clause-syntax-protect
                #'[(id)
                   (:do-in
                    ;;outer bindings
                    ([(xs HT fn) (let ([xs set-expr])
                                   (if (and (custom-set? xs) (-test? xs))
                                       (values
                                        #f
                                        (custom-set-table xs)
                                        (if (custom-set-spec xs)
                                            custom-elem-contents
                                            (lambda (x) x)))
                                       (values xs #f #f)))])
                    ;; outer check
                    (unless HT (raise-custom-set-exn xs 'SETTYPE))
                    ;; loop bindings
                    ([i (-first HT)])
                    ;; pos check
                    i
                    ;; inner bindings
                    ([(id) (fn (-get HT i))])
                    ;; pre guard
                    #t
                    ;; post guard
                    #t
                    ;; loop args
                    ((-next HT i)))])]
              [_ #f]))))]))
(define-in-set-sequence-syntax set-type: immutable)
(define-in-set-sequence-syntax set-type: mutable)
(define-in-set-sequence-syntax set-type: weak) 

(struct custom-elem [contents] #:transparent)

(struct custom-spec [elem? wrap intern])

(serializable-struct custom-set [spec table]
  #:property prop:sequence custom-in-set
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define write-proc write-custom-set)]
  #:methods gen:equal+hash
  [(define (equal-proc x y rec)
     (and (eq? (custom-set-spec x)
               (custom-set-spec y))
          (rec (custom-set-table x)
               (custom-set-table y))))
   (define (hash-proc x rec)
     (+ (eq-hash-code (custom-set-spec x))
        (rec (custom-set-table x))
        custom-set-constant))
   (define (hash2-proc x rec)
     (rec (custom-set-table x)))])

(define custom-set-constant
  (equal-hash-code "hash code for a set based on a hash table"))

(define (((mk-not-allowed #:immut [immut #t]) method-name) s . args)
  (raise
   (exn:fail:support
    (format 
     (string-append "~a:\n"
                    "expected: ~a\n"
                    "given ~a: ~e\n"
                    "argument position: 1st")
     method-name
     (if immut "(not/c set-mutable?)" "set-mutable?")
     (if immut "mutable set" "immutable set")
     s)
    (current-continuation-marks))))
(define mk-not-allowed/immut (mk-not-allowed #:immut #f))
(define mk-not-allowed/mut (mk-not-allowed #:immut #t))

(serializable-struct immutable-custom-set custom-set []
  #:methods gen:stream
  [(define stream-empty? custom-set-empty?)
   (define stream-first custom-set-first)
   (define stream-rest custom-set-rest)]
  #:methods gen:set
  [(define set-empty? custom-set-empty?)
   (define set-member? custom-set-member?)
   (define set-count custom-set-count)
   (define set=? custom-set=?)
   (define subset? custom-subset?)
   (define proper-subset? custom-proper-subset?)
   (define set-map custom-set-map)
   (define set-for-each custom-set-for-each)
   (define set-copy custom-set-copy)
   (define set-copy-clear custom-set-copy-clear)
   (define set->list custom-set->list)
   (define set->stream custom-set->stream)
   (define in-set custom-in-set)
   (define set-first custom-set-first)
   (define set-rest custom-set-rest)
   (define set-add custom-set-add)
   (define set-add! (mk-not-allowed/immut 'set-add!))
   (define set-remove custom-set-remove)
   (define set-remove! (mk-not-allowed/immut 'set-remove!))
   (define set-clear custom-set-clear)
   (define set-clear! (mk-not-allowed/immut 'set-clear!))
   (define set-union custom-set-union)
   (define set-union! (mk-not-allowed/immut 'set-union!))
   (define set-intersect custom-set-intersect)
   (define set-intersect! (mk-not-allowed/immut 'set-intersect!))
   (define set-subtract custom-set-subtract)
   (define set-subtract! (mk-not-allowed/immut 'set-subtract!))
   (define set-symmetric-difference custom-set-symmetric-difference)
   (define set-symmetric-difference! (mk-not-allowed/immut 'set-symmetric-difference!))]
  )

(serializable-struct imperative-custom-set custom-set []
  #:methods gen:set
  [(define set-empty? custom-set-empty?)
   (define set-member? custom-set-member?)
   (define set-count custom-set-count)
   (define set=? custom-set=?)
   (define subset? custom-subset?)
   (define proper-subset? custom-proper-subset?)
   (define set-map custom-set-map)
   (define set-for-each custom-set-for-each)
   (define set-copy custom-set-copy)
   (define set-copy-clear custom-set-copy-clear)
   (define set->list custom-set->list)
   (define set->stream custom-set->stream)
   (define in-set custom-in-set)
   (define set-first custom-set-first)
   (define set-rest (mk-not-allowed/mut 'set-rest))
   (define set-add (mk-not-allowed/mut 'set-add))
   (define set-add! custom-set-add!)
   (define set-remove (mk-not-allowed/mut 'set-remove))
   (define set-remove! custom-set-remove!)
   (define set-clear (mk-not-allowed/mut 'set-clear))
   (define set-clear! custom-set-clear!)
   (define set-union (mk-not-allowed/mut 'set-union))
   (define set-union! custom-set-union!)
   (define set-intersect (mk-not-allowed/mut 'set-intersect))
   (define set-intersect! custom-set-intersect!)
   (define set-subtract (mk-not-allowed/mut 'set-subtract))
   (define set-subtract! custom-set-subtract!)
   (define set-symmetric-difference (mk-not-allowed/mut 'set-symmetric-difference))
   (define set-symmetric-difference! custom-set-symmetric-difference!)])

(serializable-struct weak-custom-set imperative-custom-set [])

(serializable-struct mutable-custom-set imperative-custom-set [])

(define-syntax (define-custom-set-types stx)
  (parameterize ([current-syntax-context stx])
    (define-values (base-id args-stx)
      (syntax-case stx ()
        [(_ name #:elem? elem? =? hc1 hc2)
         (values #'name #'(#:elem? elem? =? hc1 hc2))]
        [(_ name #:elem? elem? =? hc1)
         (values #'name #'(#:elem? elem? =? hc1))]
        [(_ name #:elem? elem? =?)
         (values #'name #'(#:elem? elem? =?))]
        [(_ name =? hc1 hc2)
         (values #'name #'(=? hc1 hc2))]
        [(_ name =? hc1)
         (values #'name #'(=? hc1))]
        [(_ name =?)
         (values #'name #'(=?))]))
    (unless (identifier? base-id)
      (wrong-syntax base-id "expected an identifier"))
    (define (id fmt) (format-id base-id fmt base-id))
    (define/with-syntax name (id "~a"))
    (define/with-syntax name? (id "~a?"))
    (define/with-syntax weak-name? (id "weak-~a?"))
    (define/with-syntax mutable-name? (id "mutable-~a?"))
    (define/with-syntax immutable-name? (id "immutable-~a?"))
    (define/with-syntax make-weak-name (id "make-weak-~a"))
    (define/with-syntax make-mutable-name (id "make-mutable-~a"))
    (define/with-syntax make-immutable-name (id "make-immutable-~a"))
    (define/with-syntax args args-stx)
    #'(define-values (name?
                      weak-name?
                      mutable-name?
                      immutable-name?
                      make-weak-name
                      make-mutable-name
                      make-immutable-name)
        (make-custom-set-types #:for 'define-custom-set-types
                                #:name 'name
                                . args))))

(define (make-custom-set-types =? [hc1 default-hc] [hc2 default-hc]
                                #:elem? [elem? default-pred]
                                #:for [who 'make-custom-set-types]
                                #:name [name 'custom-set])
  (define spec (make-custom-spec who elem? =? hc1 hc2))
  (define (sym fmt) (format-symbol fmt name))
  (values (custom-set-predicate spec (sym "~a?"))
          (weak-custom-set-predicate spec (sym "weak-~a?"))
          (mutable-custom-set-predicate spec (sym "mutable-~a?"))
          (immutable-custom-set-predicate spec (sym "immutable-~a?"))
          (weak-custom-set-maker spec (sym "make-weak-~a"))
          (mutable-custom-set-maker spec (sym "make-mutable-~a"))
          (immutable-custom-set-maker spec (sym "make-immutable-~a"))))

(define (make-mutable-custom-set =? [hc1 default-hc] [hc2 default-hc]
                          #:elem? [elem? default-pred])
  (define spec (make-custom-spec 'make-custom-set elem? =? hc1 hc2))
  (define make (mutable-custom-set-maker spec 'make))
  (make))

(define (make-weak-custom-set =? [hc1 default-hc] [hc2 default-hc]
                          #:elem? [elem? default-pred])
  (define spec (make-custom-spec 'make-custom-set elem? =? hc1 hc2))
  (define make (weak-custom-set-maker spec 'make))
  (make))

(define (make-custom-set =? [hc1 default-hc] [hc2 default-hc]
                          #:elem? [elem? default-pred])
  (define spec (make-custom-spec 'make-custom-set elem? =? hc1 hc2))
  (define make (immutable-custom-set-maker spec 'make))
  (make))

(define (make-custom-spec who elem? =? hc1 hc2)
  (check-arities who =? 2 3)
  (check-arities who hc1 1 2)
  (check-arities who hc2 1 2)
  (check-arity who elem? 1)
  (struct wrapped-elem custom-elem []
    #:methods gen:equal+hash
    [(define equal-proc
       (if (procedure-arity-includes? =? 3)
           (lambda (a b f)
             (=? (custom-elem-contents a)
                 (custom-elem-contents b)
                 f))
           (lambda (a b f)
             (=? (custom-elem-contents a)
                 (custom-elem-contents b)))))
     (define hash-proc
       (if (procedure-arity-includes? hc1 2)
           (lambda (a f)
             (hc1 (custom-elem-contents a) f))
           (lambda (a f)
             (hc1 (custom-elem-contents a)))))
     (define hash2-proc
       (if (procedure-arity-includes? hc2 2)
           (lambda (a f)
             (hc2 (custom-elem-contents a) f))
           (lambda (a f)
             (hc2 (custom-elem-contents a)))))])
  (custom-spec elem? wrapped-elem (make-weak-hasheq)))

(define (default-hc x f) 1)
(define (default-pred x) #t)

(define (check-arities who f a b)
  (unless (and (procedure? f)
               (or (procedure-arity-includes? f a)
                   (procedure-arity-includes? f b)))
    (raise-argument-error who (arities-string a b) f)))

(define (check-arity who f a)
  (unless (and (procedure? f)
               (procedure-arity-includes? f a))
    (raise-argument-error who (arity-string a) f)))

(define (arities-string a b)
  (format "(or/c ~a ~a)" (arity-string a) (arity-string b)))

(define (arity-string a)
  (format "(procedure-arity-includes/c ~a)" a))

(define (custom-set-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (custom-set? x)
         (eq? (custom-set-spec x) spec)))
  (procedure-rename proc name))

(define (weak-custom-set-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (weak-custom-set? x)
         (eq? (custom-set-spec x) spec)))
  (procedure-rename proc name))

(define (mutable-custom-set-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (mutable-custom-set? x)
         (eq? (custom-set-spec x) spec)))
  (procedure-rename proc name))

(define (immutable-custom-set-predicate spec name)
  (define (proc x)
    (dprintf "~a\n" name)
    (and (immutable-custom-set? x)
         (eq? (custom-set-spec x) spec)))
  (procedure-rename proc name))

(define (immutable-custom-set-maker spec name)
  (define (proc [st '()])
    (unless (stream? st)
      (raise-argument-error name "stream?" st))
    (dprintf "~a\n" name)
    (define table
      (for/fold ([table (make-immutable-hash)]) ([x (in-stream st)])
        (check-elem name spec x)
        (hash-set table (wrap-elem spec x) #t)))
    (immutable-custom-set spec table))
  (procedure-rename proc name))

(define (imperative-custom-set-maker spec name make-table make-set)
  (define (proc [st '()])
    (unless (stream? st)
      (raise-argument-error name "stream?" st))
    (dprintf "~a\n" name)
    (define table (make-table))
    (for ([x (in-stream st)])
      (check-elem name spec x)
      (hash-set! table (wrap-elem spec x) #t))
    (make-set spec table))
  (procedure-rename proc name))

(define (mutable-custom-set-maker spec name)
  (imperative-custom-set-maker spec name make-hash mutable-custom-set))

(define (weak-custom-set-maker spec name)
  (imperative-custom-set-maker spec name make-weak-hash weak-custom-set))

(define dprintf void)

(define (make-immutable-set spec make-table st)
  (define table
    (for/fold ([table (make-table)]) ([x (in-stream st)])
      (hash-set table (wrap-elem spec x) #t)))
  (immutable-custom-set spec table))

(define (make-imperative-set spec make-table make-set st)
  (define table (make-table))
  (for ([x (in-stream st)])
    (hash-set! table (wrap-elem spec x) #t))
  (make-set spec table))

(define (make-mutable-set spec make-table st)
  (make-imperative-set spec make-table mutable-custom-set st))

(define (make-weak-set spec make-table st)
  (make-imperative-set spec make-table weak-custom-set st))

(define (list->set xs)
  (dprintf "list->set\n")
  (make-immutable-set #f make-immutable-hash xs))
(define (list->seteq xs)
  (dprintf "list->seteq\n")
  (make-immutable-set #f make-immutable-hasheq xs))
(define (list->seteqv xs)
  (dprintf "list->seteqv\n")
  (make-immutable-set #f make-immutable-hasheqv xs))
(define (list->weak-set xs)
  (dprintf "list->weak-set\n")
  (make-weak-set #f make-weak-hash xs))
(define (list->weak-seteq xs)
  (dprintf "list->weak-seteq\n")
  (make-weak-set #f make-weak-hasheq xs))
(define (list->weak-seteqv xs)
  (dprintf "list->weak-seteqv\n")
  (make-weak-set #f make-weak-hasheqv xs))
(define (list->mutable-set xs)
  (dprintf "list->mutable-set\n")
  (make-mutable-set #f make-hash xs))
(define (list->mutable-seteq xs)
  (dprintf "list->mutable-seteq\n")
  (make-mutable-set #f make-hasheq xs))
(define (list->mutable-seteqv xs)
  (dprintf "list->mutable-seteqv\n")
  (make-mutable-set #f make-hasheqv xs))

(define (set . xs)
  (dprintf "set\n")
  (list->set xs))
(define (seteq . xs)
  (dprintf "seteq\n")
  (list->seteq xs))
(define (seteqv . xs)
  (dprintf "seteqv\n")
  (list->seteqv xs))
(define (weak-set . xs)
  (dprintf "weak-set\n")
  (list->weak-set xs))
(define (weak-seteq . xs)
  (dprintf "weak-seteq\n")
  (list->weak-seteq xs))
(define (weak-seteqv . xs)
  (dprintf "weak-seteqv\n")
  (list->weak-seteqv xs))
(define (mutable-set . xs)
  (dprintf "mutable-set\n")
  (list->mutable-set xs))
(define (mutable-seteq . xs)
  (dprintf "mutable-seteq\n")
  (list->mutable-seteq xs))
(define (mutable-seteqv . xs)
  (dprintf "mutable-seteqv\n")
  (list->mutable-seteqv xs))

(define (set-eq? x)
  (dprintf "set-eq?\n")
  (and (custom-set? x) (hash-eq? (custom-set-table x))))
(define (set-eqv? x)
  (dprintf "set-eqv?\n")
  (and (custom-set? x) (hash-eqv? (custom-set-table x))))
(define (set-equal? x)
  (dprintf "set-equal?\n")
  (and (custom-set? x) (hash-equal? (custom-set-table x))))

(define (set? x)
  (dprintf "set?\n")
  (immutable-custom-set? x))
(define (set-mutable? x)
  (dprintf "set-mutable?\n")
  (mutable-custom-set? x))
(define (set-weak? x)
  (dprintf "set-weak?\n")
  (weak-custom-set? x))

(begin-for-syntax

  (define (immutable-for for-id table-id)
    (with-syntax ([for_/fold/derived for-id]
                  [make-table table-id])
      (lambda (stx)
        (syntax-case stx ()
          [(form clauses body ... expr)
           (with-syntax ([original stx]
                         [((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... expr))])
             (syntax-protect
              #'(immutable-custom-set
                 (begin0 #f (dprintf "~a\n" 'form))
                 (for_/fold/derived original ([table (make-table)]) clauses
                   pre-body ...
                   (hash-set table (let () post-body ...) #t)))))]))))

  (define (immutable-fors table-id)
    (values (immutable-for #'for/fold/derived table-id)
            (immutable-for #'for*/fold/derived table-id)))

  (define (imperative-for for-id table-id set-id)
    (with-syntax ([for_/fold/derived for-id]
                  [make-set set-id]
                  [make-table table-id])
      (lambda (stx)
        (syntax-case stx ()
          [(form clauses body ... expr)
           (with-syntax ([original stx]
                         [((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... expr))])
             (syntax-protect
              #'(let ([table (make-table)])
                  (dprintf "~a\n" 'form)
                  (for_/fold/derived original () clauses
                    pre-body ...
                    (hash-set! table (let () post-body ...) #t)
                    (values))
                  (make-set #f table))))]))))

  (define (imperative-fors table-id set-id)
    (values (imperative-for #'for/fold/derived table-id set-id)
            (imperative-for #'for*/fold/derived table-id set-id)))

  (define (mutable-fors table-id)
    (imperative-fors table-id #'mutable-custom-set))
  (define (weak-fors table-id)
    (imperative-fors table-id #'weak-custom-set)))

(define-syntaxes (for/set for*/set)
  (immutable-fors #'make-immutable-hash))
(define-syntaxes (for/seteq for*/seteq)
  (immutable-fors #'make-immutable-hasheq))
(define-syntaxes (for/seteqv for*/seteqv)
  (immutable-fors #'make-immutable-hasheqv))

(define-syntaxes (for/weak-set for*/weak-set)
  (weak-fors #'make-weak-hash))
(define-syntaxes (for/weak-seteq for*/weak-seteq)
  (weak-fors #'make-weak-hasheq))
(define-syntaxes (for/weak-seteqv for*/weak-seteqv)
  (weak-fors #'make-weak-hasheqv))

(define-syntaxes (for/mutable-set for*/mutable-set)
  (mutable-fors #'make-hash))
(define-syntaxes (for/mutable-seteq for*/mutable-seteq)
  (mutable-fors #'make-hasheq))
(define-syntaxes (for/mutable-seteqv for*/mutable-seteqv)
  (mutable-fors #'make-hasheqv))
