#lang racket/base

(require (for-template racket/base "runtime.rkt" racket/stxparam racket/unsafe/ops)
         syntax/boundmap
         syntax/stx
         "patterns.rkt"
         "split-rows.rkt"
         "reorder.rkt"
         racket/stxparam
         racket/syntax)

(provide compile*)

;; should we reorder stuff?
(define can-reorder? (make-parameter #t #f 'can-reorder?))

;; for non-linear patterns
(define vars-seen (make-parameter null #f 'vars-seen))

(define (hash-on f elems #:equal? [eql #t])
  (define-values (ht h-ref h-set!)
    (case eql
      [(#t) (values (make-hash) hash-ref hash-set!)]
      [(#f) (values (make-hasheq) hash-ref hash-set!)]))
  (define keys null)
  ;; put all the elements e in the ht, indexed by (f e)
  (for ([r
         ;; they need to be in the original order when they come out
         (reverse elems)])
    (define k (f r))
    (h-set! ht k (cons r (h-ref ht k (lambda ()
                                       (set! keys (cons k keys))
                                       null)))))
  ;; Return a list, instead of a hash, to make order deterministic
  ;; based on the recorded order of keys
  (for/list ([k (in-list keys)])
    (cons k (hash-ref ht k))))

;; Like `hash-map`, bu for a list produced by `hash-on`:
(define (hash-on-map ht-l f)
  (map (lambda (p) (f (car p) (cdr p))) ht-l))

(define (and* . vs)
  (andmap values vs))

;; Produce a bool for every column in a set of rows, where #t means
;; that every pat in that column is a Dummy.
(define (dummy?-columns rows pat-acc)
  (apply map and* (for/list ([r (in-list rows)])
                    (map Dummy? (pat-acc (Row-first-pat r))))))

;; generate a clause of kind k
;; for rows rows, with matched variable x and rest variable xs
;; escaping to esc
(define (gen-clause k rows x xs esc)
  (define-syntax-rule (constant-pat predicate-stx)
    (with-syntax ([lhs (if (procedure? predicate-stx)
                           (predicate-stx x)
                           (quasisyntax/loc predicate-stx
                             (#,predicate-stx #,x)))]
                  [rhs (compile* (cons x xs)
                                 (map (lambda (row)
                                        (define-values (p ps)
                                          (Row-split-pats row))
                                        (make-Row (cons (make-Dummy #f) ps)
                                                  (Row-rhs row)
                                                  (Row-unmatch row)
                                                  (Row-vars-seen row)))
                                      rows)
                                 esc)])
      #'[lhs rhs]))
  (define (compile-con-pat accs pred pat-acc)
    ;; eliminate accessors for columns where every pat is a Dummy
    (let* ([dummy?s (dummy?-columns rows pat-acc)]
           [accs (for/list ([acc (in-list accs)]
                            [dummy? (in-list dummy?s)]
                            #:unless dummy?)
                   acc)]
           [filtered-acc (lambda (v)
                           (for/list ([pat (in-list (pat-acc v))]
                                      [dummy? (in-list dummy?s)]
                                      #:unless dummy?)
                             pat))])
      (with-syntax* ([(tmps ...) (generate-temporaries accs)]
                     [(accs ...) accs]
                     [question (if (procedure? pred)
                                   (pred x)
                                   #`(#,pred #,x))]
                     [body (compile*
                            (append (syntax->list #'(tmps ...)) xs)
                            (map (lambda (row)
                                   (define-values (p1 ps) (Row-split-pats row))
                                   (make-Row (append (filtered-acc p1) ps)
                                             (Row-rhs row)
                                             (Row-unmatch row)
                                             (Row-vars-seen row)))
                                 rows)
                            esc)])
        #`[question (let ([tmps (accs #,x)] ...) body)])))
  (cond
    [(eq? 'box k)
     (compile-con-pat (list #'unsafe-unbox*) #'box? (compose list Box-p))]
    [(eq? 'pair k)
     (compile-con-pat (list #'unsafe-car #'unsafe-cdr) #'pair?
                      (lambda (p) (list (Pair-a p) (Pair-d p))))]
    [(eq? 'mpair k)
     (compile-con-pat (list #'unsafe-mcar #'unsafe-mcdr) #'mpair?
                      (lambda (p) (list (MPair-a p) (MPair-d p))))]
    [(eq? 'null k)    (constant-pat #'null?)]
    ;; vectors are handled specially
    ;; because each arity is like a different constructor
    [(eq? 'vector k)
     (let ([ht (hash-on (lambda (r)
                          (length (Vector-ps (Row-first-pat r)))) rows)])
       (with-syntax ([(clauses ...)
                      (hash-on-map
                       ht
                       (lambda (arity rows)
                         (define dummy?s (dummy?-columns rows Vector-ps))
                         (define ns
                           (for/list ([n (in-range arity)]
                                      [dummy? (in-list dummy?s)]
                                      #:unless dummy?)
                             n))
                         (define (filtered-acc v)
                           (for/list ([pat (in-list (Vector-ps v))]
                                      [dummy? (in-list dummy?s)]
                                      #:unless dummy?)
                             pat))
                         (with-syntax ([(tmps ...) (generate-temporaries ns)])
                           (with-syntax ([body
                                          (compile*
                                           (append (syntax->list #'(tmps ...)) xs)
                                           (map (lambda (row)
                                                  (define-values (p1 ps)
                                                    (Row-split-pats row))
                                                  (make-Row (append (filtered-acc p1) ps)
                                                            (Row-rhs row)
                                                            (Row-unmatch row)
                                                            (Row-vars-seen row)))
                                                rows)
                                           esc)]
                                         [(n ...) ns])
                             #`[(#,arity)
                                (let ([tmps (unsafe-vector-ref #,x n)] ...)
                                  body)]))))])
         #`[(vector? #,x)
            (case (unsafe-vector-length #,x)
              clauses ...
              [else (#,esc)])]))]
    ;; it's a structure
    [(box? k)
     ;; all the rows are structures with the same predicate
     (let* ([s (Row-first-pat (car rows))]
            [accs (Struct-accessors s)]
            [accs (if (Struct-complete? s)
                      (build-list (length accs) (λ (i) #`(λ (x) (unsafe-struct-ref x #,i))))
                      accs)]
            [pred (Struct-pred s)])
       (compile-con-pat accs pred Struct-ps))]
    [(syntax? k) (constant-pat k)]
    [(procedure? k) (constant-pat k)]
    [else (error 'match-compile "bad key: ~a" k)]))

;; produces the syntax for a let clause
(define (compile-one vars block esc)
  (define-values (first rest-pats) (Row-split-pats (car block)))
  (define x (car vars))
  (define xs (cdr vars))
  (cond
    ;; the Exact rule
    [(Exact? first)
     (let ([ht (hash-on (compose Exact-v Row-first-pat) block #:equal? #t)])
       (with-syntax ([(clauses ...)
                      (hash-on-map
                       ht
                       (lambda (k v)
                         #`[(equal? #,x '#,k)
                            #,(compile* xs
                                        (map (lambda (row)
                                               (make-Row (cdr (Row-pats row))
                                                         (Row-rhs row)
                                                         (Row-unmatch row)
                                                         (Row-vars-seen row)))
                                             v)
                                        esc)]))])
         #`(cond clauses ... [else (#,esc)])))]
    ;; the Var rule
    [(Var? first)
     (let ([transform
            (lambda (row)
              (define-values (p ps) (Row-split-pats row))
              (define v (Var-v p))
              (define seen (Row-vars-seen row))
              ;; a new row with the rest of the patterns
              (cond
                ;; if this was a wild-card variable, don't bind
                [(Dummy? p) (make-Row ps
                                      (Row-rhs row)
                                      (Row-unmatch row)
                                      (Row-vars-seen row))]
                ;; if we've seen this variable before, check that it's equal to
                ;; the one we saw
                [(for/or ([e (in-list seen)])
                   (let ([v* (car e)] [id (cdr e)])
                     (and (bound-identifier=? v v*) (or id (list v v*)))))
                 =>
                 (lambda (id)
                   (if (identifier? id)
                       (make-Row ps
                                 #`(if ((match-equality-test) #,x #,id)
                                       #,(Row-rhs row)
                                       (fail))
                                 (Row-unmatch row)
                                 seen)
                       (begin
                         (log-error "non-linear pattern used in `match` with ... at ~a and ~a"
                                    (car id) (cadr id)) 
                         (let ([v* (free-identifier-mapping-get
                                    (current-renaming) v (lambda () v))])
                           (make-Row ps
                                     #`(let ([#,v* #,x]) #,(Row-rhs row))
                                     (Row-unmatch row)
                                     (cons (cons v x) (Row-vars-seen row)))))))]
                ;; otherwise, bind the matched variable to x, and add it to the
                ;; list of vars we've seen
                [else (let ([v* (free-identifier-mapping-get
                                 (current-renaming) v (lambda () v))])
                        (make-Row ps
                                  #`(let ([#,v* #,x]) #,(Row-rhs row))
                                  (Row-unmatch row)
                                  (cons (cons v x) (Row-vars-seen row))))]))])
       ;; compile the transformed block
       (compile* xs (map transform block) esc))]
    ;; the Constructor rule
    [(CPat? first)
     (let ;; put all the rows in the hash, indexed by their constructor
         ([ht (hash-on (lambda (r) (pat-key (Row-first-pat r))) block)])
       (with-syntax ([(clauses ...)
                      (hash-on-map
                       ht (lambda (k v) (gen-clause k v x xs esc)))])
         #`(cond clauses ... [else (#,esc)])))]
    ;; the Or rule
    [(Or? first)
     ;; we only handle 1-row Ors atm - this is all the mixture rule should give
     ;; us
     (unless (null? (cdr block))
       (error 'compile-one "Or block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            [seen (Row-vars-seen row)]
            ;; all the pattern alternatives
            [qs (Or-ps (car pats))]
            ;; the variables bound by this pattern - they're the same for the
            ;; whole list
            [vars
             (for/list ([bv (bound-vars (car qs))]
                        #:when (for/and ([seen-var seen])
                                        (not (free-identifier=? bv (car seen-var)))))
               bv)])
       (with-syntax ([(esc* success? var ...) (append (generate-temporaries '(esc* success?)) vars)])
         ;; do the or matching, and bind the results to the appropriate
         ;; variables
         #`(let ([esc* (lambda () (values #f #,@(for/list ([v vars]) #'#f)))])
             (let-values ([(success? var ...)
                           #,(compile* (list x)
                                       (map (lambda (q)
                                              (make-Row (list q)
                                                        #'(values #t var ...)
                                                        #f
                                                        seen))
                                            qs)
                                       #'esc*
                                       #f)])
               ;; then compile the rest of the row
               (if success?
                   #,(compile* xs
                               (list (make-Row (cdr pats)
                                               (Row-rhs row)
                                               (Row-unmatch row)
                                               (append (map cons vars vars) seen)))
                               esc
                               #f)
                   (#,esc))))))]
    ;; the App rule
    [(App? first)
     ;; we only handle 1-row Apps atm - this is all the mixture rule should
     ;; give us
     (unless (null? (cdr block))
       (error 'compile-one "App block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            [app-pats (App-ps first)]
            [app-expr (App-expr first)])
       (with-syntax ([(t ...) (generate-temporaries app-pats)])
         #`(let-values ([(t ...)
                         #,(if (procedure? app-expr)
                               (app-expr x)
                               (quasisyntax/loc app-expr
                                 (#,app-expr #,x)))])
             #,(compile* (append (syntax->list #'(t ...)) xs)
                         (list (make-Row (append app-pats (cdr pats))
                                         (Row-rhs row)
                                         (Row-unmatch row)
                                         (Row-vars-seen row)))
                         esc))))]
    ;; the And rule
    [(And? first)
     ;; we only handle 1-row Ands
     ;; this is all the mixture rule should give us
     (unless (null? (cdr block))
       (error 'compile-one "And block with multiple rows: ~a" block))
     (define row (car block))
     (define pats (Row-pats row))
     ;; all the patterns
     (define qs (And-ps (car pats)))
     (compile* (append (map (lambda _ x) qs) xs)
               (list (make-Row (append qs (cdr pats))
                               (Row-rhs row)
                               (Row-unmatch row)
                               (Row-vars-seen row)))
               esc
               ;; don't re-order OrderedAnd patterns
               (not (OrderedAnd? first)))]
    ;; the Not rule
    [(Not? first)
     ;; we only handle 1-row Nots atm - this is all the mixture rule should
     ;; give us
     (unless (null? (cdr block))
       (error 'compile-one "Not block with multiple rows: ~a" block))
     (let* ([row (car block)]
            [pats (Row-pats row)]
            ;; the single pattern
            [q (Not-p (car pats))])
       (with-syntax ([(f) (generate-temporaries #'(f))])
         #`(let ;; if q fails, we jump to here
                ([f (lambda ()
                      #,(compile* xs
                                  (list (make-Row (cdr pats)
                                                  (Row-rhs row)
                                                  (Row-unmatch row)
                                                  (Row-vars-seen row)))
                                  esc))])
             #,(compile* (list x)
                         ;; if q doesn't fail, we jump to esc and fail the not
                         ;; pattern
                         (list (make-Row (list q)
                                         #`(#,esc)
                                         (Row-unmatch row)
                                         (Row-vars-seen row)))
                         #'f))))]
    [(Pred? first)
     ;; put all the rows in the hash, indexed by their Pred pattern
     ;; we use the pattern so that it can have a custom equal+hash
     (define ht (hash-on (lambda (r) (Row-first-pat r)) block #:equal? #t))
     (with-syntax ([(clauses ...)
                    (hash-on-map
                     ht (lambda (k v)
                          (gen-clause (Pred-pred k) v x xs esc)))])
       #`(cond clauses ... [else (#,esc)]))]
    ;; Generalized sequences... slightly tested
    [(GSeq? first)
     (unless (null? (cdr block))
       (error 'compile-one "GSeq block with multiple rows: ~a" block))
     (let* ([headss (GSeq-headss first)]
            [mins (GSeq-mins first)]
            [maxs (GSeq-maxs first)]
            [onces? (GSeq-onces? first)]
            [tail (GSeq-tail first)]
            [mutable? (GSeq-mutable? first)]
            [make-Pair (if mutable? make-MPair make-Pair)]
            [k (Row-rhs (car block))]
            [xvar (car (generate-temporaries (list #'x)))]
            [complete-heads-pattern
             (lambda (ps)
               (define (loop ps pat)
                 (if (pair? ps)
                     (make-Pair (car ps) (loop (cdr ps) pat))
                     pat))
               (loop ps (make-Var xvar)))]
            [heads
             (for/list ([ps headss])
               (complete-heads-pattern ps))]
            [head-idss
             (for/list ([heads headss])
               (apply append (map bound-vars heads)))]
            [heads-seen
             (map (lambda (x) (cons x #f))
                  (apply append (map bound-vars heads)))]
            [tail-seen
             (map (lambda (x) (cons x x))
                  (bound-vars tail))]
            [hid-argss (map generate-temporaries head-idss)]
            [head-idss* (map generate-temporaries head-idss)]
            [hid-args (apply append hid-argss)]
            [reps (generate-temporaries (for/list ([head heads]) 'rep))])
       (with-syntax ([x xvar]
                     [var0 (car vars)]
                     [((hid ...) ...) head-idss]
                     [((hid* ...) ...) head-idss*]
                     [((hid-arg ...) ...) hid-argss]
                     [(rep ...) reps]
                     [(maxrepconstraint ...)
                      ;; FIXME: move to side condition to appropriate pattern
                      (for/list ([repvar reps] [maxrep maxs])
                        (if maxrep #`(< #,repvar #,maxrep) #`#t))]
                     [(minrepclause ...)
                      (for/list ([repvar reps] [minrep mins] #:when minrep)
                        #`[(< #,repvar #,minrep) (fail)])]
                     [((hid-rhs ...) ...)
                      (for/list ([hid-args hid-argss] [once? onces?])
                        (for/list ([hid-arg hid-args])
                          (if once?
                              #`(car (reverse #,hid-arg))
                              #`(reverse #,hid-arg))))]
                     [(parse-loop failkv fail-tail)
                      (generate-temporaries #'(parse-loop failkv fail-tail))])
         (with-syntax ([(rhs ...)
                        #`[(let ([hid-arg (cons hid* hid-arg)] ...)
                             (if maxrepconstraint
                                 (let ([rep (add1 rep)])
                                   (parse-loop x #,@hid-args #,@reps fail))
                                 (begin (fail))))
                           ...]]
                       [tail-rhs
                        #`(cond minrepclause ...
                                [else
                                 (let ([hid hid-rhs] ... ...
                                       [fail-tail fail])
                                   #,(compile*
                                      (cdr vars)
                                      (list (make-Row rest-pats k
                                                      (Row-unmatch (car block))
                                                      (append
                                                       heads-seen
                                                       tail-seen
                                                       (Row-vars-seen
                                                        (car block)))))
                                      #'fail-tail))])])
           (parameterize ([current-renaming
                           (for/fold ([ht (copy-mapping (current-renaming))])
                               ([id (apply append head-idss)]
                                [id* (apply append head-idss*)])
                             (free-identifier-mapping-put! ht id id*)
                             (free-identifier-mapping-for-each
                              ht
                              (lambda (k v)
                                (when (free-identifier=? v id)
                                  (free-identifier-mapping-put! ht k id*))))
                             ht)])
             #`(let parse-loop ([x var0]
                                [hid-arg null] ... ...
                                [rep 0] ...
                                [failkv #,esc])
                 #,(compile* (list #'x)
                             (append
                              (map (lambda (pats rhs)
                                     (make-Row pats
                                               rhs
                                               (Row-unmatch (car block))
                                               (Row-vars-seen
                                                (car block))))
                                   (map list heads)
                                   (syntax->list #'(rhs ...)))
                              (list (make-Row (list tail)
                                              #`tail-rhs
                                              (Row-unmatch (car block))
                                              (append
                                               heads-seen
                                               (Row-vars-seen
                                                (car block))))))
                             #'failkv))))))]
    [else (error 'compile "unsupported pattern: ~a\n" first)]))

(define (compile* vars rows esc [reorder? (can-reorder?)])
  (define (let/wrap clauses body)
    (if (stx-null? clauses)
      body
      (quasisyntax (let* #,clauses #,body))))
  (cond
   ;; if there are no rows, then just call the esc continuation
   [(null? rows) #`(#,esc)]
    ;; if we have no variables, there are no more patterns to match
    ;; so we just pick the first RHS
   [(null? vars)
    (let ([fns
           (let loop ([blocks (reverse rows)] [esc esc] [acc null])
             (if (null? blocks)
                 ;; if we're done, return the blocks
                 (reverse acc)
                 (with-syntax
                  (;; f is the name this block will have
                   [(f) (generate-temporaries #'(f))]
                   ;; compile the block, with jumps to the previous esc
                   [c (with-syntax ([rhs #`(syntax-parameterize
                                            ([fail (make-rename-transformer
                                                    (quote-syntax #,esc))])
                                            #,(Row-rhs (car blocks)))])
                                   (define unmatch (Row-unmatch (car blocks)))
                                   (if unmatch
                                       (quasisyntax/loc unmatch
                                         (call-with-continuation-prompt
                                          (lambda () (let ([#,unmatch
                                                            (lambda ()
                                                              (abort-current-continuation match-prompt-tag))])
                                                       rhs))
                                          match-prompt-tag
                                          (lambda () (#,esc))))
                                       #'rhs))])
                  ;; then compile the rest, with our name as the esc
                  (loop (cdr blocks) #'f (cons #'[f (lambda () c)] acc)))))])
      (with-syntax ([(fns ... [_ (lambda () body)]) fns])
                   (let/wrap #'(fns ...) #'body)))]
    ;; otherwise, we split the matrix into blocks
    ;; and compile each block with a reference to its continuation
   [else
    (let*-values
        ([(rows vars) (if (and (>= 1 (length vars))
                               reorder?
                               ;; moving Or patterns early breaks Typed Racket
                               (not (ormap Or? (apply append (map Row-pats rows)))))
                          (reorder-columns rows vars)
                          (values rows vars))]
         [(fns)
          (let loop ([blocks (reverse (split-rows rows))] [esc esc] [acc null])
            (if (null? blocks)
                ;; if we're done, return the blocks
                (reverse acc)
                (with-syntax (;; f is the name this block will have
                              [(f) (generate-temporaries #'(f))]
                              ;; compile the block, with jumps to the previous
                              ;; esc
                              [c 
                               (parameterize ([can-reorder? reorder?])
                                 (compile-one vars (car blocks) esc))])
                  ;; then compile the rest, with our name as the esc
                  (loop (cdr blocks)
                        #'f
                        (cons #`[f #,(syntax-property
                                      #'(lambda () c)
                                      'typechecker:called-in-tail-position #t)]
                              acc)))))])
      (with-syntax ([(fns ... [_ (lambda () body)]) fns])
        (let/wrap #'(fns ...) #'body)))]))

;; (require mzlib/trace)
;; (trace compile* compile-one)
