#lang scheme

(require "matcher.ss"
         "reduction-semantics.ss"
         "underscore-allowed.ss"
         "term.ss"
         "error.ss"
         "struct.ss"
         (for-syntax "rewrite-side-conditions.ss")
         (for-syntax "term-fn.ss")
         (for-syntax "reduction-semantics.ss")
         (for-syntax "keyword-macros.ss")
         mrlib/tex-table)

(define redex-pseudo-random-generator
  (make-parameter (current-pseudo-random-generator)))
(define (generator-random . arg)
  (parameterize ([current-pseudo-random-generator (redex-pseudo-random-generator)])
    (apply random arg)))

(define (exotic-choice? [random generator-random]) (= 0 (random 5)))
(define (use-lang-literal? [random generator-random]) (= 0 (random 20)))

(define default-check-attempts 1000)

(define ascii-chars-threshold 1000)
(define tex-chars-threshold 1500)
(define chinese-chars-threshold 2500)

(define (pick-var lang-lits attempt [random generator-random])
  (let ([length (add1 (random-natural 4/5 random))])
    (string->symbol (random-string lang-lits length attempt random))))

(define (pick-char attempt [random generator-random])
  (cond [(or (< attempt ascii-chars-threshold) (not (exotic-choice? random)))
         (let ([i (random (add1 (- (char->integer #\z) (char->integer #\a))))]
               [cap? (zero? (random 2))])
           (integer->char (+ i (char->integer (if cap? #\A #\a)))))]
        [(or (< attempt tex-chars-threshold) (not (exotic-choice? random)))
         (let ([i (random (- #x7E #x20 1))]
               [_ (- (char->integer #\_) #x20)])
           (integer->char (+ #x20 (if (= i _) (add1 i) i))))]
        [(or (< attempt chinese-chars-threshold) (not (exotic-choice? random)))
         (car (string->list (pick-from-list (map cadr tex-shortcut-table) random)))]
        [else
         (integer->char (+ #x4E00 (random (- #x9FCF #x4E00))))]))

(define (random-string lang-lits length attempt [random generator-random])
  (if (and (not (null? lang-lits)) (use-lang-literal? random))
      (pick-from-list lang-lits random)
      (list->string (build-list length (λ (_) (pick-char attempt random))))))

(define (pick-any lang sexp [random generator-random]) 
  (if (and (> (dict-count (rg-lang-non-cross lang)) 0) (zero? (random 5)))
      (let ([nts (rg-lang-non-cross lang)])
        (values lang (pick-from-list (dict-map nts (λ (nt _) nt)) random)))
      (values sexp 'sexp)))

(define (pick-string lang-lits attempt [random generator-random])
  (random-string lang-lits (random-natural 1/5 random) attempt random))

;; next-non-terminal-decision selects a subset of a non-terminal's productions.
;; This implementation, the default, chooses them all, but many of the
;; generator's test cases restrict the productions.
(define pick-nts values)

(define (pick-from-list l [random generator-random])
  (list-ref l (random (length l))))

;; Chooses a random (exact) natural number from the "shifted" geometric distribution:
;;   P(random-natural = k) = p(1-p)^k
;;
;; P(random-natural >= k) = (1-p)^(k+1)
;; E(random-natural) = (1-p)/p
;; Var(random-natural) = (1-p)/p^2
(define (random-natural p [random generator-random])
  (sub1 (inexact->exact (ceiling (real-part (/ (log (random)) (log (- 1 p))))))))

(define (negative? random)
  (zero? (random 2)))

(define (random-integer p [random generator-random])
  (* (if (negative? random) -1 1) (random-natural p random)))

(define (random-rational p [random generator-random])
  (/ (random-integer p random) (add1 (random-natural p random))))

(define (random-real p [random generator-random])
  (* (random) 2 (random-integer p random)))

(define (random-complex p [random generator-random])
  (let ([randoms (list random-integer random-rational random-real)])
    (make-rectangular ((pick-from-list randoms random) p random) 
                      ((pick-from-list randoms random) p random))))

(define integer-threshold 100)
(define rational-threshold 500)
(define real-threshold 1000)
(define complex-threshold 2000)

(define default-retries 100)
(define retry-threshold (max chinese-chars-threshold complex-threshold))
(define proportion-before-threshold 9/10)
(define proportion-at-size 1/10)
(define post-threshold-incr 50)

;; Determines the parameter p for which random-natural's expected value is E
(define (expected-value->p E)
  ;; E = 0 => p = 1, which breaks random-natural
  (/ 1 (+ (max 1 E) 1)))

; Determines a size measure for numbers, sequences, etc., using the
; attempt count.
(define default-attempt-size
  (λ (n) (inexact->exact (floor (/ (log (add1 n)) (log 5))))))
(define attempt-size/c
  (-> natural-number/c natural-number/c))
(define attempt->size 
  (make-parameter default-attempt-size))

(define (pick-number attempt #:top-threshold [top-threshold complex-threshold] [random generator-random])
  (let loop ([threshold 0] 
             [generator random-natural]
             [levels `((,integer-threshold . ,random-integer)
                       (,rational-threshold . ,random-rational)
                       (,real-threshold . ,random-real)
                       (,complex-threshold . ,random-complex))])
    (if (or (null? levels)
            (< attempt (caar levels))
            (< top-threshold (caar levels))
            (not (exotic-choice? random)))
        (generator (expected-value->p ((attempt->size) (- attempt threshold))) random)
        (loop (caar levels) (cdar levels) (cdr levels)))))

(define (pick-natural attempt [random generator-random])
  (pick-number attempt #:top-threshold 0 random))

(define (pick-integer attempt [random generator-random])
  (pick-number attempt #:top-threshold integer-threshold random))

(define (pick-real attempt [random generator-random])
  (pick-number attempt #:top-threshold real-threshold random))

(define (pick-sequence-length attempt)
  (random-natural (expected-value->p ((attempt->size) attempt))))

(define (min-prods nt prods base-table)
  (let* ([sizes (hash-ref base-table nt)]
         [min-size (apply min/f sizes)])
    (map cadr (filter (λ (x) (equal? min-size (car x))) (map list sizes prods)))))

(define-struct rg-lang (non-cross cross base-cases))
(define (prepare-lang lang)
  (let ([parsed (parse-language lang)])
    (values parsed (map symbol->string (compiled-lang-literals lang)) (find-base-cases parsed))))

(define-struct (exn:fail:redex:generation-failure exn:fail:redex) ())
(define (raise-gen-fail who what attempts)
  (let ([str (format "~a: unable to generate ~a in ~a attempt~a" 
                     who what attempts (if (= attempts 1) "" "s"))])
    (raise (make-exn:fail:redex:generation-failure str (current-continuation-marks)))))

(define (compile lang what)
  (define-values/invoke-unit (generation-decisions)
    (import) (export decisions^))
  
  (define (gen-nt lang name cross? retries size attempt filler)
    (let*-values
        ([(productions)
          (hash-ref ((if cross? rg-lang-cross rg-lang-non-cross) lang) name)]
         [(term _)
          (let ([gen (pick-from-list
                      (if (zero? size)
                          (min-prods name productions 
                                     ((if cross? base-cases-cross base-cases-non-cross)
                                      (rg-lang-base-cases lang)))
                          ((next-non-terminal-decision) productions)))])
            (gen retries (max 0 (sub1 size)) attempt empty-env filler))])
      term))
  
  (define (generate/pred name gen pred init-sz init-att retries)
    (let ([pre-threshold-incr 
           (ceiling
            (/ (- retry-threshold init-att)
               (* proportion-before-threshold (add1 retries))))]
          [incr-size? 
           (λ (remain)
             (zero? 
              (modulo (sub1 remain) 
                      (ceiling (* proportion-at-size retries)))))])
      (let retry ([remaining (add1 retries)]
                  [size init-sz]
                  [attempt init-att])
        (if (zero? remaining)
            (raise-gen-fail what (format "pattern ~a" name) retries)
            (let-values ([(term env) (gen size attempt)])
              (if (pred term env)
                  (values term env)
                  (retry (sub1 remaining)
                         (if (incr-size? remaining) (add1 size) size)
                         (+ attempt
                            (if (>= attempt retry-threshold)
                                post-threshold-incr
                                pre-threshold-incr)))))))))
  
  (define (generate/prior name env gen)
    (let* ([none (gensym)]
           [prior (hash-ref env name none)])
      (if (eq? prior none)
          (let-values ([(term env) (gen)])
            (values term (hash-set env name term)))
          (values prior env))))
  
  (define (generate-sequence gen env vars length)
    (define (split-environment env)
      (foldl (λ (var seq-envs)
               (let ([vals (hash-ref env var #f)])
                 (if vals
                     (map (λ (seq-env val) (hash-set seq-env var val)) seq-envs vals)
                     seq-envs)))
             (build-list length (λ (_) #hash())) vars))
    (define (merge-environments seq-envs)
      (foldl (λ (var env)
               (hash-set env var (map (λ (seq-env) (hash-ref seq-env var)) seq-envs)))
             env vars))
    (let-values
        ([(seq envs)
          (let recur ([envs (split-environment env)])
            (if (null? envs)
                (values null null)
                (let*-values 
                    ([(hd env) (gen (car envs))]
                     [(tl envs) (recur (cdr envs))])
                  (values (cons hd tl) (cons env envs)))))])
      (values seq (merge-environments envs))))
  
  (define ((generator/attempts g) r s a e f)
    (values (g a) e))
  
  (define (mismatches-satisfied? env)
    (let ([groups (make-hasheq)])
      (define (get-group group)
        (hash-ref groups group
                  (λ ()
                    (let ([vals (make-hash)])
                      (hash-set! groups group vals)
                      vals))))
      (for/and ([(name val) env])
        (or (not (mismatch? name))
            (let ([prior (get-group (mismatch-group name))])
              (and (not (hash-ref prior val #f))
                   (hash-set! prior val #t)))))))
  
  (define empty-env #hash())
  
  (define (bindings env)
    (make-bindings
     (for/fold ([bindings null]) ([(key val) env])
       (if (binder? key) 
           (cons (make-bind (binder-name key) val) bindings)
           bindings))))
  
  (let*-values ([(langp lits lang-bases) (prepare-lang lang)]
                [(sexpp _ sexp-bases) (prepare-lang sexp)]
                [(lit-syms) (compiled-lang-literals lang)])
    (letrec-values 
        ([(compile)
          (λ (pat any?)
            (let* ([nt? (is-nt? (if any? sexpp langp))]
                   [mismatches? #f]
                   [generator 
                    ; retries size attempt env filler -> (values terms env)
                    ;
                    ; Patterns like (in-hole C_1 p) require constructing both an unfilled context
                    ; (exposed via the C_1 binding) and a filled context (exposed as the result).
                    ; A generator constructs both by constructing the context, using either 
                    ; `the-hole' or `the-not-hole' as appropriate, then filling it using `plug'.
                    ; Before returning its result, a generator replaces occurrences of `the-not-hole'
                    ; with `the-hole' to avoid exposing the distinction to the user, but 
                    ; `the-not-hole' remains in bindings supplied to side-condition predicates, to
                    ; match the behavior of the matcher.
                    ;
                    ; Repeated traversals via `plug' are not asymptotically worse than simultaneously
                    ; constructing the filled and unfilled pattern, due to languages like this one,
                    ; which names contexts in a way that prevents any sharing.
                    ; (define-language L
                    ;  (W hole
                    ;  ; extra parens to avoid matcher loop
                    ;  (in-hole (W_1) (+ natural hole))))
                    (let recur ([pat pat])
                      (match pat
                        [`number (generator/attempts (λ (a) ((next-number-decision) a)))]
                        [`natural (generator/attempts (λ (a) ((next-natural-decision) a)))]
                        [`integer (generator/attempts (λ (a) ((next-integer-decision) a)))]
                        [`real (generator/attempts (λ (a) ((next-real-decision) a)))]
                        [`(variable-except ,vars ...)
                         (let ([g (recur 'variable)])
                           (λ (r s a e f)
                             (generate/pred pat
                                            (λ (s a) (g r s a e f))
                                            (λ (var _) (not (memq var vars)))
                                            s a r)))]
                        [`variable (generator/attempts (λ (a) ((next-variable-decision) lits a)))]
                        [`variable-not-otherwise-mentioned
                         (let ([g (recur 'variable)])
                           (λ (r s a e f)
                             (generate/pred pat
                                            (λ (s a) (g r s a e f))
                                            (λ (var _) (not (memq var lit-syms)))
                                            s a r)))]
                        [`(variable-prefix ,prefix)
                         (define (symbol-append prefix suffix)
                           (string->symbol (string-append (symbol->string prefix) (symbol->string suffix))))
                         (let ([g (recur 'variable)])
                           (λ (r s a e f)
                             (let-values ([(t e) (g r s a e f)])
                               (values (symbol-append prefix t) e))))]
                        [`string (generator/attempts (λ (a) ((next-string-decision) lits a)))]
                        [`(side-condition ,pat ,(? procedure? condition) ,guard-src-loc)
                         (let ([g (recur pat)])
                           (λ (r s a e f)
                             (generate/pred `(side-condition ,(unparse-pattern pat) ,guard-src-loc) 
                                            (λ (s a) (g r s a e f))
                                            (λ (_ env) (condition (bindings env)))
                                            s a r)))]
                        [`(name ,(? symbol? id) ,p)
                         (let ([g (recur p)])
                           (λ (r s a e f)
                             (let-values ([(t env) (g r s a e f)])
                               (values t (hash-set env (make-binder id) t)))))]
                        [`hole (λ (r s a e f) (values f e))]
                        [`(in-hole ,context ,filler)
                         (let ([c-context (recur context)]
                               [c-filler (recur filler)])
                           (λ (r s a e f)
                             (let*-values ([(filler env) (c-filler r s a e f)]
                                           [(context env) (c-context r s a env the-hole)])
                               (values (plug context filler) env))))]
                        [`(hide-hole ,pattern)
                         (let ([g (recur pattern)])
                           (λ (r s a e f)
                             (g r s a e the-not-hole)))]
                        [`any
                         (λ (r s a e f)
                           (let*-values ([(lang nt) ((next-any-decision) langc sexpc)]
                                         [(term) (gen-nt lang nt #f r s a (list the-hole))])
                             (values term e)))]
                        [(or (? symbol? (? nt? p)) `(cross ,(? symbol? p)))
                         (let ([cross? (not (symbol? pat))])
                           (λ (r s a e f)
                             (values (gen-nt (if any? sexpc langc) p cross? r s a f) e)))]
                        [(? binder?)
                         (let ([g (recur (binder-pattern pat))])
                           (λ (r s a e f)
                             (generate/prior pat e (λ () (g r s a e f)))))]
                        [(? mismatch?)
                         (let ([g (recur (mismatch-pattern pat))])
                           (set! mismatches? #t)
                           (λ (r s a e f)
                             (let-values ([(t e) (g r s a e f)])
                               (values t (hash-set e pat t)))))]
                        [(or (? symbol?) (? number?) (? string?) (? boolean?) (? null?))
                         (λ (r s a e f) (values pat e))]
                        [(list-rest (struct ellipsis (name sub-pat class vars)) rest)
                         (let ([elemg (recur sub-pat)]
                               [tailg (recur rest)])
                           (when (mismatch? name)
                             (set! mismatches? #t))
                           (λ (r s a e f)
                             (let*-values ([(len) 
                                            (let ([prior (hash-ref e class #f)])
                                              (if prior
                                                  prior
                                                  (if (zero? s) 0 ((next-sequence-decision) a))))]
                                           [(seq env)
                                            (generate-sequence (λ (e) (elemg r s a e f)) e vars len)]
                                           [(tail env) 
                                            (let ([e (hash-set (hash-set env class len) name len)])
                                              (tailg r s a e f))])
                               (values (append seq tail) env))))]
                        [(list-rest hdp tlp)
                         (let ([hdg (recur hdp)]
                               [tlg (recur tlp)])
                           (λ (r s a e f)
                             (let*-values 
                                 ([(hd env) (hdg r s a e f)]
                                  [(tl env) (tlg r s a env f)])
                               (values (cons hd tl) env))))]
                        [else
                         (error what "unknown pattern ~s\n" pat)]))])
              (if mismatches?
                  (λ (r s a e f)
                    (let ([g (λ (s a) (generator r s a e f))]
                          [p? (λ (_ e) (mismatches-satisfied? e))])
                      (generate/pred (unparse-pattern pat) g p? s a r)))
                  generator)))]
         [(compile-non-terminals)
          (λ (nts any?)
            (make-immutable-hash
             (map (λ (nt) (cons (nt-name nt)
                                (map (λ (p) (compile (rhs-pattern p) any?))
                                     (nt-rhs nt))))
                  nts)))]
         [(compile-language)
          (λ (lang bases any?)
            (make-rg-lang
             (compile-non-terminals (compiled-lang-lang lang) any?)
             (compile-non-terminals (compiled-lang-cclang lang) any?)
             bases))]
         [(langc sexpc compile-pattern)
          (values
           (compile-language langp lang-bases #f)
           (compile-language sexpp sexp-bases #t)
           (λ (pat) (compile pat #f)))])
      (λ (pat)
        (let ([g (compile-pattern (reassign-classes (parse-pattern pat lang 'top-level)))])
          (λ (size attempt retries)
            (let-values ([(t e) (g retries size attempt empty-env the-hole)])
              (values (let replace-the-not-hole ([t t])
                        (cond [(eq? t the-not-hole) the-hole]
                              [(list? t) (map replace-the-not-hole t)]
                              [else t])) 
                      (bindings e)))))))))

(define-struct base-cases (cross non-cross))

;; find-base-cases : (list/c nt) -> base-cases
(define (find-base-cases lang)
  (define nt-table (make-hash))
  (define changed? #f)
  (define (nt-get nt) (hash-ref nt-table nt 'inf))
  (define (nt-set nt new) 
    (let ([old (nt-get nt)])
      (unless (equal? new old)
        (set! changed? #t)
        (hash-set! nt-table nt new))))
  
  (define ((process-nt cross?) nt)
    (nt-set (cons cross? (nt-name nt)) (apply min/f (map process-rhs (nt-rhs nt)))))
  
  (define (process-rhs rhs)
    (let ([nts (rhs->nts (rhs-pattern rhs))])
      (if (null? nts) 
          0
          (add1/f (apply max/f (map nt-get nts))))))
  
  ;; rhs->path : pattern -> (listof (cons/c boolean symbol))
  ;; determines all of the non-terminals in a pattern
  (define (rhs->nts pat)
    (let ([nts '()])
      (let loop ([pat pat])
        (match pat
          [(? binder?)
           (set! nts (cons (cons #f (binder-pattern pat)) nts))]
          [(? mismatch?)
           (set! nts (cons (cons #f (mismatch-pattern pat)) nts))]
          [(? symbol?)
           (when ((is-nt? lang) pat)
             (set! nts (cons (cons #f pat) nts)))]
          [`(cross ,(? symbol? x-nt))
           (set! nts (cons (cons #t x-nt) nts))]
          [`(variable-except ,s ...) (void)]
          [`(variable-prefix ,p) (void)]
          [`(name ,_ ,p) (loop p)]
          [`() (void)]
          [(struct ellipsis (_ p _ _))
           (loop p)]
          [`(,a . ,b)
           (loop a)
           (loop b)]
          [_ (void)]))
      nts))

  ;; build-table : (listof nt) -> hash
  (define (build-table nts)
    (let ([tbl (make-hasheq)])
      (for-each
       (λ (nt) (hash-set! tbl (nt-name nt) (map process-rhs (nt-rhs nt))))
       nts)
      tbl))
  
  (let loop ()
    (set! changed? #f)
    (for-each (process-nt #f) (compiled-lang-lang lang))
    (for-each (process-nt #t) (compiled-lang-cclang lang))
    (when changed?
      (loop)))
  
  (make-base-cases
   (build-table (compiled-lang-cclang lang))
   (build-table (compiled-lang-lang lang))))

(define min/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) b]
       [(eq? b 'inf) a]
       [else (min a b)])]
    [(a b . c) (min/f a (apply min/f b c))]))
(define max/f
  (case-lambda
    [(a) a]
    [(a b) 
     (cond
       [(eq? a 'inf) a]
       [(eq? b 'inf) b]
       [else (max a b)])]
    [(a b . c) (max/f a (apply max/f b c))]))
(define (add1/f a) (if (eq? a 'inf) 'inf (+ a 1)))

;; is-nt? : compiled-lang any -> boolean
(define ((is-nt? lang) x)
  (and (hash-ref (compiled-lang-ht lang) x #f) #t))

;; built-in? : any -> boolean
(define (built-in? x)
  (and (memq x underscore-allowed) #t))

;; nt-by-name : lang symbol boolean -> nt
(define (nt-by-name lang name cross?)
  (findf (λ (nt) (eq? name (nt-name nt))) 
         (if cross? 
             (compiled-lang-cclang lang)
             (compiled-lang-lang lang))))

(define named-nt-rx #rx"^([^_]+)_[^_]*$")
(define mismatch-nt-rx #rx"([^_]+)_!_[^_]*$")
(define named-ellipsis-rx #rx"^\\.\\.\\._[^_]*$")
(define mismatch-ellipsis-rx #rx"^\\.\\.\\._!_[^_]*$")

;; symbol-match : regexp -> any -> (or/c false symbol)
;; Returns the sub-symbol matching the sub-pattern inside
;; the first capturing parens.
(define ((symbol-match rx) x)
  (and (symbol? x) 
       (let ([match (regexp-match rx (symbol->string x))])
         (and match (cadr match) (string->symbol (cadr match))))))

(define-struct class (id) #:inspector (make-inspector))

(define-struct mismatch (id group) #:inspector (make-inspector))
(define mismatch-pattern
  (match-lambda
    [(struct mismatch (_ name))
     ((symbol-match mismatch-nt-rx) name)]))

(define-struct binder (name) #:inspector (make-inspector))
(define binder-pattern
  (match-lambda
    [(struct binder (name))
     (match ((symbol-match named-nt-rx) name)
       [#f name]
       [p p])]))

;; name: (or/c symbol? mismatch?)
;;   The generator records `name' in the environment when generating an ellipsis,
;;   to enforce sequence length constraints.
;; class: class?
;;   When one binding appears under two (non-nested) ellipses, the sequences generated
;;   must have the same length; `class' groups ellipses to reflect this constraint.
;; var: (list/c (or/c symbol? class? mismatch? binder?))
;;   the bindings within an ellipses, used to split and merge the environment before
;;   and after generating an ellipsis
(define-struct ellipsis (name pattern class vars) #:inspector (make-inspector))

;; parse-pattern : pattern compiled-lang (or/c 'cross 'top-level 'grammar) -> parsed-pattern
;; Turns "pat ...", "pat ..._id", and "pat ..._!_id" into ellipsis structs,
;; "nt_!_id" into mismatch structs, "nt_id" into binder structs, and
;; "nt/underscore-allowed" in top-level patterns into binder structs.
(define (parse-pattern pattern lang mode)
  (define (recur pat vars)
    (match pat
      [(or (app (symbol-match named-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
           (and (? (λ (_) (eq? mode 'top-level))) (or (? (is-nt? lang)) (? built-in?))))
       (let ([b (make-binder pat)])
         (values b (cons b vars)))]
      [(app (symbol-match mismatch-nt-rx) (or (? (is-nt? lang)) (? built-in?)))
       (let ([mismatch (make-mismatch (gensym) pat)])
         (values mismatch (cons mismatch vars)))]
      [`(name ,name ,sub-pat)
       (let-values ([(parsed vars) (recur sub-pat vars)])
         (values `(name ,name ,parsed) (cons (make-binder name) vars)))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp named-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(seq) (make-ellipsis name sub-pat-parsed (make-class name) sub-pat-vars)]
                     [(vars) (append (list* name (make-class name) sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat '... rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis '... sub-pat-parsed class sub-pat-vars)]
                     [(rest-parsed vars) (recur rest (cons class (append sub-pat-vars vars)))])
         (values (cons seq rest-parsed) vars))]
      [(list-rest sub-pat (and (? symbol?) (app symbol->string (regexp mismatch-ellipsis-rx)) name) rest)
       (let*-values ([(sub-pat-parsed sub-pat-vars) (recur sub-pat null)]
                     [(mismatch) (make-mismatch (gensym) name)]
                     [(class) (make-class (gensym))]
                     [(seq) (make-ellipsis mismatch sub-pat-parsed class sub-pat-vars)]
                     [(vars) (append (list* class mismatch sub-pat-vars) vars)]
                     [(rest-parsed vars) (recur rest vars)])
         (values (cons seq rest-parsed) vars))]
      [(and (? (λ (_) (not (eq? mode 'cross)))) `(cross ,(and (? (is-nt? lang)) nt)))
       (let ([nt-str (symbol->string nt)])
         (values `(cross ,(string->symbol (string-append nt-str "-" nt-str))) vars))]
      [`(side-condition ,pat ,guard ,guard-src-loc)
       (let-values ([(parsed vars) (recur pat vars)])
         (values `(side-condition ,parsed ,guard ,guard-src-loc) vars))]
      [(cons first rest)
       (let-values ([(first-parsed vars) (recur first vars)])
         (let-values ([(rest-parsed vars) (recur rest vars)])
           (values (cons first-parsed rest-parsed) vars)))]
      [_ (values pat vars)]))
  (let-values ([(parsed _) (recur pattern null)])
    parsed))

;; parse-language: compiled-lang -> compiled-lang
(define (parse-language lang)
  (define ((parse-nt mode) nt)
    (make-nt (nt-name nt) (map (parse-rhs mode) (nt-rhs nt))))
  (define ((parse-rhs mode) rhs)
    (make-rhs (reassign-classes (parse-pattern (rhs-pattern rhs) lang mode))))
  
  (struct-copy 
   compiled-lang lang
   [lang (map (parse-nt 'grammar) (compiled-lang-lang lang))]
   [cclang (map (parse-nt 'cross) (compiled-lang-cclang lang))]))

;; unparse-pattern: parsed-pattern -> pattern
(define unparse-pattern
  (match-lambda
    [(struct binder (name)) name]
    [(struct mismatch (_ group)) group]
    [(list-rest (struct ellipsis (name sub-pat _ _)) rest)
     (let ([ellipsis (if (mismatch? name) (mismatch-group name) name)])
       (list* (unparse-pattern sub-pat) ellipsis (unparse-pattern rest)))]
    [(cons first rest) 
     (cons (unparse-pattern first) (unparse-pattern rest))]
    [else else]))

;; class-reassignments : parsed-pattern -> hash[sym -o> sym]
(define (class-reassignments pattern)
  ; union-find w/o balancing or path compression (at least for now)
  (define (union e f sets)
    (hash-set sets (find f sets) (find e sets)))
  (define (find e sets)
    (let recur ([chd e] [par (hash-ref sets e #f)])
      (if (and par (not (eq? chd par))) (recur par (hash-ref sets par #f)) chd)))
  
  (let* ([last-contexts (make-hasheq)]
         [record-binder
          (λ (pat under assignments)
            (if (null? under) 
                assignments
                (let ([last (hash-ref last-contexts pat #f)])
                  (if last
                      (foldl (λ (cur last asgns) (union cur last asgns)) assignments under last)
                      (begin
                        (hash-set! last-contexts pat under)
                        assignments)))))]
         [assignments
          (let recur ([pat pattern] [under null] [assignments #hasheq()])
            (match pat
              ;; `(name ,id ,sub-pat) not considered, since bindings introduced
              ;; by name must be unique.
              [(struct binder (name))
               (record-binder name under assignments)]
              [(struct ellipsis (name sub-pat (struct class (cls)) _))
               (recur sub-pat (cons cls under)
                 (if (and (symbol? name) (regexp-match named-ellipsis-rx (symbol->string name)))
                     (record-binder name under assignments)
                     assignments))]
              [(? list?)
               (foldl (λ (pat asgns) (recur pat under asgns)) assignments pat)]
              [_ assignments]))])
    (make-immutable-hasheq (hash-map assignments (λ (cls _) (cons cls (find cls assignments)))))))

(define (reassign-classes pattern)
  (let* ([reassignments (class-reassignments pattern)]
         [rewrite (λ (c) (make-class (hash-ref reassignments (class-id c) (class-id c))))])
    (let recur ([pat pattern])
      (match pat
        [(struct ellipsis (name sub-pat class vars))
         (make-ellipsis name (recur sub-pat) (rewrite class)
                        (map (λ (v) (if (class? v) (rewrite v) v)) vars))]
        [(? list?) (map recur pat)]
        [_ pat]))))

;; used in generating the `any' pattern
(define-language sexp (sexp variable string number hole (sexp ...)))

(define-for-syntax (metafunc name)
  (let ([tf (syntax-local-value name (λ () #f))])
    (and (term-fn? tf) (term-fn-get-id tf))))

(define-for-syntax (metafunc/err name stx)
  (let ([m (metafunc name)])
    (if m m (raise-syntax-error #f "not a metafunction" stx name))))

(define-for-syntax (term-generator lang pat what)
  (with-syntax ([pattern 
                 (rewrite-side-conditions/check-errs 
                  (language-id-nts lang what)
                  what #t pat)])
    #`((compile #,lang '#,what) `pattern)))

(define-syntax (generate-term stx)
  (syntax-case stx ()
    [(_ lang pat size . kw-args)
     (with-syntax ([generator (syntax/loc stx (generate-term lang pat))])
       (syntax/loc stx
         (generator size . kw-args)))]
    [(name lang pat)
     #`(let ([generate #,(term-generator #'lang #'pat (syntax-e #'name))])
         (with-contract
          name #:result 
          (->* (natural-number/c)
               (#:attempt-num natural-number/c #:retries natural-number/c)
               any)
          (λ (size #:attempt-num [attempt-num 1] #:retries [retries default-retries])
            (let-values ([(term _) (generate size attempt-num retries)])
              term))))]))

(define-for-syntax (show-message stx)
  (syntax-case stx ()
    [(what . _)
     (identifier? #'what)
     (with-syntax ([loc (if (and (path? (syntax-source stx))
                                 (syntax-line stx))
                            (format "~a:~a"
                                    (path->string (syntax-source stx)) 
                                    (syntax-line stx))
                            #f)])
       #`(λ (msg)
           (fprintf 
            (current-output-port)
            "~a: ~a~a"
            'what (if loc (string-append loc "\n") "") msg)))]))

(define-for-syntax attempts-keyword
  (list '#:attempts #'default-check-attempts
        (list #'natural-number/c "#:attempts argument")))
(define-for-syntax source-keyword
  (list '#:source #f))
(define-for-syntax retries-keyword
  (list '#:retries #'default-retries 
        (list #'natural-number/c "#:retries argument")))
(define-for-syntax print?-keyword
  (list '#:print? #t))
(define-for-syntax attempt-size-keyword
  (list '#:attempt-size #'default-attempt-size 
        (list #'attempt-size/c "#:attempt-size argument")))
(define-for-syntax (prepare-keyword lists?)
  (list '#:prepare #f 
        (list (if lists? #'(-> list? list?) #'(-> any/c any/c)) 
              "#:prepare argument")))

(define-syntax (redex-check stx)
  (syntax-case stx ()
    [(form lang pat property . kw-args)
     (let-values ([(names names/ellipses) 
                   (extract-names (language-id-nts #'lang 'redex-check)
                                  'redex-check #t #'pat)]
                  [(attempts-stx source-stx retries-stx print?-stx size-stx fix-stx)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               source-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #f))
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))])
       (with-syntax ([(name ...) names]
                     [(name/ellipses ...) names/ellipses]
                     [show (show-message stx)])
         (with-syntax ([property (syntax
                                  (bind-prop
                                   (λ (bindings)
                                     (term-let ([name/ellipses (lookup-binding bindings 'name)] ...)
                                               property))))])
           (quasisyntax/loc stx
             (let ([att #,attempts-stx]
                   [ret #,retries-stx]
                   [print? #,print?-stx]
                   [fix #,fix-stx]
                   [term-match (λ (generated)
                                 (cond [(test-match lang pat generated) => values]
                                       [else (redex-error 'redex-check "~s does not match ~s" generated 'pat)]))])
               (parameterize ([attempt->size #,size-stx])
               #,(if source-stx
                     #`(let-values ([(metafunc/red-rel num-cases) 
                                     #,(cond [(and (identifier? source-stx) (metafunc source-stx))
                                              => (λ (x) #`(values #,x (length (metafunc-proc-cases #,x))))]
                                             [else
                                              #`(let ([r #,(apply-contract #'reduction-relation? source-stx 
                                                                           "#:source argument" (syntax-e #'form))])
                                                  (values r (length (reduction-relation-make-procs r))))])])
                         (check-lhs-pats
                          lang
                          metafunc/red-rel
                          property
                          (max 1 (floor (/ att num-cases)))
                          ret
                          'redex-check
                          (and print? show)
                          fix
                          #:term-match term-match))
                     #`(check-one
                        #,(term-generator #'lang #'pat 'redex-check)
                        property att ret (and print? show) fix (and fix term-match)))))))))]))

(define (format-attempts a)
  (format "~a attempt~a" a (if (= 1 a) "" "s")))

(define (check-one generator property attempts retries show term-fix term-match) 
  (let ([c (check generator property attempts retries show 
                  #:term-fix term-fix
                  #:term-match term-match)])
    (if (counterexample? c)
        (unless show c) ; check printed it
        (if show
            (show (format "no counterexamples in ~a\n"
                          (format-attempts attempts)))
            #t))))

(define-struct (exn:fail:redex:test exn:fail:redex) (source term))
(define-struct counterexample (term) #:transparent)

(define-struct term-prop (pred))
(define-struct bind-prop (pred))

(define (check generator property attempts retries show
               #:source [source #f]
               #:term-fix [term-fix #f]
               #:term-match [term-match #f])
  (let loop ([remaining attempts])
    (if (zero? remaining)
        #t
        (let ([attempt (add1 (- attempts remaining))])
          (let-values ([(term bindings) (generator ((attempt->size) attempt) attempt retries)]
                       [(handler) 
                        (λ (action term)
                          (λ (exn)
                            (let ([msg (format "~a ~s raises an exception" action term)])
                              (when show (show (format "~a\n" msg)))
                              (raise 
                               (if show
                                   exn
                                   (make-exn:fail:redex:test
                                    (format "~a:\n~a" msg (exn-message exn))
                                    (current-continuation-marks)
                                    exn
                                    term))))))])
            (let ([term (with-handlers ([exn:fail? (handler "fixing" term)])
                          (if term-fix (term-fix term) term))])
              (if (if term-match
                      (let ([bindings (make-bindings 
                                       (match-bindings
                                        (pick-from-list (term-match term))))])
                        (with-handlers ([exn:fail? (handler "checking" term)])
                          (match property
                            [(term-prop pred) (pred term)]
                            [(bind-prop pred) (pred bindings)])))
                      (with-handlers ([exn:fail? (handler "checking" term)])
                        (match (cons property term-fix)
                          [(cons (term-prop pred) _) (pred term)]
                          [(cons (bind-prop pred) #f) (pred bindings)])))
                  (loop (sub1 remaining))
                  (begin
                    (when show
                      (show
                       (format "counterexample found after ~a~a:\n"
                               (format-attempts attempt)
                               (if source (format " with ~a" source) "")))
                      (pretty-print term (current-output-port)))
                    (make-counterexample term)))))))))

(define (check-lhs-pats lang mf/rr prop attempts retries what show term-fix
                        #:term-match [term-match #f])
  (let ([lang-gen (compile lang what)])
    (let-values ([(pats srcs)
                  (cond [(metafunc-proc? mf/rr)
                         (values (map (λ (case) ((metafunc-case-lhs+ case) lang)) 
                                      (metafunc-proc-cases mf/rr))
                                 (metafunction-srcs mf/rr))]
                        [(reduction-relation? mf/rr)
                         (values (map (λ (rwp) ((rewrite-proc-lhs rwp) lang)) (reduction-relation-make-procs mf/rr))
                                 (reduction-relation-srcs mf/rr))])])
      (let loop ([pats pats] [srcs srcs])
        (if (and (null? pats) (null? srcs))
            (if show
                (show
                 (format "no counterexamples in ~a (with each clause)\n"
                         (format-attempts attempts)))
                #t)
            (let ([c (with-handlers ([exn:fail:redex:generation-failure?
                                      ; Produce an error message that blames the LHS as a whole.
                                      (λ (_)
                                        (raise-gen-fail what (format "LHS of ~a" (car srcs)) retries))])
                       (check
                        (lang-gen (car pats))
                        prop
                        attempts
                        retries
                        show
                        #:source (car srcs)
                        #:term-match term-match
                        #:term-fix term-fix))])
              (if (counterexample? c)
                  (unless show c)
                  (loop (cdr pats) (cdr srcs)))))))))

(define-syntax (check-metafunction stx)
  (syntax-case stx ()
    [(form name property . kw-args)
     (let-values ([(attempts retries print? size fix)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #t))
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))]
                  [(m) (metafunc/err #'name stx)])
       (quasisyntax/loc stx
         (parameterize ([attempt->size #,size])
           (let ([att #,attempts]
                 [ret #,retries]
                 [fix #,fix])
             (check-lhs-pats 
              (metafunc-proc-lang #,m)
              #,m
              (term-prop #,(apply-contract #'(-> (listof any/c) any) #'property #f (syntax-e #'form)))
              att
              ret
              'check-metafunction
              (and #,print? #,(show-message stx))
              fix)))))]))

(define (reduction-relation-srcs r)
  (map (λ (proc) (or (rewrite-proc-name proc)
                     (format "clause at ~a" (rewrite-proc-lhs-src proc))))
       (reduction-relation-make-procs r)))

(define (metafunction-srcs m)
  (map (compose (curry format "clause at ~a") metafunc-case-src-loc)
       (metafunc-proc-cases m)))

(define-syntax (check-reduction-relation stx)
  (syntax-case stx ()
    [(form relation property . kw-args)
     (let-values ([(attempts retries print? size fix)
                   (apply values
                          (parse-kw-args (list attempts-keyword
                                               retries-keyword
                                               print?-keyword
                                               attempt-size-keyword
                                               (prepare-keyword #f))
                                         (syntax kw-args)
                                         stx
                                         (syntax-e #'form)))])
       (quasisyntax/loc stx
         (parameterize ([attempt->size #,size])
           (let ([att #,attempts]
                 [ret #,retries]
                 [rel #,(apply-contract #'reduction-relation? #'relation #f (syntax-e #'form))]
                 [fix #,fix])
             (check-lhs-pats
              (reduction-relation-lang rel)
              rel
              (term-prop #,(apply-contract #'(-> any/c any) #'property #f (syntax-e #'form)))
              att
              ret
              'check-reduction-relation
              (and #,print? #,(show-message stx))
              fix)))))]))

(define-signature decisions^
  (next-variable-decision
   next-number-decision
   next-natural-decision
   next-integer-decision
   next-real-decision
   next-non-terminal-decision
   next-sequence-decision
   next-any-decision
   next-string-decision))

(define random-decisions@
  (unit (import) (export decisions^)
        (define (next-variable-decision) pick-var)
        (define (next-number-decision) pick-number)
        (define (next-natural-decision) pick-natural)
        (define (next-integer-decision) pick-integer)
        (define (next-real-decision) pick-real)
        (define (next-non-terminal-decision) pick-nts)
        (define (next-sequence-decision) pick-sequence-length)
        (define (next-any-decision) pick-any)
        (define (next-string-decision) pick-string)))

(define generation-decisions (make-parameter random-decisions@))

(provide redex-check
         generate-term
         check-reduction-relation
         check-metafunction
         default-attempt-size
         attempt-size/c
         exn:fail:redex:generation-failure?
         redex-pseudo-random-generator)

(provide (struct-out ellipsis) 
         (struct-out mismatch)
         (struct-out class)
         (struct-out binder)
         (struct-out rg-lang)
         (struct-out base-cases)
         (struct-out counterexample)
         (struct-out exn:fail:redex:test))

(provide pick-from-list pick-sequence-length pick-nts
         pick-char pick-var pick-string pick-any
         pick-number pick-natural pick-integer pick-real
         parse-pattern unparse-pattern
         parse-language prepare-lang
         class-reassignments reassign-classes
         default-retries proportion-at-size
         retry-threshold proportion-before-threshold post-threshold-incr
         is-nt? nt-by-name min-prods
         generation-decisions decisions^ 
         random-string
         sexp find-base-cases)
