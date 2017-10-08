#lang racket/base
(require racket/list
         racket/set
         racket/string
         racket/cmdline
         (rename-in racket/match [match-define defmatch])
         racket/pretty
         raco/command-name
         syntax/id-table
         syntax/modresolve
         syntax/modcode
         macro-debugger/model/deriv
         "private/util.rkt")

;; term-size : Syntaxish -> Nat
;; Rough measure of the size of a term.
(define term-size
  (let ([memo (make-weak-hasheq)])
    (lambda (x)
      (hash-ref! memo x
                 (lambda ()
                   (cond [(syntax? x) (term-size (syntax-e x))]
                         [(pair? x) (+ 1 (term-size (car x)) (term-size (cdr x)))]
                         [(vector? x)
                          (+ 1 (for/sum ([y (in-vector x)]) (term-size y)))]
                         [(prefab-struct-key x)
                          (+ 1 (for/sum ([y (in-vector (struct->vector x) 1)])
                                 (term-size y)))]
                         [(box? x)
                          (+ 1 (term-size (unbox x)))]
                         [(hash? x)
                          (+ 1 (for/sum ([(k v) (in-hash x)])
                                 (+ (term-size k) (term-size v))))]
                         [else 1]))))))

;; ----

;; A ProfInfo is (profinfo Hash[Nat => (Listof Occ)] Nat Nat)
;; The hash maps each phase to a DeltaTable.

;; An Occ is (list Context Integer)
;;   -- code size delta (|expanded| - |original|)
(struct profinfo (phases init-size final-size) #:mutable)

(define (make-profinfo) (profinfo (make-hash) 0 0))

(define (profinfo-add! profinfo phase occ)
  (define phases (profinfo-phases profinfo))
  (hash-set! phases phase (cons occ (hash-ref phases phase null))))

;; ----

;; profile : (Listof ModulePath) -> ProfInfo
(define (profile modpaths)
  (define profinfo (make-profinfo))
  (for ([modpath (in-list modpaths)])
    (define-values (compiled deriv) (get-module-code/trace modpath))
    (set-profinfo-init-size! profinfo
      (+ (term-size (node-z1 deriv)) (profinfo-init-size profinfo)))
    (set-profinfo-final-size! profinfo
      (+ (term-size (node-z2 deriv)) (profinfo-final-size profinfo)))
    (profile/deriv deriv profinfo))
  (profinfo->profile profinfo))

;; ----

;; A Profile is (list Nat Nat Hash[Nat => PhaseProfile])
;; A PhaseProfile is (Listof ProfileEntry)
;; A ProfileEntry is (list Identifier IndirectStats DirectStats)
;; A DirectStats is (list Nat Nat Nat Real (Listof Nat))
;;   -- total, mean (rounded), count, stddev, raw data
;; An IndirectStats is Nat -- total

(define (pe->total pe) (second pe))
(define (pe->direct pe) (first (third pe)))
(define (pe->count pe) (third (third pe)))
(define (pe->totalmean pe) (/ (pe->total pe) (pe->count pe)))
(define (pe->directmean pe) (/ (pe->direct pe) (pe->count pe)))

(define (profinfo->profile profinfo)
  (list (profinfo-init-size profinfo)
        (profinfo-final-size profinfo)
        (for/hash ([(ph occs) (in-hash (profinfo-phases profinfo))])
          (parameterize ((phase ph))
            (define direct (occs->direct-table occs))
            (define indirect (occs->indirect-table occs))
            (define entries
              (for/list ([(id direct) (in-free-id-table direct)])
                (list id (free-id-table-ref indirect id 0) direct)))
            (values ph entries)))))

;; A DirectTable is FreeIdTable[DirectStats]
;; An IndirectTable is FreeIdTable[IndirectStats]

(define (occs->direct-table occs)
  (define t (make-free-id-table))
  (for ([occ (in-list occs)])
    (defmatch (list (cons macro-id context) delta) occ)
    (free-id-table-set! t macro-id (cons delta (free-id-table-ref t macro-id null))))
  (define dt (make-free-id-table))
  (for ([(id deltas) (in-free-id-table t)])
    (free-id-table-set! dt id (deltas->direct-stats deltas)))
  dt)

(define (deltas->direct-stats deltas)
  (define sum (apply + deltas))
  (define count (length deltas))
  (define mean (/ sum count))
  (define imean (exact->inexact mean))
  (define var (/ (for/sum ([delta (in-list deltas)]) (sqr (- delta imean))) count))
  (list sum (round mean) count (sqrt var) deltas))

(define (occs->indirect-table occs)
  (define it (make-free-id-table))
  (for ([occ (in-list occs)])
    (defmatch (list context0 delta) occ)
    (defmatch (cons macro-id context) (remove-duplicates context0 id/f=?))
    ;; Now context has no duplicates and does not contain macro-id.
    (for ([ctx-id (in-list (cons macro-id context))])
      (free-id-table-set! it ctx-id (+ delta (free-id-table-ref it ctx-id 0)))))
  it)

(define (id/f=? a b)
  (cond [(and (identifier? a) (identifier? b))
         (free-identifier=? a b (phase))]
        [else (eq? a b)]))

;; print-profile : Profile
;;                 #:sort (U 'total 'direct 'totalmean 'directmean)
;;                 #:excludes (Listof (Id Nat -> Boolean))
;;              -> Void
(define (print-profile pr
                       #:sort [sort-order 'total]
                       #:excludes [excludes null])
  (match pr
    [(list init-size final-size p)
     (printf "Initial code size: ~s\n" init-size)
     (printf "Final code size:   ~s\n" final-size)
     (for ([(ph pes) (in-hash p)])
       (printf "~a\n" (make-string 40 #\-))
       (printf "Phase ~s\n" ph)
       (parameterize ((phase ph))
         (for ([pe (in-list (sort pes > #:key (sort-order->key sort-order)))]
               #:unless (for/or ([exclude (in-list excludes)]) (exclude (car pe) ph)))
           (print-entry pe)))
       (printf "\n"))]))

(define (sort-order->key so)
  (case so
    [(total) pe->total]
    [(direct) pe->direct]
    [(totalmean) pe->totalmean]
    [(directmean) pe->directmean]))

;; make-exclude : String -> (Id Nat -> Boolean)
(define ((make-exclude prefix) id ph)
  (match (identifier-binding id ph)
    [(list* def-mpi def-sym _)
     (let loop ([mod (mpi->module-path def-mpi)])
       (match mod
         [(? string?) (string-prefix? mod prefix)]
         [(? symbol?) (loop (symbol->string mod))]
         [(? path?) (loop (path->string mod))]
         [(list 'submod mod _ ...) (loop mod)]
         [(list 'file mod) (loop mod)]
         [(list 'lib mod) (loop mod)]
         [_ #f]))]
    [_ #f]))

(define (print-entry e)
  (match e
    [(list id indirect (list dtotal dmean dcount dstddev ddeltas))
     (unless (and (zero? indirect) (zero? dtotal))
       (printf "~a\n" (id->string id))
       (printf "  total: ~s, mean: ~s\n"
               indirect (round (/ indirect dcount)))
       (printf "  direct: ~s, mean: ~s, count: ~s, stddev: ~s\n"
               dtotal dmean dcount dstddev))]))

(define (id->string id)
  (match (identifier-binding id (phase))
    [(list* def-mpi def-sym nom-mpi nom-sym def-phase _)
     (define at-phase
       (cond [(zero? def-phase) ""]
             [else (format " at phase ~s" def-phase)]))
     (cond [(eq? def-sym nom-sym)
            (format "~s (defined in ~s~a)"
                    def-sym (mpi->module-path def-mpi) at-phase)]
           [else
            (format "~s (defined as ~s in ~s~a)"
                    nom-sym def-sym (mpi->module-path def-mpi) at-phase)])]
    ['lexical
     (format "~s (lexical)" (syntax-e id))]
    [#f
     (format "~s (top-level)" (syntax-e id))]))

(define (sqr x) (* x x))

;; ----------------------------------------

;; phase : (Parameterof Nat)
(define phase (make-parameter 0))

;; scope=>context : Hash[Integer => (Listof Identifier)]
;; Maps macro scopes to contexts.
(define scope=>context (make-hash))

;; profile/deriv : Derivation ProfInfo -> Void
;; Record size deltas from all macro steps of deriv in profinfo.
(define (profile/deriv deriv profinfo)
  (define (recur . ds)
    (for ([d (in-list ds)])
      (cond [(list? d) (for-each recur d)]
            [else (profile/deriv d profinfo)])))
  (define (recur/phase-up . ds)
    (parameterize ((phase (add1 (phase)))) (apply recur ds)))
  ;; Handle individual variants
  (#%expression
    (match deriv
      ;; ====
      [(mrule z1 z2 rs ?1 me1 locals me2 ?2 etx next)
       (define macro-id (and (pair? rs) (car rs)))
       (define macro-scope (and z1 me1 (get-new-scope z1 me1)))
       (define z1-scope (get-macro-scope z1))
       (define context (hash-ref scope=>context z1-scope null))
       (when (and macro-id macro-scope)
         (hash-set! scope=>context macro-scope (cons macro-id context)))
       (recur locals next)
       (when macro-id
         ;; FIXME: macro gets "charged" for local-expansion!
         (define delta (- (term-size etx) (term-size z1)))
         (define adj (apply + (map profile/local (or locals null))))
         (profinfo-add! profinfo (phase) (list (cons macro-id context) (+ delta adj)))
         (when #f
           (eprintf "* macro-id ~e\n" macro-id)
           (eprintf "  ctx = ~e\n" context)
           (eprintf "  from (~s): ~e\n" (term-size z1) (syntax->datum z1))
           (eprintf "  to   (~s): ~e\n" (term-size etx) (syntax->datum etx))
           (unless (zero? adj)
             (eprintf "  with local adjustment: ~s\n" adj))))]
      ;; ====
      [(lift-deriv z1 z2 first lift-stx second)
       (recur first second)]
      [(tagrule z1 z2 tagged-stx next)
       (recur next)]
      [(lift/let-deriv z1 z2 first lift-stx second)
       (recur first second)]
      [(local-exn exn)
       (void)]
      [(local-expansion z1 z2 for-stx? me1 inner lifted me2 opaque)
       (if for-stx? (recur/phase-up inner) (recur inner))]
      [(local-lift expr ids)
       (void)]
      [(local-lift-end decl)
       (void)]
      [(local-lift-require req expr mexpr)
       (void)]
      [(local-lift-provide prov)
       (void)]
      [(local-bind names ?1 renames bindrhs)
       (recur bindrhs)]
      [(local-value name ?1 resolves bound? binding)
       (void)]
      [(track-origin before after)
       (void)]
      [(local-remark contents)
       (void)]
      [(p:variable z1 z2 rs ?1)
       (void)]
      [(p:module z1 z2 rs ?1 locals tag rename check tag2 ?3 body shift)
       (recur locals check body)]
      [(p:#%module-begin z1 z2 rs ?1 me body ?2 subs)
       (recur body subs)]
      [(p:define-syntaxes z1 z2 rs ?1 prep rhs locals)
       (recur prep locals)
       (recur/phase-up rhs)]
      [(p:define-values z1 z2 rs ?1 rhs)
       (recur rhs)]
      [(p:begin-for-syntax z1 z2 rs ?1 prep body locals)
       (recur prep locals)
       (recur/phase-up body)]
      [(p:#%expression z1 z2 rs ?1 inner untag)
       (recur inner)]
      [(p:if z1 z2 rs ?1 test then else)
       (recur test then else)]
      [(p:wcm z1 z2 rs ?1 key mark body)
       (recur key mark body)]
      [(p:set! _ _ _ _ id-resolves ?2 rhs)
       (recur rhs)]
      [(p:set!-macro _ _ _ _ deriv)
       (recur deriv)]
      [(p:#%app _ _ _ _ lderiv)
       (recur lderiv)]
      [(p:begin _ _ _ _ lderiv)
       (recur lderiv)]
      [(p:begin0 _ _ _ _ first lderiv)
       (recur first lderiv)]
      [(p:lambda _ _ _ _ renames body)
       (recur body)]
      [(p:case-lambda _ _ _ _ renames+bodies)
       (recur renames+bodies)]
      [(p:let-values _ _ _ _ renames rhss body)
       (recur rhss body)]
      [(p:letrec-values _ _ _ _ renames rhss body)
       (recur rhss body)]
      [(p:letrec-syntaxes+values _ _ _ _ srenames prep sbindrhss vrenames vrhss body tag)
       (recur prep sbindrhss vrhss body)]
      [(p:provide _ _ _ _ inners ?2)
       (recur inners)]
      [(p:require _ _ _ _ locals)
       (recur locals)]
      [(p:submodule _ _ _ _ exp) ;; FIXME!
       (recur exp)]
      [(p:submodule* _ _ _ _) ;; FIXME!
       (void)]
      [(p:#%stratified-body _ _ _ _ bderiv)
       (recur bderiv)]
      [(p:stop _ _ _ _) (void)]
      [(p:unknown _ _ _ _) (void)]
      [(p:#%top _ _ _ _) (void)]
      [(p:#%datum _ _ _ _) (void)]
      [(p:quote _ _ _ _) (void)]
      [(p:quote-syntax z1 z2 _ _) (void)]
      [(p:#%variable-reference _ _ _ _) (void)]
      [(lderiv _ _ ?1 derivs)
       (recur derivs)]
      [(bderiv _ _ _ pass1 trans pass2)
       (recur pass1 pass2)]
      [(b:error ?1) (void)]
      [(b:expr head)
       (recur head)]
      [(b:splice head ?1 tail ?2)
       (recur head)]
      [(b:defvals head ?1 rename ?2)
       (recur head)]
      [(b:defstx head ?1 rename ?2 prep bindrhs)
       (recur head prep bindrhs)]
      [(bind-syntaxes rhs locals)
       (recur/phase-up rhs)
       (recur locals)]
      [(clc ?1 renames body)
       (recur body)]
      [(module-begin/phase pass1 pass2 pass3)
       (recur pass1 pass2 pass3)]
      [(mod:prim head rename prim)
       (recur head prim)]
      [(mod:splice head rename ?1 tail)
       (recur head)]
      [(mod:lift head locals renames tail)
       (recur head locals)]
      [(mod:lift-end tail)
       (void)]
      [(mod:cons head locals)
       (recur head locals)]
      [(mod:skip)
       (void)]
      ;; Shouldn't occur in module expansion.
      ;; (Unless code calls 'expand' at compile-time; weird, but possible.)
      [(ecte _ _ locals first second locals2)
       (recur locals first second locals2)]
      [(bfs:lift lderiv lifts)
       (recur lderiv)]
      [#f
       (void)])))

;; profile/local : LocalAction -> Integer
;; Adjustment to mrule's delta due to local actions.
(define (profile/local l)
  (define adj
    (match l
      [(local-expansion z1 z2 for-stx? me1 inner lifted me2 opaque)
       ;; Assume z1 is from macro args, z2 appears in macro result.
       ;; Then macro is not responsible for the difference, so *subtract*
       ;; the delta z2-z1; equivalently, add z1-z2.
       (- (term-size z1) (term-size z2))]
      [(local-lift expr ids) ;; (define-values [] []) : 5 nodes
       (+ (term-size expr) (term-size ids))]
      [(local-lift-end decl)
       (term-size decl)]
      [(local-lift-require req expr mexpr) ;; (require []) : 4 nodes
       (+ 4 (term-size req))]
      [(local-lift-provide prov) ;; (provide []) : 4 nodes
       (+ 4 (term-size prov))]
      [_ ;; local-value, local-bind, etc
       0]))
  (when #f
    (unless (zero? adj)
      (eprintf "! adjust by ~s for ~e\n" adj l)))
  adj)

;; get-new-scope : Syntax Syntax -> Integer
(define (get-new-scope x mx)
  (define xi (get-macro-scopes x))
  (define mxi (get-macro-scopes mx))
  (define diff (set-subtract mxi xi))
  (match diff
    [(list scope) scope]
    [_ (error 'get-new-scope "bad scopes diff: ~e" diff)]))

(define (get-macro-scopes x)
  (sort (append*
         (for/list ([ph (in-range (add1 (phase)))])
           (define xi (hash-ref (syntax-debug-info x (phase)) 'context))
           (for/list ([v (in-list xi)]
                      #:when (eq? (vector-ref v 1) 'macro))
             (vector-ref v 0))))
        <))

(define (get-macro-scope x)
  (define mscopes (get-macro-scopes x))
  (and (pair? mscopes) (apply max mscopes)))

;; ====

(module+ main
  (define mode 'auto)
  (define the-sort-order 'total)
  (define the-excludes null)

  (define (->modpath x)
    (cond [(string? x)
           (case mode
             [(auto)
              (if (file-exists? x)
                  `(file ,x)
                  (read (open-input-string x)))]
             [(file) `(file ,x)]
             [(module-path)
              (read (open-input-string x))])]
          [else x]))

  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-f" "--file") "Interpret arguments as file-paths"
    (set! mode 'file)]
   [("-m" "--module-path") "Interpret arguments as module-paths"
    (set! mode 'module-path)]
   [("-s" "--sort") sort-order
    "Sort entries by <sort-order> (one of total, totalmean, direct, or directmean)"
    (let ([so (string->symbol sort-order)])
      (unless (memq so '(total totalmean direct directmean))
        (error 'profile "expected one of (total, totalmean, direct, or directmean) for sort order, given: ~a" so))
      (set! the-sort-order so))]
   #:multi
   [("-x" "--exclude") prefix "Exclude macros defined in modules starting with <prefix> from output"
    (set! the-excludes (cons (make-exclude prefix) the-excludes))]
   #:args module-path
   (let ()
     (print-profile (profile (map ->modpath module-path))
                    #:sort the-sort-order
                    #:excludes the-excludes)))
  (void))
