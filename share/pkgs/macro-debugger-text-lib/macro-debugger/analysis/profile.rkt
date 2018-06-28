#lang racket/base
(require racket/list
         racket/match
         racket/dict
         racket/set
         racket/string
         racket/format
         racket/cmdline
         (rename-in racket/match [match-define defmatch])
         racket/pretty
         raco/command-name
         syntax/id-table
         syntax/modresolve
         syntax/modcode
         setup/collects ;; setup/path-to-relative
         macro-debugger/model/deriv
         "private/util.rkt")
(provide (all-defined-out))

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

(define (sqr x) (* x x))

(define (push! b x) (set-box! b (cons x (unbox b))))

;; enclosing-modpath : Parameterof ModulePath/#f
(define enclosing-modpath (make-parameter #f))

;; nice-modpath : ModulePath/ModulePathIndex [Datum/#f] -> Datum
(define (nice-modpath mod)
  (define rmp0
    (cond [(module-path-index? mod)
           (unless (enclosing-modpath)
             (error 'nice-modpath "enclosing modpath not set"))
           (resolve-module-path-index mod (enclosing-modpath))]
          [else (resolve-module-path mod)]))
  (let loop ([rmp rmp0])
    (match rmp
      [(? path?)
       (match (path->collects-relative rmp #:cache nice-modpath-cache)
         [(list* 'collects parts)
          (define (bytes->string b) (path->string (bytes->path b)))
          `(lib ,(string-join (map bytes->string parts) "/"))]
         [(? path? p) (path->string p)])]
      [(? symbol?) `(quote ,rmp)]
      [(list* 'submod "." submodnames)
       (match (and (enclosing-modpath) (nice-modpath (enclosing-modpath)))
         [(list* 'submod enc-base enc-submodnames)
          `(submod ,enc-base ,@(append enc-submodnames submodnames))]
         [(? values enc-base)
          `(submod ,enc-base ,@submodnames)]
         [#f (error 'nice-modpath "relative submod path: ~e => ~e" mod rmp)])]
      [(list* 'submod ".." submodnames)
       (error 'nice-modpath "relative (up) submod path: ~e => ~e" mod rmp)]
      [(list* 'submod base submodnames)
       `(submod ,(loop base) ,@submodnames)]
      [_
       (eprintf "nice-modpath: ~e, ~e\n" mod (enclosing-modpath))
       (eprintf "  rmp0 = ~e\n" rmp0)
       (eprintf "  rmp  = ~e\n" rmp)
       (error 'nice-modpath "invalid")])))

(define nice-modpath-cache (make-hash))

;; ============================================================
;; Raw Data Collection

;; A moc (macro occurrence) is (moc Context Nat Nat Integer)
(struct moc (ctx init-size final-size adj) #:prefab)
(define (moc-phase m) (fr-phase (car (moc-ctx m))))

;; adj is adjustment for local expansion, usually negative

;; Context is (listof Frame); Frame is (fr Id Nat)
;; (list (fr m2 0) (fr m1 0)) means an expansion of m2 at phase 0,
;; where the reference was produced by m1 at phase 0, which (probably)
;; occurred in the original program
(struct fr (id phase) #:prefab)

(define (fr=? f1 f2)
  (match* [f1 f2]
    [[(fr id1 ph1) (fr id2 ph2)]
     (and (= ph1 ph2) (free-identifier=? id1 id2 ph1))]))
(define (fr-hash-code f)
  (match f [(fr id ph) (+ ph (equal-hash-code (identifier-binding-symbol id ph)))]))

(define (context? v) (and (list? v) (andmap fr? v)))
(define (context=? ctx1 ctx2)
  (and (= (length ctx1) (length ctx2)) (andmap fr=? ctx1 ctx2)))
(define (context-hash-code ctx)
  (apply + (map fr-hash-code ctx)))

(define-custom-hash-types fr-hash #:key? fr?
  fr=? fr-hash-code)
(define-custom-hash-types ctx-hash #:key? context?
  context=? context-hash-code)

;; ----------------------------------------

;; A ScopeTable is Hash[ScopeInt => Context]

;; get-new-scope : Syntax Syntax Nat -> ScopeInt
(define (get-new-scope x mx phase)
  (define xi (get-macro-scopes x phase))
  (define mxi (get-macro-scopes mx phase))
  (define diff (set-subtract mxi xi))
  (match diff
    [(list scope) scope]
    [_ (error 'get-new-scope "bad scopes diff: ~e" diff)]))

;; get-macro-scopes : Syntax Nat -> (listof ScopeInt)
(define (get-macro-scopes x phase)
  (append* (for/list ([ph (in-range (add1 phase))])
             (for/list ([v (in-list (hash-ref (syntax-debug-info x ph) 'context))]
                        #:when (eq? (vector-ref v 1) 'macro))
               (vector-ref v 0)))))

;; get-macro-scope : Syntax ScopeTable Nat -> ScopeInt/#f
;; Returns the last macro scope present in the table.
(define (get-macro-scope x h phase)
  (define mscopes (sort (get-macro-scopes x phase) >))
  (for/first ([mscope (in-list mscopes)] #:when (hash-ref h mscope #f))
    mscope))

;; ----------------------------------------

;; phase : (Parameterof Nat)
(define phase (make-parameter 0))

;; moc-box : Parameterof (Boxof MOC)
(define moc-box (make-parameter #f))

;; profile/top : Deriv -> (Listof MOC)
(define (profile/top deriv)
  (define mocs (box null))
  (parameterize ((phase 0))
    (profile/deriv deriv mocs (make-hash)))
  (unbox mocs))

;; profile/deriv : Derivation (Boxof MOC) -> Void
;; Record size deltas from all macro steps of deriv in profinfo.
(define (profile/deriv deriv mocs scope=>context)
  (define (recur . ds)
    (for ([d (in-list ds)])
      (cond [(list? d) (for-each recur d)]
            [else (profile/deriv d mocs scope=>context)])))
  (define (recur/phase-up . ds)
    (parameterize ((phase (add1 (phase)))) (recur ds)))
  ;; Handle individual variants
  (#%expression
    (match deriv
      ;; ====
      [(mrule z1 z2 rs ?1 me1 locals me2 ?2 etx next)
       (define macro-id (and (pair? rs) (resolves->macro-id rs (phase))))
       (define macro-scope (and z1 me1 (get-new-scope z1 me1 (phase))))
       (define z1-scope (get-macro-scope z1 scope=>context (phase)))
       (define context (hash-ref scope=>context z1-scope null))
       (define context* (cons (fr macro-id (phase)) context))
       (when (and macro-id macro-scope)
         (hash-set! scope=>context macro-scope context*))
       (recur locals next)
       (when macro-id
         (define adj (apply + (map profile/local (or locals null))))
         (push! mocs (moc context* (term-size z1) (term-size etx) adj))
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
      [(p:module z1 z2 rs ?1 locals tag rename check tag2 check2 ?3 body shift)
       (recur locals check check2 body)]
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
      [(p:letrec-syntaxes+values z1 _ rs _ srenames prep sbindrhss vrenames vrhss body tag)
       (recur prep sbindrhss vrhss body)
       (when tag ;; means syntax bindings get dropped
         (define rhss-size
           (for/sum ([bind (in-list (or sbindrhss null))])
             (+ (term-size (node-z2 (bind-syntaxes-rhs bind)))
                ;; 2 for (svars . (srhs . ())) pairs
                ;; FIXME: also count svars term-size (in srename?)
                2)))
         (define lsv-id (and (pair? rs) (resolves->macro-id rs (phase))))
         (define z1-scope (get-macro-scope z1 scope=>context (phase)))
         (define context (hash-ref scope=>context z1-scope null))
         (define context* (cons (fr lsv-id (phase)) context))
         (when lsv-id
           (push! mocs (moc context* 0 0 (- rhss-size)))
           (when #f
             (eprintf "* lsv-id-id = ~e\n" lsv-id)
             (eprintf "  ctx       = ~e\n" context))))]
      [(p:provide _ _ _ _ inners ?2)
       (recur inners)]
      [(p:require _ _ _ _ locals)
       (recur locals)]
      [(p:submodule _ _ _ _ exp)
       (recur exp)]
      [(p:submodule* _ _ _ _)
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

(define (resolves->macro-id rs phase)
  ;; For application, want #%app, not procedure name; this might not
  ;; be optimal for rename-transformers, though.
  (define mlast (last rs))
  (for/first ([id (in-list rs)] #:when (free-identifier=? id mlast phase)) id))

;; ============================================================
;; Processing

;; ProfInfo =
;;   (profinfo Dict[Frame => (Listof Integer)] Dict[Frame => Integer] Integer Integer)
(struct profinfo (init-size final-size mocs) #:mutable)

(define (new-profinfo) (profinfo 0 0 null))

(define (profinfo-update! pi init-size final-size mocs)
  (set-profinfo-init-size! pi (+ (profinfo-init-size pi) init-size))
  (set-profinfo-final-size! pi (+ (profinfo-final-size pi) final-size))
  (set-profinfo-mocs! pi (append mocs (profinfo-mocs pi))))

;; print-profinfo : ProfInfo
;;                  #:sort (U 'total 'direct 'totalmean 'directmean)
;;                  #:excludes (Listof (Id Nat -> Boolean))
;;               -> Void
(define (print-profinfo pr
                        #:sort [sort-order 'total]
                        #:excludes [excludes null])
  (match-define (profinfo init-size final-size mocs) pr)
  (printf "Initial code size: ~s\n" init-size)
  (printf "Final code size  : ~s\n" final-size)
  (printf "========================================\n")
  (define phases (hash-keys (for/hash ([m (in-list mocs)]) (values (moc-phase m) #t))))
  (for ([phase (in-list (sort phases <))])
    (define entries (mocs->profile-entries mocs phase))
    (printf "Phase ~s\n" phase)
    (for-each print-entry (sort entries > #:key (sort-order->key sort-order)))
    (printf "----------------------------------------\n\n")))

;; ProfileEntry[K] = (list K IndirectStats DirectStats)
;; DirectStats = (list Integer Integer Nat Nat Real)
;;   -- total, mean (rounded), count, stddev
;; An IndirectStats is Integer -- total
(define (pe->total pe) (second pe))
(define (pe->direct pe) (first (third pe)))
(define (pe->count pe) (third (third pe)))
(define (pe->totalmean pe) (/ (pe->total pe) (pe->count pe)))
(define (pe->directmean pe) (/ (pe->direct pe) (pe->count pe)))

;; mocs->profile-entries : (Listof MOC) -> (Listof ProfileEntry)
(define (mocs->profile-entries mocs phase)
  (define direct-d (make-mutable-fr-hash))
  (define indirect-d (make-mutable-fr-hash))
  (for ([m (in-list mocs)] #:when (= (moc-phase m) phase))
    (match-define (moc ctx init-size final-size adj) m)
    (match-define (cons fr0 ctx*) ctx)
    (define delta (+ (- final-size init-size) adj))
    (dict-set! direct-d fr0 (cons delta (dict-ref direct-d fr0 null)))
    (for ([f (in-list (remove-duplicates ctx fr=?))])
      (dict-set! indirect-d f (+ (dict-ref indirect-d f 0) delta))))
  (for/list ([(f directs) (in-dict direct-d)])
    (define indirect (dict-ref indirect-d f 0))
    (list f indirect (deltas->direct-stats directs))))

;; deltas->direct-stats : (Listof Integer) -> (List Int Int Nat Real)
(define (deltas->direct-stats deltas)
  (define sum (apply + deltas))
  (define count (length deltas))
  (define mean (/ sum count))
  (define imean (exact->inexact mean))
  (define var (/ (for/sum ([delta (in-list deltas)]) (sqr (- delta imean))) count))
  (list sum (round mean) count (sqrt var)))

;; sort-order->key : Symbol -> (ProfileEntry -> Real)
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

;; print-entry : ProfileEntry[Frame] -> Void
(define (print-entry e)
  (match-define (list key indirect (list* dtotal dmean dcount dstddev _)) e)
  (unless (and (zero? indirect) (zero? dtotal))
    (printf "~a\n" (frame->string key))
    (printf "  total: ~s, mean: ~s\n"
            indirect (round (/ indirect dcount)))
    (printf "  direct: ~s, mean: ~s, count: ~s, stddev: ~a\n"
            dtotal dmean dcount (~r #:precision 2 dstddev))))

;; frame->string : Frame -> String
(define (frame->string f)
  (match-define (fr id phase) f)
  (id->string id phase))

;; id->string : Identifier Nat -> String
(define (id->string id phase)
  (match (identifier-binding id phase)
    [(list* def-mpi def-sym nom-mpi nom-sym def-phase _)
     (define at-phase (if (zero? def-phase) "" (format " at phase ~s" def-phase)))
     (define at-src (mpi->module-path def-mpi))
     (cond [(eq? def-sym nom-sym)
            (format "~s (defined in ~s~a)" def-sym at-src at-phase)]
           [else
            (format "~s (defined as ~s in ~s~a)" nom-sym def-sym at-src at-phase)])]
    ['lexical
     (format "~s (lexical)" (syntax-e id))]
    [#f
     (format "~s (top-level)" (syntax-e id))]))

;; ============================================================
;; External representations

;; mod->external : ModulePath/ModulePathIndex -> String
(define (mod->external m) (format "~s" (mod->external* m)))

;; mod->external* : ModulePath/ModulePathIndex -> Datum
(define (mod->external* m)
  (or (hash-ref mod->external-cache m #f)
      (let ([ext (mod->external** m)])
        (hash-set! mod->external-cache m ext)
        ext)))
(define mod->external-cache (make-weak-hash))
(define (mod->external** m) (nice-modpath m))

;; frame->external : Frame -> (list String*3)
(define (frame->external f)
  (or (dict-ref frame->external-cache f #f)
      (let ([ext (frame->external* f)])
        (dict-set! frame->external-cache f ext)
        ext)))
(define frame->external-cache (make-weak-fr-hash))
(define (frame->external* f)
  (match f [(fr id phase) (map (lambda (v) (format "~s" v)) (id->external id phase))]))

;; id->external : Identifier Nat ModulePath -> (list Datum*3)
(define (id->external id phase)
  (match (identifier-binding id phase)
    [(list* def-mpi def-sym nom-mpi nom-sym def-phase _)
     (define src
       (cond [(here-mpi? def-mpi) (nice-modpath (enclosing-modpath))]
             [else (nice-modpath def-mpi)]))
     (list nom-sym src (list 'def def-sym def-phase))]
    ['lexical
     (list (syntax-e id) (nice-modpath (enclosing-modpath)) (list 'lex phase))]
    [#f
     (list (syntax-e id) 'top '(top))]))

;; ============================================================
;; DB Schema
(require db/base db/sqlite3)

(define SCHEMA-VERSION 1)

;; get-profile-db : Path -> DB
(define (get-profile-db db-file)
  (define db (sqlite3-connect #:database db-file #:mode 'create))
  (cond [(table-exists? db "racket_macro_profiler_meta")
         (define schema-version
           (query-maybe-value db "select value from racket_macro_profiler_meta where key = ?"
                              "schema version"))
         (unless (equal? schema-version SCHEMA-VERSION)
           (error 'profile "incompatible database file\n  file: ~e" db-file))]
        [else (call-with-transaction db (lambda () (setup-db db)))])
  db)

;; setup-db : DB -> Void
(define (setup-db db)
  ;; ---- meta ----
  (query-exec
   db (~a "create table racket_macro_profiler_meta "
          "(key text primary key, value any)"))
  (query-exec db "insert into racket_macro_profiler_meta (key, value) values (?, ?)"
              "schema version" SCHEMA-VERSION)
  ;; ---- raw data ----
  (query-exec
   db (~a "create table id_module "
          "(id integer primary key, mod text, unique (mod))"))
  (query-exec
   db (~a "create table id_macro "
          "(id integer primary key, m_sym text, m_src text, m_etc text, "
          "unique (m_sym, m_src, m_etc))"))
  (query-exec
   db (~a "create table mocs "
          "(expmod integer, ctr integer, start integer, end integer, adj integer, "
          "primary key (expmod, ctr), "
          "foreign key (expmod) references id_module (id) on delete cascade)"))
  (query-exec
   db (~a "create table mocctx "
          "(expmod integer, ctr integer, depth integer, macro integer, phase integer, "
          "primary key (expmod, ctr, depth), "
          "foreign key (expmod, ctr) references mocs (expmod, ctr) on delete cascade, "
          "foreign key (macro) references id_macro (id) on delete cascade)"))
  ;; ---- views ----
  (query-exec
   db (~a "create view mocs_direct "
          "as select expmod, ctr, macro, phase, (end - start + adj) as cost "
          "from mocs natural inner join mocctx "
          "where mocctx.depth = 0"))
  (query-exec
   db (~a "create view mocs_indirect "
          "as select distinct expmod, ctr, macro, phase, (end - start + adj) as cost "
          "from mocs natural inner join mocctx"))
  (query-exec
   db (~a "create view cost_direct as "
          "select macro, phase, "
          " sum(cost) as dtotal, count(ctr) as dcount, avg(cost) as dmean "
          "from mocs_direct group by macro, phase"))
  (query-exec
   db (~a "create view cost_indirect as "
          "select macro, phase, sum(cost) as itotal "
          "from mocs_indirect group by macro, phase"))
  (query-exec
   db (~a "create view cost_summary_pre as "
          "select macro, phase, "
          " dcount, dtotal, dmean, itotal, (1.0 * itotal / dcount) as imean "
          "from cost_direct natural inner join cost_indirect"))
  (query-exec
   db (~a "create view cost_summary as "
          "select m_sym, m_src, m_etc, phase, dcount, dtotal, dmean, itotal, imean "
          "from cost_summary_pre inner join id_macro on (cost_summary_pre.macro = id_macro.id)"))
  )

(define frame=>key (make-weak-fr-hash))
(define (frame->key db f)
  (define (notfound)
    (match-define (list f-sym f-src f-etc) (frame->external f))
    (or (query-maybe-value db "select id from id_macro where m_sym = ? and m_src = ? and m_etc = ?"
                           f-sym f-src f-etc)
        (let ([next (add1 (query-value db "select coalesce(max(id),0) from id_macro"))])
          (query-exec db "insert into id_macro (id, m_sym, m_src, m_etc) values (?,?,?,?)"
                      next f-sym f-src f-etc)
          next)))
  (dict-ref! frame=>key f notfound))

(define mod=>key (make-weak-hash))
(define (mod->key db m)
  (define (notfound)
    (define ext (mod->external m))
    (or (query-maybe-value db "select id from id_module where mod = ?" ext)
        (let ([next (add1 (query-value db "select coalesce(max(id),0) from id_module"))])
          (query-exec db "insert into id_module (id, mod) values (?,?)" next ext)
          next)))
  (dict-ref! mod=>key m notfound))

;; db-update! : DB ModulePath (Listof MOC) -> ProfInfo
(define (db-update! db modpath mocs)
  (define mod-id (mod->key db modpath))
  (query-exec db "delete from mocs where expmod = ?" mod-id)
  (query-exec db "delete from mocctx where expmod = ?" mod-id)
  (for ([m (in-list mocs)] [ctr (in-naturals)])
    (match-define (moc ctx init final adj) m)
    (query-exec db "insert into mocs (expmod, ctr, start, end, adj) values (?,?,?,?,?)"
                mod-id ctr init final adj)
    (for ([f (in-list ctx)] [depth (in-naturals)])
      (query-exec db "insert into mocctx (expmod, ctr, depth, macro, phase) values (?,?,?,?,?)"
                  mod-id ctr depth (frame->key db f) (fr-phase f)))))

;; db-has-mod? : DB ModulePath -> Boolean
(define (db-has-mod? db rmodpath)
  (define mod-ext (mod->external rmodpath))
  (cond [(query-maybe-value db "select id from id_module where mod = ?" mod-ext)
         => (lambda (mod-id)
              (not (zero? (query-value db "select count(*) from mocs where expmod = ?" mod-id))))]
        [else #f]))

;; ============================================================

(module+ main
  (define mode 'auto)
  (define the-sort-order 'total)
  (define the-excludes null)
  (define print-summary? #t)
  (define the-db-file #f)
  (define always-update-db? #t)

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

  (define (process-mod rmodpath db profinfo)
    (parameterize ((enclosing-modpath rmodpath))
      (printf "profiling ~s\n" (nice-modpath rmodpath))
      (define-values (compiled deriv) (get-module-code/trace rmodpath))
      (define mocs (profile/top deriv))
      (when profinfo
        (profinfo-update! profinfo
                          (term-size (node-z1 deriv))
                          (term-size (node-z2 deriv))
                          mocs))
      (when db
        (call-with-transaction db
          (lambda () (db-update! db rmodpath mocs))))))

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
   [("-q" "--quiet")
    "Do not print summary"
    (set! print-summary? #f)]
   [("-d" "--database") db-file
    "Store profile information in sqlite3 db file"
    (set! the-db-file db-file)]
   [("-t" "--trust")
    "Trust existing profile information in the db file"
    (set! always-update-db? #f)]
   #:multi
   [("-x" "--exclude") prefix "Exclude macros defined in modules starting with <prefix> from output"
    (set! the-excludes (cons (make-exclude prefix) the-excludes))]
   #:args module-path
   (let ()
     (define modpaths (map ->modpath module-path))
     (define db (and the-db-file (get-profile-db the-db-file)))
     (define profinfo (and print-summary? (new-profinfo)))
     ;; ----
     (for ([modpath (in-list modpaths)])
       (define rmodpath (resolve-module-path modpath))
       (cond [(or profinfo always-update-db? (not (db-has-mod? db rmodpath)))
              (with-handlers ([exn:fail?
                               (lambda (e)
                                 (eprintf "ERROR processing ~e\n" rmodpath)
                                 ((error-display-handler) (exn-message e) e))])
                (process-mod rmodpath db profinfo))]
             [else (eprintf "skipping ~s\n" (nice-modpath rmodpath))]))
     ;; ----
     (when db
       (disconnect db))
     (when profinfo
       (print-profinfo profinfo #:sort the-sort-order #:excludes the-excludes))))
  (void))
