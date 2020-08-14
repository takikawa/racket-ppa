#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/stxparam
         racket/contract/base
         racket/list
         syntax/stx
         racket/match
         racket/pretty
         "deriv-util.rkt"
         "stx-util.rkt"
         "pattern.rkt"
         "context.rkt"
         "tracking.rkt"
         "steps.rkt")

(provide STRICT-CHECKS
         DEBUG)

(define-syntax-rule (STRICT-CHECKS form ...)
  (when #t form ... (void)))

(define-syntax-rule (DEBUG form ...)
  (when #f form ... (void)))

(define (hash-set-list h ks v)
  (for/fold ([h h]) ([k (in-list ks)]) (hash-set h k v)))
(define (hash-remove-list h ks)
  (for/fold ([h h]) ([k (in-list ks)]) (hash-remove h k)))

(define state/c (or/c state? #f))

;; ============================================================
;; Hiding configuration

(provide
 (contract-out
  [macro-policy (parameter/c (-> identifier? any/c))]))

(define macro-policy (make-parameter (lambda (id) #t)))

;; ============================================================
;; Expansion Context

(provide
 (contract-out
  [the-phase (parameter/c exact-nonnegative-integer?)]
  [the-context (parameter/c list?)]
  [the-big-context (parameter/c (listof bigframe?))]
  [call-with-initial-context (-> (-> any) #:xstate xstate? any)])
 honest?
 not-complete-fiction?)

(define the-phase (make-parameter 0))
(define the-context (make-parameter null))
(define the-big-context (make-parameter null))

(define the-vt (make-parameter #f))         ;; (Parameterof VT)
(define honesty (make-parameter 'T))        ;; (Parameterof HonestyMask)

(define (call-with-initial-context proc #:xstate xst)
  (parameterize ((the-xstate xst)
                 (the-phase 0)
                 (the-context null)
                 (the-big-context null)
                 (the-vt #f)
                 (honesty 'T))
    (proc)))

;; set-honesty : HonestyMask Stx -> Void
;; PRE: hm <= (honesty) -- that is, honesty is only decreased or left unchanged
;; Invariant: (honesty) = 'T  iff  (the-vt) = #f
(define (set-honesty hm f)
  (define current-hm (honesty))
  (DEBUG (unless (eq? (honesty) hm) (eprintf "set-honesty : ~s => ~s\n" (honesty) hm)))
  (unless (equal? current-hm hm)
    (when (eq? current-hm 'T) (the-vt (vt-base f)))
    (honesty hm)))

;; honest? : -> Boolean
(define (honest?) (eq? (honesty) 'T))

;; not-complete-fiction? : -> Boolean
(define (not-complete-fiction?) (not (eq? (honesty) 'F)))

;; ============================================================
;; Expansion State

(provide
 (struct-out xstate)
 (contract-out
  [the-xstate (parameter/c (or/c xstate? #f))]
  [new-xstate (-> xstate?)]
  [next-seqno (-> exact-nonnegative-integer?)]
  [add-step (->* [protostep?] [any/c] void?)])
 ;; FIXME
 learn-binders 
 learn-definites
 add-lift
 add-endlift
 get/clear-lifts
 get/clear-endlifts)

;; An XState is:
(struct xstate
  (seqno        ;; Nat
   binders      ;; ImmutableHasheq[Identifier => Phase]
   definites    ;; ImmutableHasheq[Identifier => Phase]
   lifts        ;; (Listof Lift)
   endlifts     ;; (Listof Syntax)
   frontier     ;; ImmutableHashEq[Syntax => #t]
   steps        ;; ReductionSequence
   all-steps    ;; (Listof AnnotatedStep) or #f
   ) #:transparent #:mutable)
;; where Lift = (list 'def Ids Syntax) | (list 'req Syntax) | (list 'mod Syntax)

;; An AnnotatedStep is (annotated Boolean HonestyMask Step)
(struct annotated (shown? hm step) #:transparent)

;; the-xstate : (Parameterof XState/#f)
(define the-xstate (make-parameter #f))

;; new-xstate : -> XState
(define (new-xstate)
  (xstate 0 '#hasheq() '#hasheq() null null '#hasheq() null #f))

;; next-seqno : -> Nat
(define (next-seqno #:xstate [xst (the-xstate)])
  (let ([n (xstate-seqno xst)]) (set-xstate-seqno! xst (add1 n)) n))

;; learn-{binders,definites} : Id/s -> Void
(define (learn-binders ids #:xstate [xst (the-xstate)])
  (set-xstate-binders! xst (hash-set-list (xstate-binders xst) (flatten-identifiers ids) (the-phase))))
(define (learn-definites ids #:xstate [xst (the-xstate)])
  (set-xstate-definites! xst (hash-set-list (xstate-definites xst) (flatten ids) (the-phase))))

;; add-lift : Lift -> Void
;; add-endlift : Syntax -> Void
(define (add-lift lift #:xstate [xst (the-xstate)])
  (let ([li (liftinfo lift (honesty) (the-vt))])
    (set-xstate-lifts! xst (cons li (xstate-lifts xst)))))
(define (add-endlift lift #:xstate [xst (the-xstate)])
  (let ([li (liftinfo lift (honesty) (the-vt))])
    (set-xstate-endlifts! xst (cons li (xstate-endlifts xst)))))

;; get/clear-lifts : -> (Listof Lift)
;; get/clear-endlifts : -> (Listof Syntax)
(define (get/clear-lifts #:xstate [xst (the-xstate)])
  (set-xstate-lifts! xst null))
(define (get/clear-endlifts #:xstate [xst (the-xstate)])
  (set-xstate-endlifts! xst null))

;; add-step : Step -> Void
(define (add-step step [add? (honest?)] #:xstate [xst (the-xstate)])
  (when add? (set-xstate-steps! xst (cons step (xstate-steps xst))))
  (let ([all-steps (xstate-all-steps xst)])
    (when all-steps (set-xstate-all-steps! xst (cons (annotated add? (honesty) step) all-steps)))))

;; ----------------------------------------
;; Lifts

(struct liftinfo (lift hm vt) #:prefab)

;; ============================================================
;; Creating steps

(provide
 immediate-frame
 (contract-out
  [current-state-with
   (-> syntaxish? syntaxish?
       state?)]
  [walk
   (->* [syntaxish? syntaxish? step-type?]
        [#:foci1 syntaxish? #:foci2 syntaxish?]
        step?)]
  [stumble
   (->* [syntaxish? exn?]
        [#:focus syntaxish?]
        misstep?)]
  [walk/talk
   (-> step-type? (listof (or/c syntax? string? 'arrow))
       protostep?)]
  [foci
   (-> any/c (listof syntax?))]))

(define (current-state-with e fs)
  (define xst (the-xstate))
  (let loop ([e e] [fs (foci fs)] [ctx (the-context)])
    (cond [(and (pair? ctx) (immediate-frame? (car ctx)))
           (define e* ((car ctx) e))
           (loop e*
                 (for/list ([f (in-list fs)])
                   (if (eq? f e) e* f))
                 (cdr ctx))]
          [else
           (make state e fs ctx (the-big-context)
                 (xstate-binders xst) (xstate-definites xst)
                 (xstate-frontier xst) (xstate-seqno xst))])))

;; An immediate-frame's procedure should not add syntactic structure; it should
;; only add/adjust syntax properties, rearm terms, etc. When the current state
;; is captured, any immediate frames at the top of the context are applied to
;; the visible term and any focus identical to the visible term. This preserves
;; eq? connections between term and foci; without it, whole-term steps lose
;; their focus highlighting. See add-rearm-frame in reductions.rkt

(struct immediate-frame (f)
  #:property prop:procedure (lambda (self x) ((immediate-frame-f self) x)))

(define (walk e1 e2 type
              #:foci1 [foci1 e1]
              #:foci2 [foci2 e2])
  (make step type
        (current-state-with e1 foci1)
        (current-state-with e2 foci2)))

(define (stumble stx exn #:focus [focus stx])
  (make misstep 'error (current-state-with stx focus) exn))

(define (walk/talk type contents)
  (make remarkstep type (current-state-with #f null) contents))

(define (foci x) (filter syntax? (flatten x)))


;; ============================================================
;; RS: the reduction monad

;; This monad acts like an Exception monad whose success type is always
;; specialized to a 4-tuple containing the *local* reduction state: real term,
;; visible term, pattern, and state (cf step.rkt).

;; Contextual state and threaded state are handled by parameters and a
;; parameter-held "xstate" mutable object (see later sections).

(provide
 RS/c
 (contract-out
  [RSunit
   (-> syntaxish? syntaxish? pattern/c state/c RS/c)]
  [RSfail
   (-> exn? RS/c)]
  [RSbind
   (-> RS/c (-> any/c any/c state/c RS/c) RS/c)]
  [RScase
   (-> RS/c
       (-> any/c any/c any/c state/c any)
       (-> exn? any)
       any)]
  [RSreset
   (->* [RS/c] [#:pattern (or/c pattern/c #f)] RS/c)]))

;; RS = (rsok Stx Stx Pattern State)
;;    | (rsfailed Exn)
(struct rsok (f v p s))
(struct rsfailed (exn))

(define RS/c (or/c rsok? rsfailed?))
(define pattern/c any/c)

(define RST/c
  ;; First two args are any/c instead of syntaxish? because of
  ;; #:new-local-context that initially sets syntax to #f.
  (-> (or/c syntaxish? #f) (or/c syntaxish? #f) pattern/c state/c
      RS/c))

(define (RSunit f v p s) (rsok f v p s))
(define (RSfail exn) (rsfailed exn))

(define (RSbind a fun)
  (match a
    [(rsok f v p s) (fun f v p s)]
    [(rsfailed exn) a]))

(define (RScase a ok fail)
  (match a
    [(rsok f v p s) (ok f v p s)]
    [(rsfailed exn) (fail exn)]))

(define (RSreset a #:pattern [reset-p #f])
  (RSbind a (lambda (f v p s) (RSunit f v (or reset-p p) s))))


;; ============================================================
;; Implicit match from #:pattern

;; In a subexpression of an R-clause (see below), the % and %e macros have
;; access to the pattern variables bound by matching the current term (f)
;; against the current pattern (p).

;; Unlike with-syntax and #', pattern-match and % never create syntax
;; objects. Also, patterns here are first-class values, which simplifies
;; continuation management.

(provide % %e)

(define-syntax-parameter the-match-result
  (lambda (stx)
    (raise-syntax-error #f "no match result; used outside of with-pattern-match" stx)))

(define-syntax-rule (% p) (%e (quote-template-pattern p)))
(define-syntax-rule (%e p) (pattern-template p the-match-result))

(define-syntax with-pattern-match
  (syntax-parser
    [(_ [f p] expr:expr)
     #'(let ([mv (pattern-match p f)])
         (syntax-parameterize ((the-match-result (make-rename-transformer #'mv)))
           expr))]))


;; ============================================================
;; The Reduction Language

(provide R !)

(define-syntax ! (syntax-rules ()))

;; (R R-clause ...) : RST
(begin-for-syntax
  (define clause-kw->macro
    (hash '#:set-syntax #'R/set-syntax
          '#:pattern #'R/pattern
          '#:do #'R/do
          '#:let #'R/let
          '#:parameterize #'R/parameterize
          '#:walk #'R/walk
          '#:rename #'R/rename
          '#:rename/mark #'R/rename/mark
          '#:rename/unmark #'R/rename/unmark
          '#:with-marking #'R/with-marking
          '#:new-local-context #'R/new-local-context
          '#:if #'R/if
          '#:when #'R/when
          '#:in-hole #'R/in-hole
          '#:hide-check #'R/hide-check
          '#:seek-check #'R/seek-check
          ))

  (define-syntax-class RClause #:attributes (macro)
    #:literals (!)
    (pattern [! . _]
             #:with macro #'R/!)
    (pattern [e:expr . _]
             #:with macro #'R/run)
    (pattern [kw:keyword . _]
             #:attr macro (hash-ref clause-kw->macro (syntax-e #'kw) #f)
             #:fail-when (and (not (attribute macro)) #'kw) "unknown keyword")))

;; syntax (R RClause ...) : RST
(define-syntax-rule (R . clauses)
  (lambda (f v p s) (R** f v p s . clauses)))

;; syntax (R** f v p s RClause ...) : RS
(define-syntax R**
  (syntax-parser
    #:literals (=>)
    [(R** f v p s)
     #'(RSunit f v p s)]
    [(R** f v p s => k . more)
     #:declare k (expr/c #'RST/c)
     #'(RSbind (RSreset (k.c f v p s) #:pattern p)
               (R . more))]
    [(R** f v p s c:RClause . more)
     #'(begin
         (DEBUG (do-debug-clause (quote c) (quote-syntax c) v))
         (c.macro f v p s c (R . more)))]))

(define (do-debug-clause c cstx v)
  (define where (format "[~s:~s]" (syntax-line cstx) (syntax-column cstx)))
  (eprintf "doing ~a ~.s, honesty = ~s, v = ~.s\n"
           where (trim-quoted-clause c) (honesty) (stx->datum v)))

(define (trim-quoted-clause c)
  (define (abbrev-kw? x) (memq x '(#:parameterize #:when #:if #:with-marking #:do)))
  (match c [(cons (? abbrev-kw? kw) _) `(,kw _)] [_ c]))

;; A R/<Clause> macro has the form
;;   (R/<Clause> f v p s <Clause> kexpr)
;; where f,v,p,w,ws are *variables* and kexpr is *expression*
;; - f is the "real" form -- it should never contain artificial syntax
;; - v is the "virtual/visible" form (used for steps)
;; - p is the current pattern
;; - s is the last marked state, or #f
;; - kexpr is the continuation (RST)

(define-syntax R/!
  (syntax-parser
    #:literals (!)
    ;; Error-point case
    [(_ f v p s [! maybe-exn] ke)
     #:declare maybe-exn (expr/c #'(or/c exn? #f))
     #'(let ([x maybe-exn.c])
         (if x
             (begin (add-step (stumble v x) #t)
                    (RSfail x))
             (ke f v p s)))]))

(define-syntax R/pattern
  (syntax-parser
    ;; Change patterns
    [(_ f v p s [#:pattern p2] ke)
     #'(ke f v (quote-pattern p2) s)]))

(define-syntax R/do
  (syntax-parser
    ;; Execute expressions for effect
    [(_ f v p s [#:do expr ...] ke)
     #'(begin
         (with-pattern-match [f p] (let () expr ... (void)))
         (ke f v p s))]))

(define-syntax R/let
  (syntax-parser
    [(_ f v p s [#:let var:id expr] ke)
     #'(let ([var (with-pattern-match [f p] expr)])
         (ke f v p s))]))

(define-syntax R/parameterize
  (syntax-parser
    [(_ f v p s [#:parameterize ((param expr) ...) . clauses] ke)
     #:declare param (expr/c #'parameter?)
     #'(RSbind (parameterize ((param.c (with-pattern-match [f p] expr)) ...)
                 (R** f v p s . clauses))
               ke)]))

(define-syntax R/set-syntax
  (syntax-parser
    ;; Change syntax
    [(_ f v p s [#:set-syntax form] ke)
     #:declare form (expr/c #'syntaxish?)
     #'(let ([f2 (with-pattern-match [f p] form.c)])
         (ke f2 (change-visible-term f f2 v) p s))]))

(define (change-visible-term f f2 v)
  (cond [(honest?) f2]
        [else (set-honesty 'F f) v]))

(begin-for-syntax
  (define-syntax-class walk-clause
    #:attributes (state1.c form1.c form2.c foci1.c foci2.c type)
    (pattern [#:walk form2 type:expr
              (~alt (~optional (~seq #:foci foci2))
                    (~optional (~seq #:from-state state1))
                    (~optional (~seq #:from form1))
                    (~optional (~seq #:from-foci foci1))) ...]
             #:declare state1 (expr/c #'state/c)
             #:declare form1 (expr/c #'syntaxish?)
             #:declare foci1 (expr/c #'syntaxish?)
             #:declare form2 (expr/c #'syntaxish?)
             #:declare foci2 (expr/c #'syntaxish?))))

(define-syntax R/walk
  (syntax-parser
    [(_ f v p s w:walk-clause ke)
     #'(let ()
         (define-values (state1 f1 f2 type)
           (with-pattern-match [f p]
             (values (~? w.state1.c #f) (~? w.form1.c v) w.form2.c w.type)))
         (define-values (fs1 fs2)
           (with-pattern-match [f p]
             (values (~? w.foci1.c f1) (~? w.foci2.c f2))))
         (do-walk f v p s state1 f1 fs1 f2 fs2 type ke))]))

(define (do-walk f v p s state1 f1 fs1 f2 fs2 type k)
  (define s1 (or state1 (current-state-with f1 fs1)))
  (define s2 (current-state-with f2 fs2))
  (when type (add-step (make step type s1 s2)))
  (k f2 (change-visible-term f f2 v) p s2))

(define-syntax R/rename
  (syntax-parser
    ;; Rename
    [(_ f v p s [#:rename pattern renames] ke)
     #'(RSbind (Rename f v p s pattern renames #f #f) ke)]
    [(_ f v p s [#:rename pattern renames description] ke)
     #'(RSbind (Rename f v p s pattern renames description #f) ke)]))

(define-syntax-rule (Rename f v p s pattern renames description mark-flag)
  (let ()
    (define-values (renames-var description-var)
      (with-pattern-match [f p] (values renames description)))
    (do-rename f v p s (quote-pattern pattern) renames-var description-var mark-flag)))

(define (do-rename f v p s ren-p renames description mode)
  (DEBUG
   (eprintf "do-rename(~s): ~.s at ~s\n" (or mode description) (stx->datum renames) ren-p)
   (eprintf "  v = ~.s\n" (stx->datum v)))
  (define pre-renames (pattern-template ren-p (pattern-match p f)))
  (cond [(equal? pre-renames renames)
         (RSunit f v p s)]
        [else
         (do-rename* f v p s ren-p pre-renames renames description mode)]))

(define (do-rename* f v p s ren-p pre-renames renames description mode)
  (STRICT-CHECKS
   (unless (same-contour? pre-renames renames)
     (error 'rename "different contours!\n  contour-diff: ~s\n  pre:  ~s\n  post: ~s"
            (stx-contour-diff pre-renames renames) pre-renames renames)))
  (define f2 (pattern-replace p f ren-p renames #:resyntax? #f))
  ;; renaming preserves honesty
  (when (the-vt) (the-vt (vt-track pre-renames renames (the-vt) description)))
  ;; ----
  ;; Note: renames might have more structure than pre-renames, especially if arming!
  (define-values (v2 foci1 foci2)
    (cond [(honest?)
           (values (pattern-replace p f ren-p renames #:resyntax? #t)
                   pre-renames renames)]
          [(eq? mode 'mark)
           ;; FIXME: if honesty = (T . F), then what about mark visibility?
           ;; FIXME: if mode is mark or unmark, should honesty be strictly 'T or 'F ??
           (values v null null)]
          [else
           (define-values (v2 foci1 foci2)
             (do-rename-v v (the-vt) (honesty) pre-renames renames))
           (values (honesty-composite (honesty) f2 v2)
                   ;; Must include pre-renames,renames for true part (FIXME: need narrowing?)
                   (cons foci1 pre-renames) (cons foci2 renames))]))
  (DEBUG
   (eprintf "  renamed: diff=~s, v2 = ~.s \n" (stx-eq-diff v2 v) (stx->datum v2)))
  (when (not (memq description '(#f sync)))
    ;; FIXME: better condition/heuristic for when to add rename step?
    (add-step (walk v v2 description #:foci1 foci1 #:foci2 foci2)
              (not-complete-fiction?)))
  (RSunit f2 v2 p s))

(define (honesty-composite hm f v #:resyntax? [resyntax? #t])
  (DEBUG (eprintf "honesty-composite: ~s\n f = ~.s\n v = ~.s\n" hm f v))
  (let loop ([hm hm] [f f] [v v])
    (match hm
      ['T f]
      ['F v]
      [(cons hma hmb)
       (define c (cons (loop hma (stxd-car f) (stxd-car v))
                       (loop hmb (stxd-cdr f) (stxd-cdr v))))
       (if resyntax? (restx c v) c)])))

(define (do-rename-v v vt hm pre post)
  (DEBUG
   (eprintf " do-rename-v\n")
   (eprintf "  vt-stx = ~.s\n" (stx->datum (vt->stx vt))))
  ;; Note: pre,post can have different shape because of rename-transformers
  (STRICT-CHECKS
   (unless (same-contour? pre post)
     (eprintf "RENAME MISMATCH\npre  = ~s\npost = ~s\n" (stx->datum pre) (stx->datum post))))
  ;; Recur through pre,post to find the largest sub-renames that apply to v.
  (define (init-k v accren) (values v (map car accren) (map cdr accren)))
  (let loop ([pre pre] [post post] [v v] [accren null] [k init-k])
    (define (try-rename)
      (match (vt-seek pre vt)
        [(cons path _)
         (DEBUG
          (eprintf "  found at ~s, pre = ~.s\n" path (stx->datum pre))
          (eprintf "    actually = ~.s\n" (stx->datum (path-get v path)))
          (eprintf "  do-rename-v : replace at ~s : ~.s => ~.s\n"
                   path (stx->datum v) (stx->datum (path-replace v path post #:resyntax? #f))))
         (cons (path-replace v path post #:resyntax? #t)
               (cons (cons pre post) accren))]
        [else #f]))
    (cond [(and (syntax? pre) (try-rename))
           => (match-lambda [(cons v accren) (k v accren)])]
          [(stx-pair? pre)
           (loop (stxd-car pre) (stxd-car post) v accren
                 (lambda (v accren)
                   (loop (stxd-cdr pre) (stxd-cdr post) v accren k)))]
          [else (k v accren)])))

(define-syntax R/rename/mark
  (syntax-parser
    [(_ f v p s [#:rename/mark pvar to] ke)
     #:declare to (expr/c #'syntaxish?)
     #'(RSbind (Rename f v p s pvar to.c #f 'mark) ke)]))

(define-syntax R/rename/unmark
  (syntax-parser
    [(_ f v p s [#:rename/unmark pvar to] ke)
     #:declare to (expr/c #'syntaxish?)
     #'(RSbind (Rename f v p s pvar to.c #f 'unmark) ke)]))

;; - corresponds to the dynamic extent of a syntax-local-introduce bindings
(define-syntax-rule (R/with-marking f v p s [#:with-marking c ...] ke)
  (RSbind ((R c ...) f v p s) ke))

(define-syntax R/if
  (syntax-parser
    ;; Conditional (pattern changes lost afterwards ...)
    [(_ f v p s [#:if test [consequent ...] [alternate ...]] ke)
     #'(RSbind (RSreset (if (with-pattern-match [f p] test)
                            (R** f v p s consequent ...)
                            (R** f v p s alternate ...))
                        #:pattern p)
               ke)]))

(define-syntax R/when
  (syntax-parser
    ;; Conditional (pattern changes lost afterwards ...)
    [(_ f v p s [#:when test consequent ...] ke)
     #'(R/if f v p s [#:if test [consequent ...] []] ke)]))

(define-syntax R/new-local-context
  (syntax-parser
    [(_ f v p s [#:new-local-context clause ...] ke)
     #'(do-local-context f v p s (R clause ...) ke)]))

(define (do-local-context f v p s rst k)
  (cond [(honest?)
         (RSbind (call/local-context v (lambda () (rst #f #f (quote-pattern _) #f)))
                 (lambda (_f2 _v2 _p2 _s2)
                   (k f v p s)))]
        [else
         (RSbind (rst #f v (quote-pattern _) #f)
                 (lambda (_f2 v2 _p2 _s2)
                   (k f v2 p s)))]))

(define (call/local-context v proc)
  (define bf (bigframe (the-context) (list v) v))
  (parameterize ((the-big-context (cons bf (the-big-context)))
                 (the-context null)
                 (honesty 'T) ;; FIXME?
                 (the-vt #f))
    (proc)))

(define-syntax R/run
  (syntax-parser
    ;; Subterm handling
    [(R** f v p s [reducer hole fill] ke)
     #:declare reducer (expr/c #'(-> any/c RST/c))
     #'(RSbind (run reducer.c f v p s (quote hole) fill)
               ke)]))

(define-syntax R/in-hole
  (syntax-parser
    [(_ f v p s [#:in-hole hole . clauses] ke)
     #'(RSbind (let ([reducer (lambda (_) (R . clauses))])
                 (run reducer f v p s (quote hole) #f))
               ke)]))

;; ============================================================

(define-syntax R/hide-check
  (syntax-parser
   [(_ f v p s [#:hide-check rs] ke)
    #:declare rs (expr/c #'(listof identifier?))
    #'(do-hide-check f v p s (with-pattern-match [f p] rs.c) ke)]))

(define (do-hide-check f v p s ids k)
  (unless (or (eq? (honesty) 'F) (andmap (macro-policy) ids))
    (DEBUG
     (eprintf "hide-check: hiding with f=~.s, v=~.s\n" (stx->datum f) (stx->datum v)))
    (set-honesty 'F f))
  (k f v p s))

(define-syntax-rule (R/seek-check f v p s [#:seek-check] ke)
  (do-seek-check f v p s ke))

(define (do-seek-check f v p s k)
  (cond [(honest?) (k f v p s)]
        [else
         (match (vt-seek f (the-vt))
           ['()
            (DEBUG (eprintf "seek-check: no paths found for ~.s\n" (stx->datum f))
                   #;(begin (eprintf "  the-vt =\n") (pretty-print (the-vt)) (eprintf "\n")))
            (k f v p s)]
           [(cons path more-paths)
            (DEBUG (eprintf "seek-check: found path ~s for ~.s within ~.s\n"
                            path (stx->datum f) (stx->datum v)))
            (define vctx (path-replacer v path #:resyntax? #t))
            ((parameterize ((the-context (cons vctx (the-context)))
                            (honesty 'T)
                            (the-vt #f))
               (RScase (k f f p s)
                       (lambda (f2 v2 p2 s2)
                         ;; inside parameterize
                         (define end-vt (the-vt))
                         (lambda ()
                           ;; outside parameterize
                           (the-vt (vt-merge-at-path (the-vt) path (or end-vt f2)))
                           ;; note: returning a true term into a fictional
                           ;; context does not increase honesty
                           (RSunit f2 (vctx v2) p s)))
                       (lambda (exn)
                         (lambda ()
                           (RSfail exn))))))])]))

;; ============================================================
;; Running reducers in a sub-context

;; Within a context created by run/path:

;; - Honesty (with respect to the context) never increases; the local honesty on
;;   exit from the context is <= the local honesty when the context is entered.

;; - Global honesty does not change on entry to a context. It only changes on
;;   hide decisions, steps (if local honesty is < T, then a step immediately
;;   sets it to F), and exit from a context.

;; - A successful seek creates a locally honest context, but the honesty is lost
;;   on exit from the context. Honesty is measured from the root of the current
;;   term, so an honest term gets "lost" when returned into dishonest context.

;; run : (X -> RST) Stx Stx Pattern State Hole (U X (Listof X)) -> RS
;; where Hole = Symbol | (list Symbol '...) -- NOT a pattern value
;; Note: run restores pattern after running reducer
(define (run reducer f v p s hole fill)
  (match hole
    [(? symbol? hole)
     (define path (subpattern-path p hole))
     (run/path reducer f v p s path fill)]
    [(list (? symbol? hole) '...)
     (match-define (vector pre-path sub-path) (subpattern-path p hole #t))
     (let loop ([fill fill] [k 0] [f f] [v v] [s s])
       (match fill
         [(cons fill0 fill*)
          (define path (append pre-path (path-add-ref k sub-path)))
          (RSbind (run/path reducer f v p s path fill0)
                  (lambda (f v _p s) (loop fill* (add1 k) f v s)))]
         ['() (RSunit f v p s)]))]))

(define (run/path reducer f v p s path fill)
  (define fctx (path-replacer f path #:resyntax? #f))
  (define sub-f (path-get f path))
  (define sub-hm (honesty-at-path (honesty) path))
  (DEBUG (eprintf "run/path: honesty ~s at path ~s => ~s\n" (honesty) path sub-hm))
  (define-values (vctx sub-v sub-vt)
    (cond [(eq? sub-hm 'F)
           ;; path might be out of bounds for v => can't take vctx => sub-v is meaningless
           ;; probably not much point in narrowing VT (and nontrivial to do right)
           ;; FIXME: it would be slightly better to know whether we were *inside* an F,
           ;;   because we care about whether the context is honest, not the term
           (define sub-v v)
           (define sub-vt (the-vt))
           (values #f sub-v sub-vt)]
          [else
           ;; can take vctx, but must also take narrowed VT (when sub-hm != 'T)
           (define vctx (path-replacer v path #:resyntax? #t))
           (define sub-v (path-get v path))
           (define sub-vt (if (eq? sub-hm 'T) #f (vt-zoom (the-vt) path)))
           (values vctx sub-v sub-vt)]))
  (DEBUG (eprintf "run/path: run ~s on f=~.s; v=~.s\n"
                  reducer (stx->datum sub-f) (stx->datum sub-v)))
  ((parameterize ((the-context (if vctx (cons vctx (the-context)) (the-context)))
                  (honesty sub-hm)
                  (the-vt sub-vt))
     (RScase ((reducer fill) sub-f sub-v (quote-pattern _) s)
             (lambda (f2 v2 _p2 _s2)
               ;; inside parameterize
               (define end-hm (honesty))
               (define end-vt (the-vt))
               (lambda ()
                 ;; outside of parameterize
                 (define merged-hm (honesty-merge-at-path (honesty) path end-hm))
                 (DEBUG
                  (eprintf "\n<< run/path merge old ~s and sub ~s => ~s\n"
                           (honesty) end-hm merged-hm)
                  (eprintf "  v => ~.s\n" (stx->datum (if vctx (vctx v2) v2))))
                 (honesty merged-hm)
                 (the-vt (cond
                           ;; Case: sub-hm = F
                           ;; - then sub-vt was not zoomed, end-vt extends sub-vt, return as is
                           [(eq? sub-hm 'F) end-vt]
                           ;; Case: sub-hm > F and sub-vt != #f
                           ;; - then sub-vt was zoomed, end-vt extends it, so unzoom end-vt
                           [sub-vt (vt-unzoom end-vt path)]
                           ;; Case: sub-hm = T and end-vt != #f -- honesty decreased during reducer
                           [end-vt
                            (if (the-vt)
                                (vt-merge-at-path (the-vt) path end-vt)
                                (vt-merge-at-path f path end-vt))]
                           ;; Case: sub-hm = end-hm = T
                           [else (the-vt)]))
                 (DEBUG
                  (eprintf "  vt => ~e\n" (the-vt))
                  (when (the-vt)
                    (eprintf "  vt-stx => ~.s\n" (stx->datum (vt->stx (the-vt))))))
                 (RSunit (fctx f2) (if vctx (vctx v2) v2) p s)))
             (lambda (exn)
               (lambda () (RSfail exn)))))))

;; ------------------------------------

(define (revappend a b)
  (cond [(pair? a) (revappend (cdr a) (cons (car a) b))]
        [(null? a) b]))

(define (same-contour? x y)
  (let loop ([x (stx->datum x)] [y (stx->datum y)])
    (cond [(and (pair? x) (pair? y))
           (and (loop (car x) (car y)) (loop (cdr x) (cdr y)))]
          [else (not (or (pair? x) (pair? y)))])))

(define (stx-contour-diff x y)
  (let loop ([x (stx->datum x)] [y (stx->datum y)])
    (cond [(and (pair? x) (pair? y))
           (let ([d1 (loop (car x) (car y))]
                 [d2 (loop (cdr x) (cdr y))])
             (cond [(and (eq? d1 '_) (eq? d2 '_)) '_]
                   [else (cons d1 d2)]))]
          [(and (null? x) (null? y)) '()]
          [(equal? x y) '_]
          [else `#s(DIFF ,x ,y)])))

(define (stx-eq-diff a b)
  (let loop ([a a] [b b])
    (cond [(and (stx-null? a) (stx-null? b)) '()]
          [(equal? a b) '_]
          [(stx-pair? a)
           (cons (loop (stx-car a) (stx-car b))
                 (loop (stx-cdr a) (stx-cdr b)))]
          [else
           (unless (equal? (stx->datum a) (stx->datum b))
             (error 'stx-eq-diff "different shapes: ~.s, ~.s" a b))
           (stx->datum a)])))

;; flatten-identifiers : syntaxlike -> (list-of identifier)
(define (flatten-identifiers stx)
  (syntax-case stx ()
    [id (identifier? #'id) (list #'id)]
    [() null]
    [(x . y) (append (flatten-identifiers #'x) (flatten-identifiers #'y))]
    [else (error 'flatten-identifiers "neither syntax list nor identifier: ~s"
                 (if (syntax? stx)
                     (syntax->datum stx)
                     stx))]))

;; ============================================================
;; Macro hiding

;; The behavior of the reductions generator is determined by the current
;; *honesty* level.
;;
;; - honesty: Does the current *visible* local term correspond to the current
;;   *actual* local term? This depends on the history of macro hiding within the
;;   current context and (partly) on the honesty of the parent context.

;; Honesty forms a lattice with top 'T and bottom 'F:
;;
;; - 'T = honest: The current local *actual* and *visible* terms correspond
;;   exactly, although the actual term (f) may have plain pairs in some places
;;   where the visible term has artificial syntax pairs.
;;
;; - 'F = fictional: The current visible term is (potentially) completely
;;   fictional. It may originate from an outer context. For example:
;;
;;     (define x (let () 1)) -> (define-values (x) (let () 1))
;;
;;     Suppose that define is hidden. Then when we recur into its right-hand
;;     side, we will have f = (let () 1) and v = (define x (let () 1)). Then we
;;     seek for (let () 1) and find it at vctx = (define x [ ]), and we create a
;;     context for the (initially honest) reduction of (let () 1), but we put
;;     that reduction sequence in the synthetic context vctx. (Note: see also
;;     Visible Term Tracking.)
;;
;; - (cons hm1 hm2) = an honest pair with contents whose honesty is described by
;;   hm1 and hm2, respectively. We consider (cons 'T 'T) = 'T.

;; The honesty level has the following consequences:
;;
;; On *steps*:
;; - 'T: the step can be shown, and it updates the visible term
;; - 'F: the step is not shown, and the visible term is not updated
;; - cons: the step cannot be shown, or it must be simulated
;;   Consider the actual expansion (#%expression A) -> (#%expression A*) -> A*.
;;   If hiding produces (#%expression A) -> (#%expression A**), then we cannot
;;   apply the step (#%expression A*) -> A*. There are two options:
;;   - drop the step (this is the current behavior)
;;   - simulation: rewrite the step to (#%expression A**) -> A**; this
;;     requires custom code for each step (?)
;;
;; On entering a new context for reduction:
;; - 'T: the mode of the new context is still 'T
;; - 'F: the mode of the new context is still 'F
;;
;; - cons: the honesty level of the new context is the subtree of the old level
;;   at the path corresponding to the context. For example, if the old honesty
;;   level is (cons 'F 'T), then if the new context is ([ ] e), then the new
;;   local honesty level is 'F, but if the context is (e [ ]), then the new
;;   local level is 'T. (See Honesty Masks.)
;;
;; On returning from a context:
;; - 'T: the parent context's mode is unchanged
;; - otherwise, we merge the final local honesty level into the parent's level
;;   at the context's path

;; Why not simplify the lattice to just 'T and 'F?
;;
;; - It means there is no need to explicitly specify dependent vs independent
;;   contexts. For example, the if form has three independent subexpressions,
;;   and macro hiding in the first subexpression should not affect the second
;;   subexpression. But in a block, hiding that occurs in pass1 should inhibit
;;   the letrec transformation, and pass2 should start with the visible results
;;   of pass1. That is, pass2's context depends on pass1. The old macro stepper
;;   implementation addressed this with #:pass1/#:pass2 annotations that had to
;;   be sprinkled everywhere, and there always seemed to be one missing
;;   somewhere. Honesty masks subsume those annotations.
;;
;; - The current lattice also allow greater precision. For example, in the
;;   expansion of (λ () (define x 1) (begin 2 3)), the honesty lattice allows us
;;   to hide the define form but show the begin splicing.

;; ----------------------------------------
;; Visible Term Tracking

;; The difficult half of macro hiding is *seeking* visible subterms so their
;; expansions can be shown in synthetic contexts. The "visible term" (v) is not
;; sufficient for determining whether a term should be shown or for determining
;; in what context to show it, due to scopes-renames, syntax-arm/disarm, etc.

;; Instead, when the honesty level drops below 'T, a VT (visible terms tracker)
;; is created from the last visible honest term. The VT records scope-renames
;; and other syntax adjustements. When a term is searched in the VT, it applies
;; the history in reverse to see if the adjusted term existed in the last
;; visible honest term, and if so at what path. The synthetic context (vctx) is
;; created from the path and the current visible term.

;; Invariants:
;; - if (honesty) = 'T, then (the-vt) = #f
;; - if (honesty) < 'T, then
;;   - (the-vt) is a VT, and
;;   - (stx->datum (vt->stx (the-vt))) = (stx->datum v)

;; ----------------------------------------
;; Honesty Masks

;; An *honesty mask* indicates what parts of the current term may be fictional.

;; An HonestyMask is one of
;; - 'F -- (partly, potentially) fictional term
;; - 'T -- true term
;; - (cons HonestyMask HonestyMask) -- a true pair

;; Note: Since HonestyMask < Stxish, can use path functions on HonestyMasks.

;; hmcons : HonestyMask HonestyMask -> HonestyMask
;; Note: (cons T T) = T --- pair might be artificial, but can sync? (FIXME)
(define (hmcons hm1 hm2) (if (and (eq? hm1 'T) (eq? hm2 'T)) 'T (cons hm1 hm2)))

;; honesty-at-path : HonestyMask Path -> HonestyMask
(define (honesty-at-path hm path)
  (define-values (hm* path*) (path-get-until hm path symbol?))
  ;; Either we used whole path, or we stopped short at 'T or 'F, and
  ;; the subterms of a 'T or 'F term is 'T or 'F, respectively.
  hm*)

;; honesty-merge : HonestyMask HonestyMask -> HonestyMask
(define (honesty-merge hm1 hm2)
  (let loop ([hm1 hm1] [hm2 hm2])
    (match* [hm1 hm2]
      [['T hm] hm]
      [[hm 'T] hm]
      [[(cons hm1a hm1b) (cons hm2a hm2b)]
       (hmcons (loop hm1a hm2a) (loop hm1b hm2b))]
      [[_ _] 'F])))

;; honesty-merge-at-path: HonestyMask Path HonestyMask -> HonestyMask
;; Merges the first hm's subtree at path with second subtree.
(define (honesty-merge-at-path hm1 path hm2)
  (define (loop hm1 path)
    (match path
      ['() (honesty-merge hm1 hm2)]
      [(cons 'car path)
       (match hm1
         [(cons hm1a hm1b) (cons (loop hm1a path) hm1b)]
         ['T (hmcons (loop 'T path) 'T)]
         ['F 'F])]
      [(cons (? exact-positive-integer? n) path)
       (let tailloop ([hm1 hm1] [n n])
         (cond [(zero? n) (loop hm1 path)]
               [else
                (match hm1
                  [(cons hm1a hm1b) (hmcons hm1a (tailloop hm1b (sub1 n)))]
                  ['T (hmcons 'T (tailloop 'T (sub1 n)))]
                  ['F 'F])]))]))
  (loop hm1 path))

;; An HonestyMaskSpec extends HonestyMask with
;; - #(hmrep HonestyMask) -- a true list whose elements have the given honesty
(struct hmrep (hm) #:prefab)

;; honesty>=? : HonestyMask HonestyMaskSpec -> Boolean
;; Retuns #t if hm1 is at least as honest as hm2.
(define (honesty>=? hm1 hm2)
  (let loop ([hm1 hm1] [hm2 hm2])
    (match* [hm1 hm2]
      [['T _] #t]
      [[_ 'F] #t]
      [[(cons hm1a hm1b) (cons hm2a hm2b)]
       (and (loop hm1a hm2a) (loop hm1b hm2b))]
      [[(cons hm1a hm1b) (hmrep hm2e)]
       (and (loop hm1a hm2e) (loop hm1b hm2))]
      [[_ _] #f])))
