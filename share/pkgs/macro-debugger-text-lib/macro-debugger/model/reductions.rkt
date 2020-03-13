#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/list
         racket/format
         syntax/stx
         "deriv-util.rkt"
         "deriv.rkt"
         "pattern.rkt"
         "stx-util.rkt"
         "reductions-util.rkt")

(provide reductions
         reductions+)

;; ============================================================
;; Reductions

;; reductions : WDeriv -> ReductionSequence
(define (reductions d)
  (let-values ([(steps binders definites estx exn) (reductions+ d)])
    steps))

;; Binders = hasheq[identifier => phase-level]
;; Definites = hasheq[identifier => phase-level]

;; reductions+ : WDeriv -> (list-of step) Binders Definites ?stx ?exn
(define (reductions+ d)
  (define xst (new-xstate))
  (call-with-initial-context
   #:xstate xst
   (lambda ()
     (RScase ((Expr d) (wderiv-e1 d) (wderiv-e1 d) (quote-pattern _) #f)
             (lambda (f v p s)
               (values (reverse (xstate-steps xst))
                       (xstate-binders xst) (xstate-definites xst)
                       (datum->artificial-syntax v) #f))
             (lambda (exn)
               (values (reverse (xstate-steps xst))
                       (xstate-binders xst) (xstate-definites xst)
                       #f exn))))))

;; Syntax

(define-syntax-rule (match/count x clause ...)
  (begin (next-seqno)
         (let ([v x])
           (match v
             clause ...
             [_ (error 'match "failed to match ~e at line ~s" v (line-of x))]))))

(define-syntax (line-of stx)
  (syntax-case stx ()
    [(line-of x) #`(quote #,(syntax-line #'x))]))

;; Derivations => Steps

;; Expr : Deriv -> RST
(define (Expr d)
  (match/count d
    [(deriv e1 e2)
     (R [#:pattern ?form]
        [#:do (DEBUG (eprintf "\n>> ~.s\n" (stx->datum e1)))]
        [#:do (STRICT-CHECKS
               (when (and e1 (not (eq? (% ?form) e1)))
                 (eprintf "MISMATCH: not eq\n  actual = ~.s\n  deriv  = ~.s\n"
                          (values #;stx->datum (% ?form)) (values #;stx->datum e1))
                 (eprintf "  deriv = ~e\n" d)))]
        [#:when (and e1 (not (eq? (% ?form) e1)))
         [#:rename ?form e1 'sync]] ;; FIXME, neither sync nor #:set-syntax
        [#:parameterize ((the-context (add-rearm-frame e1 (the-context))))
         [#:when (base? d)
          [#:do (learn-definites (or (base-resolves d) null))]
          [#:when (base-de1 d)
           [#:rename ?form (base-de1 d) #;'disarm]]]
         [#:seek-check]
         => (Expr* d)]
        [#:when (and e2 (not (eq? (% ?form) e2)))
         [#:rename ?form e2 'sync]])]
    [#f
     (R [#:seek-check]
        => (Expr* d))]))

;; add-rearm-frame : Stx Context -> Context
(define (add-rearm-frame e1 ctx)
  ;; This arms the artificial intermediate terms, since
  ;; the expander generally (always?) re-arms the result
  ;; of expanding an armed term.
  (cond [(and (syntax? e1) (syntax-armed? e1) (not-complete-fiction?))
         (define (rearm-frame x)
           (let ([x (datum->artificial-syntax x)])
             (cond [(syntax-armed/tainted? x) x]
                   [else (syntax-property (syntax-rearm (datum->artificial-syntax x) e1)
                                          property:unlocked-by-expander #t)])))
         (cons (immediate-frame rearm-frame) (the-context))]
        [else (the-context)]))

(define (Expr* d)
  (match d
    ;; Primitives
    [(p:variable e1 e2 rs de1 ?1)
     (R [#:do (learn-definites (list e2))]
        [#:when (or (not (identifier? e1))
                    (not (bound-identifier=? e1 e2)))
                [#:walk e2 'resolve-variable]])]
    [(p:module e1 e2 rs de1 ?1 prep rename ensure-mb body shift)
     (R [#:hide-check rs]
        [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?module ?name ?lang . ?bodys)]
        [#:rename ?bodys rename 'rename-module]
        => (ModEnsureMB ensure-mb)
        [#:pattern (?module ?name ?lang ?body)]
        [Expr ?body body]
        [#:pattern ?form]
        [#:rename ?form shift 'rename-mod-shift])]
    [(p:#%module-begin e1 e2 rs de1 ?1 me pass12 ?2 pass3 ?3 pass4)
     (R [! ?1]
        [#:pattern ?form]
        [#:rename ?form me 'rename-modbeg]
        [#:pattern (?module-begin . ?forms)]
        [ModPass1And2 ?forms pass12]
        [! ?2]
        [ModulePass3 ?forms pass3]
        [! ?3]
        [ModulePass4 ?forms pass4])]
    [(p:define-syntaxes e1 e2 rs de1 ?1 prep rhs locals)
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?define-syntaxes ?vars ?rhs)]
        [#:do (learn-binders (% ?vars))]
        [Expr/PhaseUp ?rhs rhs]
        [LocalActions ?rhs locals])]
    [(p:define-values e1 e2 rs de1 ?1 rhs)
     (R [! ?1]
        [#:pattern (?define-values ?vars ?rhs)]
        [#:do (learn-binders (% ?vars))]
        [#:when rhs
                [Expr ?rhs rhs]])]
    [(p:#%expression e1 e2 rs de1 ?1 inner #f)
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [Expr ?inner inner])]
    [(p:#%expression e1 e2 rs de1 ?1 inner untag)
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [Expr ?inner inner]
        [#:when untag
         [#:rename ?inner untag #;'track]
         [#:pattern ?form]
         [#:walk untag 'finish-expr]])]
    [(p:if e1 e2 rs de1 ?1 test then else)
     (R [! ?1]
        [#:pattern (?if TEST THEN ELSE)]
        [Expr TEST test]
        [Expr THEN then]
        [Expr ELSE else])]
    [(p:wcm e1 e2 rs de1 ?1 key mark body)
     (R [! ?1]
        [#:pattern (?wcm KEY MARK BODY)]
        [Expr KEY key]
        [Expr MARK mark]
        [Expr BODY body])]
    [(p:begin e1 e2 rs de1 ?1 derivs)
     (R [! ?1]
        [#:pattern (?begin ?form ...)]
        [Expr (?form ...) derivs])]
    [(p:begin0 e1 e2 rs de1 ?1 derivs)
     (R [! ?1]
        [#:pattern (?begin0 ?form ...)]
        [Expr (?form ...) derivs])]
    [(p:#%app e1 e2 rs de1 ?1 derivs)
     (R [! ?1]
        [#:pattern (?app ?e ...)]
        [#:if (pair? derivs)
              ([Expr (?e ...) derivs])
              ([#:walk e2 'macro])])]
    [(p:lambda e1 e2 rs de1 ?1 renames body)
     (R [! ?1]
        [#:pattern (?lambda ?formals . ?body)]
        [#:rename (?formals . ?body) renames 'rename-lambda]
        [#:do (learn-binders (% ?formals))]
        [Block ?body body])]
    [(p:case-lambda e1 e2 rs de1 ?1 clauses)
     (R [! ?1]
        [#:pattern (?case-lambda . ?clauses)]
        [CaseLambdaClauses ?clauses clauses])]
    [(p:let-values e1 e2 rs de1 ?1 renames rhss body)
     (R [! ?1]
        [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-letX]
        [#:do (learn-binders (% (?vars ...)))]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(p:letrec-values e1 e2 rs de1 ?1 renames rhss body)
     (R [! ?1]
        [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-letX]
        [#:do (learn-binders (% (?vars ...)))]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(p:letrec-syntaxes+values e1 e2 rs de1 ?1 srenames prep srhss vrhss body)
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
        [#:rename (((?svars ?srhs) ...) ((?vvars ?vrhs) ...) . ?body)
                   srenames
                   'rename-letX]
        [#:do (learn-binders (% (?svars ... ?vvars ...)))]
        [BindSyntaxes (?srhs ...) srhss]
        [Expr (?vrhs ...) vrhss]
        [Block ?body body]
        [#:pattern ?form]
        [#:walk e2 'finish-lsv])]
    [(p:#%datum e1 e2 rs de1 ?1)
     (R [! ?1]
        [#:hide-check rs]
        [#:walk e2 'macro])]
    [(p:#%top e1 e2 rs de1 ?1)
     (R [! ?1]
        [#:pattern ?form]
        [#:do (learn-definites
               (syntax-case (% ?form) ()
                 [(?top . ?var) (identifier? #'?var) (list #'?var)]
                 [?var (identifier? #'?var) (list #'?var)]
                 [?form (error 'macro-debugger "#%top has wrong form: ~s\n" #'?form)]))]
        [#:when (not (eq? de1 e2))
         [#:walk e2 'tag-top]])]

    [(p:provide e1 e2 rs de1 ?1 inners ?2)
     (let ([wrapped-inners (map expr->local-action inners)])
       (R [! ?1]
          [#:pattern ?form]
          [#:let old-form (% ?form)]
          [LocalActions ?form wrapped-inners]
          [! ?2]
          [#:walk e2 'provide #:from old-form]
          ))]

    [(p:require e1 e2 rs de1 ?1 locals)
     (R [! ?1]
        [#:pattern ?form]
        [LocalActions ?form locals])]

    [(p:#%stratified-body e1 e2 rs de1 ?1 bderiv)
     (R [! ?1]
        [#:pattern (?sb . ?body)]
        [Block ?body bderiv]
        [#:hide-check rs]
        [#:pattern ?form]
        [#:walk e2 'macro])]

    [(p:submodule* e1 e2 rs de1 ?1 exp locals)
     (R [! ?1]
        [#:pattern ?form]
        [Expr ?form exp]
        [LocalActions ?form locals])]
    [(p:submodule e1 e2 rs de1 ?1 exp locals)
     (R [! ?1]
        [#:pattern ?form]
        [Expr ?form exp]
        [LocalActions ?form locals])]
    [(p:declare e1 e2 rs de1 ?1)
     (R [! ?1])]

    [(p:stop e1 e2 rs de1 ?1)
     (R [! ?1])]
    [(p:opaque e1 e2 rs #f #f)
     (define transparent-stx (hash-ref opaque-table (syntax-e e1) #f))
     (R [#:pattern ?form]
        [#:when transparent-stx
         [#:set-syntax transparent-stx]])] ;; FIXME: walk?

    ;; The rest of the automatic primitives
    [(p::STOP e1 e2 rs de1 ?1)
     (R [! ?1])]

    [(p:set!-macro e1 e2 rs de1 ?1 deriv)
     (R [! ?1]
        [#:pattern ?form]
        [#:rename ?form e1] ;; macro starts from e1, not de1
        [Expr ?form deriv])]
    [(p:set! e1 e2 rs de1 ?1 id-rs ?2 rhs)
     (R [! ?1]
        [#:pattern (?set! ?var ?rhs)]
        [#:do (learn-definites id-rs)]
        [! ?2]
        [Expr ?rhs rhs])]

    [(p:begin-for-syntax e1 e2 rs de1 ?1 prep body locals)
     (R [! ?1]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern (?bfs . ?forms)]
        [#:parameterize ((the-phase (add1 (the-phase))))
          [#:if (mod:pass-1-and-2? body)
                [[ModPass1And2 ?forms body]]
                [[BeginForSyntax ?forms body]]]]
        [LocalActions ?forms locals])]

    ;; Macros
    [(mrule e1 e2 rs de1 ?1 me1 locals me2 ?2 etx retx next)
     (R [! ?1]
        [#:pattern ?form]
        [#:hide-check rs]
        [#:do (learn-definites rs)]
        [#:let old-state (current-state-with e1 (list e1))] ;; use (non-disarmed) e1
        [#:with-marking
         [#:rename/mark ?form me1]
         [#:when (pair? locals)
          [LocalActions ?form locals]]
         [! ?2]
         [#:set-syntax me2]
         [#:rename/unmark ?form etx]
         [#:rename ?form retx]]
        [#:walk retx 'macro #:from-state old-state]
        [Expr ?form next])]

    [(tagrule e1 e2 disarmed-untagged-stx tagged-stx next)
     (R [#:pattern ?form]
        [#:hide-check (list (stx-car tagged-stx))]
        [#:let old-state (current-state-with e1 (list e1))]
        [#:let disarmed-tagged-stx (cons (stxd-car tagged-stx) disarmed-untagged-stx)]
        [#:rename ?form disarmed-untagged-stx] ;; disarm
        [#:set-syntax disarmed-tagged-stx]
        [#:rename ?form tagged-stx] ;; rearm
        [#:walk tagged-stx (case (syntax-e (stx-car disarmed-tagged-stx))
                             [(#%app) 'tag-app]
                             [(#%datum) 'tag-datum]
                             [(#%top) 'tag-top]
                             [else (error 'reductions "unknown tagged syntax: ~s" tagged-stx)])
         #:from-state old-state]
        [Expr ?form next])]

    ;; expand/compile-time-evals

    [(ecte e1 e2 locals first second locals2)
     (R [#:pattern ?form]
        [LocalActions ?form locals]
        [Expr ?form first]
        [Expr ?form second]
        [LocalActions ?form locals2])]

    ;; Lifts

    [(lift-deriv e1 e2 first lifted-stx second)
     (R [#:pattern ?form]
        [Expr ?form first]
        [#:walk lifted-stx 'capture-lifts]
        [Expr ?form second])]

    [(lift/let-deriv e1 e2 first lifted-stx second)
     (R [#:pattern ?form]
        [Expr ?form first]
        [#:walk lifted-stx 'capture-lifts]
        [Expr ?form second])]

    ;; Skipped
    [#f
     (R)]))

;; Expr/PhaseUp : Deriv -> RST
(define (Expr/PhaseUp d)
  (R [#:parameterize ((the-phase (add1 (the-phase))))
     => (Expr* d)]))

;; CaseLambdaClauses : (Listof CaseLambdaClause) -> RST
(define (CaseLambdaClauses clauses)
  (match/count clauses
    ['()
     (R)]
    [(cons (clc ?1 rename body) rest)
     (R [! ?1]
        [#:pattern ((?formals . ?body) . ?rest)]
        [#:rename (?formals . ?body) rename 'rename-lambda]
        [#:do (learn-binders (% ?formals))]
        [Block ?body body]
        [CaseLambdaClauses ?rest rest])]))

(define (PrepareEnv prep)
  (LocalActions prep))

;; local-actions-reductions
(define (LocalActions locals)
  (match locals
    ['()
     (R)]
    [(cons local rest)
     (R [#:pattern ?form]
        [#:parameterize ((macro-policy
                          ;; If macro with local-expand is transparent,
                          ;; then all local-expansions must be transparent.
                          (if (honest?) (lambda (x) #t) (macro-policy))))
         [#:new-local-context
          [#:pattern ?form]
          [LocalAction ?form local]]]
        [LocalActions ?form rest])]))

(define (LocalAction local)
  (match/count local
    [(local-exn exn)
     (R [! exn])]

    [(local-expansion e1 e2 for-stx? me1 inner #f me2 opaque)
     (R [#:parameterize ((the-phase (if for-stx? (add1 (the-phase)) (the-phase))))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/unmark ?form me1]
         [#:with-marking
          [Expr ?form inner]]
         [#:rename/mark ?form e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(local-expansion e1 e2 for-stx? me1 inner lifted me2 opaque)
     (R [#:parameterize ((the-phase (if for-stx? (add1 (the-phase)) (the-phase))))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/unmark ?form me1]
         [#:with-marking
          [Expr ?form inner]]
         ;; FIXME: catch lifts
         [#:set-syntax lifted]
         [#:rename/mark ?form e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(local-lift-expr orig renamed ids)
     (R [#:do (learn-binders ids)]
        [#:do (add-lift local)]
        [#:do (add-step
               (walk/talk 'local-lift
                          (list "The macro lifted an expression."
                                ""
                                "Expression:"
                                renamed
                                "Identifiers:"
                                (datum->artificial-syntax ids))))])]
    [(local-lift-end orig renamed wrapped)
     (R [#:pattern ?form]
        [#:set-syntax orig]
        [#:rename ?form renamed]
        [#:do (add-lift local)] ;; captured vt includes [orig->renamed]
        [#:do (add-step
               (walk/talk 'local-lift
                          (list "The macro lifted a declaration to the end of the module."
                                ""
                                "Declaration:"
                                wrapped)))])]
    [(local-lift-module orig renamed)
     (R [#:pattern ?form]
        [#:set-syntax orig]
        [#:rename ?form renamed]
        [#:do (add-lift local)] ;; captured vt includes [orig->renamed]
        [#:do (add-step
               (walk/talk 'local-lift
                          (list "The macro lifted a submodule."
                                ""
                                "Declaration:"
                                renamed)))])]
    [(local-lift-require req expr mexpr)
     ;; lift require
     (R [#:do (add-lift local)]
        [#:do (add-step
               (walk/talk 'local-lift
                          (list "The macro lifted a require."
                                ""
                                "Require:"
                                req)))]
        [#:set-syntax expr]
        [#:pattern ?form]
        [#:rename/mark ?form mexpr])]
    [(local-lift-provide prov)
     ;; lift provide
     (R [#:do (add-lift local)]
        [#:do (add-step
               (walk/talk 'local-lift
                          (list "The macro lifted a provide."
                                ""
                                "Provide:"
                                prov)))])]

    [(local-bind names ?1 renames bindrhs)
     [R [! ?1]
        ;; FIXME: use renames
        [#:do (learn-binders names)]
        [#:when bindrhs
         [#:set-syntax (node-z1 (bind-syntaxes-rhs bindrhs))] ;; FIXME: use renames?
         [#:pattern ?form]
         ;; FIXME: use #:with-marking?
         [BindSyntaxes ?form bindrhs]]]]
    [(track-syntax operation new-stx old-stx)
     (R [#:set-syntax old-stx]
        [#:pattern ?form]
        [#:rename ?form new-stx #;operation])]
    [(local-value name ?1 resolves bound? binding)
     [R [! ?1]
        ;; FIXME: notify if binding != current (identifier-binding name)???
        ;; [#:do (learn-definites (list name))]
        ;; Add remark step?
        ]]
    [(local-remark contents)
     (R [#:do (add-step (walk/talk 'remark contents))])]
    [(local-mess events)
     ;; FIXME: While it is not generally possible to parse tokens as one or more
     ;; interrupted derivations (possibly interleaved with successful derivs),
     ;; it should be possible to recover *some* information and display it.
     (R [#:do (add-step
               (let ([texts
                      (list (~a "Some expansion history has been lost due to a jump "
                                "within expansion.")
                            (~a "For example, a macro may have caught an "
                                "exception coming from within a call to `local-expand'."))])
                 (list (walk/talk 'remark texts))))])]
    [#f
     (R)]))

;; List : ListDerivation -> RST
(define (List ld)
  (match ld
    [(lderiv es1 es2 ?1 derivs)
     (R [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f
     (R)]))

;; Block  : BlockDerivation -> RST
(define (Block bd)
  (match/count bd
    [(bderiv es1 es2 renames pass1 pass2)
     (R [#:pattern ?block]
        [#:do (STRICT-CHECKS
               (unless (equal? (stx->list (% ?block)) (stx->list es1))
                 (eprintf "MISMATCH (BLOCK): not equal\n  actual = ~.s\n  deriv  = ~.s\n"
                          (values (% ?block)) (values es1))))]
        [#:rename ?block (car renames) 'rename-block]
        [BlockPass ?block pass1]
        [#:if (block:letrec? pass2)
              ([BlockLetrec ?block pass2]
               [#:walk es2 'finish-block])
              ([#:rename ?block (wlderiv-es1 pass2)]
               [List ?block pass2])])]
    ;; Alternatively, allow lists, since `let`, etc., bodies
    ;; (generated form an internal definition context) are
    ;; processed as a list.
    [(lderiv es1 es2 ?1 derivs)
     (R [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f
     (R)]))

(define (BlockLetrec b)
  (match/count b
    [(block:letrec letrec-stx rhss body)
     (R [#:walk (list letrec-stx) 'block->letrec]
        [#:pattern ((?letrec-values ([?ids ?rhs] ...) . ?body))]
        [Expr (?rhs ...) rhss]
        [List ?body body])]))

;; BlockPass : (list-of BRule) -> RST
(define (BlockPass brules)
  (match/count brules
    ['()
     (R)]
    [(cons (b:error exn) rest)
     (R [! exn])]
    [(cons (b:splice head da ?1 tail ?2) rest)
     (R [#:pattern ?forms]
        [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [#:when da
         [#:rename ?first da #;'disarm]]
        [! ?1]
        [#:let begin-form (% ?first)]
        [#:let rest-forms (% ?rest)]
        [#:pattern ?forms]
        [#:walk (append (stx->list (stx-cdr begin-form)) rest-forms) 'splice-block
         #:foci (stx->list (stx-cdr begin-form)) #:from-foci (list begin-form)]
        [#:rename ?forms tail #;'track]
        [! ?2]
        [#:pattern ?forms]
        [BlockPass ?forms rest])]
    [(cons (b:defvals head da ?1 rename ?2) rest)
     (R [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [! ?1]
        [#:when da
         [#:rename ?first da #;'disarm]]
        [#:pattern ((?define-values ?vars . ?body) . ?rest)]
        [#:rename (?vars . ?body) rename]
        [#:do (learn-binders (% ?vars))]
        [! ?2]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (b:defstx head da ?1 rename ?2 prep bindrhs) rest)
     (R [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [! ?1]
        [#:when da
         [#:rename ?first da #;'disarm]]
        [#:pattern ((?define-syntaxes ?vars . ?body) . ?rest)]
        [#:rename (?vars . ?body) rename]
        [#:do (learn-binders (% ?vars))]
        [! ?2]
        [#:pattern ?form]
        [PrepareEnv ?form prep]
        [#:pattern ((?define-syntaxes ?vars ?rhs) . ?rest)]
        [BindSyntaxes ?rhs bindrhs]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (b:expr head) rest)
     (R [#:pattern (?first . ?rest)]
        [Expr ?first head]
        [BlockPass ?rest rest])]
    ))

;; BindSyntaxes : BindSyntaxes -> RST
(define (BindSyntaxes bindrhs)
  (match bindrhs
    [(bind-syntaxes rhs locals)
     (R [#:pattern ?form]
        [Expr/PhaseUp ?form rhs]
        [LocalActions ?form locals])]))

;; ModEnsureMB : (U ModEnsureMB ModAddMB) -> RST
(define (ModEnsureMB emb)
  (match emb
    [(mod:ensure-mb track1 check add-mb track2)
     (R [#:pattern (?module ?name ?lang . ?bodys)]
        [#:when track1
         [#:pattern (?module ?name ?lang ?body)]
         [#:rename ?body (cadr track1) #;'property]
         [#:when check
          [Expr ?body check]]]
        => (ModEnsureMB add-mb)
        [#:pattern (?module ?name ?lang ?body)]
        [#:rename ?body (cadr track2) #;'property])]
    [(mod:add-mb ?1 tag track check ?2)
     (R [#:pattern (?module ?name ?lang . ?bodys)]
        [! ?1]
        [#:in-hole ?bodys
         [#:walk (list tag) 'tag-module-begin]]
        [#:pattern (?module ?name ?lang ?body)]
        [#:rename ?body (cadr track) #;'property]
        [Expr ?body check]
        [! ?2])]
    [#f (R)]))

(define (BeginForSyntax passes)
  ;; Note: an lderiv doesn't necessarily cover all stxs, due to lifting.
  (match/count passes
    [(cons (? lderiv? lderiv) '())
     (R [#:pattern ?forms]
        [List ?forms lderiv])]
    [(cons (bfs:lift lderiv stxs) rest)
     (R [#:pattern ?forms]
        [List ?forms lderiv]
        ;; FIXME...
        [#:walk (append stxs (% ?forms)) 'splice-lifts
         #:foci stxs #:from-foci null]
        [BeginForSyntax ?forms rest])]))

(define (ModPass1And2 pass12)
  (match/count pass12
    [(mod:pass-1-and-2 pass1 pass2)
     (R [#:pattern ?forms]
        [ModulePass1 ?forms pass1]
        [ModulePass2 ?forms pass2])]))

;; Synthetic fine-grained pass1 steps
(struct modp1*:head (head) #:transparent)
(struct modp1*:case (prim) #:transparent)
(struct modp1*:lift (lifted-defs lifted-reqs lifted-mods) #:transparent)

;; ModulePass1 : (Listof ModRule1) -> RST
(define (ModulePass1 mbrules)
  (match/count mbrules
    ['()
     (R)]
    [(cons (mod:lift-end stxs) rest)
     (R [#:pattern ?forms]
        [#:when (pair? stxs)
         [#:walk (append stxs (stx->list (% ?forms))) 'splice-end-lifts
          #:foci stxs #:from-foci null]]
        [ModulePass1 ?forms rest])]
    ;; A modp1:prim node isn't ideally matched to the structure of steps that
    ;; must be generated (for splicing, lifting, etc). So translate it just in
    ;; time to a list of fine-grained steps.
    [(cons (modp1:prim head prim) rest)
     (let ([mbrules*
            (match head
              [(modp1:lift head* lifted-defs lifted-reqs lifted-mods mods)
               `(,(modp1*:head head*)
                 ,(modp1*:lift lifted-defs lifted-reqs lifted-mods)
                 ,@(make-list (+ (length lifted-defs) (length lifted-reqs)) #f)
                 ,@mods
                 ,(modp1*:case prim)
                 ,@rest)]
              [(? deriv? head*)
               (list* (modp1*:head head*) (modp1*:case prim) rest)])])
       (R [#:pattern ?forms]
          [ModulePass1 ?forms mbrules*]))]

    ;; Synthetic fine-grained pseudo-derivs
    [(cons (modp1*:head head) rest)
     (R [#:pattern (?firstP . ?rest)]
        ;; FIXME: #:pass1 / #:pass2 ???
        [Expr ?firstP head]
        [#:pattern ?forms]
        [ModulePass1 ?forms rest])]
    [(cons (modp1*:case (modp1:splice da ?1 tail)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:when da
         [#:rename ?first da #;'disarm]]
        [#:let begin-form (% ?first)]
        [#:let rest-forms (% ?rest)]
        [#:pattern ?forms]
        [#:walk (append (stx->list (stx-cdr begin-form)) (stx->list rest-forms)) 'splice-module
         #:foci (stx->list (stx-cdr begin-form)) #:from-foci (list begin-form)]
        [#:rename ?forms tail #;'track]
        [ModulePass1 ?forms rest])]
    [(cons (modp1*:case (? p:submodule? deriv)) rest)
     (R [#:pattern (?first . ?rest)]
        ;; For submodules, there is an implicit rename: remove use-site scopes
        [#:rename ?first (wderiv-e1 deriv)]
        [Expr ?first deriv]
        [ModulePass1 ?rest rest])]
    [(cons (modp1*:case (? prule? prim)) rest)
     (R [#:pattern (?firstP . ?rest)]
        [Expr ?firstP prim]
        [ModulePass1 ?rest rest])]
    [(cons (modp1*:lift lifted-defs lifted-reqs lifted-mods) rest)
     (R [#:pattern ?forms]
        [#:walk (append lifted-defs lifted-reqs lifted-mods (stx->list (% ?forms))) 'splice-lifts
         #:foci (append lifted-defs lifted-reqs lifted-mods) #:from-foci null]
        [ModulePass1 ?forms rest])]
    [(cons #f rest)
     (R [#:pattern (?first . ?rest)]
        [ModulePass1 ?rest rest])]))

;; ModulePass2 : (Listof ModRule2) -> RST
(define (ModulePass2 mbrules)
  (match/count mbrules
    ['()
     (R)]
    [(cons (mod:lift-end stxs) rest)
     (R [#:pattern ?forms]
        [#:when (pair? stxs)
         [#:walk (append stxs (stx->list (% ?forms))) 'splice-end-lifts
          #:foci stxs #:from-foci null]]
        [ModulePass2 ?forms rest])]
    [(cons (modp2:skip) rest)
     (R [#:pattern (?first . ?rest)]
        [ModulePass2 ?rest rest])]
    [(cons (modp2:cons deriv locals) rest)
     (R [#:pattern (?first . ?rest)]
        [Expr ?first deriv]
        [LocalActions ?first locals]
        [ModulePass2 ?rest rest])]
    [(cons (modp2:lift deriv locals lifted-reqs lifted-mods lifted-defs mods defs) rest)
     (let ([mbrules*
            `(,@(make-list (length lifted-reqs) (modp2:skip))
              ,@(map (lambda (d) (if d (modp2:cons d null) (modp2:skip))) mods)
              ,@defs
              ,(modp2:skip)
              ,@rest)])
       (R [#:pattern (?first . ?rest)]
          [Expr ?first deriv]
          [LocalActions ?first locals]
          [#:pattern ?forms]
          [#:walk (append lifted-reqs lifted-mods lifted-defs (stx->list (% ?forms))) 'splice-lifts
           #:foci (append lifted-reqs lifted-mods lifted-defs) #:from-foci null]
          [ModulePass2 ?forms mbrules*]))]))

(define (ModulePass3 pass3)
  (match/count pass3
    ['()
     (R)]
    [(cons #f rest)
     (R [#:pattern (?first . ?rest)]
        [ModulePass3 ?rest rest])]
    [(cons (modp34:bfs subs) rest)
     (R [#:pattern ((?bfs . ?subs) . ?rest)]
        [ModulePass3 ?subs subs]
        [ModulePass3 ?rest rest])]
    [(cons (? p:provide? deriv) rest)
     (R [#:pattern (?first . ?rest)]
        [Expr ?first deriv]
        [ModulePass3 ?rest rest])]))

(define (ModulePass4 pass4)
  (match/count pass4
    ['()
     (R)]
    [(cons #f rest)
     (R [#:pattern (?first . ?rest)]
        [ModulePass4 ?rest rest])]
    [(cons (modp34:bfs subs) rest)
     (R [#:pattern ((?bfs . ?subs) . ?rest)]
        [ModulePass4 ?subs subs]
        [ModulePass4 ?rest rest])]
    [(cons (? p:submodule*? deriv) rest)
     (R [#:pattern (?first . ?rest)]
        ;; Implicit rename: remove use-site scopes, possibly phase-shift
        [#:rename ?first (wderiv-e1 deriv)]
        [Expr ?first deriv]
        [ModulePass4 ?rest rest])]))

;; Lifts

(define (expr->local-action d)
  (match d
    [(deriv e1 e2)
     (make local-expansion e1 e2
           #f e1 d #f e2 #f)]))

;; opaque-table
;; Weakly remembers assoc between opaque values and
;; actual syntax, so that actual can be substituted in
;; for destructuring.
;; FIXME: perhaps add event for opaque-stx unwrapping?
(define opaque-table (make-weak-hasheq))
