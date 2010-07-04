
#lang scheme/base
(require scheme/match
         "stx-util.ss"
         "deriv-util.ss"
         "deriv.ss"
         "reductions-engine.ss")

(provide reductions
         reductions+)

;; Reductions

;; reductions : WDeriv -> ReductionSequence
(define (reductions d)
  (let-values ([(steps definites estx exn) (reductions+ d)])
    steps))

;; reductions+ : WDeriv -> (list-of step) (list-of identifier) ?stx ?exn
(define (reductions+ d)
  (parameterize ((current-definites null)
                 (current-frontier null)
                 (hides-flags (list (box #f)))
                 (sequence-number 0))
    (RScase ((Expr d) (wderiv-e1 d) (wderiv-e1 d) #f null)
            (lambda (steps stx vstx s)
              (values (reverse steps) (current-definites) vstx #f))
            (lambda (steps exn)
              (values (reverse steps) (current-definites) #f exn)))))

;; Syntax

(define-syntax-rule (match/count x . clauses)
  (begin (sequence-number (add1 (sequence-number)))
         (match x . clauses)))

;; Derivations => Steps

;; Expr : Deriv -> RST
(define (Expr d)
  (match/count d
    [(Wrap deriv (e1 e2))
     (R [#:pattern ?form]
        [#:let transparent-stx (hash-ref opaque-table (syntax-e #'?form) #f)]
        [#:when transparent-stx
                [#:set-syntax transparent-stx]]
        [#:expect-syntax e1 (list d)]
        [#:when (base? d)
                [#:learn (or (base-resolves d) null)]]
        [#:seek-check]
        [Expr* ?form d]
        [#:when (not (current-pass-hides?))
                [#:set-syntax e2]])]
    [#f
     (R [#:seek-check]
        => (Expr* d))]))

(define (Expr* d)
  (match d
    ;; Primitives
    [(Wrap p:variable (e1 e2 rs ?1))
     (R [#:learn (list e2)]
        [#:when (or (not (identifier? e1))
                    (not (bound-identifier=? e1 e2)))
                [#:walk e2 'resolve-variable]])]
    [(Wrap p:module (e1 e2 rs ?1 ?2 tag rename check tag2 ?3 body shift))
     (R [#:hide-check rs]
        [! ?1]
        [#:pattern (?module ?name ?language . ?body-parts)]
        [! ?2]
        [#:when tag
                [#:in-hole ?body-parts
                           [#:walk (list tag) 'tag-module-begin]]]
        [#:pattern (?module ?name ?language ?body)]
        [#:rename ?body rename]
        [#:pass1]
        [#:when check
                [Expr ?body check]]
        [#:when tag2
                [#:in-hole ?body
                           [#:walk tag2 'tag-module-begin]]]
        [#:pass2]
        [! ?3]
        [Expr ?body body]
        [#:pattern ?form]
        [#:rename ?form shift])]
    [(Wrap p:#%module-begin (e1 e2 rs ?1 me pass1 pass2 ?2))
     (R [! ?1]
        [#:pattern ?form]
        [#:rename ?form me]
        [#:pattern (?module-begin . ?forms)]
        [#:pass1]
        [ModulePass ?forms pass1]
        [#:pass2]
        [#:do (DEBUG (printf "** module begin pass 2\n"))]
        [ModulePass ?forms pass2]
        [! ?1])]
    [(Wrap p:define-syntaxes (e1 e2 rs ?1 rhs ?2))
     (R [! ?1]
        [#:pattern (?define-syntaxes formals ?rhs)]
        [Expr/PhaseUp ?rhs rhs]
        [! ?2])]
    [(Wrap p:define-values (e1 e2 rs ?1 rhs))
     (R [! ?1]
        [#:pattern (?define-values ?formals ?rhs)]
        [#:when rhs
                [Expr ?rhs rhs]]
        [#:when (not rhs)
                [#:do (DEBUG (printf "=== end (dvrhs) ===\n"))]
                [#:do (DEBUG (printf "===\n"))]])]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner #f))
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [Expr ?inner inner])]
    [(Wrap p:#%expression (e1 e2 rs ?1 inner untag))
     (R [! ?1]
        [#:pattern (?expr-kw ?inner)]
        [#:pass1]
        [Expr ?inner inner]
        [#:pattern ?form]
        [#:let oldform #'?form]
        [#:with-visible-form
         [#:left-foot]
         [#:set-syntax (stx-car (stx-cdr #'?form))]
         [#:step 'macro]]
        [#:pass2]
        [#:set-syntax (stx-car (stx-cdr oldform))]
        [#:rename ?form untag])]
    [(Wrap p:if (e1 e2 rs ?1 test then else))
     (R [! ?1]
        [#:pattern (?if TEST THEN ELSE)]
        [Expr TEST test]
        [Expr THEN then]
        [Expr ELSE else])]
    [(Wrap p:wcm (e1 e2 rs ?1 key mark body))
     (R [! ?1]
        [#:pattern (?wcm KEY MARK BODY)]
        [Expr KEY key]
        [Expr MARK mark]
        [Expr BODY body])]
    [(Wrap p:begin (e1 e2 rs ?1 lderiv))
     (R [! ?1]
        [#:pattern (?begin . ?lderiv)]
        [List ?lderiv lderiv])]
    [(Wrap p:begin0 (e1 e2 rs ?1 first lderiv))
     (R [! ?1]
        [#:pattern (?begin0 FIRST . LDERIV)]
        [Expr FIRST first]
        [List LDERIV lderiv])]
    [(Wrap p:#%app (e1 e2 rs ?1 lderiv))
     (R [! ?1]
        [#:pattern (?app . LDERIV)]
        [#:if lderiv
              ([List LDERIV lderiv])
              ([#:walk e2 'macro])])]
    [(Wrap p:lambda (e1 e2 rs ?1 renames body))
     (R [! ?1]
        [#:pattern (?lambda ?formals . ?body)]
        [#:rename (?formals . ?body) renames 'rename-lambda]
        [Block ?body body])]
    [(Wrap p:case-lambda (e1 e2 rs ?1 clauses))
     (R [! ?1]
        [#:pattern (?case-lambda . ?clauses)]
        [CaseLambdaClauses ?clauses clauses])]
    [(Wrap p:let-values (e1 e2 rs ?1 renames rhss body))
     (R [! ?1]
        [#:pattern (?let-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-let-values]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-values (e1 e2 rs ?1 renames rhss body))
     (R [! ?1]
        [#:pattern (?letrec-values ([?vars ?rhs] ...) . ?body)]
        [#:rename (((?vars ?rhs) ...) . ?body) renames 'rename-letrec-values]
        [Expr (?rhs ...) rhss]
        [Block ?body body])]
    [(Wrap p:letrec-syntaxes+values
           (e1 e2 rs ?1 srenames srhss vrenames vrhss body tag))
     (R [! ?1]
        [#:pass1]
        [#:pattern (?lsv ([?svars ?srhs] ...) ([?vvars ?vrhs] ...) . ?body)]
        [#:rename (((?svars ?srhs) ...) ((?vvars ?vrhs) ...) . ?body)
                   srenames
                   'rename-lsv]
        [BindSyntaxes (?srhs ...) srhss]
        ;; If vrenames is #f, no var bindings to rename
        [#:when vrenames
                [#:rename (((?vvars ?vrhs) ...) . ?body) vrenames 'rename-lsv]]
        [Expr (?vrhs ...) vrhss]
        [Block ?body body]
        [#:pass2]
        [#:pattern ?form]
        [#:when tag
                [#:walk tag 'lsv-remove-syntax]])]
    [(Wrap p:#%datum (e1 e2 rs ?1))
     (R [! ?1]
        [#:hide-check rs]
        [#:walk e2 'macro])]
    [(Wrap p:#%top (e1 e2 rs ?1))
     (R [! ?1]
        [#:pattern (?top . ?var)]
        [#:learn (list #'?var)])]

    [(Wrap p:provide (e1 e2 rs ?1 inners ?2))
     (let ([wrapped-inners
            (for/list ([inner inners])
              (match inner
                [(Wrap deriv (e1 e2))
                 (make local-expansion e1 e2
                       #f e1 inner #f e2 #f)]))])
       (R [! ?1]
          [#:pattern ?form]
          [#:pass1]
          [#:left-foot]
          [LocalActions ?form wrapped-inners]
          [! ?2]
          [#:pass2]
          [#:set-syntax e2]
          [#:step 'provide]
          [#:set-syntax e2]))]

    [(Wrap p:stop (e1 e2 rs ?1))
     (R [! ?1])]

    ;; The rest of the automatic primitives
    [(Wrap p::STOP (e1 e2 rs ?1))
     (R [! ?1])]

    [(Wrap p:set!-macro (e1 e2 rs ?1 deriv))
     (R [! ?1]
        [#:pattern ?form]
        [Expr ?form deriv])]
    [(Wrap p:set! (e1 e2 rs ?1 id-rs rhs))
     (R [! ?1]
        [#:pattern (?set! ?var ?rhs)]
        [#:learn id-rs]
        [Expr ?rhs rhs])]

    ;; Macros
    [(Wrap mrule (e1 e2 rs ?1 me1 locals me2 ?2 etx next))
     (R [! ?1]
        [#:pattern ?form]
        [#:hide-check rs]
        [#:learn rs]
        [#:pass1]
        [#:left-foot]
        [#:rename/mark ?form e1 me1] ;; MARK
        [LocalActions ?form locals]
        [! ?2]
        [#:pass2]
        [#:set-syntax me2]
        [#:rename/unmark ?form me2 etx] ;; UNMARK
        [#:step 'macro]
        [#:set-syntax etx]
        [Expr ?form next])]

    [(Wrap tagrule (e1 e2 tagged-stx next))
     (R [#:pattern ?form]
        [#:hide-check (list (stx-car tagged-stx))]
        [#:walk tagged-stx
                (case (syntax-e (stx-car tagged-stx))
                  ((#%app) 'tag-app)
                  ((#%datum) 'tag-datum)
                  ((#%top) 'tag-top)
                  (else
                   (error 'reductions "unknown tagged syntax: ~s" tagged-stx)))]
        [Expr ?form next])]

    ;; Lifts

    [(Wrap lift-deriv (e1 e2 first lifted-stx second))
     (R [#:pattern ?form]
        ;; lifted-stx has form (begin lift-n ... lift-1 orig-expr)
        [#:let avail (cdr (reverse (stx->list (stx-cdr lifted-stx))))]
        [#:parameterize ((available-lift-stxs avail)
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?form first]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'lift-deriv "available lifts left over"))]
          [#:with-visible-form
           ;; If no lifts visible, then don't show begin-wrapping
           [#:when (pair? (visible-lift-stxs))
                   [#:walk (reform-begin-lifts lifted-stx
                                               (visible-lift-stxs)
                                               #'?form)
                           'capture-lifts]]]
          [#:pass2]
          [#:set-syntax lifted-stx]
          [Expr ?form second]])]

    [(Wrap lift/let-deriv (e1 e2 first lifted-stx second))
     (R [#:pattern ?form]
        ;; lifted-stx has form
        ;; (let-values ((last-v last-lifted))
        ;;   ...
        ;;     (let-values ((first-v first-lifted)) orig-expr))
        [#:let avail lifted-stx]
        [#:parameterize ((available-lift-stxs avail)
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?form first]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'lift/let-deriv "available lifts left over"))]
          [#:let visible-lifts (visible-lift-stxs)]
          [#:with-visible-form
           [#:left-foot]
           [#:set-syntax (reform-let-lifts lifted-stx visible-lifts #'?form)]
           [#:step 'capture-lifts]]
          [#:pass2]
          [#:set-syntax lifted-stx]
          [Expr ?form second]])]

    ;; Skipped
    [#f
     (R)]))

;; Expr/PhaseUp : Deriv -> RST
(define (Expr/PhaseUp d)
  (R [#:parameterize ((phase (add1 (phase))))
     => (Expr* d)]))

;; case-lambda-clauses-reductions : 
;;   (list-of (W (list ?exn rename (W BDeriv)))) stxs -> RST
(define (CaseLambdaClauses clauses)
  (match/count clauses
    ['()
     (R)]
    [(cons (Wrap clc (?1 rename body)) rest)
     (R [! ?1]
        [#:pattern ((?formals . ?body) . ?rest)]
        [#:rename (?formals . ?body) rename 'rename-case-lambda]
        [Block ?body body]
        [CaseLambdaClauses ?rest rest])]))

;; local-actions-reductions
(define (LocalActions locals)
  (match locals
    ['()
     (R)]
    [(cons local rest)
     (R [#:pattern ?form]
        [#:if (visibility)
              ;; If macro with local-expand is transparent,
              ;; then all local-expansions must be transparent.
              ([#:parameterize ((macro-policy (lambda _ #t)))
                 [#:new-local-context
                  [LocalAction ?form local]]])
              ([#:pass1]
               [LocalAction ?form local]
               [#:pass2])]
        [LocalActions ?form rest])]))

(define (LocalAction local)
  (match/count local
    [(struct local-expansion (e1 e2 for-stx? me1 inner #f me2 opaque))
     (R [#:parameterize ((phase (if for-stx? (add1 (phase)) (phase))))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/mark ?form e1 me1]
         [Expr ?form inner]
         [#:rename/mark ?form me2 e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(struct local-expansion (e1 e2 for-stx? me1 inner lifted me2 opaque))
     (R [#:let avail
               (if for-stx?
                   lifted
                   (cdr (reverse (stx->list (stx-cdr lifted)))))]
        [#:let recombine
               (lambda (lifts form)
                 (if for-stx?
                     (reform-let-lifts lifted lifts form)
                     (reform-begin-lifts lifted lifts form)))]
        [#:parameterize ((phase (if for-stx? (add1 (phase)) (phase)))
                         (available-lift-stxs avail)
                         (visible-lift-stxs null))
         [#:set-syntax e1]
         [#:pattern ?form]
         [#:rename/unmark ?form e1 me1]
         [#:pass1]
         [Expr ?form inner]
         [#:do (when (pair? (available-lift-stxs))
                 (lift-error 'local-expand/capture-lifts
                             "available lifts left over"))]
         [#:let visible-lifts (visible-lift-stxs)]
         [#:with-visible-form
          [#:left-foot]
          [#:set-syntax (recombine visible-lifts #'?form)]
          [#:step 'splice-lifts visible-lifts]]
         [#:pass2]
         [#:set-syntax lifted]
         [#:rename/mark ?form me2 e2]
         [#:do (when opaque
                 (hash-set! opaque-table (syntax-e opaque) e2))]])]

    [(struct local-lift (expr ids))
     ;; FIXME: add action
     (R [#:do (take-lift!)]
        [#:reductions (list (walk expr ids 'local-lift))])]

    [(struct local-lift-end (decl))
     ;; (walk/mono decl 'module-lift)
     (R)]
    [(struct local-lift-require (req expr mexpr))
     ;; lift require
     (R [#:set-syntax expr]
        [#:pattern ?form]
        [#:rename/mark ?form expr mexpr])]
    [(struct local-lift-provide (prov))
     ;; lift provide
     (R)]
    [(struct local-bind (names ?1 renames bindrhs))
     [R [! ?1]
        ;; FIXME: use renames
        [#:when bindrhs => (BindSyntaxes bindrhs)]]]))

;; List : ListDerivation -> RST
(define (List ld)
  (match ld
    [(Wrap lderiv (es1 es2 ?1 derivs))
     (R [! ?1]
        [#:pattern (?form ...)]
        [Expr (?form ...) derivs])]
    [#f
     (R)]))

;; Block  : BlockDerivation -> RST
(define (Block bd)
  (match/count bd
    [(Wrap bderiv (es1 es2 pass1 trans pass2))
     (R [#:pattern ?block]
        [#:parameterize ((block-syntax-bindings null)
                         (block-value-bindings null)
                         (block-expressions null))
          [#:pass1]
          [BlockPass ?block pass1]
          [#:pass2]
          [#:when (eq? trans 'letrec)
                  [#:walk
                   (let* ([pass2-stxs (wlderiv-es1 pass2)]
                          [letrec-form (car pass2-stxs)]
                          [letrec-kw (stx-car letrec-form)]
                          [stx-bindings (reverse (block-syntax-bindings))]
                          [val-bindings (reverse (block-value-bindings))]
                          [exprs (block-expressions)]
                          [mk-letrec-form (lambda (x) (datum->syntax #f x))])
                     (list
                      (mk-letrec-form
                       `(,letrec-kw ,@(if (pair? stx-bindings)
                                          (list stx-bindings)
                                          null)
                                    ,val-bindings
                                    . ,exprs))))
                   'block->letrec]]
          [#:rename ?block (wlderiv-es1 pass2)]
          [#:set-syntax (wlderiv-es1 pass2)]
          [List ?block pass2]])]
    [#f
     (R)]))

;; BlockPass : (list-of BRule) -> RST
(define (BlockPass brules)
  (match/count brules
    ['()
     (R)]
    [(cons (Wrap b:error (exn)) rest)
     (R [! exn])]
    [(cons (Wrap b:splice (renames head ?1 tail ?2)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pass2]
        [#:let begin-form #'?first]
        [#:let rest-forms #'?rest]
        [#:pattern ?forms]
        [#:left-foot (list begin-form)]
        [#:set-syntax (append (stx->list (stx-cdr begin-form)) rest-forms)]
        [#:step 'splice-block (stx->list (stx-cdr begin-form))]
        [#:rename ?forms tail]
        [! ?2]
        [#:pattern ?forms]
        [BlockPass ?forms rest])]

    ;; FIXME: are these pass1/2 necessary?

    [(cons (Wrap b:defvals (renames head ?1 rename ?2)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pass2]
        [#:pattern ((?define-values . ?clause) . ?rest)]
        [#:rename ?clause rename]
        [! ?2]
        [#:do (block-value-bindings
               (cons #'?clause (block-value-bindings)))]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (Wrap b:defstx (renames head ?1 rename ?2 bindrhs)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [#:pass1]
        [Expr ?first head]
        [! ?1]
        [#:pass2]
        [#:pattern ((?define-syntaxes . ?clause) . ?rest)]
        [#:rename ?clause rename]
        [! ?2]
        [#:do (block-syntax-bindings
               (cons #'?clause (block-syntax-bindings)))]
        [#:pattern ((?define-syntaxes ?vars ?rhs) . ?rest)]
        [BindSyntaxes ?rhs bindrhs]
        [#:pattern (?first . ?rest)]
        [BlockPass ?rest rest])]
    [(cons (Wrap b:expr (renames head)) rest)
     (R [#:pattern (?first . ?rest)]
        [#:rename/no-step ?first (car renames) (cdr renames)]
        [Expr ?first head]
        [#:do (block-expressions #'(?first . ?rest))]
        ;; rest better be empty
        [BlockPass ?rest rest])]

    ))

;; BindSyntaxes : BindSyntaxes -> RST
(define (BindSyntaxes bindrhs)
  (match bindrhs
    [(Wrap bind-syntaxes (rhs ?1))
     (R [#:set-syntax (node-z1 rhs)] ;; set syntax; could be in local-bind
        [#:pattern ?form]
        [Expr/PhaseUp ?form rhs]
        [! ?1])]))

;; ModulePass : (list-of MBRule) -> RST
(define (ModulePass mbrules)
  (match/count mbrules
    ['()
     (R)]
    [(cons (Wrap mod:prim (head rename prim)) rest)
     (R [#:pattern (?firstP . ?rest)]
        [Expr ?firstP head]
        [#:do (DEBUG (printf "** after head\n"))]
        [#:rename ?firstP rename]
        [#:do (DEBUG (printf "** after rename\n"))]
        [#:when prim
                [Expr ?firstP prim]]
        [#:do (DEBUG (printf "** after prim\n"))]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:splice (head rename ?1 tail)) rest)
     (R [#:pattern (?firstB . ?rest)]
        [#:pass1]
        [Expr ?firstB head]
        [#:pass2]
        [#:rename ?firstB rename]
        [! ?1]
        [#:let begin-form #'?firstB]
        [#:let rest-forms #'?rest]
        [#:pattern ?forms]
        [#:left-foot (list #'?firstB)]
        [#:set-syntax (append (stx->list (stx-cdr begin-form)) rest-forms)]
        [#:step 'splice-module (stx->list (stx-cdr begin-form))]
        [#:rename ?forms tail]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:lift (head renames stxs)) rest)
     (R [#:pattern (?firstL . ?rest)]
        ;; renames has form (head-e2 . ?rest)
        ;; stxs has form (lifted ...),
        ;;   specifically (last-lifted ... first-lifted)
        [#:parameterize ((available-lift-stxs (reverse stxs))
                         (visible-lift-stxs null))
          [#:pass1]
          [Expr ?firstL head]
          [#:do (when (pair? (available-lift-stxs))
                  (lift-error 'mod:lift "available lifts left over"))]
          [#:let visible-lifts (visible-lift-stxs)]
          [#:pattern ?forms]
          [#:pass2]
          [#:when renames
                  [#:rename ?forms renames]]
          [#:let old-forms #'?forms]
          [#:left-foot null]
          [#:set-syntax (append visible-lifts old-forms)]
          [#:step 'splice-lifts visible-lifts]
          [#:set-syntax (append stxs old-forms)]
          [ModulePass ?forms rest]])]
    [(cons (Wrap mod:lift-end (stxs)) rest)
     (R [#:pattern ?forms]
        [#:when (pair? stxs)
                [#:left-foot null]
                [#:set-syntax (append stxs #'?forms)]
                [#:step 'splice-module-lifts stxs]]
        [ModulePass ?forms rest])]
    [(cons (Wrap mod:skip ()) rest)
     (R [#:pattern (?firstS . ?rest)]
        [ModulePass ?rest rest])]
    [(cons (Wrap mod:cons (head)) rest)
     (R [#:pattern (?firstC . ?rest)]
        [Expr ?firstC head]
        [ModulePass ?rest rest])]))

;; Lifts

(define (take-lift!)
  (define avail (available-lift-stxs))
  (cond [(list? avail)
         (unless (pair? avail)
           (lift-error 'local-lift "out of lifts (begin)!"))
         (when (pair? avail)
           (let ([lift-stx (car avail)])
             (available-lift-stxs (cdr avail))
             (when (visibility)
               (visible-lift-stxs
                (cons lift-stx (visible-lift-stxs))))))]
        [else
         (syntax-case avail ()
           [(?let-values ?lift ?rest)
            (eq? (syntax-e #'?let-values) 'let-values)
            (begin (available-lift-stxs #'?rest)
                   (when (visibility)
                     (visible-lift-stxs
                      (cons (datum->syntax avail (list #'?let-values #'?lift)
                                           avail avail)
                            (visible-lift-stxs)))))]
           [_
            (lift-error 'local-lift "out of lifts (let)!")])]))

(define (reform-begin-lifts orig-lifted lifts body)
  (define begin-kw (stx-car orig-lifted))
  (datum->syntax orig-lifted
                 `(,begin-kw ,@lifts ,body)
                 orig-lifted
                 orig-lifted))

(define (reform-let-lifts orig-lifted lifts body)
  (if (null? lifts)
      body
      (reform-let-lifts orig-lifted
                        (cdr lifts)
                        (with-syntax ([(?let-values ?lift) (car lifts)])
                          (datum->syntax (car lifts)
                                         `(,#'?let-values ,#'?lift ,body)
                                         (car lifts)
                                         (car lifts))))))

;; lift-error
(define (lift-error sym . args)
  (apply fprintf (current-error-port) args)
  (newline (current-error-port))
  (when #f
    (apply error sym args)))

;; opaque-table
;; Weakly remembers assoc between opaque values and
;; actual syntax, so that actual can be substituted in
;; for destructuring.
;; FIXME: perhaps add event for opaque-stx unwrapping?
(define opaque-table (make-weak-hasheq))
