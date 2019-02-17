#lang racket/base
(require (for-syntax racket/base)
         syntax/stx
         "yacc-ext.rkt"
         "yacc-interrupted.rkt"
         "deriv.rkt"
         "deriv-util.rkt"
         "deriv-tokens.rkt")
(provide parse-derivation)

(define (deriv-error ok? name value start end)
  (if ok?
      (error 'derivation-parser
             "error on token #~a: <~s, ~s>"
             start name value)
      (error 'derivation-parser "bad token #~a" start)))

;; PARSER

(define-production-splitter production/I values values)

(define-syntax (productions/I stx)
  (syntax-case stx ()
    [(productions/I def ...)
     #'(begin (production/I def) ...)]))

(define parse-derivation
  (parser
   (options (start Expansion)
            (src-pos)
            (tokens basic-empty-tokens basic-tokens prim-tokens)
            (end EOF)
            #| (debug "/tmp/DEBUG-PARSER.txt") |#
            (error deriv-error))

   ;; tokens
   (skipped-token-values
    visit resolve next next-group return
    enter-macro macro-pre-x macro-post-x exit-macro
    enter-prim exit-prim
    enter-block block->list block->letrec splice
    enter-list exit-list
    enter-check exit-check
    local-post exit-local exit-local/expr
    local-bind enter-bind exit-bind exit-local-bind
    local-value-result local-value-binding
    phase-up module-body
    lambda-renames
    let-renames
    letrec-syntaxes-renames
    block-renames
    rename-one
    rename-list
    tag
    IMPOSSIBLE
    start start-top start-ecte
    top-non-begin
    prepare-env)

    ;; Entry point
   (productions
    (Expansion
     [(start-ecte ExpandCTE) $2]
     [(start-ecte ExpandCTE/Interrupted) $2]
     [(MainExpand) $1]
     [(MainExpand/Interrupted) $1]))

   (productions/I

    ;; ----------------------------------------
    ;; ./trace.rkt expand/compile-time-evals

    (ExpandCTE
     ;; The first 'Eval' is there for---I believe---lazy phase 1 initialization.
     [(visit (? MainExpandToTop) top-non-begin (? MainExpand) (? Eval) return)
      (make ecte $1 $6 null $2 $4 $5)]
     [(visit MainExpandToTop top-begin (? NextExpandCTEs) return)
      (make ecte $1 $5 null $2
            (let ([b-e1 $3] [b-e2 $5])
              (make p:begin b-e1 b-e2 (list (stx-car b-e1)) #f
                    (derivs->lderiv (stx-cdr b-e1) $4)))
            null)])

    (NextExpandCTEs
     (#:skipped null)
     [() null]
     [(next (? ExpandCTE) (? NextExpandCTEs)) (cons $2 $3)])

    ;; ----------------------------------------
    ;; src/eval/main.rkt expand and expand-to-top-form

    (MainExpand
     [(start-top (? PTLL)) $2])

    (MainExpandToTop
     [(start-top (? PTLL)) $2])

    (PTLL ;; per-top-level loop
     [(visit (? ECL) return)
      $2]
     [(visit ECL (? EE))
      (let ([e2 (and $3 (node-z2 $3))])
        (make ecte $1 e2 null $2 $3 null))]
     [(visit ECL lift-loop (? PTLL))
      (make lift-deriv $1 (wderiv-e2 $4) $2 $3 $4)]
     [(visit ECL prim-begin ! (? NextPTLLs) return)
      (make ecte $1 $6 null $2
            (let* ([b-e1 (and $2 (node-z2 $2))]
                   [ld (and b-e1 (derivs->lderiv (stx-cdr b-e1) $5))])
              (make p:begin b-e1 $6 (list (stx-car b-e1)) $4 ld))
            null)]
     [(visit ECL prim-begin-for-syntax ! (? PrepareEnv) (? NextPTLLs) return)
      (make ecte $1 $7 null $2
            (let* ([b-e1 (and $2 (node-z2 $2))]
                   [ld (and b-e1 (derivs->lderiv (stx-cdr b-e1) $6))])
              (make p:begin-for-syntax b-e1 $7 (list (stx-car b-e1)) $4 $5 ld null))
            null)])

    (NextPTLLs
     (#:skipped null)
     [() null]
     [(next (? PTLL) (? NextPTLLs)) (cons $2 $3)])

    (ECL ;; expand-capturing-lifts
     [((? CheckImmediateMacro)) $1])

    ;; ----------------------------------------
    ;; EE = src/expander/expand/main.rkt expand
    ;; CheckImmediateMacro = like EE but with ctx w/ only-immediate?=#t

    ;; Expand, convert lifts to let (rhs of define-syntaxes, mostly)
    (EE/LetLifts
     [((? EE)) $1]
     [(EE letlift-loop (? EE/LetLifts))
      (let ([initial (wderiv-e1 $1)]
            [final (wderiv-e2 $3)])
        (make lift/let-deriv initial final $1 $2 $3))])

    ;; Evaluation
    ;; Answer = (listof LocalAction)
    (Eval
     (#:skipped null)
     [((? LocalActions)) $1])

    ;; Prepare env for compilation
    (PrepareEnv
     [(prepare-env (? Eval)) $2])

    ;; Expansion of an expression to primitive form
    (CheckImmediateMacro
     [(enter-check (? CheckImmediateMacro/Inner) exit-check)
      ($2 $1 $3)])
    (CheckImmediateMacro/Inner
     (#:args le1 e2)
     [(!)
      (make p:stop le1 e2 null $1)]
     [(visit Resolves (? MacroStep) return (? CheckImmediateMacro/Inner))
      ($3 $1 $2 ($5 $4 e2))]
     [(visit Resolves tag (? MacroStep) return (? CheckImmediateMacro/Inner))
      (let ([mnode ($4 $3 $2 ($6 $5 e2))])
        (make tagrule $1 (wderiv-e2 mnode) $3 mnode))])

    ;; Expansion of multiple expressions, next-separated
    (NextEEs
     (#:skipped null)
     [() null]
     [(next (? EE) (? NextEEs)) (cons $2 $3)])

    ;; EE

    ;; Expand expression (term)
    (EE
     [(visit Resolves (? EE/k))
      ($3 $1 $2)]
     [(visit Resolves tag (? EE/k))
      (let ([next ($4 $3 $2)])
        (make tagrule $1 (wderiv-e2 next) $3 next))]
     [(visit opaque-expr)
      (make p:stop $1 $2 null #f)])

    (EE/k
     (#:args e1 rs)
     [(!!)
      (make p:unknown e1 #f rs $1)]
     [(variable return)
      (make p:variable e1 $2 rs #f)]
     [(enter-prim (? Prim) exit-prim return)
      (begin
        (unless (eq? $3 $4)
          (eprintf "warning: exit-prim and return differ:\n~s\n~s\n" $3 $4))
        ($2 $1 $3 rs))]
     [((? MacroStep) (? EE))
      ($1 e1 rs $2)])

    (MacroStep
     (#:args e1 rs next)
     [(enter-macro ! macro-pre-x (? LocalActions) macro-post-x ! exit-macro)
      (let ([e2 (and next (wderiv-e2 next))])
        (make mrule e1 e2 rs $2
              $3 $4 (and $5 (car $5)) $6 $7 next))])

    ;; Keyword resolution
    (Resolves
     [() null]
     [(resolve Resolves) (cons $1 $2)])

    ;; Local actions taken by macro
    ;; LocalAction Answer = (list-of LocalAction)
    (LocalActions
     (#:skipped null)
     [() null]
     [((? LocalAction) (? LocalActions)) (cons $1 $2)])

    (LocalAction
     [(!!) (make local-exn $1)]
     [(enter-local OptPhaseUp
       local-pre (? LocalExpand/Inner) OptLifted local-post
       OptOpaqueExpr exit-local)
      (make local-expansion $1 $8 $2 $3 $4 $5 $6 $7)]
     [(lift-expr)
      (make local-lift (cdr $1) (car $1))]
     [(lift-statement)
      (make local-lift-end $1)]
     [(lift-require)
      (make local-lift-require (car $1) (cadr $1) (cddr $1))]
     [(lift-provide)
      (make local-lift-provide $1)]
     [(local-bind ! rename-list exit-local-bind)
      (make local-bind $1 $2 $3 #f)]
     [(local-bind rename-list (? BindSyntaxes) exit-local-bind)
      (make local-bind $1 #f $2 $3)]
     [(track-origin)
      (make track-origin (car $1) (cdr $1))]
     [(local-value ! Resolves local-value-result local-value-binding)
      (make local-value $1 $2 $3 $4 $5)]
     [(local-remark)
      (make local-remark $1)]
     [(local-artificial-step)
      (let ([ids (list-ref $1 0)]
            [before (list-ref $1 1)]
            [mbefore (list-ref $1 2)]
            [mafter (list-ref $1 3)]
            [after (list-ref $1 4)])
        (make local-expansion
          before after #f mbefore
          (make mrule mbefore mafter ids #f
                before null after #f mafter
                (make p:stop mafter mafter null #f))
          #f after #f))]
     [(local-mess)
      ;; Represents subsequence of event stream incoherent due to
      ;; jump (eg, macro catches exn raised from within local-expand).
      (make local-mess $1)])

    (LocalExpand/Inner
     [(start (? EE)) $2]
     [((? CheckImmediateMacro)) $1])

    (OptLifted
     [(lift-loop) $1]
     [() #f])
    (OptOpaqueExpr
     [(opaque-expr) $1]
     [() #f])
    (OptPhaseUp
     [(phase-up) #t]
     [() #f])

    (Prim
     (#:args e1 e2 rs)
     [((? PrimModule)) ($1 e1 e2 rs)]
     [((? Prim#%ModuleBegin)) ($1 e1 e2 rs)]
     [((? PrimDefineSyntaxes)) ($1 e1 e2 rs)]
     [((? PrimDefineValues)) ($1 e1 e2 rs)]
     [((? PrimExpression)) ($1 e1 e2 rs)]
     [((? Prim#%App)) ($1 e1 e2 rs)]
     [((? Prim#%Datum)) ($1 e1 e2 rs)]
     [((? Prim#%Top)) ($1 e1 e2 rs)]
     [((? PrimIf)) ($1 e1 e2 rs)]
     [((? PrimWCM)) ($1 e1 e2 rs)]
     [((? PrimSet)) ($1 e1 e2 rs)]
     [((? PrimBegin)) ($1 e1 e2 rs)]
     [((? PrimBegin0)) ($1 e1 e2 rs)]
     [((? PrimLambda)) ($1 e1 e2 rs)]
     [((? PrimCaseLambda)) ($1 e1 e2 rs)]
     [((? PrimLetValues)) ($1 e1 e2 rs)]
     [((? PrimLetrecValues)) ($1 e1 e2 rs)]
     [((? PrimLetrecSyntaxes+Values)) ($1 e1 e2 rs)]
     [((? PrimSTOP)) ($1 e1 e2 rs)]
     [((? PrimQuote)) ($1 e1 e2 rs)]
     [((? PrimQuoteSyntax)) ($1 e1 e2 rs)]
     [((? PrimRequire)) ($1 e1 e2 rs)]
     [((? PrimProvide)) ($1 e1 e2 rs)]
     [((? PrimVarRef)) ($1 e1 e2 rs)]
     [((? PrimStratifiedBody)) ($1 e1 e2 rs)]
     [((? PrimBeginForSyntax)) ($1 e1 e2 rs)])

    (PrimModule
     (#:args e1 e2 rs)
     [(prim-module ! (? PrepareEnv) OptTag rename-one
                   (? OptCheckImmediateMacro) (? OptTagAndCheckImmediateMacro) !
                   (? EE) rename-one)
      (make p:module e1 e2 rs $2 $3 $4 $5 $6 (and $7 (car $7)) (and $7 (cadr $7)) $8 $9 $10)])
    (OptTag
     [() #f]
     [(tag) $1])
    (OptCheckImmediateMacro
     [() #f]
     [((? CheckImmediateMacro)) $1])
    (OptTagAndCheckImmediateMacro
     [() (list #f #f)]
     [(tag) (list $1 #f)]
     [(tag (? CheckImmediateMacro)) (list $1 $2)])

    ;; FIXME: workaround for problem in expander instrumentation:
    ;;   observer not propagated correctly to expand_all_provides
    ;;   so local actions that should be within prim-provide's EE 
    ;;   instead appear directly here
    (Prim#%ModuleBegin
     (#:args e1 e2 rs)
     [(prim-module-begin ! rename-one (? ModuleBegin/Phase) (? Eval) next (? ExpandSubmodules))
      (make p:#%module-begin e1 e2 rs $2 $3 $4
            (for/or ([la (in-list $5)])
              (and (local-exn? la) (local-exn-exn la)))
            $7)])
    #|
    ;; restore this version when expander fixed
    (Prim#%ModuleBegin-REAL
     (#:args e1 e2 rs)
     [(prim-#%module-begin ! rename-one (? ModuleBegin/Phase) ! (? ExpandSubmodules))
      (make p:#%module-begin e1 e2 rs $2 $3 $4 $5)])
    |#
    (ExpandSubmodules
     (#:skipped null)
     [(enter-prim (? PrimModule) exit-prim (? ExpandSubmodules))
      (cons ($2 $1 $3 null) $4)]
     [() null])

    (ModuleBegin/Phase
     [((? ModulePass1) next-group (? ModulePass2) next-group (? ModulePass3))
      (make module-begin/phase $1 $3 $5)])

    (ModulePass1
     (#:skipped null)
     [() null]
     [(next (? ModulePass1-Part) (? ModulePass1))
      (cons $2 $3)]
     [(module-lift-end-loop (? ModulePass1))
      (cons (make mod:lift-end $1) $2)])

    (ModulePass1-Part
     [((? EE) rename-one (? ModulePass1/Prim))
      (make mod:prim $1 $2 ($3 $2))]
     [(EE rename-one ! splice)
      (make mod:splice $1 $2 $3 $4)]
     [(EE rename-list module-lift-loop)
      (make mod:lift $1 null $2 $3)])

    (ModulePass1/Prim
     (#:args e1)
     [(enter-prim prim-define-values ! exit-prim)
      (make p:define-values $1 $4 null $3 #f)]
     [(enter-prim prim-define-syntaxes ! (? PrepareEnv)
                  phase-up (? EE/LetLifts) (? Eval) exit-prim)
      (make p:define-syntaxes $1 $8 null $3 $4 $6 $7)]
     [(enter-prim prim-begin-for-syntax ! (? PrepareEnv)
                  phase-up (? ModuleBegin/Phase) (? Eval) exit-prim)
      (make p:begin-for-syntax $1 $7 null $3 $4 $6 $7)]
     [(enter-prim prim-require (? Eval) exit-prim)
      (make p:require $1 $4 null #f $3)]
     [(enter-prim prim-submodule ! (? ExpandSubmodules #|one|#) exit-prim)
      (make p:submodule $1 $5 null $3 (car $4))]
     [(enter-prim prim-submodule* ! exit-prim)
      (make p:submodule* $1 $4 null $3)]
     [()
      (make p:stop e1 e1 null #f)])

    (ModulePass2
     (#:skipped null)
     [() null]
     [(next (? ModulePass2-Part) (? ModulePass2))
      (cons $2 $3)]
     [(module-lift-end-loop (? ModulePass2))
      (cons (make mod:lift-end $1) $2)])

    (ModulePass2-Part
     ;; not normal; already handled
     [()
      (make mod:skip)]
     ;; normal: expand completely
     [((? EE) (? Eval))
      ;; after expansion, may compile => may eval letstx rhss again!
      ;; need to include those evals too (for errors, etc)
      (make mod:cons $1 $2)]
     ;; catch lifts
     [(EE Eval module-lift-loop)
      ;; same as above: after expansion, may compile => may eval
      (make mod:lift $1 $2 #f $3)])

    (ModulePass3
     (#:skipped null)
     [() null]
     [((? ModulePass3-Part) (? ModulePass3))
      (cons $1 $2)])

    (ModulePass3-Part
     [(enter-prim prim-provide (? ModuleProvide/Inner) ! exit-prim)
      (make p:provide $1 $5 null #f $3 $4)])

    (ModuleProvide/Inner
     (#:skipped null)
     [() null]
     [((? EE) (? ModuleProvide/Inner))
      (cons $1 $2)])

    ;; Definitions
    (PrimDefineSyntaxes
     (#:args e1 e2 rs)
     [(prim-define-syntaxes ! (? PrepareEnv) (? EE/LetLifts) (? Eval))
      (make p:define-syntaxes e1 e2 rs $2 $3 $4 $5)])

    (PrimDefineValues
     (#:args e1 e2 rs)
     [(prim-define-values ! (? EE))
      (make p:define-values e1 e2 rs $2 $3)])

    ;; Simple expressions
    (PrimExpression
     (#:args e1 e2 rs)
     [(prim-#%expression ! (? EE))
      (make p:#%expression e1 e2 rs $2 $3 #f)]
     [(prim-#%expression EE tag)
      (make p:#%expression e1 e2 rs #f $2 $3)])

    (PrimIf
     (#:args e1 e2 rs)
     [(prim-if ! (? EE) next (? EE) next (? EE))
      (make p:if e1 e2 rs $2 $3 $5 $7)])

    (PrimWCM 
     (#:args e1 e2 rs)
     [(prim-with-continuation-mark ! (? EE) next (? EE) next (? EE))
      (make p:wcm e1 e2 rs $2 $3 $5 $7)])

    ;; Sequence-containing expressions
    (PrimBegin
     (#:args e1 e2 rs)
     [(prim-begin ! (? EL))
      (make p:begin e1 e2 rs $2 $3)])

    (PrimBegin0
     (#:args e1 e2 rs)
     [(prim-begin0 ! next (? EE) next (? EL))
      (make p:begin0 e1 e2 rs $2 $4 $6)])

    (Prim#%App
     (#:args e1 e2 rs)
     [(prim-#%app !)
      (make p:#%app e1 e2 rs $2 #f)]
     [(prim-#%app (? EL))
      (make p:#%app e1 e2 rs #f $2)])

    ;; Binding expressions
    (PrimLambda
     (#:args e1 e2 rs)
     [(prim-lambda ! lambda-renames (? EB))
      (make p:lambda e1 e2 rs $2 $3 $4)])

    (PrimCaseLambda
     (#:args e1 e2 rs)
     [(prim-case-lambda ! (? NextCaseLambdaClauses))
      (make p:case-lambda e1 e2 rs $2 $3)])

    (NextCaseLambdaClauses
     (#:skipped null)
     [(next (? CaseLambdaClause) (? NextCaseLambdaClauses))
      (cons $2 $3)]
     [() null])

    (CaseLambdaClause
     [(! lambda-renames (? EB))
      (make clc $1 $2 $3)])

    (PrimLetValues
     (#:args e1 e2 rs)
     [(prim-let-values ! let-renames (? NextEEs) next-group (? EB/EL))
      (make p:let-values e1 e2 rs $2 $3 $4 $6)])

    (PrimLetrecValues
     (#:args e1 e2 rs)
     [(prim-letrec-values ! let-renames (? NextEEs) next-group (? EB/EL))
      (make p:letrec-values e1 e2 rs $2 $3 $4 $6)])

    (PrimLetrecSyntaxes+Values
     (#:args e1 e2 rs)
     [(prim-letrec-syntaxes+values ! letrec-syntaxes-renames
       (? PrepareEnv) (? NextBindSyntaxess) next-group (? EB/EL) OptTag)
      (make p:letrec-syntaxes+values e1 e2 rs $2 $3 $4 $5 #f null $7 $8)]
     [(prim-letrec-syntaxes+values letrec-syntaxes-renames
       PrepareEnv NextBindSyntaxess next-group
       prim-letrec-values
       let-renames (? NextEEs) next-group (? EB/EL) OptTag)
      (make p:letrec-syntaxes+values e1 e2 rs #f $2 $3 $4 $7 $8 $10 $11)])

    ;; Atomic expressions
    (Prim#%Datum
     (#:args e1 e2 rs)
     [(prim-#%datum !) (make p:#%datum e1 e2 rs $2)])

    (Prim#%Top
     (#:args e1 e2 rs)
     [(prim-#%top !) (make p:#%top e1 e2 rs $2)])

    (PrimSTOP
     (#:args e1 e2 rs)
     [(prim-stop !) (make p:stop e1 e2 rs $2)])

    (PrimQuote
     (#:args e1 e2 rs)
     [(prim-quote !) (make p:quote e1 e2 rs $2)])

    (PrimQuoteSyntax
     (#:args e1 e2 rs)
     [(prim-quote-syntax !) (make p:quote-syntax e1 e2 rs $2)])

    (PrimRequire
     (#:args e1 e2 rs)
     [(prim-require (? Eval))
      (make p:require e1 e2 rs #f $2)])

    (PrimProvide 
     (#:args e1 e2 rs)
     [(prim-provide !) (make p:provide e1 e2 rs $2 null #f)])

    (PrimVarRef
     (#:args e1 e2 rs)
     [(prim-#%variable-reference !)
      (make p:#%variable-reference e1 e2 rs $2)])

    (PrimStratifiedBody
     (#:args e1 e2 rs)
     [(prim-#%stratified ! (? EB)) (make p:#%stratified-body e1 e2 rs $2 $3)])

    (PrimBeginForSyntax
     (#:args e1 e2 rs)
     [(prim-begin-for-syntax ! (? PrepareEnv) (? BeginForSyntax*) (? Eval))
      (make p:begin-for-syntax e1 e2 rs $2 $3 $4 $5)])
    (BeginForSyntax*
     [((? EL))
      (list $1)]
     [(EL module-lift-loop (? BeginForSyntax*))
      (cons (make bfs:lift $1 $2) $3)])

    (PrimSet
     (#:args e1 e2 rs)
     ;; Unrolled to avoid shift/reduce
     [(prim-set! ! resolve Resolves ! next (? EE))
      (make p:set! e1 e2 rs $2 (cons $3 $4) $5 $7)]
     [(prim-set! Resolves (? MacroStep) (? EE))
      (make p:set!-macro e1 e2 rs #f ($3 e1 $2 $4))])
    
    ;; When an internal-definition context expands to `let`, `letrec`, etc.,
    ;; then the body is processed as a list (since it has already been
    ;; processed as a block)
    (EB/EL
     [((? EB)) $1]
     [((? EL)) $1])

    ;; Blocks
    ;; EB Answer = BlockDerivation
    (EB
     [(enter-block block-renames (? BlockPass1) block->list (? EL))
      (make bderiv $1 (and $5 (wlderiv-es2 $5))
            $2 $3 'list $5)]
     [(enter-block block-renames BlockPass1 block->letrec (? EE))
      (make bderiv $1 (and $5 (list (wderiv-e2 $5)))
            $2 $3 'letrec $5)])

    ;; BlockPass1 Answer = (list-of BRule)
    (BlockPass1
     (#:skipped null)
     [() null]
     [((? BRule) (? BlockPass1))
      (cons $1 $2)])

    ;; BRule Answer = BRule
    (BRule
     [(next !!)
      (make b:error $2)]
     [(next (? CheckImmediateMacro))
      (make b:expr $2)]
     [(next CheckImmediateMacro prim-begin ! splice !)
      (make b:splice $2 $4 $5 $6)]
     [(next CheckImmediateMacro prim-define-values ! rename-one !)
      (make b:defvals $2 $4 $5 $6)]
     [(next CheckImmediateMacro
            prim-define-syntaxes ! rename-one ! (? PrepareEnv) (? BindSyntaxes))
      (make b:defstx $2 $4 $5 $6 $7 $8)])

    ;; BindSyntaxes Answer = Derivation
    (BindSyntaxes
     [(enter-bind (? EE/LetLifts) next (? Eval) exit-bind)
      (make bind-syntaxes $2 $4)])

    ;; NextBindSyntaxess Answer = (list-of Derivation)
    (NextBindSyntaxess
     (#:skipped null)
     [() null]
     [(next (? BindSyntaxes) (? NextBindSyntaxess)) (cons $2 $3)])

    ;; Lists
    ;; EL Answer = ListDerivation
    (EL
     (#:skipped #f)
     [(enter-list ! (? EL*) exit-list)
      ;; FIXME: Workaround for bug in events
      (if (null? $3)
          (make lderiv null null $2 $3)
          (make lderiv $1 $4 $2 $3))])

    ;; EL* Answer = (listof Derivation)
    (EL*
     (#:skipped null)
     [() null]
     [(next (? EE) (? EL*)) (cons $2 $3)])

    )))

(define (derivs->lderiv es1 ds)
  (define es2 (map node-z2 ds))
  (lderiv (stx->list es1) (and es2 (andmap values es2)) #f ds))
