#lang racket/base
(require (for-syntax racket/base)
         racket/match
         syntax/stx
         "stx-util.rkt"
         "parser-util.rkt"
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

(define (car/f x) (and x (car x)))
(define (cdr/f x) (and x (cdr x)))

(use-tokens! macro-expansion-tokens)

;; ----------------------------------------
;; Entry point

(define-nt Expansion
  [(start-ecte ExpandCTE) $2]
  [(start-ecte ExpandCTE/Interrupted) $2]
  [(MainExpand) $1]
  [(MainExpand/Interrupted) $1])

(define-nts*
  ;; ----------------------------------------
  ;; ./trace.rkt expand/compile-time-evals

  (ExpandCTE
   ;; The first 'Eval' is there for---I believe---lazy phase 1 initialization.
   [(visit ?MainExpandToTop top-non-begin ?MainExpand ?Eval return)
    (make ecte $1 $6 null $2 $4 $5)]
   [(visit MainExpandToTop top-begin ?NextExpandCTEs return)
    (make ecte $1 $5 null $2
          (let ([b-e1 $3] [b-e2 $5])
            (make p:begin b-e1 b-e2 (list (stx-car b-e1)) #f #f $4))
          null)])

  (NextExpandCTEs
   #:skipped null
   [() null]
   [(next ?ExpandCTE ?NextExpandCTEs) (cons $2 $3)])

  ;; ----------------------------------------
  ;; src/eval/main.rkt expand and expand-to-top-form

  (MainExpand
   [(start-top ?PTLL) $2])

  (MainExpandToTop
   [(start-top ?PTLL) $2])

  (PTLL ;; per-top-level loop
   [(visit ?ECL return)
    $2]
   [(visit ECL ?ExpandSingle)
    (let ([e2 (and $3 (node-z2 $3))])
      (make ecte $1 e2 null $2 $3 null))]
   [(visit ECL lift-loop ?PTLL)
    (make lift-deriv $1 (wderiv-e2 $4) $2 $3 $4)]
   [([e1 visit] ECL prim-begin [?1 !] [body ?NextPTLLs] [e2 return])
    (make ecte e1 e2 null $2 (make p:begin (wderiv-e2 $2) e2 null $3 ?1 body) null)]
   [([e1 visit] ECL prim-begin-for-syntax [?1 !] [prep ?PrepareEnv] [body ?NextPTLLs] [e2 return])
    (make ecte e1 e2 null $2
          (make p:begin-for-syntax (wderiv-e2 $2) e2 null $3 ?1  prep body null)
          null)])

  (NextPTLLs
   #:skipped null
   [() null]
   [(next ?PTLL ?NextPTLLs) (cons $2 $3)])

  (ECL ;; expand-capturing-lifts
   [(?EE) $1])

  (ExpandSingle
   [(?ECL) $1]
   [(ECL lift-loop ?ExpandSingle)
    (make lift-deriv (wderiv-e1 $1) (wderiv-e2 $3) $1 $2 $3)])

  ;; ----------------------------------------
  ;; EE = src/expander/expand/main.rkt expand

  ;; expand/capture-lifts from src/expander/expand/main.rkt
  ;; Expand, convert lifts to let (rhs of define-syntaxes, mostly)
  (EE/LetLifts
   [(?EE) $1]
   [(EE letlift-loop ?EE/LetLifts)
    (let ([initial (wderiv-e1 $1)]
          [final (wderiv-e2 $3)])
      (make lift/let-deriv initial final $1 $2 $3))])

  ;; Evaluation
  ;; Answer = (listof LocalAction)
  (Eval
   #:skipped null
   [(?LocalActions) $1])

  ;; Prepare env for compilation
  (PrepareEnv
   [(prepare-env ?Eval) $2])

  ;; Expansion of multiple expressions, next-separated
  (NextEEs
   #:skipped null
   [() null]
   [(next ?EE ?NextEEs) (cons $2 $3)])

  ;; EE

  ;; Expand expression (term)
  (EE
   [(visit Resolves ?EE/k)
    ($3 $1 $2)])

  (EE/k
   #:args (e1 rs)
   [(!!)
    (make p:unknown e1 #f rs #f $1)]
   [(stop/return)
    (make p:stop e1 $1 rs #f #f)]
   [(variable return)
    (make p:variable e1 $2 rs #f #f)]
   [(tag2 ?EE/k)
    (match $1
      [(list* tagged-stx disarmed-untagged-stx)
       (define next ($2 tagged-stx rs))
       (make tagrule e1 (wderiv-e2 next) disarmed-untagged-stx tagged-stx next)])]
   [(tag/context ?EE)
    (make tagrule e1 (wderiv-e2 $2) e1 $1 $2)]
   [(opaque-expr)
    (make p:opaque e1 $1 rs #f #f)]
   [(enter-prim ?Prim exit-prim/return)
    ($2 $1 $3 rs)]
   [(rename-transformer visit Resolves ?EE/k)
    ($4 e1 (append rs (list $1)))]
   [(?MacroStep ?EE)
    ($1 e1 rs $2)])

  (MacroStep
   #:args (e1 rs next)
   [(enter-macro ! macro-pre-x ?LocalActions macro-post-x ! exit-macro)
    (let ([e2 (and next (wderiv-e2 next))])
      (make mrule e1 e2 rs (car $1) $2 $3 $4 (car/f $5) $6 (cdr/f $7) (car/f $7) next))])

  ;; Keyword resolution
  (Resolves
   [() null]
   [(resolve Resolves) (cons $1 $2)])

  ;; Local actions taken by macro
  ;; LocalAction Answer = (list-of LocalAction)
  (LocalActions
   #:skipped null
   [() null]
   [(?LocalAction ?LocalActions) (cons $1 $2)])

  (LocalAction
   [(!!) (make local-exn $1)]
   [(enter-local OptPhaseUp
     local-pre ?LocalExpand/Inner OptLifted local-post
     OptOpaqueExpr exit-local)
    (make local-expansion $1 $8 $2 $3 $4 $5 $6 $7)]
   [(lift-expr)
    (match $1 [(list* ids orig-expr ren-expr) (make local-lift-expr orig-expr ren-expr ids)])]
   [(lift-end-decl)
    (match $1 [(list* orig renamed wrapped) (make local-lift-end orig renamed wrapped)])]
   [(lift-require)
    (match $1
      [(list* wrapped-req orig-use renamed-use)
       (make local-lift-require wrapped-req orig-use renamed-use)])]
   [(lift-provide)
    (make local-lift-provide $1)]
   [(lift-module)
    (match $1 [(list* orig renamed) (make local-lift-module orig renamed)])]
   [(local-bind ! rename-list exit-local-bind)
    (make local-bind $1 $2 $3 #f)]
   [(local-bind rename-list ?BindSyntaxes exit-local-bind)
    (make local-bind $1 #f $2 $3)]
   [(track-syntax)
    (match $1
      [(list* operation new-stx old-stx)
       (make track-syntax operation new-stx old-stx)])]
   [(local-value Resolves ! local-value-result local-value-binding)
    ;; FIXME: (... ! Resolves ! ...) represents possibilities better,
    ;; but that would be ambiguous and require structure change.
    (make local-value $1 $3 $2 $4 $5)]
   [(local-remark)
    (make local-remark $1)]
   [(local-artificial-step)
    (match $1
      [(list ids before mbefore mafter after)
       (make local-expansion
             before after #f mbefore
             (make mrule mbefore mafter ids #f #f ;; FIXME: disarmed
                   before null after #f mafter mafter
                   (make p:stop mafter mafter null #f #f))
             #f after #f)])]
   [(local-mess)
    ;; Represents subsequence of event stream incoherent due to
    ;; jump (eg, macro catches exn raised from within local-expand).
    (make local-mess $1)])

  (LocalExpand/Inner
   [(start ?EE) $2]
   [(?EE) $1])

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
   #:args (e1 e2 rs)
   [(?PrimModule) ($1 e1 e2 rs)]
   [(?Prim#%ModuleBegin) ($1 e1 e2 rs)]
   [(?PrimDefineSyntaxes) ($1 e1 e2 rs)]
   [(?PrimDefineValues) ($1 e1 e2 rs)]
   [(?PrimExpression) ($1 e1 e2 rs)]
   [(?Prim#%App) ($1 e1 e2 rs)]
   [(?Prim#%Datum) ($1 e1 e2 rs)]
   [(?Prim#%Top) ($1 e1 e2 rs)]
   [(?PrimIf) ($1 e1 e2 rs)]
   [(?PrimWCM) ($1 e1 e2 rs)]
   [(?PrimSet) ($1 e1 e2 rs)]
   [(?PrimBegin) ($1 e1 e2 rs)]
   [(?PrimBegin0) ($1 e1 e2 rs)]
   [(?PrimLambda) ($1 e1 e2 rs)]
   [(?PrimCaseLambda) ($1 e1 e2 rs)]
   [(?PrimLetValues) ($1 e1 e2 rs)]
   [(?PrimLetrecValues) ($1 e1 e2 rs)]
   [(?PrimLetrecSyntaxes+Values) ($1 e1 e2 rs)]
   [(?PrimSTOP) ($1 e1 e2 rs)]
   [(?PrimQuote) ($1 e1 e2 rs)]
   [(?PrimQuoteSyntax) ($1 e1 e2 rs)]
   [(?PrimRequire) ($1 e1 e2 rs)]
   [(?PrimProvide) ($1 e1 e2 rs)]
   [(?PrimVarRef) ($1 e1 e2 rs)]
   [(?PrimStratifiedBody) ($1 e1 e2 rs)]
   [(?PrimBeginForSyntax) ($1 e1 e2 rs)])

  ;; ----------------------------------------
  ;; src/expander/expand/module.rkt

  (PrimModule
   #:args (e1 e2 rs)
   [(?ExpandModule)
    ($1 e1 e2 rs)])

  (ExpandModule
   #:args (e1 e2 rs)
   [(prim-module ! ?PrepareEnv rename-one ?EnsureModuleBegin next ?EE rename-one)
    (make p:module e1 e2 rs $1 $2 $3 $4 $5 $7 $8)])

  (EnsureModuleBegin
   [(track-syntax)
    (mod:ensure-mb #f #f  #f  $1)]
   [(track-syntax ?EE track-syntax)
    (mod:ensure-mb $1 $2  #f  $3)]
   [(track-syntax EE ?AddModuleBegin track-syntax)
    (mod:ensure-mb $1 $2 $3 $4)]
   [(?AddModuleBegin track-syntax)
    (mod:ensure-mb #f #f $1 $2)])

  (AddModuleBegin
   [(! tag track-syntax ?EE !)
    (mod:add-mb $1 $2 $3 $4 $5)])

  ;; --------------------

  (Prim#%ModuleBegin
   #:args (e1 e2 rs)
   [(prim-module-begin ! ?ModuleBeginK)
    ($3 e1 e2 rs $1 $2)])

  (ModuleBeginK
   #:args (e1 e2 rs de1 ?1)
   [([me rename-one] [p12 ?Pass1And2Loop] next-group [?2 !]
     next-group [p3 ?ModulePass3] [?3 !] next-group [p4 ?ModulePass4])
    (make p:#%module-begin e1 e2 rs de1 ?1 me p12 ?2 p3 ?3 p4)])

  (Pass1And2Loop
   ;; ModPass1And2
   [(?ModulePass1 next-group ?ModulePass2)
    (mod:pass-1-and-2 $1 $3)])

  (ModulePass1  ;; partially-expand-bodys loop
   #:skipped null
   [() null]
   [(module-end-lifts ?ModulePass1)
    (cons (make mod:lift-end $1) $2)]
   [(next ?ModulePass1-Head ?ModulePass1Case ?ModulePass1)
    (cons (make modp1:prim $2 $3) $4)])

  (ModulePass1-Head
   [(?EE) $1]
   [(EE module-pass1-lifts ?ModulePass1)
    (match $2
      [(list* lifted-defns lifted-reqs lifted-mods)
       (make modp1:lift $1 lifted-defns lifted-reqs lifted-mods $3)])])

  (ModulePass1Case
   [(module-pass1-case ?ModulePass1CaseBody)
    ($2 $1)])

  (ModulePass1CaseBody
   #:args (e1)
   [(prim-begin ! splice)
    (make modp1:splice $1 $2 $3)] ;; !!
   [(prim-begin-for-syntax [?1 !] [prep ?PrepareEnv] phase-up [p12 ?Pass1And2Loop]
                           next-group [ev ?Eval] [e2 exit-case])
    (make p:begin-for-syntax e1 #f null $1 ?1 prep p12 ev)]
   [(prim-define-values ! exit-case)
    (make p:define-values e1 $3 null $1 $2 #f)]
   [(prim-define-syntaxes ! ?PrepareEnv phase-up ?EE/LetLifts ?Eval exit-case)
    (make p:define-syntaxes e1 $7 null $1 $2 $3 $5 $6)]
   [(prim-require ?Eval exit-case)
    (make p:require e1 $3 null $1 #f $2)]
   ;; provide : stop
   [(prim-submodule ?ExpandSubmodule)
    $2]
   ;; module* : stop
   [(prim-declare !)
    (make p:declare e1 e1 null $1 $2)]
   [(prim-stop)
    (make p:stop e1 e1 null $1 #f)])

  (ModulePass2 ;; finish-expanding-body-expressions
   #:skipped null
   [() null]
   [(module-end-lifts ?ModulePass2)
    (cons (make mod:lift-end $1) $2)]
   [(next ?ModulePass2-Part ?ModulePass2)
    (cons $2 $3)])

  (ModulePass2-Part
   ;; already handled
   [()
    (make modp2:skip)]
   [(?EE ?Eval)
    ;; after expansion, may compile => may eval letstx rhss again!
    ;; need to include those evals too (for errors, etc)
    ;; FIXME: Is this still true? Is the Eval really needed?
    (make modp2:cons $1 $2)]
   [(EE Eval module-pass2-lifts
        [emods ?ExpandNonModule*Submodules] next-group [edefs ?ModulePass2] next-group)
    (match $3
      [(list* lifted-reqs lifted-mods lifted-defns)
       (make modp2:lift $1 $2 lifted-reqs lifted-mods lifted-defns emods edefs)])])

  (ExpandNonModule*Submodules
   #:skipped null
   [() null]
   [(next ?ExpandNonModule*Submodules)
    (cons #f $2)]
   [(next ?ExpandSubmodule ?ExpandNonModule*Submodules)
    (cons $2 $3)])

  (ModulePass3 ;; resolve-provides
   ;; RPs = (Listof (U #f RPs p:provide))
   #:skipped null
   [() null]
   [(next ?ModulePass3) (cons #f $2)]
   [(?ModulePass3/BFS ?ModulePass3)
    (cons $1 $2)]
   [(?ModulePass3/Provide ?ModulePass3)
    (cons $1 $2)])

  (ModulePass3/BFS
   [(enter-begin-for-syntax ?ModulePass3 exit-begin-for-syntax)
    (modp34:bfs $2)])

  (ModulePass3/Provide
   ;; p:provide
   [([e1 enter-prim] [de1 prim-provide] [ds ParseAndExpandProvides] [?2 !] [e2 exit-prim])
    (make p:provide e1 e2 null de1 #f ds ?2)])

  (ParseAndExpandProvides ;; in src/expander/expand/provide.rkt
   #:skipped null
   [() null]
   [(?EE ?ParseAndExpandProvides) (cons $1 $2)])

  (ModulePass4 ;; expand-post-submodules loop
   ;; Pass4 = (Listof (U #f Pass4 p:module))
   #:skipped null
   [() null]
   [(next ?ModulePass4)
    (cons #f $2)]
   [(?ModulePass4/BFS ?ModulePass4)
    (cons $1 $2)]
   [(?ExpandSubmodule ?ModulePass4)
    (cons $1 $2)])

  (ModulePass4/BFS
   [(enter-begin-for-syntax ?ModulePass4 exit-begin-for-syntax)
    (modp34:bfs $2)])

  (ExpandSubmodule
   ;; Deriv
   [(enter-prim prim-submodule [?1 !] [e1 enter-prim] [m ?ExpandModule] [e2 exit-prim] [ev ?Eval])
    (let ([mod (m e1 e2 null)])
      (p:submodule $1 (and mod (wderiv-e2 mod)) null $2 ?1 mod ev))]
   [(enter-prim prim-submodule* [?1 !] [e1 enter-prim] [m ?ExpandModule] [e2 exit-prim] [ev ?Eval])
    (let ([mod (m e1 e2 null)])
      (p:submodule* $1 (and mod (wderiv-e2 mod)) null $2 ?1 mod ev))])


  ;; ----------------------------------------
  ;; src/expander/expand/top.rkt

  ;; Definitions
  (PrimDefineSyntaxes
   #:args (e1 e2 rs)
   [(prim-define-syntaxes ! ?PrepareEnv ?EE/LetLifts ?Eval)
    (make p:define-syntaxes e1 e2 rs $1 $2 $3 $4 $5)])

  (PrimDefineValues
   #:args (e1 e2 rs)
   [(prim-define-values ! ?EE)
    (make p:define-values e1 e2 rs $1 $2 $3)])

  (PrimBeginForSyntax
   #:args (e1 e2 rs)
   [(prim-begin-for-syntax ! ?PrepareEnv ?BeginForSyntax* ?Eval)
    (make p:begin-for-syntax e1 e2 rs $1 $2 $3 $4 $5)])
  (BeginForSyntax*
   #:skipped null
   [(?EL)
    (list $1)]
   [(EL module-lift-loop ?BeginForSyntax*)
    (cons (make bfs:lift $1 $2) $3)])

  (PrimRequire
   #:args (e1 e2 rs)
   [(prim-require ?Eval)
    (make p:require e1 e2 rs $1 #f $2)])

  (PrimProvide 
   #:args (e1 e2 rs)
   [(prim-provide !) (make p:provide e1 e2 rs $1 $2 null #f)])

  ;; ----------------------------------------
  ;; src/expander/expand/expr.rkt

  ;; Simple expressions
  (PrimExpression
   #:args (e1 e2 rs)
   [(prim-#%expression ! ?EE)
    (make p:#%expression e1 e2 rs $1 $2 $3 #f)]
   [(prim-#%expression EE tag)
    (make p:#%expression e1 e2 rs $1 #f $2 $3)])

  (PrimIf
   #:args (e1 e2 rs)
   [(prim-if ! ?EE next ?EE next ?EE)
    (make p:if e1 e2 rs $1 $2 $3 $5 $7)])

  (PrimWCM 
   #:args (e1 e2 rs)
   [(prim-with-continuation-mark ! ?EE next ?EE next ?EE)
    (make p:wcm e1 e2 rs $1 $2 $3 $5 $7)])

  ;; Sequence-containing expressions
  (PrimBegin
   #:args (e1 e2 rs)
   [(prim-begin ! ?NextEEs)
    (make p:begin e1 e2 rs $1 $2 $3)])

  (PrimBegin0
   #:args (e1 e2 rs)
   [(prim-begin0 ! ?NextEEs)
    (make p:begin0 e1 e2 rs $1 $2 $3)])

  (Prim#%App
   #:args (e1 e2 rs)
   [(prim-#%app ! ?NextEEs)
    (make p:#%app e1 e2 rs $1 $2 $3)])

  ;; Binding expressions
  (PrimLambda
   #:args (e1 e2 rs)
   [(prim-lambda ! lambda-renames ?EB)
    (make p:lambda e1 e2 rs $1 $2 $3 $4)])

  (PrimCaseLambda
   #:args (e1 e2 rs)
   [(prim-case-lambda ! ?NextCaseLambdaClauses)
    (make p:case-lambda e1 e2 rs $1 $2 $3)])

  (NextCaseLambdaClauses
   #:skipped null
   [(next ?CaseLambdaClause ?NextCaseLambdaClauses)
    (cons $2 $3)]
   [() null])

  (CaseLambdaClause
   [(! lambda-renames ?EB)
    (make clc $1 $2 $3)])

  (PrimLetValues
   #:args (e1 e2 rs)
   [(prim-let-values ! letX-renames ?NextEEs ?EB)
    (let ([rename (and $3 (match $3
                            [(list* _ _ val-idss val-rhss bodys)
                             (cons (map list val-idss val-rhss) bodys)]))])
      (make p:let-values e1 e2 rs $1 $2 rename $4 $5))])

  (PrimLetrecValues
   #:args (e1 e2 rs)
   [(prim-letrec-values ! letX-renames ?NextEEs ?EB)
    (let ([rename (and $3 (match $3
                            [(list* _ _ val-idss val-rhss bodys)
                             (cons (map list val-idss val-rhss) bodys)]))])
      (make p:letrec-values e1 e2 rs $1 $2 rename $4 $5))])

  (PrimLetrecSyntaxes+Values
   #:args (e1 e2 rs)
   [(prim-letrec-syntaxes+values
     ! letX-renames
     ?PrepareEnv ?NextBindSyntaxess next-group
     ?ESBBRLoop ?EB)
    (let ([rename (and $3 (match $3
                            [(list* stx-idss stx-rhss val-idss val-rhss bodys)
                             (list* (map list stx-idss stx-rhss)
                                    (map list val-idss val-rhss)
                                    bodys)]))])
      (make p:letrec-syntaxes+values e1 e2 rs $1 $2 rename $4 $5 $7 $8))])

  ;; Atomic expressions
  (Prim#%Datum
   #:args (e1 e2 rs)
   [(prim-#%datum !) (make p:#%datum e1 e2 rs $1 $2)])

  (Prim#%Top
   #:args (e1 e2 rs)
   [(prim-#%top !) (make p:#%top e1 e2 rs $1 $2)])

  (PrimSTOP
   #:args (e1 e2 rs)
   [(prim-stop !) (make p:stop e1 e2 rs $1 $2)])

  (PrimQuote
   #:args (e1 e2 rs)
   [(prim-quote !) (make p:quote e1 e2 rs $1 $2)])

  (PrimQuoteSyntax
   #:args (e1 e2 rs)
   [(prim-quote-syntax !) (make p:quote-syntax e1 e2 rs $1 $2)])

  (PrimVarRef
   #:args (e1 e2 rs)
   [(prim-#%variable-reference !)
    (make p:#%variable-reference e1 e2 rs $1 $2)])

  (PrimStratifiedBody
   #:args (e1 e2 rs)
   [(prim-#%stratified ! ?EB)
    (make p:#%stratified-body e1 e2 rs $1 $2 $3)])

  (PrimSet
   #:args (e1 e2 rs)
   ;; Unrolled to avoid shift/reduce
   [(prim-set! ! resolve Resolves ! next ?EE)
    (make p:set! e1 e2 rs $1 $2 (cons $3 $4) $5 $7)]
   [(prim-set! Resolves ?MacroStep ?EE)
    (make p:set!-macro e1 e2 rs $1 #f ($3 e1 $2 $4))])

  ;; ----------------------------------------
  ;; src/expander/expand/body.rkt

  (EB ;; expand-body
   [(enter-block block-renames ?BlockPass1 block->list ?EL)
    (make bderiv $1 (and $5 (wlderiv-es2 $5)) $2 $3 $5)]
   [(enter-block block-renames BlockPass1 block->letrec
                 ?ESBBRLoop ?EL finish-block)
    (make bderiv $1 $7 $2 $3
          (let* ([letrec-values-id
                  (or (syntax-case (stx-disarm $7) ()
                        [(letX-expr . _)
                         (syntax-case (stx-disarm #'letX-expr) ()
                           [(letX-id . _) (datum->syntax #'letX-id 'letrec-values)]
                           [_ #f])]
                        [_ #f])
                      (datum->artificial-syntax 'letrec-values))]
                 [stx `(,letrec-values-id ,(map list (car $4) (cadr $4)) ,@(cddr $4))])
            (block:letrec stx $5 $6)))])
  (ESBBRLoop
   [(?NextEEs) $1])

  ;; BlockPass1 Answer = (list-of BRule)
  (BlockPass1
   #:skipped null
   [() null]
   [(?BRule ?BlockPass1)
    (cons $1 $2)])

  ;; BRule Answer = BRule
  (BRule
   [(next !!)
    (make b:error $2)]
   [(next ?EE)
    (make b:expr $2)]
   [(next EE prim-begin ! splice !)
    (make b:splice $2 $3 $4 $5 $6)]
   [(next EE prim-define-values ! rename-one !)
    (make b:defvals $2 $3 $4 $5 $6)]
   [(next EE prim-define-syntaxes ! rename-one ! ?PrepareEnv ?BindSyntaxes)
    (make b:defstx $2 $3 $4 $5 $6 $7 $8)])

  ;; ----------------------------------------

  ;; BindSyntaxes Answer = Derivation
  (BindSyntaxes
   [(enter-bind ?EE/LetLifts next ?Eval exit-bind)
    (make bind-syntaxes $2 $4)])

  ;; NextBindSyntaxess Answer = (list-of Derivation)
  (NextBindSyntaxess
   #:skipped null
   [() null]
   [(next ?BindSyntaxes ?NextBindSyntaxess) (cons $2 $3)])

  ;; ----------------------------------------

  ;; Lists
  ;; EL Answer = ListDerivation
  (EL
   #:skipped #f
   [(enter-list ! ?NextEEs exit-list)
    ;; FIXME: Workaround for bug in events
    (if (null? $3)
        (make lderiv null null $2 $3)
        (make lderiv $1 $4 $2 $3))])

  )

;; ----------------------------------------

(define parse-derivation
  (parser #:start Expansion
          #:end EOF
          #:src-pos
          ;; #:debug "/tmp/DEBUG-PARSER.txt"
          #:error deriv-error))

;; ----------------------------------------

(define (derivs->lderiv es1 ds)
  (define es2 (map node-z2 ds))
  (lderiv (stx->list es1) (and es2 (andmap values es2)) #f ds))
