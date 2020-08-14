#lang racket/base
(provide (all-defined-out))

;; For brevity, the type (Listof X) is abbreviated Xs. For example,
;; (Listof Stx) is written Stxs.

;; Because of errors, any node might have missing fields after an
;; error or an interrupted sub-node. For example, consider the type
;; declaration for MRule:
;;   (mrule Stx Stx Ids ?Exn Stx LocalActions ?Exn Stx Deriv)
;; The ?Exn means an Exn value is present iff the subsequent fields
;; are skipped (ie, have #f or other default values). Furthermore, the
;; LocalActions type might also be interrupted, which means that the
;; fields following it would be skipped.

;; type Node(a) = (node a ?a)
(struct node (z1 z2) #:transparent)

;; type Deriv = (deriv Stx Stx) <: Node(Stx)
;; type Base = (base Stx Stx Ids Stx/#f ?Exn) <: Deriv
;; Sometimes, deriv's z1/z2 (aka e1/e2) may be #f because deriv is fake; eg,
;; p:define-values in module pass2.
(struct deriv node () #:transparent)
(struct base deriv (resolves de1 ?1) #:transparent)

;; A TopDeriv is one of
;; - (ecte Stx Stx LocalActions Deriv DerivInECTE LocalActions)
;;   represents expand/compile-time-evals, also main expand per-top-level loop
;; - (lift-deriv Stx Stx Deriv Stxs TopDeriv)
;; - Deriv
;; where DerivInECTE = TopDeriv | p:begin or p:begin-for-syntax containing TopDerivs
(struct ecte deriv (locals first second locals2) #:transparent)
(struct lift-deriv deriv (first lift-stx second) #:transparent)

;; A DerivLL is one of Deriv | (lift/let-deriv Stx Stx Deriv Stx DerivLL)
(struct lift/let-deriv deriv (first lift-stx second) #:transparent)

;; A Deriv is one of
;; - MRule
;; - PrimDeriv
;; - (tagrule Stx Stx Stx Stx Deriv)
(struct tagrule deriv (untagged-stx tagged-stx next) #:transparent)


;; ============================================================
;; Macros

;; A MRule is (mrule <Base> Stx LocalActions ?Exn Stx Stx Deriv)
(struct mrule base (me1 locals me2 ?2 etx retx next) #:transparent)

;; A LocalAction is one of:
;; - (local-exn Exn)
;; - (local-expansion Stx Stx Bool Stx Deriv ??? Stx Opaque/#f)
;; - (local-lift-expr Stx Stx Ids)      -- orig, w/ scopes, ids
;; - (local-lift-end Stx Stx Stx)       -- orig, w/ scopes, wrapped
;; - (local-lift-require Stx Stx Stx)   -- wrapped-req, orig-use-s, renamed-use-s
;; - (local-lift-provide Stx)
;; - (local-lift-module Stx Stx)        -- orig, renamed
;; - (local-bind Ids ?Exn Stx Stx) -- FIXME: renames structure?
;; - (local-value Id ?Exn Ids Bool IdentifierBinding)
;; - (track-syntax (U 'track-origin 'arm 'disarm 'rearm) Stx Stx)
;; - (local-remark (Listof (U String Syntax)))
;; - (local-mess (Listof Event))
(struct local-exn (exn) #:transparent)
(struct local-expansion node (for-stx? me1 inner lifted me2 opaque) #:transparent)
(struct local-lift-expr (orig-expr renamed-expr ids) #:transparent)
(struct local-lift-end (orig renamed wrapped) #:transparent)
(struct local-lift-require (req expr mexpr) #:transparent)
(struct local-lift-provide (prov) #:transparent)
(struct local-lift-module (orig renamed) #:transparent)
(struct local-bind (names ?1 renames bindrhs) #:transparent)
(struct local-value (name ?1 resolves bound? binding) #:transparent)
;;   binding is saved (identifier-binding name) at time of lookup, since it may change
;;   if name is rebound in definition context
(struct track-syntax (operation new-stx old-stx) #:transparent)
(struct local-remark (contents) #:transparent)
(struct local-mess (events) #:transparent)


;; ============================================================
;; Primitives

;; A PrimDeriv <: Base is one of the following:
(struct prule base () #:transparent)

;; - p:variable is (p:variable <Base>)
(struct p:variable prule () #:transparent)

;; - (p:module <Base> PrepareEnv Stx ModEnsureMB Deriv Stx)
(struct p:module prule (prep rename ensure-mb body shift) #:transparent)

;; - (p:#%module-begin <Base> Stx ModPass1And2 ?Exn ModPass3 ?Exn ModPass4)
(struct p:#%module-begin prule (me pass12 ?2 pass3 ?3 pass4) #:transparent)

;; - (p:define-syntaxes <Base> LocalActions DerivLL LocalActions)
(struct p:define-syntaxes prule (prep rhs locals) #:transparent)

;; - (p:define-values <Base> Deriv)
(struct p:define-values prule (rhs) #:transparent)

;; - (p:#%expression <Base> Deriv Stx/#f)
(struct p:#%expression prule (inner untag) #:transparent)

;; - (p:if <Base> Deriv Deriv Deriv)
(struct p:if prule (test then else) #:transparent)
;; - (p:wcm <Base> Deriv Deriv Deriv)
(struct p:wcm prule (key mark body) #:transparent)
;; - (p:set! <Base> Resolves ?Exn Deriv)
(struct p:set! prule (id-rs ?2 rhs) #:transparent)
;; - (p:set!-macro <Base> Deriv)
(struct p:set!-macro prule (deriv) #:transparent)

;; - (p:#%app <Base> Derivs)
(struct p:#%app prule (derivs) #:transparent)
;; - (p:begin <Base> Derivs)
(struct p:begin prule (derivs) #:transparent)
;; - (p:begin0 <Base> Derivs)
(struct p:begin0 prule (derivs) #:transparent)

;; - (p:lambda <Base> LambdaRenames BDeriv)
(struct p:lambda prule (renames body) #:transparent)
;; - (p:case-lambda <Base> CaseLambdaClauses)
;;   where a CaseLambdaClause is (clc ?Exn CaseLambdaRename BDeriv)
(struct p:case-lambda prule (renames+bodies) #:transparent)
(struct clc (?1 renames body) #:transparent)

;; LambdaRenames = (list* Stx Stxs)    -- renamed (formals . body)
;; LetRenames = (list* Stxs Stxs)      -- renamed (((vars rhs) ...) . body)
;; LSVRenames = (list* Stxs Stxs Stxs) -- renamed (((svars srhs) ...) ((vvars vrhs) ...) . body)

;; - (p:let-values <Base> LetRenames (list-of Deriv) BDeriv)
(struct p:let-values prule (renames rhss body) #:transparent)
;; - (p:letrec-values <Base> LetRenames (list-of Deriv) BDeriv)
(struct p:letrec-values prule (renames rhss body) #:transparent)
;; - (p:letrec-syntaxes+values <Base> LSVRenames PrepareExpEnv BindSyntaxes-s Derivs BDeriv)
(struct p:letrec-syntaxes+values prule (srenames prep sbindrhss vrhss body) #:transparent)

;; - (p:provide <Base> Derivs ?Exn)
(struct p:provide prule (inners ?2) #:transparent)

;; - (p:require <Base> LocalActions)
(struct p:require prule (locals) #:transparent)

;; - (p:submodule <Base> p:module LocalActions)
(struct p:submodule prule (exp locals) #:transparent)
;; - (p:submodule* <Base> p:module LocalActions)
(struct p:submodule* prule (exp locals) #:transparent)

;; - (p:#%stratified-body <Base> BDeriv)
(struct p:#%stratified-body prule (bderiv) #:transparent)

;; - (p:begin-for-syntax <Base> LocalActions BFSBody)
;;   where BFSBody = ModPass1And2 | (list BeginForSyntaxLifts ... LDeriv)
(struct p:begin-for-syntax prule (prep body locals) #:transparent)

;; - (p:stop <Base>)
;; - (p:unknown <Base>)
;; - (p:#%top <Base>)
;; - (p:#%datum <Base>)
;; - (p:quote <Base>)
;; - (p:quote-syntax <Base>)
;; - (p:#%variable-reference <Base>)
;; - (p:opaque <Base>)
(struct p::STOP prule () #:transparent)
(struct p:stop p::STOP () #:transparent)
(struct p:unknown p::STOP () #:transparent)
(struct p:#%top p::STOP () #:transparent)
(struct p:#%datum p::STOP () #:transparent)
(struct p:quote p::STOP () #:transparent)
(struct p:quote-syntax p::STOP () #:transparent)
(struct p:#%variable-reference p::STOP () #:transparent)
(struct p:opaque p::STOP () #:transparent)

;; - (p:declare <Base>)
(struct p:declare prule () #:transparent)


;; ============================================================
;; Blocks

;; A LDeriv is (lderiv Stxs Stxs ?Exn Derivs)
(struct lderiv node (?1 derivs) #:transparent)

;; A BDeriv is (bderiv Stxs Stxs BlockRenames BRules (U LDeriv BlockLetrec))
;; where BlockRenames = (list* Stxs Stxs) -- (list* renamed-stxs orig-stxs)
(struct bderiv node (renames pass1 pass2) #:transparent)

;; A BlockLetrec is (block:letrec Stx Derivs LDeriv)
(struct block:letrec (stx rhss lderiv) #:transparent)

;; A BRule is one of
;; - (b:error exn)
;; - (b:expr Deriv)
;; - (b:splice Deriv Stx/#f ?Exn Stxs ?Exn)
;; - (b:defvals Deriv Stx/#f ?Exn Stx ?Exn)
;; - (b:defstx Deriv Stx/#f ?Exn Stx ?Exn PrepareExpEnv BindSyntaxes)
(struct brule () #:transparent)
(struct b:error brule (?1) #:transparent)
(struct b:expr brule (head) #:transparent)
(struct b:splice brule (head da ?1 tail ?2) #:transparent)
(struct b:defvals brule (head da ?1 rename ?2) #:transparent)
(struct b:defstx brule (head da ?1 rename ?2 prep bindrhs) #:transparent)


;; ============================================================
;; Modules

;; A ModEnsureMB is (mod:ensure-mb Track/#f Deriv/#f ModAddMB Track)
;; A ModAddMB is (mod:add-mb ?Exn Stx Track Deriv ?Exn)
;; where Track = (list* 'property Stx Stx)
(struct mod:ensure-mb (track1 check add-mb track2) #:transparent)
(struct mod:add-mb (?1 tag track check ?2) #:transparent)

;; A ModuleBegin/Phase is (module-begin/phase ModulePass1 ModulePass2 ModulePass3)
;;X (struct module-begin/phase (pass1 pass2 pass3) #:transparent)
;; FIXME

;; A ModPass1And2 is (mod:pass-1-and-2 ModPass1 ModPass2)
(struct mod:pass-1-and-2 (pass1 pass2) #:transparent)

;; A ModPass1 is (Listof ModRule1)
;; A ModPass2 is (Listof ModRule2)
;; A ModPass3 is (Listof ModRule3)
;; A ModPass4 is (Listof ModRule4)

;; A ModRule1 is one of 
;; - (make-mod:lift-end Stxs)
;; - (make-modp1:prim ModPass1Head ModPass1Prim)
;; A ModPass1Head is one of
;; - Deriv
;; - (modp1:lift Deriv Syntaxes Syntaxes Syntaxes ModPass1)
;; A ModPass1Prim is one of
;; - (modp1:splice Stx/#f ?Exn Syntaxes)
;; - (p:begin-for-syntax Stxs Stxs Ids ?Exn LocalActions ModPass1And2 LocalActions)
;; - p:define-values, p:define-syntaxes, p:require, p:submodule, p:declare, p:stop
(struct mod:lift-end (ends) #:transparent)
(struct modp1:prim (head prim) #:transparent)
(struct modp1:lift (head lifted-defs lifted-reqs lifted-mods mods) #:transparent)
(struct modp1:splice (de1 ?1 tail) #:transparent)

;; A ModRule2 is one of
;; - (mod:lift-end Stxs)
;; - (modp2:skip)
;; - (modp2:cons Deriv LocalActions)
;; - (modp2:lift Deriv LocalActions Syntaxes Syntaxes Syntaxes ModP2Submods ModPass2)
(struct modp2:skip () #:transparent)
(struct modp2:cons (deriv locals) #:transparent)
(struct modp2:lift (deriv locals lifted-reqs lifted-mods lifted-defs mods defs) #:transparent)

;; A ModP2Submod is one of
;; - #f
;; - p:submodule

;; A ModRule3 is one of
;; - #f
;; - (modp34:bfs (Listof ModRule3))
;; - p:provide
(struct modp34:bfs (subs) #:transparent)

;; A ModRule4 is one of
;; - #f
;; - (modp34:bfs (Listof ModRule4))
;; - p:submodule*


;; ============================================================
;; Misc

;; type PrepareExpEnv = LocalActions

;; A BindSyntaxes is (bind-syntaxes DerivLL LocalActions)
(struct bind-syntaxes (rhs locals) #:transparent)

;; A BeginForSyntaxLifts is (make-bfs:lift LDeriv Stxs)
(struct bfs:lift (lderiv lifts) #:transparent)
