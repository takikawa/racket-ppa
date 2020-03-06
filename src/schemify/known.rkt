#lang racket/base

;; Ths module uses `#:omit-define-syntaxes` and doesn't use
;; `struct-out` so that none of the exports are syntax bindings.

(provide known-constant known-constant?
         known-consistent known-consistent?
         known-authentic known-authentic?
         known-copy? known-copy known-copy-id
         known-literal known-literal? known-literal-value
         known-procedure known-procedure? known-procedure-arity-mask
         known-procedure/no-prompt known-procedure/no-prompt?
         known-procedure/folding known-procedure/folding?
         known-procedure/folding/limited known-procedure/folding/limited? known-procedure/folding/limited-kind
         known-procedure/can-inline known-procedure/can-inline? known-procedure/can-inline-expr
         known-procedure/can-inline/need-imports known-procedure/can-inline/need-imports?
         known-procedure/can-inline/need-imports-needed
         known-procedure/succeeds known-procedure/succeeds?
         known-procedure/pure known-procedure/pure?
         known-procedure/pure/folding known-procedure/pure/folding? ; not a subtype of `known-procedure/folding`
         known-procedure/pure/folding-unsafe known-procedure/pure/folding-unsafe?
         known-procedure/pure/folding-unsafe-safe
         known-procedure/has-unsafe known-procedure/has-unsafe? known-procedure/has-unsafe-alternate
         known-procedure/has-unsafe/folding known-procedure/has-unsafe/folding?  ; not a subtype of `known-procedure/folding`
         known-procedure/has-unsafe/folding/limited known-procedure/has-unsafe/folding/limited?
         known-procedure/has-unsafe/folding/limited-kind
         known-struct-type known-struct-type? known-struct-type-type
         known-struct-type-field-count known-struct-type-pure-constructor?
         known-constructor known-constructor? known-constructor-type
         known-predicate known-predicate? known-predicate-type
         known-accessor known-accessor? known-accessor-type
         known-mutator known-mutator? known-mutator-type
         known-field-accessor known-field-accessor? known-field-accessor-type-id known-field-accessor-pos
         known-field-mutator known-field-mutator? known-field-mutator-type-id known-field-mutator-pos
         known-field-accessor/need-imports known-field-accessor/need-imports? known-field-accessor/need-imports-needed
         known-field-mutator/need-imports known-field-mutator/need-imports? known-field-mutator/need-imports-needed
         known-struct-type-property/immediate-guard known-struct-type-property/immediate-guard?
         a-known-constant
         a-known-consistent)

;; reflects an immutable variable, but nothing is known about the
;; variable's value
(struct known-constant () #:prefab #:omit-define-syntaxes)

;; the value at run time always has the same "shape", such as always being
;; a procedure of 1 argument, always being a structure type, or always
;; being a predicate for a structure type
(struct known-consistent () #:prefab #:omit-define-syntaxes #:super struct:known-constant)

;; the value at run time is never an impersonator
(struct known-authentic () #:prefab #:omit-define-syntaxes #:super struct:known-constant)

;; copy propagation --- use for local bindings or copies of primitives, only
(struct known-copy (id) #:prefab #:omit-define-syntaxes #:super struct:known-constant)

;; literal for constant propagation:
(struct known-literal (value) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

;; procedure with arity mask; the procedure has to be a procedure from the host
;; Scheme's perspective --- not an applicable struct or chaperoned procedure, which
;; means that parameters don't count
(struct known-procedure (arity-mask) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

;; procedure that does not need to run inside a module prompt, which implies that the
;; procedure does not call arbitrary other code, not even through an impersonator/chaperone
;; interposition procedure
(struct known-procedure/no-prompt () #:prefab #:omit-define-syntaxes #:super struct:known-procedure)

;; procedure that can be inlined, where the `expr` is in pre-schemify form
(struct known-procedure/can-inline (expr) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-procedure/can-inline/need-imports (needed) ; (list (cons <sym> (cons <sym> <#f-or-index>)) ...)
  #:prefab #:omit-define-syntaxes #:super struct:known-procedure/can-inline)

;; procedure that can be applied at compile time to literals and returns a single value
(struct known-procedure/folding () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/no-prompt)

;; procedure that's folding, but with some constraint described by `kind` (e.g.,
;; `'expt` to mean "apply only to small numbers")
(struct known-procedure/folding/limited (kind) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/folding)

;; procedure that never raises an exception or otherwise captures/escapes the calling context
(struct known-procedure/succeeds () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/no-prompt)

;; procedure that accepts any arguments, returns a single value, and is functional so that it can be reordered
(struct known-procedure/pure () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/succeeds)

;; pure and folding:
(struct known-procedure/pure/folding () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure)
(struct known-procedure/pure/folding-unsafe (safe) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure/folding)

;; procedure (no-prompt) with an unsafe variant, especially ones that won't get substituted
;; simply by compiling in unsafe mode
(struct known-procedure/has-unsafe (alternate) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/no-prompt)
(struct known-procedure/has-unsafe/folding () #:prefab #:omit-define-syntaxes #:super struct:known-procedure/has-unsafe)
(struct known-procedure/has-unsafe/folding/limited (kind) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/has-unsafe/folding)

(struct known-struct-type (type field-count pure-constructor?) #:prefab #:omit-define-syntaxes #:super struct:known-consistent)

;; procedures with a known connection to a structure type:
(struct known-constructor (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure)
(struct known-predicate (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure/pure)
(struct known-accessor (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-mutator (type) #:prefab #:omit-define-syntaxes #:super struct:known-procedure)
(struct known-field-accessor (type-id pos) #:prefab #:omit-define-syntaxes #:super struct:known-accessor)
(struct known-field-mutator (type-id pos) #:prefab #:omit-define-syntaxes #:super struct:known-mutator)
(struct known-field-accessor/need-imports (needed) #:prefab #:omit-define-syntaxes #:super struct:known-field-accessor)
(struct known-field-mutator/need-imports (needed) #:prefab #:omit-define-syntaxes #:super struct:known-field-mutator)

(struct known-struct-type-property/immediate-guard () #:prefab #:omit-define-syntaxes)

(define a-known-constant (known-constant))
(define a-known-consistent (known-consistent))
