#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title{Modules and reusable syntax classes}

As demonstrated in the @secref{stxparse-intro}, the simplest place to
define a syntax class is within the macro definition that uses it. But
this location, of course, limits the scope of the syntax class to the
one client macro. Creating reusable syntax classes is slightly
complicated, however, by the Racket @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{phase level} separation. A
syntax class defined within a module cannot be used by macros in the
same module; it is defined at the wrong phase.

@myinteraction[
(module phase-mismatch-mod racket
  (require syntax/parse (for-syntax syntax/parse))
  (define-syntax-class foo
    (pattern (a b c)))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))
]

In the module above, the syntax class @scheme[foo] is defined at phase
level 0. The reference to @scheme[foo] within @scheme[macro], however,
is at phase level 1, being the implementation of a macro
transformer. (Needing to require @schememodname[syntax/parse] twice,
once normally and once @scheme[for-syntax] is another sign of the
phase level incompatibility.) The only way to define reusable syntax
classes that can be used within macros is to define them in a separate
module and require that module @scheme[for-syntax].

@myinteraction[
(module stxclass-mod racket
  (require syntax/parse)
  (define-syntax-class foo
    (pattern (a b c)))
  (provide foo))
(module macro-mod racket
  (require (for-syntax syntax/parse
                       'stxclass-mod))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)]))
  (provide macro))
(require 'macro-mod)
(macro (1 2 3))
]

If the syntax classes refer to keywords, or if they compute
expressions via syntax templates, then the module containing the
syntax classes must generally require the keywords or bindings used in
the syntax templates @scheme[for-template].

@myinteraction[
(module arith-keywords-mod racket
  (define-syntax plus (syntax-rules ()))
  (define-syntax times (syntax-rules ()))
  (provide plus times))

(module arith-stxclass-mod racket
  (require syntax/parse
           (for-template 'arith-keywords-mod
                         racket))
  (define-syntax-class arith
    #:literals (plus times)
    (pattern n:nat
             #:with expr #'n)
    (pattern (plus a:arith b:arith)
             #:with expr #'(+ a.expr b.expr))
    (pattern (times a:arith b:arith)
             #:with expr #'(* a.expr b.expr)))
  (provide arith))

(module arith-macro-mod racket
  (require (for-syntax syntax/parse
                       'arith-stxclass-mod)
           'arith-keywords-mod)
  (define-syntax (arith-macro stx)
    (syntax-parse stx
      [(_ a:arith)
       #'(values 'a.expr a.expr)]))
  (provide arith-macro
           (all-from-out 'arith-keywords-mod)))

(require 'arith-macro-mod)
(arith-macro (plus 1 (times 2 3)))
]

In @scheme['arith-stxclass-mod], the module
@scheme['arith-keywords-mod] must be required @scheme[for-template]
because the keywords are used in phase-0 expressions. Likewise, the
module @schememodname[racket] must be required @scheme[for-template]
because the syntax class contains syntax templates involving
@scheme[+] and @scheme[*] (and, in fact, the implicit @scheme[#%app]
syntax). All of these identifiers (the keywords @scheme[plus] and
@scheme[times]; the procedures @scheme[+] and @scheme[*]; and the
implicit syntax @scheme[#%app]) must be bound at ``absolute'' phase
level 0. Since the module @scheme['arith-stxclass-mod] is required
with a phase level offset of 1 (that is, @scheme[for-syntax]), it must
compensate with a phase level offset of -1, or @scheme[for-template].
