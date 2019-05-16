#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@(define the-eval (make-sp-eval))

@title[#:tag "exprc"]{Contracts on Macro Sub-expressions}

Just as procedures often expect certain kinds of values as arguments,
macros often have expectations about the expressions they are
given. And just as procedures express those expectations via
contracts, so can macros, using the @racket[expr/c] syntax class.

For example, here is a macro @racket[myparameterize] that behaves like
@racket[parameterize] but enforces the @racket[parameter?] contract on
the parameter expressions.

@interaction[#:eval the-eval
(define-syntax (myparameterize stx)
  (syntax-parse stx
    [(_ ((p v:expr) ...) body:expr)
     #:declare p (expr/c #'parameter?
                         #:name "parameter argument")
     #'(parameterize ([p.c v] ...) body)]))
(myparameterize ([current-input-port
                  (open-input-string "(1 2 3)")])
  (read))
(myparameterize (['whoops 'something])
  'whatever)
]

@bold{Important:} Make sure when using @racket[expr/c] to use the
@racket[c] attribute. If the macro above had used @racket[p] in the
template, the expansion would have used the raw, unchecked
expressions. The @racket[expr/c] syntax class does not change how
pattern variables are bound; it only computes an attribute that
represents the checked expression.

The previous example shows a macro applying a contract on an argument,
but a macro can also apply a contract to an expression that it
produces. In that case, it should use @racket[#:arg? #f] to indicate
that the macro, not the calling context, is responsible for expression
produced.

@interaction[#:eval the-eval
(code:comment "BUG: rationals not closed under inversion")
(define-syntax (invert stx)
  (syntax-parse stx
    [(_ e)
     #:declare e (expr/c #'rational?)
     #:with result #'(/ 1 e.c)
     #:declare result (expr/c #'rational? #:arg? #f)
     #'result.c]))

(invert 4)
(invert 'abc)
(invert 0.0)
]

@(close-eval the-eval)
