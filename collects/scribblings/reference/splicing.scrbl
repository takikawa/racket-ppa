#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/splicing
                     scheme/stxparam
                     scheme/local))

@(define splice-eval (make-base-eval))
@interaction-eval[#:eval splice-eval (require scheme/splicing 
                                              scheme/stxparam
                                              (for-syntax scheme/base))]

@title[#:tag "splicing"]{Local Binding with Splicing Body}

@note-lib-only[scheme/splicing]

@deftogether[(
@defidform[splicing-let]
@defidform[splicing-letrec]
@defidform[splicing-let-values]
@defidform[splicing-letrec-values]
@defidform[splicing-let-syntax]
@defidform[splicing-letrec-syntax]
@defidform[splicing-let-syntaxes]
@defidform[splicing-letrec-syntaxes]
@defidform[splicing-letrec-syntaxes+values]
@defidform[splicing-local]
)]{

Like @scheme[let], @scheme[letrec], @scheme[let-values],
@scheme[letrec-values], @scheme[let-syntax], @scheme[letrec-syntax],
@scheme[let-syntaxes], @scheme[letrec-syntaxes],
@scheme[letrec-syntaxes+values], and @scheme[local], except that in a
definition context, the body forms are spliced into the enclosing
definition context (in the same way as for @scheme[begin]).

@examples[
#:eval splice-eval
(splicing-let-syntax ([one (lambda (stx) #'1)])
  (define o one))
o
one
]

When a splicing binding form occurs in a @tech{top-level context} or
@tech{module context}, its local bindings are treated similarly to
definitions. In particular, if a reference to one of the splicing
form's bound variables is evaluated before the variable is
initialized, an unbound variable error is raised, instead of the
variable evaluating to the undefined value.  Also, syntax bindings are
evaluated every time the module is @tech{visit}ed, instead of only
once during compilation as in @scheme[let-syntax], etc.

@examples[
#:eval splice-eval
(splicing-letrec ([x bad]
                  [bad 1])
  x)]
}

@defidform[splicing-syntax-parameterize]{

Like @scheme[syntax-parameterize], except that in a definition
context, the body forms are spliced into the enclosing definition
context (in the same as as for @scheme[begin]), as long as the body
forms are valid in an internal-definition context. In particular,
@scheme[require] and @scheme[provide] forms cannot appear in the body
of @scheme[splicing-syntax-parameterize], even if
@scheme[splicing-syntax-parameterize] is used in a @scheme[module]
body. In a definition context, the body of
@scheme[splicing-syntax-parameterize] can be empty.

@examples[
#:eval splice-eval
(define-syntax-parameter place (lambda (stx) #'"Kansas"))
(define-syntax-rule (where) `(at ,(place)))
(where)
(splicing-syntax-parameterize ([place (lambda (stx) #'"Oz")])
  (define here (where)))
here
]}

@; ----------------------------------------------------------------------

@close-eval[splice-eval]
