#lang scribble/doc
@(require "common.ss"
          "std-grammar.ss"
          "prim-ops.ss"
          (for-label lang/htdp-advanced))

@(define-syntax-rule (bd intm-define intm-define-struct intm-lambda intm-local intm-letrec intm-let intm-let* intm-time)
   (begin
    (require (for-label lang/htdp-intermediate))
    (define intm-define (scheme define))
    (define intm-define-struct (scheme define-struct))
    (define intm-lambda (scheme lambda))
    (define intm-local (scheme local))
    (define intm-letrec (scheme letrec))
    (define intm-let (scheme let))
    (define intm-let* (scheme let*))
    (define intm-time (scheme time))))
@(bd intm-define intm-define-struct intm-lambda intm-local intm-letrec intm-let intm-let* intm-time)

@(define-syntax-rule (bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)
   (begin
    (require (for-label lang/htdp-beginner))
    (define beg-define (scheme define))
    (define beg-define-struct (scheme define-struct))
    (define beg-cond (scheme cond))
    (define beg-if (scheme if))
    (define beg-and (scheme and))
    (define beg-or (scheme or))
    (define beg-check-expect (scheme check-expect))
    (define beg-require (scheme require))))
@(bbd beg-define beg-define-struct beg-cond beg-if beg-and beg-or beg-check-expect beg-require)


@title[#:style 'toc #:tag "advanced"]{Advanced Student}

@declare-exporting[lang/htdp-advanced]

@schemegrammar*+qq[
#:literals (define define-struct lambda cond else if and or empty true false require lib planet
            local let let* letrec time begin begin0 set! delay shared recur when case unless
            check-expect check-within check-error)
(check-expect check-within check-error require)
[program (code:line def-or-expr ...)]
[def-or-expr definition
             expr
             test-case
             library-require]
[definition (define (id id id ...) expr)
            (define id expr)
            (define-struct id (id ...))]
[expr (begin expr expr ...)
      (begin0 expr expr ...)
      (set! id expr)
      (delay expr)
      (lambda (id id ...) expr)
      (local [definition ...] expr)
      (letrec ([id expr] ...) expr)
      (shared ([id expr] ...) expr)
      (let ([id expr] ...) expr)
      (let id ([id expr] ...) expr)
      (let* ([id expr] ...) expr)
      (recur id ([id expr] ...) expr)
      (code:line (expr expr expr ...) (code:comment #, @seclink["intermediate-lambda-call"]{function call}))
      (cond [expr expr] ... [expr expr])
      (cond [expr expr] ... [else expr])
      (case expr [(choice choice ...) expr] ... 
                 [(choice choice ...) expr])
      (case expr [(choice choice ...) expr] ... 
                 [else expr])
      (if expr expr expr)
      (when expr expr)
      (unless expr expr)
      (and expr expr expr ...)
      (or expr expr expr ...)
      (time expr)
      empty
      (code:line id (code:comment #, @seclink["intermediate-id"]{identifier}))
      (code:line prim-op (code:comment #, @seclink["intermediate-lambda-prim-op"]{primitive operation}))
      'id
      (code:line #, @elem{@schemevalfont{'}@scheme[quoted]} (code:comment #, @seclink["beginner-abbr-quote"]{quoted value}))
      (code:line #, @elem{@schemevalfont{`}@scheme[quasiquoted]} (code:comment #, @seclink["beginner-abbr-quasiquote"]{quasiquote}))
      number
      true
      false
      string
      character]
[choice (code:line id (code:comment #, @t{treated as a symbol}))
        number]
]

@|prim-nonterms|

@prim-ops['(lib "htdp-advanced.ss" "lang") #'here]

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define"]{@scheme[define]}

@deftogether[(
@defform[(define (id id ...) expr)]
@defform/none[#:literals (define)
              (define id expr)]
)]{

The same as Intermediate with Lambda's @|intm-define|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-define-struct"]{@scheme[define-struct]}

@defform[(define-struct structid (fieldid ...))]{

The same as Intermediate's @|intm-define-struct|, but defines an
additional set of operations:

@itemize{

 @item{@schemeidfont{set-}@scheme[structid]@schemeidfont{-}@scheme[fieldid]@schemeidfont{!}
       : takes an instance of the structure and a value, and changes
       the instance's field to the given value.}

}}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-lambda"]{@scheme[lambda]}

@defform[(lambda (id ...) expr)]{

The same as Intermediate with Lambda's @|intm-lambda|, except that a
function is allowed to accept zero arguments.}

@; ----------------------------------------------------------------------

@section{@scheme[begin]}

@defform[(begin expr expr ...)]{

Evaluates the @scheme[expr]s in order from left to right. The value of
the @scheme[begin] expression is the value of the last @scheme[expr].}

@; ----------------------------------------------------------------------

@section{@scheme[begin0]}

@defform[(begin0 expr expr ...)]{

Evaluates the @scheme[expr]s in order from left to right. The value of
the @scheme[begin] expression is the value of the first @scheme[expr].}

@; ----------------------------------------------------------------------

@section{@scheme[set!]}

@defform[(set! id expr)]{

Evaluates @scheme[expr], and then changes the definition @scheme[id]
to have @scheme[expr]'s value. The @scheme[id] must be defined or
bound by @scheme[letrec], @scheme[let], or @scheme[let*].}

@; ----------------------------------------------------------------------

@section{@scheme[delay]}

@defform[(delay expr)]{

Produces a ``promise'' to evaluate @scheme[expr]. The @scheme[expr] is
not evaluated until the promise is forced through the @scheme[force]
operator; when the promise is forced, the result is recorded, so that
any further @scheme[force] of the promise always produces the
remembered value.}

@; ----------------------------------------------------------------------

@section{@scheme[shared]}

@defform[(shared ([id expr] ...) expr)]{

Like @scheme[letrec], but when an @scheme[expr] next to an @scheme[id]
is a @scheme[cons], @scheme[list], @scheme[vector], quasiquoted
expression, or @schemeidfont{make-}@scheme[_structid] from a
@scheme[define-struct], the @scheme[expr] can refer directly to any
@scheme[id], not just @scheme[id]s defined earlier. Thus,
@scheme[shared] can be used to create cyclic data structures.}

@; ----------------------------------------------------------------------

@section[#:tag "advanced-let"]{@scheme[let]}

@defform*[[(let ([id expr] ...) expr)
           (let id ([id expr] ...) expr)]]{

The first form of @scheme[let] is the same as Intermediate's
@|intm-let|.

The second form is equivalent to a @scheme[recur] form.}


@; ----------------------------------------------------------------------

@section{@scheme[recur]}

@defform[(recur id ([id expr] ...) expr)]{

A short-hand recursion construct. The first @scheme[id] corresponds to
the name of the recursive function. The parenthesized @scheme[id]s are
the function's arguments, and each corresponding @scheme[expr] is a
value supplied for that argument in an initial starting call of the
function. The last @scheme[expr] is the body of the function.

More precisely, a @scheme[recur] form 

@schemeblock[
(recur func-id ([arg-id arg-expr] (unsyntax @schemeidfont{...}))
  body-expr)
]

is equivalent to

@schemeblock[
((local [(define (func-id arg-id (unsyntax @schemeidfont{...}))
           body-expr)]
   func-id)
  arg-expr (unsyntax @schemeidfont{...}))
]}

@; ----------------------------------------------------------------------

@section{@scheme[case]}

@defform[(case expr [(choice ...) expr] ... [(choice ...) expr])]{

A @scheme[case] form contains one or more ``lines'' that are
surrounded by parentheses or square brackets. Each line contains a
sequence of choices---numbers and names for symbols---and an answer
@scheme[expr]. The initial @scheme[expr] is evaluated, and the
resulting value is compared to the choices in each line, where the
lines are considered in order. The first line that contains a matching
choice provides an answer @scheme[expr] whose value is the result of
the whole @scheme[case] expression. If none of the lines contains a
matching choice, it is an error.}

@defform/none[#:literals (cond else)
              (cond expr [(choice ...) expr] ... [else expr])]{

This form of @scheme[case] is similar to the prior one, except that
the final @scheme[else] clause is always taken if no prior line
contains a choice matching the value of the initial @scheme[expr]. In
other words, so there is no possibility to ``fall off them end'' of
the @scheme[case] form.}

@; ----------------------------------------------------------------------

@section{@scheme[when] and @scheme[unless]}

@defform[(when expr expr)]{

The first @scheme[expr] (known as the ``test'' expression) is
evaluated. If it evaluates to @scheme[true], the result of the
@scheme[when] expression is the result of evaluating the second
@scheme[expr], otherwise the result is @scheme[(void)] and the second
@scheme[expr] is not evaluated. If the result of evaluating the test
@scheme[expr] is neither @scheme[true] nor @scheme[false], it is an
error.}

@defform[(unless expr expr)]{

Like @scheme[when], but the second @scheme[expr] is evaluated when the
first @scheme[expr] produces @scheme[false] instead of @scheme[true].}

@; ----------------------------------------

@section[#:tag "advanced-prim-ops"]{Primitive Operations}

@prim-op-defns['(lib "htdp-advanced.ss" "lang") #'here '()]

@; ----------------------------------------------------------------------

@section[#:tag "advanced-unchanged"]{Unchanged Forms}

@deftogether[(
@defform[(local [definition ...] expr)]
@defform[(letrec ([id expr-for-let] ...) expr)]
@defform[(let* ([id expr-for-let] ...) expr)]
)]{

The same as Intermediate's @|intm-local|, @|intm-letrec|, and
@|intm-let*|.}


@deftogether[(
@defform[(cond [expr expr] ... [expr expr])]
@defidform[else]
)]{

The same as Beginning's @|beg-cond|, except that @scheme[else] can be
used with @scheme[case].}



@defform[(if expr expr expr)]{

The same as Beginning's @|beg-if|.}

@deftogether[(
@defform[(and expr expr expr ...)]
@defform[(or expr expr expr ...)]
)]{

The same as Beginning's @|beg-and| and @|beg-or|.}


@defform[(time expr)]{

The same as Intermediate's @|intm-time|.}


@deftogether[(
@defform[(check-expect expr expr)]
@defform[(check-within expr expr expr)]
@defform[(check-error expr expr)]
)]{

The same as Beginning's @|beg-check-expect|, etc.}



@deftogether[(
@defthing[empty empty?]
@defthing[true boolean?]
@defthing[false boolean?]
)]{

Constants for the empty list, true, and false.}

@defform[(require string)]{

The same as Beginning's @|beg-require|.}
