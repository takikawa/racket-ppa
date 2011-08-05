#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@title{Utilities}

Typed Racket provides some additional utility functions to facilitate typed programming.

@defproc*[
([(assert [v (U #f A)]) A]
 [(assert [v A] [p? (A -> Any : B)]) B])]{
Verifies that the argument satisfies the constraint.  If no predicate
is provided, simply checks that the value is not
@racket[#f].
}

@examples[#:eval the-top-eval
(define: x : (U #f String) (number->string 7))
x
(assert x)
(define: y : (U String Symbol) "hello")
y
(assert y string?)
(assert y boolean?)]

@defform*/subs[[(with-asserts ([id maybe-pred] ...) body ...+)]
              ([maybe-pred code:blank
                           (code:line predicate)])]{
Guard the body with assertions. If any of the assertions fail, the
program errors. These assertions behave like @racket[assert].
}


@defproc[(defined? [v any/c]) boolean?]{A predicate for determining if
@racket[v] is @emph{not} @|undefined-const|.}

@defproc[(index? [v any/c]) boolean?]{A predicate for the @racket[Index]
type.}

@(close-eval the-eval)
