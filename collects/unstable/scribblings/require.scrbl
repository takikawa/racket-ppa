#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/require))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/require))

@title{Requiring Modules}

@defmodule[unstable/require]

This module provides tools for importing from modules.

@defform[(require/provide module-path ...)]{

Re-exports all bindings provided by each @racket[module-path].  Equivalent to:

@racketblock[
(require module-path ...)
(provide (all-from-out module-path ...))
]

}

@defform[(quote-require require-spec ...)]{

Produces the names exported by the @racket[require-spec]s as a list of symbols.

@examples[
#:eval the-eval
(quote-require racket/bool racket/function)
]

}

@(close-eval the-eval)
