#lang scribble/doc
@(require "common.rkt" (for-label syntax/free-vars))

@title[#:tag "free-vars"]{Computing the Free Variables of an Expression}

@defmodule[syntax/free-vars]

@defproc[(free-vars [expr-stx syntax?]) (listof identifier?)]{

Returns a list of free @racket[lambda]- and @racket[let]-bound
identifiers in @racket[expr-stx]. The expression must be fully
expanded (see @secref[#:doc refman "fully-expanded"] and
@racket[expand]).}
