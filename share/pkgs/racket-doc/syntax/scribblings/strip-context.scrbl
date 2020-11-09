#lang scribble/doc
@(require "common.rkt" (for-label syntax/strip-context))

@title[#:tag "strip-context"]{Replacing Lexical Context}

@defmodule[syntax/strip-context]

@defproc[(strip-context [stx syntax?]) syntax?]{

Removes all lexical context from @racket[stx], preserving
source-location information and properties.

@history[#:changed "7.7.0.10" @elem{Repaired to traverse hash tables in @racket[stx].}]}


@defproc[(replace-context [ctx-stx (or/c syntax? #f)] [stx syntax?]) syntax?]{

Uses the lexical context of @racket[ctx-stx] to replace the lexical
context of all parts of @racket[stx], preserving source-location
information and properties of @racket[stx].

@history[#:changed "7.7.0.10" @elem{Repaired to traverse hash tables in @racket[stx].}]}
