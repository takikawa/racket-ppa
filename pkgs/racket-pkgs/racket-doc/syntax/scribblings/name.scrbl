#lang scribble/doc
@(require "common.rkt" (for-label syntax/name))

@title[#:tag "name"]{Extracting Inferred Names}

@defmodule[syntax/name]

@defproc[(syntax-local-infer-name [stx syntax?]) any/c]{

Similar to @racket[syntax-local-name] except that @racket[stx] is
checked for an @racket['inferred-name] property (which overrides any
inferred name). If neither @racket[syntax-local-name] nor
@racket['inferred-name] produce a name, or if the
@racket['inferred-name] property value is @|void-const|, then a name
is constructed from the source-location information in @racket[stx],
if any. If no name can be constructed, the result is @racket[#f].}
