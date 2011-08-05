#lang scribble/manual
@(require (for-label scribble/core
                     scribble/decode
                     scriblib/figure
                     scheme/base
                     scheme/contract))

@title[#:tag "figure"]{Figures}

@defmodule[scriblib/figure]

@deftogether[(
@defproc[(figure [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
@defproc[(figure* [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
@defproc[(figure** [tag string?] [caption content?] [pre-flow pre-flow?] ...)
block?]
@defproc[(figure-here [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
)]{

Creates a figure. The given @racket[tag] is for use with
@racket[figure-ref] or @racket[Figure-ref]. The @racket[caption] is an
element. The @racket[pre-flow] is decoded as a flow.

For HTML output, the @racket[figure] and @racket[figure*] functions
center the figure content, while @racket[figure**] allows the content
to be wider than the document body.
For two-column Latex output, @racket[figure*] and @racket[figure**]
generate a figure that spans columns.

For Latex output, @racket[figure-here] generates a figure to be included at
the position in the output text where the @racket[figure-here] occurs
in the source text. For HTML output, all @racket[figure] variants
place the figure where the use appears in the source text.}


@defproc[(figure-ref [tag string?]) element?]{

Generates a reference to a figure, using a lowercase word ``figure''.}


@defproc[(Figure-ref [tag string?]) element?]{

Generates a reference to a figure, capitalizing the word ``Figure''.}


@defproc[(Figure-target [tag string?]) element?]{

Generates a new figure label. This function is normally not used
directly, since it is used by @racket[figure].}
