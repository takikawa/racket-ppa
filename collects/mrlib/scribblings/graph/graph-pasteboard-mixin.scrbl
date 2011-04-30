#lang scribble/doc
@(require "common.ss")

@defmixin/title[graph-pasteboard-mixin (pasteboard%) (graph-pasteboard<%>)]{

@defconstructor/auto-super[([edge-labels? boolean? #t]
                            [edge-label-font (or/c #f (is-a?/c font%)) #f]
                            [cache-arrow-drawing? any])]{

If @scheme[edge-labels?] is @scheme[#f], no edge labels are
drawn. Otherwise, they are.

If @scheme[edge-label-font] is supplied, it is used when drawing the
labels on the edges. Otherwise, the font is not set before drawing
the labels, defaulting to the @scheme[dc<%>] object's font.

If @racket[cache-arrow-drawing?] is @racket[#f], then the arrows in the snip
are not cached in a bitmap (to speed up drawing when the mouse moves around).
Otherwise, they are.

}

This mixin overrides many methods to draw lines between
@scheme[graph-snip<%>] that it contains.}
