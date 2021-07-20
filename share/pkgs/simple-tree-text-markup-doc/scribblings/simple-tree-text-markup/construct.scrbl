#lang scribble/doc

@(require (for-label (only-in simple-tree-text-markup/data markup? image-markup)
                     (except-in simple-tree-text-markup/construct image-markup)
		     racket) ; srcloc
          scribble/manual)

@title[#:style 'toc #:tag "simple-tree-text-markup-construct"]{Markup Construction}

@defmodule[simple-tree-text-markup/construct]

While the struct definitions in
@racketmodname[simple-tree-text-markup/data] can also be used for
constructing markup, the procedures exported here are somewhat more
convenient to use, and do a fair amount of normalization upon
constructions.

@defproc[(srcloc-markup [srcloc srcloc?] [markup markup?]) markup?]{
This constructs a markup object that will represent a link to a source
location, represented by @racket[srcloc], where the link visualization
is represented by @racket[markup].
}

@defproc[(framed-markup [markup markup?]) markup?]{
This markup constructor puts a frame around @racket[markup].
}

@defthing[empty-markup markup?]{
This is the empty markup object.
}

@defthing[empty-line markup?]{
This is a markup object representing an empty line, i.e. empty
vertical space.
}

@defproc[(number [number number?]
		 [#:exact-prefix exact-prefix (or/c 'always 'never 'when-necessary) 'never]
		 [#:inexact-prefix inexact-prefix (or/c 'always 'never 'when-necessary) 'never]
		 [#:fraction-view fraction-view (or/c #f 'mixed 'improper 'decimal) #f])
  markup?]{
This constructs markup for a number to be rendered in a format that can be read back.

The @racket[exact-prefix] argument specifies whether the representation
should carry a @litchar{#e} prefix: Always, never, or when necessary to
identify a representation that would otherwise be considered inexact.

Similarly for @racket[inexact-prefix].  Note however that @racket['when-necessary]
is usually equivalent to @racket['never], as inexact numbers are always
printed with a decimal dot, which is sufficient to identify a number
representation as inexact.

The @racket[fraction-view] field specifies how exact non-integer reals
- fractions - should be rendered: As a mixed fraction, an improper fraction,
or a decimal, possibly identifying periodic digits.  For @racket['decimal],
if it's not possible to render the number as a decimal exactly, a fraction
representation might be generated. For @racket['mixed] an improper fraction
representation might be generated if a mixed representation could not be read
back.

If @racket[fraction-view] is @racket[#f], this option comes from some
unspecified user preference.
}

@defproc[(horizontal [markup markup?] ...) markup?]{
This procedure arranges the @racket[markup] arguments horizontally.
}

@defproc[(vertical [markup markup?] ...) markup?]{
This procedure arranges the @racket[markup] arguments vertically.
}

@defproc[(markup-transform-image-data [transform-image-data (any/c . -> . any/c)]
				      [markup markup?])
	 markup?]{
This walks over a markup tree, leaving everything unchanged except
@racket[image-markup] values.  For those, it applies
@racket[transform-image-data] to its @racket[data]field, replacing
it by the return value.
}
