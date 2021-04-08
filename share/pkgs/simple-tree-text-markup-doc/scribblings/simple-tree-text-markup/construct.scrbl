#lang scribble/doc

@(require (for-label simple-tree-text-markup/construct)
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

@defproc[(horizontal [markup markup?] ...) markup?]{
This procedure arranges the @racket[markup] arguments horizontally.
}

@defproc[(vertical [markup markup?] ...) markup?]{
This procedure arranges the @racket[markup] arguments vertically.
}
