#lang scribble/doc

@(require (for-label simple-tree-text-markup/data
		     simple-tree-text-markup/port
                     racket) ; output-port?
          scribble/manual)

@title[#:style 'toc #:tag "simple-tree-text-markup-port"]{Generating Markup From a Port}

@defmodule[simple-tree-text-markup/port]

This modules define procedures for creating output ports whose output
is captured as a markup object.

@defproc[(make-markup-output-port [special->markup (any/c . -> . markup?)]) (values output-port? (-> markup?))]{
This procedure returns an output port and a thunk.

The thunk will return whatever has been output to the port as a markup object.

The port also supports @racket[write-special]: Any object output through it will be converted into
markup by the @racket[special->markup] procedure.
}

@defproc[(make-markup-output-port/unsafe [special->markup (any/c . -> . markup?)]) (values output-port? (-> markup?))]{
Thread-unsafe version of @racket[make-markup-output-port].
}

@definterface[srclocs-special<%> ()]{

  This interface is for implementation by objects written via
  @racket[write-special] to a port created by the procedures above: It marks
  objects (typically snips) that represent a sequence of source locations, for
  which the markup output should render a link.

  Note that, in order to make use of this, you will need to
  call @racket[make-markup-output-port] with a @racket[special->markup] argument
  that looks for specials implementing this interface and converts them to
  markup appropriately.
  
  @defmethod*[(((get-srclocs) (or/c #f (listof srcloc?))))]{
    Returns the source locations represented by the special object,
    most relevant first in the list.
  }
}


