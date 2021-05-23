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

