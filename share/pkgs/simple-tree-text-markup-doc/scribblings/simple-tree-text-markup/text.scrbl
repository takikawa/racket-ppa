#lang scribble/doc

@(require (for-label simple-tree-text-markup/data
	  	     simple-tree-text-markup/text
		     racket) ; output-port?
          scribble/manual)

@title[#:style 'toc #:tag "simple-tree-text-markup-text"]{Rendering Markup to Text}

@defmodule[simple-tree-text-markup/text]

This module renders markup to text by printing to a port.

@defproc[(display-markup [markup markup?] [output-port output-port? (current-output-port)]) any]{
Renders a textual version of @racket[markup] to @racket[output-port].
It uses Unicode lines and corners to display framed markup.
}
