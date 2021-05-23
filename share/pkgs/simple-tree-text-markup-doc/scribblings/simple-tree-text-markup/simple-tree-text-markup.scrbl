#lang scribble/doc

@(require scribble/manual)

@title[#:style '(toc)]{Simple Tree Text Markup: Simple Markup for Display as Text or in GUI}

@author["Mike Sperber"]

This is a tree-based combinator library for simple markup, mainly for
displaying messages in a REPL. It features horizontal and vertical
composition as well as framed markup.  Its main distinguishing feature
is its ability to embed source locations, which can be rendered as
links.

This package comes with separate modules for @emph{inspecting} and
@emph{constructing} markup -
@racketmodname[simple-tree-text-markup/data] and
@racketmodname[simple-tree-text-markup/construct], respectively.
Markup can also be constructed through a custom output port,
supplied by @racketmodname[simple-tree-text-markup/port].

There's also a module @racketmodname[simple-tree-text-markup/text]
that renders markup to text.  Rendering markup to GUI is quite
context-specific.  Hence, the code for rendering to GUIs is
implemented with specific applications, such as DrRacket or the test
engine.

@include-section["data.scrbl"]
@include-section["construct.scrbl"]
@include-section["text.scrbl"]
@include-section["port.scrbl"]
