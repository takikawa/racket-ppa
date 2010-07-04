#lang scribble/doc
@(require "../common.ss")

@title[#:style 'toc]{Aligned Pasteboard}

The aligned-pasteboard library provides classes derived from
@scheme[pasteboard%] with geometry management that mirrors that of
@scheme[vertical-panel%] and @scheme[horizontal-panel%].

@defmodule[mrlib/aligned-pasteboard]

@local-table-of-contents[]

@include-section["aligned-pasteboard-intf.scrbl"]
@include-section["horizontal-pasteboard-class.scrbl"]
@include-section["vertical-pasteboard-class.scrbl"]
@include-section["aligned-editor-snip-class.scrbl"]
@include-section["aligned-editor-canvas-class.scrbl"]
@include-section["aligned-pasteboard-parent-intf.scrbl"]
@include-section["stretchable-snip-intf.scrbl"]
