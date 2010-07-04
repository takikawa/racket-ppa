#lang scribble/doc
@(require "common.ss")

@title{@bold{GL}: 3-D Graphics}

The @schememodname[sgl] libraries provide access to the rendering
functions of @as-index{OpenGL} 1.5 and @as-index{GLU} 1.3
libraries. The @schememodname[sgl] libraries to not address
system-level concerns, such as the attachment of GL rendering contexts
to displays. Instead, the libraries should work with any PLT Scheme
extension that provides GL with access to the system (such as a
binding for @tt{glx}). Notably, the @schememodname[scheme/gui/base]
library provides support for rendering contexts via the
@scheme[canvas%] class and its @method[canvas% with-gl-context]
method.

@table-of-contents[]

@include-section["overview.scrbl"]
@include-section["gl.scrbl"]
@include-section["main.scrbl"]
@include-section["gl-vectors.scrbl"]
@include-section["bitmap.scrbl"]

@index-section[]
