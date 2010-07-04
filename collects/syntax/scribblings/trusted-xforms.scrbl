#lang scribble/doc
@(require "common.ss"
          (for-label syntax/trusted-xforms))

@title[#:tag "trusted-xforms"]{Trusting Standard Recertifying Transformers}

@defmodule[syntax/trusted-xforms]

The @schememodname[syntax/trusted-xforms] library has no exports. It
exists only to require other modules that perform syntax
transformations, where the other transformations must use
@scheme[syntax-recertify]. An application that wishes to provide a
less powerful code inspector to a sub-program should generally attach
@schememodname[syntax/trusted-xforms] to the sub-program's namespace
so that things like the class system from @schememodname[scheme/class]
work properly.
