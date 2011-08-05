#lang scribble/doc
@(require "common.rkt" (for-label syntax/zodiac))

@title[#:tag "zodiac"]{Legacy Zodiac Interface}

@defmodule*[(syntax/zodiac syntax/zodiac-unit syntax/zodiac-sig)]

The interface is similar to Zodiac---enough to be useful for
porting---but different in many ways. See the source
@filepath{zodiac-sig.rkt} for details. New software should not use this
compatibility layer.
