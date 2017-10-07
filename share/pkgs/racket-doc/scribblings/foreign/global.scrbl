#lang scribble/doc
@(require "utils.rkt"
          (for-label ffi/unsafe/global))

@title{Process-Wide Registration}

@defmodule[ffi/unsafe/global]{The
@racketmodname[ffi/unsafe/global] library provides a utility
registering information that spans all places in the Racket
process.}

@history[#:added "6.9.0.5"]

@defproc[(register-process-global [key bytes?]
                                  [val cpointer?])
         cpointer?]{

Gets or sets a value in a process-global table (i.e., shared across
multiple places, if any).

If @racket[val] is @racket[#f], the current mapping for @racket[key]
is reported.

If @racket[val] is not @racket[#f], and no value has been installed
for @racket[key], then the value is installed and @racket[#f] is
returned. If a value has already been installed, then no new value is
installed and the old value is returned. The given @racket[val] must
not refer to garbage-collected memory.

This function is intended for infrequent use with a small number of
keys.}