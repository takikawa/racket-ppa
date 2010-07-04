#lang scribble/doc
@(require "common.ss"
          scribble/eval
          file/md5
          (for-label file/md5))

@(define md5-eval (make-base-eval))
@interaction-eval[#:eval md5-eval (require file/md5)]

@title[#:tag "md5"]{MD5 Message Digest}

@defmodule[file/md5]

@defproc[(md5 [in (or/c input-port? bytes?)]) bytes?]{

Produces a byte string containing 32 hexadecimal digits (lowercase)
that is the MD5 hash of the given input stream or byte string.

@examples[
#:eval md5-eval
(md5 #"abc")
]}
