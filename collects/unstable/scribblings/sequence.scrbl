#lang scribble/doc
@(require scribble/base
          scribble/manual
          scribble/eval
	  scribblings/reference/mz
	  "utils.ss"
         (for-label unstable/sequence
                    scheme/contract
                    scheme/base))

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/sequence))

@title[#:tag "sequence"]{Sequences}

@defmodule[unstable/sequence]

@unstable[@author+email["Sam Tobin-Hochstadt" "samth@ccs.neu.edu"]]


@defproc[(in-syntax [stx syntax?]) sequence?]{
Produces a sequence equivalent to @scheme[(syntax->list lst)].
@speed[in-syntax "syntax"]

@examples[#:eval the-eval
(for/list ([x (in-syntax #'(1 2 3))])
  x)]}

@defproc[(in-pairs [seq sequence?]) sequence?]{
Produces a sequence equivalent to
 @scheme[(in-parallel (lift car seq) (lift cdr seq))].
}

@defproc[(in-sequence-forever [seq sequence?] [val any/c]) sequence?]{
Produces a sequence whose values are the elements of @scheme[seq], followed by @scheme[val] repeated.
}

@defproc[(sequence-lift [f procedure?] [seq sequence?]) sequence?]{
Produces the sequence of @scheme[f] applied to each element of @scheme[seq].
}