#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/bitmap-label))

@title{Bitmap Label}

@defmodule[mrlib/bitmap-label]

@defproc[(make-bitmap-label [str string?]
                            [img (or/c (is-a?/c bitmap%) path-string?)]
                            [font (is-a?/c font%) normal-control-font])
         (is-a?/c bitmap%)]{

Constructs a bitmap label suitable for use a button that contains the
image specified by @scheme[img] followed by the text in @scheme[str].}


@defproc[((bitmap-label-maker [str string?]
                              [img (or/c (is-a?/c bitmap%) path-string?)])
          [future-parent (is-a?/c area-container<%>)])
         (is-a?/c bitmap%)]{

And older variant of @scheme[make-bitmap-label] that obtains a
font to use from a container @scheme[future-parent].}
