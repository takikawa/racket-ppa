#lang scribble/manual
@(require (for-label racket/base
                     racket/class
                     syntax-color/color-textoid
                     syntax-color/racket-indentation
                     syntax-color/racket-navigation
                     framework))

@title[#:tag "racket-nav"]{Racket S-Expression Indentation and Navigation}

The @filepath{syntax-color} collection provides Racket indentation and
navigation functions that take advantage of the token categories and
parenthesis information produced by a coloring lexer. They can work
with any object that implements @racket[color-textoid<%>], which
is extended by @racket[color:text<%>].

@history[#:added "1.3"]

@section{S-Expression Navigation}

@defmodule[syntax-color/racket-navigation]

@deftogether[(
@defproc[(racket-forward-sexp [text (is-a?/c color-textoid<%>)]
                              [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]
@defproc[(racket-backward-sexp [text (is-a?/c color-textoid<%>)]
                               [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]
@defproc[(racket-up-sexp [text (is-a?/c color-textoid<%>)]
                         [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]
@defproc[(racket-down-sexp [text (is-a?/c color-textoid<%>)]
                           [pos exact-nonnegative-integer?])
         (or/c #f exact-nonnegative-integer?)]
)]{

Each of these functions takes a position @racket[pos] within
@racket[text] and returns a position corresponding to S-expression
movement. The result is @racket[#f] if no movement in the
corresponding direction is possible.}

@defproc[(racket-stick-to-next-sexp? [text (is-a?/c color-textoid<%>)]
                                     [pos exact-nonnegative-integer?])
         boolean?]{

Returns whether the content at @racket[pos] in @racket[text]
corresponds to a token that should ``stick'' to the following
parenthesized sequence for navigation purposes. For example, the
result is @racket[#t] when the token corresponds to @litchar["'"],
@litchar["`"], or @litchar["#'"].}


@section{S-Expression Indentation}

@defmodule[syntax-color/racket-indentation]

@defproc[(racket-amount-to-indent [text (is-a?/c color-textoid<%>)]
                                  [pos exact-nonnegative-integer?]
                                  [#:head-sexp-type head-sexp-type
                                                    (string?
                                                     . -> .
                                                     (or/c #f 'lambda 'define 'begin 'for/fold 'other))
                                                    (racket-tabify-table->head-sexp-type
                                                     racket-tabify-default-table)]
                                  [#:graphical-width graphical-width
                                                     (or/c #f ((is-a?/c color-textoid<%>)
                                                               exact-nonnegative-integer?
                                                               exact-nonnegative-integer?
                                                               . -> .
                                                               exact-nonnegative-integer?))])
          (or/c #f exact-nonnegative-integer?)]{

Returns an amount of indentation to use for the line in @racket[text]
that contains the position @racket[pos]. The result may be more or
less than the current amount of indentation on the line.

The @racket[head-sexp-type] function is used to map identifiers at the
start of an S-expression to the indentation rule that the identifier
should use. See @xmethod[racket:text<%> compute-racket-amount-to-indent]
for more information.

The @racket[graphical-width] function is used to
get the graphical width of content in @racket[text] between a start
and end position. If @racket[graphical-width] is @racket[#f], then
characters in @racket[text] are assumed to be all the same width.}

@defproc[(racket-tabify-table->head-sexp-type [spec (list/c (hash/c symbol? (or/c 'lambda 'define 'begin 'for/fold))
                                                            (or/c #f regexp?)
                                                            (or/c #f regexp?)
                                                            (or/c #f regexp?)
                                                            (or/c #f regexp?))])
         (string? . -> . (or/c #f 'lambda 'define 'begin 'for/fold 'other))]{

Converts a serializable representation @racket[spec] of an indentation
configuration to a function suitable for use with
@racket[racket-amount-to-indent].

The first element of @racket[spec] maps individual symbols to
indentation styles. The remaining elements provide patterns to
recognize identifiers with the corresponding style, in the order
@racket['lambda], @racket['define], @racket['begin], and
@racket['for/fold].}

@defthing[racket-tabify-default-table (list/c (hash/c symbol? (or/c 'lambda 'define 'begin 'for/fold))
                                              (or/c #f regexp?)
                                              (or/c #f regexp?)
                                              (or/c #f regexp?)
                                              (or/c #f regexp?))]{

A default configuration suitable as an argument to
@racket[racket-tabify-table->head-sexp-type].}
