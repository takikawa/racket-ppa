#lang scribble/manual
@(require (for-label racket/base
                     syntax-color/color-textoid
                     racket/gui/base
                     framework))

@title[#:tag "color-textoid"]{Interface for Clients of Syntax Color Output}

@defmodule[syntax-color/color-textoid]

@history[#:added "1.3"]

@definterface[color-textoid<%> ()]{

The @racket[color-textoid<%>] interface matches a subset of the
@racket[color:text<%>] interface. It specifies methods that
indentation and expression-navigation functions can use so that they
work either on actual @racket[color:text<%>] objects or in other
environments that use the @xmethod[color:text<%> start-colorer]
protocol without @racketmodname[racket/gui].

@defmethod[(get-text [start exact-nonnegative-integer? 0]
                     [end (or/c exact-nonnegative-integer? 'eof) 'eof])
           string?]{

Like @xmethod[text% get-text].}

@defmethod[(get-character [start exact-nonnegative-integer?])
           char?]{

Like @xmethod[text% get-character].}

@defmethod[(last-position)
           exact-nonnegative-integer?]{

Like @xmethod[text% last-position].}


@defmethod[(position-paragraph [start exact-nonnegative-integer?]
                               [at-eol? any/c #f])
           exact-nonnegative-integer?]{

Like @xmethod[text% position-paragraph].}

@defmethod[(paragraph-start-position [paragraph exact-nonnegative-integer?]
                                     [visible? any/c #t])
           exact-nonnegative-integer?]{

Like @xmethod[text% paragraph-start-position].}

@defmethod[(paragraph-end-position [paragraph exact-nonnegative-integer?]
                                   [visible? any/c #t])
           exact-nonnegative-integer?]{

Like @xmethod[text% paragraph-end-position].}


@defmethod[(skip-whitespace (position exact-nonnegative-integer?)
                            (direction (or/c 'forward 'backward))
                            (comments? boolean?))
           exact-nonnegative-integer?]{

Like @xmethod[color:text<%> skip-whitespace].}

@defmethod[(backward-match [position exact-nonnegative-integer?]
                           [cutoff exact-nonnegative-integer?])
           (or/c exact-nonnegative-integer? #f)]{

Like @xmethod[color:text<%> backward-match].}

@defmethod[(backward-containing-sexp [position exact-nonnegative-integer?]
                                     [cutoff exact-nonnegative-integer?])
           (or/c exact-nonnegative-integer? #f)]{

Like @xmethod[color:text<%> backward-containing-sexp].}

@defmethod[(forward-match [position exact-nonnegative-integer?] [cutoff exact-nonnegative-integer?])
           (or/c exact-nonnegative-integer? #f)]{

Like @xmethod[color:text<%> forward-match].}

@defmethod[(classify-position [position exact-nonnegative-integer?])
           (or/c symbol? #f)]{

Like @xmethod[color:text<%> classify-position].}

@defmethod[(classify-position* [position exact-nonnegative-integer?])
           (or/c (and/c (hash/c symbol? any/c) immutable?) #f)]{

Like @xmethod[color:text<%> classify-position*].}

@defmethod[(get-token-range [position exact-nonnegative-integer?]) 
           (values (or/c #f exact-nonnegative-integer?)
                   (or/c #f exact-nonnegative-integer?))]{

Like @xmethod[color:text<%> get-token-range].}

@defmethod[(get-backward-navigation-limit (start exact-integer?))
           exact-integer?]{

Like @xmethod[color:text<%> get-backward-navigation-limit].}

@defmethod[(get-regions)
           (listof (list/c exact-nonnegative-integer? (or/c exact-nonnegative-integer? 'end)))]{

Like @xmethod[color:text<%> get-regions].}

}
