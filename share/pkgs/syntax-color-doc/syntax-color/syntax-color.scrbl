#lang scribble/doc
@(require scribble/manual
          (for-label syntax-color/token-tree
                     syntax-color/paren-tree
                     syntax-color/racket-lexer
                     syntax-color/module-lexer
                     syntax-color/scribble-lexer
                     syntax-color/default-lexer
                     syntax-color/lexer-contract
                     framework
                     framework/private/color
                     racket))

@title{Syntax Color: Utilities}

@author["Scott Owens"]

The @filepath{syntax-color} collection provides the underlying data
structures and some helpful utilities for the @racket[color:text<%>]
class of @racketmodname[framework].

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Parenthesis Matching}

@defmodule[syntax-color/paren-tree]

@defclass[paren-tree% object% ()]

Parenthesis matching code built on top of @racket[token-tree%].

@; ----------------------------------------------------------------------

@section[#:tag "dont-stop"]{Lexer Contract and The Don't-Stop Structure Type}

@defmodule[syntax-color/lexer-contract]

@defthing[lexer/c contract?]{
  Checks to be sure a lexing function is well-behaved, constrained to
  functions where the second return value is a symbol. For more
  details, see @xmethod[color:text<%> start-colorer].
}

@defthing[lexer*/c contract?]{
  Checks to be sure a lexing function is well-behaved. For more
  details, see @xmethod[color:text<%> start-colorer].

  It also supplies a few random arguments to the lexer and checks the results,
  using @racket[option/c]'s @racket[#:tester] argument.

 @history[#:added "1.2"]}

@defthing[lexer*/c-without-random-testing contract?]{
 The same contract as @racket[lexer*/c], except without the random testing.
}

@defstruct*[dont-stop ([val any/c])]{
  A structure type used to indicate to the lexer that it should not
  allow itself to be interrupted. For more details,
  see @xmethod[color:text<%> start-colorer].
}

@defproc[(check-colorer-results-match-port-before-and-afters
          [who symbol?]
          [type any/c]
          [pos-before (or/c exact-positive-integer? #f)]
          [new-token-start (or/c exact-positive-integer? #f)]
          [new-token-end (or/c exact-positive-integer? #f)]
          [pos-after (or/c exact-positive-integer? #f)])
         void?]{

 Checks that the results of a colorer make sense with
 respect to the positions of the port, before and after the
 lexer is called.

 The @racket[pos-before] argument is expected to be the third
 result of @racket[port-next-location] before a lexer is
 called and the @racket[pos-after] argument is expected to to
 be the third result of @racket[port-next-location] after the
 lexer is called. The @racket[type], @racket[token-start],
 and @racket[token-end] arguments should be the corresponding
 results from the colorer (c.f.
 @method[color:text<%> start-colorer]).

 This function raises an error unless the following boolean
 expression is true:
 @racketblock[(or (equal? type 'eof)
                  (and (<= pos-before new-token-start pos-after)
                       (<= pos-before new-token-end pos-after)))]
 but it checks the individual parts of the expression to
 raise a more meaningful error message when some part is not
 true.

 The @racket[who] argument is used to start the error message.

 @history[#:added "1.4"]
}

@section{Racket Lexer}

@defmodule[syntax-color/racket-lexer]

@defproc[(racket-lexer [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f))]{

A lexer for Racket, including reader extensions (@secref[#:doc'(lib
"scribblings/reference/reference.scrbl")]{Reader_Extension}), built
specifically for @racket[color:text<%>].

The @racket[racket-lexer] function returns 5 values:

@itemize[
  @item{Either a string containing the matching text or the eof object.  
   Block comments and specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @racket['(error comment sexp-comment 
   white-space constant string no-color parenthesis hash-colon-keyword symbol eof other)].}

  @item{A symbol in @racket['(|(| |)| |[| |]| |{| |}|)] or @racket[#f].}

  @item{A number representing the starting position of the match (or @racket[#f] if eof).}

  @item{A number representing the ending position of the match (or @racket[#f] if eof).}]

}

@defproc[(racket-lexer* [in input-port?]
                        [offset exact-nonnegative-integer?]
                        [mode any/c])
         (values (or/c string? eof-object?) 
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c)]{

Like @racket[racket-lexer], but uses the extended lexer protocol to
track and report regions that are commented out with @litchar{#;}.

@history[#:added "1.2"]}

@defproc[(racket-lexer/status [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[racket-lexer], but returns an extra value. The last
return value indicates whether the consumed token should count as a
datum, an opening parenthesis (or similar starting token to group
other tokens), a closing parenthesis (or similar), or a prefix (such
as whitespace) on a datum.}

@defproc[(racket-lexer*/status [in input-port?]
                               [offset exact-nonnegative-integer?]
                               [mode any/c]) 
         (values (or/c string? eof-object?) 
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[racket-lexer/status], but with comment tracking like
@racket[racket-lexer*].

@history[#:added "1.2"]}

@defproc[(racket-nobar-lexer/status [in input-port?]) 
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[racket-lexer/status], except it treats
@litchar{|} as a delimiter instead of quoting syntax for a symbol.
This function is used by @racket[scribble-lexer].}

@defproc[(racket-nobar-lexer*/status [in input-port?]
                                     [offset exact-nonnegative-integer?]
                                     [mode any/c]) 
         (values (or/c string? eof-object?) 
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c
                 (or/c 'datum 'open 'close 'continue))]{

Like @racket[racket-nobar-lexer/status], but with comment tracking like
@racket[racket-lexer*].

@history[#:added "1.2"]}

@section{Default Lexer}
@defmodule[syntax-color/default-lexer]

@defproc[(default-lexer [in input-port?]) 
         (values (or/c string? eof-object?)
                 symbol? 
                 (or/c symbol? #f) 
                 (or/c number? #f)
                 (or/c number? #f))]

A lexer that only identifies @litchar{(}, @litchar{)}, @litchar{[},
@litchar{]}, @litchar["{"], and @litchar["}"] built specifically for
@racket[color:text<%>].

@racket[default-lexer] returns 5 values:

@itemize[
  @item{Either a string containing the matching text or the eof object.  
   Block specials currently return an empty string.  
   This may change in the future to other string or non-string data.}

  @item{A symbol in @racket['(comment white-space no-color eof)].}

  @item{A symbol in @racket['(|(| |)| |[| |]| |{| |}|)] or @racket[#f].}

  @item{A number representing the starting position of the match (or @racket[#f] if eof).}

  @item{A number representing the ending position of the match (or @racket[#f] if eof).}]


@section{Module Lexer}

@defmodule[syntax-color/module-lexer]

@defproc[(module-lexer [in input-port?]
                       [offset exact-nonnegative-integer?]
                       [mode (or/c #f
                                   (-> input-port? any)
                                   (cons/c (-> input-port? any/c any) any/c))])
         (values (or/c string? eof-object?) 
                 symbol?
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 (or/c #f 
                       (-> input-port? any)
                       (cons/c (-> input-port? any/c any) any/c)))]{

Like @racket[racket-lexer], but with several differences:

@itemize[

 @item{The @racket[module-lexer] function accepts an offset and lexer
       mode, instead of just an input port.}

 @item{In addition to the results of @racket[racket-lexer],
       @racket[module-lexer] returns a backup distance and a new lexer
       mode.}

 @item{When @racket[mode] is @racket[#f] (indicating the start of the
       stream), the lexer checks @racket[in] for a @hash-lang[]
       specification.

       If a @hash-lang[] line is present but the specified
       language does not exist, the entire @racket[in] input is
       consumed and colored as @racket['error].

       If the language exists and the language provides a
       @racket[get-info] function, then it is called with
       @racket['color-lexer]. If the result is not @racket[#f], then
       it should be a lexer function for use with
       @racket[color:text<%>]. The result mode is the lexer---paired
       with @racket[#f] if the lexer is a procedure arity 3---so that
       future calls will dispatch to the language-supplied lexer.

       If the language is specified but it provides no
       @racket[get-info] or @racket['color-lexer] result, then
       @racket[racket-lexer] is returned as the mode.}

 @item{When @racket[mode] is a lexer procedure, the lexer is applied
       to @racket[in]. The lexer's results are returned, plus the
       lexer again as the mode; if the lexer produces a hash-table
       attribute result, however, the @racket['type] value is
       extracted and returned in place of the hash table.}

 @item{When @racket[mode] is a pair, then the lexer procedure in the
       @racket[car] is applied to @racket[in], @racket[offset], and the mode in the
       @racket[cdr]. The lexer's results are returned, except that its
       mode result is paired back with the lexer procedure.}

]}

@defproc[(module-lexer* [in input-port?]
                        [offset exact-nonnegative-integer?]
                        [mode (or/c #f
                                    (-> input-port? any)
                                    (cons/c (-> input-port? any/c any) any/c))])
         (values (or/c string? eof-object?) 
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 (or/c #f 
                       (-> input-port? any)
                       (cons/c (-> input-port? any/c any) any/c)))]{

Like @racket[module-lexer], except that the attribute result
propagated from a language-specific lexer can be a hash table.

@history[#:added "1.2"]}

@section{Scribble Lexer}

@defmodule[syntax-color/scribble-lexer]

@defproc[(scribble-lexer [in input-port?]
                         [offset exact-nonnegative-integer?]
                         [mode any/c])
         (values (or/c string? eof-object?)
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c)]{

Like @racket[racket-lexer*], but for Racket extended with Scribble's
@"@" notation (see @secref[#:doc '(lib
"scribblings/scribble/scribble.scrbl") "reader"]).

@history[#:changed "1.2" @elem{Changed to be like @racket[racket-lexer*]
                               instead of @racket[racket-lexer].}]}

@defproc[(scribble-inside-lexer [in input-port?]
                                [offset exact-nonnegative-integer?]
                                [mode any/c])
         (values (or/c string? eof-object?) 
                 (or/c symbol?
                       (and/c (hash/c symbol? any/c) immutable?))
                 (or/c symbol? #f) 
                 (or/c number? #f) 
                 (or/c number? #f)
                 exact-nonnegative-integer?
                 any/c)]{

Like @racket[scribble-lexer], but starting in ``text'' mode instead of
Racket mode.

@history[#:changed "1.2" @elem{Changed to be like @racket[racket-lexer*]
                               instead of @racket[racket-lexer].}]}

@defproc[(make-scribble-lexer [#:command-char at (and/c char? (not/c (or/c #\] #\[))) #\@])
         lexer/c]{

Produces a lexer like @racket[scribble-lexer], but using
@racket[at] in place of @litchar["@"].

@history[#:added "1.1"
         #:changed "1.2" @elem{Changed like @racket[scribble-lexer].}]}


@defproc[(make-scribble-inside-lexer [#:command-char at (and/c char? (not/c (or/c #\] #\[))) #\@])
         lexer/c]{

Produces a lexer function like @racket[scribble-inside-lexer], but using
@racket[at] in place of @litchar["@"].

@history[#:added "1.1"
         #:changed "1.2" @elem{Changed like @racket[scribble-lexer].}]}

@; ----------------------------------------------------------------------

@section{Splay Tree for Tokenization}
@defmodule[syntax-color/token-tree]

@defclass[token-tree% object% ()]{

A splay-tree class specifically geared for the task of on-the-fly
tokenization. Instead of keying nodes on values, each node has a
length, and they are found by finding a node that follows a certain
total length of preceding nodes.

FIXME: many methods are not yet documented.

 @defconstructor[([len (or/c exact-nonnegative-integer? fasle/c)]
                  [data any/c])]{
  Creates a token tree with a single element.
 }

 @defmethod[(get-root) (or/c node? #f)]{
  Returns the root node in the tree.
 }

 @defmethod[(search! [key-position natural-number/c]) void?]{
  Splays, setting the root node to be the closest node to
  offset @racket[key-position] (i.e., making the total length of
  the left tree at least @racket[key-position], if possible).
 }

}

@deftogether[(
@defproc[(node? [v any/c]) boolean?]
@defproc[(node-token-length [n node?]) natural-number/c]
@defproc[(node-token-data [n node?]) any/c]
@defproc[(node-left-subtree-length [n node?]) natural-number/c]
@defproc[(node-left [n node?]) (or/c node? #f)]
@defproc[(node-right [n node?]) (or/c node? #f)]
)]{

Functions for working with nodes in a @racket[token-tree%].}


@defproc[(insert-first! [tree1 (is-a?/c token-tree%)] 
                        [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @racket[tree1] into @racket[tree2] as the first thing, setting
@racket[tree2]'s root to @racket[#f].}


@defproc[(insert-last! [tree1 (is-a?/c token-tree%)] 
                       [tree2 (is-a?/c token-tree%)]) 
          void?]{

Inserts @racket[tree1] into @racket[tree2] as the last thing, setting
@racket[tree2]'s root to @racket[#f].}


@defproc[(insert-last-spec! [tree (is-a?/c token-tree%)] [n natural-number/c] [v any/c]) void?]{

Same as @racketblock[(insert-last! tree
                                   (new token-tree% 
                                        [length n]
                                        [data v]))]

This optimization is important for the colorer.}

@; ----------------------------------------------------------------------

@include-section["color-textoid.scrbl"]
@include-section["racket.scrbl"]
