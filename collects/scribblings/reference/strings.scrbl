#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "strings"]{Strings}

@guideintro["strings"]{strings}

A @deftech{string} is a fixed-length array of
@seclink["characters"]{characters}.

@index['("strings" "immutable")]{A} string can be @defterm{mutable} or
@defterm{immutable}. When an immutable string is provided to a
procedure like @racket[string-set!], the
@exnraise[exn:fail:contract]. String constants generated by the
default reader (see @secref["parse-string"]) are
immutable, and they are @tech{interned} in @racket[read-syntax] mode.

Two strings are @racket[equal?] when they have the same length and
contain the same sequence of characters.

A string can be used as a single-valued sequence (see
@secref["sequences"]). The characters of the string serve as elements
of the sequence. See also @racket[in-string].

@see-read-print["string"]{strings}

See also: @racket[immutable?], @racket[symbol->string],
@racket[bytes->string/utf-8].

@; ----------------------------------------
@section{String Constructors, Selectors, and Mutators}

@defproc[(string? [v any/c]) boolean?]{ Returns @racket[#t] if @racket[v]
 is a string, @racket[#f] otherwise.

@mz-examples[(string? "Apple") (string? 'apple)]}


@defproc[(make-string [k exact-nonnegative-integer?] [char char?
#\nul]) string?]{ Returns a new mutable string of length @racket[k] where
each position in the string is initialized with the character
@racket[char].

@mz-examples[(make-string 5 #\z)]}


@defproc[(string [char char?] ...) string?]{ Returns a new
mutable string whose length is the number of provided @racket[char]s, and
whose positions are initialized with the given @racket[char]s.

@mz-examples[(string #\A #\p #\p #\l #\e)]}


@defproc[(string->immutable-string [str string?]) (and/c string? immutable?)]{
Returns an immutable string with the same content as
 @racket[str], returning @racket[str] itself if @racket[str] is
 immutable.}


@defproc[(string-length [str string?]) exact-nonnegative-integer?]{
 Returns the length of @racket[str].

@mz-examples[(string-length "Apple")]}


@defproc[(string-ref [str string?] [k exact-nonnegative-integer?])
 char?]{  Returns the character at position @racket[k] in @racket[str].
 The first position in the string corresponds to @racket[0], so the
 position @racket[k] must be less than the length of the string,
 otherwise the @exnraise[exn:fail:contract].

@mz-examples[(string-ref "Apple" 0)]}


@defproc[(string-set! [str (and/c string? (not/c immutable?))] [k
 exact-nonnegative-integer?] [char char?]) void?]{  Changes the
 character position @racket[k] in @racket[str] to @racket[char].  The first
 position in the string corresponds to @racket[0], so the position
 @racket[k] must be less than the length of the string, otherwise the
 @exnraise[exn:fail:contract].

@examples[(define s (string #\A #\p #\p #\l #\e))
          (string-set! s 4 #\y)
          s]}


@defproc[(substring [str string?] 
                    [start exact-nonnegative-integer?]
                    [end exact-nonnegative-integer? (string-length str)]) string?]{
 Returns a new mutable string that is @racket[(- end start)]
 characters long, and that contains the same characters as
 @racket[str] from @racket[start] inclusive to @racket[end] exclusive.
 The first position in a string corresponds to @racket[0], so
 the @racket[start] and @racket[end] arguments so they must be less than or
 equal to the length of @racket[str], and @racket[end] must be greater
 than or equal to @racket[start], otherwise the
 @exnraise[exn:fail:contract].

@mz-examples[(substring "Apple" 1 3)
             (substring "Apple" 1)]}


@defproc[(string-copy [str string?]) string?]{ Returns
 @racket[(substring str 0)].}


@defproc[(string-copy! [dest (and/c string? (not/c immutable?))]
                       [dest-start exact-nonnegative-integer?]
                       [src string?]
                       [src-start exact-nonnegative-integer? 0]
                       [src-end exact-nonnegative-integer? (string-length src)])
         void?]{

 Changes the characters of @racket[dest] starting at position
 @racket[dest-start] to match the characters in @racket[src] from
 @racket[src-start] (inclusive) to @racket[src-end] (exclusive), 
 where the first position in a string corresponds to @racket[0]. The
 strings @racket[dest] and @racket[src] can be the same string, and in
 that case the destination region can overlap with the source region;
 the destination characters after the copy match the source characters
 from before the copy. If any of @racket[dest-start],
 @racket[src-start], or @racket[src-end] are out of range (taking into
 account the sizes of the strings and the source and destination
 regions), the @exnraise[exn:fail:contract].

@mz-examples[(define s (string #\A #\p #\p #\l #\e))
             (string-copy! s 4 "y")
             (string-copy! s 0 s 3 4)
             s]}

@defproc[(string-fill! [dest (and/c string? (not/c immutable?))] [char
 char?]) void?]{ Changes @racket[dest] so that every position in the
 string is filled with @racket[char].

@mz-examples[(define s (string #\A #\p #\p #\l #\e))
             (string-fill! s #\q)
             s]}


@defproc[(string-append [str string?] ...) string?]{

@index['("strings" "concatenate")]{Returns} a new mutable string that is
as long as the sum of the given @racket[str]s' lengths, and that
contains the concatenated characters of the given @racket[str]s. If no
@racket[str]s are provided, the result is a zero-length string.

@mz-examples[(string-append "Apple" "Banana")]}


@defproc[(string->list [str string?]) (listof char?)]{ Returns a new
 list of characters corresponding to the content of @racket[str]. That is,
 the length of the list is @racket[(string-length str)], and the
 sequence of characters in @racket[str] is the same sequence in the
 result list.

@mz-examples[(string->list "Apple")]}


@defproc[(list->string [lst (listof char?)]) string?]{ Returns a new
 mutable string whose content is the list of characters in @racket[lst].
 That is, the length of the string is @racket[(length lst)], and
 the sequence of characters in @racket[lst] is the same sequence in
 the result string.

@mz-examples[(list->string (list #\A #\p #\p #\l #\e))]}


@defproc[(build-string [n exact-nonnegative-integer?]
                       [proc (exact-nonnegative-integer? . -> . char?)])
         string?]{

Creates a string of @racket[n] characters by applying @racket[proc] to
the integers from @racket[0] to @racket[(sub1 n)] in order. If
@racket[_str] is the resulting string, then @racket[(string-ref _str
_i)] is the character produced by @racket[(proc _i)].

@mz-examples[
(build-string 5 (lambda (i) (integer->char (+ i 97))))
]}


@; ----------------------------------------
@section{String Comparisons}


@defproc[(string=? [str1 string?] [str2 string?] ...+) boolean?]{ Returns
 @racket[#t] if all of the arguments are @racket[equal?].}

@mz-examples[(string=? "Apple" "apple")
             (string=? "a" "as" "a")]

@(define (string-sort direction folded?)
(if folded?
  @elem{Like @racket[string-ci<?], but checks whether the arguments would be @direction after case-folding.}
  @elem{Like @racket[string<?], but checks whether the arguments are @|direction|.}))

@defproc[(string<? [str1 string?] [str2 string?] ...+) boolean?]{
 Returns @racket[#t] if the arguments are lexicographically sorted
 increasing, where individual characters are ordered by
 @racket[char<?], @racket[#f] otherwise.

@mz-examples[(string<? "Apple" "apple")
             (string<? "apple" "Apple")
             (string<? "a" "b" "c")]}

@defproc[(string<=? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["nondecreasing" #f]

@mz-examples[(string<=? "Apple" "apple")
             (string<=? "apple" "Apple")
             (string<=? "a" "b" "b")]}

@defproc[(string>? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["decreasing" #f]

@mz-examples[(string>? "Apple" "apple")
             (string>? "apple" "Apple")
             (string>? "c" "b" "a")]}

@defproc[(string>=? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["nonincreasing" #f]

@mz-examples[(string>=? "Apple" "apple")
             (string>=? "apple" "Apple")
             (string>=? "c" "b" "b")]}


@defproc[(string-ci=? [str1 string?] [str2 string?] ...+) boolean?]{
 Returns @racket[#t] if all of the arguments are @racket[eqv?] after
 locale-insensitive case-folding via @racket[string-foldcase].

@mz-examples[(string-ci=? "Apple" "apple")
             (string-ci=? "a" "a" "a")]}

@defproc[(string-ci<? [str1 string?] [str2 string?] ...+) boolean?]{
 Like @racket[string<?], but checks whether the arguments would be in
 increasing order if each was first case-folded using
 @racket[string-foldcase] (which is locale-insensitive).

@mz-examples[(string-ci<? "Apple" "apple")
             (string-ci<? "apple" "banana")
             (string-ci<? "a" "b" "c")]}

@defproc[(string-ci<=? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["nondecreasing" #t]

@mz-examples[(string-ci<=? "Apple" "apple")
             (string-ci<=? "apple" "Apple")
             (string-ci<=? "a" "b" "b")]}

@defproc[(string-ci>? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["decreasing" #t]

@mz-examples[(string-ci>? "Apple" "apple")
             (string-ci>? "banana" "Apple")
             (string-ci>? "c" "b" "a")]}

@defproc[(string-ci>=? [str1 string?] [str2 string?] ...+) boolean?]{
 @string-sort["nonincreasing" #t]

@mz-examples[(string-ci>=? "Apple" "apple")
             (string-ci>=? "apple" "Apple")
             (string-ci>=? "c" "b" "b")]}

@; ----------------------------------------
@section{String Conversions}

@defproc[(string-upcase [str string?]) string?]{ Returns a string
 whose characters are the upcase conversion of the characters in
 @racket[str]. The conversion uses Unicode's locale-independent
 conversion rules that map code-point sequences to code-point
 sequences (instead of simply mapping a 1-to-1 function on code points
 over the string), so the string produced by the conversion can be
 longer than the input string.

@mz-examples[
(string-upcase "abc!")
(string-upcase "Stra\xDFe")
]}

@defproc[(string-downcase [string string?]) string?]{ Like
 @racket[string-upcase], but the downcase conversion.

@mz-examples[
(string-downcase "aBC!")
(string-downcase "Stra\xDFe")
(string-downcase "\u039A\u0391\u039F\u03A3")
(string-downcase "\u03A3")
]}


@defproc[(string-titlecase [string string?]) string?]{ Like
 @racket[string-upcase], but the titlecase conversion only for the
 first character in each sequence of cased characters in @racket[str]
 (ignoring case-ignorable characters).

@mz-examples[
(string-titlecase "aBC  twO")
(string-titlecase "y2k")
(string-titlecase "main stra\xDFe")
(string-titlecase "stra \xDFe")
]}

@defproc[(string-foldcase [string string?]) string?]{ Like
 @racket[string-upcase], but the case-folding conversion.

@mz-examples[
(string-foldcase "aBC!")
(string-foldcase "Stra\xDFe")
(string-foldcase "\u039A\u0391\u039F\u03A3")
]}

@defproc[(string-normalize-nfd [string string?]) string?]{ Returns a
string that is the Unicode normalized form D of @racket[string]. If
the given string is already in the corresponding Unicode normal form,
the string may be returned directly as the result (instead of a newly
allocated string).}

@defproc[(string-normalize-nfkd [string string?]) string?]{ Like
 @racket[string-normalize-nfd], but for normalized form KD.}

@defproc[(string-normalize-nfc [string string?]) string?]{ Like
 @racket[string-normalize-nfd], but for normalized form C.}

@defproc[(string-normalize-nfkc [string string?]) string?]{ Like
 @racket[string-normalize-nfd], but for normalized form KC.}

@; ----------------------------------------
@section{Locale-Specific String Operations}

@defproc[(string-locale=? [str1 string?] [str2 string?] ...+)
 boolean?]{  Like @racket[string=?], but the strings are compared in a
 locale-specific way, based on the value of @racket[current-locale]. See
 @secref["encodings"] for more information on locales.}

@defproc[(string-locale<? [str1 string?] [str2 string?] ...+) boolean?]{
 Like @racket[string<?], but the sort order compares strings in a
 locale-specific way, based on the value of @racket[current-locale]. In
 particular, the sort order may not be simply a lexicographic
 extension of character ordering.}

@defproc[(string-locale>? [str1 string?] [str2 string?] ...+)
 boolean?]{  Like @racket[string>?], but locale-specific like
 @racket[string-locale<?].}

@defproc[(string-locale-ci=? [str1 string?] [str2 string?] ...+)
 boolean?]{  Like @racket[string-locale=?], but strings are compared
 using rules that are both locale-specific and case-insensitive
 (depending on what ``case-insensitive'' means for the current
 locale).}

@defproc[(string-locale-ci<? [str1 string?] [str2 string?] ...+)
 boolean?]{  Like @racket[string<?], but both locale-sensitive and
 case-insensitive like @racket[string-locale-ci=?].}

@defproc[(string-locale-ci>? [str1 string?] [str2 string?] ...+)
 boolean?]{  Like @racket[string>?], but both locale-sensitive and
 case-insensitive like @racket[string-locale-ci=?].}

@defproc[(string-locale-upcase [string string?]) string?]{ Like
 @racket[string-upcase], but using locale-specific case-conversion
 rules based on the value of @racket[current-locale].}

@defproc[(string-locale-downcase [string string?]) string?]{ Like
 @racket[string-downcase], but using locale-specific case-conversion
 rules based on the value of @racket[current-locale].
}

@; ----------------------------------------
@section{Additional String Functions}

@note-lib[racket/string]
@(define string-eval (make-base-eval))
@(interaction-eval #:eval string-eval (require racket/string racket/list))

@defproc[(string-append* [str string?] ... [strs (listof string?)]) string?]{
@; Note: this is exactly the same description as the one for append*

Like @racket[string-append], but the last argument is used as a list
of arguments for @racket[string-append], so @racket[(string-append*
str ... strs)] is the same as @racket[(apply string-append str
... strs)].  In other words, the relationship between
@racket[string-append] and @racket[string-append*] is similar to the
one between @racket[list] and @racket[list*].

@mz-examples[#:eval string-eval
  (string-append* "a" "b" '("c" "d"))
  (string-append* (cdr (append* (map (lambda (x) (list ", " x))
                                     '("Alpha" "Beta" "Gamma")))))
]}

@defproc[(string-join [strs (listof string?)] [sep string?]) string?]{

Appends the strings in @racket[strs], inserting @racket[sep] between
each pair of strings in @racket[strs].

@mz-examples[#:eval string-eval
 (string-join '("one" "two" "three" "four") " potato ")
]}

@close-eval[string-eval]
