#lang scribble/doc
@(require scribble/bnf
          "mz.ss")

@title[#:style 'toc]{Reader Extension}

Scheme's reader can be extended in three ways: through a reader-macro
procedure in a readtable (see @secref["readtables"]), through a
@litchar{#reader} form (see @secref["parse-reader"]), or through a
custom-port byte reader that returns a ``special'' result procedure
(see @secref["customport"]). All three kinds of @deftech{reader
extension procedures} accept similar arguments, and their results are
treated in the same way by @scheme[read] and @scheme[read-syntax] (or,
more precisely, by the default read handler; see
@scheme[port-read-handler]).

@local-table-of-contents[]

@;------------------------------------------------------------------------
@section[#:tag "readtables"]{Readtables}

The dispatch table in @secref["default-readtable-dispatch"]
corresponds to the default @deftech{readtable}. By creating a new
readtable and installing it via the @scheme[current-readtable]
parameter, the reader's behavior can be extended.

A readtable is consulted at specific times by the reader:

@itemize{

 @item{when looking for the start of a datum;}

 @item{when determining how to parse a datum that starts with
   @litchar{#};}

 @item{when looking for a delimiter to terminate a symbol or number;}

 @item{when looking for an opener (such as @litchar{(}), closer (such
   as @litchar{)}), or @litchar{.} after the first character parsed as
   a sequence for a pair, list, vector, or hash table; or}

 @item{when looking for an opener after @litchar{#}@nonterm{n} in a
   vector of specified length @nonterm{n}.}

}

The readtable is ignored at other times.  In particular, after parsing
a character that is mapped to the default behavior of @litchar{;}, the
readtable is ignored until the comment's terminating newline is
discovered. Similarly, the readtable does not affect string parsing
until a closing double-quote is found.  Meanwhile, if a character is
mapped to the default behavior of @litchar{(}, then it starts sequence
that is closed by any character that is mapped to a close parenthesis
@litchar{)}. An apparent exception is that the default parsing of
@litchar{|} quotes a symbol until a matching character is found, but
the parser is simply using the character that started the quote; it
does not consult the readtable.

For many contexts, @scheme[#f] identifies the default readtable. In
particular, @scheme[#f] is the initial value for the
@scheme[current-readtable] parameter, which causes the reader to
behave as described in @secref["reader"].

@defproc[(readtable? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a readtable, @scheme[#f]
otherwise.

}

@defproc[(make-readtable [readtable readtable?]
                         [key (or/c character? false/c)]
                         [mode (or/c (one-of 'terminating-macro
                                             'non-terminating-macro
                                             'dispatch-macro)
                                     character?)]
                         [action (or/c procedure?
                                       readtable?)]
                        ...+)
           readtable?]{

Creates a new readtable that is like @scheme[readtable] (which can be
@scheme[#f]), except that the reader's behavior is modified for each
@scheme[key] according to the given @scheme[mode] and
@scheme[action]. The @scheme[...+] for @scheme[make-readtable] applies
to all three of @scheme[key], @scheme[mode], and @scheme[action]; in
other words, the total number of arguments to @scheme[make-readtable]
must be @math{1} modulo @math{3}.

The possible combinations for @scheme[key], @scheme[mode], and
@scheme[action] are as follows:

@itemize{

 @item{@scheme[(code:line _char (unsyntax @indexed-scheme['terminating-macro]) _proc)] --- causes
 @scheme[_char] to be parsed as a delimiter, and an
 unquoted/uncommented @scheme[_char] in the input string triggers a
 call to the @deftech{reader macro} @scheme[_proc]; the activity of
 @scheme[_proc] is described further below.  Conceptually, characters
 like @litchar{;}, @litchar{(}, and @litchar{)} are mapped to
 terminating reader macros in the default readtable.}

 @item{@scheme[(code:line _char (unsyntax @indexed-scheme['non-terminating-macro]) _proc)] --- like
 the @scheme['terminating-macro] variant, but @scheme[_char] is not
 treated as a delimiter, so it can be used in the middle of an
 identifier or number. Conceptually, @litchar{#} is mapped to a
 non-terminating macro in the default readtable.}

 @item{@scheme[(code:line _char (unsyntax @indexed-scheme['dispatch-macro]) _proc)] --- like the
 @scheme['non-terminating-macro] variant, but for @scheme[_char] only
 when it follows a @litchar{#} (or, more precisely, when the character
 follows one that has been mapped to the behavior of @litchar{#}hash
 in the default readtable).}

 @item{@scheme[(code:line _char _like-char _readtable)] --- causes
 @scheme[_char] to be parsed in the same way that @scheme[_like-char]
 is parsed in @scheme[_readtable], where @scheme[_readtable] can be
 @scheme[#f] to indicate the default readtable. Mapping a character to
 the same actions as @litchar{|} in the default reader means that the
 character starts quoting for symbols, and the same character
 terminates the quote; in contrast, mapping a character to the same
 action as a @litchar{"} means that the character starts a string, but
 the string is still terminated with a closing @litchar{"}. Finally,
 mapping a character to an action in the default readtable means that
 the character's behavior is sensitive to parameters that affect the
 original character; for example, mapping a character to the same
 action is a curly brace @litchar["{"] in the default readtable means
 that the character is disallowed when the
 @scheme[read-curly-brace-as-paren] parameter is set to @scheme[#f].}

 @item{@scheme[(code:line #f (unsyntax @indexed-scheme['non-terminating-macro]) _proc)] ---
 replaces the macro used to parse characters with no specific mapping:
 i.e., characters (other than @litchar{#} or @litchar{|}) that can
 start a symbol or number with the default readtable.}

}

If multiple @scheme['dispatch-macro] mappings are provided for a
single @scheme[_char], all but the last one are ignored. Similarly, if
multiple non-@scheme['dispatch-macro] mappings are provided for a
single @scheme[_char], all but the last one are ignored.

A reader macro @scheme[_proc] must accept six arguments, and it can
optionally accept two arguments. The first two arguments are always
the character that triggered the reader macro and the input port for
reading. When the reader macro is triggered by @scheme[read-syntax]
(or @scheme[read-syntax/recursive]), the procedure is passed four
additional arguments that represent a source location. When the reader
macro is triggered by @scheme[read] (or @scheme[read/recursive]), the
procedure is passed only two arguments if it accepts two arguments,
otherwise it is passed six arguments where the last four are all
@scheme[#f]. See @secref["reader-procs"] for information on the
procedure's results.

A reader macro normally reads characters from the given input port to
produce a value to be used as the ``reader macro-expansion'' of the
consumed characters. The reader macro might produce a special-comment
value (see @secref["special-comments"]) to cause the consumed
character to be treated as whitespace, and it might use
@scheme[read/recursive] or @scheme[read-syntax/recursive].}

@defproc[(readtable-mapping [readtable readtable?][char character?])
         (values (or/c character? 
                       (one-of 'terminating-macro
                               'non-terminating-macro))
                 (or/c false/c procedure?)
                 (or/c false/c procedure?))]{

Produces information about the mappings in @scheme[readtable] for
@scheme[char]. The result is three values:

@itemize{

 @item{either a character (mapping is to same behavior as the
 character in the default readtable), @scheme['terminating-macro], or
 @scheme['non-terminating-macro]; this result reports the main (i.e.,
 non-@scheme['dispatch-macro]) mapping for @scheme[key]. When the result
 is a character, then @scheme[key] is mapped to the same behavior as the
 returned character in the default readtable.}

 @item{either @scheme[#f] or a reader-macro procedure; the result is a
 procedure when the first result is @scheme['terminating-macro] or
 @scheme['non-terminating-macro].}

 @item{either @scheme[#f] or a reader-macro procedure; the result is a
 procedure when the character has a @scheme['dispatch-macro] mapping in
 @scheme[readtable] to override the default dispatch behavior.}

}

Note that reader-macro procedures for the default readtable are not
directly accessible. To invoke default behaviors, use
@scheme[read/recursive] or @scheme[read-syntax/recursive] with a
character and the @scheme[#f] readtable.}

@(begin
#readerscribble/comment-reader
[examples
;; Provides @scheme[raise-read-error] and @scheme[raise-read-eof-error]
(require syntax/readerr)

(define (skip-whitespace port)
  ;; Skips whitespace characters, sensitive to the current
  ;; readtable's definition of whitespace
  (let ([ch (peek-char port)])
    (unless (eof-object? ch)
      ;; Consult current readtable:
      (let-values ([(like-ch/sym proc dispatch-proc) 
                    (readtable-mapping (current-readtable) ch)])
        ;; If like-ch/sym is whitespace, then ch is whitespace
        (when (and (char? like-ch/sym)
                   (char-whitespace? like-ch/sym))
          (read-char port)
          (skip-whitespace port))))))

(define (skip-comments read-one port src)
  ;; Recursive read, but skip comments and detect EOF
  (let loop ()
    (let ([v (read-one)])
      (cond
       [(special-comment? v) (loop)]
       [(eof-object? v)
        (let-values ([(l c p) (port-next-location port)])
          (raise-read-eof-error 
           "unexpected EOF in tuple" src l c p 1))]
       [else v]))))

(define (parse port read-one src)
  ;; First, check for empty tuple
  (skip-whitespace port)
  (if (eq? #\> (peek-char port))
      null
      (let ([elem (read-one)])
        (if (special-comment? elem)
            ;; Found a comment, so look for > again
            (parse port read-one src)
            ;; Non-empty tuple:
            (cons elem
                  (parse-nonempty port read-one src))))))

(define (parse-nonempty port read-one src)
  ;; Need a comma or closer
  (skip-whitespace port)
  (case (peek-char port)
    [(#\>) (read-char port)
     ;; Done
     null]
    [(#\,) (read-char port)
     ;; Read next element and recur
     (cons (skip-comments read-one port src)
           (parse-nonempty port read-one src))]
    [else
     ;; Either a comment or an error; grab location (in case
     ;; of error) and read recursively to detect comments
     (let-values ([(l c p) (port-next-location port)]
                  [(v) (read-one)])
       (cond
        [(special-comment? v)
         ;; It was a comment, so try again
         (parse-nonempty port read-one src)]
        [else
         ;; Wasn't a comment, comma, or closer; error
         ((if (eof-object? v) 
              raise-read-eof-error 
              raise-read-error)
          "expected `,' or `>'" src l c p 1)]))]))

(define (make-delims-table)
  ;; Table to use for recursive reads to disallow delimiters
  ;;  (except those in sub-expressions)
  (letrec ([misplaced-delimiter 
            (case-lambda
             [(ch port) (unexpected-delimiter ch port #f #f #f #f)]
             [(ch port src line col pos)
              (raise-read-error 
               (format "misplaced `~a' in tuple" ch) 
               src line col pos 1)])])
    (make-readtable (current-readtable)
                    #\, 'terminating-macro misplaced-delimiter
                    #\> 'terminating-macro misplaced-delimiter)))

(define (wrap l) 
  `(make-tuple (list ,@l)))

(define parse-open-tuple
  (case-lambda
   [(ch port) 
    ;; `read' mode
    (wrap (parse port 
                 (lambda () 
                   (read/recursive port #f 
                                   (make-delims-table)))
                 (object-name port)))]
   [(ch port src line col pos)
    ;; `read-syntax' mode
    (datum->syntax
     #f
     (wrap (parse port 
                  (lambda () 
                    (read-syntax/recursive src port #f 
                                           (make-delims-table)))
                  src))
     (let-values ([(l c p) (port-next-location port)])
       (list src line col pos (and pos (- p pos)))))]))
    

(define tuple-readtable
  (make-readtable #f #\< 'terminating-macro parse-open-tuple))

(parameterize ([current-readtable tuple-readtable])
  (read (open-input-string "<1 , 2 , \"a\">")))

(parameterize ([current-readtable tuple-readtable])
  (read (open-input-string 
         "< #||# 1 #||# , #||# 2 #||# , #||# \"a\" #||# >")))

(define tuple-readtable+
  (make-readtable tuple-readtable
                  #\* 'terminating-macro (lambda a (make-special-comment #f))
                  #\_ #\space #f))
(parameterize ([current-readtable tuple-readtable+])
  (read (open-input-string "< * 1 __,__  2 __,__ * \"a\" * >")))
])

@;------------------------------------------------------------------------
@section[#:tag "reader-procs"]{Reader-Extension Procedures}

Calls to @techlink{reader extension procedures} can be triggered
through @scheme[read], @scheme[read/recursive], @scheme[read-syntax],
or @scheme[read-honu-syntax]. In addition, a special-read procedure
can be triggered by calls to @scheme[read-honu],
@scheme[read-honu/recursive], @scheme[read-honu-syntax],
@scheme[read-honu-syntax/recursive], @scheme[read-char-or-special], or
by the context of @scheme[read-bytes-avail!],
@scheme[read-bytes-avail!*], @scheme[read-bytes-avail!], and
@scheme[peek-bytes-avail!*].

Optional arities for reader-macro and special-result procedures allow
them to distinguish reads via @scheme[read], @|etc| from reads via
@scheme[read-syntax], @|etc| (where the source value is @scheme[#f] and
no other location information is available).

When a reader-extension procedure is called in syntax-reading mode
(via @scheme[read-syntax], @|etc|), it should generally return a syntax
object that has no lexical context (e.g., a syntax object created
using @scheme[datum->syntax] with @scheme[#f] as the first
argument and with the given location information as the third
argument). Another possible result is a special-comment value (see
@secref["special-comments"]). If the procedure's result is not a
syntax object and not a special-comment value, it is converted to one
using @scheme[datum->syntax].

When a reader-extension procedure is called in non-syntax-reading
modes, it should generally not return a syntax object. If a syntax
object is returned, it is converted to a plain value using
@scheme[syntax-object->datum].

In either context, when the result from a reader-extension procedure
is a special-comment value (see @secref["special-comments"]), then
@scheme[read], @scheme[read-syntax], @|etc| treat the value as a
delimiting comment and otherwise ignore it.

Also, in either context, the result may be copied to prevent mutation
to vectors or boxes before the read result is completed, and to
support the construction of graphs with cycles. Mutable boxes,
vectors, and @tech{prefab} structures are copied, along with any
pairs, boxes, vectors, pre prefab structures that lead to such mutable
values, to placeholders produced by a recursive read (see
@scheme[read/recursive]), or to references of a shared value. Graph
structure (including cycles) is preserved in the copy.

@;------------------------------------------------------------------------
@section[#:tag "special-comments"]{Special Comments}

@defproc[(make-special-comment [v any/c]) special-comment?]{

Creates a special-comment value that encapsulates @scheme[v]. The
@scheme[read], @scheme[read-syntax], @|etc| procedures treat values
constructed with @scheme[make-special-comment] as delimiting
whitespace when returned by a reader-extension procedure (see
@secref["reader-procs"]).}

@defproc[(special-comment? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is the result of
@scheme[make-special-comment], @scheme[#f] otherwise.}

@defproc[(special-comment-value [sc special-comment?]) any]{

Returns the value encapsulated by the special-comment value
@scheme[sc]. This value is never used directly by a reader, but it
might be used by the context of a @scheme[read-char-or-special], @|etc|
call that detects a special comment.}
