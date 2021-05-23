#lang scribble/doc
@(require scribble/bnf "mz.rkt")

@(define (FmtMark . s) (apply litchar "~" s))

@title{Writing}

@defproc[(write [datum any/c] [out output-port? (current-output-port)])
         void?]{

Writes @racket[datum] to @racket[out], normally in such a way that
instances of core datatypes can be read back in. If @racket[out] has a
handler associated to it via @racket[port-write-handler], then the
handler is called. Otherwise, the @seclink["printing"]{default printer} is used (in
@racket[write] mode), as configured by various parameters.

See @secref["printing"] for more information about the default
printer. In particular, note that @racket[write] may require memory
proportional to the depth of the value being printed, due to the
initial cycle check.

@examples[
(write 'hi)
(write (lambda (n) n))
(define o (open-output-string))
(write "hello" o)
(get-output-string o)
]}

@defproc[(display [datum any/c] [out output-port? (current-output-port)])
         void?]{

Displays @racket[datum] to @racket[out], similar to @racket[write],
but usually in such a way that byte- and character-based datatypes are
written as raw bytes or characters. If @racket[out] has a handler
associated to it via @racket[port-display-handler], then the handler
is called. Otherwise, the @seclink["printing"]{default printer} is used (in @racket[display]
mode), as configured by various parameters.

See @secref["printing"] for more information about the default
printer. In particular, note that @racket[display] may require memory
proportional to the depth of the value being printed, due to the
initial cycle check.}

@defproc[(print [datum any/c] [out output-port? (current-output-port)]
                [quote-depth (or/c 0 1) 0])
         void?]{

Prints @racket[datum] to @racket[out]. If @racket[out] has a handler
associated to it via @racket[port-print-handler], then the handler is
called. Otherwise, the handler specified by
@racket[global-port-print-handler] is called; the default handler uses
the @seclink["printing"]{default printer} in @racket[print] mode.

The optional @racket[quote-depth] argument adjusts printing when the
@racket[print-as-expression] parameter is set to @racket[#t]. In that
case, @racket[quote-depth] specifies the starting quote depth for
printing @racket[datum].

The rationale for providing @racket[print] is that @racket[display]
and @racket[write] both have specific output conventions, and those
conventions restrict the ways that an environment can change the
behavior of @racket[display] and @racket[write] procedures. No output
conventions should be assumed for @racket[print], so that environments
are free to modify the actual output generated by @racket[print] in
any way.}

@defproc[(writeln [datum any/c] [out output-port? (current-output-port)])
         void?]{

The same as @racket[(write datum out)] followed by @racket[(newline out)].

@history[#:added "6.1.1.8"]}


@defproc[(displayln [datum any/c] [out output-port? (current-output-port)])
         void?]{

The same as @racket[(display datum out)] followed by @racket[(newline out)],
which is similar to @tt{println} in Pascal or Java.}


@defproc[(println [datum any/c] [out output-port? (current-output-port)]
                  [quote-depth (or/c 0 1) 0])
         void?]{

The same as @racket[(print datum out quote-depth)] followed by
@racket[(newline out)].

The @racket[println] function is not equivalent to @tt{println} in
other languages, because @racket[println] uses printing conventions
that are closer to @racket[write] than to @racket[display]. For a closer
analog to @tt{println} in other languages, use @racket[displayln].

@history[#:added "6.1.1.8"]}


@defproc[(fprintf [out output-port?] [form string?] [v any/c] ...) void?]{

Prints formatted output to @racket[out], where @racket[form] is a string
that is printed directly, except for special formatting
escapes:

@itemize[

  @item{@FmtMark{n} or @FmtMark{%} prints a newline character (which
  is equivalent to @litchar{\n} in a literal format string)}

  @item{@FmtMark{a} or @FmtMark{A} @racket[display]s the next argument
  among the @racket[v]s}

  @item{@FmtMark{s} or @FmtMark{S} @racket[write]s the next argument
  among the @racket[v]s}

  @item{@FmtMark{v} or @FmtMark{V} @racket[print]s the next argument
  among the @racket[v]s}
 
  @item{@FmtMark{.}@nonterm{c} where @nonterm{c} is @litchar{a},
  @litchar{A}, @litchar{s}, @litchar{S}, @litchar{v}, or @litchar{V}:
  truncates default-handler @racket[display], @racket[write], or @racket[print] output
  to @racket[(error-print-width)] characters, using @litchar{...} as
  the last three characters if the untruncated output would be longer}

  @item{@FmtMark{e} or @FmtMark{E} outputs the next argument among the
  @racket[v]s using the current error value conversion handler (see
  @racket[error-value->string-handler]) and current error printing
  width}

  @item{@FmtMark{c} or @FmtMark{C} @racket[write-char]s the
  next argument in @racket[v]s; if the next argument is not a
  character, the @exnraise[exn:fail:contract]}

  @item{@FmtMark{b} or @FmtMark{B} prints the next argument among the
  @racket[v]s in binary; if the next argument is not an exact number, the
  @exnraise[exn:fail:contract]}

  @item{@FmtMark{o} or @FmtMark{O} prints the next argument among the
  @racket[v]s in octal; if the next argument is not an exact number, the
  @exnraise[exn:fail:contract]}

  @item{@FmtMark{x} or @FmtMark{X} prints the next argument among the
  @racket[v]s in hexadecimal; if the next argument is not an exact
  number, the @exnraise[exn:fail:contract]}

  @item{@FmtMark{~} prints a tilde.}

  @item{@FmtMark{}@nonterm{w}, where @nonterm{w} is a whitespace
  character (see @racket[char-whitespace?]), skips characters in
  @racket[form] until a non-whitespace character is encountered or
  until a second end-of-line is encountered (whichever happens
  first). On all platforms, an end-of-line can be @racket[#\return],
  @racket[#\newline], or @racket[#\return] followed immediately by
  @racket[#\newline].}

]

The @racket[form] string must not contain any @litchar{~} that is
not one of the above escapes, otherwise the
@exnraise[exn:fail:contract]. When the format string requires more
@racket[v]s than are supplied, the
@exnraise[exn:fail:contract]. Similarly, when more @racket[v]s are
supplied than are used by the format string, the
@exnraise[exn:fail:contract].

@examples[
(fprintf (current-output-port)
         "~a as a string is ~s.\n"
         '(3 4) 
         "(3 4)")
]}

@defproc[(printf [form string?] [v any/c] ...) void?]{
The same as @racket[(fprintf (current-output-port) form v ...)].}

@defproc[(eprintf [form string?] [v any/c] ...) void?]{
The same as @racket[(fprintf (current-error-port) form v ...)].}

@defproc[(format [form string?] [v any/c] ...) string?]{
Formats to a string. The result is the same as

@racketblock[
(let ([o (open-output-string)])
  (fprintf o form v ...)
  (get-output-string o))
]

@examples[
(format "~a as a string is ~s.\n" '(3 4) "(3 4)")
]}

@defboolparam[print-pair-curly-braces on?]{

A @tech{parameter} that controls pair printing. If the value is true, then
pairs print using @litchar["{"] and @litchar["}"] instead of
@litchar{(} and @litchar{)}. The default is @racket[#f].}


@defboolparam[print-mpair-curly-braces on?]{

A @tech{parameter} that controls pair printing. If the value is true, then
mutable pairs print using @litchar["{"] and @litchar["}"] instead of
@litchar{(} and @litchar{)}. The default is @racket[#t].}

@defboolparam[print-unreadable on?]{

A @tech{parameter} that enables or disables printing of values that have no
@racket[read]able form (using the default reader), including
structures that have a custom-write procedure (see
@racket[prop:custom-write]), but not including @tech{uninterned}
symbols and @tech{unreadable symbols} (which print the same as
@tech{interned} symbols). If the parameter value is @racket[#f], an
attempt to print an unreadable value raises @racket[exn:fail]. The
parameter value defaults to @racket[#t]. See @secref["printing"] for
more information.}

@defboolparam[print-graph on?]{

A @tech{parameter} that controls printing data with sharing; defaults to
@racket[#f]. See @secref["printing"] for more information.}

@defboolparam[print-struct on?]{

A @tech{parameter} that controls printing structure values in vector or
@tech{prefab} form; defaults to @racket[#t]. See @secref["printing"]
for more information. This parameter has no effect on the printing of
structures that have a custom-write procedure (see
@racket[prop:custom-write]).}

@defboolparam[print-box on?]{

A @tech{parameter} that controls printing box values; defaults to
@racket[#t]. See @secref["print-box"] for more information.}

@defboolparam[print-vector-length on?]{

A @tech{parameter} that controls printing vectors; defaults to
@racket[#f]. See @secref["print-vectors"] for more information.}

@defboolparam[print-hash-table on?]{

A @tech{parameter} that controls printing hash tables; defaults to
@racket[#t]. See @secref["print-hashtable"] for more information.}


@defboolparam[print-boolean-long-form on?]{

A @tech{parameter} that controls printing of booleans. When the parameter's
value is true, @racket[#t] and @racket[#f] print as @litchar{#true}
and @litchar{#false}, otherwise they print as @litchar{#t}
and @litchar{#f}. The default is @racket[#f].}


@defboolparam[print-reader-abbreviations on?]{

A @tech{parameter} that controls printing of two-element lists that start
with @racket[quote], @racket['quasiquote], @racket['unquote],
@racket['unquote-splicing], @racket['syntax], @racket['quasisyntax],
@racket['unsyntax], or @racket['unsyntax-splicing]; defaults to
@racket[#f]. See @secref["print-pairs"] for more information.}

@defboolparam[print-as-expression on?]{

A @tech{parameter} that controls printing in @racket[print] mode (as opposed
to @racket[write] or @racket[display]); defaults to @racket[#t]. See
@secref["printing"] for more information.}


@defparam[print-syntax-width width (or/c +inf.0 0 (and/c exact-integer? (>/c 3)))]{

A @tech{parameter} that controls printing of @tech{syntax objects}. Up to
@racket[width] characters are used to show the datum form of a syntax
object within @litchar{#<syntax}...@litchar{>} (after the
@tech{syntax object}'s source location, if any).}

@defparam[print-value-columns columns (or/c +inf.0 (and/c exact-integer? (>/c 5)))]{

A @tech{parameter} that contains a recommendation for the number of
columns that should be used for printing values via @racket[print].
May or may not be respected by @racket[print] - the current default
handler for @racket[print] does not.  It is expected that REPLs that use
some form of pretty-printing for values respect this parameter.

@history[#:added "8.0.0.13"]
}

@defparam*[current-write-relative-directory path 
                                            (or/c (and/c path-string? complete-path?) 
                                                  (cons/c (and/c path-string? complete-path?)
                                                          (and/c path-string? complete-path?))
                                                  #f)
                                            (or/c (and/c path? complete-path?) 
                                                  (cons/c (and/c path? complete-path?) 
                                                          (and/c path? complete-path?))
                                                  #f)]{

A @tech{parameter} that is used when writing compiled code (see @secref["print-compiled"]) that contains
pathname literals, including source-location pathnames for procedure
names. When the parameter's value is a @racket[_path], paths that syntactically extend @racket[_path]
are converted to relative paths; when the resulting
compiled code is read, relative paths are converted back to complete
paths using the @racket[current-load-relative-directory] parameter (if
it is not @racket[#f]; otherwise, the path is left relative).
When the parameter's value is @racket[(cons _rel-to-path _base-path)], then
paths that syntactically extend @racket[_base-path] are converted as relative to @racket[_rel-to-path];
the @racket[_rel-to-path] must extend @racket[_base-path], in which case @racket['up]
path elements (in the sense of @racket[build-path]) may be used to make a path relative
to @racket[_rel-to-path].}



@deftogether[(
@defproc*[([(port-write-handler [out output-port?]) (any/c output-port? . -> . any)]
           [(port-write-handler [out output-port?]
                                [proc (any/c output-port? . -> . any)])
            void?])]
@defproc*[([(port-display-handler [out output-port?]) (any/c output-port? . -> . any)]
           [(port-display-handler [out output-port?]
                                  [proc (any/c output-port? . -> . any)])
            void?])]
@defproc*[([(port-print-handler [out output-port?]) ((any/c output-port?) ((or/c 0 1)) . ->* . any)]
           [(port-print-handler [out output-port?]
                                [proc (any/c output-port? . -> . any)])
            void?])]
)]{

Gets or sets the @deftech{port write handler}, @deftech{port display
handler}, or @deftech{port print handler} for @racket[out]. This
handler is called to output to the port when @racket[write],
@racket[display], or @racket[print] (respectively) is applied to the
port.  Each handler must accept two arguments: the value to be printed and
the destination port. The handler's return value is ignored.

A @tech{port print handler} optionally accepts a third argument, which
corresponds to the optional third argument to @racket[print]; if a
procedure given to @racket[port-print-handler] does not accept a third
argument, it is wrapped with a procedure that discards the optional
third argument.

The default port display and write handlers print Racket expressions
with Racket's built-in printer (see @secref["printing"]). The
default print handler calls the global port print handler (the value
of the @racket[global-port-print-handler] parameter); the default
global port print handler is the same as the default write handler.}

@defproc*[([(global-port-print-handler) (->* (any/c output-port?) ((or/c 0 1)) any)]
           [(global-port-print-handler [proc (or/c (->* (any/c output-port?) ((or/c 0 1)) any)
                                                   (any/c output-port? . -> . any))])
            void?])]{

A @tech{parameter} that determines @deftech{global port print handler},
which is called by the default port print handler (see
@racket[port-print-handler]) to @racket[print] values into a port.
The default value uses the built-in printer (see
@secref["printing"]) in @racket[print] mode.

A @tech{global port print handler} optionally accepts a third
argument, which corresponds to the optional third argument to
@racket[print]. If a procedure given to
@racket[global-port-print-handler] does not accept a third argument,
it is wrapped with a procedure that discards the optional third
argument.}
