#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{Reading}

@defproc[(read [in input-port? (current-input-port)]) any]{

Reads and returns a single @tech{datum} from @scheme[in]. If
@scheme[in] has a handler associated to it via
@scheme[port-read-handler], then the handler is called. Otherwise, the
default reader is used, as parameterized by the
@scheme[current-readtable] parameter, as well as many other
parameters.

See @secref["mz:reader"] for information on the default reader.}

@defproc[(read-syntax [source-name any/c (object-name in)]
                      [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{

Like @scheme[read], but produces a @tech{syntax object} with
source-location information. The @scheme[source-name] is used as the
source field of the syntax object; it can be an arbitrary value, but
it should generally be a path for the source file.

See @secref["mz:reader"] for information on the default reader in
@scheme[read-syntax] mode.}

@defproc[(read/recursive [in input-port? (current-input-port)]
                         [start (or/c character? false/c) #f]
                         [readtable readtable? (current-readtable)]
                         [graph? any/c #f])
          any]{

Similar to calling @scheme[read], but normally used during the dynamic
extent of @scheme[read] within a reader-extension procedure (see
@secref["mz:reader-procs"]). The main effect of using
@scheme[read/recursive] instead of @scheme[read] is that
graph-structure annotations (see @secref["mz:parse-graph"]) in the
nested read are considered part of the overall read, at least when the
@scheme[graph?] argument is true; since the result is wrapped in a
placeholder, however, it is not directly inspectable.

If @scheme[start] is provided and not @scheme[#f], it is effectively
prefixed to the beginning of @scheme[in]'s stream for the read. (To
prefix multiple characters, use @scheme[input-port-append].)

The @scheme[readtable] argument is used for top-level parsing to
satisfy the read request; recursive parsing within the read (e.g., to
read the elements of a list) instead uses the current readtable as
determined by the @scheme[current-readtable] parameter.  A reader
macro might call @scheme[read/recursive] with a character and
readtable to effectively invoke the readtable's behavior for the
character.  If @scheme[readtable] is @scheme[#f], the default
readtable is used for top-level parsing.

When @scheme[graph?] is @scheme[#f], graph structure annotations in
the read datum are local to the datum.

When called within the dynamic extent of @scheme[read], the
@scheme[read/recursive] procedure produces either an opaque
placeholder value, a special-comment value, or an end-of-file.  The
result is a special-comment value (see @secref["mz:special-comments"])
when the input stream's first non-whitespace content parses as a
comment. The result is end-of-file when @scheme[read/recursive]
encounters an end-of-file. Otherwise, the result is a placeholder that
protects graph references that are not yet resolved. When this
placeholder is returned within an S-expression that is produced by any
reader-extension procedure (see @secref["mz:reader-procs"]) for the
same outermost @scheme[read], it will be replaced with the actual read
value before the outermost @scheme[read] returns.

See @secref["mz:readtables"] for an extended example that uses
@scheme[read/recursive].}

@defproc[(read-syntax/recursive [source-name any/c (object-name in)]
                                [in input-port? (current-input-port)]
                                [start (or/c character? false/c) #f]
                                [readtable readtable? (current-readtable)]
                                [graph? any/c #f])
          any]{

Analogous to calling @scheme[read/recursive], but the resulting value
encapsulates S-expression structure with source-location
information. As with @scheme[read/recursive], when
@scheme[read-syntax/recursive] is used within the dynamic extent of
@scheme[read-syntax], the result of from
@scheme[read-syntax/recursive] is either a special-comment value,
end-of-file, or opaque graph-structure placeholder (not a syntax
object). The placeholder can be embedded in an S-expression or syntax
object returned by a reader macro, etc., and it will be replaced with
the actual syntax object before the outermost @scheme[read-syntax]
returns.

Using @scheme[read/recursive] within the dynamic extent of
@scheme[read-syntax] does not allow graph structure for reading to be
included in the outer @scheme[read-syntax] parsing, and neither does
using @scheme[read-syntax/recursive] within the dynamic extent of
@scheme[read]. In those cases, @scheme[read/recursive] and
@scheme[read-syntax/recursive] produce results like @scheme[read] and
@scheme[read-syntax], except that a special-comment value is returned
when the input stream starts with a comment (after whitespace).

See @secref["mz:readtables"] for an extended example that uses
@scheme[read-syntax/recursive].}


@defboolparam[read-case-sensitive on?]{

A parameter that controls parsing and printing of symbols. When this
parameter's value is @scheme[#f], the reader case-folds symbols (e.g.,
producing @scheme['hi] when the input is any one of \litchar{hi},
\litchar{Hi}, \litchar{HI}, or \litchar{hI}). The parameter also
affects the way that @scheme[write] prints symbols containing
uppercase characters; if the parameter's value is @scheme[#f], then
symbols are printed with uppercase characters quoted by a
@litchar["\\"] or @litchar["|"]. The parameter's value is overridden by
quoting @litchar["\\"] or @litchar["|"] vertical-bar quotes and the
@litchar{#cs} and @litchar{#ci} prefixes; see
@secref["mz:parse-symbol"] for more information. While a module is
loaded, the parameter is set to @scheme[#t] (see
@scheme[current-load]).}

@defboolparam[read-square-bracket-as-paren on?]{

A parameter that controls whether @litchar["["] and @litchar["]"] 
are treated as parentheses. See @secref["mz:parse-pair"] for more
information.}

@defboolparam[read-curly-brace-as-paren on?]{

A parameter that controls whether @litchar["{"] and @litchar["}"] 
are treated as parentheses. See @secref["mz:parse-pair"] for more
information.}

@defboolparam[read-accept-box on?]{

A parameter that controls parsing @litchar{#&} input. See
@secref["mz:parse-box"] for more information.}

@defboolparam[read-accept-compiled on?]{

A parameter that controls parsing @litchar{#~} compiled input. See
@secref["mz:reader"] and @scheme[current-compile] for more
information.}

@defboolparam[read-accept-bar-quote on?]{

A parameter that controls parsing and printing of @litchar["|"] in
symbols. See @secref["mz:parse-symbol"] and @secref["mz:printing"] for
more information.}

@defboolparam[read-accept-graph on?]{

A parameter value that controls parsing input with sharing. See
@secref["mz:parse-graph"] for more information.}

@defboolparam[read-decimal-as-inexact on?]{

A parameter that controls parsing input numbers with a decimal point
or exponent (but no explicit exactness tag). See
@secref["mz:parse-number"] for more information.}

@defboolparam[read-accept-dot on?]{

A parameter that controls parsing input with a dot, which is normally
used for literal cons cells. See @secref["mz:parse-pair"] for more
information.}

@defboolparam[read-accept-infix-dot on?]{

A parameter that controls parsing input with two dots to trigger infix
 conversion. See @secref["mz:parse-pair"] for more information.}

@defboolparam[read-accept-quasiquote on?]{

A parameter that controls parsing input with @litchar{`} or
@litchar{,} which is normally used for @scheme[quasiquote],
@scheme[unquote], and @scheme[unquote-splicing] abbreviations. See
@secref["mz:parse-quote"] for more information.}

@defboolparam[read-accept-reader on?]{

A parameter that controls whether @litchar{#reader} is allowed for
selecting a parser. See @secref["mz:parse-reader"] for more
information.}

@defparam[current-reader-guard proc (any/c . -> . any)]{

A parameter whose value converts or rejects (by raising an exception)
a module-path datum following @litchar{#reader}. See
@secref["mz:parse-reader"] for more information.}


@defparam[current-readtable readtable (or/c readtable? false/c)]{

A parameter whose value determines a readtable that
adjusts the parsing of S-expression input, where @scheme[#f] implies the
default behavior. See @secref["mz:readtables"] for more information.}

@defproc*[([(port-read-handler [in input-port?]) (case->
                                                  (input-port? . -> . any)
                                                  (input-port?  any/c . -> . any))]
           [(port-read-handler [in input-port?]
                               [proc (case->
                                      (input-port? . -> . any)
                                      (input-port? any/c . -> . any))]) 
            void?])]{

Gets or sets the @deftech{port read handler} for @scheme[in]. The
handler called to read from the port when the built-in @scheme[read]
or @scheme[read-syntax] procedure is applied to the port. (The
port read handler is not used for @scheme[read/recursive] or
@scheme[read-syntax/recursive].)

A port read handler is applied to either one argument or two
arguments:

@itemize{

 @item{A single argument is supplied when the port is used
 with @scheme[read]; the argument is the port being read. The return
 value is the value that was read from the port (or end-of-file).}

 @item{Two arguments are supplied when the port is used with
 @scheme[read-syntax]; the first argument is the port being read, and
 the second argument is a value indicating the source. The return
 value is a syntax object that was read from the port (or end-of-file).}

}

The default port read handler reads standard Scheme expressions with
Scheme's built-in parser (see @secref["mz:reader"]). It handles a
special result from a custom input port (see
@scheme[make-custom-input-port]) by treating it as a single expression,
except that special-comment values (see
@secref["mz:special-comments"]) are treated as whitespace.

The default port read handler itself can be customized through a
readtable; see @secref["mz:readtables"] for more information.}
