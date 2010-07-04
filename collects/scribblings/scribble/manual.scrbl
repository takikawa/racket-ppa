#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-syntax racket/base)
          (for-label scribble/manual-struct))

@(define lit-ellipses (racket ...))
@(define lit-ellipses+ (racket ...+))

@title[#:tag "manual" #:style 'toc]{Manual Forms}

@defmodulelang[scribble/manual]{The @racketmodname[scribble/manual]
language provides all of @racketmodname[scribble/base] plus many
additional functions that are specific to writing Racket
documentation.

The @racketmodname[scribble/manual] name can also be used as a library
with @racket[require], in which case it provides all of the same
bindings, but without setting the reader or setting the default
rendering format to the Racket manual format.}

With @hash-lang[], @racketmodname[scribble/manual] associates a
@racket[latex-defaults] @tech{style property} with its @racket[doc]
export to select the default Racket manual style for Latex
rendering---unless a style is supplied to @racket[title] that already
includes a @racket[latex-defaults] @tech{style property}.

@local-table-of-contents[]

@; ------------------------------------------------------------------------
@section[#:tag "scribble:manual:code"]{Typesetting Code}

@defform[(racketblock datum ...)]{

Typesets the @racket[datum] sequence as a table of Racket code inset
by two spaces. The source locations of the @racket[datum]s determine
the generated layout. For example,

@racketblock[
(racketblock
 (define (loop x)
   (loop (not x))))
]

produces the output

@racketblock[
(define (loop x)
  (loop (not x)))
]

with the @racket[(loop (not x))] indented under @racket[define],
because that's the way it is idented the use of @racket[racketblock].

Furthermore, @racket[define] is typeset as a keyword (bold and black)
and as a hyperlink to @racket[define]'s definition in the reference
manual, because this document was built using a for-label binding of
@racket[define] (in the source) that matches a definition in the
reference manual. Similarly, @racket[not] is a hyperlink to the its
definition in the reference manual.

Use @racket[unsyntax] to escape back to an expression that produces an
@racket[element]. For example,

@let[([unsyntax #f])
(racketblock
 (racketblock
   (+ 1 (unsyntax (elem (racket x) (subscript "2"))))))
]

produces

@racketblock[
(+ 1 (unsyntax (elem (racket x) (subscript "2"))))
]

The @racket[unsyntax] form is regonized via
@racket[free-identifier=?], so if you want to typeset code that
includes @racket[unsyntax], you can simply hide the usual binding:

@RACKETBLOCK[
(racketblock
  (let ([(UNSYNTAX (racket unsyntax)) #f])
    (racketblock
      (syntax (+ 1 (unsyntax x))))))
]

Or use @racket[RACKETBLOCK], whose escape form is @racket[UNSYNTAX]
instead of @racket[unsyntax].

A few other escapes are recognized symbolically:

@itemize[

 @item{@racket[(#,(indexed-racket code:line) _datum ...)] typesets as the
       sequence of @racket[_datum]s (i.e., without the
       @racket[code:line] wrapper).}

 @item{@racket[(#,(indexed-racket code:comment) _datum)] typesets like
       @racket[_datum], but colored as a comment and prefixed with a
       semi-colon. A typical @racket[_datum] escapes from
       Racket-typesetting mode using @racket[unsyntax] and
       produces a paragraph using @racket[t]: 

       @verbatim[#:indent 2]|{
         (code:comment @#,t{this is a comment})
       }|

       (Note that @litchar|{@#,foo{...}}| reads as
       @RACKET[(unsyntax (foo "..."))].)
       }

 @item{@indexed-racket[code:blank] typesets as a blank space.}

 @item{@racket[(#,(indexed-racket code:hilite) _datum)] typesets like
       @racket[_datum], but with a background highlight.}

 @item{@racket[(#,(indexed-racket code:quote) _datum)] typesets like
       @racket[(@#,racketidfont{quote} _datum)], but without rendering the
       @racketidfont{quote} as @litchar{'}.}

 @item{@racketidfont{_}@racket[_id] typesets as @racket[id], but
       colored as a variable (like @racket[racketvarfont]); this
       escape applies only if @racketidfont{_}@racket[_id] has no
       for-label binding and is not specifically colored as a subform
       non-terminal via @racket[defform], a variable via
       @racket[defproc], etc.}

]

See also @racketmodname[scribble/comment-reader].
}

@defform[(RACKETBLOCK datum ...)]{Like @racket[racketblock], but with
the expression escape @racket[UNSYNTAX] instead of @racket[unsyntax].}

@defform[(racketblock0 datum ...)]{Like @racket[racketblock], but
without insetting the code.}

@defform[(RACKETBLOCK0 datum ...)]{Like @racket[RACKETBLOCK], but
without insetting the code.}

@defform[(racketinput datum ...)]{Like @racket[racketblock], but the
@racket[datum] are typeset after a prompt representing a REPL.}

@defform/subs[(racketmod maybe-file lang datum ...)
              ([maybe-file code:blank
                           (code:line #:file filename-expr)])]{

Like @racket[racketblock], but the @racket[datum] are typeset inside a
@racketmodfont{#lang}-form module whose language is @racket[lang].

The source location of @racket[lang] (relative to the body
@racket[datum]s) determines the relative positioning of the
@racketmodfont{#lang} line in the typeset output. So, line up
@racket[lang] with the left end of the content code.

If @racket[#:file] is provided, then the code block is typeset using
@racket[filebox] with @racket[filename-expr] as the filename
argument.}

@defform[(racket datum ...)]{Like @racket[racketblock], but typeset on
a single line and wrapped with its enclosing paragraph, independent of
the formatting of @racket[datum].}

@defform[(RACKET datum ...)]{Like @racket[racket], but with the
@racket[UNSYNTAX] escape like @racket[racketblock].}

@defform[(racketresult datum ...)]{Like @racket[racket], but typeset
as a REPL value (i.e., a single color with no hyperlinks).}

@defform[(racketid datum ...)]{Like @racket[racket], but typeset
as an unbound identifier (i.e., no coloring or hyperlinks).}

@defform*[((racketmodname datum)
           (racketmodname ((unsyntax (racket unsyntax)) expr)))]{
Like @racket[racket], but typeset as a module path. If @racket[datum]
is an identifier or @racket[expr] produces a symbol, then it is
hyperlinked to the module path's definition as created by
@racket[defmodule].}

@defform[(racketmodlink datum pre-content-expr ...)]{
Like @racket[racketmod], but separating the module path to link
from the content to be linked. The @racket[datum] module path is always
linked, even if it is not an identifier.}

@defproc[(litchar [str string?] ...) element?]{Typesets @racket[str]s as a
representation of literal text. Use this when you have to talk about
the individual characters in a stream of text, as as when documenting
a reader extension.}

@defproc[(racketfont [pre-content pre-content?] ...) element?]{Typesets
@tech{decode}d @racket[pre-content] as uncolored, unhyperlinked
Racket. This procedure is useful for typesetting things like
@racketfont{#lang}, which are not @racket[read]able by themselves.}

@defproc[(racketvalfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as a value.}

@defproc[(racketresultfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as a REPL result.}

@defproc[(racketidfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as an identifier.}

@defproc[(racketvarfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as a variable (i.e., an argument or
sub-form in a procedure being documented).}

@defproc[(racketkeywordfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as a syntactic form name.}

@defproc[(racketparenfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored like parentheses.}

@defproc[(racketmetafont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as meta-syntax, such as backquote or
unquote.}

@defproc[(racketerror [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as error-message text.}

@defproc[(racketmodfont [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as module name.}

@defproc[(racketoutput [pre-content pre-content?] ...) element?]{Like
@racket[racketfont], but colored as output.}

@defproc[(procedure [pre-content pre-content?] ...) element?]{Typesets
@tech{decode}d @racket[pre-content] as a procedure name in a REPL
result (e.g., in typewriter font with a @litchar{#<procedure:} prefix
and @litchar{>} suffix.).}

@defform[(var datum)]{Typesets @racket[datum] as an identifier that is
an argument or sub-form in a procedure being documented. Normally, the
@racket[defproc] and @racket[defform] arrange for @racket[racket] to
format such identifiers automatically in the description of the
procedure, but use @racket[var] if that cannot work for some reason.}

@defform[(svar datum)]{Like @racket[var], but for subform non-terminals
in a form definition.}

@deftogether[(
@defform[(schemeblock datum ...)]
@defform[(SCHEMEBLOCK datum ...)]
@defform[(schemeblock0 datum ...)]
@defform[(SCHEMEBLOCK0 datum ...)]
@defform[(schemeinput datum ...)]
@defform[(schememod lang datum ...)]
@defform[(scheme datum ...)]
@defform[(SCHEME datum ...)]
@defform[(schemeresult datum ...)]
@defform[(schemeid datum ...)]
@defform*[((schememodname datum)
           (schememodname ((unsyntax (scheme unsyntax)) expr)))]
@defform[(schememodlink datum pre-content-expr ...)]
@defproc[(schemefont [pre-content pre-content?] ...) element?]
@defproc[(schemevalfont [pre-content pre-content?] ...) element?]
@defproc[(schemeresultfont [pre-content pre-content?] ...) element?]
@defproc[(schemeidfont [pre-content pre-content?] ...) element?]
@defproc[(schemevarfont [pre-content pre-content?] ...) element?]
@defproc[(schemekeywordfont [pre-content pre-content?] ...) element?]
@defproc[(schemeparenfont [pre-content pre-content?] ...) element?]
@defproc[(schememetafont [pre-content pre-content?] ...) element?]
@defproc[(schemeerror [pre-content pre-content?] ...) element?]
@defproc[(schememodfont [pre-content pre-content?] ...) element?]
@defproc[(schemeoutput [pre-content pre-content?] ...) element?]
)]{

Compatibility aliases. Each @racketidfont{scheme...} name is an alias for the
corresponding @racketidfont{racket...} binding.}

@; ------------------------------------------------------------------------

@subsection{Typesetting Comments}

@defmodulereader[scribble/comment-reader]

As a reader module, @racketmodname[scribble/comment-reader] reads a
single S-expression that contains @litchar{;}-based comment lines, and
it wraps the comments with @racket[code:comment] for use with forms
like @racket[racketblock]. More precisely,
@racketmodname[scribble/comment-reader] extends the current reader to
adjust the parsing of @litchar{;}.

For example, within a Scribble document that imports
@racketmodname[scribble/manual],

@verbatim[#:indent 2]|{
  @#reader scribble/comment-reader
   (racketblock
    ;; This is not a pipe
    (make-pipe)
   )
}|

generates

@#reader scribble/comment-reader
 (racketblock
  ;; This is not a pipe
  (make-pipe)
 )

The initial @litchar["@"] is needed above to shift into S-expression
mode, so that @racketmetafont{#reader} is recognized as a reader
declaration instead of literal text. Also, the example uses
@racket[(racketblock ....)]  instead of
@racketmetafont["@"]@racket[racketblock]@racketmetafont["["]@racket[....]@racketmetafont["]"]
because the @"@"-reader would drop comments within the
@racket[racketblock] before giving
@racketmodname[scribble/comment-reader] a chance to convert them.

@; ------------------------------------------------------------------------
@section[#:tag "doc-modules"]{Documenting Modules}

@defform/subs[(defmodule maybe-req id maybe-sources pre-flow ...)
              ([maybe-req code:blank
                          (code:line #:require-form expr)]
               [maybe-sources code:blank
                              (code:line #:use-sources (mod-path ...))])]{

Produces a sequence of flow elements (encaptured in a @racket[splice])
to start the documentation for a module that can be @racket[require]d
using the path @racket[id]. The @tech{decode}d @racket[pre-flow]s
introduce the module, but need not include all of the module content.

Besides generating text, this form expands to a use of
@racket[declare-exporting] with @racket[id]; the
@racket[#:use-sources] clause, if provided, is propagated to
@racket[declare-exporting]. Consequently, @racket[defmodule] should be
used at most once in a section, though it can be shadowed with
@racket[defmodule]s in sub-sections.

If a @racket[#:require-form] clause is provided, the given expression
produces an element to use instead of @racket[(racket require)] for
the declaration of the module. This is useful to suggest a different
way of accessing the module instead of through @racket[require].

Hyperlinks created by @racket[racketmodname] are associated with the
enclosing section, rather than the local @racket[id] text.}


@defform*[[(defmodulelang id maybe-sources pre-flow ...)
           (defmodulelang content-expr #:module-paths (mod-path ...) 
                          maybe-sources pre-flow ...)]]{

Like @racket[defmodule], but documents @racket[id] as a module path
suitable for use by either @racket[require] or @hash-lang[].  If the
module path for @racket[require] is syntactically different from the
@hash-lang[] form, use the @racket[#:module-paths] to provide them
separately.}

@defform[(defmodulereader id maybe-sources pre-flow ...)]{

Like @racket[defmodule], but documents @racket[id] as a module path
suitable for use with @racketmetafont{#reader}.}


@deftogether[(
@defform[(defmodule* maybe-req  (id ...+) maybe-sources pre-flow ...)]
@defform*[[(defmodulelang* (id ...+) maybe-sources pre-flow ...)
           (defmodulelang* (content-expr ...+) #:module-paths (mod-path ...+) 
                           maybe-sources pre-flow ...)]]
@defform[(defmodulereader* (id ...+) maybe-sources pre-flow ...)]
)]{

Like @racket[defmodule], etc., but introduces multiple module paths instead
of just one.}

@deftogether[(
@defform[(defmodule*/no-declare maybe-req (id ...) pre-flow ...)]
@defform*[[(defmodulelang*/no-declare (id ...) pre-flow ...)
           (defmodulelang*/no-declare (content-expr ...) 
                                      #:module-paths (mod-path ...+) pre-flow ...)]]
@defform[(defmodulereader*/no-declare (id ...) pre-flow ...)]
)]{

Like @racket[defmodule*], etc., but without expanding to
@racket[declare-exporting]. Use this form when you want to provide a
more specific list of modules (e.g., to name both a specific module
and one that combines several modules) via your own
@racket[declare-exporting] declaration.}

@defform/subs[(declare-exporting mod-path ... maybe-sources)
              ([maybe-sources code:blank
                              (code:line #:use-sources (mod-path ...))])]{
                                 
Associates the @racket[mod-path]s to all bindings defined within the
enclosing section, except as overridden by other
@racket[declare-exporting] declarations in nested sub-sections.  The
list of @racket[mod-path]s is shown, for example, when the user hovers
the mouse over one of the bindings defined within the section.

More significantly, the first @racket[mod-path] plus the
@racket[#:use-sources] @racket[mod-path]s determine the binding that
is documented by each @racket[defform], @racket[defproc], or similar
form within the section that contains the @racket[declare-exporting]
declaration:

@itemize[

 @item{If no @racket[#:use-sources] clause is supplied, then the
       documentation applies to the given name as exported by the first
       @racket[mod-path].}

 @item{If @racket[#:use-sources] @racket[mod-path]s are supplied, then
       they are tried in order. The first one to provide an export
       with the same symbolic name and
       @racket[free-label-identifier=?] to the given name is used as
       the documented binding. This binding is assumed to be the same
       as the identifier as exported by the first @racket[mod-path] in
       the @racket[declare-exporting] declaration.}

]

The initial @racket[mod-path]s sequence can be empty if
@racket[mod-path]s are given with @racket[#:use-sources]. In that
case, the rendered documentation never reports an exporting module for
identifiers that are documented within the section, but the
@racket[mod-path]s in @racket[#:use-sources] provide a binding context
for connecting (via hyperlinks) definitions and uses of identifiers.

The @racket[declare-exporting] form should be used no more than once
per section, since the declaration applies to the entire section,
although overriding @racket[declare-exporting] forms can appear in
sub-sections.}

@; ------------------------------------------------------------------------
@section[#:tag "doc-forms"]{Documenting Forms, Functions, Structure Types, and Values}

@defform/subs[(defproc prototype
                       result-contract-expr-datum
                       pre-flow ...)
              ([prototype (id arg-spec ...)
                          (prototype arg-spec ...)]
               [arg-spec (arg-id contract-expr-datum)
                         (arg-id contract-expr-datum default-expr)
                         (keyword arg-id contract-expr-datum)
                         (keyword arg-id contract-expr-datum default-expr)
                         ellipses
                         ellipses+]
               [ellipses @#,lit-ellipses]
               [ellipses+ @#,lit-ellipses+])]{

Produces a sequence of flow elements (encapsulated in a
@racket[splice]) to document a procedure named @racket[id]. Nesting
@racket[prototype]s corresponds to a curried function, as in
@racket[define]. The @racket[id] is indexed, and it also registered so
that @racket[racket]-typeset uses of the identifier (with the same
for-label binding) are hyperlinked to this documentation.

A @racket[defmodule] or @racket[declare-exporting] form (or one of the
variants) in an enclosing section determines the @racket[id] binding
that is being defined. The @racket[id] should also have a for-label
binding (as introduced by @racket[(require (for-label ....))]) that
matches the definition binding; otherwise, the defined @racket[id]
will not typeset correctly within the definition.

Each @racket[arg-spec] must have one of the following forms:

@specsubform[(arg-id contract-expr-datum)]{
       An argument whose contract is specified by
       @racket[contract-expr-datum] which is typeset via
       @racket[racketblock0].}

@specsubform[(arg-id contract-expr-datum default-expr)]{
       Like the previous case, but with a default value. All arguments
       with a default value must be grouped together, but they can be
       in the middle of required arguments.}

@specsubform[(keyword arg-id contract-expr-datum)]{
       Like the first case, but for a keyword-based argument.}

@specsubform[(keyword arg-id contract-expr-datum default-expr)]{
       Like the previous case, but with a default
       value.}

@specsubform[@#,lit-ellipses]{Any number of the preceding argument.  This
       form is normally used at the end, but keyword-based arguments
       can sensibly appear afterward. See also the documentation for
       @racket[append] for a use of @lit-ellipses before the last
       argument.}

@specsubform[@#,lit-ellipses+]{One or more of the preceding argument
       (normally at the end, like @lit-ellipses).}

The @racket[result-contract-expr-datum] is typeset via
@racket[racketblock0], and it represents a contract on the procedure's
result.

The @tech{decode}d @racket[pre-flow] documents the procedure. In this
description, references to @svar[arg-id]s using @racket[racket],
@racket[racketblock], @|etc| are typeset as procedure arguments.

The typesetting of all information before the @racket[pre-flow]s
ignores the source layout, except that the local formatting is
preserved for contracts and default-values expressions.}


@defform[(defproc* ([prototype
                     result-contract-expr-datum] ...)
                   pre-flow ...)]{

Like @racket[defproc], but for multiple cases with the same
@racket[id]. 

When an @racket[id] has multiple calling cases, they must be defined
with a single @racket[defproc*], so that a single definition point
exists for the @racket[id]. However, multiple distinct @racket[id]s
can also be defined by a single @racket[defproc*], for the case that
it's best to document a related group of procedures at once.}


@defform/subs[(defform maybe-id maybe-literals form-datum maybe-contracts
                pre-flow ...)
              ([maybe-id code:blank
                         (code:line #:id id)]
               [maybe-literals code:blank
                               (code:line #:literals (literal-id ...))]
               [maybe-contracts code:blank
                                (code:line #:contracts ([subform-datum contract-expr-datum]
                                                        ...))])]{

Produces a sequence of flow elements (encapsulated in a
@racket[splice]) to document a syntatic form named by @racket[id]
whose syntax is described by @racket[form-datum]. If no @racket[#:id] is used
to specify @racket[id], then @racket[form-datum] must have the form
@racket[(id . _datum)].

The @racket[id] is indexed, and it is also registered so that
@racket[racket]-typeset uses of the identifier (with the same
for-label binding) are hyperlinked to this documentation.

The @racket[defmodule] or @racket[declare-exporting] requirements, as
well as the binding requirements for @racket[id], are the same as for
@racket[defproc].

The @tech{decode}d @racket[pre-flow] documents the form. In this
description, a reference to any identifier in @racket[form-datum] via
@racket[racket], @racket[racketblock], @|etc| is typeset as a sub-form
non-terminal. If @racket[#:literals] clause is provided, however,
instances of the @racket[literal-id]s are typeset normally (i.e., as
determined by the enclosing context).

If a @racket[#:contracts] clause is provided, each
@racket[subform-datum] (typically an identifier that serves as a
meta-variable in @racket[form-datum]) is shown as producing a value
that must satisfy the contract described by @racket[contract-expr-datum].

The typesetting of @racket[form-datum], @racket[subform-datum], and
@racket[contract-expr-datum] preserves the source layout, like
@racket[racketblock].}

@defform[(defform* maybe-id maybe-literals [form-datum ...+] maybe-contracts
           pre-flow ...)]{

Like @racket[defform], but for multiple forms using the same
@racket[_id].}

@defform[(defform/subs maybe-id maybe-literals form-datum
           ([nonterm-id clause-datum ...+] ...)
           maybe-contracts
           pre-flow ...)]{

Like @racket[defform], but including an auxiliary grammar of
non-terminals shown with the @racket[_id] form. Each
@racket[nonterm-id] is specified as being any of the corresponding
@racket[clause-datum]s, where the formatting of each
@racket[clause-datum] is preserved.}


@defform[(defform*/subs maybe-id maybe-literals [form-datum ...]
           maybe-contracts
           pre-flow ...)]{

Like @racket[defform/subs], but for multiple forms for @racket[_id].}


@defform[(defform/none maybe-literal form-datum maybe-contracts
           pre-flow ...)]{

Like @racket[defform], but without registering a definition.}


@defform[(defidform id pre-flow ...)]{

Like @racket[defform], but with a plain @racket[id] as the form.}


@defform[(defidform/inline id)]{

Like @racket[defidform], but @racket[id] is typeset as an inline
element. Use this form sparingly, because the typeset form does not
stand out to the reader as a specification of @racket[id].}


@defform[(specform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Like @racket[defform], but without indexing or registering a
definition, and with indenting on the left for both the specification
and the @racket[pre-flow]s.}


@defform[(specsubform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Similar to @racket[defform], but without any specific identifier being
defined, and the table and flow are typeset indented. This form is
intended for use when refining the syntax of a non-terminal used in a
@racket[defform] or other @racket[specsubform]. For example, it is
used in the documentation for @racket[defproc] in the itemization of
possible shapes for @svar[arg-spec].

The @racket[pre-flow]s list is parsed as a flow that documents the
procedure. In this description, a reference to any identifier in
@racket[datum] is typeset as a sub-form non-terminal.}


@defform[(specsubform/subs maybe-literals datum
           ([nonterm-id clause-datum ...+] ...)
           maybe-contracts
           pre-flow ...)]{

Like @racket[specsubform], but with a grammar like
@racket[defform/subs].}


@defform[(specspecsubform maybe-literals datum maybe-contracts
           pre-flow ...)]{

Like @racket[specsubform], but indented an extra level. Since using
@racket[specsubform] within the body of @racket[specsubform] already
nests indentation, @racket[specspecsubform] is for extra indentation
without nesting a description.}


@defform[(specspecsubform/subs maybe-literals datum
          ([nonterm-id clause-datum ...+] ...)
          maybe-contracts
          pre-flow ...)]{

Like @racket[specspecsubform], but with a grammar like
@racket[defform/subs].}


@defform[(defparam id arg-id contract-expr-datum pre-flow ...)]{

Like @racket[defproc], but for a parameter. The
@racket[contract-expr-datum] serves as both the result contract on the
parameter and the contract on values supplied for the parameter. The
@racket[arg-id] refers to the parameter argument in the latter case.}

@defform[(defboolparam id arg-id pre-flow ...)]{

Like @racket[defparam], but the contract on a parameter argument is
@racket[any/c], and the contract on the parameter result is
@racket[boolean?].}


@defform[(defthing id contract-expr-datum pre-flow ...)]{

Like @racket[defproc], but for a non-procedure binding.}

@deftogether[(
@defform[       (defstruct* struct-name ([field-name contract-expr-datum] ...)
                  maybe-mutable maybe-non-opaque maybe-constructor
                  pre-flow ...)]
@defform/subs[  (defstruct struct-name ([field-name contract-expr-datum] ...)
                  maybe-mutable maybe-non-opaque maybe-constructor
                  pre-flow ...)
               ([struct-name id
                             (id super-id)]
                [maybe-mutable code:blank
                               #:mutable]
                [maybe-non-opaque code:blank
                                  #:prefab
                                  #:transparent]
                [maybe-constructor code:blank
                                   (code:line #:constructor-name constructor-id)
                                   (code:line #:extra-constructor-name constructor-id)])]
)]{

Similar to @racket[defform] or @racket[defproc], but for a structure
definition. The @racket[defstruct*] form corresponds to @racket[struct],
while @racket[defstruct] corresponds to @racket[define-struct].}


@defform[(deftogether [def-expr ...] pre-flow ...)]{

Combines the definitions created by the @racket[def-expr]s into a
single definition box. Each @racket[def-expr] should produce a
definition point via @racket[defproc], @racket[defform], etc. Each
@racket[def-expr] should have an empty @racket[pre-flow]; the
@tech{decode}d @racket[pre-flow] sequence for the @racket[deftogether]
form documents the collected bindings.}


@defform/subs[(racketgrammar maybe-literals id clause-datum ...+)
              ([maybe-literals code:blank
                               (code:line #:literals (literal-id ...))])]{
 
Creates a table to define the grammar of @racket[id]. Each identifier
mentioned in a @racket[clause-datum] is typeset as a non-terminal,
except for the identifiers listed as @racket[literal-id]s, which are
typeset as with @racket[racket].}


@defform[(racketgrammar* maybe-literals [id clause-datum ...+] ...)]{

Like @racket[racketgrammar], but for typesetting multiple productions
at once, aligned around the @litchar{=} and @litchar{|}.}

@defproc[(defidentifier [id identifier?]
                        [#:form? form? boolean? #f]
                        [#:index? index? boolean? #t]
                        [#:show-libs? show-libs? boolean? #t])
         element?]{

Typesets @racket[id] as a Racket identifier, and also establishes the
identifier as the definition of a binding in the same way as
@racket[defproc], @racket[defform], etc. As always, the library that
provides the identifier must be declared via @racket[defmodule] or
@racket[declare-exporting] for an enclosing section.

If @racket[form?] is a true value, then the identifier is documented
as a syntactic form, so that uses of the identifier (normally
including @racket[id] itself) are typeset as a syntactic form.

If @racket[index?] is a true value, then the identifier is registered
in the index.

If @racket[show-libs?] is a true value, then the identifier's defining
module may be exposed in the typeset form (e.g., when viewing HTML and
the mouse hovers over the identifier).}

@deftogether[(
@defform[(schemegrammar maybe-literals id clause-datum ...+)]
@defform[(schemegrammar* maybe-literals [id clause-datum ...+] ...)]
)]{

Compatibility aliases for @racket[racketgrammar] and @racket[racketgrammar*].}


@; ------------------------------------------------------------------------
@section[#:tag "doc-classes"]{Documenting Classes and Interfaces}

@defform/subs[(defclass id super (intf-id ...) pre-flow ...)
              ([super super-id
                      (mixin-id super)])]{

Creates documentation for a class @racket[id] that is a subclass of
@racket[super] and implements each interface @racket[intf-id]. Each
identifier in @racket[super] (except @racket[object%]) and
@racket[intf-id] must be documented somewhere via @racket[defclass] or
@racket[definterface].

The decoding of the @racket[pre-flow] sequence should start with
general documentation about the class, followed by constructor
definition (see @racket[defconstructor]), and then field and method
definitions (see @racket[defmethod]). In rendered form, the
constructor and method specification are indented to visually group
them under the class definition.}

@defform[(defclass/title id super (intf-id ...) pre-flow ...)]{

Like @racket[defclass], also includes a @racket[title] declaration
with the style @racket['hidden]. In addition, the constructor and
methods are not left-indented.

This form is normally used to create a section to be rendered on its
own HTML. The @racket['hidden] style is used because the definition
box serves as a title.}

@defform[(definterface id (intf-id ...) pre-flow ...)]{

Like @racket[defclass], but for an interfaces. Naturally,
@racket[pre-flow] should not generate a constructor declaration.}

@defform[(definterface/title id (intf-id ...) pre-flow ...)]{

Like @racket[definterface], but for single-page rendering as in
@racket[defclass/title].}

@defform[(defmixin id (domain-id ...) (range-id ...) pre-flow ...)]{

Like @racket[defclass], but for a mixin. Any number of
@racket[domain-id] classes and interfaces are specified for the
mixin's input requires, and any number of result classes and (more
likely) interfaces are specified for the @racket[range-id]. The
@racket[domain-id]s supply inherited methods.}

@defform[(defmixin/title id (domain-id ...) (range-id ...) pre-flow ...)]{

Like @racket[defmixin], but for single-page rendering as in
@racket[defclass/title].}

@defform/subs[(defconstructor (arg-spec ...) pre-flow ...)
              ([arg-spec (arg-id contract-expr-datum)
                         (arg-id contract-expr-datum default-expr)])]{

Like @racket[defproc], but for a constructor declaration in the body
of @racket[defclass], so no return contract is specified. Also, the
@racket[new]-style keyword for each @racket[arg-spec] is implicit from
the @racket[arg-id].}

@defform[(defconstructor/make (arg-spec ...) pre-flow ...)]{

Like @racket[defconstructor], but specifying by-position
initialization arguments (for use with @racket[make-object]) instead
of by-name arguments (for use with @racket[new]).}

@defform[(defconstructor*/make [(arg-spec ...) ...] pre-flow ...)]{

Like @racket[defconstructor/make], but with multiple constructor
patterns analogous @racket[defproc*].}

@defform[(defconstructor/auto-super [(arg-spec ...) ...] pre-flow ...)]{

Like @racket[defconstructor], but the constructor is
annotated to indicate that additional initialization arguments are
accepted and propagated to the superclass.}

@defform/subs[#:literals (override override-final public-final 
                          augment augment-final pubment extend extend-final)
              (defmethod maybe-mode (id arg-spec ...)
                         result-contract-expr-datum
                         pre-flow ...)
              ([maybe-mode code:blank
                           (code:line #:mode override)
                           (code:line #:mode override-final)
                           (code:line #:mode public-final)
                           (code:line #:mode augment)
                           (code:line #:mode augment-final)
                           (code:line #:mode pubment)
                           (code:line #:mode extend)
                           (code:line #:mode extend-final)])]{

Like @racket[defproc], but for a method within a @racket[defclass] or
@racket[definterface] body.

The @racket[maybe-mode] specifies whether the method overrides a
method from a superclass, and so on. (For these purposes, use
@racket[#:mode override] when refining a method of an implemented
interface.) The @racket[extend] mode is like @racket[override], but
the description of the method should describe only extensions to the
superclass implementation.}

@defform[(defmethod* maybe-mode
                     ([(id arg-spec ...)
                       result-contract-expr-datum] ...)
                     pre-flow ...)]{

Like @racket[defproc*], but for a method within a @racket[defclass] or
@racket[definterface] body. The @racket[maybe-mode] specification is as in
@racket[defmethod].}


@defform[(method class/intf-id method-id)]{

Creates a hyperlink to the method named by @racket[method-id] in the
class or interface named by @racket[class/intf-id]. The hyperlink
names the method, only; see also @racket[xmethod].

For-label binding information is used with @racket[class/intf-id], but
not @racket[method-id].}

@defform[(xmethod class/intf-id method-id)]{

Like @racket[method], but the hyperlink shows both the method name and
the containing class/interface.}

@; ------------------------------------------------------------------------
@section[#:tag "doc-signatures"]{Documenting Signatures}

@defform[(defsignature id (super-id ...) pre-flow ...)]{

Defines a signature @racket[id] that extends the @racket[super-id]
signatures. Any elements defined in @tech{decode}d
@racket[pre-flow]s---including forms, procedures, structure types,
classes, interfaces, and mixins---are defined as members of the
signature instead of direct bindings. These definitions can be
referenced through @racket[sigelem] instead of @racket[racket].

The @tech{decode}d @racket[pre-flow]s inset under the signature
declaration in the typeset output, so no new sections, @|etc| can be
started.}

@defform[(defsignature/splice id (super-id ...) pre-flow ...)]{

Like @racket[defsignature], but the @tech{decode}d @racket[pre-flow]s
are not typeset under the signature declaration, and new sections,
@|etc| can be started in the @racket[pre-flow]s.}

@defproc[(signature-desc [pre-flow pre-flow?] ...) any/c]{

Produces an opaque value that @racket[defsignature] recognizes to
outdent in the typeset form. This is useful for text describing the
signature as a whole to appear right after the signature declaration.}

@defform[(sigelem sig-id id)]{

Typesets the identifier @racket[id] with a hyperlink to its definition
as a member of the signature named by @racket[sig-id].}

@; ------------------------------------------------------------------------
@section[#:tag "doc-strings"]{Various String Forms}

@defproc[(aux-elem [pre-content pre-content?] ...) element?]{
Like @racket[elem], but adds an @racket['aux] @tech{style property}.}

@defproc[(defterm [pre-content pre-content?] ...) element?]{Typesets the
@tech{decode}d @racket[pre-content] as a defined term (e.g., in
italic). Consider using @racket[deftech] instead, though, so that uses
of @racket[tech] can hyper-link to the definition.}

@defproc[(onscreen [pre-content pre-content?] ...) element?]{ Typesets the
@tech{decode}d @racket[pre-content] as a string that appears in a GUI,
such as the name of a button.}

@defproc[(menuitem [menu-name string?] [item-name string?]) element?]{
Typesets the given combination of a GUI's menu and item name.}

@defproc[(filepath [pre-content pre-content?] ...) element?]{Typesets the
@tech{decode}d @racket[pre-content] as a file name (e.g., in
typewriter font and in in quotes).}

@defproc[(exec [pre-content pre-content?] ...) element?]{Typesets the
@tech{decode}d @racket[pre-content] as a command line (e.g., in
typewriter font).}

@defproc[(envvar [pre-content pre-content?] ...) element?]{Typesets the given
@tech{decode}d @racket[pre-content] as an environment variable (e.g.,
in typewriter font).}

@defproc[(Flag [pre-content pre-content?] ...) element?]{Typesets the given
@tech{decode}d @racket[pre-content] as a flag (e.g., in typewriter
font with a leading @litchar{-}).}

@defproc[(DFlag [pre-content pre-content?] ...) element?]{Typesets the given
@tech{decode}d @racket[pre-content] a long flag (e.g., in typewriter
font with two leading @litchar{-}s).}

@defproc[(PFlag [pre-content pre-content?] ...) element?]{Typesets the given
@tech{decode}d @racket[pre-content] as a @litchar{+} flag (e.g., in typewriter
font with a leading @litchar{+}).}

@defproc[(DPFlag [pre-content pre-content?] ...) element?]{Typesets the given
@tech{decode}d @racket[pre-content] a long @litchar{+} flag (e.g., in
typewriter font with two leading @litchar{+}s).}

@; ------------------------------------------------------------------------
@section[#:tag "section-links"]{Links}

See also @secref["base-links"].

@defform[(racketlink id pre-content ...)
         #:contracts ([id identifier?]
                      [pre-content pre-content?])]{

An element where the @tech{decode}d @racket[pre-content] is hyperlinked to the definition
of @racket[id].}

@defform[(schemelink id pre-content ...)]{

Compatibility alias for @racket[racketlink].}

@defproc[(link [url string?] [pre-content any/c] ...
                [#:underline? underline? any/c #t]
                [#:style style (or/c style? string? symbol? #f) (if underline? #f "plainlink")]) 
         element?]{

Alias of @racket[hyperlink] for backward compatibility.}

@defproc[(other-manual [module-path module-path?]
                       [#:underline? underline? any/c #t])
         element?]{

Alias of @racket[other-doc] for backward compatibility.}

@defproc[(deftech [pre-content pre-content?] ...
                  [#:style? style? boolean? #t]) element?]{

Produces an element for the @tech{decode}d @racket[pre-content], and
also defines a term that can be referenced elsewhere using
@racket[tech].

The @racket[content->string] result of the @tech{decode}d
@racket[pre-content] is used as a key for references, but normalized
as follows:

@itemize[

 @item{A trailing ``ies'' is replaced by ``y''.}

 @item{A trailing ``s'' is removed.}

 @item{Consecutive hyphens and whitespaces are all replaced by a
       single space.}

]

These normalization steps help support natural-language references
that differ slightly from a defined form. For example, a definition of
``bananas'' can be referenced with a use of ``banana''.

If @racket[style?] is true, then @racket[defterm] is used on
@racket[pre-content].}

@defproc[(tech [pre-content pre-content?] ...
               [#:doc module-path (or/c module-path? false/c) #f]
               [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f])
         element?]{

Produces an element for the @tech{decode}d @racket[pre-content], and
hyperlinks it to the definition of the content as established by
@racket[deftech]. The content's string form is normalized in the same
way as for @racket[deftech]. The @racket[#:doc] and
@racket[#:tag-prefixes] arguments support cross-document and
section-specific references, like in @racket[secref].

With the default style files, the hyperlink created by @racket[tech]
is somewhat quieter than most hyperlinks: the underline in HTML output
is gray, instead of blue, and the term and underline turn blue only
when the mouse is moved over the term.

In some cases, combining both natural-language uses of a term and
proper linking can require some creativity, even with the
normalization performed on the term. For example, if ``bind'' is
defined, but a sentence uses the term ``binding,'' the latter can be
linked to the former using @racketfont["@tech{bind}ing"].}

@defproc[(techlink [pre-content pre-content?] ...
                   [#:doc module-path (or/c module-path? false/c) #f]
                   [#:tag-prefixes prefixes (or/c (listof string?) false/c) #f]) 
         element?]{

Like @racket[tech], but the link is not a quiet. For example, in HTML
output, a hyperlink underline appears even when the mouse is not over
the link.}

@; ------------------------------------------------------------------------
@section[#:tag "manual-indexing"]{Indexing}

See also @secref["base-indexing"].

@defform[(indexed-racket datum ...)]{

A combination of @racket[racket] and @racket[as-index], with the
following special cases when a single @racket[datum] is provided:

 @itemize[

 @item{If @racket[datum] is a @racket[quote] form, then the quote is
       removed from the key (so that it's sorted using its unquoted
       form).}

 @item{If @racket[datum] is a string, then quotes are removed from the
       key (so that it's sorted using the string content).}

]}

@defform[(indexed-scheme datum ...)]{

Compatibility alias for @racket[indexed-racket].}

@defproc[(idefterm [pre-content pre-content?] ...) element?]{Combines
@racket[as-index] and @racket[defterm]. The content normally should be
plural, rather than singular. Consider using @racket[deftech],
instead, which always indexes.}

@defproc[(pidefterm [pre-content pre-content?] ...) element?]{Like
@racket[idefterm], but plural: adds an ``s'' on the end of the content
for the index entry. Consider using @racket[deftech], instead.}

@defproc[(indexed-file [pre-content pre-content?] ...) element?]{A
combination of @racket[file] and @racket[as-index], but where the sort
key for the index iterm does not include quotes.}

@defproc[(indexed-envvar [pre-content pre-content?] ...) element?]{A
combination of @racket[envvar] and @racket[as-index].}

@; ------------------------------------------------------------------------
@section[#:tag "manual-images"]{Images}

@defproc[(image/plain [filename-relative-to-source string?]
                      [pre-element any/c] ...)
         element?]{

 An alias for @racket[image] for backward compatibility.}

@; ------------------------------------------------------------------------
@section{Bibliography}

@margin-note{See also @racketmodname[scriblib/autobib].}

@defproc[(cite [key string?] ...+) element?]{

Links to a bibliography entry, using the @racket[key]s both to indicate the
bibliography entry and, in square brackets, as the link text.}

@defproc[(bibliography [#:tag tag string? "doc-bibliography"]
                       [entry bib-entry?] ...)
         part?]{

Creates a bibliography part containing the given entries, each of
which is created with @racket[bib-entry]. The entries are typeset in
order as given.}

@defproc[(bib-entry [#:key key string?]
                    [#:title title (or/c false/c pre-content?)]
                    [#:is-book? is-book? boolean? #f]
                    [#:author author (or/c false/c pre-content?) #f]
                    [#:location location (or/c false/c pre-content?) #f]
                    [#:date date (or/c false/c pre-content?) #f] 
                    [#:url url (or/c false/c pre-content?) #f])
         bib-entry?]{

Creates a bibliography entry. The @racket[key] is used to refer to the
entry via @racket[cite]. The other arguments are used as elements in
the entry:

@itemize[

 @item{@racket[title] is the title of the cited work. It will be
       surrounded by quotes in typeset form if @racket[is-book?] is
       @racket[#f], otherwise it is typeset via @racket[italic].}

 @item{@racket[author] lists the authors. Use names in their usual
       order (as opposed to ``last, first''), and separate multiple
       names with commas using ``and'' before the last name (where
       there are multiple names). The @racket[author] is typeset in
       the bibliography as given, or it is omitted if given as
       @racket[#f].}

 @item{@racket[location] names the publication venue, such as a
       conference name or a journal with volume, number, and
       pages. The @racket[location] is typeset in the bibliography as
       given, or it is omitted if given as @racket[#f].}

 @item{@racket[date] is a date, usually just a year (as a string). It
       is typeset in the bibliography as given, or it is omitted if
       given as @racket[#f].}

 @item{@racket[url] is an optional URL. It is typeset in the
       bibliography using @racket[tt] and hyperlinked, or it is
       omitted if given as @racket[#f].}

]}


@defproc[(bib-entry? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a bibliography entry created by
@racket[bib-entry], @racket[#f] otherwise.}


@; ------------------------------------------------------------------------
@section{Miscellaneous}

@defproc[(t [pre-content pre-content?] ...) paragraph?]{Wraps the
@tech{decode}d @racket[pre-content] as a paragraph.}

@defthing[PLaneT element?]{@racket["PLaneT"] (to help make sure you get
the letters in the right case).}

@defproc[(hash-lang) element?]{Returns an element for @hash-lang[]
that is hyperlinked to an explanation.}

@defthing[void-const element?]{Returns an element for @|void-const|.}

@defthing[undefined-const element?]{Returns an element for @|undefined-const|.}

@defproc[(commandline [pre-content pre-content?] ...) paragraph?]{Produces
an inset command-line example (e.g., in typewriter font).}

@defproc[(centerline [pre-flow pre-flow?] ...) nested-flow?]{

An alias for @racket[centered] for backward compatibility.}

@defproc[(math [pre-content any/c] ...) element?]{The @tech{decode}d
@racket[pre-content] is further transformed:

 @itemize[

  @item{Any immediate @racket['rsquo] is converted to @racket['prime].}

  @item{Parentheses and sequences of decimal digits in immediate
        strings are left as-is, but any other immediate string is
        italicized.}

  @item{When @litchar{_} appears before a non-empty sequence of numbers
        and letters, the sequence is typeset as a subscript.}

  @item{When @litchar{^} appears before a non-empty sequence of numbers
        and letters, the sequence is typeset as a superscript.}

 ]}

@defproc[(filebox [filename (or/c string? element?)] [pre-flow pre-flow?] ...)
         block?]{

Typesets the @racket[pre-flow]s as the content of
@racket[filename]. For example, the content may be inset on the page
with @racket[filename] above it. If @racket[filename] is a string, it
is passed to @racket{filepath} to obtain an @racket[element].}

@; ------------------------------------------------------------------------
@section[#:tag "index-entries"]{Index-Entry Descriptions}

@defmodule[scribble/manual-struct]{The
@racketmodname[scribble/manual-struct] library provides types used to
describe index entries created by @racketmodname[scribble/manual]
functions. These structure types are provided separate from
@racketmodname[scribble/manual] so that
@racketmodname[scribble/manual] need not be loaded when deserializing
cross-reference information that was generated by a previously
rendered document.}

@defstruct[module-path-index-desc ()]{

Indicates that the index entry corresponds to a module definition via
@racket[defmodule] and company.}

@defstruct[exported-index-desc ([name symbol?]
                               [from-libs (listof module-path?)])]{

Indicates that the index entry corresponds to the definition of an
exported binding. The @racket[name] field and @racket[from-libs] list
correspond to the documented name of the binding and the primary
modules that export the documented name (but this list is not
exhaustive, because new modules can re-export the binding).}

@defstruct[(form-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
syntactic form via @racket[defform] and company.}

@defstruct[(procedure-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
procedure binding via @racket[defproc] and company.}

@defstruct[(thing-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
binding via @racket[defthing] and company.}

@defstruct[(struct-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
structure type via @racket[defstruct] and company.}

@defstruct[(class-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
class via @racket[defclass] and company.}

@defstruct[(interface-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of an
interface via @racket[definterface] and company.}

@defstruct[(mixin-index-desc exported-index-desc) ()]{

Indicates that the index entry corresponds to the definition of a
mixin via @racket[defmixin] and company.}

@defstruct[(method-index-desc exported-index-desc) ([method-name symbol?]
                                                    [class-tag tag?])]{

Indicates that the index entry corresponds to the definition of an
method via @racket[defmethod] and company. The @racket[_name] field
from @racket[exported-index-desc] names the class or interface that
contains the method. The @racket[method-name] field names the method.
The @racket[class-tag] field provides a pointer to the start of the
documentation for the method's class or interface.}
