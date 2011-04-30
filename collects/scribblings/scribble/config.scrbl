#lang scribble/doc
@(require scribble/manual
          scribble/core
          scribble/decode
          scribble/html-properties
          scribble/latex-properties
          "utils.ss"
          (for-label racket/base))

@(define (fake-title . str) (apply bold str))

@title[#:tag "config"]{Extending and Configuring Scribble Output}

Sometimes, Scribble's primitives and built-in styles are insufficient
to produce the output that you need. The cases in which you need to
extend or configure Scribble fall into two groups:

@itemize[

 @item{You may need to drop into the back-end ``language'' of CSS or
       Latex to create a specific output effect. For this kind of
       extension, you will mostly likely attach a
       @racket[css-addition] or @racket[tex-addition] @tech{style property}
       to style, where the addition implements the style name. This
       kind of extension is described in @secref["extra-style"].}

 @item{You may need to produce a document whose page layout is
       different from the Racket documentation style. For that
       kind of configuration, you can run the @exec{scribble} command-line
       tool and supply flags like @DFlag{prefix} or @DPFlag{style}, or
       you can associate a @racket[html-defaults] or
       @racket[latex-defaults] @tech{style property} to the main document's
       style. This kind of configuration is described in
       @secref["config-style"].}

]

@; ------------------------------------------------------------

@section[#:tag "extra-style" 
         #:style (make-style #f (list (make-css-addition "inbox.css")
                                      (make-tex-addition "inbox.tex")))
        ]{Implementing Styles}

When a string is uses as a style in an @racket[element], 
a @racket[multiarg-element], @racket[paragraph], @racket[table],
@racket[itemization], @racket[nested-flow], or
@racket[compound-paragraph], it corresponds to a CSS class for HTML
output or a Latex macro/environment for Latex output. In Latex output,
the string is used as a command name for a @racket[paragraph]
and an environment name for a @racket[table], @racket[itemization],
@racket[nested-flow], or @racket[compound-paragraph]; if the style has
a @racket['command] @tech{style property} for a @racket[nested-flow] or
@racket[compound-paragraph], then the style name is used as a command
instead of an environment; and if the style has
a @racket['multicommand] @tech{style property} for a @racket[nested-flow],
then the style name is used as a command with multiple arguments.
In addition, for an itemization, the style
string is suffixed with @racket["Item"] and used as a CSS class or Latex
macro name to use for the itemization's items (in place of @ltx{item}
in the case of Latex).

To add a mapping from your own style name to a CSS configuration, add
a @racket[css-addition] structure instance to a style's @tech{style property}
list. To map a style name to a Latex macro or environment, add a
@racket[tex-addition] structure instance. A @racket[css-addition] or
@racket[tex-addition] is normally associated with the style whose name
is implemented by the adition, but it can also be added to the style
for an enclosing part.

Scribble includes a number of predefined styles that are used by the
exports of @racket[scribble/base]. You can use them or redefine
them. The styles are specified by @filepath{scribble.css} and
@filepath{scribble.tex} in the @filepath{scribble} collection.

The styles used by @racketmodname[scribble/manual] are implemented by
@filepath{racket.css} and @filepath{racket.tex} in the
@filepath{scribble} collection. Other libraries, such as
@racketmodname[scriblib/autobib], similarly implement styles through files
that are associated by @racket[css-addition] and @racket[tex-addition]
@tech{style properties}.

To avoid collisions with future additions to Scribble, start your
style name with an uppercase letter that is not @litchar{S}. An
uppercase letter helps to avoid collisions with macros defined by
Latex packages, and future styles needed by @racketmodname[scribble/base] and
@racketmodname[scribble/manual] will start with @litchar{S}.

For example, a Scribble document

@verbatim[#:indent 2]|{
 #lang scribble/manual
 @(require scribble/core
           scribble/html-properties
           scribble/latex-properties)

 (define inbox-style
   (make-style "InBox"
               (list (make-css-addition "inbox.css")
                     (make-tex-addition "inbox.tex"))))

 @title{Quantum Pet}

 Do not open: @elem[#:style inbox-style]{Cat}
}|

combined with an @filepath{inbox.css} that contains

@verbatim[#:indent 2]|{
  .inbox {
    padding: 0.2em;
    border: 1px solid #000000;
  }
}|

and an @filepath{inbox.tex} that contains

@verbatim[#:indent 2]|{
  \newcommand{\InBox}[1]{\fbox{#1}}
}|

generates

@nested[#:style 'inset]{
 @fake-title{Quantum Pet}

 Do not open: @elem[#:style "InBox"]{Cat}
}

@; ------------------------------------------------------------

@section[#:tag "config-style"]{Configuring Output}

The implementation of styles used by libraries depends to some degree
on separately configurable parameters, and configuration is also
possible by replacing style implementations. Latex output is more
configurable in the former way, since a document class determines a
set of page-layout and font properties that are used by other
commands. The style-replacement kind of configuration corresponds to
re-defining Latex macros or overriding CSS class attributes.  When
@exec{setup-plt} builds PDF documentation, it uses both kinds of
configuration to produce a standard layout for Racket manuals;
that is, it selects a particular page layout, and it replaces some
@racketmodname[racket/base] styles.

Two kinds of files implement the two kinds of configuration:

@itemize[

 @item{A @deftech{prefix file} determines the @tt{DOCTYPE} line for
       HTML output or the @ltx{documentclass} configuration (and
       perhaps some addition package uses or other configurations) for
       Latex output.

       The default prefix files are @filepath{scribble-prefix.html}
       and @filepath{scribble-prefix.tex} in the @filepath{scribble}
       collection.}

 @item{A @deftech{style file} refines the implementation of styles
       nused in the document---typically just the ``built-in'' styles
       used by @racketmodname[scribble/base].

       The default style files, @filepath{scribble-style.css} and
       @filepath{scribble-style.tex} in the @filepath{scribble}
       collection, change no style implementations.}

]

For a given configuration of output, typically a particular prefix
file works with a particular style file. Some prefix or style files
may be more reusable. For now, reading the default files is the best
way to understand how they interact. A prefix and/or style file may
also require extra accomanying files; for example, a prefix file for
Latex mode may require a corresponding Latex class file. The default
prefix and style files require no extra files.

When rendering a document through the @exec{scribble} command-line
tool, use flags to select a prefix file, style file, and additional
accompanying files:

@itemize[

 @item{Select the prefix file using the @as-index{@DFlag{prefix}}
       flag. (Selecting the prefix file also cancels the default list
       of accompanying files, if any.)}

 @item{Replace the style file using the @as-index{@DFlag{style}}
       flag. Add additional style definitions and re-definitions using
       the @as-index{@DPFlag{style}} flag.}

 @item{Add additional accompanying files with @as-index{@DPFlag{extra}}.}

]

When using the @exec{scribble} command-line utility, a document can
declare its default style, prefix, and extra files through a
@racket[html-defaults] and/or @racket[latex-defaults]
@tech{style property}. In particular, when using the @exec{scribble}
command-line tool to generate Latex or PDF a document whose main part
is implemented with @racket[#, @hash-lang[] #,
@racketmodname[scribble/manual]], the result has the standard
Racket manual configuration, because @racketmodname[scribble/manual]
associates a @racket[latex-defaults] @tech{style property} with the exported
document. The @racketmodname[scribble/sigplan] language similarly
associates a default configuration with an exported document.  As
libraries imported with @racket[require], however,
@racketmodname[scribble/manual] and @racketmodname[scribble/sigplan]
simply implement new styles in a composable way.

Whether or not a document has a default prefix- and style-file
configuration through a @tech{style property}, the defaults can be
overridden using @exec{scribble} command-line flags. Furthermore,
languages like @racketmodname[scribble/manual] and
@racketmodname[scribble/sigplan] add a @racket[html-defaults] and/or
@racket[latex-defaults] @tech{style property} to a main-document part only if
it does not already have such a property added through the
@racket[#:style] argument of @racket[title].

@; ------------------------------------------------------------

@section[#:tag "builtin-latex"]{Predefined Latex Macros}

The @filepath{scribble.tex} Latex configuration includes several
macros and environments that you can redefine to adjust the output
style:

@itemlist[

 @item{@ltxd[0]{preDoc} --- called before the document content; the 
       default does nothing, while the @racketmodname[scribble/manual]
       configuration enabled @ltx{sloppy}.}

 @item{@ltxd[0]{postDoc} --- called after the document content; the 
       default does nothing.}

 @item{@ltxd[0]{sectionNewpage} --- called before each top-level
       section starts; the default does nothing, while the
       @racketmodname[scribble/manual] configuration uses
       @ltx{newpage} to start each chapter on a new page.}

 @item{@ltxd[2]{SecRef} --- the first argument is a section number,
       and the second argument is a section title. This macro is used
       by @racket[secref] to reference a section (other than a
       document or top-level section within a document), and the
       default shows ``section'' followed by the section number
       (ignoring the title). The @racketmodname[scribble/manual]
       redefinition of this macro shows ``§'', the section number, and
       the title in quotes.}

 @item{@ltxd[2]{ChapRef} --- like @ltx{SecRef}, but for a top-level
       section with a document. The default implementation defers to
       @ltx{SecRef}.}

 @item{@ltxd[2]{BookRef} --- like @ltx{SecRef}, but for a document (as
       opposed to a section within the document). The default
       implementation shows the title in italic.}

 @item{@ltxd[2]{SecRefUC} --- like @ltx{SecRef}, but for @racket[Secref].
       The default shows ``Section'' followed by the section number.}

 @item{@ltxd[2]{ChapRefUC} --- like @ltx{SecRefUC}, but for a top-level
       section with a document. The default implementation defers to
       @ltx{SecRefUC}.}

 @item{@ltxd[2]{SecRefUC} --- like @ltx{BookRef}, but for @racket[Secref].
       The default shows defers to @ltx{BookRef}.}

]

