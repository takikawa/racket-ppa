#lang scribble/doc
@(require scribble/manual
          "utils.ss"
          (for-label scribble/manual-struct
                     setup/main-collects))

@title[#:tag "core"]{Structures And Processing}

@defmodule[scribble/core]

A document is represented as a @techlink{part}, as described in
 @secref["parts"]. This representation is intended to
 independent of its eventual rendering, and it is intended to be
 immutable; rendering extensions and specific data in a document can
 collude arbitrarily, however.

A document is processed in three passes. The first pass is the
 @deftech{collect pass}, which globally collects information in the
 document, such as targets for hyperlinking. The second pass is the
 @deftech{resolve pass}, which matches hyperlink references with
 targets and expands delayed elements (where the expansion should not
 contribute new hyperlink targets). The final pass is the
 @deftech{render pass}, which generates the resulting document. None
 of the passes mutate the document, but instead collect information in
 side @racket[collect-info] and @racket[resolve-info] tables.

@; ------------------------------------------------------------------------

@section[#:tag "parts"]{Parts}

A @deftech{part} is an instance of @racket[part]; among other things,
 it has a title @techlink{content}, an initial @techlink{flow}, and a
 list of subsection @techlink{parts}.  There is no difference between
 a part and a full document; a particular source module just as easily
 defines a subsection (incorporated via @racket[include-section]) as a
 document.

A @deftech{flow} is a list of @techlink{blocks}.

A @deftech{block} is either a @techlink{table}, an
 @techlink{itemization}, a @techlink{nested flow}, a @techlink{paragraph},
 a @techlink{compound paragraph}, or a @techlink{delayed block}.

@itemize[

       @item{A @deftech{table} is an instance of @racket[table]; it
             has a list of list of @techlink{blocks} corresponding to
             table cells.}

       @item{A @deftech{itemization} is an instance of @racket[itemization];
             it has a list of @techlink{flows}.}

       @item{A @deftech{nested flow} is an instance of
             @racket[nested-flow]; it has a @tech{flow} that
             is typeset as sub-flow.}

       @item{A @deftech{paragraph} is an instance of
             @racket[paragraph]; it has a @tech{content}:

             @itemize[

             @item{An @deftech{content} can be a string, one of a few
                   symbols, an instance of @racket[element] (possibly
                   @racket[link-element], etc.), a @racket[multiarg-element], a
                   @techlink{part-relative element}, a
                   @techlink{delayed element}, or a list of content.

                   @itemize[

                   @item{A string is included in the result
                         document verbatim, except for space, and
                         unless the content's enclosing @tech{style} is
                         @racket['hspace]. In a style other than
                         @racket['hspace], consecutive spaces in the
                         output may be collapsed togther or replaced
                         with a line break. In the style
                         @racket['hspace], all text is converted to
                         uncollapsable spaces that cannot be broken
                         across lines.}

                   @item{A symbol content is either @racket['mdash],
                         @racket['ndash], @racket['ldquo],
                         @racket['lsquo], @racket['rsquo], @racket['larr],
                         @racket['rarr], or @racket['prime]; it is
                         rendered as the corresponding HTML entity
                         (even for Latex output).}

                   @item{An instance of @racket[element] has a
                         @techlink{content} plus a @tech{style}. The style's
                         interpretation depends on the renderer, but it
                         can be one of a few special symbols (such as
                         @racket['bold]) that are recognized by all
                         renderers.}

                   @item{An instance of @racket[link-element] has a
                         @techlink{tag} for the target of the link.}

                   @item{An instance of @racket[target-element] has a
                         @techlink{tag} to be referenced by
                         @racket[link-element]s. An instance of the
                         subtype @racket[toc-target-element] is
                         treated like a kind of section label, to be
                         shown in the ``on this page'' table for HTML
                         output.}

                   @item{An instance of @racket[index-element] has a
                         @techlink{tag} (as a target), a list of
                         strings for the keywords (for sorting and
                         search), and a list of @techlink{contents} to
                         appear in the end-of-document index.}

                   @item{An instance of @racket[image-element]
                         incorporates an image from a file into the rendered
                         document.}

                   @item{An instance of @racket[multiarg-element]
                         combines a style with a list of content,
                         where the style corresponds to a rendered
                         command that takes multiple arguments.}

                   @item{An instance of @racket[collect-element] has a
                         procedure that is called in the
                         @techlink{collect pass} of document
                         processing to record information used by
                         later passes.}

                   @item{A @deftech{part-relative element} is an
                         instance of @racket[part-relative-element],
                         which has a procedure that is called in the
                         @techlink{collect pass} of document
                         processing to obtain @defterm{content}. When the
                         part-relative element's procedure is called,
                         collected information is not yet available,
                         but information about the enclosing parts is
                         available.}

                   @item{A @deftech{delayed element} is an instance of
                         @racket[delayed-element], which has a
                         procedure that is called in the
                         @techlink{resolve pass} of document
                         processing to obtain @defterm{content}.}

                   @item{An instance of @racket[render-element] has a
                         procedure that is called in the
                         @techlink{render pass} of document
                         processing.}

             ]}]}

       @item{A @deftech{compound paragraph} is an instance of
             @racket[compound-paragraph]; like @racket[blockquote], it
             has list of @tech{blocks}, but the blocks are typeset as
             a single paragraph (e.g., no indentation after the first
             block) instead of inset.}

       @item{A @deftech{delayed block} is an instance of
             @racket[delayed-block], which has a procedure that
             is called in the @techlink{resolve pass} of document
             processing to obtain a @defterm{block}.}

]

@; ------------------------------------------------------------------------

@section[#:tag "tags"]{Tags}

A @deftech{tag} is a list containing a symbol and either a string, a
@racket[generated-tag] instance, or an arbitrary list. The symbol
effectively identifies the type of the tag, such as @racket['part] for
a tag that links to a part, or @racket['def] for a Racket function
definition. The symbol also effectively determines the interpretation
of the second half of the tag.

A part can have a @deftech{tag prefix}, which is effectively added
onto the second item within each tag whose first item is
@racket['part] or @racket['tech]. The prefix is added to a string
value by creating a list containing the prefix and string, and it is
added to a list value using @racket[cons]; a prefix is not added to a
@racket[generated-tag] instance. The prefix is used for reference
outside the part, including the use of tags in the part's
@racket[tags] field. Typically, a document's main part has a tag
prefix that applies to the whole document; references to sections and
defined terms within the document from other documents must include the prefix,
while references within the same document omit the prefix. Part
prefixes can be used within a document as well, to help disambiguate
references within the document.

Some procedures accept a ``tag'' that is just the string part of the
full tag, where the symbol part is supplied automatically. For
example, @racket[section] and @racket[secref] both accept a string
``tag'', where @racket['part] is implicit.

@; ------------------------------------------------------------------------

@section[#:tag "style"]{Styles}

A @deftech{style} combines a @tech{style name} with a list of
@tech{style properties} in a @racket[style] structure. A @deftech{style name}
is either a string, symbol, of @racket[#f]. A @deftech{style property} can be
anything, including a symbol a structure such as
@racket[color-property].

A style has a single @tech{style name}, because the name typically
corresponds to a configurable instruction to a renderer. For example,
with Latex output, a string style name corresponds to a Latex command
or environment. For more information on how string style names
interact with configuration of a renderer, see
@secref["config"]. Symbolic style names, meanwhile, provide a simple
layer of abstraction between the renderer and documents for widely
supported style; for example, the @racket['italic] style name is
supported by all renderers.

@tech{Style properties} within a style compose with style names and other
properties. Again, symbols are often used for properties that are directly
supported by renderers. For example, @racket['unnumbered] style
property for a @tech{part} renders the part without a section number.
Many properties are renderer-specific, such as a @racket[hover-property]
structure that associates text with an element to be shown in an
HTML display when the mouse hovers over the text.

@; ------------------------------------------------------------------------

@section[#:tag "passes"]{Collected and Resolved Information}

The @techlink{collect pass}, @techlink{resolve pass}, and
@techlink{render pass} processing steps all produce information that
is specific to a rendering mode. Concretely, the operations are all
represented as methods on a @racket[render%] object.

The result of the @method[render% collect] method is a
@racket[collect-info] instance. This result is provided back as an
argument to the @method[render% resolve] method, which produces a
@racket[resolve-info] value that encapsulates the results from both
iterations. The @racket[resolve-info] value is provided back to the
@method[render% resolve] method for final rendering.

Optionally, before the @method[render% resolve] method is called,
serialized information from other documents can be folded into the
@racket[collect-info] instance via the @method[render%
deserialize-info] method. Other methods provide serialized information
out of the collected and resolved records.

During the @techlink{collect pass}, the procedure associated with a
@racket[collect-element] instance can register information with
@racket[collect-put!].

During the @techlink{resolve pass}, collected information for a part
can be extracted with @racket[part-collected-info], which includes a
part's number and its parent part (or @racket[#f]). More generally,
the @racket[resolve-get] method looks up information previously
collected. This resolve-time information is normally obtained by the
procedure associated with a @techlink{delayed block} or
@techlink{delayed element}.

The @racket[resolve-get] information accepts both a @racket[part] and
a @racket[resolve-info] argument. The @racket[part] argument enables
searching for information in each enclosing part before sibling parts.

@; ------------------------------------------------------------------------

@section{Structure Reference}

@defstruct[part ([tag-prefix (or/c false/c string?)]
                 [tags (listof tag?)]
                 [title-content (or/c false/c list?)]
                 [style style?]
                 [to-collect list?]
                 [blocks (listof block?)]
                 [parts (listof part?)])]{

The @racket[tag-prefix] field determines the optional @techlink{tag
prefix} for the part.

The @racket[tags] indicates a list of @techlink{tags} that each link
to the section.

The @racket[title-content] field holds the part's title, if any.

For the @racket[style] field, the currently recognized symbolic style
names are as follows:

@itemize[

 @item{@racket['index] --- The part represents an index.}

]

The recognized @tech{style properties} are as follows:

@itemize[

 @item{@racket['unnumbered] --- A section number is computed for an
       unnumbered section during the @techlink{collect pass}, but the
       number is not rendered.}

 @item{@racket['toc] --- Sub-parts of the part are rendered on separate
       pages for multi-page HTML mode.}

 @item{@racket['non-toc] --- Initial sub-parts of the part are
       @emph{not} rendered on separate pages for multi-page HTML
       mode; this style applies only to the main part.}

 @item{@racket['reveal] --- Shows sub-parts when this part is
       displayed in a table-of-contents panel in HTML output (which
       normally shows only the top-level sections).}

 @item{@racket['hidden] --- The part title is not shown in rendered
       HTML output.}

 @item{@racket['quiet] --- In HTML output and most other output modes,
       hides entries for sub-parts of this part in a
       @racket[table-of-contents] or @racket[local-table-of-contents]
       listing except when those sub-parts are top-level entries in
       the listing.}

 @item{@racket['no-toc] --- As a style for the main part of a
       document, causes the HTML output to not include a margin box
       for the main table of contents; the ``on this page'' box that
       contains @racket[toc-element] and @racket[toc-target-element]
       links (and that only includes an ``on this page'' label for
       multi-page documents) takes on the location and color of the
       main table of contents, instead.}

 @item{@racket[document-version] structure --- A version number for
       this part and its sub-parts (except as overridden). When it is
       not @racket[""] may be used when rendering a document; at a
       minimum, a non-@racket[""] version is rendered when it is
       attached to a part representing the whole document. The default
       version for a document is @racket[(version)].}

  @item{@racket[body-id] structure --- Generated HTML uses the given
        string @tt{id} attribute of the @tt{body} tag; this style can
        be set separately for parts that start different HTML pages,
        otherwise it is effectively inherited by sub-parts; the
        default is @racket["scribble-racket-lang.org"], but
        @exec{setup-plt} installs @racket["doc-racket-lang.org"] as the
        @tt{id} for any document that it builds.}

]

The @racket[to-collect] field contains @techlink{content} that is
inspected during the @techlink{collect pass}, but ignored in later
passes (i.e., it doesn't directly contribute to the output).

The @racket[blocks] field contains the part's initial flow (before
sub-parts).

The @racket[parts] field contains sub-parts.

}


@defstruct[paragraph ([style style?] [content content?])]{

A @techlink{paragraph} has a @tech{style} and a @tech{content}.

For the @racket[style] field, a string @tech{style name} corresponds
to a CSS class for HTML output or a macro for Latex output (see
@secref["extra-style"]). The following symbolic @tech{style names} are
recognized:

@itemize[

 @item{@racket['author] --- Typeset as the author of a document.  Such
       paragraphs normally should appear only in the initial flow of a
       @racket[part] for a document, where they are treated specially
       by the Latex renderer by moving the author information to the
       title.}

]

The currently recognized @tech{style properties} are as follows:

@itemize[

 @item{@racket['omitable] --- When a table cell contains a single
       @racket[omitable-paragraph], then when rendering to HTML, no
       @tt{p} tag wraps the cell content.}

 @item{@racket['div] --- Generates @tt{<div>} HTML output instead of
       @tt{<p>}.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<p>} or @tt{<div>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<p>} or @tt{<div>} tag.}

 @item{@racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]}


@defstruct[table ([style style?]
                  [blockss (listof (listof (or/c block? (one-of/c 'cont))))])]{

A @techlink{table} has, roughly, a list of list of blocks. A cell in
the table can span multiple columns by using @racket['cont] instead of
a block in the following columns (i.e., for all but the first in a set
of cells that contain a single block).

Within @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output or an environment for Latex output (see
@secref["extra-style"]). The following symbolic style names are also
recognized:

@itemize[

 @item{@racket['boxed] --- Renders as a definition.}

 @item{@racket['centered] --- Centers HTML output horizontally.}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[table-columns] structure --- Provides column-specific
       styles, but only if a @racket[table-cells] structure is not
       included as a @tech{style property}.}

 @item{@racket[table-cells] structure --- Provides cell-specific
       styles.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<table>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<table>} tag.}

 @item{@racket['aux] --- For HTML, include the table in the
       table-of-contents display for the enclosing part.}

 @item{@racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]

For Latex output, a paragraph as a cell value is not automatically
line-wrapped, unless a vertical alignment is specified for the cell
through a @racket[table-cells] or @racket[table-columns]
@tech{style property}. To get a line-wrapped paragraph, use a
@racket[compound-paragraph] or use an element with a string style and
define a corresponding Latex macro in terms of @tt{parbox}. For Latex
output of blocks in the flow that are @racket[nested-flow]s,
@racket[itemization]s, @racket[compound-paragraph]s, or
@racket[delayed-block]s, the block is wrapped with @tt{minipage} using
@tt{linewidth} divided by the column count as the width.}


@defstruct[itemization ([style style?]
                        [blockss (listof (listof block?))])]{

A @techlink{itemization} has a @tech{style} and a list of @tech{flows}.

In @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output or a macro for Latex output (see
@secref["extra-style"]). In addition, the following symbolic style
names are recognized:

@itemize[

 @item{@racket['compact] --- Reduces space between items.}

 @item{@racket['ordered] --- Generates @tt{<ol>} HTML output instead
       of @tt{<ul>} or an Latex enumeration instead of an
       itemization.}
]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<ul>} or @tt{<ol>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<ul>} or @tt{<ol>} tag.}

 @item{@racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]}


@defstruct[nested-flow ([style any/c]
                        [paragraphs (listof block?)])]{

A @techlink{nested flow} has a style and a @tech{flow}.

In @racket[style], the @racket{style name} is normally a string that
corresponds to a CSS class for HTML @tt{blockquote} output or a Latex
environment (see @secref["extra-style"]). The following symbolic style
names are recognized:

@itemize[

 @item{@racket['inset] --- Insets the nested flow relative to
       surrounding text.}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket['command] --- For Latex output, a string @tech{style
       name} is used as a command name instead of an environment
       name.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<blockquote>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<blockquote>} tag.}

 @item{@racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]}


@defstruct[compound-paragraph ([style style?]
                               [blocks (listof block?)])]{

A @techlink{compound paragraph} has a @tech{style} and a list of
@tech{blocks}.

For HTML, a @racket[paragraph] block in @racket[blocks] is rendered
without a @tt{<p>} tag, unless the paragraph has a style with a
non-@racket[#f] @tech{style name}. For Latex, each @tech{block} in
@racket[blocks] is rendered with a preceding @tt{\noindent}, unless
the block has the @racket['never-indents] property (checking
recursively in a @racket[nested-flow] or @racket[compound-paragraph]
if the @racket[nested-flow] or @racket[compound-paragraph] itself has
no @racket['never-indents] property).

The @racket[style] field of a compound paragraph is normally a string
that corresponds to a CSS class for HTML output or Latex environment
for Latex output (see @secref["extra-style"]). The following
@tech{style properties} are currently recognized:

@itemize[

 @item{@racket['command] --- For Latex output, a string @tech{style
       name} is used as a command name instead of an environment
       name.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<p>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<p>} tag.}

 @item{@racket['never-indents] --- For Latex within another
       @tech{compound paragraph}; see above.}

]}


@defstruct[delayed-block ([resolve (any/c part? resolve-info? . -> . block?)])]{

The @racket[resolve] procedure is called during the @techlink{resolve
pass} to obtain a normal @tech{block}. The first argument to
@racket[resolve] is the renderer.

}


@defstruct[element ([style element-style?]
                    [content content?])]{

Styled @tech{content} within an enclosing @tech{paragraph} or other content.

The @racket[style] field can be a @racket[style] structure, but it can
also be just a @tech{style name}.

In @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output and a macro name for Latex output (see
@secref["extra-style"]). The following symbolic style names are
recognized:

@itemize[

 @item{@racket['tt], @racket['italic], @racket['bold], @racket['sf],
       @racket['url], @racket['subscript], @racket['superscript], 
       @racket['smaller], @racket['larger] ---
       Basic styles recognized by all renders.}

 @item{@racket['hspace] --- Renders its @racket[content] as monospace
       blanks.}
 
 @item{@racket['newline] --- Renders a line break independent of
       the @racket[content].}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[target-url] structure --- Generates a hyperlink.}

 @item{@racket[url-anchor] structure --- For HTML, inserts a hyperlink
       target before @racket[content].}

 @item{@racket[color-property] structure --- Applies a color to the
       text of @racket[content].}

 @item{@racket[background-color-property] structure --- Applies a color to the
       background of @racket[content].}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for a @tt{<span>} tag.}

 @item{@racket[hover-property] structure --- For HTML, adds a text
       label to the content to be shown when the mouse hovers over
       it.}

 @item{@racket[script-property] structure --- For HTML, supplies a
       script alternative to @racket[content].}

  @item{@racket[body-id] structure --- For HTML uses the given
        string as an @tt{id} attribute of the @tt{span} tag.}

  @item{@racket['aux] --- Intended for use in titles, where the
        auxiliary part of the title can be omitted in hyperlinks. See,
        for example, @racket[secref].}

  @item{@racket['tt-chars] --- For Latex output, when the @tech{style
        name} is a string, render the element's content with escapes
        suitable for Latex @tt{tt} mode.}

  @item{@racket['exact-chars] --- For Latex output, when the @tech{style
        name} is a string, render the elements content exactly
        (without escapes).}

]}


@defstruct[(image-element element) ([path (or/c path-string?
                                                (cons/c 'collects (listof bytes?)))]
                                    [suffixes (listof #rx"^[.]")]
                                    [scale real?])]{

Used as a style for an @racket[element] to inline an image. The
@racket[path] field can be a result of
@racket[path->main-collects-relative].

For each string in @racket[suffixes], if the rendered works with the
corresponding suffix, the suffix is added to @racket[path] and used if
the resulting path refers to a file that exists. The order in
@racket[suffixes] determines the order in which suffixes are
tried. The HTML renderer supports @racket[".png"] and @racket[".gif"],
while the Latex renderer supports @racket[".png"], @racket[".pdf"],
and @racket[".ps"] (but rendering Latex output to PDF will not work
with @racket[".ps"] files, while rendering to Latex DVI output works
only with @racket[".ps"] files). If @racket[suffixes] is empty or if
none of the suffixes lead to files that exist, @racket[path] is used
as-is.

The @racket[scale] field scales the image in its rendered form.}


@defstruct[(target-element element) ([tag tag?])]{

Declares the content as a hyperlink target for @racket[tag].}


@defstruct[(toc-target-element target-element) ()]{

Like @racket[target-element], the content is also a kind of section
label to be shown in the ``on this page'' table for HTML output.}


@defstruct[(page-target-element target-element) ()]{

Like @racket[target-element], but a link to the element goes to the
top of the containing page.}


@defstruct[(redirect-target-element target-element) ([alt-path path-string?]
                                                     [alt-anchor string?])]{

Like @racket[target-element], but a link to the element is redirected
to the given URL.}


@defstruct[(toc-element element) ([toc-content content?])]{

Similar to @racket[toc-target-element], but with specific content for
the ``on this page'' table specified in the @racket[toc-content]
field.}


@defstruct[(link-element element) ([tag tag?])]{

Hyperlinks the content to @racket[tag].

When @racket[tag] is a part tag and the content of the element is
@racket[null], then the hyperlink uses the target part's number and/or
title as the content. In that case, if the section number is preceded
by a word, the word starts in uppercase if the element's style includes a
@racket['uppercase] property.}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (and/c pair? (listof string?))]
                                    [entry-seq (listof content?)]
                                    [desc any/c])]{

The @racket[plain-seq] specifies the keys for sorting, where the first
string is the main key, the second is a sub-key, etc. For
example, an ``night'' portion of an index might have sub-entries for
``night, things that go bump in'' and ``night, defender of the''. The
former would be represented by @racket[plain-seq] @racket['("night"
"things that go bump in")], and the latter by @racket['("night"
"defender of the")]. Naturally, single-string
@racket[plain-seq] lists are the common case, and at least one word is
required, but there is no limit to the word-list length. The strings in 
@racket[plain-seq] must not contain a newline character.

The @racket[entry-seq] list must have the same length as
@racket[plain-seq]. It provides the form of each key to render in the
final document.

The @racket[desc] field provides additional information about the
index entry as supplied by the entry creator. For example, a reference
to a procedure binding can be recognized when @racket[desc] is an
instance of @racket[procedure-index-desc]. See
@racketmodname[scribble/manual-struct] for other typical types of
@racket[desc] values.

See also @racket[index].}


@defstruct[multiarg-element ([style element-style?]
                             [content (listof content?)])]{

Like @racket[element] with a list for content, except that for Latex
output, if the @tech{style name} in @racket[style] is a string, then
it corresponds to a Latex command that accepts as many arguments (each
in curly braces) as elements of @racket[content].}


@defstruct[delayed-element ([resolve (any/c part? resolve-info? . -> . list?)]
                            [sizer (-> any/c)]
                            [plain (-> any/c)])]{

The @racket[render] procedure's arguments are the same as for
@racket[delayed-block], but the result is @techlink{content}. 
Unlike @racket[delayed-block], the
result of the @racket[render] procedure's argument is remembered on
the first call for re-use for a particular resolve pass.

The @racket[sizer] field is a procedure that produces a substitute
@techlink{content} for the delayed element for the purposes of
determining the delayed element's width (see @racket[element-width]).

The @racket[plain] field is a procedure that produces a substitute
@techlink{content} when needed before the @techlink{collect pass},
such as when @racket[element->string] is used before the @tech{collect
pass}.}


@defstruct[part-relative-element ([resolve (collect-info? . -> . list?)]
                                  [sizer (-> any/c)]
                                  [plain (-> any/c)])]{

Similar to @racket[delayed-block], but the replacement
@techlink{content} is obtained in the @techlink{collect pass} by
calling the function in the @racket[resolve] field.

The @racket[resolve] function can call @racket[collect-info-parents]
to obtain a list of @techlink{parts} that enclose the element,
starting with the nearest enclosing section. Functions like
@racket[part-collected-info] and @racket[collected-info-number] can
extract information like the part number.}


@defstruct[(collect-element element) ([collect (collect-info . -> . any)])]{

Like @racket[element], but the @racket[collect] procedure is called
during the @techlink{collect pass}. The @racket[collect] procedure
normally calls @racket[collect-put!].

Unlike @racket[delayed-element] or @racket[part-relative-element], the
element remains intact (i.e., it is not replaced) by either the
@tech{collect pass} or @tech{resolve pass}.}


@defstruct[(render-element element) ([render (any/c part? resolve-info? . -> . any)])]{

Like @racket[delayed-element], but the @racket[render] procedure is called
during the @techlink{render pass}.

If a @racket[render-element] instance is serialized (such as when
saving collected info), it is reduced to a @racket[element] instance.}


@defstruct[collected-info ([number (listof (or/c false/c integer?))]
                           [parent (or/c false/c part?)]
                           [info any/c])]{

Computed for each part by the @techlink{collect pass}.}


@defstruct[target-url ([addr path-string?])]{

Used as a @tech{style property} for an @racket[element]. A path is
allowed for @racket[addr], but a string is interpreted as a URL rather
than a file path.}


@defstruct[document-version ([text (or/c string? false/c)])]{

Used as a @tech{style property} for a @racket[path] to indicate a
version number.}


@defstruct[color-property ([color (or/c string? (list/c byte? byte? byte?))])]{

Used as a @tech{style property} for an @racket[element] to set its
color. Recognized string names for @racket[color] depend on the
renderer, but at the recognized set includes at least
@racket["white"], @racket["black"], @racket["red"], @racket["green"],
@racket["blue"], @racket["cyan"], @racket["magenta"], and
@racket["yellow"]. When @racket[color] is a list of bytes, the values
are used as RGB levels.}


@defstruct[background-color-property ([color (or/c string? (list/c byte? byte? byte?))])]{

Like @racket[color-property], but sets the background color.}

@defstruct[table-cells ([styless (listof (listof style?))])]{

Used as a @tech{style property} for a @racket[table] to set its cells'
styles.

If a cell style has a string name, it is used as an HTML class for the
@tt{<td>} tag or as a Latex command name.

The following symbols are recognized as cell-@tech{style properties}:

@itemize[

 @item{@racket['left] --- Left-align the cell content.}

 @item{@racket['right] --- Right-align the cell content top baselines.}

 @item{@racket['center] --- Center the cell content horizontally.}

 @item{@racket['top] --- Top-align the cell content.}

 @item{@racket['baseline] --- Align the cell content top baselines.}

 @item{@racket['bottom] --- bottom-align the cell content.}

 @item{@racket['vcenter] --- Center the cell content vertically.}

]

In addition, for HTML output, @racket[attributes] structures as
@tech{style properties} can add arbitrary attributes to a cell's
@tt{<td>} tag.}

@defstruct[table-columns ([styles (listof style?)])]{

Like @racket[table-cells], but the @racket[styles] list is duplicated
for each row in the table. This @tech{style property} is used only when a
@racket[table-cells] is not present in a style's list of properties.}

@defproc[(block? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[paragraph],
@racket[table], @racket[itemization], @racket[nested-flow], or
@racket[delayed-block], @racket[#f] otherwise.}


@defproc[(content? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string, symbol,
@racket[element], @racket[multiarg-element], @racket[delayed-element],
@racket[part-relative-element], or list of @tech{content}, @racket[#f]
otherwise.}


@defstruct[style ([name (or/c string? symbol? #f)]
                  [properties list?])]{

Represents a @techlink{style}.}


@defthing[plain style?]{

A style @racket[(make-style #f null)].}


@defproc[(element-style? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string, symbol, @racket[#f],
or @racket[style] structure.}


@defproc[(tag? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is acceptable as a link
@techlink{tag}, which is a list containing a symbol and either a
string, a @racket[generated-tag] instance, or a list (of arbitrary
values).}


@defstruct[generated-tag ()]{

A placeholder for a tag to be generated during the @techlink{collect
 pass}. Use @racket[tag-key] to convert a tag containing a
 @racket[generated-tag] instance to one containing a string.

}


@defproc*[([(content->string (content content?)) string?]
           [(content->string (content content?) (renderer any/c) (p part?) (info resolve-info?)) string?])]{

Converts @tech{content} to a single string (essentially
rendering the content as ``plain text'').

If @racket[p] and @racket[info] arguments are not supplied, then a
pre-``collect'' substitute is obtained for @tech{delayed
elements}. Otherwise, the two arguments are used to force the
@tech{delayed element} (if it has not been forced already).}

@defproc[(content-width [c content?]) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{content}.

}


@defproc[(block-width (e block?)) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{block}.}


@defstruct[collect-info ([ht any/c] [ext-ht any/c] [parts any/c] 
                         [tags any/c] [gen-prefix any/c] 
                         [relatives any/c] 
                         [parents (listof part?)])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{collect pass}. The fields are exposed, but not currently
intended for external use, except that @racket[collect-info-parents]
is intended for external use.

}

@defstruct[resolve-info ([ci any/c] [delays any/c] [undef any/c])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{resolve pass}. The fields are exposed, but not currently
intended for external use.

}

@defproc[(info-key? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @deftech{info key}: a list of
at least two elements whose first element is a symbol. The result is
@racket[#f] otherwise.

For a list that is an info tag, the interpretation of the second
element of the list is effectively determined by the leading symbol,
which classifies the key. However, a @racket[#f] value as the second
element has an extra meaning: collected information mapped by such
info keys is not propagated out of the part where it is collected;
that is, the information is available within the part and its
sub-parts, but not in ancestor or sibling parts.

Note that every @techlink{tag} is an info key.

}

@defproc[(collect-put! [ci collect-info?] [key info-key?] [val any/c])
         void?]{

Registers information in @racket[ci]. This procedure should be called
only during the @techlink{collect pass}.

}

@defproc[(resolve-get [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         any/c]{

Extract information during the @techlink{resolve pass} or
@techlink{render pass} for @racket[p] from @racket[ri], where the
information was previously registered during the @techlink{collect
pass}. See also @secref["passes"].

The result is @racket[#f] if the no value for the given key is found.
Furthermore, the search failure is recorded for potential consistency
reporting, such as when @exec{racket setup} is used to build
documentation.

}


@defproc[(resolve-get/ext? [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         (values any/c boolean?)]{

Like @racket[render-get], but returns a second value to indicate
whether the resulting information originated from an external source
(i.e., a different document).}


@defproc[(resolve-search [dep-key any/c] [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         void?]{

Like @racket[resolve-get], but a shared @racket[dep-key] groups
multiple searches as a single request for the purposes of consistency
reporting and dependency tracking. That is, a single success for the
same @racket[dep-key] means that all of the failed attempts for the
same @racket[dep-key] have been satisfied. However, for dependency
checking, such as when using @exec{racket setup} to re-build
documentation, all attempts are recorded (in case external changes
mean that an earlier attempt would succeed next time).

}

@defproc[(resolve-get/tentative [p (or/c part? false/c)] [ri resolve-info?] [key info-key?])
         any/c]{

Like @racket[resolve-search], but without dependency tracking. For
multi-document settings where dependencies are normally tracked, such
as when using @exec{racket setup} to build documentation, this function
is suitable for use only for information within a single document.

}

@defproc[(resolve-get-keys [p (or/c part? false/c)]
                           [ri resolve-info?] 
                           [pred (info-key? . -> . any/c)])
         list?]{

Applies @racket[pred] to each key mapped for @racket[p] in
@racket[ri], returning a list of all keys for which @racket[pred]
returns a true value.

}

@defproc[(part-collected-info [p part?]
                              [ri resolve-info?])
         collected-info?]{

Returns the information collected for @racket[p] as recorded within
@racket[ri].

}

@defproc[(tag-key [t tag?] [ri resolve-info?]) tag?]{

Converts a @racket[generated-tag] value with @racket[t] to a string.

}

@; ----------------------------------------

@section{HTML Style Properties}

@defmodule[scribble/html-properties]{ The
@racket[scribble/html-properties] library provides datatypes used as
@tech{style properties} for HTML rendering.}


@defstruct[attributes ([assoc (listof (cons/c symbol? string?))])]{

Used as a @tech{style property} to add arbitrary attributes to an HTML
tag.}


@defstruct[url-anchor ([name string?])]{

Used as a @tech{style property} with @racket[element] to insert an
anchor before the element.}


@defstruct[hover-property ([text string?])]{

Used as a @tech{style property} with @racket[element] to add text that
is shown when the mouse hovers over the element.}


@defstruct[script-property ([type string?]
                           [script (or/c path-string? (listof string?))])]{

Used as a @tech{style property} with @racket[element] to supply a
script alternative to the element content.}


@defstruct[css-addition ([path (or/c path-string? 
                                     (cons/c 'collects (listof bytes?)))])]{

Used as a @tech{style property} to supply a CSS file to be referenced
in the generated HTML. This property can be attached to any style, and
all additions are collected to the top of the generated HTML page.

The @racket[path] field can be a result of
@racket[path->main-collects-relative].}


@defstruct[body-id ([value string?])]{

Used as a @tech{style property} to associate an @tt{id} attribute with
an HTML tag.}


@defstruct[html-defaults ([prefix (or/c bytes? path-string? 
                                        (cons/c 'collects (listof bytes?)))]
                          [style (or/c bytes? path-string? 
                                       (cons/c 'collects (listof bytes?)))]
                          [extra-files (listof (or/c path-string? 
                                                     (cons/c 'collects (listof bytes?))))])]{

Like @racket[latex-defaults], but use for the 
@exec{scribble} command-line tool's @DFlag{html} and
@DFlag{htmls} modes.}


@; ----------------------------------------

@section{Latex Style Properties}

@defmodule[scribble/latex-properties]{ The
@racket[scribble/latex-properties] library provides datatypes used as
@tech{style properties} for Latex rendering.}


@defstruct[tex-addition ([path (or/c path-string? 
                                     (cons/c 'collects (listof bytes?)))])]{

Used as a @tech{style property} to supply a @filepath{.tex} file to be
included in the generated Latex. This property can be attached to any
style, and all additions are collected to the top of the generated
Latex file.

The @racket[path] field can be a result of
@racket[path->main-collects-relative].}


@defstruct[latex-defaults ([prefix (or/c bytes? path-string? 
                                         (cons/c 'collects (listof bytes?)))]
                           [style (or/c bytes? path-string? 
                                        (cons/c 'collects (listof bytes?)))]
                           [extra-files (listof (or/c path-string? 
                                                      (cons/c 'collects (listof bytes?))))])]{

Used as a @tech{style property} on the main @racket[part] of a document
to set a default prefix file, style file, and extra files (see
@secref["config-style"]).  The defaults are used by the
@exec{scribble} command-line tool for and @DFlag{latex} or @DFlag{pdf}
mode if none are supplied via @DFlag{prefix} and @DFlag{style} (where
@racket[extra-files] are used only when @racket[prefix] is used). A
byte-string value is used directly like file content, and a path can
be a result of @racket[path->main-collects-relative].

Languages (used with @hash-lang[]) like
@racketmodname[scribble/manual] and @racketmodname[scribble/sigplan]
add this property to a document to specify appropriate files for Latex
rendering.}


@defstruct[latex-auto-extra-files ([paths (listof (or/c path-string? 
                                                        (cons/c 'collects (listof bytes?))))])]{

Used as a @tech{style property} for the main @racket[part] of a
document to supply extra files needed to build the document via the
@exec{scribble} command-line tool (for @DFlag{latex} and @DFlag{pdf}
mode).

Languages (used with @hash-lang[]) like
@racketmodname[scribble/sigplan] add this property to a document to specify
appropriate extra files.}
