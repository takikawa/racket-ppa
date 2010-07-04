#lang scribble/doc
@(require "common.ss"
          (for-label net/cgi
                     net/uri-codec
                     net/cgi-unit
                     net/cgi-sig))

@title[#:tag "cgi"]{CGI Scripts}

@defmodule[net/cgi]{The @schememodname[net/cgi] module provides tools
for scripts that follow the Common Gateway Interface @cite["CGI"].}

The @schememodname[net/cgi] library expects to be run in a certain
context as defined by the CGI standard.  This means, for instance,
that certain environment variables will be bound.

Unfortunately, not all CGI environments provide this.  For instance,
the FastCGI library, despite its name, does not bind the environment
variables required of the standard.  Users of FastCGI will need to
bind @envvar{REQUEST_METHOD} and possibly also @envvar{QUERY_STRING}
to successfully employ the CGI library.  The FastCGI library ought to
provide a way to extract the values bound to these variables; the user
can then put these into the CGI program's environment using the
@scheme[putenv] function.

A CGI @deftech{binding} is an association of a form item with its
value.  Some form items, such as checkboxes, may correspond to
multiple bindings.  A binding is a tag-string pair, where a tag is a
symbol or a string.

@; ----------------------------------------

@section{CGI Functions}

@deftogether[(
@defproc[(get-bindings) (listof (cons/c (or/c symbol? string?) string?))]
@defproc[(get-bindings/post) (listof (cons/c (or/c symbol? string?) string?))]
@defproc[(get-bindings/get) (listof (cons/c (or/c symbol? string?) string?))]
)]{

Returns the bindings that corresponding to the options specified by
the user.  The @scheme[get-bindings/post] and
@scheme[get-bindings/get] variants work only when POST and GET forms
are used, respectively, while @scheme[get-bindings] determines the
kind of form that was used and invokes the appropriate function.

These functions respect @scheme[current-alist-separator-mode].
}


@defproc[(extract-bindings [key? (or/c symbol? string?)]
                           [bindings (listof (cons/c (or/c symbol? string?) string?))])
         (listof string?)]{

Given a key and a set of bindings, determines which ones correspond to
a given key.  There may be zero, one, or many associations for a given
key.}


@defproc[(extract-binding/single [key? (or/c symbol? string?)]
                                 [bindings (listof (cons/c (or/c symbol? string?) string?))])
         string?]{

Like @scheme[extract-bindings], but for a key that has exactly one
association.}


@defproc[(output-http-headers) void?]{

Outputs all the HTTP headers needed for a normal response.  Only
call this function if you are not using @scheme[generate-html-output] or
@scheme[generate-error-output].}


@defproc[(generate-html-output [title string?]
                               [body (listof string?)]
                               [text-color string? "#000000"]
                               [bg-color string? "#ffffff"]
                               [link-color string? "#cc2200"]
                               [vlink-color string? "#882200"]
                               [alink-color string? "#444444"])
         void?]{

Outputs an response: a title and a list of strings for the body.

The last five arguments are each strings representing a HTML color; in
order, they represent the color of the text, the background,
un-visited links, visited links, and a link being selected.}


@defproc[(string->html [str string?]) string?]{

Converts a string into an HTML string by applying the appropriate HTML
quoting conventions.}


@defproc[(generate-link-text [str string?] [html-str string?]) string?]{

Takes a string representing a URL, a HTML string for the anchor
text, and generates HTML corresponding to an anchor.}


@defproc[(generate-error-output [strs (listof string?)]) any]{

The procedure takes a list of HTML strings representing the body,
prints them with the subject line "Internal error", and exits via
@scheme[exit].}


@defproc[(get-cgi-method) (one-of/c "GET" "POST")]{

Returns either @scheme["GET"] or @scheme["POST"] when invoked inside a
CGI script, unpredictable otherwise.}


@defproc[(bindings-as-html (listof (cons/c (or/c symbol? string?) string?)))
         (listof string?)]{

Converts a set of bindings into a list of HTML strings, which is
useful for debugging.}


@defstruct[cgi-error ()]{

A supertype for all exceptions thrown by the @schememodname[net/cgi]
library.}


@defstruct[(incomplete-%-suffix cgi-error) ([chars (listof char?)])]{

Raised when a @litchar{%} in a query is followed by an incomplete
suffix.  The characters of the suffix---excluding the
@litchar{%}---are provided by the exception.}


@defstruct[(invalid-%-suffix cgi-error) ([char char?])]{

Raised when the character immediately following a @litchar{%} in a
query is invalid.}


@; ----------------------------------------

@section{CGI Unit}

@defmodule[net/cgi-unit]

@defthing[cgi@ unit?]{

Imports nothing, exports @scheme[cgi^].}

@; ----------------------------------------

@section{CGI Signature}

@defmodule[net/cgi-sig]

@defsignature[cgi^ ()]{}

Includes everything exported by the @schememodname[net/cgi] module.
