#lang scribble/doc
@(require scribble/manual
          (for-label algol60/algol60))

@title{@bold{Algol 60}}

@section{Implementation}

The ``Algol 60'' language for DrScheme implements the language defined
by the ``Revised Report on the Algorithmic Language Algol 60,'' edited
by Peter Naur.

@section{Including Algol 60 Programs}

Although Algol 60 is mainly provided as a DrScheme language,
@scheme[include-algol] supports limited use of Algol 60 programs in
larger programs.

@defmodule[algol60/algol60]

@defform[(include-algol path-string)]{

Includes the Algol 60 program indicated by @scheme[path-string] as an
expression in a Scheme program. The included Algol 60 program is
closed (i.e., it doesn't see any bindings in the included context),
and the result is always @|void-const|.}


@section{Language}

The DrScheme and @scheme[include-algol] implementation departs from
the Algol 60 specification in the following minor ways:

@(itemize (item "Strings are not permitted to contain nested quotes.")
          (item "Identifiers cannot contain whitespace.")
          (item "Argument separators are constrained to be identifiers (i.e., they
    cannot be keywords, and they cannot consist of multiple
    identifiers separated by whitespace.)")
          (item "Numbers containing exponents (using the ``10'' subscript) are not
    supported."))

Identifiers and keywords are case-sensitive. The boldface/underlined
keywords of the report are represented by the obvious character
sequence, as are most operators. A few operators do not fit into
ASCII, and they are mapped as follows:

@(verbatim 
"   times             *
   quotient          div
   exponential       ^  
   less or equal     <= 
   greater or equal  >=
   not equal         !=
   equivalence       ==
   implication       =>
   and               &
   or                |
   negation          !")

In addition to the standard functions, the following output functions
are supported:

@(verbatim
"   prints(E)    prints the string E
   printsln(E)  prints the string E followed by a newline
   printn(E)    prints the number E
   printnln(E)  prints the number E followed by a newline")

A prompt in DrScheme's Interactions window accepts whole programs only
for the Algol 60 language.

