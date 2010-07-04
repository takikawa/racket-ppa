#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "vectors"]{Vectors}

A @deftech{vector} is a fixed-length array of arbitrary
values. Unlike a list, a vector supports constant-time access and
update of its elements.

A vector prints similar to a list---as a parenthesized sequence of its
elements---but a vector is prefixed with @litchar{#}. For a vector as
an expression, an optional length can be supplied. Also, a vector as
an expression implicitly quotes the forms for its content, which means
that identifiers and parenthesized forms in a vector constant
represent symbols and lists.

@refdetails/gory["parse-vector"]{the syntax of vectors}

@examples[
(eval:alts @#,schemevalfont{#("a" "b" "c")} #("a" "b" "c"))
(eval:alts @#,schemevalfont{#(name (that tune))} #(name (that tune)))
(vector-ref #("a" "b" "c") 1)
(vector-ref #(name (that tune)) 1)
]

Like strings, a vector is either mutable or immutable, and vectors
written directly as expressions are immutable.

Vector can be converted to lists and vice-versa via
@scheme[list->vector] and @scheme[vector->list]; such conversions are
particularly useful in combination with predefined procedures on
lists. When allocating extra lists seems too expensive, consider
using looping forms like @scheme[fold-for], which recognize vectors as
well as lists.

@examples[
(list->vector (map string-titlecase
                   (vector->list #("three" "blind" "mice"))))
]

@refdetails["vectors"]{vectors and vector procedures}
