#lang scribble/doc
@(require "utils.ss")

@title{Miscellaneous Support}


@defproc[(regexp-replaces [objname (or/c string? bytes? symbol?)]
                          [substs (listof (list regexp? string?))])
         string?]{

A function that is convenient for many interfaces where the foreign
library has some naming convention that you want to use in your
interface as well.  The @scheme[objname] argument can be any value
that will be used to name the foreign object; it is first converted
into a string, and then modified according to the given
@scheme[substs] list in sequence, where each element in this list is a
list of a regular expression and a substitution string.  Usually,
@scheme[regexp-replace*] is used to perform the substitution, except
for cases where the regular expression begins with a @litchar{^} or
ends with a @litchar{$}, in which case @scheme[regexp-replace] is
used.

For example, the following makes it convenient to define Scheme
bindings such as @scheme[foo-bar] for foreign names like
@scheme[MyLib_foo_bar]:

@schemeblock[
(define mylib (ffi-lib "mylib"))
(define-syntax defmyobj
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj 
        (regexp-replaces 'name '((#rx"-" "_") 
                                 (#rx"^" "MyLib_")))
        mylib (_fun type ...)))]))
(defmyobj foo-bar : _int -> _int)
]}


@defproc[(list->cblock [lst list>][type ctype?]) any]{

Allocates a memory block of an appropriate size, and initializes it
using values from @scheme[lst] and the given @scheme[type].  The
@scheme[lst] must hold values that can all be converted to C values
according to the given @scheme[type].}


@defproc[(vector->cblock [vector any/c][type type?]) any]{

Like @scheme[list->cblock], but for Scheme vectors.}

@; ----------------------------------------------------------------------

@section{Unsafe Miscellaneous Operations}

@defproc[(cblock->list [cblock any/c][type ctype?][length exact-nonnegative-integer?])
         list?]{

Converts C @scheme[cblock], which is a vector of @scheme[type]s, to a
Scheme list.  The arguments are the same as in the
@scheme[list->cblock]. The @scheme[length] must be specified because
there is no way to know where the block ends.}


@defproc[(cblock->vector [cblock any/c][type ctype?][length exact-nonnegative-integer?])
         vector?]{

Like @scheme[cblock->vector], but for Scheme vectors.}
