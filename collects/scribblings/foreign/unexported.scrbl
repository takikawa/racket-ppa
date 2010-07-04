#lang scribble/doc
@(require "utils.ss"
          (for-label (only-in '#%foreign
                              ffi-obj ffi-obj? ffi-obj-lib ffi-obj-name
                              ctype-basetype ctype-scheme->c ctype-c->scheme
                              ffi-call ffi-callback ffi-callback?)))

@title[#:tag "foreign:c-only"]{Unexported Primitive Functions}

@declare-exporting['#%foreign]

Parts of the @schememodname[scheme/foreign] library are implemented by
the MzScheme built-in @schememodname['#%foreign] module.  The
@schememodname['#%foreign] module is not intended for direct use, but
it exports the following procedures.  If you find any of these useful,
please let us know.

@defproc[(ffi-obj [objname (or/c string? bytes? symbol?)]
                  [lib (or/c ffi-lib? path-string? false/c)])
         any]{

Pulls out a foreign object from a library, returning a Scheme value
that can be used as a pointer.  If a name is provided instead of a
foreign-library value, @scheme[ffi-lib] is used to create a library
object.}


@defproc*[([(ffi-obj? [x any/c]) boolean?]
           [(ffi-obj-lib [obj ffi-obj?]) ffi-lib?]
           [(ffi-obj-name [obj ffi-obj?]) string?])]{

A predicate for objects returned by @scheme[ffi-obj], and accessor
functions that return its corresponding library object and name.
These values can also be used as C pointer objects.}


@defproc*[([(ctype-basetype [type ctype?]) (or/c ctype? false/c)]
           [(ctype-scheme->c [type ctype?]) procedure?]
           [(ctype-c->scheme [type ctype?]) procedure?])]{

Accessors for the components of a C type object, made by
@scheme[make-ctype].  The @scheme[ctype-basetype] selector returns
@scheme[#f] for primitive types (including cstruct types).}


@defproc[(ffi-call [ptr any/c] [in-types (listof ctype?)] [out-type ctype?]
                   [abi (or/c symbol/c false/c) #f])
         any]{

The primitive mechanism that creates Scheme ``callout'' values.  The
given @scheme[ptr] (any pointer value, including @scheme[ffi-obj]
values) is wrapped in a Scheme-callable primitive function that uses
the types to specify how values are marshaled.

The optional @scheme[abi] argument determines the foreign ABI that is
used.  @scheme[#f] or @scheme['default] will use a platform-dependent
default; other possible values are @scheme['stdcall] and
@scheme['sysv] (the latter corresponds to ``cdecl'').  This is
especially important on Windows, where most system functions are
@scheme['stdcall], which is not the default.}


@defproc[(ffi-callback [proc any/c] [in-types any/c] [out-type any/c]
                       [abi (or/c symbol/c false/c) #f])
         ffi-callback?]{

The symmetric counterpart of @scheme[ffi-call].  It receives a Scheme
procedure and creates a callback object, which can also be used as a
pointer.  This object can be used as a C-callable function, which
invokes @scheme[proc] using the types to specify how values are
marshaled.}


@defproc[(ffi-callback? [x any/c]) boolean?]{

A predicate for callback values that are created by @scheme[ffi-callback].
}
