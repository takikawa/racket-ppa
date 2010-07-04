#lang scribble/doc
@(require "mz.ss")

@title{Module Names and Loading}

@;------------------------------------------------------------------------
@section[#:tag "modnameresolver"]{Resolving Module Names}

The name of a declared module is represented by a @deftech{resolved
module path}, which encapsulates either a symbol or a complete
filesystem path (see @secref["pathutils"]). A symbol normally refers
to a predefined module or module declared through reflective
evaluation (e.g., @scheme[eval]). A filesystem path normally refers to
a module declaration that was loaded on demand via @scheme[require] or
other forms.

A @deftech{module path} is a datum that matches the grammar for
@scheme[_module-path] for @scheme[require]. A module path is relative
to another module.

@defproc[(resolved-module-path? [v any/c]) boolean?]{

Returns @scheme[#f] if @scheme[v] is a @tech{resolved module path},
@scheme[#f] otherwise.}

@defproc[(make-resolved-module-path [path (or/c symbol? (and/c path? complete-path?))])
         resolved-module-path?]{

Returns a @tech{resolved module path} that encapsulates @scheme[path].
If @scheme[path] is not a symbol, it normally should be
@tech{cleanse}d (see @scheme[cleanse-path]) and simplified (see
@scheme[simplify-path]).

A @tech{resolved module path} is interned. That is, if two
@tech{resolved module path} values encapsulate paths that are
@scheme[equal?], then the @tech{resolved module path} values are
@scheme[eq?].}

@defproc[(resolved-module-path-name [module-path resolved-module-path?])
         path?]{

Returns the path encapsulated by a @tech{resolved module path}.}


@defproc[(module-path? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] corresponds to a datum that matches
the grammar for @scheme[_module-path] for @scheme[require],
@scheme[#f] otherwise.}


@defparam[current-module-name-resolver proc 
           (case->
            (resolved-module-path?
             . -> . 
             any)
            ((or/c module-path? path?)
             (or/c false/c resolved-module-path?) 
             (or/c false/c syntax?) 
             boolean?
             . -> . 
             resolved-module-path?))]{

A parameter that determines the current @deftech{module name
resolver}, which manages the conversion from other kinds of module
references to a symbol or @tech{resolved module path}. For example,
when the expander encounters @scheme[(require _module-path)] where
@scheme[_module-path] is not an identifier, then the expander passes
@scheme['_module-path] to the module name resolver to obtain a symbol
or resolved module path. When such a @scheme[require] appears within a
module, the @deftech{module path resolver} is also given the name of
the enclosing module, so that a relative reference can be converted to
an absolute symbol or @tech{resolved module path}.

A @tech{module name resolver} takes one and four arguments:
%
@itemize{

  @item{When given one argument, it is a name for a module declaration
  that is already loaded. Such a call to the module name resolver is a
  notification that the corresponding module does not need to be
  loaded (for the current namespace, or any other namespace that
  shares the same module registry). The module name resolver's result
  is ignored.}
 
  @item{When given four arguments, the first is a module path, either
  equivalent to a quoted @scheme[module-path] for @scheme[require] or
  a file system path.  The second is name for the source module, if
  any, to which the path is relative; if the second argument is
  @scheme[#f], the module path is relative to @scheme[(or
  (current-load-relative-directory) (current-directory))].  The third
  argument is a @tech{syntax object} that can be used for error
  reporting, if it is not @scheme[#f]. If the last argument is
  @scheme[#t], then the module declaration should be loaded (if it is
  not already), otherwise the module path should be simply resolved to
  a name. The result is the resolved name.}

}

For the second case, the standard module name resolver keeps a
per-registry table of loaded module name. If a resolved module path is
not in the table, and @scheme[#f] is not provided as the third
argument to the @tech{module name resolver}, then the name is put into
the table and the corresponding file is loaded with a variant of
@scheme[load/use-compiled] that passes the expected module name to the
@tech{compiled-load handler}.

While loading a file, the default @tech{module name resolver} sets the
@scheme[current-module-declare-name] parameter to the resolved module
name. Also, the default @tech{module name resolver} records in a
private @tech{continuation mark} the filename being loaded, and it
checks whether such a mark already exists; if such a continuation mark
does exist in the current continuation, then the @exnraise[exn:fail]
with a message about a dependency cycle.

Module loading is suppressed (i.e., @scheme[#f] is supplied as a third
argument to the module name resolver) when resolving module paths in
@tech{syntax objects} (see @secref["stxobj-model"]). When a
@tech{syntax object} is manipulated, the current namespace might not
match the original namespace for the syntax object, and the module
should not necessarily be loaded in the current namespace.

The current module name resolver is called with a single argument by
@scheme[namespace-attach-module] to notify the resolver that a module
was attached to the current namespace (and should not be loaded in the
future for the namespace's registry). No other Scheme operation
invokes the module name resolver with a single argument, but other
tools (such as DrScheme) might call this resolver in this mode to
avoid redundant module loads.}


@defparam[current-module-declare-name name (or/c resolved-module-path? false/c)]{

A parameter that determines a module name that is used when evaluating
a @scheme[module] declaration (when the parameter value is not
@scheme[#f]). In that case, the @scheme[_id] from the @scheme[module]
declaration is ignored, and the parameter's value is used as the name
of the declared module.}

@;------------------------------------------------------------------------
@section[#:tag "modpathidx"]{Compiled Modules and References}

While expanding a @scheme[module] declaration, the expander resolves
module paths for imports to load module declarations as necessary and
to determine imported bindings, but the compiled form of a
@scheme[module] declaration preserves the original module path.
Consequently, a compiled module can be moved to another filesystem,
where the module name resolver can resolve inter-module references
among compiled code.

When a module reference is extracted from compiled form (see
@scheme[module-compiled-imports]) or from syntax objects in macro
expansion (see @secref["stxops"]), the module reference is reported in
the form of a @deftech{module path index}. A @tech{module path index}
is a semi-interned (multiple references to the same relative module
tend to use the same @tech{module path index} value, but not always)
opaque value that encodes a module path (see @scheme[module-path?])
and another @tech{module path index} to which it is relative.

A @tech{module path index} that uses both @scheme[#f] for its path and
base @tech{module path index} represents ``self''---i.e., the module
declaration that was the source of the @tech{module path index}---and
such a @tech{module path index} is always used as the root for a chain
of @tech{module path index}. For example, when extracting information
about an identifier's binding within a module, if the identifier is
bound by a definition within the same module, the identifier's source
module is reported using the ``self'' @tech{module path index}. If the
identifier is instead defined in a module that is imported via a
module path (as opposed to a literal module name), then the
identifier's source module will be reported using a @tech{module path
index} that contains the @scheme[require]d module path and the
``self'' @tech{module path index}.


A @tech{module path index} has state. When it is @deftech{resolved} to
a @tech{resolved module path}, then the @tech{resolved module path} is
stored with the @tech{module path index}. In particular, when a module
is loaded, its root @tech{module path index} is resolved to match the
module's declaration-time name. This resolved path is forgotten,
however, in identifiers that the module contributes to the compiled
and marshaled form of other modules. The transient nature of resolved
names allows the module code to be loaded with a different resolved
name than the name when it was compiled.

@defproc[(module-path-index? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{module path index},
@scheme[#f] otherwise.}


@defproc[(module-path-index-resolve [mpi module-path-index?])
         resolved-module-path?]{

Returns a @tech{resolved module path} for the resolved module name,
computing the resolved name (and storing it in @scheme[mpi]) if it has
not been computed before.

Resolving a @tech{module path index} uses the current @tech{module
name resolver} (see @scheme[current-module-name-resolver]). Depending
on the kind of module paths encapsulated by @scheme[mpi], the computed
resolved name can depend on the value of
@scheme[current-load-relative-directory] or
@scheme[current-directory].}


@defproc[(module-path-index-split [mpi module-path-index?])
         (values (or/c module-path? false/c)
                 (or/c module-path-index? false/c))]{

Returns two values: a module path, and a base @tech{module path index}
or @scheme[#f] to which the module path is relative.

A @scheme[#f] second result means that the path is relative to an
unspecified directory (i.e., its resolution depends on the value of
@scheme[current-load-relative-directory] and/or
@scheme[current-directory]).

A @scheme[#f] for the first result implies a @scheme[#f] for the
second result, and means that @scheme[mpi] represents ``self'' (see
above).}

@defproc[(module-path-index-join [path (or/c module-path? false/c)]
                                 [mpi (or/c module-path-index? false/c)])
         module-path-index?]{

Combines @scheme[path] and @scheme[mpi] to create a new @tech{module
path index}. The @scheme[path] argument can @scheme[#f] only if
@scheme[mpi] is also @scheme[false].}

@defproc[(compiled-module-expression? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a compiled @scheme[module]
declaration, @scheme[#f] otherwise. See also
@scheme[current-compile].}


@defproc[(module-compiled-name [compiled-module-code compiled-module-expression?])
         symbol?]{

Takes a module declaration in compiled form and returns a symbol for
the module's declared name.}


@defproc[(module-compiled-imports [compiled-module-code compiled-module-expression?])
         (listof (cons/c (or/c exact-integer? false/c) 
                         (listof module-path-index?)))]{

Takes a module declaration in compiled form and returns an association
list mapping @tech{phase level} shifts (where @scheme[#f] corresponds
to a shift into the @tech{label phase level}) to module references for
the module's explicit imports.}


@defproc[(module-compiled-exports [compiled-module-code compiled-module-expression?])
         (values (listof (cons/c (or/c exact-integer? false/c) list?))
                 (listof (cons/c (or/c exact-integer? false/c) list?)))]

Returns two association lists mapping @tech{phase level} values (where
@scheme[#f] corresponds to the @tech{label phase level}) to exports at
the corresponding phase. The first association list is for exported
variables, and the second is for exported syntax.

Each associated list more precisely matches the contract

@schemeblock[
(listof (list/c symbol?
                (listof 
                 (or/c module-path-index?
                       (list/c module-path-index?
                               (or/c exact-integer? false/c)
                               symbol?
                               (or/c exact-integer? false/c))))))
]

For each element of the list, the leading symbol is the name of the
export.

The second part---the list of @tech{module path index} values,
etc.---describes the origin of the exported identifier. If the origin
list is @scheme[null], then the exported identifier is defined in the
module. If the exported identifier is re-exported, instead, then the
origin list provides information on the import that was re-exported.
The origin list has more than one element if the binding was imported
multiple times from (possibly) different sources.

For each origin, a @tech{module path index} by itself means that the
binding was imported with a @tech{phase level} shift of @scheme[0]
(i.e., a plain @scheme[require] without @scheme[for-meta],
@scheme[for-syntax], etc.), and imported identifier has the same name
as the re-exported name. An origin represented with a list indicates
explicitly the import, the import @tech{phase level} shift (where
@scheme[#f] corresponds to a @scheme[for-label] import), the import
name of the re-exported binding, and the @tech{phase level} of the
import.}


@;------------------------------------------------------------------------
@section[#:tag "dynreq"]{Dynamic Module Access}

@defproc[(dynamic-require [mod module-path?][provided (or/c symbol? false/c void?)]) 
         any]{

Dynamically instantiates the module specified by @scheme[mod] for
@tech{phase} 0 in the current namespace's registry, if it is not yet
instantiated. If @scheme[mod] is not a symbol, the current
@tech{module name resolver} may load a module declaration to resolve
it (see @scheme[current-module-name-resolver]); the path is resolved
relative to @scheme[current-load-relative-directory] and/or
@scheme[current-directory].

If @scheme[provided] is @scheme[#f], then the result is
@|void-const|. Otherwise, when @scheme[provided] is a symbol, the
value of the module's export with the given name is returned. If the
module exports @scheme[provide] as syntax, then a use of the binding
is expanded and evaluated (in a fresh namespace to which the module is
attached). If the module has no such exported variable or syntax, or
if the variable is protected (see @secref["modprotect"]), the
@exnraise[exn:fail:contract].

If @scheme[provided] is @|void-const|, then the module is
@tech{visit}ed (see @secref["mod-parse"]), but not
@tech{instantiate}d. The result is @|void-const|.}


@defproc[(dynamic-require-for-syntax [mod module-path?]
                                     [provided (or/c symbol? false/c)])
         any]{

Like @scheme[dynamic-require], but in @tech{phase} 1.}
