#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          "common.ss"
          (for-label scheme/base
                     scheme/include
                     scheme/contract
                     compiler/cm
                     compiler/cm-accomplice))

@title[#:tag "make"]{Compiling Modified Modules to Bytecode}

The default mode for @|mzc| is to accept filenames for Scheme modules
to be compiled to bytecode format. Modules are re-compiled only if the
source Scheme file is newer than the bytecode file, or if any imported
module is recompiled.

@; ----------------------------------------------------------------------

@section{Bytecode Files}

A file @filepath{@nonterm{name}.@nonterm{ext}} is compiled to bytecode
that is saved as @filepath{compiled/@nonterm{name}_@nonterm{ext}.zo}
relative to the file. As a result, the bytecode file is normally used
automatically when @filepath{@nonterm{name}.@nonterm{ext}} is required
as a module, since the underlying @scheme[load/use-compiled] operation
detects such a bytecode file.

For example, in a directory that contains the following files:

@itemize{

 @item{@filepath{a.scm}:

@schememod[
scheme
(require "b.scm" "c.scm")
(+ b c)
]}

 @item{@filepath{b.scm}:

@schememod[
scheme
(provide b)
(define b 1)
]}

 @item{@filepath{c.scm}:

@schememod[
scheme
(provide c)
(define c 1)
]}}

then

@commandline{mzc a.scm}

triggers the creation of @filepath{compiled/a_ss.zo},
@filepath{compiled/b_ss.zo}, and @filepath{compiled/c_ss.zo}.
A subsequent

@commandline{mzscheme a.scm}

loads bytecode from the generated @filepath{.zo} files, paying attention
to the @filepath{.scm} sources only to confirm that each
@filepath{.zo} file has a later timestamp.

In contrast,

@commandline{mzc b.scm c.scm}

would create only @filepath{compiled/b_scm.zo} and
@filepath{compiled/c_scm.zo}, since neither @filepath{b.scm} nor
@filepath{c.scm} imports @filepath{a.scm}.

@; ----------------------------------------------------------------------

@section{Dependency Files}

In addition to a bytecode file, @|mzc| creates a file
@filepath{compiled/@nonterm{name}_@nonterm{ext}.dep} that records
dependencies of the compiled module on other module files.  Using this
dependency information, a re-compilation request via @|mzc| can
consult both the source file's timestamp and the timestamps for the
sources and bytecode of imported modules.  Furthermore, imported
modules are themselves compiled as necessary, including updating the
bytecode and dependency files for the imported modules, transitively.

Continuing the @exec{mzc a.scm} example from the previous section, the
@|mzc| creates @filepath{compiled/a_scm.dep},
@filepath{compiled/b_scm.dep}, and @filepath{compiled/c_scm.dep} at
the same time as the @filepath{.zo} files. The
@filepath{compiled/a_scm.dep} file records the dependency of
@filepath{a.scm} on @filepath{b.scm}, @filepath{c.scm} and the
@schememodname[scheme] library. If the @filepath{b.scm} file is
modified (so that its timestamp changes), then running

@commandline{mzc a.scm}

again rebuilds @filepath{compiled/a_ss.zo} and
@filepath{compiled/b_ss.zo}.

For module files that are within library collections, @exec{setup-plt}
uses the same @filepath{.zo} and @filepath{.dep} conventions and files
as @|mzc|, so the two tools can be used together.

@; ----------------------------------------------------------------------

@section{Scheme Compilation Manager API}

@defmodule[compiler/cm]{The @schememodname[compiler/cm] module
implements the compilation and dependency management used by @|mzc|
and @exec{setup-plt}.}

@defproc[(make-compilation-manager-load/use-compiled-handler)
         (path? (or/c symbol? false/c) . -> . any)]{

Returns a procedure suitable as a value for the
@scheme[current-load/use-compiled] parameter. The returned procedure
passes it arguments on to the @scheme[current-load/use-compiled]
procedure that is installed when
@scheme[make-compilation-manager-load/use-compiled-handler] is called,
but first it automatically compiles a source file to a @filepath{.zo}
file if

@itemize{

 @item{the file is expected to contain a module (i.e., the second
 argument to the handler is a symbol);}

 @item{the value of each of @scheme[(current-eval)],
 @scheme[(current-load)], and @scheme[(namespace-module-registry
 (current-namespace))] is the same as when
 @scheme[make-compilation-manager-load/use-compiled-handler] was
 called;}

 @item{the value of @scheme[use-compiled-file-paths] contains the
 first path that was present when
 @scheme[make-compilation-manager-load/use-compiled-handler] was
 called;}

 @item{the value of @scheme[current-load/use-compiled] is the result
 of this procedure; and}

 @item{one of the following holds:

 @itemize{

  @item{the source file is newer than the @filepath{.zo} file in the
        first sub-directory listed in @scheme[use-compiled-file-paths]
        (at the time that
        @scheme[make-compilation-manager-load/use-compiled-handler]
        was called)}

  @item{no @filepath{.dep} file exists next to the @filepath{.zo}
        file;}

  @item{the version recorded in the @filepath{.dep} file does not
        match the result of @scheme[(version)];}

  @item{one of the files listed in the @filepath{.dep} file has a
        @filepath{.zo} timestamp newer than the one recorded in the
        @filepath{.dep} file.}

  }}

}

After the handler procedure compiles a @filepath{.zo} file, it creates
a corresponding @filepath{.dep} file that lists the current version,
plus the @filepath{.zo} timestamp for every file that is
@scheme[require]d by the module in the compiled file. Additional
dependencies can be installed during compilation via
@schememodname[compiler/cm-accomplice].

The handler caches timestamps when it checks @filepath{.dep} files,
and the cache is maintained across calls to the same handler. The
cache is not consulted to compare the immediate source file to its
@filepath{.zo} file, which means that the caching behavior is
consistent with the caching of the default module name resolver (see
@scheme[current-module-name-resolver]).

If @scheme[use-compiled-file-paths] contains an empty list when
@scheme[make-compilation-manager-load/use-compiled-handler] is called,
then @scheme[exn:fail:contract] exception is raised.

@emph{Do not} install the result of
@scheme[make-compilation-manager-load/use-compiled-handler] when the
current namespace contains already-loaded versions of modules that may
need to be recompiled---unless the already-loaded modules are never
referenced by not-yet-loaded modules. References to already-loaded
modules may produce compiled files with inconsistent timestamps and/or
@filepath{.dep} files with incorrect information.}


@defproc[(managed-compile-zo [file path-string?]
                             [read-src-syntax (any/c input-port? . -> . syntax?) read-syntax]) 
         void?]{

Compiles the given module source file to a @filepath{.zo}, installing
a compilation-manager handler while the file is compiled (so that
required modules are also compiled), and creating a @filepath{.dep} file
to record the timestamps of immediate files used to compile the source
(i.e., files @scheme[require]d in the source).

If @scheme[file] is compiled from source, then
@scheme[read-src-syntax] is used in the same way as
@scheme[read-syntax] to read the source module. The normal
@scheme[read-syntax] is used for any required files, however.}


@defboolparam[trust-existing-zos trust?]{

A parameter that is intended for use by @exec{setup-plt} when
installing with pre-built @filepath{.zo} files. It causes a
compilation-manager @scheme[load/use-compiled] handler to ``touch''
out-of-date @filepath{.zo} files instead of re-compiling from source.}


@defproc[(make-caching-managed-compile-zo
          [read-src-syntax (any/c input-port? . -> . syntax?)])
         (path-string? . -> . void?)]{

Returns a procedure that behaves like @scheme[managed-compile-zo]
(providing the same @scheme[read-src-syntax] each time), but a cache
of timestamp information is preserved across calls to the procedure.}


@defparam[manager-compile-notify-handler notify (path? . -> . any)]{

A parameter for a procedure of one argument that is called whenever a
compilation starts. The argument to the procedure is the file's path.}


@defparam[manager-trace-handler notify (string? . -> . any)]{

A parameter for a procedure of one argument that is called to report
 compilation-manager actions, such as checking a file. The argument to
 the procedure is a string.}

@; ----------------------------------------------------------------------

@section{Compilation Manager Hook for Syntax Transformers}

@defmodule[compiler/cm-accomplice]

@defproc[(register-external-file [file (and path? complete-path?)]) void?]{

Logs a message (see @scheme[log-message]) at level @scheme['info]. The
message data is a @schemeidfont{file-dependency} prefab structure type
with one field whose value is @scheme[file].

A compilation manager implemented by @schememodname[compiler/cm] looks
for such messages to register an external dependency. The compilation
manager records (in a @filepath{.dep} file) the path as contributing
to the implementation of the module currently being
compiled. Afterward, if the registered file is modified, the
compilation manager will know to recompile the module.

The @scheme[include] macro, for example, calls this procedure with the
path of an included file as it expands an @scheme[include] form.}
