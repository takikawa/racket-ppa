#lang scribble/doc
@(require "mz.ss")

@title{Evaluation and Compilation}

@defparam[current-eval proc (any/c . -> . any)]{

A parameter that determines the current @deftech{evaluation handler}.
The evaluation handler is a procedure that takes a top-level form and
evaluates it, returning the resulting values. The @tech{evaluation
handler} is called by @scheme[eval], @scheme[eval-syntax], the default
@tech{load handler}, and @scheme[read-eval-print-loop] to evaluate a
top-level form. The handler should evaluate its argument in tail
position.

The @scheme[_top-level-form] provided to the handler can be a
@tech{syntax object}, a compiled form, a compiled form wrapped as a
syntax object, or an arbitrary datum.

The default handler converts an arbitrary datum to a syntax object
using @scheme[datum->syntax], and then enriches its @tech{lexical
information} in the same way as @scheme[eval]. (If
@scheme[_top-level-form] is a syntax object, then its @tech{lexical
information} is not enriched.)  The default evaluation handler
partially expands the form to splice the body of top-level
@scheme[begin] forms into the top level (see
@scheme[expand-to-top-form]), and then individually compiles and
evaluates each spliced form before continuing to expand, compile, and
evaluate later forms.}


@defproc[(eval [top-level-form any/c]
               [namespace namespace? (current-namespace)])
         any]{

Calls the current @tech{evaluation handler} to evaluate
@scheme[top-level-form]. The @tech{evaluation handler} is called in
tail position with respect to the @scheme[eval] call, and
@scheme[parameterize]d to set @scheme[current-namespace] to
@scheme[namespace].

If @scheme[top-level-form] is a syntax object whose datum is not a
compiled form, then its @tech{lexical information} is enriched before
it is sent to the @tech{evaluation handler}:

@itemize{

 @item{If @scheme[top-level-form] is a pair whose @scheme[car] is a
       symbol or identifier, and if applying
       @scheme[namespace-syntax-introduce] to the
       (@scheme[datum->syntax]-converted) identifier produces an
       identifier bound to @scheme[module] in a @tech{phase level}
       that corresponds to @scheme[namespace]'s @tech{base phase},
       then only that identifier is enriched.}

 @item{For any other @scheme[top-level-form],
       @scheme[namespace-syntax-introduce] is applied to the entire
       syntax object.}

}

For interactive evaluation in the style of
@scheme[read-eval-print-loop] and @scheme[load], wrap each expression
with @schemeidfont{#%top-interaction}, which is normally bound to
@scheme[#%top-interaction], before passing it to @scheme[eval].}


@defproc[(eval-syntax [stx syntax?]
                      [namespace namespace? (current-namespace)])
         any]{

Like @scheme[eval], except that @scheme[stx] must be a syntax object,
and its lexical context is not enriched before it is passed to the
@tech{evaluation handler}.}


@defparam[current-load proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines the current @deftech{load handler} to load
top-level forms from a file. The @tech{load handler} is called by
@scheme[load], @scheme[load-relative], @scheme[load/cd], and the
default @tech{compiled-load handler}.

A load handler takes two arguments: a path (see
@secref["pathutils"]) and an expected module name. The expected
module name is a symbol when the call is to load a module declaration
in response to a @scheme[require] (in which case the file should
contain a module declaration), or @scheme[#f] for any other load.
 
The default load handler reads forms from the file in
@scheme[read-syntax] mode with line-counting enabled for the file
port, unless the path has a @scheme[".zo"] suffix. It also
@scheme[parameterize]s each read to set both
@scheme[read-accept-compiled] and @scheme[read-accept-reader] to
@scheme[#t]. In addition, if @scheme[load-on-demand-enabled] is
@scheme[#t], then @scheme[read-on-demand-source] is effectively set to
the @tech{cleanse}d, absolute form of @scheme[path] during the
@scheme[read-syntax] call. After reading a single form, the form is
passed to the current @tech{evaluation handler}, wrapping the
evaluation in a continuation prompt (see
@scheme[call-with-continuation-prompt]) for the default continuation
prompt tag with handler that propagates the abort to the continuation
of the @scheme[load] call.

If the second argument to the load handler is a symbol, then:

@itemize{

 @item{The @scheme[read-syntax] from the file is additionally
       @scheme[parameterize]d as follows (to provide consistent reading
       of module source):

       @schemeblock[
       (current-readtable #f)
       (read-case-sensitive #t)
       (read-square-bracket-as-paren #t)
       (read-curly-brace-as-paren #t)
       (read-accept-box #t)
       (read-accept-compiled #t)
       (read-accept-bar-quote #t)
       (read-accept-graph #t)
       (read-decimal-as-inexact #t)
       (read-accept-dot #t)
       (read-accept-infix-dot #t)
       (read-accept-quasiquote #t)
       (read-accept-reader #t)
       ]}

 @item{If the read result is not a @schemeidfont{module} form with the
       expected name, or if a second @scheme[read-syntax] does not
       produce an end-of-file, then the @exnraise[exn:fail] without
       evaluating the form that was read from the file.}

 @item{The @tech{lexical information} of the initial
       @schemeidfont{module} identifier is enriched with a binding for
       @scheme[module], so that the form corresponds to a module
       declaration independent of the current namespace's bindings.}

}

If the second argument to the load handler is @scheme[#f], then each
expression read from the file is wrapped with
@schemeidfont{#%top-interaction}, which is normally bound to
@scheme[#%top-interaction], before passing it to the @tech{evaluation
handler}.

The return value from the default @tech{load handler} is the value of
the last form from the loaded file, or @|void-const| if the file
contains no forms. If the given path is a relative path, then it is
resolved using the value of @scheme[current-directory].}


@defproc[(load [file path-string?]) any]{

Calls the current @tech{load handler} in tail position. The call is
@scheme[parameterized] to set @scheme[current-load-relative-directory]
to the directory of @scheme[file], which is resolved relative to
the value of @scheme[current-directory].}


@defproc[(load-relative [file path-string?]) any]{

Like @scheme[load/use-compiled], but when @scheme[file] is a relative
path, it is resolved using the value of
@scheme[current-load-relative-directory] instead of the value of
@scheme[current-directory] if the former is not @scheme[#f], otherwise
@scheme[current-directory] is used.}


@defproc[(load/cd [file path-string?]) any]{

Like @scheme[load], but @scheme[load/cd] sets both
@scheme[current-directory] and
@scheme[current-load-relative-directory] before calling the @tech{load
handler}.}


@defparam[current-load-extension proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines a @deftech{extension-load handler}, which is
called by @scheme[load-extension] and the default @tech{compiled-load
handler}.

An @tech{extension-load handler} takes the same arguments as a
@tech{load handler}, but the file should be a platform-specific
@deftech{dynamic extension}, typically with the file suffix
@filepath{.so} (Unix), @filepath{.dll} (Windows), or @filepath{.dylib}
(Mac OS X).  The file is loaded using internal, OS-specific
primitives. See @other-manual['(lib
"scribblings/inside/inside.scrbl")] for more information on
@tech{dynamic extensions}.}


@defproc[(load-extension [file path-string?]) any]{

Sets @scheme[current-load-relative-directory] like @scheme[load], and
calls the @tech{extension-load handler} in tail position.}


@defproc[(load-relative-extension [file path-string?]) any]{

Like @scheme[load-exension], but resolves @scheme[file] using
@scheme[current-load-relative-directory] like @scheme[load-relative].}


@defparam[current-load/use-compiled proc (path? (or/c symbol? #f) . -> . any)]{

A parameter that determines the current @deftech{compiled-load
handler} to load from a file that may have a compiled form. The
@tech{compiled-load handler} is called by @scheme[load/use-compiled].

The protocol for a @tech{compiled-load handler} is the same as for the
@tech{load handler} (see @scheme[current-load]), except that a
@tech{compiled-load handler} is expected to set
@scheme[current-load-relative-directory] itself. The default
@tech{compiled-load handler}, however, checks for @filepath{.zo} files
(usually produced with @scheme[compile-file]) and @filepath{.so} (Unix),
@filepath{.dll} (Windows), or @filepath{.dylib} (Mac OS X) files.

The check for a compiled file occurs whenever the given path
@scheme[_file] ends with any extension (e.g., @filepath{.ss} or
@filepath{.scm}), and the check consults the subdirectories indicated by
the @scheme[use-compiled-file-paths] parameter relative to
@scheme[_file].  The subdirectories are checked in order. A @filepath{.zo}
version of the file is loaded if it exists directly in one of the
indicated subdirectories, or a @filepath{.so}/@filepath{.dll}/@filepath{.dylib}
version of the file is loaded if it exists within a @filepath{native}
subdirectory of a @scheme[use-compiled-file-paths] directory, in an
even deeper subdirectory as named by
@scheme[system-library-subpath]. A compiled file is loaded only if its
modification date is not older than the date for @scheme[_file]. If
both @filepath{.zo} and @filepath{.so}/@filepath{.dll}/@filepath{.dylib} files are
available, the @filepath{.so}/@filepath{.dll}/@filepath{.dylib} file is used.

While a @filepath{.zo}, @filepath{.so}, @filepath{.dll}, or
@filepath{.dylib} file is loaded, the current @scheme[load-relative]
directory is set to the directory of the original @scheme[_file].

If the original @scheme[_file] is loaded or a @filepath{.zo} variant is
loaded, the @tech{load handler} is called to load the file. If any
other kind of file is loaded, the @tech{extension-load handler} is
called.}


@defproc[(load/use-compiled [file path-string?]) any]{

Calls the current @tech{compiled-load handler} in tail position.}


@defparam[current-load-relative-directory path 
          (and/c path-string?
                 complete-path?)]{     

A parameter that is set by @scheme[load], @scheme[load-relative],
@scheme[load-extension], @scheme[load-relative-extension], and the
default @tech{compiled-load handler}, and used by
@scheme[load-relative], @scheme[load-relative-extension], and the
default @tech{compiled-load handler}.

When a new path or string is provided as the parameter's value, it is
immediately expanded (see @secref["pathutils"]) and converted to a
path. (The directory need not exist.)}


@defparam*[use-compiled-file-paths paths (listof path-string?) (listof path?)]{

A list of relative paths, which defaults to @scheme[(list
(string->path "compiled"))]. It is used by the @tech{compiled-load
handler} (see @scheme[current-load/use-compiled]).}


@defproc[(read-eval-print-loop) any]{

Starts a new @deftech{REPL} using the current input, output, and error
ports. The REPL wraps each expression to evaluate with
@schemeidfont{#%top-interaction}, which is normally bound to
@scheme[#%top-interaction], and it wraps each evaluation with a
continuation prompt using the default continuation prompt tag and
prompt handler (see @scheme[call-with-continuation-prompt]). The REPL
also wraps the read and print operations with a prompt for the default
tag whose handler ignores abort arguments and continues the loop. The
@scheme[read-eval-print-loop] procedure does not return until
@scheme[eof] is read, at which point it returns @|void-const|.

The @scheme[read-eval-print-loop] procedure can be configured through
the @scheme[current-prompt-read], @scheme[current-eval], and
@scheme[current-print] parameters.}


@defparam[current-prompt-read proc (-> any)]{

A parameter that determines a procedure that takes no arguments,
displays a prompt string, and returns a top-level form to
evaluate. This procedure is called by the read phase of
@scheme[read-eval-print-loop].  The default prompt read handler prints
@litchar{> } and returns the result of

@schemeblock[
(parameterize ((read-accept-reader #t))
  (read-syntax))
]}


@defparam[current-print proc (any/c -> any)]{

A parameter that determines the @deftech{print handler} that is called
 by @scheme[read-eval-print-loop] to print the result of an evaluation
 (and the result is ignored).

The default @tech{print handler} @scheme[print]s the value to the
 current output port (as determined by the
 @scheme[current-output-port] parameter) and then outputs a newline,
 except that it prints nothing when the value is @|void-const|.}


@defparam[current-compile proc (any/c boolean? . -> . compiled-expression?)]{

A parameter that determines the current @deftech{compilation handler}.
The @tech{compilation handler} is a procedure that takes a top-level form and
returns a compiled form; see see @secref["compilation-model"] for
more information on compilation.

The @tech{compilation handler} is called by @scheme[compile], and
indirectly by the default @tech{evaluation handler} and the default
@tech{load handler}.

The handler's second argument is @scheme[#t] if the compiled form will
be used only for immediate evaluation, or @scheme[#f] if the compiled
form may be saved for later use; the default compilation handler is
optimized for the special case of immediate evaluation.

When a compiled form is written to an output port, the written form
starts with @litchar{#~}.  These forms are essentially assembly code
for PLT Scheme, and reading such an form produces a compiled form (as
long as the @scheme[read-accept-compiled] parameter is set to
@scheme[#t]).

When a compiled form contains syntax object constants, the
@litchar{#~}-marshaled form drops source-location information and
properties (@secref["stxprops"]) for the @tech{syntax objects}.

Compiled code parsed from @litchar{#~} may contain references to
unexported or protected bindings from a module. At read time, such
references are associated with the current code inspector (see
@scheme[current-code-inspector]), and the code will only execute if
that inspector controls the relevant module invocation (see
@secref["modprotect"]).

A compiled-form object may contain @tech{uninterned} symbols (see
@secref["symbols"]) that were created by @scheme[gensym] or
@scheme[string->uninterned-symbol]. When the compiled object is read
via @litchar{#~}, each uninterned symbol in the original form is
mapped to a new uninterned symbol, where multiple instances of a
single symbol are consistently mapped to the same new symbol. The
original and new symbols have the same printed representation.

Due to the above restrictions, do not use @scheme[gensym] or
@scheme[string->uninterned-symbol] to construct an identifier for a
top-level or module binding. Instead, generate distinct identifiers
either with @scheme[generate-temporaries] or by applying the result of
@scheme[make-syntax-introducer] to an existing identifier.}


@defproc[(compile [top-level-form any/c]) compiled-expression?]{

Like @scheme[eval], but calls the current @tech{compilation handler} in
tail position with @scheme[top-level-form].}


@defproc[(compile-syntax [stx syntax?]) compiled-expression?]{

Like @scheme[eval-syntax], but calls the current @tech{compilation
handler} in tail position with @scheme[stx].}


@defproc[(compiled-expression? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a compiled form, @scheme[#f]
otherwise.}


@defboolparam[compile-enforce-module-constants on?]{

A parameter that determines how a module declaration is compiled. 

When constants are enforced, and when the macro-expanded body of a
module contains no @scheme[set!] assignment to a particular variable
defined within the module, then the variable is marked as constant
when the definition is evaluated. Afterward, the variable's value
cannot be assigned or undefined through @scheme[module->namespace],
and it cannot be defined by redeclaring the module.

Enforcing constants allows the compiler to inline some variable
values, and it allows the native-code just-in-time compiler to
generate code that skips certain run-time checks.}


@defboolparam[compile-allow-set!-undefined allow?]{

A parameter that determines how a @scheme[set!] expression is compiled
when it mutates a global variable. If the value of this parameter is a
true value, @scheme[set!] expressions for global variables are
compiled so that the global variable is set even if it was not
previously defined.  Otherwise, @scheme[set!] expressions for global
variables are compiled to raise the
@scheme[exn:fail:contract:variable] exception if the global variable
is not defined at the time the @scheme[set!] is performed.  Note that
this parameter is used when an expression is @italic{compiled}, not
when it is @italic{evaluated}.}

@defboolparam[eval-jit-enabled on?]{

A parameter that determines whether the native-code just-in-time
compiler (JIT) is enabled for code (compiled or not) that is passed to
the default evaluation handler.  The default is @scheme[#t], unless
the JIT is disabled through the @Flag{j}/@DFlag{no-jit} command-line
flag to stand-alone MzScheme (or MrEd), or through the
@as-index{@envvar{PLTNOMZJIT}} environment variable (set to any
value).}

@defboolparam[load-on-demand-enabled on?]{

A parameter that determines whether the default @tech{load handler}
sets @scheme[read-on-demand-source]. See @scheme[current-load] for
more information. The default is @scheme[#t], unless it is disabled
through the @Flag{d}/@DFlag{no-delay} command-line flag.}
