#lang scribble/doc

@(require scribble/manual
         (for-label scheme
                    errortrace
                    errortrace/errortrace-lib
                    errortrace/stacktrace))

@title[#:tag "top"]{@bold{Errortrace}: Debugging and Profiling}

@bold{Errortrace} is a stack-trace-on-exceptions, profiler, and
coverage tool for MzScheme. It is not a complete debugger; DrScheme
provides more. Meanwhile, using Errortrace might be better than
MzScheme's limited stack-trace reporting.

@table-of-contents[]

@; ----------------------------------------------

@section[#:tag "quick-instructions"]{Quick Instructions}

@itemize{@item{Throw away @filepath{.zo} versions of your source.}
              
         @item{Prefix your program with
               @schemeblock[(require errortrace)]
               or start MzScheme with the @Flag{l} option before the
               arguments to load your program:
               @commandline{mzscheme -l errortrace ...}

               If you want to use MzScheme interactively, include the
               @Flag{i} flag first:
               @commandline{mzscheme -i -l errortrace}}

         @item{When an exception occurs, the exception handler prints
               something like a stack trace, most recent contexts first.}
         }

The @schememodname[errortrace] module is strange; don't import it
into another module. Instead, the @schememodname[errortrace] 
module is meant to be invoked from the top-level, so that it can install 
an evaluation handler, exception handler, etc.

To reuse parts of the code of @schememodname[errortrace], import
@schememodname[errortrace/errortrace-lib]. It contains all of the
bindings described here, but does not set the compilation handler or
the error display handler.

@; ----------------------------------------------

@section[#:tag "installing-errortrace"]{Installing Errortrace}

Invoking the 
@schememodname[errortrace] module sets the compilation
handler to instrument Scheme source code.  It also sets the error 
display handler to report source information for an exception, and it
sets the @scheme[use-compiled-file-paths] parameter to trigger the use
of Errortrace-specific @filepath{.zo} files.

  NOTE: @schememodname[errortrace] has no effect on code 
  loaded as compiled byte code (i.e., from a @filepath{.zo} file) or
  native code (i.e., from a @filepath{.dll}, @filepath{.so} or 
  @filepath{.dylib} file). You can use the @DFlag{mode errortrace} flag
  to @exec{setup-plt} to create @filepath{.zo} files with
  Errortrace information.

Explicitly requiring @schememodname[errortrace] within a module is
generally a bad idea, since @schememodname[errortrace] sets various
parameters.

@; ---------------------------------------------
@section[#:tag "using-errortrace"]{Using Errortrace}

@defmodule[errortrace #:use-sources (errortrace/errortrace-lib)]

@; ---------------------------------------------

@subsection[#:tag "instrumentation-and-profiling"]{Instrumentation and Profiling}

By default, @schememodname[errortrace] only instruments for
stack-trace-on-exception. Profiling and coverage need to be enabled
separately.

@defboolparam[instrumenting-enabled on?]{

A parameter that determines whether tracing instrumentation is
enabled, @scheme[#t] by default.  Affects only the way that source code is
compiled, not the way that exception information is reported. The
instrumentation for storing exception information slows most programs
by a factor of 2 or 3.}

@defboolparam[profiling-enabled on?]{

Errortrace's profiling instrumentation is @scheme[#f] by default. To use it,
you also need to ensure that @scheme[instrumenting-enabled] is on.}

@defboolparam[profiling-record-enabled on?]{

Enables/disables the recording of profiling info for the instrumented code.
The default is @scheme[#t].}

Profiling information is accumulated in a hash table.  If a procedure
is redefined, new profiling information is accumulated for the new
version of the procedure, but the old information is also preserved.

Depending of the source program, profiling usually induces a factor of
2 to 4 slowdown in addition to any slowdown from the exception
information instrumentation.

@defproc[(output-profile-results [paths? any/c] [sort-time? any/c]) void?]{
                                                                      
Gets the current profile results using @scheme[get-profile-results] and 
displays them.  It optionally shows paths information (if it is recorded),
and sorts by either time or call counts.}

@defproc[(get-profile-results) list?]{

Returns a list of lists that contain all profiling information accumulated
so far:

@itemize{
   @item{the number of times a procedure was called.}

   @item{the number of milliseconds consumed by the procedure's body across
         all calls (including the time consumed by any nested non-tail call
         within the procedure, but not including time consumed by a
         tail-call from the procedure).}

   @item{an inferred name (or @scheme[#f]) for the procedure.}

   @item{the procedure's source in the form of a syntax object (which might,
         in turn, provide a source location file and position).}

   @item{optionally, a list of unique call paths (i.e. stack traces) 
         recorded if @scheme[profile-paths-enabled] is set to @scheme[#t].
         Each call path is a pair of 
         @itemize{
           @item{a count (the number of times the path occurred), and}
                
           @item{a list containing two-element lists. Each two-element list
                 contains 
                 @itemize{
                   @item{the calling procedure's name or source expression,
                         and}
                   @item{the calling procedure's source file or @scheme[#f].}}
                }
        }
        Collecting this information is relatively expensive.}
}}

@defboolparam[profile-paths-enabled on?]{

Enables/disables collecting path information for profiling. The default is
@scheme[#f], but setting the parameter to @scheme[#t] immediately affects
all procedures instrumented for profiling information.}

@defproc[(clear-profile-results) void?]{

Clears accumulated profile results.}

@; ------------------------------------------------

@subsection[#:tag "coverage"]{Coverage}

Errortrace can produce coverage information in two flavors: both count
the number of times each expression in the source was used during
execution. The first flavor uses a simple approach, where each
expression is counted when executed; the second one uses the same
annotations that the profiler uses, so only function bodies are
counted. To see the difference between the two approaches, try this
program:

@schemeblock[(define (foo x) (if x 1 2))
             (equal? (foo #t) 1)]

The first approach will produce exact results, but it is more
expensive; use it when you want to know how covered your code is (when
the expected counts are small).  The second approach produces coarser
results (which, in the above case, will miss the @scheme[2] expression),
but is less expensive; use it when you want to use the counts for
profiling (when the expected counts are large).

@deftogether[(
  @defboolparam[coverage-counts-enabled on?]
  @defboolparam[execute-counts-enabled on?])]{

Parameters that determine if the first (exact coverage) or second
(profiler-based coverage) are enabled. Remember that setting
@scheme[instrumenting-enabled] to @scheme[#f] also disables both.}

@deftogether[(
  @defproc[(get-coverage-counts) list?]
  @defproc[(get-execute-counts) list?])]{
                                         
Returns a list of pairs, one for each instrumented expression.  The
first element of the pair is a @scheme[syntax?] object (usually containing
source location information) for the original expression, and the
second element of the pair is the number of times that the
expression has been evaluated.  These elements are destructively
modified, so to take a snapshot you will need to copy them.}

@deftogether[(
  @defproc[(annotate-covered-file 
            [filename-path path-string?]
            [display-string (or/c string? false/c) #f]) 
           void?]
  @defproc[(annotate-executed-file
            [filename-path path-string?]
            [display-string (or/c string? false/c) "^.,"]) 
           void?])]{
                                                                     
Writes the named file to the @scheme[current-output-port], inserting an
additional line between each source line to reflect execution counts
(as reported by @scheme[get-coverage-counts] or @scheme[get-execute-counts]).
The optional @scheme[display-string] is used for the annotation: the first
character is used for expressions that were visited 0 times, the
second character for 1 time, ..., and the last character for
expressions that were visited more times.  It can also be @scheme[#t]
for a maximal display (@scheme["012...9ABC...Z"]), or @scheme[#f] for
a minimal display (@scheme["#-"]).}

@; ------------------------------------------------------

@subsection[#:tag "other-errortrace-bindings"]{Other Errortrace Bindings}

The @schememodname[errortrace] module also exports:

@defproc[(print-error-trace [output-port output-port?] [exn exn?]) void?]{

The @scheme[print-error-trace] procedure takes a port and exception and
prints the Errortrace-collected debugging information contained in the
exception. It is used by the exception handler installed by
Errortrace.}
  
@defparam[error-context-display-depth d integer?]{The 
@scheme[error-context-display-depth] parameter controls how much context
Errortrace's exception handler displays. The default value is 10,000.}


@; ------------------------------------------------------

@section[#:tag "errortrace-library"]{Errortrace Library}

@defmodule[errortrace/errortrace-lib]{

The @schememodname[errortrace/errortrace-lib] module exports all of the
exports of @schememodname[errortrace], plus a few more. It does
not install any handlers.}

The additional exports are as follows:

@defproc[(errortrace-compile-handler (stx any/c) (immediate-eval? any/c))
         compiled-expression?]{

Compiles @scheme[stx] using the compilation handler that was active
when the @schememodname[errortrace/errortrace-lib] module was
executed, but first instruments the code for Errortrace information.
The code is instrumented only if @scheme[(namespace-module-registry
(current-namespace))] is the same as when the
@schememodname[errortrace/errortrace-lib] module was executed. This
procedure is suitable for use as a compilation handler via
@scheme[current-compile].}

@defproc[(errortrace-error-display-handler (string string?) (exn exn?)) void?]{

Displays information about the exception; this procedure is suitable
for use as an error display handler.}

@defproc[(errortrace-annotate (stx any/c)) any/c]{

Macro-expands and instruments the given top-level form. If the form is
a module named @schemeidfont{errortrace-key}, no instrumentation is
applied. This annotation function is used by
@scheme[errortrace-compile-handler].}

@defproc[(annotate-top [stx any/c][phase-level exact-integer?]) any/c]{

Like @scheme[errortrace-annotate], but given an explicit phase level
for @scheme[stx]; @scheme[(namespace-base-phase)] is typically the
right value for the @scheme[phase-level] argument.

Unlike @scheme[errortrace-annotate], there no special case for
a module named @scheme[errortrace-key]. Also, if @scheme[stx] is a module
declaration, it is not enriched with imports to explicitly load
Errortrace run-time support.}

@; -----------------------------------------------

@section[#:tag "stacktrace"]{Re-using Errortrace Stack Tracing}

@(define-syntax-rule (schemein id) (sigelem stacktrace-imports^ id))
@(define-syntax-rule (schemeout id) (sigelem stacktrace^ id))

@defmodule[errortrace/stacktrace]{
The errortrace collection also includes a
@schememodname[errortrace/stacktrace] library. It exports
the @scheme[stacktrace@] unit, its import signature
@scheme[stacktrace-imports^], and its export signature
@scheme[stacktrace^].}

@defthing[stacktrace@ unit?]{

Imports @scheme[stacktrace-imports^] and exports @scheme[stacktrace^].}


@defsignature[stacktrace^ ()]{

@deftogether[(
  @defproc[(annotate (stx syntax?) (phase-level exact-integer?)) syntax?]
  @defproc[(annotate-top (stx syntax?) (phase-level exact-integer?)) syntax?])]{

Annotate expressions with errortrace information. The
@schemeout[annotate-top] function should be called with a top-level
expression, and @schemeout[annotate] should be called with a nested
expression (e.g., by @schemein[initialize-profile-point]).  The
@scheme[phase-level] argument indicates the phase level of the
expression, typically @scheme[(namespace-base-phase)] for a top-level
expression.}

@deftogether[(
  @defproc[(make-st-mark (syntax syntax?)) st-mark?]
  @defproc[(st-mark-source (st-mark st-mark?)) syntax?]
  @defproc[(st-mark-bindings (st-mark st-mark?)) list?])]{

The @schemeout[st-mark-source] and @schemeout[st-mark-bindings]
functions extract information from a particular kind of value. The
value must be created by @schemeout[make-st-mark]. The
@schemeout[st-mark-source] extracts the value originally provided to
the expression-maker, and @schemeout[st-mark-bindings] returns local
binding information (if available) as a list of two element (syntax?
any/c) lists. The @schemeout[st-mark-bindings] function is currently
hardwired to return @scheme[null]. }

}

@defsignature[stacktrace-imports^ ()]{

@defproc[(with-mark (source-stx any/c) (dest-stx any/c)) any/c]{

Called by @schemeout[annotate] and @schemeout[annotate-top] to wrap
expressions with @scheme[with-continuation-mark]. The first argument
is the source expression and the second argument is the expression to
be wrapped.}

@defboolparam[test-coverage-enabled on?]{

Determines if the test coverage annotation is inserted into the code.
This parameter controls how compilation happens---it does not affect the
dynamic behavior of the already compiled code. If the parameter is set,
calls to @schemein[test-covered] are inserted into the code (and
@schemein[initialize-test-coverage-point] is called during compilation).
If not, no calls to test-covered are inserted.}

@defproc[(test-covered (key any/c)) void?]{

During execution of the program, this is called for each point with
the key for that program point that was passed to
@schemein[initialize-test-coverage-point].}

@defproc[(initialize-test-coverage-point (key any/c) (stx any/c)) void?]{

During compilation of the program, this function is called with each
sub-expression of the program. The first argument is a special key
used to identify this program point. The second argument is the
syntax of this program point.}

@defthing[profile-key any/c]{

Only used for profiling paths.}

@defboolparam[profiling-enabled on?]{

Determines if profiling information is currently collected (affects
the behavior of compiling the code---does not affect running code).
If this always returns @scheme[#f], the other profiling functions are
never called.}

@defproc[(initialize-profile-point (key any/c) 
                                   (name (or/c syntax? false/c))
                                   (stx any/c))
         void?]{

Called as the program is compiled for each profiling point that
might be encountered during the program's execution. The first
argument is a key identifying this code. The second argument is the
inferred name at this point and the final argument is the syntax of
this expression.}

@defproc[(register-profile-start (key any/c)) (or/c number? false/c)]{

Called when some profiled code is about to be executed. If the
result is a number, it is expected to be the current number of
milliseconds. @scheme[key] is unique to this fragment of code---it is
the same key passed to @schemein[initialize-profile-point] for this code
fragment.}

@defproc[(register-profile-done (key any/c)
                                (start (or/c number? false/c)))
                                void?]{

This function is called when some profiled code is finished executing.

Note that @schemein[register-profile-start] and
@schemein[register-profile-done] can be called in a nested manner; in
this case, the result of @schemein[register-profile-start] should be
@scheme[#f].}

}
