#lang scribble/doc
@(require "mz.rkt" (for-label racket/system))

@title[#:tag "subprocess"]{Processes}

@defproc*[([(subprocess [stdout (or/c (and/c output-port? file-stream-port?) #f)]
                        [stdin (or/c (and/c input-port? file-stream-port?) #f)]
                        [stderr (or/c (and/c output-port? file-stream-port?) #f 'stdout)]
                        [command path-string?]
                        [arg (or/c path? string-no-nuls? bytes-no-nuls?)] ...)
            (values subprocess?
                    (or/c (and/c input-port? file-stream-port?) #f)
                    (or/c (and/c output-port? file-stream-port?) #f)
                    (or/c (and/c input-port? file-stream-port?) #f))]
           [(subprocess [stdout (or/c (and/c output-port? file-stream-port?) #f)]
                        [stdin (or/c (and/c input-port? file-stream-port?) #f)]
                        [stderr (or/c (and/c output-port? file-stream-port?) #f)]
                        [command path-string?]
                        [exact 'exact]
                        [arg string?])
            (values subprocess?
                    (or/c (and/c input-port? file-stream-port?) #f)
                    (or/c (and/c output-port? file-stream-port?) #f)
                    (or/c (and/c input-port? file-stream-port?) #f))])]{

Creates a new process in the underlying operating system to execute
@racket[command] asynchronously. See also @racket[system] and
@racket[process] from @racketmodname[racket/system].

The @racket[command] argument is a path to a program executable, and
the @racket[arg]s are command-line arguments for the program. See
@racket[find-executable-path] for locating an executable based on
the @envvar{PATH} environment variable. On
Unix and Mac OS X, command-line arguments are passed as byte strings,
and string @racket[arg]s are converted using the current locale's
encoding (see @secref["encodings"]). On Windows, command-line
arguments are passed as strings, and bytes strings are converted using
UTF-8.

On Windows, the first @racket[arg] can be replaced with
@indexed-racket['exact], which triggers a Windows-specific behavior:
the sole @racket[arg] is used exactly as the command-line for the
subprocess. Otherwise, on Windows, a command-line string is
constructed from @racket[command] and @racket[arg] so that a typical
Windows console application can parse it back to an array of
arguments. If @racket['exact] is provided on a non-Windows platform,
the @exnraise[exn:fail:contract].

@margin-note{For information on the Windows command-line conventions,
search for ``command line parsing'' at
@tt{http://msdn.microsoft.com/}.}

When provided as a port, @racket[stdout] is used for the launched
process's standard output, @racket[stdin] is used for the process's
standard input, and @racket[stderr] is used for the process's standard
error.  All provided ports must be file-stream ports. Any of the ports
can be @racket[#f], in which case a system pipe is created and
returned by @racket[subprocess]. The @racket[stderr] argument can be 
@racket['stdout], in which case the same file-stream port or system pipe
that is supplied as standard output is also used for standard error.
For each port or @racket['stdout] that is provided, no
pipe is created and the corresponding returned value is @racket[#f].

The @racket[subprocess] procedure returns four values:

@itemize[

 @item{a @deftech{subprocess} value representing the created process;}

 @item{an input port piped from the process's standard output, or
 @racket[#f] if @racket[stdout-output-port] was a port;}

 @item{an output port piped to the process standard input, or
 @racket[#f] if @racket[stdin-input-port] was a port;}

 @item{an input port piped from the process's standard error, or
 @racket[#f] if @racket[stderr-output-port] was a port or @racket['stdout].}

]

@bold{Important:} All ports returned from @racket[subprocess] must be
explicitly closed, usually with @racket[close-input-port] or
@racket[close-output-port].

The returned ports are @tech{file-stream ports} (see
@secref["file-ports"]), and they are placed into the management of
the current custodian (see @secref["custodians"]).  The
@exnraise[exn:fail] when a low-level error prevents the spawning of a
process or the creation of operating system pipes for process
communication.

If the @racket[subprocess-group-enabled] parameter's value is true,
then the new process is created as a new OS-level process group. In
that case, @racket[subprocess-kill] attempts to terminate all
processes within the group, which may include additional processes
created by the subprocess. See @racket[subprocess-kill] for details,
and see @racket[subprocess-group-enabled] for additional caveats.

The @racket[current-subprocess-custodian-mode] parameter determines
whether the subprocess itself is registered with the current
@tech{custodian} so that a custodian shutdown calls
@racket[subprocess-kill] for the subprocess.

A subprocess can be used as a @tech{synchronizable event} (see @secref["sync"]).
A subprocess value is @tech{ready for synchronization} when
@racket[subprocess-wait] would not block; @resultItself{subprocess value}.}


@defproc[(subprocess-wait [subproc subprocess?]) void?]{

Blocks until the process represented by @racket[subproc]
terminates. The @racket[subproc] value also can be used with
@racket[sync] and @racket[sync/timeout].}


@defproc[(subprocess-status [subproc subprocess?]) 
         (or/c 'running
               exact-nonnegative-integer?)]{

Returns @indexed-racket['running] if the process represented by
@racket[subproc] is still running, or its exit code otherwise. The
exit code is an exact integer, and @racket[0] typically indicates
success. If the process terminated due to a fault or signal, the exit
code is non-zero.}


@defproc[(subprocess-kill [subproc subprocess?] [force? any/c]) void?]{

Terminates the subprocess represented by @racket[subproc]. The precise
action depends on whether @racket[force?] is true, whether the process
was created in its own group by setting the
@racket[subprocess-group-enabled] parameter to a true value, and the
current platform:

@itemlist[

 @item{@racket[force?] is true, not a group, all platforms: Terminates
       the process if the process still running.}

 @item{@racket[force?] is false, not a group, on Unix or Mac OS X:
       Sends the process an interrupt signal instead of a kill
       signal.}

 @item{@racket[force?] is false, not a group, on Windows: No action
       is taken.}

 @item{@racket[force?] is true, a group, on Unix or Mac OS X:
       Terminates all processes in the group, but only if
       @racket[subprocess-status] has never produced a
       non-@racket['running] result for the subprocess and only if
       functions like @racket[subprocess-wait] and @racket[sync] have
       not detected the subprocess's completion. Otherwise, no action
       is taken (because the immediate process is known to have
       terminated while the continued existence of the group is
       unknown).}

 @item{@racket[force?] is true, a group, on Windows: Terminates
       the process if the process still running.}

 @item{@racket[force?] is false, a group, on Unix or Mac OS X: The
       same as when @racket[force?] is @racket[#t], but when the group
       is sent a signal, it is an interrupt signal instead of a kill
       signal.}

 @item{@racket[force?] is false, a group, on Windows: All processes
       in the group receive a CTRL-BREAK signal (independent of
       whether the immediate subprocess has terminated).}

]

If an error occurs during termination, the @exnraise[exn:fail].}


@defproc[(subprocess-pid [subproc subprocess?]) exact-nonnegative-integer?]{

Returns the operating system's numerical ID (if any) for the process
represented by @racket[subproc]. The result is valid only as long as
the process is running.}


@defproc[(subprocess? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a subprocess value, @racket[#f]
otherwise.}


@defparam[current-subprocess-custodian-mode mode (or/c #f 'kill 'interrupt)]{

A @tech{parameter} that determines whether a subprocess (as created by
@racket[subprocess] or wrappers like @racket[process]) is registered
with the current @tech{custodian}. If the parameter value is
@racket[#f], then the subprocess is not registered with the
custodian---although any created ports are registered. If the
parameter value is @racket['kill] or @racket['interrupt], then the
subprocess is shut down through @racket[subprocess-kill], where
@racket['kill] supplies a @racket[#t] value for the @racket[_force?]
argument and @racket['interrupt] supplies a @racket[#f] value. The
shutdown may occur either before or after ports created for the
subprocess are closed.

Custodian-triggered shutdown is limited by details of process handling
in the host system. For example, @racket[process] and @racket[system]
may create an intermediate shell process to run a program, in which
case custodian-based termination shuts down the shell process and
probably not the process started by the shell. See also
@racket[subprocess-kill]. Process groups (see
@racket[subprocess-group-enabled]) can address some limitations, but
not all of them.}


@defboolparam[subprocess-group-enabled on?]{

A @tech{parameter} that determines whether a subprocess is created as
a new process group. See @racket[subprocess-kill] for more information.

Beware that creating a group may interfere with the job control in an
interactive shell, since job control is based on process groups.}


@defproc[(shell-execute [verb (or/c string? #f)]
                        [target string?]
                        [parameters string?]
                        [dir path-string?]
                        [show-mode symbol?])
         #f]{

@index['("ShellExecute")]{Performs} the action specified by @racket[verb]
on @racket[target] in Windows. For platforms other than Windows, the
@exnraise[exn:fail:unsupported].

For example,

@racketblock[
(shell-execute #f "http://racket-lang.org" ""
               (current-directory) 'sw_shownormal)
]

Opens the Racket home page in a browser window.

The @racket[verb] can be @racket[#f], in which case the operating
system will use a default verb. Common verbs include @racket["open"],
@racket["edit"], @racket["find"], @racket["explore"], and
@racket["print"].

The @racket[target] is the target for the action, usually a filename
path. The file could be executable, or it could be a file with a
recognized extension that can be handled by an installed application.

The @racket[parameters] argument is passed on to the system to perform
the action. For example, in the case of opening an executable, the
@racket[parameters] is used as the command line (after the executable
name).

The @racket[dir] is used as the current directory when performing the
action.

The @racket[show-mode] sets the display mode for a Window affected by
the action. It must be one of the following symbols; the description
of each symbol's meaning is taken from the Windows API documentation.

@itemize[

 @item{@indexed-racket['sw_hide] or @indexed-racket['SW_HIDE] ---
 Hides the window and activates another window.}

 @item{@indexed-racket['sw_maximize] or @indexed-racket['SW_MAXIMIZE]
 --- Maximizes the window.}

 @item{@indexed-racket['sw_minimize] or @indexed-racket['SW_MINIMIZE]
 --- Minimizes the window and activates the next top-level window in
 the z-order.}

 @item{@indexed-racket['sw_restore] or @indexed-racket['SW_RESTORE]
 --- Activates and displays the window. If the window is minimized or
 maximized, Windows restores it to its original size and position.}

 @item{@indexed-racket['sw_show] or @indexed-racket['SW_SHOW] ---
 Activates the window and displays it in its current size and
 position.}

 @item{@indexed-racket['sw_showdefault] or
 @indexed-racket['SW_SHOWDEFAULT] --- Uses a default.}

 @item{@indexed-racket['sw_showmaximized] or
 @indexed-racket['SW_SHOWMAXIMIZED] --- Activates the window and
 displays it as a maximized window.}

 @item{@indexed-racket['sw_showminimized] or
 @indexed-racket['SW_SHOWMINIMIZED] --- Activates the window and
 displays it as a minimized window.}

 @item{@indexed-racket['sw_showminnoactive] or
 @indexed-racket['SW_SHOWMINNOACTIVE] --- Displays the window as a
 minimized window. The active window remains active.}

 @item{@indexed-racket['sw_showna] or @indexed-racket['SW_SHOWNA] ---
 Displays the window in its current state. The active window remains
 active.}

 @item{@indexed-racket['sw_shownoactivate] or
 @indexed-racket['SW_SHOWNOACTIVATE] --- Displays a window in its most
 recent size and position. The active window remains active.}

 @item{@indexed-racket['sw_shownormal] or
 @indexed-racket['SW_SHOWNORMAL] --- Activates and displays a
 window. If the window is minimized or maximized, Windows restores it
 to its original size and position.}

 ]

If the action fails, the @exnraise[exn:fail]. If the action succeeds,
the result is @racket[#f].

In future versions of Racket, the result may be a subprocess value if
the operating system did returns a process handle (but if a subprocess
value is returned, its process ID will be @racket[0] instead of the
real process ID).}

@; ----------------------------------------------------------------------

@section{Simple Subprocesses}

@note-lib[racket/system]

@defproc[(system [command (or/c string-no-nuls? bytes-no-nuls?)]) boolean?]{

Executes a Unix, Mac OS X, or Windows shell command synchronously
(i.e., the call to @racket[system] does not return until the
subprocess has ended). The @racket[command] argument is a string or
byte string containing no nul characters. If the command succeeds, the
return value is @racket[#t], @racket[#f] otherwise.

See also @racket[current-subprocess-custodian-mode] and
@racket[subprocess-group-enabled], which affect the subprocess used to
implement @racket[system].

The resulting process writes to @racket[(current-output-port)], reads
from @racket[(current-input-port)], and logs errors to
@racket[(current-error-port)]. To gather the process's non-error
output to a string, for example, use @racket[with-output-to-string],
which sets @racket[current-output-port] while calling the given
function:

@racketblock[
(with-output-to-string (lambda () (system "date")))
]}


@defproc*[([(system* [command path-string?]
                     [arg (or/c path? string-no-nuls? bytes-no-nuls?)] ...)
            boolean?]
           [(system* [command path-string?] [exact 'exact] [arg string?])
            boolean?])]{

Like @racket[system], except that @racket[command] is a filename that
is executed directly (instead of through a shell command; see
@racket[find-executable-path] for locating an executable based on
the @envvar{PATH} environment variable), and the
@racket[arg]s are the arguments. The executed file is passed the
specified string arguments (which must contain no nul
characters).

On Windows, the first argument after @racket[command] can be
@racket['exact], and the final @racket[arg] is a complete command
line. See @racket[subprocess] for details.}


@defproc[(system/exit-code [command (or/c string-no-nuls? bytes-no-nuls?)])
         byte?]{

Like @racket[system], except that the result is the exit code returned
by the subprocess. A @racket[0] result normally indicates success.}


@defproc*[([(system*/exit-code [command path-string?]
                               [arg (or/c path? string-no-nuls? bytes-no-nuls?)] ...)
            byte?]
           [(system*/exit-code [command path-string?]
                               [exact 'exact] [arg string?])
            byte?])]{

Like @racket[system*], but returns the exit code like
@racket[system/exit-code].}


@defproc[(process [command (or/c string-no-nuls? bytes-no-nuls?)])
         (list input-port?
               output-port?
               exact-nonnegative-integer?
               input-port?
               ((or/c 'status 'wait 'interrupt 'kill) . -> . any))]{

Executes a shell command asynchronously (using @exec{sh} on Unix
and Mac OS X, @exec{cmd} on Windows). The result is a list of five
values:

@itemize[

 @item{an input port piped from the subprocess's standard output,}

 @item{an output port piped to the subprocess standard input,} 

 @item{the system process id of the subprocess,}

 @item{an input port piped from the subprocess's standard
       error, and}

 @item{a procedure of one argument, either @racket['status], @racket['wait],
 @racket['interrupt], @racket['exit-code] or @racket['kill]:

   @itemize[

   @item{@racket['status] returns the status of the subprocess as one
    of @racket['running], @racket['done-ok], or
    @racket['done-error].}

   @item{@racket['exit-code] returns the integer exit code of the
    subprocess or @racket[#f] if it is still running.}

   @item{@racket['wait] blocks execution in the current thread until
    the subprocess has completed.}

   @item{@racket['interrupt] sends the subprocess an interrupt signal
    on @|AllUnix|, and takes no action on Windows. The result is
    @|void-const|.

     @margin-note{On Unix and Mac OS X, if @racket[command] runs a
     single program, then @exec{sh} typically runs the program in
     such a way that it replaces @exec{sh} in the same process. For
     reliable and precise control over process creation, however, use
     @racket[process*].}}

   @item{@racket['kill] terminates the subprocess and returns
     @|void-const|.  Note that the immediate process created by
     @racket[process] is a shell process that may run another program;
     terminating the shell process may not terminate processes that
     the shell starts, particularly on Windows.}

   ]}

]

@bold{Important:} All three ports returned from @racket[process] must
be explicitly closed with @racket[close-input-port] or
@racket[close-output-port].

See also @racket[current-subprocess-custodian-mode] and
@racket[subprocess-group-enabled], which affect the subprocess used to
implement @racket[process]. In particular, the @racket['interrupt] and
@racket['kill] process-control messages are implemented via
@racket[subprocess-kill], so they can affect a process group instead
of a single process.}


@defproc*[([(process* [command path-string?]
                      [arg (or/c path? string-no-nuls? bytes-no-nuls?)] ...)
            list?]
           [(process* [command path-string?] [exact 'exact] [arg string?])
            list?])]{

Like @racket[process], except that @racket[command] is a filename that
is executed directly like @racket[system*], and the @racket[arg]s are the arguments. On
Windows, as for @racket[system*], the first @racket[arg] can be
replaced with @racket['exact].}


@defproc[(process/ports [out (or/c #f output-port?)]
                        [in (or/c #f input-port?)]
                        [error-out (or/c #f output-port? 'stdout)]
                        [command (or/c path? string-no-nuls? bytes-no-nuls?)])
         list?]{

Like @racket[process], except that @racket[out] is used for the
process's standard output, @racket[in] is used for the process's
standard input, and @racket[error-out] is used for the process's
standard error.  Any of the ports can be @racket[#f], in which case a
system pipe is created and returned, as in @racket[process]. If
@racket[error-out] is @racket['stdout], then standard error is
redirected to standard output.  For each port or @racket['stdout] that
is provided, no pipe is created, and the corresponding value in the
returned list is @racket[#f].}

@defproc*[([(process*/ports [out (or/c #f output-port?)]
                            [in (or/c #f input-port?)]
                            [error-out (or/c #f output-port? 'stdout)]
                            [command path-string?]
                            [arg (or/c path? string-no-nuls? bytes-no-nuls?)]
                            ...)
            list?]
           [(process*/ports [out (or/c #f output-port?)]
                            [in (or/c #f input-port?)]
                            [error-out (or/c #f output-port? 'stdout)]
                            [command path-string?]
                            [exact 'exact]
                            [arg string?])
            list?])]{

Like @racket[process*], but with the port handling of
@racket[process/ports].}

@; ----------------------------------------------------------------------

@;section{Contract Auxiliaries}

@;note-lib[racket/system]

The contracts of @racket[system] and related functions may signal a
contract error with references to the following functions.

@defproc[(string-no-nuls? [x any/c]) boolean?]{
Ensures that @racket[x] is a string and does not contain @racket["\0"].}

@defproc[(bytes-no-nuls? [x any/c]) boolean?]{
Ensures that @racket[x] is a byte-string and does not contain @racket[#"\0"].}
