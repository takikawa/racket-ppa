#lang scribble/doc
@(require "common.ss"
          (for-label mzlib/os))

@mzlib[#:mode title os]

@defproc[(gethostname) string?]{

Returns a string for the current machine's hostname (including its
domain).}


@defproc[(getpid) exact-integer?]{

Returns an integer identifying the current process within the
operating system.}


@defproc[(truncate-file [file path-string?][n-bytes exact-nonnegative-integer? 0])
         void?]{

Truncates or extends the given @scheme[file] so that it is
@scheme[n-bytes] long. If the file does not exist, or if the process
does not have sufficient privilege to truncate the file, the
@scheme[exn:fail] exception is raised.

@bold{WARNING:} under Unix, the implementation assumes that the
system's @scheme[ftruncate] function accepts a @tt{long long} second
argument.}


