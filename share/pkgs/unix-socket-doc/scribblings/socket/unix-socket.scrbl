#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     racket/unix-socket))

@title[#:tag "unix-socket"]{Unix Domain Sockets}
@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]

@defmodule[racket/unix-socket]

This library provides support for @deftech{unix domain sockets}
(specifically, sockets with family @tt{AF_UNIX} and type
@tt{SOCK_STREAM}).

@defthing[unix-socket-available?
          boolean?]{

A boolean value that indicates whether unix domain sockets are
available and supported on the current platform. The supported
platforms are Linux, Mac OS X, and variants of BSD. This library does
not currently support other Unix variants, and Windows does not have
unix domain sockets.
}

@defproc[(unix-socket-connect [socket-path unix-socket-path?])
         (values input-port? output-port?)]{

Connects to the unix domain socket associated with
@racket[socket-path] and returns an input port and output port for
communicating with the socket. The connection is closed when both
ports are closed.
}

@defproc[(unix-socket-path? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a valid unix domain socket path
for the current system. There are two kinds of socket paths:
filesystem paths and abstract socket names.

@itemlist[

@item{If @racket[v] is a path (@racket[path-string?]), the length of
@racket[v]'s corresponding absolute path must be less than or equal to
the platform-specific length (108 bytes on Linux, 104 bytes on BSD and
Mac OS X). Example: @racket["/tmp/mysocket"].}

@item{If @racket[v] is a bytestring (@racket[bytes?]), it represents
an abstract socket name, supported on ly on Linux. The first byte of
@racket[v] must be NUL, and its length must be less than or equal to
108 bytes. Such a value refers to a socket in the Linux abstract
socket namespace. Example: @racket[#"\0mysocket"].

Note that NUL bytes are allowed in abstract socket names. For example,
@racket[#"\0abc"] and @racket[#"\0abc\0"] are both valid---and
different---abstract socket names.}

]

Otherwise, returns @racket[#f].
}

@defproc[(unix-socket-listen [socket-path unix-socket-path?]
                             [backlog exact-nonnegative-integer? 4])
         unix-socket-listener?]{

Listen for connections on a unix domain socket bound to
@racket[socket-path], returning a listener that can be used to accept
incoming connections.

If @racket[socket-path] refers to a filesystem path, binding the
socket creates a file that must be deleted separately from closing the
listener.
}

@defproc[(unix-socket-listener? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a unix socket listener created
with @racket[unix-socket-listen]; @racket[#f] otherwise.

A unix socket listener acts as a synchronizable event. It is ready
when a client connection is ready to be accepted (see
@racket[unix-socket-accept]), and its synchronization result is the
listener itself.
}

@defproc[(unix-socket-accept [listener unix-socket-listener?])
         (values input-port? output-port)]{

Accepts a client connection for @racket[listener]. If no client
connection is waiting to be accepted, the call to
@racket[unix-socket-accept] will block.
}
