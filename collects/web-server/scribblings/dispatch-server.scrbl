#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "dispatch-server-unit.ss"]{Dispatching Server}
@(require (for-label web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/private/util
                     web-server/private/connection-manager
                     net/tcp-sig
                     scheme/tcp
                     web-server/web-server-sig))

The @web-server is just a configuration of a dispatching server.

@section{Dispatching Server Signatures}

@defmodule[web-server/private/dispatch-server-sig]{

The @schememodname[web-server/private/dispatch-server-sig] library
provides two signatures.

@defsignature[dispatch-server^ ()]{

The @scheme[dispatch-server^] signature is an alias for
@scheme[web-server^].

 @defproc[(serve) (-> void)]{
  Runs the server and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @scheme[ip] and
 @scheme[op].
 }
}

@defsignature[dispatch-server-config^ ()]{

 @defthing[port port-number?]{Specifies the port to serve on.}
 @defthing[listen-ip (or/c string? false/c)]{Passed to @scheme[tcp-listen].}
 @defthing[max-waiting integer?]{Passed to @scheme[tcp-accept].}
 @defthing[initial-connection-timeout integer?]{Specifies the initial timeout given to a connection.}
 @defproc[(read-request [c connection?]
                        [p port-number?]
                        [port-addresses 
                         (input-port? . -> . (values string? string?))])
          (values any/c boolean?)]{
  Defines the way the server reads requests off connections to be passed
  to @scheme[dispatch].
 }
 @defthing[dispatch (-> connection? any/c void)]{How to handle requests.}
}

}

@section{Dispatching Server Unit}

@defmodule[web-server/private/dispatch-server-unit]{

The @schememodname[web-server/private/dispatch-server-unit] module
provides the unit that actually implements a dispatching server.

@defthing[dispatch-server@ (unit/c (import tcp^ dispatch-server-config^) 
                                   (export dispatch-server^))]{
 Runs the dispatching server config in a very basic way, except that it uses
 @secref["connection-manager.ss"] to manage connections.
}

}

@section{Threads and Custodians}

The dispatching server runs in a dedicated thread. Every time a connection is initiated, a new thread is started to handle it.
Connection threads are created inside a dedicated custodian that is a child of the server's custodian. When the server is used to
provide servlets, each servlet also receives a new custodian that is a child of the server's custodian @bold{not} the connection
custodian.