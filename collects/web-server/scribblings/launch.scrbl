#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "web-server.ss"]{Launching Servers}

@(require (for-label web-server/web-server
                     web-server/dispatchers/filesystem-map
                     web-server/web-config-unit
                     web-server/web-config-sig
                     web-server/private/dispatch-server-unit
                     web-server/private/dispatch-server-sig
                     web-server/dispatchers/dispatch
                     web-server/configuration/configuration-table)
          (prefix-in raw: (for-label net/tcp-unit))
          (prefix-in files: (for-label web-server/dispatchers/dispatch-files)))

@defmodule[web-server/web-server]{

This module provides functions for launching dispatching servers.

@defproc[(serve [#:dispatch dispatch dispatcher/c]
                [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                [#:port port integer? 80]
                [#:listen-ip listen-ip (or/c string? false/c) #f]
                [#:max-waiting max-waiting integer? 40]
                [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Constructs an appropriate @scheme[dispatch-server-config^], invokes the
 @scheme[dispatch-server@], and calls its @scheme[serve] function.
 
 The @scheme[#:tcp@] keyword is provided for building an SSL server. See @secref["faq:https"].
}

Here's an example of a simple web server that serves files
from a given path:

@schemeblock[
(define (start-file-server base)
  (serve
   #:dispatch
   (files:make
    #:url->path (make-url->path base)
    #:path->mime-type
    (lambda (path)
      #"application/octet-stream"))
   #:port 8080))
]

@defproc[(serve/ports [#:dispatch dispatch dispatcher/c]
                      [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                      [#:ports ports (listof integer?) (list 80)]
                      [#:listen-ip listen-ip (or/c string? false/c) #f]
                      [#:max-waiting max-waiting integer? 40]
                      [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @scheme[serve] multiple times, once for each @scheme[port], and returns
 a function that shuts down all of the server instances.
}

@defproc[(serve/ips+ports [#:dispatch dispatch dispatcher/c]
                          [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@]
                          [#:ips+ports ips+ports (listof (cons/c (or/c string? false/c) (listof integer?))) (list (cons #f (list 80)))]
                          [#:max-waiting max-waiting integer? 40]
                          [#:initial-connection-timeout initial-connection-timeout integer? 60])
         (-> void)]{
 Calls @scheme[serve/ports] multiple times, once for each @scheme[ip], and returns
 a function that shuts down all of the server instances.
}
                  
@defproc[(serve/web-config@ [config@ (unit/c (import) (export web-config^))]
                            [#:tcp@ tcp@ (unit/c (import) (export tcp^)) raw:tcp@])
         (-> void)]{
 Starts the @web-server with the settings defined by the given @scheme[web-config^] unit.
        
 It is very useful to combine this with @scheme[configuration-table->web-config@] and @scheme[configuration-table-sexpr->web-config@]:
 
 @schemeblock[
  (serve/web-config@
   (configuration-table->web-config@ 
    default-configuration-table-path))]
}

@defproc[(do-not-return) void]{
 This function does not return. If you are writing a script to load the @web-server
 you are likely to want to call this functions at the end of your script.
}

}

@include-section["servlet-env-int.scrbl"]
