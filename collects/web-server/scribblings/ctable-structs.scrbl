#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "configuration-table-structs.ss"]{Configuration Table Structure}
@(require (for-label web-server/configuration/configuration-table-structs
                     web-server/http
                     net/url
                     web-server/private/util))

@defmodule[web-server/configuration/configuration-table-structs]{

This module provides the following structures that
represent a standard configuration (see @secref["web-server-unit.ss"]) of the @web-server .
The contracts on this structure influence the valid types of values in
the configuration table S-expression file format described in
@secref["configuration-table.ss"].

@defstruct[configuration-table
           ([port port-number?]
            [max-waiting natural-number/c]
            [initial-connection-timeout natural-number/c]
            [default-host host-table?]
            [virtual-hosts (listof (cons/c string? host-table?))])]

@defstruct[host-table
           ([indices (listof string?)]
            [log-format symbol?]
            [messages messages?]
            [timeouts timeouts?]
            [paths paths?])]

@defstruct[host
           ([indices (listof string?)]
            [log-format symbol?]
            [log-path (or/c false/c path-string?)]
            [passwords (or/c false/c path-string?)]
            [responders responders?]
            [timeouts timeouts?]
            [paths paths?])]

@defstruct[responders
           ([servlet (url? any/c . -> . response/c)]
            [servlet-loading (url? any/c . -> . response/c)]
            [authentication (url? (cons/c symbol? string?) . -> . response/c)]
            [servlets-refreshed (-> response/c)]
            [passwords-refreshed (-> response/c)]
            [file-not-found (request? . -> . response/c)]
            [protocol (url? . -> . response/c)]
            [collect-garbage (-> response/c)])]

@defstruct[messages
           ([servlet string?]
            [authentication string?]
            [servlets-refreshed string?]
            [passwords-refreshed string?]
            [file-not-found string?]
            [protocol string?]
            [collect-garbage string?])]

@defstruct[timeouts
           ([default-servlet number?]
            [password number?]
            [servlet-connection number?]
            [file-per-byte number?]
            [file-base number?])]

@defstruct[paths
           ([conf (or/c false/c path-string?)]
            [host-base (or/c false/c path-string?)]
            [log (or/c false/c path-string?)]
            [htdocs (or/c false/c path-string?)]
            [servlet (or/c false/c path-string?)]
            [mime-types (or/c false/c path-string?)]
            [passwords (or/c false/c path-string?)])]

}
