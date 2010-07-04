#lang scribble/doc
@(require "web-server.ss")
@(require (for-label web-server/dispatchers/dispatch-servlets))

@title[#:tag "faq"]{Troubleshooting and Tips}

@section{Why are my servlets not updating on the server when I change the code on disk?}

By default, the server uses @scheme[make-cached-url->servlet] to load servlets
from the disk. As it loads them, they are cached and the disk is not referred to for future
requests. This ensures that there is a single namespace for each servlet, so that different instances
can share resources, such as database connections, and communicate through the store. The default
configuration of the server (meaning the dispatcher sequence used when you load a configuration file)
provides a special URL to localhost that will reset the cache: @filepath{/conf/refresh-servlets}. If
you want the server to reload your changed servlet code, then GET this URL and the server will reload the
servlet on the next request.

@section{What special considerations are there for security with the Web Server?}

The biggest problem is that a naive usage of continuations will allow continuations to subvert
authentication mechanisms. Typically, all that is necessary to execute a continuation is its URL.
Thus, URLs must be as protected as the information in the continuation.

Consider if you link to a public site from a private continuation URL: the @exec{Referrer} field in
the new HTTP request will contain the private URL. Furthermore, if your HTTP traffic is in the clear,
then these URLs can be easily poached.

One solution to this is to use a special cookie as an authenticator. This way, if a URL escapes, it will
not be able to be used, unless the cookie is present. For advice about how to do this well, see
@link["http://cookies.lcs.mit.edu/pubs/webauth.html"]{Dos and Don'ts of Client Authentication on the Web}
from the MIT Cookie Eaters.

Note: It may be considered a great feature that URLs can be shared this way, because delegation is
easily built into an application via URLs.

@section{IE ignores my CSS or behaves strange in other ways}

@(require (for-label xml))

In quirks mode, IE does not parse your page as XML, in particular it will not recognize many instances of
"empty tag shorthand", e.g. "<img src='...' />", whereas the @web-server uses @schememodname[xml]
to format XML, which uses empty tag shorthand by default. You can change the default with the @scheme[empty-tag-shorthand]
parameter: @scheme[(empty-tag-shorthand 'never)].
