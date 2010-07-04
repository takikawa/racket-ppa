#lang scribble/doc
@(require "web-server.ss")
@(require (for-label web-server/dispatchers/dispatch-servlets))

@title{Troubleshooting and Tips}

@section{How do I use Apache with the PLT Web Server?}

You may want to put Apache in front of your PLT Web Server application. 
Apache can rewrite and proxy requests for a private (or public) PLT Web Server:

@verbatim{
RewriteRule ^(.*)$ http://localhost:8080/$1 [P]
}

The first argument to @exec{RewriteRule} is a match pattern. The second is how to rewrite the URL.
The @exec{[P]} flag instructs Apache to proxy the request. If you do not include this, Apache will
return an HTTP Redirect response and the client should make a second request.

See Apache's documentation for more details on @link["http://httpd.apache.org/docs/2.0/mod/mod_rewrite.html#rewriterule"]{RewriteRule}.

@section{Can the server create a PID file?}

The server has no option for this, but you can add it very easily. There's two techniques.

First, if you use a UNIX platform, in your shell startup script you can use
@verbatim{
echo $$ > PID
exec run-web-server
}

Using @exec{exec} will reuse the same process, and therefore, the PID file will be accurate.

Second, if you want to make your own Scheme start-up script, you can write:
@(require (for-label mzlib/os))
@schemeblock[
(require mzlib/os)
(with-output-to-file _your-pid-file (lambda () (write (getpid))))
(_start-server)
]

@section[#:tag "faq:https"]{How do I set up the server to use HTTPS?}

This requires an SSL certificate and private key. This is very platform specific, but we will provide
the details for using OpenSSL on UNIX:

@commandline{openssl genrsa -des3 -out private-key.pem 1024}

This will generate a new private key, but it will have a passphrase on it. You can remove this via:

@commandline{openssl rsa -in private-key.pem -out private-key.pem}
@commandline{chmod 400 private-key.pem}

Now, we generate a self-signed certificate:

@commandline{openssl req -new -x509 -nodes -sha1 -days 365 -key private-key.pem > server-cert.pem}

(Each certificate authority has different instructions for generating certificate signing requests.)

We can now start the server with:

@commandline{plt-web-server --ssl}

The Web Server will start on port 443 (which can be overridden with the @exec{-p} option) using the
@filepath{private-key.pem} and @filepath{server-cert.pem} we've created.

@section{How do I limit the number of requests serviced at once by the Web Server?}

Refer to @secref["limit.ss"].
