#lang scribble/doc
@(require scribble/manual
          scribble/urls
          scribble/eval
          "../quick/keep.ss"
          (for-label scheme
                     scheme/enter
                     readline
                     net/url
                     xml
                     scheme/control))

@(define quick @other-manual['(lib "quick.scrbl" "scribblings/quick")])
@(define guide @other-manual['(lib "guide.scrbl" "scribblings/guide")])

@(define more-eval (make-base-eval))
@(interaction-eval #:eval more-eval
                   (define (show-load re?)
                     (fprintf (current-error-port) " [~aloading serve.ss]\n" (if re? "re-" ""))))
@(interaction-eval #:eval more-eval
                   (define (serve n) void))
@(interaction-eval #:eval more-eval
                   (define (show-break)
                     (fprintf (current-error-port) "^Cuser break")))
@(interaction-eval #:eval more-eval
                   (define (show-fail n)
                     (error 'tcp-listen
                            "listen on ~a failed (address already in use)"
                            n)))
@(interaction-eval #:eval more-eval (require xml net/url))

@(define (whole-prog which [last? #f])
  (let ([file (format "step~a.txt" which)])
    (margin-note (keep-file file)
                 "Here's the "
                 (if last? 
                     "final program"
                     "whole program so far")
                 " in plain text: "
                 (link file "step " which) ".")))

@title{@bold{More}: Systems Programming with PLT Scheme}

@author["Matthew Flatt"]

In contrast to the impression that @|quick| may give, PLT Scheme is
not just another pretty face. Underneath the graphical facade of
DrScheme lies a sophisticated toolbox for managing threads and
processes, which is the subject of this tutorial.

Specifically, we show how to build a secure, multi-threaded,
servlet-extensible, continuation-based web server. We use much more of
the language than in @|quick|, and we expect you to click on syntax or
function names that you don't recognize (which will take you to the
relevant documentation). Beware that the last couple of sections
present material that is normally considered difficult. If you're
still new to Scheme and have relatively little programming experience,
you may want to skip to @|guide|.

To get into the spirit of this tutorial, we suggest that you set
DrScheme aside for a moment, and switch to raw @exec{mzscheme} in a
terminal. You'll also need a text editor, such as @exec{emacs} or
@exec{vi}. Finally, you'll need a web client, perhaps @exec{lynx} or
@exec{firefox}.

@margin-note{Of course, if you're already spoiled, you can keep using
DrScheme.}

@; ----------------------------------------------------------------------
@section{Ready...}

@link[url:download-drscheme]{Download PLT Scheme}, install, and then
start @exec{mzscheme} with no command-line arguments:

@verbatim[#:indent 2]{
  $ mzscheme
  Welcome to MzScheme
  > 
}

If you're using a plain terminal, if you have GNU Readline installed
on your system, and if you'd like Readline support in @exec{mzscheme},
then evaluate @scheme[(require readline)]. If you also evaluate
@scheme[(install-readline!)], then your @filepath{~/.mzschemerc} is
updated to load Readline whenever you start @exec{mzscheme} for
interactive evaluation.

@margin-note{Unfortunately, for legal reasons related to GPL vs. LGPL,
             @exec{mzscheme} cannot provide Readline automatically.}

@interaction[
(eval:alts (require readline) (void))
(eval:alts (install-readline!) (void))
]

@; ----------------------------------------------------------------------
@section{Set...}

In the same directory where you started @exec{mzscheme}, create a text
file @filepath{serve.ss}, and start it like this:

@schememod[
scheme

(define (go)
  'yep-it-works)
]

@whole-prog["0"]

@; ----------------------------------------------------------------------
@section{Go!}

Back in @exec{mzscheme}, try loading the file and running @scheme[go]:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.ss") (show-load #f))
(eval:alts (go) 'yep-it-works)
]

Try modifying @filepath{serve.ss}, and then run @scheme[(enter!
"serve.ss")] again to re-load the module, and then check your changes.

@; ----------------------------------------------------------------------
@section{``Hello World'' Server}

We'll implement the web server through a @scheme[serve] function that
takes a IP port number for client connections:

@schemeblock[
(define (serve port-no)
  ...)
]

The server accepts TCP connections through a @defterm{listener}, which
we create with @scheme[tcp-listen]. To make interactive development
easier, we supply @scheme[#t] as the third argument to
@scheme[tcp-listen], which lets us re-use the port number without
waiting on TCP timeouts.

@schemeblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  ...)
]

The server must loop to accept connections from the listener:

@schemeblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (loop))
]

Our @scheme[accept-and-handle] function accepts a connection using
@scheme[tcp-accept], which returns two values: a stream for input from
the client, and a stream for output to the client.

@schemeblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (handle in out)
  (close-input-port in)
  (close-output-port out))
]

To handle a connection, for now, we'll read and discard the request
header, and then write a ``Hello, world!'' web page as the result:

@schemeblock[
(define (handle in out)
  (code:comment #, @t{Discard the request header (up to blank line):})
  (regexp-match #rx"(\r\n|^)\r\n" in)
  (code:comment #, @t{Send reply:})
  (display "HTTP/1.0 200 Okay\r\n" out)
  (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
  (display "<html><body>Hello, world!</body></html>" out))
]

Note that @scheme[regexp-match] operates directly on the input stream,
which is easier than bothering with individual lines.

@whole-prog["1"]

Copy the above three definitions---@scheme[serve],
@scheme[accept-and-handle], and @scheme[handle]---into
@filepath{serve.ss} and re-load:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.ss") (show-load #t))
(eval:alts (serve 8080) (void))
]

Now point your browser to @tt{http://localhost:8080} (assuming that
you used @scheme[8080] as the port number, and that the browser is
running on the same machine) to receive a friendly greeting from your
web server.

@; ----------------------------------------------------------------------
@section{Server Thread}

Before we can make the web server respond in more interesting ways, we
need to get a Scheme prompt back. Typing Ctl-C in your terminal window
interrupts the server loop:

@margin-note{In DrScheme, instead of typing Ctl-C, click the
@onscreen{Stop} button once.}

@interaction[
#:eval more-eval
(eval:alts (serve 8080) (show-break))
(eval:alts code:blank (void))
]

Unfortunately, we cannot now re-start the server with the same port
number:

@interaction[
#:eval more-eval
(eval:alts (serve 8080) (show-fail 8080))
]

The problem is that the listener that we created with @scheme[serve]
is still listening on the original port number.

To avoid this problem, let's put the listener loop in its own thread,
and have @scheme[serve] return immediately. Furthermore, we'll have
@scheme[serve] return a function that can be used to shut down the
server thread and TCP listener:

@schemeblock[
(define (serve port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (accept-and-handle listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))
]

@whole-prog["2"]

Try the new one:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.ss") (show-load #t))
(define stop (serve 8081))
]

Your server should now respond to @tt{http://localhost:8081}, but you
can shut down and restart the server on the same port number as often
as you like:

@interaction[
#:eval more-eval
(stop)
(define stop (serve 8081))
(stop)
(define stop (serve 8081))
(stop)
]

@; ----------------------------------------------------------------------
@section{Connection Threads}

In the same way that we put the main server loop into a background
thread, we can put each individual connection into its own thread:

@schemeblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (thread
   (lambda ()
     (handle in out)
     (close-input-port in)
     (close-output-port out))))
]

@whole-prog["3"]

With this change, our server can now handle multiple threads at
once. The handler is so fast that this fact will be difficult to
detect, however, so try inserting @scheme[(sleep (random 10))] before
the @scheme[handle] call above. If you make multiple connections from
the web browser at roughly the same time, some will return soon, and
some will take up to 10 seconds. The random delays will be independent
of the order in which you started the connections.

@; ----------------------------------------------------------------------
@section{Terminating Connections}

A malicious client could connect to our web server and not send the
HTTP header, in which case a connection thread will idle forever,
waiting for the end of the header. To avoid this possibility, we'd
like to implement a timeout for each connection thread.

One way to implement the timeout is to create a second thread that
waits for 10 seconds, and then kills the thread that calls
@scheme[handle]. Threads are lightweight enough in Scheme that this
watcher-thread strategy works well:

@schemeblock[
(define (accept-and-handle listener)
  (define-values (in out) (tcp-accept listener))
  (define t (thread
              (lambda () 
                (handle in out)
                (close-input-port in)
                (close-output-port out))))
  (code:comment #, @t{Watcher thread:})
  (thread (lambda ()
            (sleep 10)
            (kill-thread t))))
]

This first attempt isn't quite right, because when the thread is
killed, its @scheme[in] and @scheme[out] streams remain open.  We
could add code to the watcher thread to close the streams as well as
kill the thread, but Scheme offers a more general shutdown mechanism:
@defterm{custodians}. A custodian is a kind of container for all
resources other than memory, and it supports a
@scheme[custodian-shutdown-all] operation that terminates and closes
all resources within the container, whether they're threads, streams,
or other kinds of limited resources.

Whenever a thread or stream is created, it is placed into the current
custodian as determined by the @scheme[current-custodian]
parameter. To place everything about a connection into a custodian, we
@scheme[parameterize] all the resource creations to go into a new
custodian:

@schemeblock[
(define (accept-and-handle listener)
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  (code:comment #, @t{Watcher thread:})
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))
]

With this implementation, @scheme[in], @scheme[out], and the thread
that calls @scheme[handle] all belong to @scheme[cust]. In addition,
if we later change @scheme[handle] so that it, say, opens a file, then
the file handles will also belong to @scheme[cust], so they will be
reliably closed when @scheme[cust] is shut down.

In fact, it's a good idea to change @scheme[serve] to that it uses a
custodian, too:

@schemeblock[
(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))
]

That way, the @scheme[main-cust] created in @scheme[serve] not only
owns the TCP listener and the main server thread, it also owns every
custodian created for a connection. Consequently, the revised shutdown
procedure for the server immediately terminates all active connections,
in addition to the main server loop.

@whole-prog["4"]

After updating the @scheme[serve] and @scheme[accept-and-handle]
functions as above, here's how you can simulate a malicious client:

@interaction[
#:eval more-eval
(eval:alts (enter! "serve.ss") (show-load #t))
(define stop (serve 8081))
(eval:alts (define-values (cin cout) (tcp-connect "localhost" 8081)) (void))
]

Now wait 10 seconds. If you try reading from @scheme[cin], which is
the stream that sends data from the server back to the client, you'll
find that the server has shut down the connection:

@interaction[
#:eval more-eval
(eval:alts (read-line cin) eof)
]

Alternatively, you don't have to wait 10 seconds if you explicitly
shut down the server:

@interaction[
#:eval more-eval
(eval:alts (define-values (cin2 cout2) (tcp-connect "localhost" 8081)) (void))
(stop)
(eval:alts (read-line cin2) eof)
]

@; ----------------------------------------------------------------------
@section{Dispatching}

It's finally time to generalize our server's ``Hello, World!''
response to something more useful. Let's adjust the server so that we
can plug in dispatching functions to handle requests to different
URLs.

To parse the incoming URL and to more easily format HTML output, we'll
require two extra libraries:

@schemeblock[
(require xml net/url)
]

The @schememodname[xml] library gives us @scheme[xexpr->string], which
takes a Scheme value that looks like HTML and turns it into actual
HTML:

@interaction[
#:eval more-eval
(xexpr->string '(html (head (title "Hello")) (body "Hi!")))
]

We'll assume that our new @scheme[dispatch] function (to be written)
takes a requested URL and produces a result value suitable to use with
@scheme[xexpr->string] to send back to the client:

@schemeblock[
(define (handle in out)
  (define req
    (code:comment #, @t{Match the first line to extract the request:})
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    (code:comment #, @t{Discard the rest of the header (up to blank line):})
    (regexp-match #rx"(\r\n|^)\r\n" in)
    (code:comment #, @t{Dispatch:})
    (let ([xexpr (dispatch (list-ref req 1))])
      (code:comment #, @t{Send reply:})
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))
]

The @schememodname[net/url] library gives us @scheme[string->url],
@scheme[url-path], @scheme[path/param-path], and @scheme[url-query]
for getting from a string to parts of the URL that it represents:

@interaction[
#:eval more-eval
(define u (string->url "http://localhost:8080/foo/bar?x=bye"))
(url-path u)
(map path/param-path (url-path u))
(url-query u)
]

We use these pieces to implement @scheme[dispatch]. The
@scheme[dispatch] function consults a hash table that maps an initial
path element, like @scheme["foo"], to a handler function:

@schemeblock[
(define (dispatch str-path)
  (code:comment #, @t{Parse the request as a URL:})
  (define url (string->url str-path))
  (code:comment #, @t{Extract the path part:})
  (define path (map path/param-path (url-path url)))
  (code:comment #, @t{Find a handler based on the path's first element:})
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      (code:comment #, @t{Call a handler:})
      (h (url-query url))
      (code:comment #, @t{No handler found:})
      `(html (head (title "Error"))
            (body
             (font ((color "red"))
                   "Unknown page: " 
                   ,str-path)))))

(define dispatch-table (make-hash))
]

With the new @scheme[require] import and new @scheme[handle],
@scheme[dispatch], and @scheme[dispatch-table] definitions, our
``Hello World!'' server has turned into an error server. You don't have
to stop the server to try it out. After modifying @filepath{serve.ss}
with the new pieces, evaluate @scheme[(enter! "serve.ss")] and then
try again to connect to the server. The web browser should show an
``Unknown page'' error in red.

We can register a handler for the @scheme["hello"] path like this:

@schemeblock[
(hash-set! dispatch-table "hello"
           (lambda (query) 
             `(html (body "Hello, World!"))))
]

@whole-prog["5"]

After adding these lines and evaluating @scheme[(enter! "serve.ss")],
opening @tt{http://localhost:8081/hello} should produce the old
greeting.

@; ----------------------------------------------------------------------
@section{Servlets and Sessions}

Using the @scheme[query] argument that is passed to a handler by
@scheme[dispatch], a handler can respond to values that a user
supplies through a form.

The following helper function constructs an HTML form. The
@scheme[label] argument is a string to show the user. The
@scheme[next-url] argument is a destination for the form results. The
@scheme[hidden] argument is a value to propagate through the form as a
hidden field. When the user responds, the @scheme["number"] field in
the form holds the user's value:

@schemeblock[
(define (build-request-page label next-url hidden)
  `(html 
    (head (title "Enter a Number to Add"))
    (body ([bgcolor "white"])
          (form ([action ,next-url] [method "get"])
                ,label
                (input ([type "text"] [name "number"]
                                      [value ""]))
                (input ([type "hidden"] [name "hidden"]
                                        [value ,hidden]))
                (input ([type "submit"] [name "enter"] 
                                        [value "Enter"]))))))
]

Using this helper function, we can create a servlet that generates as
many ``hello''s as a user wants:

@schemeblock[
(define (many query)
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)
]

@whole-prog["6"]

As usual, once you have added these to your program, update with
@scheme[(enter! "serve.ss")], and then visit
@tt{http://localhost:8081/many}. Provide a number, and you'll receive
a new page with that many ``hello''s.

@; ----------------------------------------------------------------------
@section{Limiting Memory Use}

With our latest @scheme["many"] servlet, we seem to have a new
problem: a malicious client could request so many ``hello''s that the
server runs out of memory. Actually, a malicious client could also
supply an HTTP request whose first line is arbitrarily long.

The solution to this class of problems is to limit the memory use of a
connection. Inside @scheme[accept-and-handle], after the definition of
@scheme[cust], add the line

@schemeblock[(custodian-limit-memory cust (* 50 1024 1024))]

@whole-prog["7"]

We're assuming that 50MB should be plenty for any
servlet. Garbage-collector overhead means that the actual memory use
of the system can be some small multiple of 50 MB. An important
guarantee, however, is that different connections will not be charged
for each other's memory use, so one misbehaving connection will not
interfere with a different one.

So, with the new line above, and assuming that you have a couple of
hundred megabytes available for the @exec{mzscheme} process to use,
you shouldn't be able to crash the web server by requesting a
ridiculously large number of ``hello''s.

Given the @scheme["many"] example, it's a small step to a web server
that accepts arbitrary Scheme code to execute on the server. In that
case, there are many additional security issues besides limiting
processor time and memory consumption. The
@schememodname[scheme/sandbox] library provides support to managing
all those other issues.

@; ----------------------------------------------------------------------
@section{Continuations}

As a systems example, the problem of implementing a web server exposes
many system and security issues where a programming language can
help. The web-server example also leads to a classic, advanced Scheme
topic: @defterm{continuations}. In fact, this facet of a web server
needs @defterm{delimited continuations}, which PLT Scheme provides.

The problem solved by continuations is related to servlet sessions and
user input, where a computation spans multiple client connections
@cite["Queinnec00"]. Often, client-side computation (as in AJAX) is the
right solution to the problem, but many problems are best solved with
a mixture of techniques (e.g., to take advantage of the browser's
``back'' button).

As the multi-connection computation becomes more complex, propagating
arguments through @scheme[query] becomes increasingly tedious. For
example, we can implement a servlet that takes two numbers to add by
using the hidden field in the form to remember the first number:

@schemeblock[
(define (sum query)
  (build-request-page "First number:" "/one" ""))

(define (one query)
  (build-request-page "Second number:"
                      "/two"
                      (cdr (assq 'number query))))

(define (two query)
  (let ([n (string->number (cdr (assq 'hidden query)))]
        [m (string->number (cdr (assq 'number query)))])
    `(html (body "The sum is " ,(number->string (+ m n))))))

(hash-set! dispatch-table "sum" sum)
(hash-set! dispatch-table "one" one)
(hash-set! dispatch-table "two" two)
]

@whole-prog["8"]

While the above works, we would much rather write such computations in
a direct style:

@schemeblock[
(define (sum2 query)
  (define m (get-number "First number:"))
  (define n (get-number "Second number:"))
  `(html (body "The sum is " ,(number->string (+ m n)))))

(hash-set! dispatch-table "sum2" sum2)
]

The problem is that @scheme[get-number] needs to send an HTML response
back for the current connection, and then it must obtain a response
through a new connection. That is, somehow it needs to convert the
page generated by @scheme[build-request-page] into a @scheme[query]
result:

@schemeblock[
(define (get-number label)
  (define query
    ... (build-request-page label ...) ...)
  (number->string (cdr (assq 'number query))))
]

Continuations let us implement a @scheme[send/suspend] operation that
performs exactly that operation. The @scheme[send/suspend] procedure
generates a URL that represents the current connection's computation,
capturing it as a continuation. It passes the generated URL to a
procedure that creates the query page; this query page is used as the
result of the current connection, and the surrounding computation
(i.e., the continuation) is aborted. Finally, @scheme[send/suspend]
arranges for a request to the generated URL (in a new connection) to
restore the aborted computation.

Thus, @scheme[get-number] is implemented as follows:

@schemeblock[
(define (get-number label)
  (define query
    (code:comment #, @t{Generate a URL for the current computation:})
    (send/suspend
      (code:comment #, @t{Receive the computation-as-URL here:})
      (lambda (k-url)
        (code:comment #, @t{Generate the query-page result for this connection.})
        (code:comment #, @t{Send the query result to the saved-computation URL:})
        (build-request-page label k-url ""))))
  (code:comment #, @t{We arrive here later, in a new connection})
  (string->number (cdr (assq 'number query))))
]

We still have to implement @scheme[send/suspend]. Plain Scheme's
@scheme[call/cc] is not quite enough, so we import a library of
control operators:

@schemeblock[(require scheme/control)]

Specifically, we need @scheme[prompt] and @scheme[abort] from
@schememodname[scheme/control]. We use @scheme[prompt] to mark the
place where a servlet is started, so that we can abort a computation
to that point. Change @scheme[handle] by wrapping an @scheme[prompt]
around the call to @scheme[dispatch]:

@schemeblock[
(define (handle in out)
  ....
    (let ([xexpr (prompt (dispatch (list-ref req 1)))])
      ....))
]

Now, we can implement @scheme[send/suspend]. We use @scheme[call/cc]
in the guise of @scheme[let/cc], which captures the current
computation up to an enclosing @scheme[prompt] and binds that
computation to an identifier---@scheme[k], in this case:

@schemeblock[
(define (send/suspend mk-page)
  (let/cc k
    ...))
]

Next, we generate a new dispatch tag, and we record the mapping from
the tag to @scheme[k]:

@schemeblock[
(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    ...))
]

Finally, we abort the current computation, supplying instead the page
that is built by applying the given @scheme[mk-page] to a URL for the
generated tag:
  
@schemeblock[
(define (send/suspend mk-page)
  (let/cc k
    (define tag (format "k~a" (current-inexact-milliseconds)))
    (hash-set! dispatch-table tag k)
    (abort (mk-page (string-append "/" tag)))))
]

When the user submits the form, the handler associated with the form's
URL is the old computation, stored as a continuation in the dispatch
table. Calling the continuation (like a function) restores the old
computation, passing the @scheme[query] argument back to that
computation.

@whole-prog["9" #t]

In summary, the new pieces are: @scheme[(require scheme/control)],
adding @scheme[prompt] inside @scheme[handle], the definitions of
@scheme[send/suspend], @scheme[get-number], and @scheme[sum2], and
@scheme[(hash-set! dispatch-table "sum2" sum2)]. Once you have
the server updated, visit @tt{http://localhost:8081/sum2}.

@; ----------------------------------------------------------------------
@section{Where to Go From Here}

The PLT Scheme distribution includes a production-quality web server
that addresses all of the design points mentioned here and more.
To learn more, see the tutorial @other-manual['(lib
"web-server/scribblings/tutorial/continue.scrbl")], the
documentation @other-manual['(lib
"web-server/scribblings/web-server.scrbl")], or the research paper
@cite["Krishnamurthi07"].

Otherwise, if you arrived here as part of an introduction to PLT
Scheme, then your next stop is probably @|guide|.

If the topics covered here are the kind that interest you, see also
@secref["concurrency" #:doc '(lib
"scribblings/reference/reference.scrbl")] and @secref["security" #:doc
'(lib "scribblings/reference/reference.scrbl")] in @other-manual['(lib
"scribblings/reference/reference.scrbl")].

Some of this material is based on relatively recent research, and more
information can be found in papers written by the authors of PLT
Scheme, including papers on MrEd @cite["Flatt99"], memory accounting
@cite["Wick04"], kill-safe abstractions @cite["Flatt04"], and
delimited continuations @cite["Flatt07"].

@; ----------------------------------------------------------------------

@(bibliography

  (bib-entry #:key "Flatt99"
             #:author "Matthew Flatt, Robert Bruce Findler, Shriram Krishnamurthi, and Matthias Felleisen"
             #:title @elem{Programming Languages as Operating Systems
                          (@emph{or} Revenge of the Son of the Lisp Machine)}
             #:location "International Conference on Functional Programming"
             #:date "1999"
             #:url "http://www.ccs.neu.edu/scheme/pubs/icfp99-ffkf.pdf")

  (bib-entry #:key "Flatt04"
             #:author "Matthew Flatt and Robert Bruce Findler"
             #:title "Kill-Safe Synchronization Abstractions"
             #:location "Programming Language Design and Implementation" 
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/pldi04-ff.pdf")

  (bib-entry #:key "Flatt07"
             #:author "Matthew Flatt, Gang Yu, Robert Bruce Findler, and Matthias Felleisen"
             #:title "Adding Delimited and Composable Control to a Production Programming Environment"
             #:location "International Conference on Functional Programming"
             #:date "2007"
             #:url "http://www.cs.utah.edu/plt/publications/icfp07-fyff.pdf")

  (bib-entry #:key "Krishnamurthi07"
             #:author "Shriram Krishnamurthi, Peter Hopkins, Jay McCarthy, Paul T. Graunke, Greg Pettyjohn, and Matthias Felleisen"
             #:title "Implementation and Use of the PLT Scheme Web Server"
             #:location @italic{Higher-Order and Symbolic Computation}
             #:date "2007"
             #:url "http://www.cs.brown.edu/~sk/Publications/Papers/Published/khmgpf-impl-use-plt-web-server-journal/paper.pdf")

  (bib-entry #:key "Queinnec00"
             #:author "Christian Queinnec"
             #:title "The Influence of Browsers on Evaluators or, Continuations to Program Web Servers"
             #:location "International Conference on Functional Programming"
             #:date "2000"
             #:url "http://www-spi.lip6.fr/~queinnec/Papers/webcont.ps.gz")

  (bib-entry #:key "Wick04"
             #:author "Adam Wick and Matthew Flatt"
             #:title "Memory Accounting without Partitions"
             #:location "International Symposium on Memory Management"
             #:date "2004"
             #:url "http://www.cs.utah.edu/plt/publications/ismm04-wf.pdf")

)
