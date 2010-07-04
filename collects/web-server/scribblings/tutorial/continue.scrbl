#lang scribble/doc
@(require scribble/manual
          (for-label scheme)
          (for-label web-server/servlet)
          "tutorial-util.ss")

@title{@bold{Continue}: Web Applications in PLT Scheme}

@author[(author+email "Danny Yoo" "dyoo@cs.wpi.edu")
        (author+email "Jay McCarthy" "jay@cs.byu.edu")]

How do we make dynamic web applications?  This tutorial will show how we
can build web applications using PLT Scheme.  As our working example,
we'll build a simple web journal (a ``blog'').  We'll cover how to start
up a web server, how to generate dynamic web content, and how to
interact with the user.

The target audience for this tutorial are students who've gone through
the design and use of structures in
@italic{@link["http://www.htdp.org/"]{How to Design Programs}}, with
some higher-order functions, @scheme[local], and a minor bit of
mutation.

@section{Getting Started}

Everything you needed in this tutorial is provided in @link["http://plt-scheme.org/"]{PLT Scheme}.
We will be using the DrScheme Module language. Enter the following into the Definition window.

@schememod[
web-server/insta
(define (start request)
 '(html
   (head (title "My Blog"))
   (body (h1 "Under construction"))))
]

Press the @onscreen{Run} button.  If a web browser comes up with an ``Under
Construction'' page, then clap your hands with delight: you've built
your first web application!  It doesn't do much yet, but we will get
there.  Press the @onscreen{Stop} button to shut the server down for now.

@section{The Application}

We want to motivate this tutorial by showing how to develop a blog.
Users should be able to create posts and add comments to
any posts.  We'll take an iterative approach, with one or two pitfalls
along the way.  The game plan, roughly, will be:

@itemize[
         @item{Show a static list of posts.}
         @item{Allow a user to add new posts to the system.}
         @item{Extend the model to let a user add comments to a post.}
         @item{Allow all users to share the same set of posts.}
         @item{Serialize our data structures to disk.}
         ]

By the end of this tutorial, we'll have a simple blogging application.

@section{Basic Blog}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-1)]

We start by considering our data definitions.  We want to represent a
list of posts.  Let's say that a post is:

@schemeblock[(define-struct post (title body))]

@(defstruct post ([title string?] [body string?]))

@bold{Exercise.} Make a few examples of posts.

A blog, then, will be a list of posts:

@(defthing blog (listof post?))

As a very simple example of a blog:

@schemeblock[
(define BLOG (list (make-post "First Post!"
                              "Hey, this is my first post!")))
]

Now that we have a sample blog structure, let's get our web
application to show it.

@section{Rendering HTML}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-1)]

When a web browser visits our application's URL, the browser
constructs a request structure and sends it off to our web
application.  Our start function will consume requests and produce
responses.  One basic kind of response is to show an HTML page.

@schemeblock[
 (define html-response/c
   (or/c string?
         (or/c (cons/c symbol? (listof html-response/c))
               (cons/c symbol?
                       (cons/c (listof (list/c symbol? string?))
                               (listof html-response/c))))))]

For example:

The HTML @tt{hello} is represented as @scheme["hello"]. Strings are automatically escaped when output. This guarantees valid HTML. Therefore, the value @scheme["<b>Unfinished tag"] is rendered as @tt{&lt;b&gt;Unfinished tag} not @tt{<b>Unfinished tag}. Similarly, @scheme["<i>Finished tag</i>"] is rendered as @tt{&lt;i&gt;Finished tag&lt;/i&gt;} not @tt{<i>Finished tag</i>}.

@tt{<p>This is an example</p>} is

@scheme['(p "This is an example")].

@tt{<a href="link.html">Past</a>} is

@scheme['(a ((href "link.html")) "Past")].

@tt{<p>This is <div class="emph">another</div> example.</p>} is

@scheme['(p "This is " (div ((class "emph")) "another") " example.")].

We can produce these @scheme[html-response]s by using @scheme[cons] and @scheme[list] directly.
Doing so, however, can be notationally heavy.  Consider:

@schemeblock[
  (list 'html (list 'head (list 'title "Some title"))
         (list 'body (list 'p "This is a simple static page.")))
]

vs:

@schemeblock[
  '(html (head (title "Some title"))
         (body (p "This is a simple static page.")))
]

They both produce the same @scheme[html-response], but the latter is a lot
easier to type and read.  We've been using the extended list
abbreviation form described in @link["http://htdp.org/2003-09-26/Book/curriculum-Z-H-17.html#node_chap_13"]{Section 13} of @link["http://htdp.org/"]{How to Design Programs}:
by using a leading forward quote mark to concisely represent the list
structure, we can construct static html responses with aplomb.

However, we can run into a problem when we use simple list
abbreviation with dynamic content.  If we have expressions to inject
into the @scheme[html-response] structure, we can't use a simple list-abbreviation
approach because those expressions will be treated literally as part
of the list structure!

We want a notation that gives us the convenience of quoted list
abbreviations, but with the option to treat a portion of the structure
as a normal expression.  That is, we would like to define a template
whose placeholders can be easily expressed and filled in dynamically.

Scheme provides this templating functionality with quasiquotation.
Quasiquotation uses a leading back-quote in front of the whole
structure.  Like regular quoted list abbreviation, the majority of the
list structure will be literally preserved in the nested list result.
In places where we'd like a subexpression's value to be plugged in, we
prepend an unquoting comma in front of the subexpression.  As an
example:

@schemeblock[
@code:comment{render-greeting: string -> html-response}
@code:comment{Consumes a name, and produces a dynamic html-response.}
(define (render-greeting a-name)
  `(html (head (title "Welcome"))
         (body (p ,(string-append "Hello " a-name)))))
]

@bold{Exercise.} Write a function that consumes a @scheme[post] and produces
an @scheme[html-response] representing that content.

@defthing[render-post (post? . -> . html-response/c)]

As an example, we want:

@schemeblock[
    (render-post (make-post "First post!" "This is a first post."))
]

to produce:

@schemeblock[
   '(div ((class "post")) "First post!" (p "This is a first post."))
]

@bold{Exercise.} Revise @scheme[render-post] to show the number of comments attached
to a post.

@centerline{------------}

If an expression produces a list of @scheme[html-response] fragments, we may
want to splice in the elements of a list into our template, rather
plug in the whole list itself.  In these situations, we can use the
splicing form @scheme[,@expression].

As an example, we may want a helper function that transforms a
@scheme[html-response] list into a fragment representing an unordered, itemized
HTML list:

@schemeblock[
@code:comment{render-as-itemized-list: (listof html-response) -> html-response}
@code:comment{Consumes a list of items, and produces a rendering}
@code:comment{as an unordered list.}
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

@code:comment{render-as-item: html-response -> html-response}
@code:comment{Consumes an html-response, and produces a rendering}
@code:comment{as a list item.}
(define (render-as-item a-fragment)
  `(li ,a-fragment))
]

@bold{Exercise.} Write a function @scheme[render-posts] that consumes a @scheme[(listof post?)]
and produces an @scheme[html-response] for that content.

@defthing[render-posts ((listof post?) . -> . html-response/c)]

As examples:

@schemeblock[
(render-posts empty)
]

should produce:

@schemeblock[
'(div ((class "posts")))
]

While

@schemeblock[
(render-posts (list (make-post "Post 1" "Body 1")
                    (make-post "Post 2" "Body 2")))
]

should produce:

@schemeblock[
'(div ((class "posts"))
      (div ((class "post")) "Post 1" "Body 1")
      (div ((class "post")) "Post 2" "Body 2"))
]

@centerline{------------}

Now that we have the @scheme[render-posts] function handy, let's revisit our
web application and change our @scheme[start] function to return an interesting
@scheme[html-response].

@external-file["iteration-1.ss"]

If we press Run, we should see the blog posts in our web browser.

@section{Inspecting Requests}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-2
                                  web-server/servlet)]

Our application still seems a bit static: although we're building the
page dynamically, we haven't yet provided a way for an external user
to add new posts.  Let's tackle that now.  Let's provide a form that
will let the user add a new blog entry.  When the user presses the
submit button, we want the user to see the new post at the top of the
page.

Until now, we've been passing around a @scheme[request] object without doing
anything with it.  As we might expect, the @scheme[request] object isn't meant
to be ignored so much!  When a user fills out a web form and submits
it, that user's browser constructs a new @scheme[request] that holds the form
values in it.  We can use the function @scheme[request-bindings] to grab at
the values that the user has filled out.  The type of @scheme[request-bindings]
is:

@defthing[request-bindings (request? . -> . bindings?)]

Along with @scheme[request-bindings], there's another function called
@scheme[extract-binding/single] that takes this as well as a name, and returns
the value associated to that name.

@defthing[extract-binding/single (symbol? bindings? . -> . string?)]

Finally, we can check to see if a name exists in a binding with
@scheme[exists-binding?]:

@defthing[exists-binding? (symbol? bindings? . -> . boolean?)]

With these functions, we can design functions that consume @scheme[request]s
and do something useful.

@bold{Exercise.} Write a function @scheme[can-parse-post?] that consumes a @scheme[bindings?].
It should produce @scheme[#t] if there exist bindings both for the symbols
@scheme['title] and @scheme['body], and @scheme[#f] otherwise.

@defthing[can-parse-post? (bindings? . -> . boolean?)]

@bold{Exercise.} Write a function @scheme[parse-post] that consumes a bindings.
Assume that the bindings structure has values for the symbols @scheme['title]
and @scheme['body]. @scheme[parse-post] should produce a post containing those values.

@defthing[parse-post (bindings? . -> . post?)]

Now that we have these helper functions, we can extend our web
application to handle form input.  We'll add a small form at the
bottom, and adjust out program to handle the addition of new posts.
Our start method, then, will first see if the request has a parsable
post, extend its set of posts if it can, and finally display those
blog posts.

@external-file["iteration-2.ss"]

This appears to work... but there's an issue with this!  Try to add
two new posts.  What happens?

@section{Advanced Control Flow}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-3)]

For the moment, let's ignore the admittedly huge problem of having a
blog that only accepts one new blog entry.  Don't worry!  We will fix
this.

But there's a higher-level problem with our program: although we do
have a function, @scheme[start], that can respond to requests directed at our
application's URL, that @scheme[start] function is starting to get overloaded
with a lot of responsibility.  Conceptually, @scheme[start] is now handling
two different kinds of requests: it's either a request for showing a
blog, or a request for adding a new blog post.

What's happening is that @scheme[start] is becoming a traffic cop --- a
dispatcher --- for all the behavior of our web application.  As far as
we know, if we want to add more functionality to our application,
start needs to know how to deal.  Can we can get different kinds of
requests to automatically direct themselves to different functions?

The web server library provides a function, @scheme[send/suspend/dispatch],
that allows us to create URLs that direct to different parts of our
application.  Let's demonstrate a dizzying example.  In a new file,
enter the following in the definition window.

@schememod[
web-server/insta
@code:comment{start: request -> html-response}
(define (start request)
  (phase-1 request))

@code:comment{phase-1: request -> html-response}
(define (phase-1 request)
  (local [(define (response-generator embed/url)
            `(html 
              (body (h1 "Phase 1")
                    (a ((href ,(embed/url phase-2)))
                       "click me!"))))]
    (send/suspend/dispatch response-generator)))

@code:comment{phase-2: request -> html-response}
(define (phase-2 request)
  (local [(define (response-generator embed/url)
            `(html 
              (body (h1 "Phase 2")
                    (a ((href ,(embed/url phase-1)))
                       "click me!"))))]    
    (send/suspend/dispatch response-generator)))
]

This is a web application that goes round and round.  When a user
first visits the application, the user starts off in @scheme[phase-1].  The
page that's generated has a hyperlink that, when clicked, continues to
@scheme[phase-2].  The user can click back, and falls back to @scheme[phase-1], and the
cycle repeats.

Let's look more closely at the @scheme[send/suspend/dispatch] mechanism.
@scheme[send/suspend/dispatch] consumes a response-generating function, and it
gives that response-generator a function called @scheme[embed/url] that we'll
use to build special URLs.  What makes these URLs special is this:
when a web browser visits these URLs, our web application restarts,
but not from start, but from the handler that we associate to the URL.
In @scheme[phase-1], the use of @scheme[embed/url] associates the link with @scheme[phase-2], and
vice versa.

We can be more sophisticated about the handlers associated with
@scheme[embed/url].  Because the handler is just a request-consuming function,
it can be defined within a @scheme[local].  Consequently, a local-defined
handler knows about all the variables that are in the scope of its
definition.  Here's another loopy example:

@schememod[
web-server/insta
@code:comment{start: request -> html-response}
(define (start request)
  (show-counter 0 request))

@code:comment{show-counter: number request -> html-response}
@code:comment{Displays a number that's hyperlinked: when the link is pressed,}
@code:comment{returns a new page with the incremented number.}
(define (show-counter n request)
  (local [(define (response-generator embed/url)
            `(html (head (title "Counting example"))
                   (body 
                    (a ((href ,(embed/url next-number-handler)))
                       ,(number->string n)))))
                    
          (define (next-number-handler request)
            (show-counter (+ n 1) request))]

    (send/suspend/dispatch response-generator)))
]

This example shows that we can accumulate the results of an
interaction.  Even though the user start off by visiting and seeing
zero, the handlers produced by next-number-handler continue the
interaction, accumulating a larger and larger number.

Now that we've been going a little bit in circles, let's move forward
back to the blog application.  We will adjust the form's action so it
directs to a URL that's associated to a separate handler.

@external-file["iteration-3.ss"]

Note that the structure of the @scheme[render-blog-page] function looks very
similar to that of our last @scheme[show-counter] example.  The user can
finally add and see multiple posts to their blog.

Unfortunately, there's still a problem.  To see the problem: add a few
posts to the system, and then open up a new browser window.  In the
new browser window, visit the web application's URL.  What happens?

@section{Share and Share Alike}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-4)]

We have run into another flaw with our application: each browser
window keeps track of its own distinct blog.  That defeats the point a
blog for most people, that is, to share with others!  When we insert a
new post, rather than create a new blog value, we'd like to make a
structural change to the existing blog.  (HTDP Chapter 41).  So
let's add mutation into the mix.

There's one small detail we need to touch: in the web-server language,
structures are immutable by default.  We'll want to override this
default and get get access to the structure mutators.  To do so, we
adjust our structure definitions with the @scheme[#:mutable] keyword.

Earlier, we had said that a @scheme[blog] was a list of @scheme[post]s, 
but because we want to allow the blog to be changed, let's revisit our
definition so that a blog is a mutable structure:

@schemeblock[(define-struct blog (posts) #:mutable)]

@defstruct[blog ([posts (listof post?)])]

Mutable structures provide functions to change the fields of a
structure; in this case, we now have a structure mutator called
@scheme[set-blog-posts!],

@defthing[set-blog-posts! (blog? (listof post?) . -> . void)]

and this will allow us to change the posts of a blog.

@bold{Exercise.} Write a function @scheme[blog-insert-post!]

@defthing[blog-insert-post! (blog? post? . -> . void)]

The intended side effect of the function will be to extend the blog's
posts.

@centerline{------------}

Since we've changed the data representation of a blog, we'll need to
revise our web application to use the updated representation.  One
other thing to note is that, within the web application, because we're
sharing the same blog value, we don't need to pass it around with our
handlers anymore: we can get at the current blog through our @scheme[BLOG]
variable.

After doing the adjustments incorporating @scheme[insert-blog-post!], and doing
a little variable cleanup, our web application now looks like this:

@external-file["iteration-4.ss"]

Open two windows that visit our web application, and start adding in
posts from both windows.  We should see that both browsers are sharing
the same blog.

@section{Extending the Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-5)]

Next, let's extend the application so that each post can hold a list
of comments.  We refine the data definition of a blog to be:

@defstruct[post ([title string?] [body string?] [comments (listof string?)]) #:mutable]

@bold{Exercise.} Write the updated data structure definition for posts.  Make
sure to make the structure mutable, since we intend to add comments to
posts.

@bold{Exercise.} Make up a few examples of posts.

@bold{Exercise.} Define a function @scheme[post-add-comment!]

@defthing[post-add-comment! (post? string? . -> . void)]

whose intended side effect is to add a new comment to the end of the post's
list of comments.

@bold{Exercise.} Adjust @scheme[render-post] so that the produced fragment will include the
comments in an itemized list.

@bold{Exercise.} Because we've extended a post to include comments, other
post-manipulating parts of the application may need to be adjusted,
such as uses of @scheme[make-post].  Identify and fix any other part of the
application that needs to accommodate the post's new structure.

@centerline{------------}

Once we've changed the data structure of the posts and adjusted our
functions to deal with this revised structure, the web application
should be runnable.  The user may even may even see some of the fruits
of our labor: if the initial @scheme[BLOG] has a post with a comment, the user
should see those comments now.  But obviously, there's something
missing: the user doesn't have the user interface to add comments to a
post!

@section{Breaking Up the Display}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-5)]

How should we incorporate comments more fully into the user's web
experience?  Seeing all the posts and comments all on one page may
be a bit overwhelming.  Perhaps we should hold off on showing the
comments on the main blog page.  Let's present a secondary "detail"
view of a post, and present the comments there.

The top-level view of a blog then can show the blog's title and body.
We can also show a count of how many comments are associated to the
post.

So now we need some way to visit a post's detail page.  One way to do
this is to hyperlink each post's title: if the user wants to see the
detail page of a post, user should be able to click the title to get
there.  From that post's detail page, we can even add a form to let
the user add new comments.

Here's a diagram of a simple page flow of our web application that
should let us add comments.

@image{scribblings/tutorial/images/flow1.png}

Each point in the diagram corresponds to a request-consuming handler.
As we might suspect, we'll be using @scheme[send/suspend/dispatch] some more.
Every arrow in the diagram will be realized as a URL that we generate
with @scheme[embed/url].

This has a slightly messy consequence: previously, we've been
rendering the list of posts without any hyperlinks.  But since any
function that generates a special dispatching URL uses @scheme[embed/url] to do
it, we'll need to adjust @scheme[render-posts] and @scheme[render-post] to consume and
use @scheme[embed/url] itself when it makes those hyperlinked titles.

Our web application now looks like:

@external-file["iteration-5.ss"]

We now have an application that's pretty sophisticated: we can add
posts and write comments.  Still, there's a problem with this: once
the user's in a @scheme[post-detail-page], they can't get back to the blog
without pressing the browser's back button!  That's disruptive.  We
should provide a page flow that lets us get back to the main
blog-viewing page, to keep the user from every getting "stuck" in a
dark corner of the web application.

@section{Adding a Back Button}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-6)]

Here's a diagram of a our revised page flow of our web application.
Maybe we can just add a BACK link from the @scheme[render-post-detail-page]
that gets us back to viewing the top-level blog.

@image{scribblings/tutorial/images/flow2.png}

@bold{Exercise.} Adjust @scheme[render-post-detail-page] to include another link that goes
back to @scheme[render-blog-page].

To make this more interesting, maybe we should enrich the flow a
bit more.  We can give the user a choice right before committing to
their comment.  Who knows?  They may have a change of heart.

@image{scribblings/tutorial/images/flow3.png}

Although this seems complicated, the shape of our handlers will look
more-or-less like what we had before.  After we've added all the
handlers, our web application is fairly functional.

@external-file["iteration-6.ss"]

@section{Decorating With Style!}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-7
                                  web-server/insta/insta)]

We have an application that's functionally complete, but is visual
lacking.  Let's try to improve its appearance.  One way we can do this
is to use a cascading style sheet.  A style sheet can visual panache
to our web pages.  For example, if we'd like to turn all of our
paragraphs green, we might add the following style declaration within
our response.

@scheme['(style ((type "text/css")) "p { color: green }")]

It's tempting to directly embed this style information into our
@scheme[html-response]s.  However, our source file is already quite busy.  We
often want to separate the logical representation of our application
from its presentation.  Rather than directly embed the .css in the
HTML response, let's instead add a link reference to an separate .css
file.

Until now, all the content that our web application has produced has
come from a response generating handler.  Of course, we know that not
everything needs to be dynamically generated: it's common to have
files that won't be changing.  We should be able to serve these static
resources (images, documents, .css files) alongside our web
applications.

To do this, we set aside a path to store these files, and then tell
the web server where that directory is.  The function
@scheme[static-files-path],

@defthing[static-files-path (path-string? -> void)]

tells the web server to look in the given path when it receives a URL
that looks like a static resource request.

@bold{Exercise.} Create a simple web application called @filepath{test-static.ss} with the
following content:

@schememod[
web-server/insta
(define (start request)
  '(html (head (title "Testing"))
         (link ((rel "stylesheet")
                (href "/test-static.css")
                (type "text/css")))
         (body (h1 "Testing")
               (h2 "This is a header")
               (p "This is " (span ((class "hot")) "hot") "."))))

(static-files-path "htdocs")
]

Make a subdirectory called @filepath{htdocs} rooted in the same directory as
the @filepath{test-static.ss} source.  Finally, just to see that we can serve
this .css page, create a very simple .css file @filepath{test-static.css} file
in @filepath{htdocs/} with the following content:

@verbatim{
body {
  margin-left: 10%;
  margin-right: 10%;
}
p { font-family: sans-serif }
h1 { color: green }
h2 { font-size: small }
span.hot { color: red }
}

At this point, run the application and look at the browser's output.
We should see a Spartan web page, but it should still have some color
in its cheeks.

@centerline{------------}

@bold{Exercise.}
Improve the presentation of the blog web application by writing your
an external style sheet to your tastes.  Adjust all of the HTML
response handlers to include a link to the style sheet.

@section{The Double Submit Bug}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-7
                                  web-server/servlet)]

There's yet another a subtle problem in our application.  To see it,
bring our blog application up again, and add a post.  Then reload the
page.  Reload the page again.

What's happening is a well-known problem: it's an instance of the
"double-submit" problem.  If a user presses reload, a request is sent
over to our application.  This wouldn't be such a bad thing, if not
for the fact that we're handling certain requests by mutating our
application's data structures.

A common pattern that web developers use to dodge the double
submission problem is to handle state-mutating request in a peculiar
way: once the user sends over a request that affects change to the
system, we then redirect them off to a different URL that's safe to
reload.  To make this happen, we will use the function @scheme[redirect/get].

@defthing[redirect/get (-> request?)]

This @scheme[redirect/get] function has an immediate side effect: it forces the
user's browser to follow a redirection to a safe URL, and gives us
back that fresh new request.

For example, let's look at a toy application that lets the users add
names to a roster:

@external-file["no-use-redirect.ss"]

This application suffers the same problem as our blog: if the user
adds a name, and then presses reload, then the same name will be added
twice.

We can fix this by changing a single expression.  Can you see what
changed?

@external-file["use-redirect.ss"]

Double-submit, then, is painlessly easy to mitigate.  Whenever we have
handlers that mutate the state of our system, we use @scheme[redirect/get] when
we send our response back.

@bold{Exercise.}
Revise the blog application with @scheme[redirect/get] to address the
double-submit problem.

With these minor fixes, our blog application now looks like this:

@external-file["iteration-7.ss"]

@section{Abstracting the Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-8
                                  web-server/scribblings/tutorial/examples/model)]

If we "turn off the lights" by closing the program, then the state of
our application disappears into the ether.  How do we get our
ephemeral state to stick around?  Before we tackle that question, we
should consider: what do we want to save?  There's some state that we
probably don't have a lingering interest in, like requests.  What we
care about saving is our model of the blog.

If we look closely at our web application program, we see a seam
between the model of our blog, and the web application that uses that
model.  Let's isolate the model: it's all the stuff near the top:

@schemeblock[
    (define-struct blog (posts) #:mutable)
    (define-struct post (title body comments) #:mutable)
    (define BLOG ...)
    (define (blog-insert-post! ...) ...)
    (define (post-insert-comment! ...) ...)
]

In realistic web applications, the model and the web application are
separated by some wall of abstraction.  The theory is that, if we do
this separation, it should be easier to then make isolated changes
without breaking the entire system.  Let's do this: we will first rip
the model out into a separate file.  Once we've done that, then we'll
look into making the model persist.

Create a new file called @filepath{model.ss} with the following content.

@external-file["model.ss"]

This is essentially a cut-and-paste of the lines we identified as our
model.  It's written in the @schememodname[scheme] language because
the model shouldn't need to worry about web-server stuff.  There's one
additional expression that looks a little odd at first:

@schemeblock[
    (provide (all-defined-out))
]

which tells PLT Scheme to allow other files to have access to
everything that's defined in the @filepath{model.ss} file.


We change our web application to use this model.  Going back to our
web application, we rip out the old model code, and replace it with an
expression that let's use use the new model.

@schemeblock[
    (require "model.ss")
]

which hooks up our web application module to the @schememodname["model.ss"] module.

@external-file["iteration-8.ss"]

@section{A Persistent Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-9
                                  web-server/scribblings/tutorial/examples/model-2)]

Now that the model is separated into a separate module, we can more easily modify
its functionality, and in particular, make it persistent.

The first step is to make the model structures serializable. Earlier, we made the
structures mutable by adding @scheme[#:mutable] to their definitions. We can make
the structures serializable by adding @scheme[#:prefab]. This tells PLT Scheme that
these structures can be "previously fabricated", that is, created before the program
started running---which is exactly what we want when restoring the blog data from disk.
Our blog structure definition now looks like:

@schemeblock[
    (define-struct blog (posts) #:mutable #:prefab)
]

Now @scheme[blog] structures can be read from the outside world with @scheme[read] and written
with @scheme[write]. However, we need to make sure everything inside a @scheme[blog] structure is
also marked as @scheme[#:prefab]. If we had a more complicated structure, we would need to ensure
that everything (transitively) in the structure was @scheme[#:prefab]'d.

@bold{Exercise.} Write the new structure definition for posts.

At this point, we @emph{can} read and write the blog to disk. Now let's actually do it.

First, we'll make a place to record in the model where the blog lives on disk. So, we need to change
the blog structure again. Now it will be:

@defstruct[blog ([home string?] [posts (listof post?)]) #:mutable]

@bold{Exercise.} Write the new structure definition for blogs.

Then, we'll make a function that allows our application to initialize the blog:

@schemeblock[
@code:comment{initialize-blog! : path? -> blog}
@code:comment{Reads a blog from a path, if not present, returns default}
(define (initialize-blog! home)
  (local [(define (log-missing-exn-handler exn)
            (make-blog
             (path->string home)
             (list (make-post "First Post"
                              "This is my first post"
                              (list "First comment!"))
                   (make-post "Second Post"
                              "This is another post"
                              (list)))))
          (define the-blog
            (with-handlers ([exn? log-missing-exn-handler])
              (with-input-from-file home read)))]
    (set-blog-home! the-blog (path->string home))
    the-blog))
]

@scheme[initialize-blog!] takes a path and tries to @scheme[read] from it. If the path contains
a @scheme[blog] structure, then @scheme[read] will parse it, because @scheme[blog]s are @scheme[#:prefab].
If there is no file at the path, or if the file has some spurious data, then @scheme[read] or
@scheme[with-input-from-file] will throw an exception. @scheme[with-handlers] provides an
exception handler that will return the default @scheme[blog] structure for all
kinds of errors. 

After @scheme[the-blog] is bound to the newly read (or default) structure, we set the home to the
correct path. (Notice that we need to convert the path into a string. Why didn't we just make the blog
structure contain paths? Answer: They can't be used with @scheme[read] and @scheme[write].)

Next, we will need to write a function to save the model to the disk.

@schemeblock[
@code:comment{save-blog! : blog -> void}
@code:comment{Saves the contents of a blog to its home}
(define (save-blog! a-blog)
  (local [(define (write-to-blog)
            (write a-blog))]
    (with-output-to-file (blog-home a-blog) 
      write-to-blog
      #:exists 'replace)))
]

@scheme[save-blog!] @scheme[write]s the model into its home .
It provides @scheme[with-output-to-file] with an @scheme[#:exists] flag that tells it to replace the
file contents if the file at @scheme[blog-home] exists.

This function can now be used to save the blog structure whenever we modify it. Since we only ever modify the
blog structure in the model, we only need to update @scheme[blog-insert-post!] and @scheme[post-insert-comment!].

@bold{Exercise.} Change @scheme[blog-insert-post!] and @scheme[post-insert-comment!] to call @scheme[save-blog!].

@centerline{------------}

You may have had a problem when trying to update @scheme[post-insert-comment!]. It needs to call @scheme[save-blog!]
with the blog structure. But, it wasn't passed the blog as an argument. We'll need to add that argument and change the
application appropriately. While we're at it, let's change @scheme[blog-insert-post!] to accept the contents of the
post structure, rather the structure itself, to better abstract the model interface:

@defthing[blog-insert-post! (blog? string? string? . -> . void)]
@defthing[post-insert-comment! (blog? post? string? . -> . void)]

@bold{Exercise.} Write the new definitions of @scheme[blog-insert-post!] and @scheme[post-insert-comment!].
(Remember to call @scheme[save-blog!].)



In our last iteration of our model, we used @scheme[(provide
(all-defined-out))] to expose all of the model's definitions.  But we
often want to hide things like private functions and internal data
structures from others.  We'll do that here by using a form of
@scheme[provide] that explicitly names the exposed definitions.

For example, if we wanted to limit the exposed functions to
@scheme[blog-insert-post!] and @scheme[post-insert-comment!], we can
do this:
@schemeblock[
    (provide blog-insert-post!
             post-insert-comment!)
]


Of course, this set of functions is too minimal!  Let's change the
@scheme[provide] line in the model to:
@schemeblock[
(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
]
which captures the essential interactions we do with a blog.


@centerline{------------}

The last step is to change the application. We need to call @scheme[initialize-blog!] to read in the blog structure, and we
need to pass the blog value that is returned around the application, because there is no longer a @scheme[BLOG] export.

First, change @scheme[start] to call @scheme[initialize-blog!] with a path in our home directory:

@schemeblock[
 (define (start request)  
   (render-blog-page 
    (initialize-blog! 
     (build-path (current-directory)
                 "the-blog-data.db"))
    request))
]

@bold{Exercise.} Thread the @scheme[blog] structure through the application appropriately to give
@scheme[blog-insert-post!] and @scheme[post-insert-comment!] the correct values. (You'll also need to
change how @scheme[render-blog-page] adds new posts.)

@centerline{------------}

Our model is now:

@external-file["model-2.ss"]

And our application is:

@external-file["iteration-9.ss"]

@centerline{------------}

This approach to persistence can work surprisingly well for simple applications. But as our application's needs
grow, we will have to deal with concurrency issues, the lack of a simply query language over our data model, etc.
So, in the next section, we'll talk about how to use an SQL database to store our blog model.

@section{Using an SQL database}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/dummy-10
                                  web-server/scribblings/tutorial/examples/dummy-3
                                  web-server/scribblings/tutorial/dummy-sqlite)]
@(require (for-label web-server/scribblings/tutorial/dummy-sqlite))

Our next task is to employ an SQL database for the blog model. We'll be using SQLite with the @schememodname[(planet jaymccarthy/sqlite:4)] PLaneT package. We add the following to the top of our model:

@schemeblock[
(require (prefix-in sqlite: (planet jaymccarthy/sqlite:4)))
]

We now have the following bindings:

@defthing[sqlite:db? (any/c . -> . boolean?)]
@defthing[sqlite:open (path? . -> . sqlite:db?)]
@defthing[sqlite:exec/ignore (sqlite:db? string? . -> . void)]
@defthing[sqlite:select (sqlite:db? string? . -> . (listof (vectorof (or/c integer? number? string? bytes? false/c))))]
@defthing[sqlite:insert (sqlite:db? string? . -> . integer?)]


The first thing we should do is decide on the relational structure of our model. We will use the following tables:

@verbatim{
 CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT, body TEXT)
 CREATE TABLE comments (pid INTEGER, content TEXT)
}

Each post will have an identifier, a title, and a body. This is the same as our old Scheme structure,
except we've added the identifier. (Actually, there was always an identifier---the memory pointer---but now
we have to make it explicit in the database.)

Each comment is tied to a post by the post's identifier and has textual content. We could have chosen to
serialize comments with @scheme[write] and add a new TEXT column to the posts table to store the value.
By adding a new comments table, we are more in accord with the relational style.

A @scheme[blog] structure will simply be a container for the database handle:

@defstruct[blog ([db sqlite:db?])]

@bold{Exercise.} Write the @scheme[blog] structure definition. (It does not need to be mutable or serializable.)

We can now write the code to initialize a @scheme[blog] structure:
@schemeblock[
@code:comment{initialize-blog! : path? -> blog?}
@code:comment{Sets up a blog database (if it doesn't exist)}
(define (initialize-blog! home)
  (define db (sqlite:open home))
  (define the-blog (make-blog db))
  (with-handlers ([exn? void])
    (sqlite:exec/ignore db
                        (string-append
                         "CREATE TABLE posts "
                         "(id INTEGER PRIMARY KEY,"
                         "title TEXT, body TEXT)"))
    (blog-insert-post!
     the-blog "First Post" "This is my first post")
    (blog-insert-post!
     the-blog "Second Post" "This is another post")
    (sqlite:exec/ignore
     db "CREATE TABLE comments (pid INTEGER, content TEXT)")
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)
]

@scheme[sqlite:open] will create a database if one does not already exist at the @scheme[home] path. But, we still need
to initialize the database with the table definitions and initial data. 

We used @scheme[blog-insert-post!] and @scheme[post-insert-comment!] to initialize the database. Let's see their implementation:

@schemeblock[
@code:comment{blog-insert-post!: blog? string? string? -> void}
@code:comment{Consumes a blog and a post, adds the post at the top of the blog.}
(define (blog-insert-post! a-blog title body)
  (sqlite:insert
   (blog-db a-blog)
   (format "INSERT INTO posts (title, body) VALUES ('~a', '~a')"
           title body)))

@code:comment{post-insert-comment!: blog? post string -> void}
@code:comment{Consumes a blog, a post and a comment string.  As a side-effect,}
@code:comment{adds the comment to the bottom of the post's list of comments.}
(define (post-insert-comment! a-blog p a-comment)
  (sqlite:insert 
   (blog-db a-blog)
   (format 
    "INSERT INTO comments (pid, content) VALUES ('~a', '~a')"
    (post-id p) a-comment)))
]

@bold{Exercise.} Find the security hole common to these two functions.

@centerline{------------}

A user could submit a post with a title like, @scheme["null', 'null') and INSERT INTO accounts (username, password) VALUES ('ur','hacked"] and get our simple @scheme[sqlite:insert] to make two INSERTs instead of one. 

 This is called an SQL injection attack. It can be resolved by using
 prepared statements that let SQLite do the proper quoting for us. Refer
 to the SQLite package documentation for usage.

@centerline{------------}

In @scheme[post-insert-comment!], we used @scheme[post-id], but we have not yet defined the new @scheme[post] structure.
It @emph{seems} like a @scheme[post] should be represented by an integer id, because the post table contains an integer as the identifying value.

However, we cannot tell from this structure
what blog this posts belongs to, and therefore, what database; so, we could not extract the title or body values,
since we do not know what to query. Therefore, we should associate the blog with each post:

@defstruct[post ([blog blog?] [id integer?])]

@bold{Exercise.} Write the structure definition for posts.

The only function that creates posts is @scheme[blog-posts]:

@schemeblock[
@code:comment{blog-posts : blog -> (listof post?)}
@code:comment{Queries for the post ids}
(define (blog-posts a-blog)
  (local [(define (row->post a-row)
            (make-post 
             a-blog
             (vector-ref a-row 0)))
          (define rows (sqlite:select
                        (blog-db a-blog)
                        "SELECT id FROM posts"))]
    (cond [(empty? rows)
           empty]
          [else
           (map row->post (rest rows))])))
]

@scheme[sqlite:select] returns a list of vectors. The first element of the list is the name of the columns.
Each vector has one element for each column. Each element is a string representation of the value.

At this point we can write the functions that operate on posts:
@schemeblock[
@code:comment{post-title : post -> string?}
@code:comment{Queries for the title}
(define (post-title a-post)
  (vector-ref 
   (second 
    (sqlite:select 
     (blog-db (post-blog a-post))
     (format 
      "SELECT title FROM posts WHERE id = '~a'"
      (post-id a-post))))
   0))
]

@bold{Exercise.} Write the definition of @scheme[post-body].
             
@bold{Exercise.} Write the definition of @scheme[post-comments].
(Hint: Use @scheme[blog-posts] as a template, not @scheme[post-title].)

@centerline{------------}

The only change that we need to make to the application is to require the new model. The interface is exactly the same!

@centerline{------------}

Our model is now:

@external-file["model-3.ss"]

And our application is:

@schememod[
web-server/insta

(require "model-3.ss")

....
]

@section{Leaving DrScheme}

So far, to run our application, we've been pressing @onscreen{Run} in DrScheme. If we were to actually deploy
an application, we'd need to do this differently.

@(require (for-label web-server/servlet-env)
          (for-label web-server/managers/lru))

The simplest way to do this is to use @schememodname[web-server/servlet-env].

First, change the first lines in your application from
@schememod[
web-server/insta
]

to
@schememod[
scheme

(require web-server/servlet)
(provide/contract (start (request? . -> . response/c)))
]

Second, add the following at the bottom of your application:

@schemeblock[
(require web-server/servlet-env)
(serve/servlet start 
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000 
               #:extra-files-paths
               (list (build-path _your-path-here "htdocs"))
               #:servlet-path
               "/servlets/APPLICATION.ss")
]

You can change the value of the @scheme[#:port] parameter to use a different port.

@scheme[#:listen-ip] is set to @scheme[#f] so that the server will listen on @emph{all} available IPs.

You should change @scheme[_your-path-here] to be the path to the parent of your @scheme[htdocs] directory.

You should change @scheme["APPLICATION.ss"] to be the name of your application.

Third, to run your server, you can either press @onscreen{Run} in DrScheme, or type

@commandline{mzscheme -t <file.ss>}

(With your own file name, of course.) Both of these will start a Web server  for your application.

@centerline{------------}

@scheme[serve/servlet] takes other options and there are more advanced ways of starting the Web Server,
but you'll have to refer to the PLT Web Server Reference Manual for details.

@section{Using HTTPS}

This final task that we'll cover is using the server in HTTPS mode.
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

@section{Moving Forward}

As you move forward on your own applications, you may find many useful packages on PLaneT. There are interfaces to other
databases. Many tools for generating HTML, XML, and Javascript output. Etc. There is also an active community of
users on the @scheme[plt-scheme] mailing list. We welcome new users!
