#lang scribble/doc
@(require "common.ss"
          (for-label compiler/cm))

@title[#:tag "extending-drscheme"]{Extending DrScheme}

DrScheme supports two forms of extension to the programming
 environment:

@itemize[

@item{@index['("languages" "extending")]{@index['("DrScheme
  Teachpacks")]{A @deftech{teachpack}}} extends the set of procedures
  that are built into a language in DrScheme.  For example, a
  teachpack might extend the Beginning Student language with a
  procedure for playing sounds.

  Teachpacks are particularly useful in a classroom setting, where an
  instructor can provide a teachpack that is designed for a specific
  exercise. To use the teachpack, each student must download the
  teachpack file and select it through the @menuitem["Language" "Add
  Teachpack..."]  menu item.

  See @secref["teachpacks"] for information in creating teachpacks.}

@item{A @deftech{tool} extends the set of utilities within the
  DrScheme environment. For example, DrScheme's @onscreen{Check
  Syntax} button starts a syntax-checking tool. For information on
  creating @tech{tools}, see @other-manual['(lib
  "scribblings/tools/tools.scrbl")].}

]

@; ----------------------------------------

@section[#:tag "teachpacks"]{Teachpacks}

Teachpacks are designed to supplement student programs with code that 
cannot be expressed in a teaching language. For
example, to enable students to play hangman, we supply a teachpack that

@itemize[

 @item{implements the random choosing of a word,}

 @item{maintains the state variable of how many guesses have gone wrong, and}

 @item{manages the GUI.}

]

All these tasks are beyond students in the third week and/or impose
memorization of currently useless knowledge on students. The essence
of the hangman game, however, is not. The use of teachpacks enables
the students to implement the interesting part of this exercise and
still be able to enjoy today's graphics without the useless
memorization.

A single Scheme source file defines a teachpack (although the file may
access other files via @scheme[require]). The file must contain a
module (see @secref[#:doc '(lib "scribblings/guide/guide.scrbl")
"modules"]). Each exported syntax definition or value definition from
the module is provided as a new primitive form or primitive operation
to the user, respectively.

As an example, the following teachpack provides a lazy cons
implementation. To test it, be sure to save it in a file named
@filepath{lazycons.ss}.

@schememod[
scheme

(provide (rename-out [:lcons lcons]) lcar lcdr)

(define-struct lcons (hd tl))

(define-syntax (:lcons stx)
  (syntax-case stx ()
    [(_ hd-exp tl-exp)
     (syntax (make-lcons 
               (delay hd-exp)
               (delay tl-exp)))]))

(define (lcar lcons) (force (lcons-hd lcons)))
(define (lcdr lcons) (force (lcons-tl lcons)))
]

Then, in this program:

@schemeblock[
(define (lmap f l)
  (lcons
   (f (lcar l))
   (lmap f (lcdr l))))

(define all-nums (lcons 1 (lmap add1 all-nums)))
]

the list @scheme[all-nums] is bound to an infinite list
 of ascending numbers.

For more examples, see the @filepath{htdp} sub-collection in the
@filepath{teachpack} collection of the PLT installation.

@; ----------------------------------------------------------------------

@section[#:tag "environment-variables"]{Environment Variables}

Several environment variables can affect DrScheme's behavior:

@itemize[

 @item{@indexed-envvar{PLTNOTOOLS} : When this environment variable is
       set, DrScheme doesn't load any tools.}

 @item{@indexed-envvar{PLTONLYTOOL} : When this environment variable
       is set, DrScheme only loads the tools in the collection named
       by the value of the environment variable. If the variable is
       bound to a parenthesized list of collections, only the tools in
       those collections are loaded (The contents of the environment
       variable are @scheme[read] and expected to be a single symbol
       or a list of symbols).}

 @item{@indexed-envvar{PLTDRCM} : When this environment variable is
       set, DrScheme installs the compilation manager before starting
       up, which means that the @filepath{.zo} files are automatically
       kept up to date, as DrScheme's (or a tools) source is modified.

       If the variable is set to @litchar{trace} then the compilation
       manager's output is traced, using the
       @scheme[manager-trace-handler] procedure.}

 @item{@indexed-envvar{PLTDRDEBUG} : When this environment variable is
       set, DrScheme starts up with errortrace enabled. If the
       variable is set to @litchar{profile}, DrScheme also records
       profiling information about itself.}

 @item{@indexed-envvar{PLTDRPROFILE} : When this environment variable is
       set, DrScheme uses the @schememodname[profile] library (with
       a little GUI) to collect profiling information about itself.}

 @item{@indexed-envvar{PLTDRBREAK} : When this environment variable is
       set, DrScheme creates a window with a break button, during
       startup. Clicking the button breaks DrScheme's eventspace's
       main thread. This works well in combination with
       @envvar{PLTDRDEBUG} since the source locations are reported for
       the breaks.}

 @item{@indexed-envvar{PLTDRTESTS} : When this environment variable is
       set, DrScheme installs a special button in the button bar that
       starts the test suite. (The test suite is available only in the
       source distribution.)}

 @item{@indexed-envvar{PLTSTRINGCONSTANTS} : When this environment
       variable is set, DrScheme prints out the string constants that
       have not yet been translated. If it is set to a particular
       language (corresponding to one of the files in
       @filepath{string-constants} collection) it only shows the unset
       string constants matching that language.

       This environment variable must be set when @filepath{.zo} files
       are made. To ensure that you see its output properly, run
       @exec{setup-plt} with the @Flag{c} flag, set the environment
       variable, and then run @exec{setup-plt} again.}

]
