#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scheme/list
          (for-label eopl/eopl
                     scheme/contract
                     (only-in scheme printf pretty-print)))

@(define-syntax-rule (def-mz id)
   (begin
     (require (for-label mzscheme))
     (define id (scheme provide))))
@(def-mz mzscheme-provide)

@(define-syntax-rule (reprovide id ...)
   (*threecolumns (list (scheme id) ... 'nbsp 'nbsp)))
@(define (*threecolumns l)
   (let* ([len (length l)]
          [third (quotient len 3)]
          [a (take l third)]
          [b (take (list-tail l third) third)]
          [c (list-tail l (* 2 third))]
          [spacer (hspace 2)]
          [to-flow (compose make-flow list make-paragraph list)])
     (make-table #f
                 (map (lambda (a b c)
                        (list (to-flow spacer)
                              (to-flow a)
                              (to-flow spacer)
                              (to-flow b)
                              (to-flow spacer)
                              (to-flow c)))
                      a b c))))


@title{@italic{Essentials of Programming Languages} Language}

The @italic{Essentials of Programming Languages} language in DrScheme
provides a subset of functions and syntactic forms of
@schememodname[mzscheme]---mostly the ones that correspond to
@scheme[r5rs] forms. See below for a complete list. The
language is intended for use with the textbook @cite["EoPL"].

@defmodule[eopl/eopl]

The following bindings are re-@scheme[provide]d from
@schememodname[mzscheme]:

@reprovide[
           make-parameter
           parameterize
           print-struct

           unquote unquote-splicing 
           quote quasiquote if 
           lambda letrec define-syntax delay let let* let-syntax letrec-syntax
           and or cond case do
	   begin set!

           #%module-begin
	   #%app #%datum #%top #%top-interaction 
           #%require #%provide #%expression

           syntax-rules ...
           cons car cdr pair? map for-each
           caar cadr cdar cddr
           caaar caadr cadar caddr cdaar cdadr cddar cdddr
           caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
           cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
           = < > <= >= max min + - * / 
           abs gcd lcm exp log sin cos tan not eq?
           make-string
           symbol->string string->symbol make-rectangular 
           exact->inexact inexact->exact number->string string->number 
           rationalize output-port? current-input-port current-output-port current-error-port 
           open-input-file open-output-file close-input-port close-output-port
           with-output-to-file transcript-on transcript-off flush-output
           string-length string-ci<=? string-ci>=? string-append 
           string-fill!
           string->list list->string
           vector-length vector-fill!
           vector->list list->vector
           char-alphabetic? char-numeric? char-whitespace? 
           char-upper-case? char-lower-case? char->integer integer->char char-downcase
           call-with-output-file call-with-input-file with-input-from-file
           apply symbol?
           null?
           list? list length append reverse list-tail
           list-ref memq memv member assq assv assoc
           procedure?
           number? complex? real? rational? integer? exact? inexact? zero?
           positive? negative? odd? even? 
           quotient remainder modulo floor ceiling truncate round 
           numerator denominator asin acos atan sqrt
           expt make-polar real-part imag-part angle magnitude input-port?
           read read-char peek-char eof-object?
           char-ready? 
           write display
           newline write-char load 
           string? string string-ref string-set! string=? substring string-copy
           string-ci=? string<? string>? string<=? string>=? string-ci<? string-ci>?
           vector? make-vector vector vector-ref vector-set! 
           char? char=? char<? char>? char<=? char>=? 
           char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=? 
           char-upcase boolean? eqv? equal? 
           force
           call-with-values values dynamic-wind
           eval]

@defform[(define-datatype id predicate-id 
           (variant-id (field-id predicate-expr) ...) 
           ...)]{

  Defines the datatype @scheme[id] and a function
  @scheme[predicate-id] that returns @scheme[#t] for instances of the
  datatype, and @scheme[#f] for any other value.

  Each @scheme[variant-id] is defined as a constructor function that
  creates an instance of the datatype; the constructor takes as many
  arguments as the variant's @scheme[field-id]s, and each argument is
  checked by applying the function produced by the variant's
  @scheme[predicate-expr].

  In DrScheme v209 and older, when constructor-based printing was
  used, variant instances were printed with a @scheme[make-] prefix
  before the variant name.  Thus, for compatibility, in addition to
  @scheme[variant-id], @scheme[make-variant-id] is also defined for
  each @scheme[variant-id] (to the same constructor as
  @scheme[variant-id]).}

@defform*[#:literals (else)
         [(cases datatype-id expr 
            (variant-id (field-id ...) result-expr ...) 
            ...)
          (cases datatype-id expr 
            (variant-id (field-id ...) result-expr ...) 
            ... 
            (else result-expr ...))]]{

  Branches on the datatype instance produced by @scheme[expr], which
  must be an instance of the specified @scheme[datatype-id] that is
  defined with @scheme[define-datatype].}

@deftogether[(
@defidform[sllgen:make-string-scanner]
@defidform[sllgen:make-string-parser]
@defidform[sllgen:make-stream-parser]
@defidform[sllgen:make-define-datatypes]
@defidform[sllgen:show-define-datatypes]
@defidform[sllgen:list-define-datatypes])]{

  Defined in the textbook's Appendix B @cite["EoPL"]. However, the
  DrScheme versions are syntactic forms, instead of procedures, and
  the arguments must be either quoted literal tables or identifiers
  that are defined (at the top level) to quoted literal tables.}

@defthing[sllgen:make-rep-loop procedure?]{

  Defined in the @italic{EoPL} textbook's Appendix B @cite["EoPL"]
  (and still a function).}

@defthing[eopl:error procedure?]{

  As in the book.}

@deftogether[(
  @defproc[(eopl:printf (form string?) (v any/c) ...) void?]
  @defproc[(eopl:pretty-print (v any/c) (port output-port? (current-output-port))) void?])]{

  Same as @schememodname[scheme/base]'s @scheme[printf] and @scheme[pretty-print].}

@deftogether[(
  @defproc[((list-of (pred (any/c . -> . any)) ...+) (x any/c)) boolean?]
  @defproc[(always? (x any/c)) boolean?]
  @defproc[(maybe (pred (any/c . -> . boolean?))) boolean?])]{

  As in the book @cite["EoPL"].}

@defthing[empty empty?]{

  The empty list.}

@defform[(time expr)]{

  Evaluates @scheme[expr], and prints timing information before returning the
  result.}

@defproc[(collect-garbage) void?]{

  Performs a garbage collection (useful for repeatable timings).}

@deftogether[(
  @defform[(trace id ...)]
  @defform[(untrace id ...)])]{

  For debugging: @scheme[trace] redefines each @scheme[id] at the top
  level (bound to a procedure) so that it prints arguments on entry
  and results on exit. The @scheme[untrace] form reverses the action
  of @scheme[trace] for the given @scheme[id]s.

  Tracing a function causes tail-calls in the original function to
  become non-tail calls.}

@defform[(provide provide-spec ...)]{

  Useful only with a module that uses @schememodname[eopl/eopl] as a
  language: exports identifiers from the module. See @mzscheme-provide
  from @schememodname[mzscheme] for more information.}

@defthing[eopl:error-stop (-> any/c)]{

  Defined only in the top-level namespace (i.e., not in a module);
  mutate this variable to install an exception-handling
  thunk. Typically, the handler thunk escapes through a continuation.

  The @schememodname[eopl/eopl] library sets this variable to
  @scheme[#f] in the current namespace when it executes.}

@defproc[(install-eopl-exception-handler) void?]{

  Sets an exception handler to one that checks
  @scheme[eopl:error-stop].

  The @schememodname[eopl/eopl] library calls this function when it
  executes.}

@(bibliography

  (bib-entry #:key "EoPL"
             #:title @elem{@italic{Essentials of Programming Languages}, Third Edition}
             #:location "MIT Press"
             #:date "2008"
             #:url "http://www.eopl3.com/")

)
