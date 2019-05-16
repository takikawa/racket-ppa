#lang scribble/manual
@(require scribble/struct scribble/decode scribble/eval
          (for-label racket/base racket/contract syntax/contract racket/syntax syntax/parse))

@(define the-eval (make-base-eval))
@(the-eval '(require racket/contract (for-syntax racket/base syntax/contract)))

@title[#:tag "wrapc"]{Contracts for Macro Subexpressions}

@defmodule[syntax/contract]

This library provides a procedure @racket[wrap-expr/c] for applying
contracts to macro subexpressions.

@defproc[(wrap-expr/c [contract-expr syntax?]
                      [expr syntax?]
                      [#:arg? arg? any/c #t]
                      [#:positive pos-blame
                                  (or/c syntax? string? module-path-index?
                                        'from-macro 'use-site 'unknown)
                                  'from-macro]
                      [#:negative neg-blame
                                  (or/c syntax? string? module-path-index?
                                        'from-macro 'use-site 'unknown)
                                  'use-site]
                      [#:name expr-name
                              (or/c identifier? symbol? string? #f) #f]
                      [#:macro macro-name
                               (or/c identifier? symbol? string? #f) #f]
                      [#:context context (or/c syntax? #f) (current-syntax-context)])
         syntax?]{

Returns a syntax object representing an expression that applies the
contract represented by @racket[contract-expr] to the value produced
by @racket[expr].

The other arguments have the same meaning as for @racket[expr/c].

@examples[#:eval the-eval
(define-syntax (myparameterize1 stx)
  (syntax-case stx ()
    [(_ ([p v]) body)
     (with-syntax ([cp (wrap-expr/c
                        #'parameter? #'p
                        #:name "the parameter argument"
                        #:context stx)])
       #'(parameterize ([cp v]) body))]))
(myparameterize1 ([current-input-port
                   (open-input-string "(1 2 3)")])
  (read))
(myparameterize1 (['whoops 'something])
  'whatever)

(module mod racket
  (require (for-syntax syntax/contract))
  (define-syntax (app stx)
    (syntax-case stx ()
      [(app f arg)
       (with-syntax ([cf (wrap-expr/c
                          #'(-> number? number?)
                          #'f
                          #:name "the function argument"
                          #:context stx)])
         #'(cf arg))]))
  (provide app))
(require 'mod)
(app add1 5)
(app add1 'apple)
(app (lambda (x) 'pear) 5)
]

@history[#:added "6.3" #:changed "7.2.0.3" @elem{Added the
@racket[#:arg?] keyword argument and changed the default values and
interpretation of the @racket[#:positive] and @racket[#:negative]
arguments.}]}

@close-eval[the-eval]
