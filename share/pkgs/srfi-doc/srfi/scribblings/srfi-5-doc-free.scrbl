#lang scribble/doc
@(require "util.rkt"
          scribble/manual
          scribble/example
          scriblib/render-cond
          scribble/core
          scribble/html-properties
          (for-syntax scheme/base)
          (for-label (except-in scheme/base let)
                     srfi/5
                     racket/stream))

@title[#:tag "srfi-5" #:style 'unnumbered]{
 SRFI 5: A compatible let form with signatures and rest arguments}
@defmodule[srfi/5]

@begin[
 (define-syntax-rule (defrkt rkt-let)
   (begin
     (require (for-label racket/base))
     (define rkt-let (racket let))))
 (defrkt rkt-let)
 (define reference-doc
   '(lib "scribblings/reference/reference.scrbl"))
 (define guide-doc
   '(lib "scribblings/guide/guide.scrbl"))
 (define srfi-nf-doc
   '(lib "srfi/scribblings/srfi-nf.scrbl"))
 ]

Original specification:
@seclink[#:indirect? #t #:doc srfi-nf-doc srfi-5-std-taglet]{SRFI 5}

For @hyperlink[srfi-license-history-url]{historical
 reasons}, the SRFI 5 specification document has a
@seclink[#:indirect? #t #:doc srfi-nf-doc srfi-5-license-taglet]{
 restrictive license} and is not included in the main Racket distribution.

The implementation in @racketmodname[srfi/5] and this
documentation are distributed under the same
@racket-license-link{license} as Racket: only the original
specification document is restrictively licensed.

@defform*[[(let ([id init-expr] ...)
             body ...+)
           (let ([id init-expr] ...+ rest-binding)
             body ...+)
           (let loop-id ([id init-expr] ... maybe-rest-binding)
             body ...+)
           (let (loop-id [id init-expr] ... maybe-rest-binding)
             body ...+)]
          #:grammar
          ([maybe-rest-binding code:blank rest-binding]
           [rest-binding (code:line rest-id rest-init-expr ...)])]{

 Like @rkt-let from @racketmodname[racket/base], but
 extended to support additional variants of
 @tech[#:doc reference-doc]{named @rkt-let}.

 As with @rkt-let from @racketmodname[racket/base], SRFI 5's
 @racket[let] form conceptually expands to the immediate
 application of a function to the values of the
 @racket[init-expr]s: the @racket[id]s are bound in the
 @racket[body]s (but not in any @racket[init-expr]s or
 @racket[rest-init-expr]s), and @racket[loop-id], if present,
 is bound in the @racket[body]s to the function itself,
 allowing it to be used recursively. An @racket[id] or a
 @racket[rest-id] can shadow @racket[loop-id], but the
 @racket[rest-id] (if given) and all @racket[is]s much be
 distinct.

 SRFI 5's @racket[let] adds support for a syntax like
 @racket[define]'s @seclink[#:doc guide-doc "Function_Shorthand"]{
  function shorthand}, which allows the bindings to be written in a
 syntax resembling an application of the function bound to
 @racket[loop-id].

 Additionally, SRFI 5's @racket[let] adds support for
 @tech[#:doc guide-doc]{rest arguments}. If a
 @racket[rest-id] is present, the function bound to
 @racket[loop-id] (or the conceptual anonymous function, if
 @racket[loop-id] is not used) will accept an unlimited
 number of additional arguments after its required
 by-position arguments, and the @racket[rest-id] will be
 bound in the @racket[body]s (but not in any
 @racket[init-expr]s or @racket[rest-init-expr]s) to a list
 of those additional arguments. The values of the
 @racket[rest-init-expr]s are supplied as arguments to the
 initial, implicit application when the @racket[let] form is
 evaluated, so the initial value bound to @racket[rest-id] is
 @racket[(list rest-init-expr ...)].

 @margin-note{Unlike the @racket[_kw-formals] of
  @racket[lambda] and @racket[define] or the @racket[_formals]
  of @racket[case-lambda], the bindings of SRFI 5's
  @racket[let], with or without a @racket[rest-binding], are
  always a proper (syntactic) list.}

 A @racket[rest-binding] can be used with both the
 @racket[define]-like and the
 @tech[#:doc reference-doc]{named-@rkt-let}--like variants of
 @racket[let]. It is also possible to use @racket[rest-id]
 without any @racket[loop-id]; however, as specified in the
 grammar, at least one @racket[id]--@racket[init-expr] pair
 is required in that case. (Otherwise, there would be an
 ambiguity with the @racket[define]-like variant).

 @examples[
 #:eval (make-base-eval '(require srfi/5)) #:once
 (code:comment "define-like bindings")
 (define (factorial n)
   (let (fact [n n] [acc 1])
     (if (zero? n)
         acc
         (fact (sub1 n) (* n acc)))))
 (eval:check (factorial 5) 120)
 (eval:check (factorial 11) 39916800)
 (code:comment "rest arguments with named-let--like bindings")
 (eval:check (let reverse-onto ([lst '(a b c)]
                                tail)
               (if (null? lst)
                   tail
                   (apply reverse-onto (cdr lst) (car lst) tail)))
             '(c b a))
 (eval:check (let reverse-onto ([lst '(a b c)]
                                tail 'x 'y 'z)
               (if (null? lst)
                   tail
                   (apply reverse-onto (cdr lst) (car lst) tail)))
             '(c b a x y z))
 (eval:check (let no-evens (lst 1 2 3 4 5)
               (cond
                 [(null? lst)
                  '()]
                 [(even? (car lst))
                  (apply no-evens (cdr lst))]
                 [else
                  (cons (car lst) (apply no-evens (cdr lst)))]))
             '(1 3 5))
 (code:comment "rest arguments with define-like bindings")
 (eval:check (let (reverse-onto [lst '(a b c)] tail)
               (if (null? lst)
                   tail
                   (apply reverse-onto (cdr lst) (car lst) tail)))
             '(c b a))
 (eval:check (let (reverse-onto [lst '(a b c)] . [tail 'x 'y 'z])
               (if (null? lst)
                   tail
                   (apply reverse-onto (cdr lst) (car lst) tail)))
             '(c b a x y z))
 (eval:check (let (loop [continue? 0] args 'a 'a1 'a2)
               (case continue?
                 [(0) (cons args (loop 1 'b))]
                 [(1) (cons args (loop 2 'c 'd))]
                 [else (list args)]))
             '((a a1 a2) (b) (c d)))
 (code:comment "rest arguments without any loop-id")
 (eval:check (let ([x 1]
                   [y 2]
                   z 3 4 5 6 7)
               (list* x y z))
             '(1 2 3 4 5 6 7))
 ]
}
