#lang scribble/doc
@(require scribble/manual scribble/eval racket/sandbox
          "utils.rkt"
          (for-label racket/base racket/contract))

@title[#:tag "contracts-gotchas"]{Gotchas}

@ctc-section{Contracts and @racket[eq?]}

As a general rule, adding a contract to a program should
either leave the behavior of the program unchanged, or
should signal a contract violation. And this is almost true
for Racket contracts, with one exception: @racket[eq?].

The @racket[eq?] procedure is designed to be fast and does
not provide much in the way of guarantees, except that if it
returns true, it means that the two values behave
identically in all respects. Internally, this is implemented
as pointer equality at a low-level so it exposes information
about how Racket is implemented (and how contracts are
implemented). 

Contracts interact poorly with @racket[eq?] because function
contract checking is implemented internally as wrapper
functions. For example, consider this module:
@racketmod[
racket

(define (make-adder x)
  (if (= 1 x)
      add1
      (lambda (y) (+ x y))))
(provide (contract-out 
          [make-adder (-> number? (-> number? number?))]))
]

It exports the @racket[make-adder] function that is the usual curried
addition function, except that it returns Racket's @racket[add1] when
its input is @racket[1].

You might expect that
@racketblock[
(eq? (make-adder 1)
     (make-adder 1))
]

would return @racket[#t], but it does not. If the contract were
changed to @racket[any/c] (or even @racket[(-> number? any/c)]), then
the @racket[eq?] call would return @racket[#t].

Moral: Do not use @racket[eq?] on values that have contracts.

@ctc-section[#:tag "gotcha-nested"]{Contract boundaries and @racket[define/contract]}

The contract boundaries established by @racket[define/contract], which
creates a nested contract boundary, are sometimes unintuitive. This is
especially true when multiple functions or other values with contracts
interact. For example, consider these two interacting functions:

@(define e2 (make-base-eval))
@(interaction-eval #:eval e2 (require racket/contract))
@interaction[#:eval e2
(define/contract (f x)
  (-> integer? integer?)
  x)
(define/contract (g)
  (-> string?)
  (f "not an integer"))
(g)
]

One might expect that the function @racket[g] will be blamed
for breaking the terms of its contract with @racket[f]. 
Blaming @racket[g] would be right if @racket[f] and @racket[g]
were directly establishing contracts with each other.
They aren't, however. Instead, the access between @racket[f]
and @racket[g] is mediated through the top-level of the enclosing
module.

More precisely, @racket[f] and the top-level of the module have
the @racket[(-> integer? integer?)] contract mediating their
interaction; @racket[g] and the top-level have @racket[(-> string?)]
mediating their interaction, but there is no contract directly
between @racket[f] and @racket[g]. This means that the reference to
@racket[f] in the body of @racket[g] is really the top-level
of the module's responsibility, not @racket[g]'s. In other words,
the function @racket[f] has been given to @racket[g] with
no contract between @racket[g] and the top-level and thus
the top-level is blamed.

If we wanted to add a contract between @racket[g] and the
top-level, we can use @racket[define/contract]'s
@racket[#:freevar] declaration and see the expected blame:

@interaction[#:eval e2
(define/contract (f x)
  (-> integer? integer?)
  x)
(define/contract (g)
  (-> string?)
  #:freevar f (-> integer? integer?)
  (f "not an integer"))
(g)
]
@(close-eval e2)

Moral: if two values with contracts should interact,
       put them in separate modules with contracts at
       the module boundary or use @racket[#:freevar].

@ctc-section[#:tag "exists-gotcha"]{Exists Contracts and Predicates}

Much like the @racket[eq?] example above, @racket[#:∃] contracts
can change the behavior of a program.

Specifically,
the @racket[null?] predicate (and many other predicates) return @racket[#f]
for @racket[#:∃] contracts, and changing one of those contracts to @racket[any/c]
means that @racket[null?] might now return @racket[#t] instead, resulting in
arbitrarily different behavior depending on how this boolean might flow around
in the program.

Moral: Do not use predicates on @racket[#:∃] contracts.

@ctc-section{Defining Recursive Contracts}

When defining a self-referential contract, it is natural to use
@racket[define]. For example, one might try to write a contract on
streams like this:

@(define e (make-base-eval))
@(interaction-eval #:eval e (require racket/contract))
@interaction[
  #:eval e
(define stream/c
  (promise/c
   (or/c null?
         (cons/c number? stream/c))))
]
@close-eval[e]

Unfortunately, this does not work because the value of
@racket[stream/c] is needed before it is defined. Put another way, all
of the combinators evaluate their arguments eagerly, even though the
values that they accept do not.

Instead, use
@racketblock[
(define stream/c
  (promise/c
   (or/c
    null?
    (cons/c number? (recursive-contract stream/c)))))
]

The use of @racket[recursive-contract] delays the evaluation of the
identifier @racket[stream/c] until after the contract is first
checked, long enough to ensure that @racket[stream/c] is defined.

See also @ctc-link["lazy-contracts"].

@ctc-section{Mixing @racket[set!] and @racket[contract-out]}

The contract library assumes that variables exported via
@racket[contract-out] are not assigned to, but does not enforce
it. Accordingly, if you try to @racket[set!] those variables, you 
may be surprised. Consider the following example:

@interaction[
(module server racket
  (define (inc-x!) (set! x (+ x 1)))
  (define x 0)
  (provide (contract-out [inc-x! (-> void?)]
                         [x integer?])))

(module client racket
  (require 'server)

  (define (print-latest) (printf "x is ~s\n" x))

  (print-latest)
  (inc-x!)
  (print-latest))

(require 'client)
]

Both calls to @racket[print-latest] print @racket[0], even though the
value of @racket[x] has been incremented (and the change is visible
inside the module @racket[x]).

To work around this, export accessor functions, rather than
exporting the variable directly, like this:

@racketmod[
racket

(define (get-x) x)
(define (inc-x!) (set! x (+ x 1)))
(define x 0)
(provide (contract-out [inc-x! (-> void?)]
                       [get-x (-> integer?)]))
]

Moral: This is a bug that we will address in a future release.
