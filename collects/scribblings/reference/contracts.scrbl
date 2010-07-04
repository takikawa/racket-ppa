#lang scribble/doc
@(require "mz.ss")
@(require (for-label syntax/modcollapse))

@title[#:tag "contracts" #:style 'toc]{Contracts}

This chapter is long on detail and short on the motivation
and pragmatics of using contracts. See
@guidesecref["contracts"] in the Guide for more of the
latter and less of the former.

A @defterm{contract} controls the flow of values to ensure that the
expectations of one party are met by another party.  The
@scheme[provide/contract] form is the primary mechanism for
associating a contract with a binding.

Note that all of the combinators that accept contracts as arguments
use @scheme[coerce-contract], meaning that symbols, booleans, strings,
bytess, characters, numbers, regular expressions, and predicates
are all implicitly converted into contracts.

@note-lib[scheme/contract #:use-sources (scheme/private/contract-ds
                                         scheme/private/contract
                                         scheme/private/contract-guts)]

@local-table-of-contents[]

@; ----------------------------------------

@section{Data-structure Contracts}

A @deftech{flat contract} can be fully checked immediately for
a given value.

@defproc[(flat-contract [predicate (any/c . -> . any/c)]) flat-contract?]{

Constructs a @tech{flat contract} from @scheme[predicate]. A value
satisfies the contract if the predicate returns a true value.}


@defproc[(flat-named-contract [type-name string?][predicate (any/c . -> . any)])
         flat-contract?]{

Like @scheme[flat-contract], but the first argument must be a string
used for error reporting. The string describes the type that the
predicate checks for.}

@defthing[any/c flat-contract?]{

A flat contract that accepts any value.

When using this contract as the result portion of a function contract,
consider using @scheme[any] instead; using @scheme[any] leads to
better memory performance, but it also allows multiple results.}


@defthing[none/c flat-contract?]{

A @tech{flat contract} that accepts no values.}


@defproc[(or/c [contract (or/c contract? (any/c . -> . any/c))] ...)
         contract?]{

Takes any number of predicates and higher-order contracts and returns
a contract that accepts any value that any one of the contracts
accepts, individually.

If all of the arguments are procedures or @tech{flat contracts}, the
result is a @tech{flat contract}. If only one of the arguments is a
higher-order contract, the result is a contract that just checks the
flat contracts and, if they don't pass, applies the higher-order
contract.

If there are multiple higher-order contracts, @scheme[or/c] uses
@scheme[contract-first-order-passes?] to distinguish between
them. More precisely, when an @scheme[or/c] is checked, it first
checks all of the @tech{flat contracts}. If none of them pass, it
calls @scheme[contract-first-order-passes?] with each of the
higher-order contracts. If only one returns true, @scheme[or/c] uses
that contract. If none of them return true, it signals a contract
violation. If more than one returns true, it signals an error
indicating that the @scheme[or/c] contract is malformed.

The @scheme[or/c] result tests any value by applying the contracts in
order, from left to right, with the exception that it always moves the
non-@tech{flat contracts} (if any) to the end, checking them last.}
 
@defproc[(and/c [contract (or/c contract? (any/c . -> . any/c))] ...)
         contract?]{

Takes any number of contracts and returns a contract that checks that
accepts any value that satisfies all of the contracts, simultaneously.

If all of the arguments are procedures or @tech{flat contracts},
the result is a @tech{flat contract}.

The contract produced by @scheme[and/c] tests any value by applying
the contracts in order, from left to right.}


@defproc[(not/c [flat-contract (or/c flat-contract? (any/c . -> . any/c))]) 
         flat-contract?]{

Accepts a flat contracts or a predicate and returns a flat contract
that checks the inverse of the argument.}


@defproc[(=/c [z number?]) flat-contract?]{

Returns a flat contract that requires the input to be a number and
@scheme[=] to @scheme[z].}


@defproc[(</c [n real?]) flat-contract?]{

Returns a flat contract that requires the input to be a number and
@scheme[<] to @scheme[n].}


@defproc[(>/c [n number?]) flat-contract?]{
Like @scheme[</c], but for @scheme[>].}


@defproc[(<=/c [n number?]) flat-contract?]{
Like @scheme[</c], but for @scheme[<=].}


@defproc[(>=/c [n number?]) flat-contract?]{
Like @scheme[</c], but for @scheme[>=].}

@defproc[(between/c [n number?] [m number?])
flat-contract?]{ Returns a flat contract that requires the
input to be a between @scheme[n] and @scheme[m] or equal to
one of them.}

@defproc[(real-in [n real?][m real?]) flat-contract?]{

Returns a flat contract that requires the input to be a real number
between @scheme[n] and @scheme[m], inclusive.}


@defproc[(integer-in [j exact-integer?][k exact-integer?]) flat-contract?]{

Returns a flat contract that requires the input to be an exact integer
between @scheme[j] and @scheme[k], inclusive.}


@defthing[natural-number/c flat-contract?]{

A flat contract that requires the input to be an exact non-negative integer.}


@defproc[(string-len/c [len exact-nonnegative-integer?]) flat-contract?]{

Returns a flat contract that recognizes strings that have fewer than
@scheme[len] characters.}


@defthing[false/c flat-contract?]{

This is just @scheme[#f]. It is here for backwards compatibility.}


@defthing[printable/c flat-contract?]{

A flat contract that recognizes values that can be written out and
read back in with @scheme[write] and @scheme[read].}


@defproc[(one-of/c [v any/c] ...+) flat-contract?]{

Accepts any number of atomic values and returns a flat contract that
recognizes those values, using @scheme[eqv?]  as the comparison
predicate.  For the purposes of @scheme[one-of/c], atomic values are
defined to be: characters, symbols, booleans, null keywords, numbers,
void, and undefined.}


@defproc[(symbols [sym symbol?] ...+) flat-contract?]{

Accepts any number of symbols and returns a flat contract that
recognizes those symbols.}


@defproc[(vectorof [c (or/c flat-contract? (any/c . -> . any/c))]) flat-contract?]{

Accepts a @tech{flat contract} (or a predicate that is converted to a
flat contract via @scheme[flat-contract]) and returns a flat contract
that checks for vectors whose elements match the original contract.}


@defproc[(vector-immutableof [c (or/c contract? (any/c . -> . any/c))]) contract?]{

Like @scheme[vectorof], but the contract needs not be a @tech{flat
contract}. Beware that when this contract is applied to a
value, the result is not @scheme[eq?] to the input.}


@defproc[(vector/c [c (or/c flat-contract? (any/c . -> . any/c))] ...) flat-contract?]{

Accepts any number of flat contracts (or predicates that are converted
to flat contracts via @scheme[flat-contract]) and returns a
flat-contract that recognizes vectors. The number of elements in the
vector must match the number of arguments supplied to
@scheme[vector/c], and each element of the vector must match the
corresponding flat contract.}


@defproc[(vector-immutable/c [c (or/c contract? (any/c . -> . any/c))] ...) contract?]{

Like @scheme[vector/c], but the individual contracts need not be
@tech{flat contracts}. Beware that when this contract is applied to a
value, the result is not @scheme[eq?] to the input.}


@defproc[(box/c [c (or/c flat-contract? (any/c . -> . any/c))]) flat-contract?]{

Returns a flat-contract that recognizes boxes. The content of the box
must match @scheme[c].}


@defproc[(box-immutable/c [c (or/c contract? (any/c . -> . any/c))]) contract?]{

Like @scheme[box/c], but @scheme[c] need not be @tech{flat
contract}. Beware that when this contract is applied to a value, the
result is not @scheme[eq?] to the input.}


@defproc[(listof [c (or/c contract? (any/c . -> . any/c))]) contract?]{

Returns a contract that recognizes a list whose every element matches
the contract @scheme[c]. Beware that when this contract is applied to
a value, the result is not necessarily @scheme[eq?] to the input.}


@defproc[(cons/c [car-c contract?][cdr-c contract?]) contract?]{

Produces a contract the recognizes pairs first and second elements
match @scheme[car-c] and @scheme[cdr-c], respectively. Beware that
when this contract is applied to a value, the result is not
necessarily @scheme[eq?] to the input.}


@defproc[(list/c [c (or/c contract? (any/c . -> . any/c))] ...) contract?]{

Produces a contract for a list. The number of elements in the list
must match the number of arguments supplied to @scheme[list/c], and
each element of the list must match the corresponding contract. Beware
that when this contract is applied to a value, the result is not
necessarily @scheme[eq?] to the input.}


@defproc[(syntax/c [c flat-contract?]) flat-contract?]{

Produces a flat contract that recognizes syntax objects whose
@scheme[syntax-e] content matches @scheme[c].}


@defform[(struct/c struct-id flat-contract-expr ...)]{

Produces a flat contract that recognizes instances of the structure
type named by @scheme[struct-id], and whose field values match the
@tech{flat contracts} produced by the @scheme[flat-contract-expr]s.}


@defproc[(parameter/c [c contract?]) contract?]{

Produces a contract on parameters whose values must match
@scheme[contract].}


@defform[(flat-rec-contract id flat-contract-expr ...)]

Constructs a recursive @tech{flat contract}. A
@scheme[flat-contract-expr] can refer to @scheme[id] to refer
recursively to the generated contract.

For example, the contract

@schemeblock[
   (flat-rec-contract sexp
     (cons/c sexp sexp)
     number?
     symbol?)
]

is a flat contract that checks for (a limited form of)
S-expressions. It says that an @scheme[sexp] is either two
@scheme[sexp] combined with @scheme[cons], or a number, or a symbol.

Note that if the contract is applied to a circular value, contract
checking will not terminate.}


@defform[(flat-murec-contract ([id flat-contract-expr ...] ...) body ...+)]{

A generalization of @scheme[flat-rec-contracts] for defining several
mutually recursive flat contracts simultaneously. Each @scheme[id] is
visible in the entire @scheme[flat-murec-contract] form, and the
result of the final @scheme[body] is the result of the entire form.}


@defidform[any]{

Represents a contract that is always satisfied. In particular, it can accept
multiple values.  It can only be used in a result position of contracts like
@scheme[->]. Using @scheme[any] elsewhere is a syntax error.}

@defform[(promise/c expr)]{

Constructs a contract on a promise. The contract does not force the
promise, but when the promise is forced, the contract checks that the
result value meets the contract produced by @scheme[expr].}

@; ------------------------------------------------------------------------

@section{Function Contracts}

A @deftech{function contract} wraps a procedure to delay
checks for its arguments and results. There are three
primary function contract combinators that have increasing
amounts of expressiveness and increasing additional
overheads. The first @scheme[->] is the cheapest. It
generates wrapper functions that can call the original
function directly. Contracts built with @scheme[->*] require
packaging up arguments as lists in the wrapper function and
then using either @scheme[keyword-apply] or
@scheme[apply]. Finally, @scheme[->d] is the most expensive,
because it requires delaying the evaluation of the contract
expressions for the domain and range until the function
itself is called or returns.

The @scheme[case->] contract is a specialized contract,
designed to match @scheme[case-lambda] and
@scheme[unconstrained-domain->] allows range checking
without requiring that the domain have any particular shape
(see below for an exmaple use).

@defform*/subs[#:literals (any values)
               [(-> dom ... range)]
               ([dom dom-expr (code:line keyword dom-expr)]
                [range range-expr (values range-expr ...) any])]{

Produces a contract for a function that accepts a fixed
number of arguments and returns either a fixed number of
results or completely unspecified results (the latter when
@scheme[any] is specified).

Each @scheme[dom-expr] is a contract on an argument to a
function, and each @scheme[res-expr] is a contract on a
result of the function.

@margin-note{Using an @scheme[->] between two whitespace-delimited
@schemeparenfont{.}s is the same as putting the @scheme[->] right
after the enclosing open parenthesis. See
@guidesecref["lists-and-syntax"] or @secref["parse-pair"] for more
information.}

For example,

@schemeblock[(integer? boolean? . -> . integer?)] 

produces a contract on functions of two arguments. The first argument
must be an integer, and the second argument must be a boolean. The
function must produce an integer.

A domain specification may include a keyword. If so, the function must
accept corresponding (mandatory) keyword arguments, and the values for
the keyword arguments must match the corresponding contracts. For
example:

@schemeblock[(integer? #:x boolean? . -> . integer?)]

is a contract on a function that accepts a by-position argument that
is an integer and a @scheme[#:x] argument is that a boolean.

If @scheme[any] is used as the last sub-form for @scheme[->], no
contract checking is performed on the result of the function, and
tail-recursion is preserved. Note that the function may return
multiple values in that case.

If @scheme[(values res-expr ...)] is used as the last sub-form of
@scheme[->], the function must produce a result for each contract, and
each values must match its respective contract.}


@defform*/subs[#:literals (any values)
          [(->* (mandatory-dom ...) (optional-dom ...) rest range)]
          ([mandatory-dom dom-expr (code:line keyword dom-expr)]
           [optional-dom dom-expr (code:line keyword dom-expr)]
           [rest (code:line) (code:line #:rest rest-expr)]
           [range range-expr (values range-expr ...) any])]{

The @scheme[->*] contract combinator produces contracts for
functions that accept optional arguments (either keyword or
positional) and or arbitrarily many arguments. The first
clause of a @scheme[->*] contract describes the mandatory
arguments, and is similar to the argument description of a
@scheme[->] contract. The second clause describes the
optional arguments. The last clause describes the range of
the function. It can either be @scheme[any] or a
sequence of contracts, indicating that the function must
return multiple values. If present, the @scheme[rest-expr]
contract governs the arguments in the rest parameter.

As an example, the contract 
@schemeblock[(->* () (boolean? #:x integer?) #:rest (listof symbol?) symbol?)] 
matches functions that optionally accept a boolean, an
integer keyword argument @scheme[#:x] and arbitrarily more
symbols, and that return a symbol.

}

@defform*/subs[#:literals (any values)
[(->d (mandatory-dependent-dom ...) 
      (optional-dependent-dom ...) 
      dependent-rest
      pre-cond
      dep-range)]
([mandatory-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [optional-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [dependent-rest (code:line) (code:line #:rest id rest-expr)]
 [pre-cond (code:line) (code:line #:pre-cond boolean-expr)]
 [dep-range any
            (code:line [_ range-expr] post-cond)
            (code:line (values [_ range-expr] ...) post-cond)
            (code:line [id range-expr] post-cond)
            (code:line (values [id range-expr] ...) post-cond)]
 [post-cond (code:line) (code:line #:post-cond boolean-expr)]
)]{

The @scheme[->d] is similar in shape to @scheme[->*], with
two extensions: names have been added to each argument and
result, which allows the contracts to depend on the values
of the arguments and results, and pre- and post-condition
expressions have been added in order to express contracts
that are not naturally tied to a particular argument or
result.

The first two subforms of a @scheme[->d] contract cover the
mandatory and optional arguments. Following that is an
optional rest-args contract, and an optional
pre-condition. The @scheme[dep-range] non-terminal covers
the possible post-condition contracts. If it is
@scheme[any], then any result (or results) are
allowed. Otherwise, the result contract can be a name and a
result contract, or a multiple values return and, in either
of the last two cases, it may be optionally followed by a
post-condition.

Each of the @scheme[id]s on an argument (including the rest
argument) is visible in all of the sub-expressions of
@scheme[->d]. Each of the @scheme[id]s on a result is
visible in the subexpressions of the @scheme[dep-range].

If the identifier position of the range contract is
@scheme[_] (an underscore), then the range contract
expressions are evaluated when the function is called (and
the underscore is not bound in the range). Otherwise the
range expressions are evaluated when the function returns.

If there are optional arguments that are not supplied, then 
the corresponding variables will be bound to a special value
called the @scheme[unsupplied-arg] value.
}

@defform*/subs[#:literals (any values ->)
[(case-> (-> dom-expr ... rest range) ...)]
([rest (code:line) (code:line #:rest rest-expr)]
 [range range-expr (values range-expr ...) any])]{
This contract form is designed to match
@scheme[case-lambda]. Each argument to @scheme[case->] is a
contract that governs a clause in the
@scheme[case-lambda]. If the @scheme[#:rest] keyword is
present, the corresponding clause must accept an arbitrary
number of arguments. The @scheme[range] specification is
just like that for @scheme[->] and @scheme[->*]. 
}


@defform[(unconstrained-domain-> res-expr ...)]{

Constructs a contract that accepts a function, but makes no constraint
on the function's domain. The @scheme[res-expr]s determine the number
of results and the contract for each result.

Generally, this contract must be combined with another contract to
ensure that the domain is actually known to be able to safely call the
function itself.

For example, the contract

@schemeblock[
(provide/contract 
 [f (->d ([size natural-number/c]
          [proc (and/c (unconstrained-domain-> number?)
                       (lambda (p) 
                         (procedure-arity-includes? p size)))])
         ()
         number?)])
]

says that the function @scheme[f] accepts a natural number
and a function. The domain of the function that @scheme[f]
accepts must include a case for @scheme[size] arguments,
meaning that @scheme[f] can safely supply @scheme[size]
arguments to its input.

For example, the following is a definition of @scheme[f] that cannot
be blamed using the above contract:

@schemeblock[
(define (f i g) 
  (apply g (build-list i add1)))
]}


@; ------------------------------------------------------------------------

@section{Lazy Data-structure Contracts}

@defform[
(define-contract-struct id (field-id ...))
]{

Like @scheme[define-struct], but with two differences: it does not
define field mutators, and it does define two contract constructors:
@scheme[id]@schemeidfont{/c} and @scheme[id]@schemeidfont{/dc}. The
first is a procedure that accepts as many arguments as there are
fields and returns a contract for struct values whose fields match the
arguments. The second is a syntactic form that also produces contracts
on the structs, but the contracts on later fields may depend on the
values of earlier fields. 

The generated contract combinators are @italic{lazy}: they only verify
the contract holds for the portion of some data structure that is
actually inspected. More precisely, a lazy data structure contract is
not checked until a selector extracts a field of a struct.

@specsubform/subs[
(#,(elem (scheme id) (schemeidfont "/dc")) field-spec ...)

([field-spec
  [field-id contract-expr]
  [field-id (field-id ...) contract-expr]])
]{

In each @scheme[field-spec] case, the first @scheme[field-id]
specifies which field the contract applies to; the fields must be
specified in the same order as the original
@scheme[define-contract-struct]. The first case is for when the
contract on the field does not depend on the value of any other
field. The second case is for when the contract on the field does
depend on some other fields, and the parenthesized @scheme[field-id]s
indicate which fields it depends on; these dependencies can only be to
earlier fields.}

As an example, consider the following module:

@(begin
#reader scribble/comment-reader
[schemeblock
(module product mzscheme
  (require mzlib/contract)

  (define-contract-struct kons (hd tl))
  
  ;; @scheme[sorted-list/gt : number -> contract]
  ;; produces a contract that accepts
  ;; sorted kons-lists whose elements
  ;; are all greater than @scheme[num].
  (define (sorted-list/gt num)
    (or/c null?
          (kons/dc [hd (>=/c num)]
                   [tl (hd) (sorted-list/gt hd)])))
  
  ;; @scheme[product : kons-list -> number]
  ;; computes the product of the values
  ;; in the list. if the list contains
  ;; zero, it avoids traversing the rest
  ;; of the list.
  (define (product l)
    (cond
      [(null? l) 1]
      [else
       (if (zero? (kons-hd l))
           0
           (* (kons-hd l) 
              (product (kons-tl l))))]))
  
  (provide kons? make-kons kons-hd kons-tl)
  (provide/contract [product (-> (sorted-list/gt -inf.0) number?)]))
])

The module provides a single function, @scheme[product] whose contract
indicates that it accepts sorted lists of numbers and produces
numbers. Using an ordinary flat contract for sorted lists, the product
function cannot avoid traversing having its entire argument be
traversed, since the contract checker will traverse it before the
function is called. As written above, however, when the product
function aborts the traversal of the list, the contract checking also
stops, since the @scheme[kons/dc] contract constructor generates a
lazy contract.}

@; ------------------------------------------------------------------------

@section{Attaching Contracts to Values}

@defform/subs[
#:literals (struct rename)
(provide/contract p/c-item ...)
([p/c-item
  (struct id ((id contract-expr) ...))
  (struct (id identifier) ((id contract-expr) ...))
  (rename orig-id id contract-expr)
  (id contract-expr)])]{

Can only appear at the top-level of a @scheme[module]. As with
@scheme[provide], each @scheme[id] is provided from the module. In
addition, clients of the module must live up to the contract specified
by @scheme[contract-expr] for each export.

The @scheme[provide/contract] form treats modules as units of
blame. The module that defines the provided variable is expected to
meet the positive (co-variant) positions of the contract. Each module
that imports the provided variable must obey the negative
(contra-variant) positions of the contract.

Only uses of the contracted variable outside the module are
checked. Inside the module, no contract checking occurs.

The @scheme[rename] form of a @scheme[provide/contract] exports the
first variable (the internal name) with the name specified by the
second variable (the external name).

The @scheme[struct] form of a @scheme[provide/contract] clause
provides a structure definition, and each field has a contract that
dictates the contents of the fields. The struct definition must come
before the provide clause in the module's body. If the struct has a
parent, the second @scheme[struct] form (above) must be used, with the
first name referring to the struct itself and the second name
referring to the parent struct. Unlike @scheme[define-struct],
however, all of the fields (and their contracts) must be listed. The
contract on the fields that the sub-struct shares with its parent are
only used in the contract for the sub-struct's maker, and the selector
or mutators for the super-struct are not provided.}

@defform[(define/contract id contract-expr init-value-expr)]{

Attaches the contract @scheme[contract-expr] to
@scheme[init-value-expr] and binds that to @scheme[id].

The @scheme[define/contract] form treats individual definitions as
units of blame. The definition itself is responsible for positive
(co-variant) positions of the contract and each reference to
@scheme[id] (including those in the initial value expression) must
meet the negative positions of the contract.

Error messages with @scheme[define/contract] are not as clear as those
provided by @scheme[provide/contract], because
@scheme[define/contract] cannot detect the name of the definition
where the reference to the defined variable occurs. Instead, it uses
the source location of the reference to the variable as the name of
that definition.}

@defform*[[(contract contract-expr to-protect-expr
                     positive-blame-expr negative-blame-expr)
           (contract contract-expr to-protect-expr 
                     positive-blame-expr negative-blame-expr
                     contract-source-info)]]{

The primitive mechanism for attaching a contract to a value. The
purpose of @scheme[contract] is as a target for the expansion of some
higher-level contract specifying form.

The @scheme[contract] expression adds the contract specified by
@scheme[contract-expr] to the value produced by
@scheme[to-protect-expr]. The result of a @scheme[contract] expression
is the result of the @scheme[to-protect-expr] expression, but with the
contract specified by @scheme[contract-expr] enforced on
@scheme[to-protect-expr].

The values of @scheme[positive-blame-expr] and
@scheme[negative-blame-expr] must be symbols indicating how to assign
blame for positive and negative positions of the contract specified by
@scheme[contract-expr]. 

If specified, @scheme[contract-source-info], indicates where the
contract was assumed. Its value must be a either:
@itemize{
@item{a list of two elements: @scheme[srcloc] struct and
either a string or @scheme[#f]. The srcloc struct inidates
where the contract was assumed. Its @tt{source} field
should be a syntax object, and @scheme[module-path-index-resolve]
is called on it to extract the path of syntax object.

If the second element of
the list is not @scheme[#f], it is used as the name of the
identifier whose contract was assumed.}

@item{a syntax object specifying the
source location of the location where the contract was assumed. If the
syntax object wraps a symbol, the symbol is used as the name of the
primitive whose contract was assumed.}
}

If absent, it defaults to the source location of the
@scheme[contract] expression with no identifying name.

The second form above is not recommended, because mzscheme strips
source location information from compiled files.
}

@; ------------------------------------------------------------------------

@section{Building New Contract Combinators}

Contracts are represented internally as functions that
accept information about the contract (who is to blame,
source locations, etc) and produce projections (in the
spirit of Dana Scott) that enforce the contract. A
projection is a function that accepts an arbitrary value,
and returns a value that satisfies the corresponding
contract. For example, a projection that accepts only
integers corresponds to the contract @scheme[(flat-contract
integer?)], and can be written like this:

@schemeblock[
(define int-proj
  (lambda (x)
    (if (integer? x)
        x
        (signal-contract-violation))))
]

As a second example, a projection that accepts unary functions
on integers looks like this:

@schemeblock[
(define int->int-proj
  (lambda (f)
    (if (and (procedure? f)
             (procedure-arity-includes? f 1))
        (lambda (x)
          (int-proj (f (int-proj x))))
        (signal-contract-violation))))
]

Although these projections have the right error behavior,
they are not quite ready for use as contracts, because they
do not accomodate blame, and do not provide good error
messages. In order to accomodate these, contracts do not
just use simple projections, but use functions that accept
the names of two parties that are the candidates for blame,
as well as a record of the source location where the
contract was established and the name of the contract. They
can then, in turn, pass that information
to @scheme[raise-contract-error] to signal a good error
message (see below for details on its behavior).

Here is the first of those two projections, rewritten for
use in the contract system:

@schemeblock[
(define (int-proj pos neg src-info name)
  (lambda (x)
    (if (integer? x)
        x
        (raise-contract-error
         val
         src-info
         pos
         name
         "expected <integer>, given: ~e"
         val))))
]

The first two new arguments specify who is to be blamed for
positive and negative contract violations,
respectively. Contracts, in this system, are always
established between two parties. One party provides some
value according to the contract, and the other consumes the
value, also according to the contract. The first is called
the ``positive'' person and the second the ``negative''. So,
in the case of just the integer contract, the only thing
that can go wrong is that the value provided is not an
integer. Thus, only the positive argument can ever accrue
blame (and thus only @scheme[pos] is passed
to @scheme[raise-contract-error]).

Compare that to the projection for our function contract:

@schemeblock[
(define (int->int-proj pos neg src-info name)
  (let ([dom (int-proj neg pos src-info name)]
        [rng (int-proj pos neg src-info name)])
    (lambda (f)
      (if (and (procedure? f)
               (procedure-arity-includes? f 1))
          (lambda (x)
            (rng (f (dom x))))
          (raise-contract-error
           val
           src-info
           pos
           name
           "expected a procedure of one argument, given: ~e"
           val)))))
]

In this case, the only explicit blame covers the situation
where either a non-procedure is supplied to the contract, or
where the procedure does not accept one argument. As with
the integer projection, the blame here also lies with the
producer of the value, which is
why @scheme[raise-contract-error] gets @scheme[pos] and
not @scheme[neg] as its argument. 

The checking for the domain and range are delegated to
the @scheme[int-proj] function, which is supplied its
arguments in the first two line of
the @scheme[int->int-proj] function. The trick here is that,
even though the @scheme[int->int-proj] function always
blames what it sees as positive we can reverse the order of
the @scheme[pos] and @scheme[neg] arguments so that the
positive becomes the negative. 

This is not just a cheap trick to get this example to work,
however. The reversal of the positive and the negative is a
natural consequence of the way functions behave. That is,
imagine the flow of values in a program between two
modules. First, one module defines a function, and then that
module is required by another. So, far the function itself
has to go from the original, providing module to the
requiring module. Now, imagine that the providing module
invokes the function, suppying it an argument. At this
point, the flow of values reverses. The argument is
travelling back from the requiring module to the providing
module! And finally, when the function produces a result,
that result flows back in the original
direction. Accordingly, the contract on the domain reverses
the positive and the negative, just like the flow of values
reverses.

We can use this insight to generalize the function contracts
and build a function that accepts any two contracts and
returns a contract for functions between them.

@schemeblock[
(define (make-simple-function-contract dom-proj range-proj)
  (lambda (pos neg src-info name)
    (let ([dom (dom-proj neg pos src-info name)]
          [rng (range-proj pos neg src-info name)])
      (lambda (f)
        (if (and (procedure? f)
                 (procedure-arity-includes? f 1))
            (lambda (x)
              (rng (f (dom x))))
            (raise-contract-error
             val
             src-info
             pos
             name
             "expected a procedure of one argument, given: ~e"
             val))))))
]

Projections like the ones described above, but suited to
other, new kinds of value you might make, can be used with
the contract library primitives below.

@defproc[(make-proj-contract [name any/c]
                             [proj (symbol? symbol? any/c any/c . -> . any/c)]
                             [first-order-test (any/c . -> . any/c)])
         contract?]{

The simplest way to build a contract. It can be less
efficient than using other contract constructors described
below, but it is the right choice for new contract
constructors or first-time contract builders.

The first argument is the name of the contract. It can be an
arbitrary S-expression. The second is a projection (see
above).

The final argument is a predicate that is a
conservative, first-order test of a value. It should be a
function that accepts one argument and returns a boolean. If
it returns @scheme[#f], its argument must be guaranteed to
fail the contract, and the contract should detect this right
when the projection is invoked. If it returns true,
the value may or may not violate the contract, but any
violations must not be signaled immediately. 

From the example above, the predicate should accept unary
functions, but reject all other values.}

@defproc[(build-compound-type-name [c/s any/c] ...) any]{

Produces an S-expression to be used as a name
for a contract. The arguments should be either contracts or
symbols. It wraps parenthesis around its arguments and
extracts the names from any contracts it is supplied with.}

@defproc[(coerce-contract [id symbol?] [x any/c]) contract?]{

If @scheme[x] is a contract, it returns it. If it is a procedure of
arity one, it converts that into a contract by treating the result as
a predicate. If it is a symbol, boolean, or character, it makes a
contract that accepts values that are @scheme[eq?] to @scheme[x]. If
@scheme[x] is a string or a bytes, it makes a contract that
accespts values that are @scheme[equal?] to @scheme[x]. If @scheme[x]
is a regular expression or a byte regular expression, it makes a
contract that accepts strings and bytes, as long as they match the
regular expression.

If @scheme[x] is none of the above, @scheme[coerce-contract]
signals an error, using the first argument in the error
message.}

@defproc[(coerce-contracts [id symbol?] [xs (listof any/c)]) (listof contract?)]{

Coerces all of the arguments in 'xs' into contracts (via
@scheme[coerce-contract/f]) and signals an error if any of them are not
contracts.  The error messages assume that the function named by
@scheme[id] got @scheme[xs] as its entire argument list.
}

@defproc[(coerce-flat-contract [id symbol?] [x any/c]) flat-contract?]{
  Like @scheme[coerce-contract], but requires the result
  to be a flat contract, not an arbitrary contract.
}

@defproc[(coerce-flat-contracts [id symbol?] [x (listof any/c)]) (listof/c flat-contract?)]{
  Like @scheme[coerce-contracts], but requires the results
  to be flat contracts, not arbitrary contracts.
}

@defproc[(coerce-contract/f [x any/c]) (or/c contract? #f)]{
  Like @scheme[coerce-contract], but returns @scheme[#f] if
  the value cannot be coerced to a contract.
}

@defproc[(raise-contract-error [val any/c]
                               [src-info any/c]
                               [to-blame symbol?]
                               [contract-name any/c]
                               [fmt string?]
                               [arg any/c] ...)
         any]{

Signals a contract violation. The first argument is the value that
failed to satisfy the contract. The second argument is is the
@scheme[src-info] passed to the projection and the third should be
either @scheme[pos] or @scheme[neg] (typically @scheme[pos], see the
beginning of this section) that was passed to the projection. The
fourth argument is the @scheme[contract-name] that was passed to the
projection and the remaining arguments are used with @scheme[format]
to build an actual error message.}

@;{
% to document:
%           proj-prop proj-pred? proj-get
%           name-prop name-pred? name-get
%           stronger-prop stronger-pred? stronger-get
%           flat-prop flat-pred? flat-get
%           first-order-prop first-order-get
%           contract-stronger?
}

@; ------------------------------------------------------------------------

@section{Contract Utilities}

@defproc[(guilty-party [exn exn?]) any]{

Extracts the name of the guilty party from an exception
raised by the contract system.}

@defproc[(contract? [v any/c]) boolean?]{

Returns @scheme[#t] if its argument is a contract (ie, constructed
with one of the combinators described in this section), @scheme[#f]
otherwise.}

@defproc[(flat-contract? [v any/c]) boolean?]{

Returns @scheme[#t] when its argument is a contract that has been
constructed with @scheme[flat-contract] (and thus is essentially just
a predicate), @scheme[#f] otherwise.}

@defproc[(flat-contract-predicate [v flat-contract?])
         (any/c . -> . any/c)]{

Extracts the predicate from a flat contract.}


@defproc[(contract-first-order-passes? [contract contract?]
                                       [v any/c])
         boolean?]{

Returns a boolean indicating if the first-order tests
of @scheme[contract] pass for @scheme[v].

If it returns @scheme[#f], the contract is guaranteed not to
hold for that value; if it returns @scheme[#t], the contract
may or may not hold. If the contract is a first-order
contract, a result of @scheme[#t] guarantees that the
contract holds.}


@defproc[(make-none/c [sexp-name any/c]) contract?]{

Makes a contract that accepts no values, and reports the
name @scheme[sexp-name] when signaling a contract violation.}


@defparam[contract-violation->string 
          proc 
          (-> any/c any/c (or/c #f any/c) any/c string? string?)]{

This is a parameter that is used when constructing a
contract violation error. Its value is procedure that
accepts five arguments: 
@itemize{
@item{the value that the contract applies to,}
@item{a syntax object representing the source location where
the contract was established, }
@item{the name of the party that violated the contract (@scheme[#f] indicates that the party is not known, not that the party's name is @scheme[#f]), }
@item{an sexpression representing the contract, and }
@item{a message indicating the kind of violation.
}}
The procedure then
returns a string that is put into the contract error
message. Note that the value is often already included in
the message that indicates the violation.

If the contract was establised via
@scheme[provide/contract], the names of the party to the
contract will be sexpression versions of the module paths
(as returned by @scheme[collapse-module-path]).

}


@defform[(recursive-contract contract-expr)]{

Delays the evaluation of its argument until the contract is checked,
making recursive contracts possible.}


@defform[(opt/c contract-expr)]{

This optimizes its argument contract expression by
traversing its syntax and, for known contract combinators,
fuses them into a single contract combinator that avoids as
much allocation overhad as possible. The result is a
contract that should behave identically to its argument,
except faster (due to the less allocation).}


@defform[(define-opt/c (id id ...) expr)]{

This defines a recursive contract and simultaneously
optimizes it. Semantically, it behaves just as if
the @scheme[-opt/c] were not present, defining a function on
contracts (except that the body expression must return a
contract). But, it also optimizes that contract definition,
avoiding extra allocation, much like @scheme[opt/c] does.

For example, 

@schemeblock[
(define-contract-struct bt (val left right))

(define-opt/c (bst-between/c lo hi)
  (or/c null?
        (bt/c [val (real-in lo hi)]
              [left (val) (bst-between/c lo val)]
              [right (val) (bst-between/c val hi)])))

(define bst/c (bst-between/c -inf.0 +inf.0))
]

defines the @scheme[bst/c] contract that checks the binary
search tree invariant. Removing the @scheme[-opt/c] also
makes a binary search tree contract, but one that is
(approximately) 20 times slower.}

