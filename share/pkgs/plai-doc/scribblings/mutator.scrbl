#lang scribble/doc
@(require scribble/manual
          "rkt-exports.rkt"
          "plai-exports.rkt"
          "lang-names.rkt"
          (for-syntax scheme)
          (for-label (only-in racket/base
                              list modulo
                              procedure? path-string? 
                              input-port? string? void?
                              exact-nonnegative-integer?
                              exact-positive-integer?)
                     (only-in racket/contract/base
                              or/c listof any/c)
                     (only-in plai/collector
                              root?
                              heap-size
                              location?
                              heap-value?
                              heap-set! heap-ref with-heap
                              get-root-set read-root set-root!
                              procedure-roots)
                     plai/scribblings/fake-collector
                     plai/scribblings/fake-mutator
                     plai/scribblings/fake-web
                     plai/random-mutator
                     (only-in plai/web
                              no-web-browser
                              static-files-path)
                     plai/mutator))

@title[#:tag "mutator"]{@MUTATE-LANG}

@defmodulelang[plai/mutator]

The @MUTATE-LANG language is used to test garbage collectors written with the
@secref["collector"] language.  Since collectors support a subset of Scheme's
values, the @MUTATE-LANG language supports a subset of procedures and syntax.
In addition, many procedures that can be written in the mutator are omitted as
they make good test cases.  Therefore, the mutator language provides only
primitive procedures, such as @racket[+], @racket[cons], etc.

@section{Building Mutators}

@declare-exporting[#:use-sources (plai/scribblings/fake-mutator)]

The first expression of a mutator must be:

@defform/subs[
(allocator-setup collector-module 
                 heap-size)
([heap-size exact-nonnegative-integer])]{

The @racket[_collector-module] form specifies the path to the garbage collector that the
mutator should use.  The collector must be written in the @COLLECT-LANG
language.
}

The rest of a mutator module is a sequence of definitions, expressions and test
cases. The @MUTATE-LANG language transforms these definitions and statements to
use the collector specified in @racket[allocator-setup].  In particular, many
of the primitive forms, such as @racket[cons] map directly to procedures such
as @racket[gc:cons], written in the collector.

@section{Mutator API}

The @MUTATE-LANG language supports the following procedures and syntactic
forms:

@(define-syntax-rule (defprocthing id content ...)
   @defthing[id procedure? content ...]) 

@(define-syntax (document/lift stx)
   (syntax-case stx ()
     [(_ defidform a ...)
      (with-syntax ([(doc ...)
                     (map (λ (a)
                            (with-syntax ([a a]
                                          [rkt:a (string->symbol (format "rkt:~a" (syntax-e a)))]) 
                              #'@defidform[a]{Just like Racket's @|rkt:a|.}))
                          (syntax->list #'(a ...)))])

      #'(begin
          doc ...))]))

@document/lift[defidform
               if and or cond case define-values let let-values let* set! quote begin]

@defform[(define (id arg-id ...) body-expression ...+)]{
  Just like Racket's @racket[define], except restricted to the simpler form
  above.
}
@deftogether[(@defform[(lambda (arg-id ...) body-expression ...+)]{}
              @defform[(λ (arg-id ...) body-expression ...+)]{})]{
  Just like Racket's @racket[lambda] and @racket[λ], except restricted to the
  simpler form above.
}

@document/lift[defprocthing
               error
               add1 sub1 zero? + - * / even? odd? = < > <= >= 
                    symbol? symbol=? number? boolean? empty? eq?]

@defproc[(cons [hd any/c] [tl any/c]) cons?]{
  Constructs a (mutable) pair.
}
@defproc[(cons? [v any/c]) boolean?]{
  Returns @racket[#t] when given a value created by @racket[cons],
          @racket[#f] otherwise.
}
@defproc[(first [c cons?]) any/c]{
  Extracts the first component of @racket[c].
}
@defproc[(rest [c cons?]) any/c]{
  Extracts the rest component of @racket[c].
}

@defproc[(set-first! [c cons?] [v any/c])
         void?]{
  Sets the @racket[first] of the cons cell @racket[c].
}

@defproc[(set-rest! [c cons?] [v any/c])
         void?]{
  Sets the @racket[rest] of the cons cell @racket[c].
}

@defidform[empty]{
  The identifier @racket[empty] is defined to invoke
  @racket[(gc:alloc-flat '())] wherever it is used.
}

@defprocthing[print-only-errors]{
  Behaves like PLAI's @|plai:print-only-errors|.
}

@defprocthing[halt-on-errors]{
  Behaves like PLAI's @|plai:halt-on-errors|.
}

Other common procedures are left undefined as they can be defined in
terms of the primitives and may be used to test collectors.

Additional procedures from @racketmodname[scheme] may be imported with:

@defform/subs[(import-primitives id ...)()]{

Imports the procedures @racket[_id ...] from @racketmodname[scheme].  Each
procedure is transformed to correctly interface with the mutator.  That is, its
arguments are dereferenced from the mutator's heap and the result is allocated
on the mutator's heap.  The arguments and result must be @racket[heap-value?]s,
even if the imported procedure accepts or produces structured data.

For example, the @MUTATE-LANG language does not define @racket[modulo]:

@racketblock[

(import-primitives modulo)

(test/value=? (modulo 5 3) 2)
]

}

@section{Testing Mutators}

@MUTATE-LANG provides two forms for testing mutators:

@defform/subs[(test/location=? mutator-expr1 mutator-expr2)()]{

@racket[test/location=?] succeeds if @racket[_mutator-expr1] and
@racket[_mutator-expr2] reference the same location on the heap.

}

@defform/subs[(test/value=? mutator-expr scheme-datum/quoted)()]{

@racket[test/value=?] succeeds if @racket[_mutator-expr] and
@racket[_scheme-datum/expr] are structurally equal.
@racket[_scheme-datum/quoted] is not allocated on the mutator's
heap. Futhermore, it must either be a quoted value or a literal value.

}

@defform/subs[
(printf format mutator-expr ...)
([format literal-string])]{

In @|MUTATE-LANG|, @racket[printf] is a syntactic form and not a procedure. The
format string, @racket[_format] is not allocated on the mutator's heap.

}
