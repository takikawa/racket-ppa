#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@(interaction-eval (require (lib "mzlib/for.ss")))

@title[#:tag "set!"]{Assignment: @scheme[set!]}

@refalso["set!"]{@scheme[set!]}

Assign to a variable using @scheme[set!]:

@specform[(set! id expr)]

A @scheme[set!] expression evaluates @scheme[_expr] and changes
@scheme[_id] (which must be bound in the enclosing environment) to the
resulting value. The result of the @scheme[set!]  expression itself is
@|void-const|.

@defexamples[
(define greeted null)
(define (greet name)
  (set! greeted (cons name greeted))
  (string-append "Hello, " name))

(greet "Athos")
(greet "Porthos")
(greet "Aramis")
greeted
]

@defs+int[
[(define (make-running-total)
   (let ([n 0])
     (lambda ()
       (set! n (+ n 1))
       n)))
 (define win (make-running-total))
 (define lose (make-running-total))]
(win)
(win)
(lose)
(win)
]

@;------------------------------------------------------------------------
@section[#:tag "using-set!"]{Guidelines for Using Assignment}

Although using @scheme[set!] is sometimes appropriate, Scheme style
generally discourages the use of @scheme[set!]. The following
guidelines may help explain when using @scheme[set!] is appropriate.

@itemize[

 @item{As in any modern language, assigning to shared identifier is no
       substitute for passing an argument to a procedure or getting
       its result.

       @as-examples[@t{@bold{@italic{Really awful}} example:}
       @defs+int[
       [(define name "unknown")
        (define result "unknown")
        (define (greet)
          (set! result (string-append "Hello, " name)))]
        (set! name "John")
        (greet)
        result
       ]]

      @as-examples[@t{Ok example:}
      @def+int[
        (define (greet name)
          (string-append "Hello, " name))
        (greet "John")
        (greet "Anna")
      ]]}

@;-- FIXME: explain more _why_ it's inferior
 @item{A sequence of assignments to a local variable is far inferior
       to nested bindings.

       @as-examples[@t{@bold{Bad} example:}
       @interaction[
       (let ([tree 0])
         (set! tree (list tree 1 tree))
         (set! tree (list tree 2 tree))
         (set! tree (list tree 3 tree))
         tree)]]

       @as-examples[@t{Ok example:}
       @interaction[
       (let* ([tree 0]
              [tree (list tree 1 tree)]
              [tree (list tree 2 tree)]
              [tree (list tree 3 tree)])
         tree)]]}

 @item{Using assignment to accumulate results from an iteration is
       bad style. Accumulating through a loop argument is better.

       @as-examples[@t{Somewhat bad example:}
       @def+int[
       (define (sum lst)
         (let ([s 0])
           (for-each (lambda (i) (set! s (+ i s)))
                     lst)
           s))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{Ok example:}
       @def+int[
       (define (sum lst)
         (let loop ([lst lst][s 0])
           (if (null? lst)
               s
               (loop (cdr lst) (+ s (car lst))))))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{Better (use an existing function) example:}
       @def+int[
       (define (sum lst)
         (apply + lst))
       (sum '(1 2 3))
       ]]

       @as-examples[@t{Good (a general approach) example:}
       @def+int[
       (define (sum lst)
         (for/fold ([s 0])
                   ([i (in-list lst)])
           (+ s i)))
       (sum '(1 2 3))
       ]]  }

 @item{For cases where stateful objects are necessary or appropriate,
       then implementing the object's state with @scheme[set!] is
       fine.

       @as-examples[@t{Ok example:}
       @def+int[
       (define next-number!
         (let ([n 0])
           (lambda ()
             (set! n (add1 n))
             n)))
       (next-number!)
       (next-number!)
       (next-number!)]]}

]

All else being equal, a program that uses no assignments or mutation
is always preferable to one that uses assignments or mutation. While
side effects are to be avoided, however, they should be used if the
resulting code is significantly more readable or if it implements a
significantly better algorithm.

The use of mutable values, such as vectors and hash tables, raises
fewer suspicions about the style of a program than using @scheme[set!]
directly. Nevertheless, simply replacing @scheme[set!]s in a program
with a @scheme[vector-set!]s obviously does not improve the style of
the program.

@;------------------------------------------------------------------------
@section{Multiple Values: @scheme[set!-values]}

@refalso["set!"]{@scheme[set!-values]}

The @scheme[set!-values] form assigns to multiple variables at once,
given an expression that produces an appropriate number of values:

@specform[(set!-values (id ...) expr)]

This form is equivalent to using @scheme[let-values] to receive
multiple results from @scheme[_expr], and then assigning the results
individually to the @scheme[_id]s using @scheme[set!].

@defexamples[
(define game
  (let ([w 0]
        [l 0])
    (lambda (win?)
      (if win?
          (set! w (+ w 1))
          (set! l (+ l 1)))
      (begin0
        (values w l)
        (code:comment #, @t{swap sides...})
        (set!-values (w l) (values l w))))))
(game #t)
(game #t)
(game #f)]
