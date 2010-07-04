#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          (for-label scheme/base
                     macro-debugger/expand
                     macro-debugger/stepper
                     macro-debugger/stepper-text
                     macro-debugger/syntax-browser
                     (rename-in scheme (free-identifier=? module-identifier=?))))

@(define the-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require macro-debugger/expand
                         macro-debugger/stepper-text))
     the-eval))

@title{@bold{Macro Debugger}}

@author["Ryan Culpepper"]

The macro-debugger collection contains two tools: a stepper for macro
expansion and a standalone syntax browser. The macro stepper shows the
programmer the expansion of a program as a sequence of rewriting
steps, using the syntax browser to display the individual terms. The
syntax browser uses colors and a properties panel to show the term's
syntax properties, such as lexical binding information and source
location.

@section{Macro stepper}

@defmodule[macro-debugger/stepper]

@defproc[(expand/step [stx any/c])
         (is-a/c macro-stepper<%>)]{

  Expands the syntax (or S-expression) and opens a macro stepper frame
  for stepping through the expansion. 
}

@definterface[macro-stepper<%> ()]{

@defmethod[(at-start?) boolean?]
@defmethod[(at-end?) boolean?]
@defmethod[(navigate-to-start) void?]
@defmethod[(navigate-to-end) void?]
@defmethod[(navigate-previous) void?]
@defmethod[(navigate-next) void?]
@defmethod[(at-top?) boolean?]
@defmethod[(at-bottom?) boolean?]
@defmethod[(navigate-up) void?]
@defmethod[(navigate-down) void?]
}

@section{Macro expansion tools}

@defmodule[macro-debugger/expand]

This module provides @scheme[expand]-like procedures that allow the
user to specify macros whose expansions should be hidden.

Warning: because of limitations in the way macro expansion is
selectively hidden, the resulting syntax may not evaluate to the same
thing as the original syntax.

@defproc[(expand-only [stx any/c] [transparent-macros (listof identifier?)])
         syntax?]{

  Expands the given syntax @scheme[stx], but only shows the expansion
  of macros whose names occur in @scheme[transparent-macros].

  @(examples #:eval the-eval
             (syntax->datum
              (expand-only #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                           (list #'or))))

}

@defproc[(expand/hide [stx any/c] [hidden-macros (listof identifier?)])
         syntax?]{

  Expands the given syntax @scheme[stx], but hides the expansion of macros in the
  given identifier list (conceptually, the complement of expand-only).

  @(examples #:eval the-eval
             (syntax->datum
              (expand/hide #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                           (list #'or))))
}

@defproc[(expand/show-predicate [stx any/c] [show? (-> identifier? boolean?)])
         syntax?]{

  Expands the given syntax @scheme[stx], but only shows the expansion of macros
  whose names satisfy the predicate @scheme[show?].

  @(examples #:eval the-eval
             (syntax->datum
              (expand/show-predicate
               #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
               (lambda (id) (memq (syntax-e id) '(or #%app))))))
}

@section{Macro stepper text interface}

@defmodule[macro-debugger/stepper-text]

@defproc[(expand/step-text [stx any/c]
                           [show? (or/c (-> identifier? boolean?)
                                        (listof identifier?))
                                  (lambda (x) #t)])
         void?]{

  Expands the syntax and prints the macro expansion steps. If the
  identifier predicate is given, it determines which macros are shown
  (if absent, all macros are shown). A list of identifiers is also
  accepted.

  @(examples #:eval the-eval
             (expand/step-text #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                               (list #'or))
             #;(expand/step-text #'(let ([x 1]) (even? x)))
             #;(expand/step-text #'(let ([x 1] [y 2]) (or (even? x) (even? y)))
                               (lambda (id) (eq? (syntax-e id) 'or))))
}

@defproc[(stepper-text [stx any/c]
                       [show? (or/c (-> identifier? boolean?)
                                    (listof identifier?))
                              (lambda (x) #t)])
         (symbol? -> void?)]{

  Returns a procedure that can be called on the symbol
  @scheme['next] to print the next step or on the symbol
  @scheme['all] to print out all remaining steps.
}

@section{Syntax browser}

@defmodule[macro-debugger/syntax-browser]

@defproc[(browse-syntax [stx syntax?])
         void?]{

  Creates a frame with the given syntax object shown. More information
  on using the GUI is available below.
}

@defproc[(browse-syntaxes [stxs (listof syntax?)])
         void?]{

  Like @scheme[browse-syntax], but shows multiple syntax objects in
  the same frame. The coloring partitions are shared between the two,
  showing the relationships between subterms in different syntax
  objects.
}

@;{
@defproc[(syntax-snip [stx syntax?])
         (is-a/c snip%)]{

  Like @scheme[browse-syntax], but creates a snip that can be
  displayed in an editor.
}
}

@section{Using the macro stepper}

@subsection{Navigation}

The stepper presents expansion as a linear sequence of rewriting
process, and it gives the user controls to step forward or backwards
as well as to jump to the beginning or end of the expansion process.

If the macro stepper is showing multiple expansions, then it also
provides ``Previous term'' and ``Next term'' buttons to go up and down in
the list of expansions. Horizontal lines delimit the current expansion
from the others.

@subsection{Macro hiding}

Macro hiding lets one see how expansion would look if certain macros
were actually primitive syntactic forms. The macro stepper skips over
the expansion of the macros you designate as opaque, but it still
shows the expansion of their subterms.

The bottom panel of the macro stepper controls the macro hiding
policy. The user changes the policy by selecting an identifier in the
syntax browser pane and then clicking one of ``Hide module'', ``Hide
macro'', or ``Show macro''. The new rule appears in the policy display,
and the user may later remove it using the "Delete" button.

The stepper also offers coarser-grained options that can hide
collections of modules at once. These options have lower precedence
than the rules above.

Macro hiding, even with no macros marked opaque, also hides certain
other kinds of steps: internal defines are not rewritten to letrecs,
begin forms are not spliced into module or block bodies, etc.

@section{Using the syntax browser}

@subsection{Selection}

The selection is indicated by bold text.

The user can click on any part of a subterm to select it. To select a
parenthesized subterm, click on either of the parentheses. The
selected syntax is bolded. Since one syntax object may occur inside of
multiple other syntax objects, clicking on one occurrence will cause
all occurrences to be bolded.

The syntax browser displays information about the selected syntax
object in the properties panel on the right, when that panel is
shown. The selected syntax also determines the highlighting done by
the secondary partitioning (see below).

@subsection{Primary partition}

The primary partition is indicated by foreground color.

The primary partitioning always assigns two syntax subterms the same
color if they have the same marks. In the absence of unhygienic
macros, this means that subterms with the same foreground color were
either present in the original pre-expansion syntax or generated by
the same macro transformation step.

Syntax colored in black always corresponds to unmarked syntax. Such
syntax may be original, or it may be produced by the expansion of a
nonhygienic macro.

Note: even terms that have the same marks might not be
@scheme[bound-identifier=?] to each other, because they might occur in
different environments.

@;@example[(bound-identifier=? (let ([x 1]) #'x) #'x)]

@subsection{Secondary partitioning}

The user may select a secondary partitioning through the Syntax
menu. This partitioning applies only to identifiers. When the user
selects an identifier, all terms in the same equivalence class as the
selected term are highlighted in yellow.

The available secondary partitionings are:
@itemize[
@item{@scheme[bound-identifier=?]}
@item{@scheme[module-identifier=?]}
@item{@scheme[module-or-top-identifier=?]}
@item{@bold{symbolic-identifier=?}: 
  Two identifiers are symbolic-identifier=? if discarding all lexical
  context information yields the same symbol.
}
@item{@bold{same marks}: 
  Two identifiers have the same marks if (barring nonhygienic macros)
  they were produced by the same macro transformation step.
}
@item{@bold{same source module}:
  The bindings of the two identifiers come from definitions in the
  same module.
}
@item{@bold{same nominal module}:
  The bindings of the two identifiers were imported into the current
  context by requiring the same module.
}
]

@subsection{Properties}

When the properties pane is shown, it displays properties of the
selected syntax object. The properties pane has two tabbed pages:

@itemize[
@item{@bold{Term}:

      If the selection is an identifier, shows the binding information
      associated with the syntax object. For more information, see
      @scheme[identifier-binding], etc.
}
@item{@bold{Syntax Object}:

      Displays source location information and other properties (see
      @scheme[syntax-property]) carried by the syntax object.
}
]

@subsection{Interpreting syntax}

The binding information of a syntax object may not be the same as
the binding structure of the program it represents. The binding
structure of a program is only determined after macro expansion is
complete.
