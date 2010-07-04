#lang scribble/doc
@(require scribble/manual
          "common.ss"
          (for-label scheme/base))

@title[#:tag "decompile"]{Decompiling Bytecode}

The @DFlag{decompile} mode for @|mzc| takes a bytecode file (which
 usually has the file extension @filepath{.zo}) and converts it back
 to an approximation of Scheme code. Decompiled bytecode is mostly
 useful for checking the compiler's transformation and optimization of
 the source program.

Many forms in the decompiled code, such as @scheme[module],
 @scheme[define], and @scheme[lambda], have the same meanings as
 always. Other forms and transformations are specific to the rendering
 of bytecode, and they reflect a specific execution model:

@itemize[

 @item{Top-level variables, variables defined within the module, and
 variables imported from other modules are prefixed with @litchar{_},
 which helps expose the difference between uses of local variables
 versus other variables. Variables imported from other modules,
 moreover, have a suffix that indicates the source module.

 Non-local variables are always accessed indirectly though an implicit
 @schemeidfont{#%globals} or @schemeidfont{#%modvars} variable that
 resides on the value stack (which otherwise contains local
 variables). Variable accesses are further wrapped with
 @schemeidfont{#%checked} when the compiler cannot prove that the
 variable will be defined before the access.

 Uses of core primitives are shown without a leading @litchar{_}, and
 they are never wrapped with @schemeidfont{#%checked}.}

 @item{Local-variable access may be wrapped with
 @schemeidfont{#%sfs-clear}, which indicates that the variable-stack
 location holding the variable will be cleared to prevent the
 variable's value from being retained by the garbage collector.

 Mutable variables are converted to explicitly boxed values using
 @schemeidfont{#%box}, @schemeidfont{#%unbox}, and
 @schemeidfont{#%set-boxes!} (which works on multiple boxes at once).
 A @schemeidfont{set!-rec-values} operation constructs
 mutually-recursive closures and simultaneously updates the
 corresponding variable-stack locations that bind the closures.  A
 @schemeidfont{set!}, @schemeidfont{set!-values}, or
 @schemeidfont{set!-rec-values} form is always used on a local
 variable before it is captured by a closure; that ordering reflects
 how closures capture values in variable-stack locations, as opposed
 to stack locations.}

 @item{In a @scheme[lambda] form, if the procedure produced by the
 @scheme[lambda] has a name (accessible via @scheme[object-name])
 and/or source-location information, then it is shown as a quoted
 constant at the start of the procedure's body. Afterward, if the
 @scheme[lambda] form captures any bindings from its context, those
 bindings are also shown in a quoted constant. Neither constant
 corresponds to a computation when the closure is called, though the
 list of captured bindings corresponds to a closure allocation when
 the @scheme[lambda] form itself is evaluated.

 A @scheme[lambda] form that closes over no bindings is wrapped with
 @schemeidfont{#%closed} plus an identifier that is bound to the
 closure. The binding's scope covers the entire decompiled output, and
 it may be referenced directly in other parts of the program; the
 binding corresponds to a constant closure value that is shared, and
 it may even contain cyclic references to itself or other constant
 closures.}

 @item{Some applications of core primitives are annotated with
 @schemeidfont{#%in}, which indicates that the JIT compiler will
 inline the operation. (Inlining information is not part of the
 bytecode, but is instead based on an enumeration of primitives that
 the JIT is known to handle specially.)}

 @item{A form @scheme[(#%apply-values _proc _expr)] is equivalent to
 @scheme[(call-with-values (lambda () _expr) _proc)], but the run-time
 system avoids allocating a closure for @scheme[_expr].}

 @item{A @schemeidfont{#%decode-syntax} form corresponds to a syntax
 object. Future improvements to the decompiler will convert such
 syntax objects to a readable form.}

]
