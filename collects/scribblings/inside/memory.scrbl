#lang scribble/doc
@(require "utils.ss")

@title[#:tag "im:memoryalloc"]{Memory Allocation}

@section-index{memory}
@section-index{garbage collection}

PLT Scheme uses both @cppi{malloc} and allocation functions provided
by a garbage collector. Embedding/extension C/C++ code may use either
allocation method, keeping in mind that pointers to
garbage-collectable blocks in @cpp{malloc}ed memory are invisible
(i.e., such pointers will not prevent the block from being
garbage-collected).

PLT Scheme CGC uses a conservative garbage collector.  This garbage
collector normally only recognizes pointers to the beginning of
allocated objects. Thus, a pointer into the middle of a GC-allocated
string will normally not keep the string from being collected. The
exception to this rule is that pointers saved on the stack or in
registers may point to the middle of a collectable object.  Thus, it
is safe to loop over an array by incrementing a local pointer
variable.

PLT Scheme 3m uses a precise garbage collector that moves objects
during collection, in which case the C code must be instrumented to
expose local pointer bindings to the collector, and to provide tracing
procedures for (tagged) records containing pointers. This
instrumentation is described further in @secref["im:3m"].

The basic collector allocation functions are:

@itemize{

 @item{@cppi{scheme_malloc} --- Allocates collectable memory that may
 contain pointers to collectable objects; for 3m, the memory must be
 an array of pointers (though not necessarily to collectable
 objects). The newly allocated memory is initially zeroed.}

 @item{@cppi{scheme_malloc_atomic} --- Allocates collectable memory
 that does not contain pointers to collectable objects. If the memory
 does contain pointers, they are invisible to the collector and will
 not prevent an object from being collected. Newly allocated atomic
 memory is not necessary zeroed.

 Atomic memory is used for strings or other blocks of memory which do
 not contain pointers. Atomic memory can also be used to store
 intentionally-hidden pointers.}

 @item{@cppi{scheme_malloc_tagged} --- Allocates collectable memory
 that contains a mixture of pointers and atomic data. With the
 conservative collector, this function is the same
 as @cppi{scheme_malloc}, but under 3m, the type tag stored at the
 start of the block is used to determine the size and shape of the
 object for future garbage collection (as described
 in @secref["im:3m"]).}

 @item{@cppi{scheme_malloc_allow_interior} --- Allocates a large
 array of pointers such that references are allowed into the middle of
 the block under 3m, and such pointers prevent the block from being
 collected. This procedure is the same as @cppi{scheme_malloc} with
 the conservative collector, but in the that case, having @italic{only}
 a pointer into the interior will not prevent the array from being
 collected.}

 @item{@cppi{scheme_malloc_atomic_allow_interior} --- Like
 @cpp{scheme_malloc_allow_interior} for memory that does not
 contain pointers.}

 @item{@cppi{scheme_malloc_uncollectable} --- Allocates
 uncollectable memory that may contain pointers to collectable
 objects. There is no way to free the memory. The newly allocated
 memory is initially zeroed. This function is not available in 3m.}

}

@index['("globals" "in extension code")]{If} a PLT Scheme extension
stores Scheme pointers in a global or static variable, then that
variable must be registered with
@cppi{scheme_register_extension_global}; this makes the pointer
visible to the garbage collector. Registered variables need not
contain a collectable pointer at all times (even with 3m, but the
variable must contain some pointer, possibly uncollectable, at all
times).

With conservative collection, no registration is needed for the global
or static variables of an embedding program, unless it calls
@cpp{scheme_main_setup} or @cppi{scheme_set_stack_base} with a non-zero
first or second (respectively) argument. In that case, global and
static variables containing collectable pointers must be registered
with @cppi{scheme_register_static}. The @cppi{MZ_REGISTER_STATIC}
macro takes any variable name and registers it with
@cppi{scheme_register_static}. The @cppi{scheme_register_static}
function can be safely called even when it's not needed, but it must
not be called multiple times for a single memory address.

Collectable memory can be temporarily locked from collection by using
the reference-counting function @cppi{scheme_dont_gc_ptr}. Under 3m,
such locking does not prevent the object from being moved.

Garbage collection can occur during any call into Scheme or its
allocator, on anytime that Scheme has control, except during functions
that are documented otherwise.  The predicate and accessor macros
listed in @secref["im:stdtypes"] never trigger a collection.

@; ----------------------------------------------------------------------

@section[#:tag "im:3m"]{Cooperating with 3m}

To allow 3m's precise collector to detect and update pointers during
garbage collection, all pointer values must be registered with the
collector, at least during the times that a collection may occur.  The
content of a word registered as a pointer must contain either
@cpp{NULL}, a pointer to the start of a collectable object, a pointer
into an object allocated by @cpp{scheme_malloc_allow_interior}, a
pointer to an object currently allocated by another memory manager
(and therefore not into a block that is currently managed by the
collector), or a pointer to an odd-numbered address (e.g., a Scheme
fixnum).

Pointers are registered in three different ways:

@itemize{

 @item{Pointers in static variables should be registered with
 @cppi{scheme_register_static} or @cpp{MZ_REGISTER_STATIC}.}

 @item{Pointers in allocated memory are registered automatically when
 they are in an array allocated with @cpp{scheme_malloc}, etc.  When a
 pointer resides in an object allocated with
 @cpp{scheme_malloc_tagged}, etc.~the tag at the start of the object
 identifiers the object's size and shape. Handling of tags is
 described in @secref["im:3m:tagged"].}

 @item{Local pointers (i.e., pointers on the stack or in registers)
 must be registered through the @cpp{MZ_GC_DECL_REG}, @|etc| macros
 that are described in @secref["im:3m:stack"].}

}

A pointer must never refer to the interior of an allocated object
(when a garbage collection is possible), unless the object was
allocated with @cppi{scheme_malloc_allow_interior}. For this reason,
pointer arithmetic must usually be avoided, unless the variable
holding the generated pointer is @cpp{NULL}ed before a collection.

@bold{IMPORTANT:} The @cppi{SCHEME_SYM_VAL},
@cppi{SCHEME_KEYWORD_VAL}, @cppi{SCHEME_VEC_ELS}, and
@cppi{SCHEME_PRIM_CLOSURE_ELS} macros produce pointers into the middle
of their respective objects, so the results of these macros must not
be held during the time that a collection can occur. Incorrectly
retaining such a pointer can lead to a crash.

@; - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - - 

@subsection[#:tag "im:3m:tagged"]{Tagged Objects}

As explained in @secref["im:values+types"], the @cpp{scheme_make_type}
function can be used to obtain a new tag for a new type of object.
These new types are in relatively short supply for 3m; the maximum tag
is 255, and Scheme itself uses nearly 200.

After allocating a new tag in 3m (and before creating instances of the
tag), a @defterm{size procedure}, a @defterm{mark procedure}, and a
@defterm{fixup procedure} must be installed for the tag using
@cppi{GC_register_traversers}.

A size procedure simply takes a pointer to an object with the tag and
returns its size in words (not bytes). The @cppi{gcBYTES_TO_WORDS}
macro converts a byte count to a word count.

A mark procedure is used to trace references among objects without
moving any objects. The procedure takes a pointer to an object, and it
should apply the @cppi{gcMARK} macro to every pointer within the
object.  The mark procedure should return the same result as the size
procedure.

A fixup procedure is used to update references to objects after or
while they are moved. The procedure takes a pointer to an object, and
it should apply the @cppi{gcFIXUP} macro to every pointer within the
object; the expansion of this macro takes the address of its
argument. The fixup procedure should return the same result as the
size procedure.

Depending on the collector's implementation, the mark or fixup
procedure might not be used. For example, the collector may only use
the mark procedure and not actually move the object. Or it may use the
fixup procedure to mark and move objects at the same time. To
dereference an object pointer during a fixup procedure, use
@cppi{GC_fixup_self} to convert the address passed to the procedure to
refer to the potentially moved object, and use @cppi{GC_resolve} to
convert an address that is not yet fixed up to determine the object's
current location.

When allocating a tagged object in 3m, the tag must be installed
immediately after the object is allocated---or, at least, before the
next possible collection.

@; - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - - 

@subsection[#:tag "im:3m:stack"]{Local Pointers}

The 3m collector needs to know the address of every local or temporary
pointer within a function call at any point when a collection can be
triggered. Beware that nested function calls can hide temporary
pointers; for example, in

@verbatim[#:indent 2]{
  scheme_make_pair(scheme_make_pair(scheme_true, scheme_false),
                   scheme_make_pair(scheme_false, scheme_true))
}

the result from one @cpp{scheme_make_pair} call is on the stack or in
a register during the other call to @cpp{scheme_make_pair}; this
pointer must be exposed to the garbage collection and made subject to
update. Simply changing the code to

@verbatim[#:indent 2]{
  tmp = scheme_make_pair(scheme_true, scheme_false);
  scheme_make_pair(tmp,
                   scheme_make_pair(scheme_false, scheme_true))
}

does not expose all pointers, since @cpp{tmp} must be evaluated before
the second call to @cpp{scheme_make_pair}. In general, the above code
must be converted to the form

@verbatim[#:indent 2]{
  tmp1 = scheme_make_pair(scheme_true, scheme_false);
  tmp2 = scheme_make_pair(scheme_true, scheme_false);
  scheme_make_pair(tmp1, tmp2);
}

and this is converted form must be instrumented to register @cpp{tmp1}
and @cpp{tmp2}. The final result might be

@verbatim[#:indent 2]{
  {
    Scheme_Object *tmp1 = NULL, *tmp2 = NULL, *result;
    MZ_GC_DECL_REG(2);

    MZ_GC_VAR_IN_REG(0, tmp1);
    MZ_GC_VAR_IN_REG(1, tmp2);
    MZ_GC_REG();

    tmp1 = scheme_make_pair(scheme_true, scheme_false);
    tmp2 = scheme_make_pair(scheme_true, scheme_false);
    result = scheme_make_pair(tmp1, tmp2);

    MZ_GC_UNREG();

    return result;
  }
}

Notice that @cpp{result} is not registered above. The
@cpp{MZ_GC_UNREG} macro cannot trigger a garbage collection, so the
@cpp{result} variable is never live during a potential
collection. Note also that @cpp{tmp1} and @cpp{tmp2} are initialized
with @cpp{NULL}, so that they always contain a pointer whenever a
collection is possible.

The @cppi{MZ_GC_DECL_REG} macro expands to a local-variable
declaration to hold information for the garbage collector. The
argument is the number of slots to provide for
registration. Registering a simple pointer requires a single slot,
whereas registering an array of pointers requires three slots. For
example, to register a pointer @cpp{tmp} and an array of 10
@cpp{char*}s:

@verbatim[#:indent 2]{
  {
    Scheme_Object *tmp1 = NULL;
    char *a[10];
    int i;
    MZ_GC_DECL_REG(4);

    MZ_GC_ARRAY_VAR_IN_REG(0, a, 10);
    MZ_GC_VAR_IN_REG(3, tmp1);
    /* Clear a before a potential GC: */
    for (i = 0; i < 10; i++) a[i] = NULL;
    ...
    f(a);
    ...
  }
}

The @cppi{MZ_GC_ARRAY_VAR_IN_REG} macro registers a local array given
a starting slot, the array variable, and an array size. The
@cppi{MZ_GC_VAR_IN_REG} takes a slot and simple pointer variable. A
local variable or array must not be registered multiple times.

In the above example, the first argument to @cppi{MZ_GC_VAR_IN_REG} is
@cpp{3} because the information for @cpp{a} uses the first three
slots. Even if @cpp{a} is not used after the call to @cpp{f}, @cpp{a}
must be registered with the collector during the entire call to
@cpp{f}, because @cpp{f} presumably uses @cpp{a} until it returns.

The name used for a variable need not be immediate. Structure members
can be supplied as well:

@verbatim[#:indent 2]{
  {
    struct { void *s; int v; void *t; } x = {NULL, 0, NULL};
    MZ_GC_DECL_REG(2);

    MZ_GC_VAR_IN_REG(0, x.s);
    MZ_GC_VAR_IN_REG(0, x.t);
    ...
  }
}

In general, the only constraint on the second argument to
@cppi{MZ_GC_VAR_IN_REG} or @cppi{MZ_GC_ARRAY_VAR_IN_REG} is that
@cpp{&} must produce the relevant address, and that address must be on
the stack.

Pointer information is not actually registered with the collector
until the @cppi{MZ_GC_REG} macro is used. The @cppi{MZ_GC_UNREG} macro
de-registers the information. Each call to @cpp{MZ_GC_REG} must be
balanced by one call to @cpp{MZ_GC_UNREG}.

Pointer information need not be initialized with
@cppi{MZ_GC_VAR_IN_REG} and @cppi{MZ_GC_ARRAY_VAR_IN_REG} before
calling @cpp{MZ_GC_REG}, and the set of registered pointers can change
at any time---as long as all relevent pointers are registered when a
collection might occur. The following example recycles slots and
completely de-registers information when no pointers are relevant. The
example also illustrates how @cpp{MZ_GC_UNREG} is not needed when
control escapes from the function, such as when
@cpp{scheme_signal_error} escapes.

@verbatim[#:indent 2]{
  {
    Scheme_Object *tmp1 = NULL, *tmp2 = NULL;
    mzchar *a, *b;
    MZ_GC_DECL_REG(2);

    MZ_GC_VAR_IN_REG(0, tmp1);
    MZ_GC_VAR_IN_REG(1, tmp2);
    
    tmp1 = scheme_make_utf8_string("foo");
    MZ_GC_REG();
    tmp2 = scheme_make_utf8_string("bar");
    tmp1 = scheme_append_char_string(tmp1, tmp2);

    if (SCHEME_FALSEP(tmp1))
      scheme_signal_error("shouldn't happen!");

    a = SCHEME_CHAR_VAL(tmp1);

    MZ_GC_VAR_IN_REG(0, a);

    tmp2 = scheme_make_pair(scheme_read_bignum(a, 0, 10), tmp2);

    MZ_GC_UNREG();

    if (SCHEME_INTP(tmp2)) {
      return 0;
    }

    MZ_GC_REG();
    tmp1 = scheme_make_pair(scheme_read_bignum(a, 0, 8), tmp2);
    MZ_GC_UNREG();

    return tmp1;
  }
}

A @cpp{MZ_GC_DECL_REG} can be used in a nested block to hold
declarations for the block's variables. In that case, the nested
@cpp{MZ_GC_DECL_REG} must have its own @cpp{MZ_GC_REG} and
@cpp{MZ_GC_UNREG} calls.

@verbatim[#:indent 2]{
  {
    Scheme_Object *accum = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, accum);
    MZ_GC_REG();

    accum = scheme_make_pair(scheme_true, scheme_null);
    {
      Scheme_Object *tmp = NULL;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, tmp);
      MZ_GC_REG();

      tmp = scheme_make_pair(scheme_true, scheme_false);
      accum = scheme_make_pair(tmp, accum);

      MZ_GC_UNREG();
    }
    accum = scheme_make_pair(scheme_true, accum);

    MZ_GC_UNREG();
    return accum;
  }
}

Variables declared in a local block can also be registered together
with variables from an enclosing block, but the local-block variable
must be unregistered before it goes out of scope. The
@cppi{MZ_GC_NO_VAR_IN_REG} macro can be used to unregister a variable
or to initialize a slot as having no variable.

@verbatim[#:indent 2]{
  {
    Scheme_Object *accum = NULL;
    MZ_GC_DECL_REG(2);
    MZ_GC_VAR_IN_REG(0, accum);
    MZ_GC_NO_VAR_IN_REG(1);
    MZ_GC_REG();

    accum = scheme_make_pair(scheme_true, scheme_null);
    {
      Scheme_Object *tmp = NULL;
      MZ_GC_VAR_IN_REG(1, tmp);

      tmp = scheme_make_pair(scheme_true, scheme_false);
      accum = scheme_make_pair(tmp, accum);

      MZ_GC_NO_VAR_IN_REG(1);
    }
    accum = scheme_make_pair(scheme_true, accum);

    MZ_GC_UNREG();
    return accum;
  }
}

The @cpp{MZ_GC_} macros all expand to nothing when @cpp{MZ_PRECISE_GC}
is not defined, so the macros can be placed into code to be compiled
for both conservative and precise collection.

The @cpp{MZ_GC_REG} and @cpp{MZ_GC_UNREG} macros must never be
used in an OS thread other than Scheme's thread.

@; - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - - 

@subsection[#:tag "im:3m:mzc"]{Local Pointers and @|mzc| @DFlag{xform}}

When @|mzc| is run with the @DFlag{xform} flag and a source C program,
it produces a C program that is instrumented in the way described in
the previous section (but with a slightly different set of macros).
For each input file @filepath{@italic{name}.c}, the transformed output
is @filepath{@italic{name}.3m.c}.

The @DFlag{xform} mode for @|mzc| does not change allocation calls,
nor does it generate size, mark, or fixup predocures. It merely
converts the code to register local pointers.

Furthermore, the @DFlag{xform} mode for @|mzc| does not handle all of
C. It's ability to rearrange compound expressions is particularly
limited, because @DFlag{xform} merely converts expression text
heuristically instead of parsing C. A future version of the tool will
correct such problems. For now, @|mzc| in @DFlag{xform} mode attempts
to provide reasonable error messages when it is unable to convert a
program, but beware that it can miss cases. To an even more limited
degree, @DFlag{xform} can work on C++ code. Inspect the output of
@DFlag{xform} mode to ensure that your code is correctly instrumented.

Some specific limitations:

@itemize{

 @item{The body of a @cpp{for}, @cpp{while}, or @cpp{do} loop must be
       surrounded with curly braces.  (A conversion error is normally
       reported, otherwise.)}

 @item{Function calls may not appear on the right-hand side of an
       assignment within a declaration block.  (A conversion error is
       normally reported if such an assignment is discovered.)}

 @item{Multiple function calls in @cpp{... ? ... : ...} cannot be
       lifted. (A conversion error is normally reported, otherwise.)}

 @item{In an assignment, the left-hand side must be a local or static
       variable, not a field selection, pointer dereference, etc. (A
       conversion error is normally reported, otherwise.)}

 @item{The conversion assumes that all function calls use an immediate
       name for a function, as opposed to a compound expression as
       in @cpp{s->f()}. The function name need not be a top-level
       function name, but it must be bound either as an argument or
       local variable with the form @cpp{@var{type} @var{id}}; the
       syntax @cpp{@var{ret_type} (*@var{id})(...)} is not
       recgoinzed, so bind the function type to a simple name
       with @cpp{typedef}, first: @cpp{typedef @var{ret_type}
       (*@var{type})(...); .... @var{type} @var{id}}.}

 @item{Arrays and structs must be passed by address, only.}

 @item{GC-triggering code must not appear in system headers.}

 @item{Pointer-comparison expressions are not handled correctly when
       either of the compared expressions includes a function call.
       For example, @cpp{a() == b()} is not converted correctly when
       @cpp{a} and @cpp{b} produce pointer values.}

 @item{Passing the address of a local pointer to a function works only
       when the pointer variable remains live after the function call.}

 @item{A @cpp{return;} form can get converted to @cpp["{ " @var{stmt}
       "; return; };"], which can break an @cpp{if (...) return; else
       ...} pattern.}

 @item{Local instances of union types are generally not supported.}

 @item{Pointer arithmetic cannot be converted away, and is instead
       reported as an error.}

} 

@; - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - -  - - 

@subsection[#:tag "im:3m:macros"]{Guiding @|mzc| @DFlag{xform}}

The following macros can be used (with care!) to navigate
@DFlag{xform} around code that it cannot handle:

@itemize{

@item{@cppi{XFORM_START_SKIP} and @cppi{XFORM_END_SKIP}: code
  between these two statements is ignored by the transform tool,
  except to tokenize it.

 Example:

@verbatim[#:indent 2]{
  int foo(int c, ...) {
    int r = 0;
    XFORM_START_SKIP;
    {
      /* va plays strange tricks that confuse xform */
      va_list args;
      va_start(args, c);
      while (c--) {
        r += va_arg(args, int);
      }
    }
    XFORM_END_SKIP;
    return r;
  }
}

 These macros can also be used at the top level, outside of any
 function.  Since they have to be terminated by a semi-colon, however,
 top-level uses usually must be wrapped with @cpp{#ifdef
   MZ_PRECISE_GC} and @cpp{#endif}; a semi-colon by itself at the
 top level is not legal in C.}

@item{@cppi{XFORM_HIDE_EXPR}: a macro that takes wraps an expression to
  disable processing of the expression.

  Example:

  @verbatim[#:indent 2]{
    int foo(int c, ...) {
      int r = 0;
      {
        /* va plays strange tricks that confuse xform */
        XFORM_CAN_IGNORE va_list args; /* See below */
        XFORM_HIDE_EXPR(va_start(args, c));
        while (c--) {
          r += XFORM_HIDE_EXPR(va_arg(args, int));
        }
      }
      return r;
    }
  }}

@item{@cppi{XFORM_CAN_IGNORE}: a macro that acts like a type
  modifier (must appear first) to indicate that a declared variable
  can be treated as atomic. See above for an example.}

@item{@cppi{XFORM_START_SUSPEND} and @cppi{XFORM_END_SUSPEND}: for
  use at the top level (outside of any function definition), and
  similar to @cpp{XFORM_START_SKIP} and @cpp{XFORM_END_SKIP} in
  that function and class bodies are not transformed. Type and
  prototype information is still collected for use by later
  transformations, however. These forms must be terminated by a
  semi-colon.}

@item{@cppi{XFORM_START_TRUST_ARITH} and
  @cppi{XFORM_END_TRUST_ARITH}: for use at the top level (outside
  of any function definition) to disable warnings about pointer
  arithmetic. Use only when you're absolutely certain that the garbage
  collector cannot be pointers offset into the middle of a collectable
  object. These forms must be terminated by a semi-colon.}

@item{@cppi{XFORM_TRUST_PLUS}: a replacement for @cpp{+} that does
  not trigger pointer-arithmetic warnings. Use with care.}

@item{@cppi{XFORM_TRUST_MINUS}: a replacement for @cpp{-} that does
  not trigger pointer-arithmetic warnings. Use with care.}

}

@section{Memory Functions}

@function[(void* scheme_malloc
           [size_t n])]{

Allocates @var{n} bytes of collectable memory, initially filled with
zeros. In 3m, the allocated object is treated as an array of
pointers.}

@function[(void* scheme_malloc_atomic
           [size_t n])]{

Allocates @var{n} bytes of collectable memory containing no pointers
visible to the garbage collector. The object is @italic{not}
initialized to zeros.}

@function[(void* scheme_malloc_uncollectable
           [size_t n])]{

Non-3m, only. Allocates @var{n} bytes of uncollectable memory.}

@function[(void* scheme_malloc_eternal
           [size_t n])]{

Allocates uncollectable atomic memory. This function is equivalent to
 @cpp{malloc}, except that the memory cannot be freed.}

@function[(void* scheme_calloc
           [size_t num]
           [size_t size])]{

Allocates @var{num} * @var{size} bytes of memory using @cpp{scheme_malloc}.}

@function[(void* scheme_malloc_tagged
           [size_t n])]{

Like @cpp{scheme_malloc}, but in 3m, the type tag determines how the
 garbage collector traverses the object; see @secref["im:memoryalloc"].}

@function[(void* scheme_malloc_allow_interior
           [size_t n])]{

Like @cpp{scheme_malloc}, but in 3m, pointers are allowed to
 reference the middle of the object; see @secref["im:memoryalloc"].}

@function[(char* scheme_strdup
           [char* str])]{

Copies the null-terminated string @var{str}; the copy is collectable.}

@function[(char* scheme_strdup_eternal
           [char* str])]{

Copies the null-terminated string @var{str}; the copy will never be freed.}

@function[(void* scheme_malloc_fail_ok
           [size_t size]
           [size_t size])]{

Attempts to allocate @var{size} bytes using @var{mallocf}. If the
allocation fails, the @scheme[exn:misc:out-of-memory] exception is
raised.}

@function[(void** scheme_malloc_immobile_box
           [void* p])]{

Allocates memory that is not garbage-collected and that does not move
(even with 3m), but whose first word contains a pointer to a
collectable object. The box is initialized with @var{p}, but the value
can be changed at any time. An immobile box must be explicitly freed
using @cpp{scheme_free_immobile_box}.}

@function[(void scheme_free_immobile_box
           [void** b])]{

Frees an immobile box allocated with @cpp{scheme_malloc_immobile_box}.}

@function[(void* scheme_malloc_code [long size])]{

Allocates non-collectable memory to hold executable machine code. Use
this function instead of @cpp{malloc} to ensure that the allocated
memory has ``execute'' permissions. Use @cpp{scheme_free_code} to free
memory allocated by this function.}

@function[(void scheme_free_code [void* p])]{

Frees memory allocated with @cpp{scheme_malloc_code}.}

@function[(void scheme_register_extension_global
           [void* ptr]
           [long size])]{

Registers an extension's global variable that can contain Scheme
 pointers. The address of the global is given in @var{ptr}, and its
 size in bytes in @var{size}.In addition to global variables, this
 function can be used to register any permanent memory that the
 collector would otherwise treat as atomic. A garbage collection can
 occur during the registration.}


@function[(int scheme_main_setup
           [int no_auto_statics]
           [Scheme_Env_Main main]
           [int argc]
           [char** argv])]{

Initializes the GC stack base, creates the initial namespace by
calling @cpp{scheme_basic_env}, and then calls @var{main} with the
namespace, @var{argc}, and @var{argv}. (The @var{argc} and @var{argv}
are just passed on to @var{main}, and are not inspected in any way.)

The @cpp{Scheme_Env_Main} type is defined as follows:

@verbatim[#:indent 4]{
typedef int (*Scheme_Env_Main)(Scheme_Env *env, 
                               int argc, char **argv);
}

The result of @var{main} is the result of @cpp{scheme_main_setup}.

If @var{no_auto_statics} is non-zero, then static variables must be
explicitly registered with the garbage collector; see
@secref["im:memoryalloc"] for more information.}


@function[(int scheme_main_stack_setup
           [int no_auto_statics]
           [Scheme_Nested_Main main]
           [void* data])]{

A more primitive variant of @cpp{scheme_main_setup} that initializes
the GC stack base but does not create the initial namespace (so an
embedding application can perform other operations that involve
garbage-collected data before creating a namespace).

The @var{data} argument is passed through to @var{main}, where the
@cpp{Scheme_Nested_Main} type is defined as follows:

@verbatim[#:indent 4]{
typedef int (*Scheme_Nested_Main)(void *data);
}}


@function[(void scheme_set_stack_base
           [void* stack_addr]
           [int no_auto_statics])]{

Overrides the GC's auto-determined stack base, and/or disables the
 GC's automatic traversal of global and static variables. If
 @var{stack_addr} is @cpp{NULL}, the stack base determined by the GC is
 used. Otherwise, it should be the ``deepest'' memory address on the
 stack where a collectable pointer might be stored. This function
 should be called only once, and before any other @cpp{scheme_}
 function is called. It never triggers a garbage collection.

The following example shows a typical use for setting the stack base
for CGC:

@verbatim[#:indent 4]{
    int main(int argc, char **argv) {
       int dummy;
       scheme_set_stack_base(&dummy, 0);
       real_main(argc, argv); /* calls scheme_basic_env(), etc. */
    }
}

Under 3m, the above code does not quite work, because @var{stack_addr}
must be the beginning or end of a local-frame registration. Worse, in
CGC or 3m, if @cpp{real_main} is declared @cpp{static}, the compiler
may inline it and place variables containing collectable values deeper
in the stack than @cpp{dummy}. To avoid these problems, use
@cpp{scheme_main_setup} or @cpp{scheme_main_stack_setup}, instead.}

@function[(void scheme_set_stack_bounds
           [void* stack_addr]
           [void* stack_end]
           [int no_auto_statics])]{

Like @cpp{scheme_set_stack_base}, except for the extra
@var{stack_end} argument. If @var{stack_end} is non-@cpp{NULL}, then
it corresponds to a point of C-stack growth after which Scheme
should attempt to handle stack overflow. The @var{stack_end} argument
should not correspond to the actual stack end, since detecting stack
overflow may take a few frames, and since handling stack overflow
requires a few frames.

If @var{stack_end} is @cpp{NULL}, then the stack end is computed
automatically: the stack size assumed to be the limit reported by
@cpp{getrlimit} under Unix and Mac OS X, or it is assumed to be 1 MB
under Windows; if this size is greater than 8 MB, then 8 MB is
assumed, instead; the size is decremented by 50000 bytes to cover a
large margin of error; finally, the size is subtracted from (for
stacks that grow down) or added to (for stacks that grow up) the stack
base in @var{stack_addr} or the auotmatically computed stack
base. Note that the 50000-byte margin of error is assumed to cover the
difference between the actual stack start and the reported stack base,
in addition to the margin needed for detecting and handling stack
overflow.}

@function[(void scheme_register_static
           [void* ptr]
           [long size])]{

Like @cpp{scheme_register_extension_global}, for use in embedding
 applications in situations where the collector does not automatically
 find static variables (i.e., when @cpp{scheme_set_stack_base} has
 been called with a non-zero second argument).

The macro @cppi{MZ_REGISTER_STATIC} can be used directly on a static
 variable. It expands to a comment if statics need not be registered,
 and a call to @cpp{scheme_register_static} (with the address of the
 static variable) otherwise.}

@function[(void scheme_weak_reference
           [void** p])]{

Registers the pointer @var{*p} as a weak pointer; when no other
(non-weak) pointers reference the same memory as @var{*p} references,
then @var{*p} will be set to @cpp{NULL} by the garbage collector. The
value in @var{*p} may change, but the pointer remains weak with
respect to the value of @var{*p} at the time @var{p} was registered.

This function is not available in 3m.}

@function[(void scheme_weak_reference_indirect
           [void** p]
           [void* v])]{

Like @cppi{scheme_weak_reference}, but @var{*p} is set to @cpp{NULL}
(regardless of its prior value) when there are no references to @var{v}.

This function is not available in 3m.}

@function[(void scheme_register_finalizer
           [void* p]
           [fnl_proc f]
           [void* data]
           [fnl_proc* oldf]
           [void** olddata])]{

Registers a callback function to be invoked when the memory @var{p}
would otherwise be garbage-collected, and when no ``will''-like
finalizers are registered for @var{p}.

The @cpp{fnl_proc} type is not actually defined, but it is equivalent
to

@verbatim[#:indent 2]{typedef void (*fnl_proc)(void *p, void *data)}

The @var{f} argument is the callback function; when it is called, it
will be passed the value @var{p} and the data pointer @var{data};
@var{data} can be anything --- it is only passed on to the callback
function. If @var{oldf} and @var{olddata} are not @cpp{NULL}, then
@var{*oldf} and @var{*olddata} are filled with the old callback
information (@var{f} and @var{data} will override this old callback).

To remove a registered finalizer, pass @cpp{NULL} for @var{f} and
@var{data}.

Note: registering a callback not only keeps @var{p} from collection
until the callback is invoked, but it also keeps @var{data} reachable
until the callback is invoked.}

@function[(void scheme_add_finalizer
           [void* p]
           [fnl_proc f]
           [void* data])]{

Adds a finalizer to a chain of primitive finalizers. This chain is
separate from the single finalizer installed with
@cpp{scheme_register_finalizer}; all finalizers in the chain are
called immediately after a finalizer that is installed with
@cpp{scheme_register_finalizer}.

See @cpp{scheme_register_finalizer}, above, for information about
the arguments.

To remove an added finalizer, use @cpp{scheme_subtract_finalizer}.}

@function[(void scheme_add_scheme_finalizer
           [void* p]
           [fnl_proc f]
           [void* data])]{

Installs a ``will''-like finalizer, similar to @scheme[will-register].
 Scheme finalizers are called one at a time, requiring the collector
 to prove that a value has become inaccessible again before calling
 the next Scheme finalizer. Finalizers registered with
 @cpp{scheme_register_finalizer} or @cpp{scheme_add_finalizer} are
 not called until all Scheme finalizers have been exhausted.

See @cpp{scheme_register_finalizer}, above, for information about
 the arguments.

There is currently no facility to remove a ``will''-like finalizer.}

@function[(void scheme_add_finalizer_once
           [void* p]
           [fnl_proc f]
           [void* data])]{

Like @cpp{scheme_add_finalizer}, but if the combination @var{f} and
 @var{data} is already registered as a (non-``will''-like) finalizer
 for @var{p}, it is not added a second time.}

@function[(void scheme_add_scheme_finalizer_once
           [void* p]
           [fnl_proc f]
           [void* data])]{

Like @cpp{scheme_add_scheme_finalizer}, but if the combination of
 @var{f} and @var{data} is already registered as a ``will''-like
 finalizer for @var{p}, it is not added a second time.}

@function[(void scheme_subtract_finalizer
           [void* p]
           [fnl_proc f]
           [void* data])]{

Removes a finalizer that was installed with
 @cpp{scheme_add_finalizer}.}

@function[(void scheme_remove_all_finalization
           [void* p])]{

Removes all finalization (``will''-like or not) for @var{p}, including
 wills added in Scheme with @scheme[will-register] and finalizers used
 by custodians.}

@function[(void scheme_dont_gc_ptr
           [void* p])]{

Keeps the collectable block @var{p} from garbage collection. Use this
 procedure when a reference to @var{p} is be stored somewhere
 inaccessible to the collector. Once the reference is no longer used
 from the inaccessible region, de-register the lock with
 @cpp{scheme_gc_ptr_ok}. A garbage collection can occur during the
 registration.

This function keeps a reference count on the pointers it registers, so
 two calls to @cppi{scheme_dont_gc_ptr} for the same @var{p} should
 be balanced with two calls to @cpp{scheme_gc_ptr_ok}.}

@function[(void scheme_gc_ptr_ok
           [void* p])]{

See @cpp{scheme_dont_gc_ptr}.}


@function[(void scheme_collect_garbage)]{

Forces an immediate garbage-collection.}


@function[(void GC_register_traversers
           [short tag]
           [Size_Proc s]
           [Mark_Proc m]
           [Fixup_Proc f]
           [int is_const_size]
           [int is_atomic])]{

3m only. Registers a size, mark, and fixup procedure for a given type
 tag; see @secref["im:3m:tagged"] for more information.

Each of the three procedures takes a pointer and returns an integer:

@verbatim[#:indent 2]{
  typedef int (*Size_Proc)(void *obj);
  typedef int (*Mark_Proc)(void *obj);
  typedef int (*Fixup_Proc)(void *obj);
}

If the result of the size procedure is a constant, then pass a
 non-zero value for @var{is_const_size}. If the mark and fixup
 procedures are no-ops, then pass a non-zero value
 for @var{is_atomic}.}


@function[(void* GC_resolve [void* p])]{

3m only. Can be called by a size, mark, or fixup procedure that is registered
with @cpp{GC_register_traversers}. It returns the current address of
an object @var{p} that might have been moved already, where @var{p}
corresponds to an object that is referenced directly by the object
being sized, marked, or fixed. This translation is necessary, for
example, if the size or structure of an object depends on the content
of an object it references. For example, the size of a class instance
usually depends on a field count that is stored in the class. A fixup
procedure should call this function on a reference @emph{before}
fixing it.}


@function[(void* GC_fixup_self [void* p])]{

3m only. Can be called by a fixup procedure that is registered with
@cpp{GC_register_traversers}. It returns the final address of @var{p},
which must be the pointer passed to the fixup procedure. For some
implementations of the memory manager, the result is the same as
@var{p}, either because objects are not moved or because the object is
moved before it is fixed. With other implementations, an object might
be moved after the fixup process, and the result is the location that
the object will have after garbage collection finished.}
