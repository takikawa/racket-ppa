#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scheme/class
          "guide-utils.ss"

          (for-label scheme/class
                     scheme/trait))

@(define class-eval
   (let ([e (make-base-eval)])
     (e '(require scheme/class))
     e))

@; FIXME: at some point, discuss classes vs. units vs. modules


@title[#:tag "classes"]{Classes and Objects}

@margin-note{This chapter is based on a paper @cite["Flatt06"].}

A @scheme[class] expression denotes a first-class value,
just like a @scheme[lambda] expression:

@specform[(class superclass-expr decl-or-expr ...)]

The @scheme[_superclass-expr] determines the superclass for the new
class. Each @scheme[_decl-or-expr] is either a declaration related to
methods, fields, and initialization arguments, or it is an expression
that is evaluated each time that the class is instantiated. In other
words, instead of a method-like constructor, a class has
initialization expressions interleaved with field and method
declarations.

By convention, class names end with @schemeidfont{%}. The built-in root class is
@scheme[object%]. The following expression creates a class with
public methods @scheme[get-size], @scheme[grow], and @scheme[eat]:

@schemeblock[
(class object%
  (init size)                (code:comment #,(t "initialization argument"))

  (define current-size size) (code:comment #,(t "field"))

  (super-new)                (code:comment #,(t "superclass initialization"))

  (define/public (get-size)
    current-size)

  (define/public (grow amt)
    (set! current-size (+ amt current-size)))

  (define/public (eat other-fish)
    (grow (send other-fish get-size))))
]

@(interaction-eval
#:eval class-eval
(define fish%
  (class object%
    (init size)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size))))))

The @scheme[size] initialization argument must be supplied via a named
 argument when instantiating the class through the @scheme[new] form:

@schemeblock[
(new (class object% (init size) ....) [size 10])
]

Of course, we can also name the class and its instance:

@schemeblock[
(define fish% (class object% (init size) ....))
(define charlie (new fish% [size 10]))
]

@(interaction-eval
#:eval class-eval
(define charlie (new fish% [size 10])))

In the definition of @scheme[fish%], @scheme[current-size] is a
private field that starts out with the value of the @scheme[size]
initialization argument. Initialization arguments like @scheme[size]
are available only during class instantiation, so they cannot be
referenced directly from a method. The @scheme[current-size] field, in
contrast, is available to methods.

The @scheme[(super-new)] expression in @scheme[fish%] invokes the
initialization of the superclass. In this case, the superclass is
@scheme[object%], which takes no initialization arguments and performs
no work; @scheme[super-new] must be used, anyway, because a class must
always invoke its superclass's initialization.

Initialization arguments, field declarations, and expressions such as
@scheme[(super-new)] can appear in any order within a @scheme[class],
and they can be interleaved with method declarations. The relative
order of expressions in the class determines the order of evaluation
during instantiation. For example, if a field's initial value requires
calling a method that works only after superclass initialization, then
the field declaration must be placed after the @scheme[super-new]
call. Ordering field and initialization declarations in this way helps
avoid imperative assignment. The relative order of method declarations
makes no difference for evaluation, because methods are fully defined
before a class is instantiated.

@section[#:tag "methods"]{Methods}

Each of the three @scheme[define/public] declarations in
@scheme[fish%] introduces a new method. The declaration uses the same
syntax as a Scheme function, but a method is not accessible as an
independent function.  A call to the @scheme[grow] method of a
@scheme[fish%] object requires the @scheme[send] form:

@interaction[
#:eval class-eval
(send charlie grow 6)
(send charlie get-size)
]

Within @scheme[fish%], self methods can be called like functions,
because the method names are in scope.  For example, the @scheme[eat]
method within @scheme[fish%] directly invokes the @scheme[grow]
method.  Within a class, attempting to use a method name in any way
other than a method call results in a syntax error.

In some cases, a class must call methods that are supplied by the superclass
but not overridden. In that case, the class can use @scheme[send]
with @scheme[this] to access the method:

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)
                         (send this eat fish2))))
]

Alternately, the class can declare the existence of a method using @scheme[inherit],
which brings the method name into scope for a direct call:

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

With the @scheme[inherit] declaration, if @scheme[fish%] had not
provided an @scheme[eat] method, an error would be signaled in the
evaluation of the @scheme[class] form for @scheme[hungry-fish%]. In
contrast, with @scheme[(send this ....)], an error would not be
signaled until the @scheme[eat-more] method is called and the
@scheme[send] form is evaluated. For this reason, @scheme[inherit] is
preferred.

Another drawback of @scheme[send] is that it is less efficient than
@scheme[inherit]. Invocation of a method via @scheme[send] involves
finding a method in the target object's class at run time, making
@scheme[send] comparable to an interface-based method call in Java. In
contrast, @scheme[inherit]-based method invocations use an offset
within the class's method table that is computed when the class is
created.

To achieve performance similar to @scheme[inherit]-based method calls when
invoking a method from outside the method's class, the programmer must use the
@scheme[generic] form, which produces a class- and method-specific
@defterm{generic method} to be invoked with @scheme[send-generic]:

@def+int[
#:eval class-eval
(define get-fish-size (generic fish% get-size))
(send-generic charlie get-fish-size)
(send-generic (new hungry-fish% [size 32]) get-fish-size)
(send-generic (new object%) get-fish-size)
]

Roughly speaking, the form translates the class and the external
method name to a location in the class's method table. As illustrated
by the last example, sending through a generic method checks that its
argument is an instance of the generic's class.

Whether a method is called directly within a @scheme[class],
through a generic method,
or through @scheme[send], method overriding works in the usual way:

@defs+int[
#:eval class-eval
[
(define picky-fish% (class fish% (super-new)
                      (define/override (grow amt)
                        ;; Doesn't eat all of its food
                        (super grow (* 3/4 amt)))))
(define daisy (new picky-fish% [size 20]))
]
(send daisy eat charlie)
(send daisy get-size)
]

The @scheme[grow] method in @scheme[picky-fish%] is declared with
@scheme[define/override] instead of @scheme[define/public], because
@scheme[grow] is meant as an overriding declaration. If @scheme[grow]
had been declared with @scheme[define/public], an error would have
been signaled when evaluating the @scheme[class] expression, because
@scheme[fish%] already supplies @scheme[grow].

Using @scheme[define/override] also allows the invocation of the
overridden method via a @scheme[super] call. For example, the
@scheme[grow] implementation in @scheme[picky-fish%] uses
@scheme[super] to delegate to the superclass implementation.

@section[#:tag "initargs"]{Initialization Arguments}

Since @scheme[picky-fish%] declares no initialization arguments, any
initialization values supplied in @scheme[(new picky-fish% ....)]  are
propagated to the superclass initialization, i.e., to @scheme[fish%].
A subclass can supply additional initialization arguments for its
superclass in a @scheme[super-new] call, and such initialization
arguments take precedence over arguments supplied to @scheme[new]. For
example, the following @scheme[size-10-fish%] class always generates
fish of size 10:

@def+int[
#:eval class-eval
(define size-10-fish% (class fish% (super-new [size 10])))
(send (new size-10-fish%) get-size)
]

In the case of @scheme[size-10-fish%], supplying a @scheme[size]
initialization argument with @scheme[new] would result in an
initialization error; because the @scheme[size] in @scheme[super-new]
takes precedence, a @scheme[size] supplied to @scheme[new] would have
no target declaration.

An initialization argument is optional if the @scheme[class] form
declares a default value. For example, the following @scheme[default-10-fish%]
class accepts a @scheme[size] initialization argument, but its value defaults to
10 if no value is supplied on instantiation:

@def+int[
#:eval class-eval
(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))
(new default-10-fish%)
(new default-10-fish% [size 20])
]

In this example, the @scheme[super-new] call propagates its own
@scheme[size] value as the @scheme[size] initialization argument to
the superclass.

@section[#:tag "intnames"]{Internal and External Names}

The two uses of @scheme[size] in @scheme[default-10-fish%] expose the
double life of class-member identifiers. When @scheme[size] is the
first identifier of a bracketed pair in @scheme[new] or
@scheme[super-new], @scheme[size] is an @defterm{external name} that
is symbolically matched to an initialization argument in a class. When
@scheme[size] appears as an expression within
@scheme[default-10-fish%], @scheme[size] is an @defterm{internal name}
that is lexically scoped. Similarly, a call to an inherited
@scheme[eat] method uses @scheme[eat] as an internal name, whereas a
@scheme[send] of @scheme[eat] uses @scheme[eat] as an external name.

The full syntax of the @scheme[class] form allows a programmer to
specify distinct internal and external names for a class member. Since
internal names are local, they can be renamed to avoid shadowing or
conflicts. Such renaming is not frequently necessary, but workarounds
in the absence of renaming can be especially cumbersome.

@section{Interfaces}

Interfaces are useful for checking that an object or a class
implements a set of methods with a particular (implied) behavior.
This use of interfaces is helpful even without a static type system
(which is the main reason that Java has interfaces).

An interface in PLT Scheme is created using the @scheme[interface]
form, which merely declares the method names required to implement the
interface. An interface can extend other interfaces, which means that
implementations of the interface automatically implement the extended
interfaces.

@specform[(interface (superinterface-expr ...) id ...)]

To declare that a class implements an interface, the
@scheme[class*] form must be used instead of @scheme[class]:

@specform[(class* superclass-expr (interface-expr ...) decl-or-expr ...)]

For example, instead of forcing all fish classes to be derived from
@scheme[fish%], we can define @scheme[fish-interface] and change the
@scheme[fish%] class to declare that it implements
@scheme[fish-interface]:

@schemeblock[
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface) ....))
]

If the definition of @scheme[fish%] does not include
@scheme[get-size], @scheme[grow], and @scheme[eat] methods, then an
error is signaled in the evaluation of the @scheme[class*] form,
because implementing the @scheme[fish-interface] interface requires
those methods.

The @scheme[is-a?] predicate accepts either a class or interface as
its first argument and an object as its second argument. When given a
class, @scheme[is-a?] checks whether the object is an instance of that
class or a derived class.  When given an interface, @scheme[is-a?]
checks whether the object's class implements the interface. In
addition, the @scheme[implementation?]  predicate checks whether a
given class implements a given interface.

@section[#:tag "inner"]{Final, Augment, and Inner}

As in Java, a method in a @scheme[class] form can be specified as
@defterm{final}, which means that a subclass cannot override the
method.  A final method is declared using @scheme[public-final] or
@scheme[override-final], depending on whether the declaration is for a
new method or an overriding implementation.

Between the extremes of allowing arbitrary overriding and disallowing
overriding entirely, the class system also supports Beta-style
@defterm{augmentable} methods @cite["Goldberg04"]. A method
declared with @scheme[pubment] is like @scheme[public], but the method
cannot be overridden in subclasses; it can be augmented only. A
@scheme[pubment] method must explicitly invoke an augmentation (if any)
using @scheme[inner]; a subclass augments the method using
@scheme[augment], instead of @scheme[override].

In general, a method can switch between augment and override modes in
a class derivation. The @scheme[augride] method specification
indicates an augmentation to a method where the augmentation is itself
overrideable in subclasses (though the superclass's implementation
cannot be overridden). Similarly, @scheme[overment] overrides a method
and makes the overriding implementation augmentable.

@section[#:tag "extnames"]{Controlling the Scope of External Names}

As noted in @secref["intnames"], class members have both
internal and external names. A member definition binds an internal
name locally, and this binding can be locally renamed.  External
names, in contrast, have global scope by default, and a member
definition does not bind an external name. Instead, a member
definition refers to an existing binding for an external name, where
the member name is bound to a @defterm{member key}; a class ultimately
maps member keys to methods, fields, and initialization arguments.

Recall the @scheme[hungry-fish%] @scheme[class] expression:

@schemeblock[
(define hungry-fish% (class fish% ....
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

During its evaluation, the @scheme[hungry-fish%] and @scheme[fish%]
classes refer to the same global binding of @scheme[eat].  At run
time, calls to @scheme[eat] in @scheme[hungry-fish%] are matched with
the @scheme[eat] method in @scheme[fish%] through the shared method
key that is bound to @scheme[eat].

The default binding for an external name is global, but a
programmer can introduce an external-name binding with the
@scheme[define-member-name] form.

@specform[(define-member-name id member-key-expr)]

In particular, by using @scheme[(generate-member-key)] as the
@scheme[member-key-expr], an external name can be localized for a
particular scope, because the generated member key is inaccessible
outside the scope. In other words, @scheme[define-member-name] gives
an external name a kind of package-private scope, but generalized from
packages to arbitrary binding scopes in Scheme.

For example, the following @scheme[fish%] and @scheme[pond%] classes cooperate
via a @scheme[get-depth] method that is only accessible to the
cooperating classes:

@schemeblock[
(define-values (fish% pond%) (code:comment #,(t "two mutually recursive classes"))
  (let () ; create a local definition scope
    (define-member-name get-depth (generate-member-key))
    (define fish%
      (class ....
        (define my-depth ....)
	(define my-pond ....)
	(define/public (dive amt)
        (set! my-depth
              (min (+ my-depth amt)
                   (send my-pond get-depth))))))
    (define pond%
      (class ....
        (define current-depth ....)
        (define/public (get-depth) current-depth)))
    (values fish% pond%)))
]

External names are in a namespace that separates them from other Scheme
names. This separate namespace is implicitly used for the method name in
@scheme[send], for initialization-argument names in @scheme[new], or for
the external name in a member definition.  The special form
@scheme[member-name-key] provides access to the binding of an external name
in an arbitrary expression position: @scheme[(member-name-key id)]
produces the member-key binding of @scheme[id] in the current scope.

A member-key value is primarily used with a
@scheme[define-member-name] form. Normally, then,
@scheme[(member-name-key id)] captures the method key of @scheme[id]
so that it can be communicated to a use of @scheme[define-member-name]
in a different scope. This capability turns out to be useful for
generalizing mixins, as discussed next.

@; ----------------------------------------------------------------------

@section{Mixins}

Since @scheme[class] is an expression form instead of a top-level
declaration as in Smalltalk and Java, a @scheme[class] form can be
nested inside any lexical scope, including @scheme[lambda]. The result
is a @deftech{mixin}, i.e., a class extension that is parameterized
with respect to its superclass.

For example, we can parameterize the @scheme[picky-fish%] class over
its superclass to define @scheme[picky-mixin]:

@schemeblock[
(define (picky-mixin %)
  (class % (super-new)
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define picky-fish% (picky-mixin fish%))
]

Many small differences between Smalltalk-style classes and Scheme
classes contribute to the effective use of mixins. In particular, the
use of @scheme[define/override] makes explicit that
@scheme[picky-mixin] expects a class with a @scheme[grow] method. If
@scheme[picky-mixin] is applied to a class without a @scheme[grow]
method, an error is signaled as soon as @scheme[picky-mixin] is
applied.

Similarly, a use of @scheme[inherit] enforces a ``method existence''
requirement when the mixin is applied:

@schemeblock[
(define (hungry-mixin %)
  (class % (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2) 
      (eat fish1) 
      (eat fish2))))
]

The advantage of mixins is that we can easily combine them to create
new classes whose implementation sharing does not fit into a
single-inheritance hierarchy---without the ambiguities associated with
multiple inheritance. Equipped with @scheme[picky-mixin] and
@scheme[hungry-mixin], creating a class for a hungry, yet picky fish
is straightforward:

@schemeblock[
(define picky-hungry-fish% 
  (hungry-mixin (picky-mixin fish%)))
]

The use of keyword initialization arguments is critical for the easy
use of mixins. For example, @scheme[picky-mixin] and
@scheme[hungry-mixin] can augment any class with suitable @scheme[eat]
and @scheme[grow] methods, because they do not specify initialization
arguments and add none in their @scheme[super-new] expressions:

@schemeblock[
(define person% 
  (class object%
    (init name age)
    ....
    (define/public (eat food) ....)
    (define/public (grow amt) ....)))
(define child% (hungry-mixin (picky-mixin person%)))
(define oliver (new child% [name "Oliver"][age 6]))
]

Finally, the use of external names for class members (instead of
lexically scoped identifiers) makes mixin use convenient. Applying
@scheme[picky-mixin] to @scheme[person%] works because the names
@scheme[eat] and @scheme[grow] match, without any a priori declaration
that @scheme[eat] and @scheme[grow] should be the same method in
@scheme[fish%] and @scheme[person%]. This feature is a potential
drawback when member names collide accidentally; some accidental
collisions can be corrected by limiting the scope external names, as
discussed in @secref["extnames"].

@subsection{Mixins and Interfaces}

Using @scheme[implementation?], @scheme[picky-mixin] could require
that its base class implements @scheme[grower-interface], which could
be implemented by both @scheme[fish%] and @scheme[person%]:

@schemeblock[
(define grower-interface (interface () grow))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class % ....))
]

Another use of interfaces with a mixin is to tag classes generated by
the mixin, so that instances of the mixin can be recognized. In other
words, @scheme[is-a?] cannot work on a mixin represented as a
function, but it can recognize an interface (somewhat like a
@defterm{specialization interface}) that is consistently implemented
by the mixin.  For example, classes generated by @scheme[picky-mixin]
could be tagged with @scheme[picky-interface], enabling the
@scheme[is-picky?] predicate:

@schemeblock[
(define picky-interface (interface ()))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class* % (picky-interface) ....))
(define (is-picky? o)
  (is-a? o picky-interface))
]

@subsection{The @scheme[mixin] Form}

To codify the @scheme[lambda]-plus-@scheme[class] pattern for
implementing mixins, including the use of interfaces for the domain
and range of the mixin, the class system provides a @scheme[mixin]
macro:

@specform[
(mixin (interface-expr ...) (interface-expr ...)
  decl-or-expr ...)
]

The first set of @scheme[interface-expr]s determines the domain of the
mixin, and the second set determines the range. That is, the expansion
is a function that tests whether a given base class implements the
first sequence of @scheme[interface-expr]s and produces a class that
implements the second sequence of @scheme[interface-expr]s. Other
requirements, such as the presence of @scheme[inherit]ed methods in
the superclass, are then checked for the @scheme[class] expansion of
the @scheme[mixin] form.

Mixins not only override methods and introduce public methods, they
can also augment methods, introduce augment-only methods, add an
overrideable augmentation, and add an augmentable override --- all of
the things that a class can do (see @secref["inner"]).


@subsection[#:tag "parammixins"]{Parameterized Mixins}

As noted in @secref["extnames"], external names can be bound with
@scheme[define-member-name]. This facility allows a mixin to be
generalized with respect to the methods that it defines and uses.  For
example, we can parameterize @scheme[hungry-mixin] with respect to the
external member key for @scheme[eat]:

@schemeblock[
(define (make-hungry-mixin eat-method-key)
  (define-member-name eat eat-method-key)
  (mixin () () (super-new)
    (inherit eat)
    (define/public (eat-more x y) (eat x) (eat y))))
]

To obtain a particular hungry-mixin, we must apply this function to a
member key that refers to a suitable
@scheme[eat] method, which we can obtain using @scheme[member-name-key]: 

@schemeblock[
((make-hungry-mixin (member-name-key eat))
 (class object% .... (define/public (eat x) 'yum)))
]

Above, we apply @scheme[hungry-mixin] to an anonymous class that provides
@scheme[eat], but we can also combine it with a class that provides 
@scheme[chomp], instead:

@schemeblock[
((make-hungry-mixin (member-name-key chomp))
 (class object% .... (define/public (chomp x) 'yum)))
]

@; ----------------------------------------------------------------------

@section{Traits}

A @defterm{trait} is similar to a mixin, in that it encapsulates a set
of methods to be added to a class. A trait is different from a mixin
in that its individual methods can be manipulated with trait operators
such as @scheme[trait-sum] (merge the methods of two traits), @scheme[trait-exclude]
(remove a method from a trait), and @scheme[trait-alias] (add a copy of a
method with a new name; do not redirect any calls to the old name).

The practical difference between mixins and traits is that two traits
can be combined, even if they include a common method and even if
neither method can sensibly override the other. In that case, the
programmer must explicitly resolve the collision, usually by aliasing
methods, excluding methods, and merging a new trait that uses the
aliases.

Suppose our @scheme[fish%] programmer wants to define two class
extensions, @scheme[spots] and @scheme[stripes], each of which
includes a @scheme[get-color] method. The fish's spot color should not
override the stripe color nor vice-versa; instead, a
@scheme[spots+stripes-fish%] should combine the two colors, which is
not possible if @scheme[spots] and @scheme[stripes] are implemented as
plain mixins. If, however, @scheme[spots] and @scheme[stripes] are
implemented as traits, they can be combined. First, we alias
@scheme[get-color] in each trait to a non-conflicting name. Second,
the @scheme[get-color] methods are removed from both and the traits
with only aliases are merged. Finally, the new trait is used to create
a class that introduces its own @scheme[get-color] method based on the
two aliases, producing the desired @scheme[spots+stripes] extension.

@subsection{Traits as Sets of Mixins}

One natural approach to implementing traits in PLT Scheme is as a set
of mixins, with one mixin per trait method.  For example, we might
attempt to define the spots and stripes traits as follows, using
association lists to represent sets:

@schemeblock[
(define spots-trait
  (list (cons 'get-color 
               (lambda (%) (class % (super-new)
                             (define/public (get-color) 
                               'black))))))
(define stripes-trait
  (list (cons 'get-color 
              (lambda (%) (class % (super-new)
                            (define/public (get-color) 
                              'red))))))
]

A set representation, such as the above, allows @scheme[trait-sum] and
@scheme[trait-exclude] as simple manipulations; unfortunately, it does
not support the @scheme[trait-alias] operator. Although a mixin can be
duplicated in the association list, the mixin has a fixed method name,
e.g., @scheme[get-color], and mixins do not support a method-rename
operation. To support @scheme[trait-alias], we must parameterize the
mixins over the external method name in the same way that @scheme[eat]
was parameterized in @secref["parammixins"].

To support the @scheme[trait-alias] operation, @scheme[spots-trait]
should be represented as:

@schemeblock[
(define spots-trait
  (list (cons (member-name-key get-color)
              (lambda (get-color-key %) 
                (define-member-name get-color get-color-key)
                (class % (super-new)
                  (define/public (get-color) 'black))))))
]

When the @scheme[get-color] method in @scheme[spots-trait] is aliased
to @scheme[get-trait-color] and the @scheme[get-color] method is
removed, the resulting trait is the same as

@schemeblock[
(list (cons (member-name-key get-trait-color)
            (lambda (get-color-key %)
              (define-member-name get-color get-color-key)
              (class % (super-new)
                (define/public (get-color) 'black)))))
]

To apply a trait @scheme[_T] to a class @scheme[_C] and obtain a derived
class, we use @scheme[((trait->mixin _T) _C)]. The @scheme[trait->mixin]
function supplies each mixin of @scheme[_T] with the key for the mixin's
method and a partial extension of @scheme[_C]:

@schemeblock[
(define ((trait->mixin T) C)
  (foldr (lambda (m %) ((cdr m) (car m) %)) C T))
]

Thus, when the trait above is combined with other traits and then
applied to a class, the use of @scheme[get-color] becomes a reference
to the external name @scheme[get-trait-color].

@subsection{Inherit and Super in Traits}

This first implementation of traits supports @scheme[trait-alias], and it
 supports a trait method that calls itself, but it does not support
 trait methods that call each other. In particular, suppose that a spot-fish's
 market value depends on the color of its spots:

@schemeblock[
(define spots-trait
  (list (cons (member-name-key get-color) ....)
        (cons (member-name-key get-price)
              (lambda (get-price %) ....
                (class % ....
                  (define/public (get-price) 
                    .... (get-color) ....))))))
]

In this case, the definition of @scheme[spots-trait] fails, because
@scheme[get-color] is not in scope for the @scheme[get-price]
mixin. Indeed, depending on the order of mixin application when the
trait is applied to a class, the @scheme[get-color] method may not be
available when @scheme[get-price] mixin is applied to the class.
Therefore adding an @scheme[(inherit get-color)] declaration to the
@scheme[get-price] mixin does not solve the problem.

One solution is to require the use of @scheme[(send this get-color)] in
methods such as @scheme[get-price]. This change works because
@scheme[send] always delays the method lookup until the method call is
evaluated. The delayed lookup is more expensive than a direct call,
however. Worse, it also delays checking whether a @scheme[get-color] method
even exists.

A second, effective, and efficient solution is to change the encoding
of traits. Specifically, we represent each method as a pair of mixins:
one that introduces the method and one that implements it. When a
trait is applied to a class, all of the method-introducing mixins are
applied first. Then the method-implementing mixins can use
@scheme[inherit] to directly access any introduced method.

@schemeblock[
(define spots-trait
  (list (list (local-member-name-key get-color)
              (lambda (get-color get-price %) ....
                (class % ....
                  (define/public (get-color) (void))))
              (lambda (get-color get-price %) ....
                 (class % ....
                   (define/override (get-color) 'black))))
        (list (local-member-name-key get-price)
              (lambda (get-price get-color %) ....
                (class % ....
                  (define/public (get-price) (void))))
              (lambda (get-color get-price %) ....
                (class % ....
                  (inherit get-color)
                  (define/override (get-price)
		    .... (get-color) ....))))))
]

With this trait encoding, @scheme[trait-alias] adds a new method with
a new name, but it does not change any references to the old method.

@subsection{The @scheme[trait] Form}

The general-purpose trait pattern is clearly too complex for a
programmer to use directly, but it is easily codified in a
@scheme[trait] macro:

@specform[
(trait trait-clause ...)
]

The @scheme[id]s in the optional @scheme[inherit] clause are available for direct
reference in the method @scheme[expr]s, and they must be supplied
either by other traits or the base class to which
the trait is ultimately applied.

Using this form in conjunction with trait operators such as
@scheme[trait-sum], @scheme[trait-exclude], @scheme[trait-alias], and
@scheme[trait->mixin], we can implement @scheme[spots-trait] and
@scheme[stripes-trait] as desired.

@schemeblock[
(define spots-trait
  (trait
    (define/public (get-color) 'black)
    (define/public (get-price) ... (get-color) ...)))

(define stripes-trait
  (trait 
    (define/public (get-color) 'red)))

(define spots+stripes-trait
  (trait-sum 
   (trait-exclude (trait-alias spots-trait 
                               get-color get-spots-color)
                  get-color) 
   (trait-exclude (trait-alias stripes-trait 
                               get-color get-stripes-color)
                  get-color)
   (trait
     (inherit get-spots-color get-stripes-color)
     (define/public (get-color)
       .... (get-spots-color) .... (get-stripes-color) ....))))
]

@; ----------------------------------------------------------------------

@close-eval[class-eval]
