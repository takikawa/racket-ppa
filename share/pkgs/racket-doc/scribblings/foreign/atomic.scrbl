#lang scribble/doc
@(require "utils.rkt" (for-label ffi/unsafe/atomic))

@title{Atomic Execution}

@defmodule[ffi/unsafe/atomic]

@deftech{Atomic mode} evaluates a Racket expression without switching
among Racket threads and with limited support for events. An atomic
computation in this sense is @emph{not} atomic with respect to other
@tech[#:doc reference.scrbl]{places}, but only to other @tech[#:doc
reference.scrbl]{threads} within a place.

Atomic mode is unsafe, because the Racket scheduler is not able to
operate while execution is in atomic mode; the scheduler cannot switch
threads or poll certain kinds of events, which can lead to deadlock or
starvation of other threads. Beware that many operations can involve
such synchronization, such as writing to an output port.

@deftogether[(
@defproc[(start-atomic) void?]
@defproc[(end-atomic) void?]
)]{

Disables/re-enables context switches at the level of Racket threads,
and also suspends/resumes delivery of break exceptions (independent of the
result of @racket[break-enabled]). Calls to @racket[start-atomic] and
@racket[end-atomic] can be nested.

Note that pairing @racket[start-atomic] and @racket[end-atomic] with
@racket[dynamic-wind] is useful only when

@itemlist[

 @item{the current @tech[#:doc reference.scrbl]{exception handler} is
       known to safely escape atomic mode, or else all possible
       escapes are through known continuation jumps or aborts (because
       breaks are disabled and no other exceptions are possible) that
       escape safely; and}

 @item{exception constructions, if any, avoid printing values in the
       exception message, or else the @tech[#:doc
       reference.scrbl]{error value conversion handler} is always used
       and known to be safe for atomic mode.}

]

Using @racket[call-as-atomic] is somewhat safer than using
@racket[start-atomic] and @racket[end-atomic], because
@racket[call-as-atomic] catches exceptions and re-raises them after
exiting atomic mode, and it wraps any call to the error value
conversion handler with @racket[call-as-nonatomic]. The latter is safe
for a particular atomic region, however, only if the region can be
safely interrupted by a non-atomic exception construction.}


@deftogether[(
@defproc[(start-breakable-atomic) void?]
@defproc[(end-breakable-atomic) void?]
)]{

Like @racket[start-atomic] and @racket[end-atomic], but the delivery
of break exceptions is not suspended.

These functions are not significantly faster than
@racket[start-atomic] and @racket[end-atomic], so they provide no
benefit in a context where breaks are disabled.}


@defproc[(call-as-atomic [thunk (-> any)]) any]{

Calls @racket[thunk] in atomic mode, where @racket[call-as-nonatomic]
can be used during the dynamic extent of the call to revert to
non-atomic mode for a nested computation.

When @racket[call-as-atomic] is used in the dynamic extent of
@racket[call-as-atomic], then @racket[thunk] is called directly as a
non-tail call.

If @racket[thunk] raises an exception, the exception is caught and
re-raised after exiting atomic mode. Any call to the current
@tech[#:doc reference.scrbl]{error value conversion handler} is
effectively wrapped with @racket[call-as-nonatomic].

Besides obvious paths to unknown expressions that may not be safe for
atomic mode, beware of printing an arbitrary value in any way other
than the error value conversion handler, because values can have
arbitrary printing procedures attached through
@racket[prop:custom-write].}


@defproc[(call-as-nonatomic [thunk (-> any)]) any]{

Within the dynamic extent of a call to @racket[call-as-atomic], calls
@racket[thunk] in non-atomic mode. Beware that the current thread may
be suspended or terminated by other threads during the execution of
@racket[thunk].

When used not in the dynamic extent of a call to
@racket[call-as-atomic], @racket[call-as-nonatomic] raises
@racket[exn:fail:contract].}


@defproc[(in-atomic-mode?) boolean?]{

Returns @racket[#t] when in @tech{atomic mode} (within the current
@tech[#:doc reference.scrbl]{place}), @racket[#f] otherwise.}
