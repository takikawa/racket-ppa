#lang scribble/doc
@(require scribble/struct "mz.rkt" (for-label racket/async-channel))

@title[#:tag "sync"]{Events}

@section-index["select"]
@section-index["poll"]

A @deftech{synchronizable event} (or just @defterm{event} for short)
works with the @racket[sync] procedure to coordinate synchronization
among threads. Certain kinds of objects double as events, including
ports and threads. Other kinds of objects exist only for their use as
events.

At any point in time, an event is either @deftech{ready for
synchronization}, or it is not; depending on the kind of event and how
it is used by other threads, an event can switch from not ready to
ready (or back), at any time.  If a thread synchronizes on an event
when it is ready, then the event produces a particular
@deftech{synchronization result}.

Synchronizing an event may affect the state of the event. For example,
when synchronizing a semaphore, then the semaphore's internal count is
decremented, just as with @racket[semaphore-wait]. For most kinds of
events, however (such as a port), synchronizing does not modify the
event's state.

Racket values that act as @tech{synchronizable events} include
@tech{semaphores}, @tech{channels}, @tech{asynchronous channels},
@tech{ports}, @tech{TCP listeners}, @tech{threads},
@tech{subprocess}es, @tech{will executors}, and @tech{custodian
box}es. Libraries can define new synchronizable events, especially
though @racket[prop:evt].

@;------------------------------------------------------------------------

@defproc[(evt? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{synchronizable event},
@racket[#f] otherwise.}


@defproc[(sync [evt evt?] ...+) any]{

Blocks as long as none of the @tech{synchronizable events}
@racket[evt]s are ready, as defined above.

When at least one @racket[evt] is ready, its @tech{synchronization
result} (often @racket[evt] itself) is returned.  If multiple
@racket[evt]s are ready, one of the @racket[evt]s is chosen
pseudo-randomly for the result; the
@racket[current-evt-pseudo-random-generator] parameter sets the
random-number generator that controls this choice.}


@defproc[(sync/timeout [timeout (or/c #f (and/c real? (not/c negative?)) (-> any))]
                       [evt evt?] ...+) 
          any]{

Like @racket[sync] if @racket[timeout] is @racket[#f]. If
@racket[timeout] is a real number, then the result is @racket[#f]
if @racket[timeout] seconds pass without a
successful synchronization. If @racket[timeout] is a procedure, then
it is called in tail position if polling the @racket[evt]s discovers
no ready events.

A zero value for @racket[timeout] is equivalent to @racket[(lambda ()
#f)]. In either case, each @racket[evt] is checked at least once
before returning @racket[#f] or calling @racket[timeout].

See also @racket[alarm-evt] for an alternative timeout mechanism.}


@defproc[(sync/enable-break [evt evt?] ...+) any]{

Like @racket[sync], but breaking is enabled (see
@secref["breakhandler"]) while waiting on the @racket[evt]s. If
breaking is disabled when @racket[sync/enable-break] is called, then
either all @racket[evt]s remain unchosen or the @racket[exn:break]
exception is raised, but not both.}


@defproc[(sync/timeout/enable-break [timeout (or/c #f (and/c real? (not/c negative?)) (-> any))]
                                    [evt evt?] ...+) 
         any]{

Like @racket[sync/enable-break], but with a timeout as for @racket[sync/timeout].}


@defproc[(choice-evt [evt evt?] ...) evt?]{

Creates and returns a single event that combines the
@racket[evt]s. Supplying the result to @racket[sync] is the same as
supplying each @racket[evt] to the same call.

That is, an event returned by @racket[choice-evt] is @tech{ready for
synchronization} when one or more of the @racket[_evt]s supplied to
@racket[choice-evt] are @tech{ready for synchronization}. If the
choice event is chosen, one of its ready @racket[_evt]s is chosen
pseudo-randomly, and the @tech{synchronization result} is the chosen
@racket[_evt]'s @tech{synchronization result}.}


@defproc[(wrap-evt [evt (and/c evt? (not/c handle-evt?))]
                   [wrap (any/c . -> . any)]) 
         evt?]{

Creates an event that is @tech{ready for synchronization} when
@racket[evt] is @tech{ready for synchronization}, but whose
@tech{synchronization result} is determined by applying @racket[wrap]
to the @tech{synchronization result} of @racket[evt].

The call to @racket[wrap] is
@racket[parameterize-break]ed to disable breaks initially. The
@racket[evt] cannot be an event created by @racket[handle-evt] or any
combination of @racket[choice-evt] involving an event from
@racket[handle-evt].}


@defproc[(handle-evt [evt (and/c evt? (not/c handle-evt?))]
                     [handle (any/c . -> . any)]) 
         handle-evt?]{

Like @racket[wrap], except that @racket[handle] is called in @tech{tail
position} with respect to the synchronization request, and without
breaks explicitly disabled.}

@defproc[(guard-evt [generator (-> evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator.

An event @racket[_guard] returned by @racket[guard-evt] generates a
new event every time that @racket[_guard] is used with @racket[sync]
(or whenever it is part of a choice event used with @racket[sync],
etc.). The generated event is the result of calling
@racket[_generator] when the synchronization begins; if
@racket[_generator] returns a non-event, then @racket[_generator]'s
result is replaced with an event that is @tech{ready for
synchronization} and whose @tech{synchronization result} is
@racket[_guard].}

@defproc[(nack-guard-evt [generator (evt? . -> . evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator.

An event @racket[_nack-guard] returned by @racket[nack-guard-evt]
applied to @racket[_proc] generates a new event every time that
@racket[_nack-guard] is used with @racket[sync] (or whenever it is
part of a choice event used with @racket[sync], etc.). The generated
event is the result of calling @racket[_generator] with a NACK (``negative
acknowledgment'') event when the synchronization begins; if
@racket[_generator] returns a non-event, then @racket[_generator]'s result is
replaced with an event that is ready and whose result is
@racket[_nack-guard].

If the event from @racket[_generator] is not ultimately chosen as the
unblocked event, then the NACK event supplied to @racket[_generator]
becomes @tech{ready for synchronization} with a @|void-const| value.
This NACK event becomes @tech{ready for synchronization} when the
event is abandoned when either some other event is chosen, the
synchronizing thread is dead, or control escapes from the call to
@racket[sync] (even if @racket[_nack-guard]'s @racket[_generator] has
not yet returned a value). If the event returned by
@racket[_generator] is chosen, then the NACK event never becomes
@tech{ready for synchronization}.}


@defproc[(poll-guard-evt [generator (boolean? . -> . evt?)]) evt?]{

Creates a value that behaves as an event, but that is actually an
event generator.

An event @racket[_poll-guard]returned by @racket[poll-guard-evt]
generates a new event every time that @racket[_poll-guard] is used
with @racket[sync] (or whenever it is part of a choice event used with
@racket[sync], etc.). The generated event is the result of calling
@racket[_generator] with a boolean: @racket[#t] if the event will be
used for a poll, @racket[#f] for a blocking synchronization.

If @racket[#t] is supplied to @racket[_generator], if breaks are
disabled, if the polling thread is not terminated, and if polling the
resulting event produces a @tech{synchronization result}, the event
will certainly be chosen for its result.}


@defthing[always-evt evt?]{A constant event that is always @tech{ready
for synchronization}, with itself as its @tech{synchronization result}.}


@defthing[never-evt evt?]{A constant event that is never @tech{ready
for synchronization}.}


@defproc[(system-idle-evt) evt?]{

Returns an event that is @tech{ready for synchronization} when the
system is otherwise idle: if the result event were replaced by
@racket[never-evt], no thread in the system would be available to run.
In other words, all threads must be suspended or blocked on events
with timeouts that have not yet expired. The system-idle event's
@tech{synchronization result} is @|void-const|. The result of the
@racket[system-idle-evt] procedure is always the same event.}


@defproc[(alarm-evt [msecs nonnegative-number?]) evt]{

Returns a @tech{synchronizable event} that is not @tech{ready for synchronization} when
@racket[(current-inexact-milliseconds)] would return a value that is
less than @racket[msecs], and it is @tech{ready for synchronization} when
@racket[(current-inexact-milliseconds)] would return a value that is
more than @racket[msecs]. @ResultItself{alarm event}.}


@defproc[(handle-evt? [evt evt?]) boolean?]{

Returns @racket[#t] if @racket[evt] was created by @racket[handle-evt]
or by @racket[choice-evt] applied to another event for which
@racket[handle-evt?] produces @racket[#t]. Such events are illegal as
an argument to @racket[handle-evt] or @racket[wrap-evt], because they
cannot be wrapped further. For any other event, @racket[handle-evt?]
produces @racket[#f], and the event is a legal argument to
@racket[handle-evt] or @racket[wrap-evt] for further wrapping.}

@;------------------------------------------------------------------------
@defthing[prop:evt struct-type-property?]{

A @tech{structure type property} that identifies structure types whose
 instances can serve as @tech{synchronizable events}. The property value can
 be any of the following:

@itemize[
 
 @item{An event @racket[_evt]: In this case, using the structure as an
 event is equivalent to using @racket[_evt].}

 @item{A procedure @racket[_proc] of one argument: In this case, the
 structure is similar to an event generated
 by @racket[guard-evt], except that the would-be guard
 procedure @racket[_proc] receives the structure as an argument, instead
 of no arguments.}

 @item{An exact, non-negative integer between @racket[0] (inclusive)
 and the number of non-automatic fields in the structure type
 (exclusive, not counting supertype fields): The integer identifies a
 field in the structure, and the field must be designated as
 immutable. If the field contains an object or an event-generating
 procedure of one argument, the event or procedure is used as
 above. Otherwise, the structure acts as an event that is never
 ready.}

]

Instances of a structure type with the @racket[prop:input-port] or
@racket[prop:output-port] property are also @tech{synchronizable events} by virtue
of being a port. If the structure type has more than one of
@racket[prop:evt], @racket[prop:input-port], and
@racket[prop:output-port], then the @racket[prop:evt] value (if any)
takes precedence for determining the instance's behavior as an event,
and the @racket[prop:input-port] property takes precedence over
@racket[prop:output-port] for synchronization.

@examples[
(define-struct wt (base val)
               #:property prop:evt (struct-field-index base))

(define sema (make-semaphore))
(sync/timeout 0 (make-wt sema #f))
(semaphore-post sema)
(sync/timeout 0 (make-wt sema #f))
(semaphore-post sema)
(sync/timeout 0 (make-wt (lambda (self) (wt-val self)) sema))
(semaphore-post sema)
(define my-wt (make-wt (lambda (self) (wrap-evt
                                       (wt-val self)
                                       (lambda (x) self)))
                       sema))
(sync/timeout 0 my-wt)
(sync/timeout 0 my-wt)
]}


@defparam[current-evt-pseudo-random-generator generator pseudo-random-generator?]{

A @tech{parameter} that determines the pseudo-random number generator used by
@racket[sync] for events created by @racket[choice-evt].}
