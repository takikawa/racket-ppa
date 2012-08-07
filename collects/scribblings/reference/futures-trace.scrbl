#lang scribble/doc 
@(require "mz.rkt" (for-label racket/future future-visualizer/trace)) 

@title[#:tag "futures-trace"]{Futures Tracing} 

@guideintro["effective-futures"]{the future visualizer}

@defmodule[future-visualizer/trace]

The @deftech{futures trace} module exposes low-level information about 
the execution of parallel programs written using @racket[future].   
  
@deftogether[(
  @defform[(trace-futures e ...)] 
  @defproc[(trace-futures-thunk [thunk (-> any)]) (listof indexed-future-event?)]
)]{
 The @racket[trace-futures] macro and @racket[trace-futures-thunk] function 
 track the execution of a program using futures and return the program 
 trace as a list of @racket[indexed-future-event] structures.
 
 This program:
 
 @racketblock[ 
    (require racket/future 
             future-visualizer/trace) 
     
    (trace-futures  
     (let ([f (future (lambda () ...))]) 
       ... 
       (touch f)))
 ]
 
 Is equivalent to:
 
 @racketblock[ 
    (require racket/future 
             future-visualizer/trace) 
                                      
    (start-future-tracing!) 
    (let ([f (future (lambda () ...))]) 
      ... 
      (touch f)) 
    (stop-future-tracing!)
    (timeline-events)
 ]
}

@deftogether[(
  @defproc[(start-future-tracing!) void?]
  @defproc[(stop-future-tracing!) void?]
  @defproc[(timeline-events) (listof indexed-future-event?)]
)]{
 The @racket[start-future-tracing!] procedure enables the collection 
 of future-related execution data.  This function should be called immediately 
 prior to executing code the programmer wishes to profile. 
 
 The @racket[stop-future-tracing!] procedure must be used to indicate the 
 end of code the programmer wishes to trace.  Tracing works by simply using a 
 log receiver to record all future-related log events; this procedure logs a 
 special message that is well-known to the log receiver to mean 'stop recording'.
 
 The @racket[timeline-events] procedure returns the program trace as 
 a list of @racket[indexed-future-event] structures. 
}
  
@defstruct[indexed-future-event ([index exact-nonnegative-integer?] 
                                 [event future-event?])]{
  Represents an individual log message in a program trace.  Because multiple 
  @racket[future-event] structures may contain identical timestamps, the 
  @racket[index] field ranks them in the order in which they were recorded 
  in the log output.
}

@; ------------------------------------------------------------

@section[#:tag "future-logging"]{Future Performance Logging}

Racket traces use logging (see @secref["logging"]) extensively to
report information about how futures are evaluated.  Logging output is
useful for debugging the performance of programs that use futures.

Though textual log output can be viewed directly (or retrieved in 
code via @racket[trace-futures]), it is much  
easier to use the graphical profiler tool provided by 
@racketmodname[future-visualizer].  

In addition to its string message, each event logged for a future has
a data value that is an instance of a @racket[future-event]
@tech{prefab} structure:

@defstruct[future-event ([future-id (or exact-nonnegative-integer? #f)]
                         [proc-id exact-nonnegative-integer?] 
                         [action symbol?] 
                         [time-id real?] 
                         [prim-name (or symbol? #f)] 
                         [user-data (or #f symbol? exact-nonnegative-integer?)]) 
                        #:prefab]

The @racket[future-id] field is an exact integer that identifies a
future, or it is @racket[#f] when @racket[action] is
@racket['missing]. The @racket[future-id] field is particularly useful
for correlating logged events.

The @racket[proc-id] fields is an exact, non-negative integer that
identifies a parallel process. Process 0 is the main Racket process,
where all expressions other than future thunks evaluate.

The @racket[time-id] field is an inexact number that represents time in
the same way as @racket[current-inexact-milliseconds].

The @racket[action] field is a symbol:

@itemlist[

 @item{@racket['create]: a future was created.}

 @item{@racket['complete]: a future's thunk evaluated successfully, so
       that @racket[touch] will produce a value for the future
       immediately.}

 @item{@racket['start-work] and @racket['end-work]: a particular
       process started and ended working on a particular future.}

 @item{@racket['start-0-work]: like @racket['start-work], but for a
       future thunk that for some structural reason could not be
       started in a process other than 0 (e.g., the thunk requires too
       much local storage to start).}

 @item{@racket['start-overflow-work]: like @racket['start-work], where
       the future thunk's work was previously stopped due to an
       internal stack overflow.}

 @item{@racket['sync]: blocking (processes other than 0) or initiation
       of handing (process 0) for an ``unsafe'' operation in a future
       thunk's evaluation; the operation must run in process 0.}

 @item{@racket['block]: like @racket['sync], but for a part of
       evaluation that must be delayed until the future is
       @racket[touch]ed, because the evaluation may depend on the
       current continuation.}

 @item{@racket['touch] (never in process 0): like @racket['sync] or
       @racket['block], but for a @racket[touch] operation within a
       future thunk.}

 @item{@racket['overflow] (never in process 0): like @racket['sync] or
       @racket['block], but for the case that a process encountered an
       internal stack overflow while evaluating a future thunk.}

 @item{@racket['result] or @racket['abort]: waiting or handling for
       @racket['sync], @racket['block], or @racket['touch] ended with
       a value or an error, respectively.}

 @item{@racket['suspend] (never in process 0): a process blocked by
       @racket['sync], @racket['block], or @racket['touch] abandoned
       evaluation of a future; some other process may pick up the
       future later.}

 @item{@racket['touch-pause] and @racket['touch-resume] (in process 0,
       only): waiting in @racket[touch] for a future whose thunk is
       being evaluated in another process.}

 @item{@racket['missing]: one or more events for the process were lost
       due to internal buffer limits before they could be reported,
       and the @racket[time-id] field reports an upper limit on the time
       of the missing events; this kind of event is rare.}

]

Assuming no @racket['missing] events, then @racket['start-work],
@racket['start-0-work], @racket['start-overflow-work] is always paired with @racket['end-work];
@racket['sync], @racket['block], and @racket['touch] are always paired
with @racket['result], @racket['abort], or @racket['suspend]; and
@racket['touch-pause] is always paired with @racket['touch-resume].

In process 0, some event pairs can be nested within other event pairs:
@racket['sync], @racket['block], or @racket['touch] with
@racket['result] or @racket['abort]; and @racket['touch-pause] with
@racket['touch-resume].

An @racket['block] in process 0 is generated when an unsafe operation 
is handled.  This type of event will contain a symbol in the 
@racket[unsafe-op-name] field that is the name of the operation.  In all 
other cases, this field contains @racket[#f].

The @racket[prim-name] field will always be @racket[#f] unless the event occurred 
on process 0 and its @racket[action] is either @racket['block] or @racket['sync].  If 
these conditions are met, @racket[prim-name] will contain the name 
of the Racket primitive which required the future to synchronize with the runtime 
thread (represented as a symbol).

The @racket[user-data] field may take on a number of different 
values depending on both the @racket[action] and @racket[prim-name] fields:

@itemlist[
          
 @item{@racket['touch] on process 0: contains the integer ID of the future 
        being touched.}
  
 @item{@racket['sync] and @racket[prim-name] = @racket[|allocate memory|]: 
        The size (in bytes) of the requested allocation.}
 
 @item{@racket['sync] and @racket[prim-name] = @racket[|jit_on_demand|]: 
        The runtime thread is performing a JIT compilation on behalf of the 
        future @racket[future-id].  The field contains the name of the function 
        being JIT compiled (as a symbol).}
 
 @item{@racket['create]: A new future was created.  The field contains the integer ID 
        of the newly created future.}
                                     
 ]

@; ----------------------------------------------------------------------                                                       