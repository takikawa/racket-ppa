#lang scribble/doc
@(require scribble/manual
  (for-label scheme
    scheme/class
    (except-in scheme/gui/base send-event)
    (only-in frtime undefined undefined? behavior? event? signal? seconds milliseconds never-e
	     new-cell set-cell! event-receiver send-event
	     map-e ==> filter-e =#> accum-e accum-b collect-e collect-b changes hold merge-e
	     value-now delay-by integral derivative once-e switch when-e lift-strict)
    (only-in "gui/fred.ss" ft-frame% ft-message% ft-button% ft-check-box% ft-slider%
	     ft-text-field% ft-radio-box% ft-choice% ft-list-box%)))

@title[#:tag "frtime"]{@bold{FrTime}: A Language for Reactive Programs}

The @scheme[frtime.ss] language supports declarative construction of
reactive systems in a syntax very similar to that of MzScheme.  To
interact with FrTime, select FrTime from the "Choose Language" menu.
You can also make FrTime the language for a module:

@schemeblock[
(module <module-name> frtime
   <module-body>)]

@defmodulelang[frtime]

@section{Primitives}

@defthing[undefined any/c]{stands for an undefined value.}

@defproc[(undefined? [val any/c]) boolean?]{return @scheme[#t] iff
@scheme[val] is @scheme[undefined].}

@defproc[(behavior? [val any/c]) boolean?]{returns @scheme[#t] iff
@scheme[val] is a behavior (a time-varying value whose current value can be
projected at any time).}

@defproc[(event? [val any/c]) boolean?]{returns @scheme[#t] iff
@scheme[val] is an event (a time-varying stream of values that can occur
at arbitrary times).}

@defproc[(signal? [val any/c]) boolean?]{returns @scheme[#t] iff val
is a signal.  @scheme[(signal? v)] is equivalent to @scheme[(or
(behavior? v) (event? v))].}

@defthing[seconds behavior?]{updates approximately once per second
with the value of @scheme[(current-seconds)].}

@defthing[milliseconds behavior?]{updates frequently with the value of
@scheme[(current-inexact-milliseconds)].}

@defthing[never-e event?]{is an event that never occurs.}

@section{Defining Custom Input Signals}

@defproc[(new-cell [init-expr signal? undefined]) signal? ]{returns a
signal whose values initially track that of @scheme[init-expr], but
that may be rewired to a different signal by @scheme[set-cell!].}

@defproc[(set-cell! [cell signal?] [val signal?]) void?]{rewires
@scheme[cell] (which must have been created by @scheme[new-cell]) to
take on the value(s) of @scheme[val].}

@defproc[(event-receiver) event?]{returns an event stream that can be
triggered imperatively by @scheme[send-event].}

@defproc[(send-event [rcvr event?] [val any/c]) void?]{emits
@scheme[val] on @scheme[rcvr] (which must have been created by
@scheme[event-receiver]).}

@section{Signal-Processing Procedures}

@defproc[(value-now [val any/c]) any/c]{projects the current value of
a behavior or constant.}

@defproc[(delay-by [val behavior?] [duration number?])
behavior?]{delays @scheme[val] by @scheme[duration] milliseconds.}

@defproc[(integral [val (or/c number? behavior?)]) behavior?]{computes
a numeric approximation of the integral of @scheme[val] with respect
to time (measured in milliseconds).}

@defproc[(derivative [val behavior?]) behavior?]{computes a numeric
approximation of the derivative of @scheme[val] with respect to time.}

@deftogether[(
@defproc[(map-e [proc (-> any/c any)] [ev event?]) event?]
@defproc[([ev event?] . ==> . [proc (-> any/c any)]) event?]
)]{returns an event stream that fires whenever @scheme[ev] fires, whose
values are transformed by application of @scheme[proc].}

@deftogether[(
@defproc[(filter-e [pred (-> any/c boolean?)] [ev event?]) event?]
@defproc[([ev event?] . =#> . [pred (-> any/c boolean?)]) event?])]{
returns an event stream that passes through only the values from
@scheme[ev] for which @scheme[pred] returns @scheme[#t].}

@defproc[(merge-e [ev event?] ...) event?]{merges all of the input
event sources into a single event source.}

@defproc[(once-e [ev event?]) event?]{returns an event source that
carries only the first occurrence of @scheme[ev].  (The rest are
filtered out.)}

@defproc[(changes [val behavior?]) event?]{returns an event source
that occurs each time the argument behavior changes.  The value of the
occurrence is the behavior's new value.}

@defproc[(hold [ev event?] [init any/c undefined]) behavior?]{
constructs a behavior that starts out as @scheme[init] and then
takes on the last value produced by @scheme[ev]}

@defproc[(switch [ev event?] [init behavior? undefined])
behavior?]{returns a behavior that starts as @scheme[init].  Each time
@scheme[ev] yields a (potentially time-varying) value, the behavior
switches to that value.}

@defproc[(accum-e [ev event?] [init any/c]) event?]{constructs an event source
by accumulating changes (carried by the given event source) over an
initial value.}

@defproc[(accum-b [ev event?] [init any/c]) behavior?]{combines functionality
from @scheme[accum-e] and @scheme[hold] to construct a behavior.
@scheme[(accum-b ev init)] is equivalent to @scheme[(hold init
(accum-e ev init))].}

@defproc[(collect-e [ev event?] [init any/c] [proc (-> any/c any/c
any)]) event?]{is similar to @scheme[accum-e], except the transformer
function is fixed and is applied to the event occurrence and the
current accumulator (in that order).}

@defproc[(collect-b [ev event?] [init any/c] [proc (-> any/c any/c any)]) behavior?]{is similar to
@scheme[collect-e] in the same way as @scheme[accum-b] is similar to
@scheme[accum-e].}

@defproc[(when-e [val behavior?]) event?]{returns an event stream that
carries an occurrence each time @scheme[val] changes from @scheme[#f] to
anything else.}
  
@defproc[(lift-strict [proc (-> [arg any/c] ... any)] [val any/c] ...)
any]{provides a mechanism for applying ordinary Scheme primitives to
behaviors.  If any of the @scheme[val]s are behaviors, returns a
behavior whose current value is always equal to @scheme[(proc
(value-now arg) ...)].  In FrTime, many MzScheme primitives are
implicitly lifted.}

The following forms allow importation of lifted procedures that aren't
included in the basic FrTime language.

@schemeblock[
(require (lifted module-spec proc-name ...) ...)
(require (lifted:nonstrict module-spec proc-name ...) ...)
]

@section[#:tag "fred"]{Fred: Functional Reactive Wrapper around MrEd}

@defmodule[frtime/gui/fred]

@defclass[ft-frame% frame% (top-level-window<%>)]{

@defconstructor[([label (or/c label-string? behavior?)]
                 [parent (or/c (is-a?/c frame%) false/c) #f]
                 [width (or/c (integer-in 0 10000) false/c) #f]
                 [height (or/c (integer-in 0 10000) false/c) #f]
                 [x (or/c (integer-in -10000 10000) false/c) #f]
                 [y (or/c (integer-in -10000 10000) false/c) #f]
                 [style (listof (one-of/c 'no-resize-border 'no-caption 
                                          'no-system-menu 'hide-menu-bar 
                                          'mdi-parent 'mdi-child
                                          'toolbar-button 'float 'metal)) null]
                 [enabled any/c #t]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t]
		 [shown any/c #f])]{
The constructor arguments are as in @scheme[frame%], except that @scheme[shown]
@scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.}
}

@defclass[ft-message% message% (control<%>)]{

@defconstructor[([label (or/c label-string? behavior? (is-a?/c bitmap%) 
                              (or-of/c 'app 'caution 'stop))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'deleted)) null]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled (or/c any/c behavior?) #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{The constructor
arguments are the same as in @scheme[message%], except that
@scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.}}

@defclass[ft-button% button% (control<%>)]{

@defconstructor[([label (or/c label-string? behavior (is-a?/c bitmap%))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (one-of/c 'border 'deleted) null]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f])]{The constructor
arguments are the same as in @scheme[message%], except that
@scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.}

@defmethod[(get-value-e) event?]{returns an event stream that yields a
value whenever the user clicks the button.}
}

@defclass[ft-check-box% check-box% (control<%>)]{

@defconstructor[([label (or/c label-string? behavior? (is-a?/c bitmap%))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'deleted)) null]
                 [value any/c #f]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
		 [value-set event? never-e])]{The constructor arguments
are the same as in @scheme[check-box%], except that @scheme[label],
@scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the check box's state to that of the event value.}

@defmethod[(get-value-b) behavior?]{returns a value that always reflects
the current state of the check box.}
}

@defclass[ft-slider% slider% (control<%>)]{

@defconstructor[([label (or/c label-string? behavior? false/c)]
                 [min-value (integer-in -10000 10000)]
                 [max-value (integer-in -10000 10000)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [init-value (integer-in -10000 10000) min-value]
                 [style (listof (one-of/c 'horizontal 'vertical 'plain 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(horizontal)]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c (memq 'horizontal style)]
                 [stretchable-height any/c (memq 'vertical style)]
		 [value-set event? never-e])]{The constructor arguments
are the same as in @scheme[check-box%], except that @scheme[label],
@scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the slider's state to that of the event value.}

@defmethod[(get-value-b) behavior?]{returns a value that always reflects
the current state of the slider.}
}

@defclass[ft-text-field% text-field% (control<%>)]{

@defconstructor[([label (or/c label-string? false/c)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [init-value string? ""]
                 [style (listof (one-of/c 'single 'multiple 'hscroll 'password 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(single)]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c (memq 'multiple style)]
		 [value-set event? never-e])]{The
constructor arguments are the same as in @scheme[check-box%], except
that @scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the text field's state to that of the event
value.}

@defmethod[(get-value-b) behavior?]{returns a value that always reflects
the current state of the text field.}
}

@defclass[ft-radio-box% radio-box% (control<%>)]{

@defconstructor[([label (or/c label-string? behavior? false/c)]
                 [choices (or/c (listof label-string?) (listof (is-a?/c bitmap%)))]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'horizontal 'vertical 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(vertical)]
                 [selection exact-nonnegative-integer? 0]
                 [font (is-a?/c font%) normal-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
		 [value-set event? never-e])]{The
constructor arguments are the same as in @scheme[check-box%], except
that @scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the text field's state to that of the event
value.}
@defmethod[(get-selection-b) behavior?]{returns a value that always reflects
the currently selected element in the radio box.}
}

@defclass[ft-choice% choice% (control<%>)]{
@defconstructor[([label (or/c label-string? false/c)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'horizontal-label 'vertical-label
                                          'deleted)) 
                   null]
                 [selection exact-nonnegative-integer? 0]
                 [font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #f]
                 [stretchable-height any/c #f]
		 [value-set event? never-e])]{The
constructor arguments are the same as in @scheme[check-box%], except
that @scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the text field's state to that of the event
value.}
@defmethod[(get-selection-b) behavior?]{returns a value that always reflects
the currently selected element in the choice control.}
}

@defclass[ft-list-box% list-box% (control<%>)]{
@defconstructor[([label (or/c label-string? false/c)]
                 [choices (listof label-string?)]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'single 'multiple 'extended 
                                          'vertical-label 'horizontal-label 
                                          'deleted)) 
                        '(single)]
                 [selection (or/c exact-nonnegative-integer? false/c) #f]
                 [font (is-a?/c font%) @scheme[view-control-font]]
                 [label-font (is-a?/c font%) @scheme[normal-control-font]]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t]
		 [value-set event? never-e])]{The
constructor arguments are the same as in @scheme[check-box%], except
that @scheme[label], @scheme[enabled], @scheme[stretchable-width], and
@scheme[stretchable-height] may be time-varying.  Also, any occurrence
on @scheme[value-set] sets the text field's state to that of the event
value.}
@defmethod[(get-selection-b) behavior?]{returns a value that always reflects
the primary selection in the list box.}
@defmethod[(get-selections-b) behavior?]{returns a value that always reflects
the current set of selected elements in the list box.}
}

@section{Graphical Demo Programs}

TODO: document the animation library itself!

To run the following animation/GUI demos, simply set the language
level to FrTime, open the corresponding file, and Execute.  See the
demo source code for more information.

@scheme[orbit-mouse.ss] : A collection of balls that move in circles
around the mouse pointer.

@scheme[piston.ss] : Simulation of a piston/cylinder.

@scheme[rotation.ss] : Balls moving in circles.

@scheme[delay-mouse.ss] : A trail of balls following the mouse.

@scheme[ball-on-string.ss] : A ball chasing the mouse.

@scheme[pong.ss] : A simple pong/air-hockey game.  The left paddle moves with
numeric keypad; the right paddle moves with the mouse.  The 'r' key
resets the score.

@scheme[pizza.ss] : A simple "pizza ordering" user interface based on an HtDP
exercise.

@scheme[calculator.ss] : A simple calculator interface, also based on an HtDP
exercise except that the result updates continuously as the arguments
and operator change.

The next three animation examples are courtesy of Robb Cutler:

@scheme[analog-clock.ss] : An animated real-time clock.  A slider adjusts the radius
of the face.  Click and drag to move the face around.

@scheme[growing-points.ss] : A field of points that grow as the mouse approaches.

@scheme[needles.ss] : A field of needles that point at the mouse.

