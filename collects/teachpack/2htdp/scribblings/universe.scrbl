#lang scribble/doc

@(require scribble/manual "shared.ss" 
          (for-label scheme
	  	     (only-in lang/htdp-beginner check-expect)
		     teachpack/2htdp/universe
		     teachpack/htdp/image))
@(require scribble/struct)

@(define (table* . stuff)
  ;; (list paragraph paragraph) *-> Table
   (define (flow* x) (make-flow (list x)))
   (make-blockquote #f
     (list 
       (make-table (make-with-attributes 'boxed
		     '((cellspacing . "6")))
	 ;list
	 (map (lambda (x) (map flow* x)) stuff)
	 #;(map flow* (map car stuff))
	 #;(map flow* (map cadr stuff))))))

@; -----------------------------------------------------------------------------

@teachpack["universe"]{Worlds and the Universe}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/universe #:use-sources (teachpack/htdp/image)]

@;{FIXME: the following paragraph uses `defterm' instead of `deftech',
   because the words "world" and "universe" are used as datatypes, and
   datatypes are currently linked as technical terms --- which is a hack.
   Fix the paragraph when we have a better way to link datatype names.}

This @tt{universe.ss} teachpack implements and provides the functionality
 for creating interactive, graphical programs that consist of plain
 mathematical functions. We refer to such programs as @deftech{world}
 programs. In addition, world programs can also become a part of a
 @deftech{universe}, a collection of worlds that can exchange messages.

The purpose of this documentation is to give experienced Schemers and HtDP
 teachers a concise overview for using the library. The first part of the
 documentation focuses on @tech{world} programs. Section
 @secref["world-example"] presents an illustration of how to design such
 programs for a simple domain; it is suited for a novice who knows how to
 design conditional functions for enumerations, intervals, and unions. The
 second half of the documentation focuses on "universe" programs: how it is
 managed via a server, how @tech{world} programs register with the server,
 etc. The last two sections show how to design a simple universe of two
 communicating worlds.

@emph{Note}: For a quick and educational introduction to just worlds, see
 @link["http://www.ccs.neu.edu/home/matthias/HtDP/Prologue/book.html"]{How
 to Design Programs, Second Edition: Prologue}. As of August 2008, we also
 have a series of projects available as a small booklet on
 @link["http://world.cs.brown.edu/"]{How to Design Worlds}.

@; -----------------------------------------------------------------------------

@section[#:tag "basics"]{Basics}

The teachpack assumes working knowledge of the basic image manipulation
 primitives and supports several functions that require a special kind of
 image, called a @deftech{scene}, which is an image whose pinholes are at
 position @math{(0, 0)}. For example, the teachpack displays only
 @tech{scene}s in its canvas. 

@defproc[(scene? [x any/c]) boolean?]{
 determines whether @scheme[x] is a @tech{scene}.}

@defproc[(empty-scene [width natural-number/c]
                      [height natural-number/c])
         scene?]{
 creates a plain white, @scheme[width] x @scheme[height] @tech{scene}.}

@defproc[(place-image [img image?] [x number?] [y number?]
                      [s scene?])
         scene?]{
 creates a scene by placing @scheme[img] at
 @math{(@scheme[x], @scheme[y])} into @scheme[s];
 @math{(@scheme[x], @scheme[y])} are computer graphics coordinates,
 i.e., they count right and down from the upper-left corner.}

@; -----------------------------------------------------------------------------
@section[#:tag "simulations"]{Simple Simulations}

The simplest kind of animated @tech{world} program is a time-based
 simulation, which is a series of scenes.  The programmer's task is to
 supply a function that creates a scene for each natural number. By handing
 this function to the teachpack displays the simulation. 

@defproc[(run-simulation [create-image (-> natural-number/c scene)])
         true]{

 opens a canvas and starts a clock that tick 28 times per second.  Every
 time the clock ticks, DrScheme applies @scheme[create-image] to the
 number of ticks passed since this function call. The results of these
 function calls are displayed in the canvas. The simulation runs until you
 click the @tt{Stop} button in DrScheme or close the window. At that
 point, @scheme[run-simulation] returns the number of ticks that have
 passed. 
}

Example:
@schemeblock[
(define (create-UFO-scene height)
  (place-image UFO 50 height (empty-scene 100 100)))

(define UFO
  (overlay (circle 10 'solid 'green)
           (rectangle 40 4 'solid 'green)))

(run-simulation create-UFO-scene)
]

@;-----------------------------------------------------------------------------
@section[#:tag "interactive"]{Interactions}

The step from simulations to interactive programs is relatively
 small. Roughly speaking, a simulation designates one function,
 @scheme[_create-image], as a handler for one kind of event: clock ticks.  In
 addition to clock ticks, @tech{world} programs can also deal with two
 other kinds of events: keyboard events and mouse events. A keyboard event
 is triggered when a computer user presses or releases a key on the
 keyboard. Similarly, a mouse event is the  movement of the mouse, a click
 on a mouse button, the crossing of a boundary by a mouse movement, etc.

Your program may deal with such events via the @emph{designation} of
 @emph{handler} functions.  Specifically, the teachpack provides for the
 installation of three event handlers: @scheme[on-tick], @scheme[on-key],
 and @scheme[on-mouse]. In addition, a @tech{world} program may specify a
 @scheme[draw] function, which is called every time your program should
 visualize the current world, and a @scheme[done] predicate, which is used
 to determine when the @tech{world} program should shut down. 

Each handler function consumes the current state of the @tech{world} and
 optionally a data representation of the event. It produces a new state of
 the @tech{world}. 

The following picture provides an intuitive overview of the workings of a
 @tech{world} program in the form of a state transition diagram. 

@image["nuworld.png"]

 The @scheme[big-bang] form installs @scheme[World_0] as the initial @tech{WorldState}.
 The handlers @scheme[tock], @scheme[react], and @scheme[click] transform
 one world into another one; each time an event is handled, @scheme[done] is
 used to check whether the world is final, in which case the program is
 shut down; and finally, @scheme[draw] renders each world as a scene, which
 is then displayed on an external canvas. 

@deftech{WorldState} : @scheme[any/c]

The design of a world program demands that you come up with a data
 definition of all possible states. We use @tech{WorldState} to refer to
 this collection of data, using a capital W to distinguish it from the
 program.  In principle, there are no constraints on this data
 definition though it mustn't be an instance of the @tech{Package}
 structure (see below). You can even keep it implicit, even if this
 violates the Design Recipe.

@defform/subs[#:id big-bang
              #:literals 
	      (on-tick on-draw on-key on-mouse on-receive 
	        stop-when register record? name)
              (big-bang state-expr clause ...)
              ([clause
		 (on-tick tick-expr)
		 (on-tick tick-expr rate-expr)
		 (on-key key-expr)
		 (on-mouse key-expr)
		 (on-draw draw-expr)
		 (on-draw draw-expr width-expr height-expr)
		 (stop-when stop-expr)	   
		 (record? boolean-expr)
		 (on-receive rec-expr)
		 (register IP-expr)
		 (name name-expr)
		 ])]{

 starts a @tech{world} program in the initial state specified with
 @scheme[state-expr], which must of course evaluate to an element of
 @tech{WorldState}.  Its behavior is specified via the handler functions
 designated in the optional @scheme[spec] clauses, especially how the
 @tech{world} program deals with clock ticks, with key events, with mouse
 events, and eventually with messages from the universe; how it renders
 itself as a scene; when the program must shut down; where to register the
 world with a universe; and whether to record the stream of events. A world
 specification may not contain more than one @scheme[on-tick],
 @scheme[on-draw], or @scheme[register] clause. A @scheme[big-bang]
 expression returns the last world when the stop condition is satisfied
 (see below) or when the programmer clicks on the @tt{Stop} button or
 closes the canvas.
}

@itemize[

@item{
@defform[(on-tick tick-expr)
         #:contracts
	 ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech{WorldState}))])]{

tell DrScheme to call the @scheme[tick-expr] function on the current
world every time the clock ticks. The result of the call becomes the
current world. The clock ticks at the rate of 28 times per second.}}

@item{
@defform/none[(on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech{WorldState}))]
               [rate-expr natural-number/c])]{
tell DrScheme to call the @scheme[tick-expr] function on the current
world every time the clock ticks. The result of the call becomes the
current world. The clock ticks at the rate of @scheme[rate-expr].}}

@item{A @tech{KeyEvent} represents key board events, e.g., keys pressed or
 released. 

@deftech{KeyEvent} : @scheme[string?]

For simplicity, we represent key events with strings, but not all strings
 are key events. The representation of key events comes in distinct
 classes. First, a single-character string is used to signal that the user
 has hit a "regular" key. Some of these one-character strings may look
 unusual:
@itemize[

@item{@scheme[" "] stands for the space bar (@scheme[#\space]);}
@item{@scheme["\r"] stands for the return key (@scheme[#\return]);}
@item{@scheme["\t"] stands for the tab key (@scheme[#\tab]); and}
@item{@scheme["\b"] stands for the backspace key (@scheme[#\backspace]).}

]
 On rare occasions you may also encounter @scheme["\u007F"], which is the
 string representing the delete key (aka rubout). 

Second, some keys have multiple-character string representations. Strings
 with more than one character denotes arrow keys or other special events, 
 starting with the most important: 
@itemize[
@item{@scheme["left"] is the left arrow;}
@item{@scheme["right"] is the right arrow;}
@item{@scheme["up"] is the up arrow;}
@item{@scheme["down"] is the down arrow;}
@item{@scheme["release"] is the event of releasing a key;}
@item{@scheme["start"]}
@item{@scheme["cancel"]}
@item{@scheme["clear"]}
@item{@scheme["shift"]}
@item{@scheme["control"]}
@item{@scheme["menu"]}
@item{@scheme["pause"]}
@item{@scheme["capital"]}
@item{@scheme["prior"]}
@item{@scheme["next"]}
@item{@scheme["end"]}
@item{@scheme["home"]}
@item{@scheme["escape"]}
@item{@scheme["select"]}
@item{@scheme["print"]}
@item{@scheme["execute"]}
@item{@scheme["snapshot"]}
@item{@scheme["insert"]}
@item{@scheme["help"]}
@item{@scheme["numpad0"], 
 @scheme["numpad1"], 
 @scheme["numpad2"], 
 @scheme["numpad3"],
 @scheme["numpad4"],
 @scheme["numpad5"],
 @scheme["numpad6"],
 @scheme["numpad7"],
 @scheme["numpad8"],
 @scheme["numpad9"],
 @scheme["numpad-enter"],
 @scheme["multiply"],
 @scheme["add"],
 @scheme["separator"],
 @scheme["subtract"],
 @scheme["decimal"],
 @scheme["divide"]}
@item{@scheme["f1"],
 @scheme["f2"],
 @scheme["f3"],
 @scheme["f4"],
 @scheme["f5"],
 @scheme["f6"],
 @scheme["f7"],
 @scheme["f8"],
 @scheme["f9"],
 @scheme["f10"],
 @scheme["f11"],
 @scheme["f12"],
 @scheme["f13"],
 @scheme["f14"],
 @scheme["f15"],
 @scheme["f16"],
 @scheme["f17"],
 @scheme["f18"],
 @scheme["f19"],
 @scheme["f20"],
 @scheme["f21"],
 @scheme["f22"],
 @scheme["f23"],
 @scheme["f24"]}
@item{@scheme["numlock"]}
@item{@scheme["scroll"]}
@item{@scheme["wheel-up"]}
@item{@scheme["wheel-down"]}
]

@defproc[(key-event? [x any]) boolean?]{
 determines whether @scheme[x] is a @tech{KeyEvent}}

@defproc[(key=? [x key-event?][y key-event?]) boolean?]{
 compares two @tech{KeyEvent} for equality}

@defform[(on-key change-expr)
         #:contracts
	  ([change-expr (-> (unsyntax @tech{WorldState}) key-event? (unsyntax @tech{WorldState}))])]{
 tell DrScheme to call @scheme[change-expr] function on the current world and a 
 @tech{KeyEvent} for every keystroke the user of the computer makes. The result
 of the call becomes the current world.

 Here is a typical key-event handler: 
@schemeblock[
(define (change w a-key)
  (cond
    [(key=? a-key "left")  (world-go w -DELTA)]
    [(key=? a-key "right") (world-go w +DELTA)]
    [(= (string-length a-key) 1) w] (code:comment "to demonstrate order-free checking")
    [(key=? a-key "up")    (world-go w -DELTA)]
    [(key=? a-key "down")  (world-go w +DELTA)]
    [else w]))
]
 }
 The omitted, auxiliary function @emph{world-go} is supposed to consume a
 world and a number and produces a world.
}

@item{ A @tech{MouseEvent} represents mouse events, e.g., mouse movements
 or mouse clicks, by the computer's user. 
 
@deftech{MouseEvent} : @scheme[(one-of/c "button-down" "button-up" "drag" "move" "enter" "leave")]

All @tech{MouseEvent}s are represented via strings:
@itemize[

@item{@scheme["button-down"] 
 signals that the computer user has pushed a mouse button down;}
@item{@scheme["button-up"]
 signals that the computer user has let go of a mouse button;}
@item{@scheme["drag"]
 signals that the computer user is dragging the mouse;}
@item{@scheme["move"]
 signals that the computer user has moved the mouse;}
@item{@scheme["enter"]
 signals that the computer user has moved the mouse into the canvas area; and}
@item{@scheme["leave"]
 signals that the computer user has moved the mouse out of the canvas area.}
]

@defproc[(mouse-event? [x any]) boolean?]{
 determines whether @scheme[x] is a @tech{KeyEvent}}

@defproc[(mouse=? [x mouse-event?][y mouse-event?]) boolean?]{
 compares two @tech{KeyEvent} for equality}

@defform[(on-mouse clack-expr)
         #:contracts
	 ([clack-expr 
           (-> (unsyntax @tech{WorldState}) natural-number/c natural-number/c (unsyntax @tech{MouseEvent}) (unsyntax @tech{WorldState}))])]{
 tell DrScheme to call @scheme[clack-expr] on the current world, the current
 @scheme[x] and @scheme[y] coordinates of the mouse, and and a
 @tech{MouseEvent} for every (noticeable) action of the mouse by the
 computer user. The result of the call becomes the current world. 

 Note: the computer's software doesn't really notice every single movement
 of the mouse (across the mouse pad). Instead it samples the movements and
 signals most of them.}
}

@item{
 
@defform[(on-draw render-expr)
         #:contracts
         ([render-expr (-> (unsyntax @tech{WorldState}) scene?)])]{ 

 tell DrScheme to call the function @scheme[render-expr] whenever the
 canvas must be drawn. The external canvas is usually re-drawn after DrScheme has
 dealt with an event. Its size is determined by the size of the first
 generated @tech{scene}.}

@defform/none[(on-draw render-expr width-expr height-expr)
              #:contracts
              ([render-expr (-> (unsyntax @tech{WorldState}) scene?)]
	       [width-expr natural-number/c]
               [height-expr natural-number/c])]{ 

 tell DrScheme to use a @scheme[width-expr] by @scheme[height-expr]
 canvas instead of one determine by the first generated @tech{scene}.
}
}

@item{

@defform[(stop-when last-world?)
         #:contracts
         ([last-world? (-> (unsyntax @tech{WorldState}) boolean?)])]{
 tell DrScheme to call the @scheme[last-world?] function whenever the canvas is
 drawn. If this call produces @scheme[true], the world program is shut
 down. Specifically, the  clock is stopped; no more
 tick events, @tech{KeyEvent}s, or @tech{MouseEvent}s are forwarded to
 the respective handlers. The @scheme[big-bang] expression returns this
 last world. 
}}

@item{

@defform[(record? boolean-expr)
         #:contracts
         ([boolean-expr boolean?])]{
 tell DrScheme to record all events and to enable a replay of the entire
 interaction. The replay action also generates one png image per scene and
 an animated gif for the entire sequence.
}}
]

The following example shows that @scheme[(run-simulation create-UFO-scene)] is
a short-hand for three lines of code: 

@(begin
#reader scribble/comment-reader
@schemeblock[ 
(define (create-UFO-scene height) 
  (place-image UFO 50 height (empty-scene 100 100)))

(define UFO
  (overlay (circle 10 'solid 'green)
           (rectangle 40 4 'solid 'green)))

;; (run-simulation create-UFO-scene) is short for: 
(big-bang 0 
          (on-tick add1)
	  (on-draw create-UFO-scene))
])

Exercise: Add a condition for stopping the flight of the UFO when it
reaches the bottom. 

@; -----------------------------------------------------------------------------
@section[#:tag "scenes-and-images"]{Scenes and Images}

For the creation of scenes from the world, use the functions from
@secref["image"].  The teachpack adds the following two functions, which
are highly useful for creating scenes. 

@defproc[(nw:rectangle [width natural-number/c] [height natural-number/c] [solid-or-filled Mode] [c Color]) image?]{
   creates a @scheme[width] by @scheme[height] rectangle, solid or outlined as specified by
   @scheme[solid-or-filled] and colored according to @scheme[c], with a pinhole at the upper left
   corner.}
   
@defproc[(scene+line [s scene?][x0 number?][y0 number?][x1 number?][y1 number?][c Color]) scene?]{
   creates a scene by placing a line of color @scheme[c] from
   @math{(@scheme[x0], @scheme[y0])} to @math{(@scheme[x1],
   @scheme[y1])} using computer graphics coordinates.  In contrast to
   the @scheme[add-line] function, @scheme[scene+line] cuts off those
   portions of the line that go beyond the boundaries of the given
   @scheme[s].}

@; -----------------------------------------------------------------------------
@section[#:tag "world-example"]{A First Sample World} 

This section uses a simple example to explain the design of worlds. The
 first subsection introduces the sample domain, a door that closes
 automatically. The second subsection is about the design of @tech{world}
 programs in general, the remaining subsections implement a simulation of
 the door.

@subsection{Understanding a Door}

Say we wish to design a @tech{world} program that simulates the working of
 a door with an automatic door closer. If this kind of door is locked, you
 can unlock it with a key. While this doesn't open the door per se, it is
 now possible to do so. That is, an unlocked door is closed and pushing at
 the door opens it. Once you have passed through the door and you let go,
 the automatic door closer takes over and closes the door again. When a
 door is closed, you can lock it again.

Here is a diagram that translates our words into a graphical
 representation: 

@image["door-real.png"]

Like the picture of the general workings of a @tech{world} program, this
 diagram displays a so-called ``state machine.'' The three circled words are
 the states that our informal description of the door identified: locked,
 closed (and unlocked), and open. The arrows specify how the door can go
 from one state into another. For example, when the door is open, the
 automatic door closer shuts the door as time passes. This transition is
 indicated by the arrow labeled ``time passes.'' The other arrows represent
 transitions in a similar manner:

@itemize[

@item{``push'' means a person pushes the door open (and let's go);}

@item{``lock'' refers to the act of inserting a key into the lock and turning
it to the locked position; and}

@item{``unlock'' is the opposite of ``lock.''}

]

@; -----------------------------------------------------------------------------
@subsection{Hints on Designing Worlds}

Simulating any dynamic behavior via a @tech{world} program demands two
 different activities. First, we must tease out those portions of our
 domain that change over time or in reaction to actions, and we must
 develop a data representation for this information.  This is what we call
 @tech{WorldState}. Keep in
 mind that a good data definition makes it easy for readers to map data to
 information in the real world and vice versa. For all others aspects of
 the world, we use global constants, including graphical or visual
 constants that are used in conjunction with the rendering operations.

Second, we must translate the actions in our domain---the arrows in the
 above diagram---into interactions with the computer that the universe
 teachpack can deal with. Once we have decided to use the passing of time
 for one aspect, key presses for another, and mouse movements for a third,
 we must develop functions that map the current state of the
 world---represented as data from @tech{WorldState}---into the next state of the
 world. Put differently, we have just created a wish list with three
 handler functions that have the following general contract and purpose
 statements:

@(begin
#reader scribble/comment-reader
(schemeblock
;; tick : WorldState -> WorldState
;; deal with the passing of time 
(define (tick w) ...)

;; click : WorldState @emph{Number} @emph{Number} @tech{MouseEvent} -> WorldState
;; deal with a mouse click at @emph{(x,y)} of kind @emph{me}
;; in the current world @emph{w}
(define (click w x y me) ...)

;; control : WorldState @tech{KeyEvent} -> WorldState
;; deal with a key event @emph{ke}
;; in the current world @emph{w}
(define (control w ke) ...)
))

That is, the contracts of the various handler designations dictate what the
 contracts of our functions are, once we have defined how to represent the
 domain with data in our chosen language. 

A typical program does not use all three of these functions. Furthermore,
 the design of these functions provides only the top-level, initial design
 goal. It often demands the design of many auxiliary functions. The
 collection of all these functions is your @tech{world} program. 

@; -----------------------------------------------------------------------------
@subsection{Simulating a Door: Data}

Our first and immediate goal is to represent the world as data. In this
 specific example, the world consists of our door and what changes about
 the door is whether it is locked, unlocked but closed, or open. We use
 three symbols to represent the three states:

@(begin
#reader scribble/comment-reader
(schemeblock
;; WorldState is one of: 
;; -- @scheme['locked]
;; -- @scheme['closed]
;; -- @scheme['open]
;; interpretation: state of door 
))

Symbols are particularly well-suited here because they directly express
 the state of the door. 

Now that we have a data definition, we must also decide which computer
 interactions should model the various actions on the door.  Our pictorial
 representation of the door's states and transitions, specifically the
 arrow from @tt{open} to @tt{closed} suggests the use of a function that
 simulates time. For the other three arrows, we could use either keyboard
 events or mouse clicks or both. Our solution uses three keystrokes:
 @scheme[#\u] for unlocking the door, @scheme[#\l] for locking it, and
 @scheme[#\space] for pushing it open.  We can express these choices
 graphically by translating the above state-machine diagram from the world
 of information into the world of data.

@table*[ @list[ @t{@image["door-sim.png"]} @t{@image["door-real.png"]}] ]

For completeness, we have repeated the original diagram on the right so
that you can see which computer interaction corresponds to which domain
action. 

@; -----------------------------------------------------------------------------
@subsection{Simulating a Door: Functions}

Our analysis and data definition leaves us with three functions to design: 

@itemize[

@item{@emph{automatic-closer}, which closes the time during one tick;}

@item{@emph{door-actions}, which manipulates the time in response to
pressing a key; and}

@item{@emph{render}, which translates the current state of the door into
a visible scene.}

]

Let's start with @emph{automatic-closer}. Since @emph{automatic-closer}
acts as the @scheme[on-tick] handler, we get its contract, 
and it is easy to refine the purpose statement, too:

@(begin
#reader scribble/comment-reader
(schemeblock
;; automatic-closer : WorldState -> WorldState
;; closes an open door over the period of one tick 
(define (automatic-closer state-of-door) ...)
))

 Making up examples is trivial when the world can only be in one of three
 states: 

@table*[
	@list[@t{ given state } @t{ desired state }]
        @list[@t{ @scheme['locked] } @t{ @scheme['locked] }]
        @list[@t{ @scheme['closed] } @t{ @scheme['closed] }]
        @list[@t{ @scheme['open]   } @t{ @scheme['closed] }]
]

@(begin
#reader scribble/comment-reader
(schemeblock
;; automatic-closer : WorldState -> WorldState
;; closes an open door over the period of one tick 

(check-expect (automatic-closer 'locked) 'locked)
(check-expect (automatic-closer 'closed) 'closed)
(check-expect (automatic-closer 'open) 'closed)

(define (automatic-closer state-of-door) ...)
))

 The template step demands a conditional with three clauses: 

@(begin
#reader scribble/comment-reader
(schemeblock
(define (automatic-closer state-of-door)
  (cond
    [(symbol=? 'locked state-of-door) ...]
    [(symbol=? 'closed state-of-door) ...]
    [(symbol=? 'open state-of-door) ...]))
))

 The examples basically dictate what the outcomes of the three cases must
 be:

@(begin
#reader scribble/comment-reader
(schemeblock
(define (automatic-closer state-of-door)
  (cond
    [(symbol=? 'locked state-of-door) 'locked]
    [(symbol=? 'closed state-of-door) 'closed]
    [(symbol=? 'open state-of-door) 'closed]))
))

 Don't forget to run the example-tests. 

For the remaining three arrows of the diagram, we design a function that
 reacts to the three chosen keyboard events. As mentioned, functions that
 deal with keyboard events consume both a world and a keyevent:

@(begin
#reader scribble/comment-reader
(schemeblock
;; door-actions : WorldState @tech{KeyEvent} -> WorldState
;; key events simulate actions on the door 
(define (door-actions s k) ...)
))

@table*[
	@list[@t{ given state } @t{ given keyevent } @t{ desired state }]
 
@list[ @t{ @scheme['locked] } @t{ @scheme[#\u]}     @t{@scheme['closed]}]
@list[ @t{ @scheme['closed] } @t{ @scheme[#\l]}     @t{@scheme['locked]}]
@list[ @t{ @scheme['closed] } @t{ @scheme[#\space]} @t{@scheme['open]  }]
@list[ @t{ @scheme['open] }   @t{  --- }             @t{@scheme['open]  }]]

 The examples combine what the above picture shows and the choices we made
 about mapping actions to keyboard events. 

From here, it is straightforward to turn this into a complete design:
 
@schemeblock[
(define (door-actions s k)
  (cond
    [(and (symbol=? 'locked s) (key=? #\u k)) 'closed]
    [(and (symbol=? 'closed s) (key=? #\l k)) 'locked]
    [(and (symbol=? 'closed s) (key=? #\space k)) 'open]
    [else s]))

(check-expect (door-actions 'locked #\u) 'closed)
(check-expect (door-actions 'closed #\l) 'locked)
(check-expect (door-actions 'closed #\space) 'open)
(check-expect (door-actions 'open 'any) 'open)
(check-expect (door-actions 'closed 'any) 'closed)
]

Last but not least we need a function that renders the current state of the
world as a scene. For simplicity, let's just use a large text for this
purpose:

@(begin
#reader scribble/comment-reader
(schemeblock
;; render : WorldState -> @tech{scene}
;; translate the current state of the door into a large text 
(define (render s)
  (text (symbol->string s) 40 'red))

(check-expect (render 'closed) (text "closed" 40 'red))
))
 The function @scheme[symbol->string] translates a symbol into a string,
 which is needed because @scheme[text] can deal only with the latter, not
 the former. A look into the language documentation revealed that this
 conversion function exists, and so we use it. 

Once everything is properly designed, it is time to @emph{run} the
program. In the case of the universe teachpack, this means we must specify
which function takes care of tick events, key events, and drawing: 

@schemeblock[
(big-bang 'locked
          (on-tick automatic-closer)
	  (on-key door-actions)
	  (on-draw render))
]
 
Now it's time for you to collect the pieces and run them in DrScheme to see
whether it all works. 

Exercise: Design a data representation that closes the door over two (or
three or more) clock ticks instead of one. 

@; -----------------------------------------------------------------------------
@section[#:tag "world2"]{The World is not Enough} 

The library facilities covered so far are about designing individual
 programs with interactive graphical user interfaces (simulations,
 animations, games, etc.). In this section, we introduce capabilities for
 designing a distributed program, which is really a number of programs that
 coordinate their actions in some fashion. Each of the individual programs
 may run on any computer in the world (as in our planet and the spacecrafts
 that we sent out), as long as it is on the internet and as long as the
 computer allows the program to send and receive messages (via TCP). We
 call this arrangement a @tech{universe} and the program that coordinates
 it all a @emph{universe server} or just @tech{server}.

This section explains what messages are, how to send them from a
 @tech{world} program, how to receive them, and how to connect a
 @tech{world} program to a @tech{universe}.

@; -----------------------------------------------------------------------------

@subsection{Messages}

After a world program has become a part of a universe, it may send messages
 and receive them. In terms of data, a message is just an
 @tech{S-expression}.

@deftech{S-expression} An S-expression is roughly a nested list of basic
data; to be precise, an S-expression is one of: 

@itemize[
 @item{a string,}
 @item{a symbol,}
 @item{a number,}
 @item{a boolean,}
 @item{a char, or}
 @item{a list of S-expressions.}
]
Note the last clause includes @scheme[empty] of course. 

@defproc[(sexp? [x any/c]) boolean?]{
 determines whether @scheme[x] is an @tech{S-expression}.}

@subsection{Sending Messages}

Each world-producing callback in a world program---those for handling clock
 tick events, keyboard events, and mouse events---may produce a
 @tech{Package} in addition to just a @tech{WorldState}. 

@deftech{Package} represents a pair consisting of a @tech{WorldState}
 and a message from a @tech{world} program to the @tech{server}.  Because
 programs only send messages via @tech{Package}, the teachpack does not
 provide the selectors for the structure, only the constructor and a
 predicate.

@defproc[(package? [x any/c]) boolean?]{
 determine whether @scheme[x] is a @tech{Package}.}

@defproc[(make-package [w any/c][m sexp?]) package?]{
 create a @tech{Package} from a @tech{WorldState} and an @tech{S-expression}.}

As mentioned, all event handlers may return @tech{WorldState}s or @tech{Package}s;
here are the revised specifications: 

@defform/none[(on-tick tick-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{WorldState}) (or/c (unsyntax @tech{WorldState}) package?))])]{
} 

@defform/none[(on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{WorldState}) (or/c (unsyntax @tech{WorldState}) package?))]
               [rate-expr natural-number/c])]{
}

@defform/none[(on-key change-expr)
              #:contracts
              ([change-expr (-> (unsyntax @tech{WorldState}) key-event? (or/c (unsyntax @tech{WorldState}) package?))])]{
}

@defform/none[(on-mouse clack-expr)
              #:contracts
              ([clack-expr
                (-> (unsyntax @tech{WorldState}) natural-number/c natural-number/c (unsyntax @tech{MouseEvent})
                    (or/c (unsyntax @tech{WorldState}) package?))])]{
}

If one of these event handlers produces a @tech{Package}, the content of the world
 field becomes the next world and the message field specifies what the
 world sends to the universe. This distinction also explains why the data
 definition for @tech{WorldState} may not include a @tech{Package}.

@subsection{Connecting with the Universe}

Messages are sent to the universe program, which runs on some computer in
 the world. The next section is about constructs for creating such a universe
 server. For now, we just need to know that it exists and that it is the recipient
 of messages. 

@deftech{IP} @scheme[string?]

Before a world program can send messages, it must register with the
 server. Registration must specify the internet address of the computer on which
 the server runs, also known as an @tech{IP} address or a host.  Here a
 @tech{IP} address is a string of the right shape, e.g., @scheme["192.168.1.1"]
 or @scheme["www.google.com"]. 

@defthing[LOCALHOST string?]{the @tech{IP} of your computer. Use it while you
 are developing a distributed program, especially while you are
 investigating whether the participating world programs collaborate in an
 appropriate manner. This is called @emph{integration testing} and differs
 from unit testing quite a bit.}

A @scheme[big-bang] description of a world program that wishes to communicate
with other programs must contain a @scheme[register] clause of one of the
following shapes: 

@itemize[

@item{
@defform[(register ip-expr) #:contracts ([ip-expr string?])]{
 connect this world to a universe server at the specified @scheme[ip-expr]
 address and set up capabilities for sending and receiving messages.}
}

@item{
@defform[(name name-expr)
         #:contracts
         ([name-expr (or/c symbol? string?)])]{
 provide a name (@scheme[namer-expr]) to this world, which is used as the
 title of the canvas and the name sent to the server.}
}

]

When a world program registers with a universe program and the universe program
stops working, the world program stops working, too. 

@subsection{Receiving Messages}

Finally, the receipt of a message from the server is an event, just like
 tick events, keyboard events, and mouse events. Dealing with the receipt of a
 message works exactly like dealing with any other event. DrScheme
 applies the event handler that the world program specifies; if there is no
 clause, the message is discarded.

The @scheme[on-receive] clause of a @scheme[big-bang] specifies the event handler
 for message receipts. 

@defform[(on-receive receive-expr)
         #:contracts
	 ([receive-expr (-> (unsyntax @tech{WorldState}) sexp? (or/c (unsyntax @tech{WorldState}) package?))])]{
 tell DrScheme to call @scheme[receive-expr] for every message receipt, on the current
 @tech{WorldState} and the received message. The result of the call becomes the current
 @tech{WorldState}. 

 Because @scheme[receive-expr] is (or evaluates to) a world-transforming
 function, it too can produce a @tech{Package} instead of just a
 @tech{WorldState}. If the result is a @tech{Package}, its message content is
 sent to the @tech{server}.}

The diagram below summarizes the extensions of this section in graphical form. 

@image["universe.png"]

A registered world program may send a message to the universe server
 at any time by returning a @tech{Package} from an event handler. The
 message is transmitted to the server, which may forward it to some
 other world program as given or in some massaged form. The arrival of a
 message is just another event that a world program must deal with. Like
 all other event handlers @emph{receive} accepts a @tech{WorldState} and some
 auxiliary arguments (a message in this case) and produces a
 @tech{WorldState} or a @tech{Package}.

When messages are sent from any of the worlds to the universe or vice versa,
 there is no need for the sender and receiver to synchronize. Indeed, a sender
 may dispatch as many messages as needed without regard to whether the
 receiver has processed them yet. The messages simply wait in queue until
 the receiving @tech{server} or @tech{world} program take care of them. 

@; -----------------------------------------------------------------------------
@section[#:tag "universe-server"]{The Universe Server}

A @deftech{server} is the central control program of a @tech{universe} and
 deals with receiving and sending of messages between the world
 programs that participate in the @tech{universe}. Like a @tech{world}
 program, a server is a program that reacts to events, though to different
 events than @tech{world}s. The two primary kinds of events are the
 appearance of a new @tech{world} program in the @tech{universe} 
 and the receipt of a message from a @tech{world} program. 

The teachpack provides a mechanism for designating event handlers for
 servers that is quite similar to the mechanism for describing @tech{world}
 programs. Depending on the designated event handlers, the server takes on
 distinct roles:

@itemize[

@item{A server may be a ``pass through'' channel between two worlds, in which case
 it has no other function than to communicate whatever message it receives
 from one world to the other, without any interference.}

@item{A server may enforce a ``back and forth'' protocol, i.e., it may force two
 (or more) worlds to engage in a civilized tit-for-tat exchange. Each
 world is given a chance to send a message and must then wait 
 to get a reply before it sends anything again.}

@item{A server may play the role of a special-purpose arbiter, e.g., the referee
 or administrator of a game. It may check that each world ``plays'' by the rules,
 and it administrate the resources of the game.}

]

As a matter of fact, a pass-through @tech{server} can become basically
invisible, making it appear as if all communication goes from peer
@tech{world} to peer in a @tech{universe}. 

This section first introduces some basic forms of data that the
 @tech{server} uses to represent @tech{world}s and other matters. Second,
 it explains how to describe a server program. 

@; -----------------------------------------------------------------------------
@subsection{Worlds and Messages}

Understanding the server's event handling functions demands several data
 representations: that of (a connection to) a @tech{world} program and that
 of a response of a handler to an event. 

@itemize[

@item{The @tech{server} and its event handlers must agree on a
 data representation of the @tech{world}s that participate in the
 universe. 

@defproc[(iworld? [x any/c]) boolean?]{
 determines whether @scheme[x] is a @emph{iworld}. Because the universe server
 represents worlds via structures that collect essential information about
 the connections, the teachpack does not export any constructor or selector
 functions on worlds.} 

@defproc[(iworld=? [u iworld?][v iworld?]) boolean?]{
 compares two @emph{iworld}s for equality.}

@defproc[(iworld-name [w iworld?]) symbol?]{
 extracts the name from a @emph{iworld} structure.}

@defthing[iworld1 iworld?]{an @emph{iworld} for testing your programs}
@defthing[iworld2 iworld?]{another iworld for testing your programs}
@defthing[iworld3 iworld?]{and a third one}

The three sample iworlds are provided so that you can test your functions
for universe programs. For example: 

@schemeblock[
(check-expect (iworld=? iworld1 iworld2) false)
(check-expect (iworld=? iworld2 iworld2) true)
]
}

@item{Each event handler produces a @emph{bundle}, which is a structure
 that contains the list of @emph{iworld}s that the universe must track; the
 @tech{server}'s remaining state; and a list of mails to other
 worlds: 

@defproc[(bundle? [x any/c]) boolean?]{
 determines whether @scheme[x] is a @emph{bundle}.}

@defproc[(make-bundle [low (listof iworld?)] [state any/c] [mails (listof mail?)]) bundle?]{
 creates a @emph{bundle} from a list of iworlds, a piece of data that
 represents a server state, and a list of mails.}

If an event handler returns a bundle with an empty list of worlds, the
universe server is restarted in the initial state. 

A @emph{mail} represents a message from an event handler to a world. The
teachpack provides only a predicate and a constructor for these structures:

@defproc[(mail? [x any/c]) boolean?]{
 determines whether @scheme[x] is a @emph{mail}.}

@defproc[(make-mail [to iworld?] [content sexp?]) mail?]{
 creates a @emph{mail} from a @emph{iworld} and an @tech{S-expression}.}
}
]

@; -----------------------------------------------------------------------------
@subsection{Universe Descriptions}

A @tech{server} keeps track of information about the @tech{universe} that
 it manages. One kind of tracked information is obviously the collection of
 participating world programs, but in general the kind of information that
 a server tracks and how the information is represented depends on the
 situation and the programmer, just as with @tech{world} programs.

@deftech{UniverseState} @scheme[any/c] represents the server's state. For running
@tech{universe}s, the teachpack demands that you come up with a data
definition for (your state of the) @tech{server}.  Any piece of data can
represent the state. We just assume that you introduce a data definition
for the possible states and that your event handlers are designed
according to the design recipe for this data definition.

The @tech{server} itself is created with a description that includes the
 first state and a number of clauses that specify functions for dealing
 with @tech{universe} events.

@defform/subs[#:id universe
              #:literals 
	      (on-new on-msg on-tick on-disconnect to-string)
              (universe state-expr clause ...)
              ([clause
		 (on-new new-expr)
		 (on-msg msg-expr)
		 (on-tick tick-expr)
		 (on-tick tick-expr rate-expr)
		 (on-disconnect dis-expr)
		 (to-string render-expr)
		 ])]{

creates a server with a given state, @scheme[state-expr]. The
behavior is specified via handler functions through mandatory and optional
@emph{clause}s. These functions govern how the server deals with the
registration of new worlds, how it disconnects worlds, how it sends
messages from one world to the rest of the registered worlds, and how it
renders its current state as a string.}

Evaluating a @scheme[universe] expression starts a server. Visually it opens
 a console window on which you can see that worlds join, which messages are
 received from which world, and which messages are sent to which world. For
 convenience, the console also has two buttons: one for shutting down a
 universe and another one for re-starting it. The latter functionality is
 especially useful during the integration of the various pieces of a
 distributed program. 

The mandatory clauses of a @scheme[universe] server description are
@scheme[on-new] and @scheme[on-msg]: 

@itemize[

@item{
 @defform[(on-new new-expr)
          #:contracts
          ([new-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) iworld? bundle?)])]{
 tell DrScheme to call the function @scheme[new-expr] every time another world joins the
 universe. The event handler is called on the current list of iworlds and the
 joining iworld, which isn't on the list yet. In particular, the handler may
 reject a @tech{world} program from participating in a @tech{universe},
 simply by not including it in the resulting @scheme[bundle] structure. The
 handler may still send one message to the world that attempts to join. }
}

@item{
 @defform[(on-msg msg-expr)
          #:contracts
          ([msg-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) iworld? sexp? bundle?)])]{

 tell DrScheme to apply @scheme[msg-expr] to the list of currently
 participating worlds @scheme[low], the current state of the universe, the world
 @scheme[w] that sent the message, and the message itself. Note that
 @scheme[w] is guaranteed to be on the list @scheme[low]. 
 }
}]
 All proper event handlers produce a @emph{bundle}. The list of worlds in
 this @emph{bundle} becomes the server's list of worlds, meaning that only
 the server listens only to messages from "approved" worlds.  The state in
 the bundle is safe-guarded by the server until the next event, and the
 mails are broadcast as specified.

The following picture provides a graphical overview of the server's workings. 

@; -----------------------------------------------------------------------------
@;; THE PICTURE IS WRONG
@; -----------------------------------------------------------------------------

@image["server.png"]

In addition to the mandatory handlers, a program may wish to add some
optional handlers: 

@itemize[

@item{
@defform/none[(on-tick tick-expr)
              #:contracts
              ([tick-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) bundle?)])]{
 tell DrScheme to apply @scheme[tick-expr] to the current list of
 participating worlds and the current state of the
 universe. 
 }

@defform/none[(on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) bundle?)]
               [rate-expr natural-number/c])]{ 
 tell DrScheme to apply @scheme[tick-expr] as above but use the specified
 clock tick rate instead of the default.
 }

}

@item{
 @defform[(on-disconnect dis-expr)
          #:contracts
          ([dis-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) iworld? bundle?)])]{
 tell DrScheme to invoke @scheme[dis-expr] every time a participating
 @tech{world} drops its connection to the server. The first two arguments
 are the current list of participating worlds and the state of the
 universe; the third one is the world that got disconnected. 
 }
}

@item{
 @defform[(to-string render-expr)
          #:contracts
          ([render-expr (-> [listof iworld?] (unsyntax @tech{UniverseState}) string?)])]{
 tell DrScheme to render the state of the universe after each event and to
 display this string in the universe console. 
 }
}

]

@; -----------------------------------------------------------------------------
@section[#:tag "universe-sample"]{A First Sample Universe} 

This section uses a simple example to explain the design of a universe,
 especially its server and some participating worlds. The first subsection
 explains the example, the second introduces the general design plan for
 such universes. The remaining sections present the full-fledged solution.

@subsection{Two Ball Tossing Worlds}

Say we want to represent a universe that consists of a number of worlds and
 that gives each world a ``turn'' in a round-robin fashion. If a world is
 given its turn, it displays a ball that ascends from the bottom of a
 canvas to the top. It relinquishes its turn at that point and the server
 gives the next world a turn. 

Here is an image that illustrates how this universe would work if two
 worlds participated: 

@image["balls.gif"]

 The two @tech{world} programs could be located on two distinct computers
 or on just one. A @tech{server} mediates between the two worlds, including
 the initial start-up. 

@; -----------------------------------------------------------------------------
@subsection{Hints on Designing Universes}

The first step in designing a @tech{universe} is to understand the
 coordination of the @tech{world}s from a global perspective. To some
 extent, it is all about knowledge and the distribution of knowledge
 throughout a system. We know that the @tech{universe} doesn't exist until
 the server starts and the @tech{world}s are joining. Because of the nature
 of computers and networks, however, we may assume little else. Our network
 connections ensure that if some @tech{world} or the @tech{server} sends
 two messages to the @emph{same} place in some order, they arrive in the
 same order (if they arrive at all). In contrast, if two distinct
 @tech{world} programs send one message each, the network does not
 guarantee the order of arrival at the server; similarly, if the
 @tech{server} is asked to send some messages to several distinct
 @tech{world} programs, they may arrive at those worlds in the order sent
 or in the some other order. In the same vein, it is impossible to ensure
 that one world joins before another. Worst, when someone removes the
 connection (cable, wireless) between a computer that runs a @tech{world}
 program and the rest of the network or if some network cable is cut,
 messages don't go anywhere. Due to this vagaries, it is therefore the
 designer's task to establish a protocol that enforces a certain order onto
 a universe and this activity is called @emph{protocol design}.

From the perspective of the @tech{universe}, the design of a protocol is
 about the design of data representations for tracking universe information
 in the server and the participating worlds and the design of a data
 representation for messages. As for the latter, we know that they must be
 @tech{S-expression}s, but usually @tech{world} programs don't send all
 kinds of @tech{S-expression}s. The data definitions for messages must
 therefore select a subset of suitable @tech{S-expression}s. As for the
 state of the server and the worlds, they must reflect how they currently
 relate to the universe. Later, when we design their ``local'' behavior, we
 may add more components to their state space. 

In summary, the first step of a protocol design is to introduce: 

@itemize[

@item{a data definition for the information about the universe that the
server tracks, call it @tech{UniverseState};} 

@item{a data definition for the world(s) about their current relationship
to the universe;}

@item{data definitions for the messages that are sent from the server to
the worlds and vice versa. Let's call them @deftech{S2W} for messages
from the server to the worlds and @deftech{W2S} for the other direction;
in the most general case you may need one pair per world.}
]

If all the worlds exhibit the same behavior over time, a single data
definition suffices for step 2. If they play different roles, we may need
one data definition per world. 

Of course, as you define these collections of data always keep in mind what
the pieces of data mean, what they represent from the universe's
perspective. 

The second step of a protocol design is to figure out which major
 events---the addition of a world to the universe, the arrival of a message
 at the server or at a world---to deal with and what they imply for the
 exchange of messages. Conversely, when a server sends a message to a
 world, this may have implications for both the state of the server and the
 state of the world. A good tool for writing down these agreements is an
 interaction diagram. 


@verbatim{
 
     Server              World1                  World2 
       |                   |                       |
       |   'go             |                       |
       |<------------------|                       |
       |    'go            |                       |
       |------------------------------------------>|
       |                   |                       |
       |                   |                       |
}

 Each vertical line is the life line of a @tech{world} program or the
 @tech{server}. Each horizontal arrow denotes a message sent from one
 @tech{universe} participant to another. 

The design of the protocol, especially the data definitions, have direct
implications for the design of event handling functions. For example, in
the server we may wish to deal with two kinds of events: the joining of a
new world and the receipt of a message from one of the worlds. This
translates into the design of two functions with the following headers, 

@(begin
#reader scribble/comment-reader
(schemeblock
;; Bundle is
;;   (make-bundle [Listof iworld?] UniverseState [Listof mail?])

;; [Listof iworld?] UniverseState iworld? -> Bundle 
;; compute next list of worlds and new @tech{UniverseState} 
;; when world w is joining the universe, which is in state s; 
(define (add-world s w) ...)

;; [Listof iworld?] UniverseState iworld? W2U -> Bundle 
;; compute next list of worlds and new @tech{UniverseState} 
;; when world w is sending message m to universe in state s
(define (process s p m) ...)
))

Finally, we must also decide how the messages affect the states of the
 worlds; which of their callback may send messages and when; and what to do
 with the messages a world receives. Because this step is difficult to
 explain in the abstract, we move on to the protocol design for the
 universe of ball worlds. 

@; -----------------------------------------------------------------------------
@subsection{Designing the Ball Universe}

Running the ball @tech{universe} has a simple overall goal: to ensure that at any
 point in time, one @tech{world} is active and all others are passive. The active
 @tech{world} displays a moving ball, and the passive @tech{world}s should display
 something, anything that indicates that it is some other @tech{world}'s turn. 

As for the server's state, it must obviously keep track of all @tech{world}s that
 joined the @tech{universe}, and it must know which one is active and which ones
 are passive. Of course, initially the @tech{universe} is empty, i.e., there are
 no @tech{world}s and, at that point, the server has nothing to track. 

While there are many different useful ways of representing such a
 @tech{universe}, we just use the list of @emph{iworlds} that is handed to
 each handler and that handlers return via their bundles. The
 @tech{UniverseState} itself is useless for this trivial example. We
 interpret non-empty lists as those where the first @emph{iworld} is active
 and the remainder are the passive @emph{iworld}s. As for the two possible
 events, 

@itemize[

@item{it is natural to add new @emph{iworld}s to the end of the list; and}

@item{it is natural to move an active @emph{iworld} that relinquishes its turn to
the end of the list, too.}
]

The server should send messages to the first @emph{iworld} of its list as
 long as it wishes this @emph{iworld} to remain active. In turn, it should
 expect to receive messages only from this one active @emph{iworld} and no
 other @emph{iworld}. The content of these two messages is nearly irrelevant
 because a message from the server to a @emph{iworld} means that it is the
 @emph{iworld}'s turn and a message from the @emph{iworld} to the server
 means that the turn is over. Just so that we don't confuse ourselves, we
 use two distinct symbols for these two messages:
@itemize[
@item{A @defterm{GoMessage} is @scheme['it-is-your-turn].}
@item{A @defterm{StopMessage} is @scheme['done].}
]

From the @tech{universe}'s perspective, each @tech{world} is in one of two states: 
@itemize[
@item{A passive @tech{world} is @emph{resting}. We use @scheme['resting] for this state.}
@item{An active @tech{world} is not resting. We delay choosing a representation
for this part of a @tech{world}'s state until we design its ``local'' behavior.}
]
 It is also clear that an active @tech{world} may receive additional messages,
 which it may ignore. When it is done with its turn, it will send a
 message. 

@verbatim{
     Server
       |                 World1
       |<==================|
       |  'it-is-your-turn |
       |------------------>|
       |                   |                    World2 
       |<==========================================|
       |  'done            |                       |
       |<------------------|                       |
       |  'it-is-your-turn |                       |
       |------------------------------------------>|
       |                   |                       |
       |                   |                       |
       |  'done            |                       |
       |<------------------------------------------|
       |  'it-is-your-turn |                       |
       |------------------>|                       |
       |                   |                       |
       |                   |                       |       
}

Here the double-lines (horizontal) denote the registration step, the others
 are message exchanges. The diagram thus shows how the @tech{server}
 decides to make the first registered world the active one and to enlist
 all others as they join. 


@; -----------------------------------------------------------------------------
@subsection{Designing the Ball Server}

The preceding subsection dictates that our server program starts like this: 

@(begin
#reader scribble/comment-reader
[schemeblock
;; teachpack: universe.ss
  
;; UniverseState is '*
;; StopMessage is 'done. 
;; GoMessage is 'it-is-your-turn.
])

 The design of a protocol has immediate implications for the design of the
 event handling functions of the server. Here we wish to deal with two
 events: the appearance of a new world and the receipt of a message. Based
 on our data definitions and based on the general contracts of the event
 handling functions spelled out in this documentation, we get two functions
 for our wish list:

@(begin
#reader scribble/comment-reader
[schemeblock
;; Result is 
;;   (make-bundle [Listof world?] '* (list (make-mail world? GoMessage)))

;; [Listof world?] UniverseState world? -> Result 
;; add world w to the universe, when server is in state u
(define (add-world u w) ...)

;; [Listof world?] UniverseState world? StopMessage -> Result
;; world w sent message m when server is in state u 
(define (switch u w m) ...)
])

Although we could have re-used the generic contracts from this
documentation, we also know from our protocol that our server sends a
message to exactly one world. Note how these contracts are just refinements
of the generic ones. (A type-oriented programmer would say that the
contracts here are subtypes of the generic ones.)

The second step of the design recipe calls for functional examples: 

@(begin
#reader scribble/comment-reader
[schemeblock
;; an obvious example for adding a world: 
(check-expect
  (add-world '() '* world1) 
  (make-bundle (list world1)
               '*
               (list (make-mail world1 'it-is-your-turn))))

;; an example for receiving a message from the active world:
(check-expect
 (switch (list world1 world2) '* world1 'done)
 (make-bundle (list world2 world1)
              '*
              (list (make-mail world2 'it-is-your-turn))))
])

 Note that our protocol analysis dictates this behavior for the two
 functions. Also note how we use @scheme[world1], @scheme[world2], and
 @scheme[world3] because the teachpack applies these event handlers to real
 worlds.

Exercise: Create additional examples for the two functions based on our
protocol. 

The protocol tells us that @emph{add-world} just adds the given
 @emph{world} structure---recall that this a data representation of the
 actual @tech{world} program---to the given list of worlds. It then sends a
 message to the first world on this list to get things going:

@(begin
#reader scribble/comment-reader
[schemeblock
(define (add-world univ state wrld)
  (local ((define univ* (append univ (list wrld))))
    (make-bundle univ*
                 '*
                 (list (make-mail (first univ*) 'it-is-your-turn)))))
])

Because @emph{univ*} contains at least @emph{wrld}, it is acceptable to
create a mail to @scheme[(first univ*)]. Of course, this same reasoning
also implies that if @emph{univ} isn't empty, its first element is an
active world and is about to receive a second @scheme['it-is-your-turn] message. 

Similarly, the protocol says that when @emph{switch} is invoked because a
 @tech{world} program sends a message, the data representation of the
 corresponding world is moved to the end of the list and the next world on
 the (resulting) list is sent a message: 

@(begin
#reader scribble/comment-reader
[schemeblock
(define (switch univ state wrld m)
  (local ((define univ* (append (rest univ) (list (first univ)))))
    (make-bundle univ*
                 '*
                 (list (make-mail (first univ*) 'it-is-your-turn)))))
])

 As before, appending the first world to the end of the list guarantees
 that there is at least this one world on this list. It is therefore
 acceptable to create a mail for this world. 

Start the server now. 

 @schemeblock[(universe '* (on-new add-world) (on-msg switch))]
          
Exercise: The function definition simply assumes that @emph{wrld} is
 @scheme[world=?] to @scheme[(first univ)] and that the received message
 @emph{m} is @scheme['done]. Modify the function definition so that it
 checks these assumptions and raises an error signal if either of them is
 wrong. Start with functional examples. If stuck, re-read the section on
 checked functions from HtDP. (Note: in a @tech{universe} it is quite
 possible that a program registers with a @tech{server} but fails to stick
 to the agreed-upon protocol. How to deal with such situations properly
 depends on the context. For now, stop the @tech{universe} at this point by
 returning an empty list of worlds. Consider alternative solutions, too.) 

Exercise: An alternative state representation would equate 
 @tech{UniverseState} with @emph{world} structures, keeping track of the
 active world. The list of world in the server would track the passive
 worlds only. Design appropriate @scheme[add-world] and @scheme[switch]
 functions. 

@; -----------------------------------------------------------------------------
@subsection{Designing the Ball World}

The final step is to design the ball @tech{world}. Recall that each world
 is in one of two possible states: active or passive. The second kind of
 @tech{world} moves a ball upwards, decreasing the ball's @emph{y}
 coordinate; the first kind of @tech{world} displays something that says
 it's someone else's turn.  Assuming the ball always moves along a vertical
 line and that the vertical line is fixed, the state of the world is an
 enumeration of two cases: 

@(begin #reader scribble/comment-reader
(schemeblock
;; teachpack: universe.ss

;; WorldState is one of:
;; -- Number	         %% representing the @emph{y} coordinate 
;; -- @scheme['resting]

(define WORLD0 'resting)

;; A WorldResult is one of:
;; -- WorldState
;; -- (make-package WorldState StopMessage)
))
 The definition says that initially a @tech{world} is passive. 

The communication protocol and the refined data definition of @tech{WorldState}
 imply a number of contract and purpose statements: 

@(begin
#reader scribble/comment-reader
(schemeblock

;; WorldState GoMessage -> WorldResult 
;; make sure the ball is moving 
(define (receive w n) ...)

;; WorldState -> WorldResult
;; move this ball upwards for each clock tick 
;; or stay @scheme['resting]
(define (move w) ...) 

;; WorldState -> Scene
;; render the world as a scene
(define (render w) ...)
))

Let's design one function at a time, starting with @emph{receive}.  Since
 the protocol doesn't spell out what @emph{receive} is to compute, let's
 create a good set of functional examples, exploiting the structure of the
 data organization of @tech{WorldState}:

@(begin
#reader scribble/comment-reader
(schemeblock
(check-expect (receive 'resting 'it-is-your-turn) HEIGHT)
(check-expect (receive (- HEIGHT 1) 'it-is-your-turn) ...)
))

Since there are two kinds of states, we make up at least two kinds of
 examples: one for a @scheme['resting] state and another one for a numeric
 state. The dots in the result part of the second unit test reveal the
 first ambiguity; specifically it isn't clear what the result should be
 when an active @tech{world} receives another message to activate itself. The
 second ambiguity shows up when we study additional examples, which are
 suggested by our approach to designing functions on numeric intervals
 (HtDP, section 3). That is we should consider the following three inputs
 to @emph{receive}: 

@itemize[
@item{@scheme[HEIGHT] when the ball is at the bottom of the scene;}
@item{@scheme[(- HEIGHT 1)] when the ball is properly inside the scene; and}
@item{@scheme[0] when the ball has hit the top of the scene.}
]

 In the third case the function could produce three distinct results:
 @scheme[0], @scheme['resting], or @scheme[(make-package 'resting
 'done)]. The first leaves things alone; the second turns the active @tech{world}
 into a resting one; the third does so, too, and tells the universe about
 this switch. 

We choose to design @emph{receive} so that it ignores the message and
 returns the current state of an active @tech{world}.  This ensures that the ball
 moves in a continuous fashion and that the @tech{world} remains active.

Exercise: One alternative design is to move the ball back to the bottom of
the scene every time @scheme['it-is-your-turn] is received. Design this function, too. 

@(begin
#reader scribble/comment-reader
(schemeblock

(define (receive w m)
  (cond
    [(symbol? w) HEIGHT] ;; meaning: @scheme[(symbol=? w 'resting)]
    [else w]))
))

 Our second function to design is @emph{move}, the function that computes
 the ball movement. We have the contract and the second step in the design
 recipe calls for examples: 

@(begin
#reader scribble/comment-reader
(schemeblock
; WorldState -> WorldState or @scheme[(make-package 'resting 'done)]
; move the ball if it is flying

(check-expect (move 'resting) 'resting)
(check-expect (move HEIGHT) (- HEIGHT 1))
(check-expect (move (- HEIGHT 1)) (- HEIGHT 2))
(check-expect (move 0) (make-package 'resting 'done))

(define (move x) ...)
))

 Following HtDP again, the examples cover four typical situations:
 @scheme['resting], two end points of the specified numeric interval, and
 one interior point. They tell us that @emph{move} leaves a passive @tech{world}
 alone and that it otherwise moves the ball until the @emph{y} coordinate
 becomes @scheme[0]. In the latter case, the result is a package that
 renders the @tech{world} passive and tells the server about it.

 Turning these thoughts into a complete definition is straightforward now: 

@(begin
#reader scribble/comment-reader
(schemeblock
(define (move x)
  (cond
    [(symbol? x) x]
    [(number? x) (if (<= x 0) (make-package 'resting 'done) (sub1 x))]))
))

Exercise: what could happen if we had designed @emph{receive} so that it
 produces @scheme['resting] when the state of the world is @scheme[0]?  Use
 your answer to explain why you think it is better to leave this kind of
 state change to the tick event handler instead of the message receipt
 handler?

Finally, here is the third function, which renders the state as a scene: 

@(begin
#reader scribble/comment-reader
(schemeblock
; WorldState -> Scene
; render the state of the world as a scene 

(check-expect (render HEIGHT) (place-image BALL 50 HEIGHT MT))
(check-expect (render 'resting)
              (place-image  (text "resting" 11 'red) 10 10 MT))

(define (render w)
  (place-image 
    (text name 11 'black) 5 85 
    (cond
      [(symbol? w) (place-image (text "resting" 11 'red) 10 10 MT)]
      [(number? w) (place-image BALL 50 w MT)])))

))

 Here is an improvement that adds a name to the scene and abstracts over
 the name at the same time:

@(begin
#reader scribble/comment-reader
(schemeblock
; String -> (WorldState -> Scene)
; render the state of the world as a scene 

(check-expect 
 ((draw "Carl") 100) 
 (place-image (text "Carl" 11 'black) 
              5 85 
              (place-image BALL 50 100 MT)))

(define (draw name)
  (lambda (w)
    (place-image 
     (text name 11 'black) 5 85 
     (cond
       [(symbol? w) (place-image (text "resting" 11 'red) 10 10 MT)]
       [(number? w) (place-image BALL 50 w MT)]))))

))

 By doing so, we can use the same program to create many different
 @tech{world}s that register with a @tech{server} on your computer:
@(begin
#reader scribble/comment-reader
(schemeblock

; String -> WorldState 
; create and hook up a world with the @scheme[LOCALHOST] server
(define (create-world n)
  (big-bang WORLD0
           (on-receive receive)
	   (on-draw (draw n))
	   (on-tick move)
           (name n)
	   (register LOCALHOST)))
))

 Now you can use @scheme[(create-world 'carl)] and @scheme[(create-world 'same)],
 respectively, to run two different worlds, after launching a @tech{server}
 first. 

Exercise: Design a function that takes care of a world to which the
 universe has lost its connection. Is @emph{Result} the proper contract for
 the result of this function? 

