#lang scribble/doc
@(require "ss.ss"
          "pict-diagram.rkt"
          (for-label racket/gui
                     slideshow/code
                     slideshow/flash
                     slideshow/face
                     slideshow/balloon))

@title[#:style 'toc]{Making Pictures}

@declare-exporting[slideshow/pict slideshow]

@defmodule*/no-declare[(slideshow/pict)]{ The
@racketmodname[slideshow/pict] layer provides core functions for
constructing pictures, and it is independent of the slide viewer. This
layer can be used, for example, to generate a picture as encapsulated
PostScript for inclusion into a larger document. The
@racketmodname[slideshow/pict] layer is re-provided by the
@racketmodname[slideshow] language.}

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section{Pict Datatype}

A picture is a @racket[pict] structure. Some functions, such as
@racket[hline], create new simple picts. Other functions, such as
@racket[ht-append], build new picts out of existing picts. In the
latter case, the embedded picts retain their identity, so that
offset-finding functions, such as @racket[lt-find], can find the
offset of an embedded pict in a larger pict.

In addition to its drawing part, a pict has the following
@deftech{bounding box} structure:

@centerline[pict-diagram]

That is, the bounding box has a width @math{w} and a height
@math{h}. For a single text line, @math{d} is descent below the
baseline, and @math{a+d=h}. For multiple text lines (often created
with a function like @racket[vc-append]), @math{a} is the ascent of
the top line, and @math{d} is the descent of the bottom line, so
@math{a+d<h}. Many picts have @math{d=0} and @math{a=h}.

In addition, a pict can have a @defterm{last} sub-pict that
corresponds to the last item on the last line of text, so that extra
lines can be added to the last line. In particular, the @defterm{last}
element is useful for adding closing parentheses to a block of Racket
code, where the last line of code not the longest line in the block.

The size information for a pict is computed when the pict is
created. This strategy supports programs that create new picts though
arbitrarily complex computations on the size and shape of existing
picts. The functions @racket[pict-width], @racket[pict-height],
@racket[pict-descent], and @racket[pict-ascent] extract bounding-box
information from a pict.

A pict is a convertible datatype through the @racketmodname[file/convertible]
protocol. Supported conversions include @racket['png-bytes],
@racket['eps-bytes], and @racket['pdf-bytes].


@defstruct[pict ([draw any/c]
                 [width real?]
                 [height real?]
                 [ascent real?]
                 [descent real?]
                 [children (listof child?)]
                 [panbox (or/c #f any/c)]
                 [last (or/c #f pict-path?)])]{

A @racket[pict] structure is normally not created directly with
@racket[make-pict]. Instead, functions like @racket[text],
@racket[hline], and @racket[dc] are used to construct a pict.

The @racket[draw] field contains the pict's drawing information in an
internal format. Roughly, the drawing information is a procedure that
takes a @racket[dc<%>] drawing context and an offset for the pict's
top-left corner (i.e., it's bounding box's top left corner relative to
the @racket[dc<%>] origin). The state of the @racket[dc<%>] is
intended to affect the pict's drawing; for example, the pen and brush
will be set for a suitable default drawing mode, and the
@racket[dc<%>] scale will be set to scale the resulting image. Use
@racket[draw-pict] (as opposed to @racket[pict-draw]) to draw the
picture.

The @racket[panbox] field is internal and initialized to @racket[#f].

The @racket[last] field indicates a pict within the @racket[children]
list (transitively) that can be treated as the last element of the
last line in the pict. A @racket[#f] value means that the pict is its
own last sub-pict.}


@defstruct[child ([pict pict?]
                  [dx real?]
                  [dy real?]
                  [sx real?]
                  [sy real?])]{

Records, for a pict constructed of other picts, the relative location
and scale of one nested pict.

A @racket[child] structure is normally not created directly with
@racket[make-child]. Instead, functions like @racket[hc-append] create
@racket[child] structures when combining picts to create a new one.}

@; ------------------------------------------------------------------------

@section{Basic Pict Constructors}

@defproc*[([(dc [draw ((is-a?/c dc<%>) real? real? . -> . any)]
                [w real?]
                [h real?])
            pict?]
           [(dc [draw ((is-a?/c dc<%>) real? real? . -> . any)]
                [w real?]
                [h real?]
                [a real?]
                [d real?])
            pict?])]{

Creates an arbitrary self-rendering pict.  The arguments to the
rendering procedure will be a device context and top-left location for
drawing.
  
When the rendering procedure is called, the current pen and brush will
be solid and in the pict's color (and linewidth), and the scale and
offset of the dc will be set. The text mode will be transparent, but
the font and text colors are not guaranteed to be anything in
particular.}

@defproc*[([(blank [size real? 0]) pict?]
           [(blank [w real?] [h real?]) pict?]
           [(blank [w real?] [a real?] [d real?]) pict?]
           [(blank [w real?] [h real?] [a real?] [d real?]) pict?])]{

Creates a pict that draws nothing. The one-argument case supplies a
value used for both the width and height. In the one- and two-argument
case, the ascent and descent are @math{0} for the resulting pict's
bounding box; in the three-argument case, the height is computed by
adding the given ascent and descent.}

@defproc[(text [content string?]
               [style text-style/c null]
               [size (integer-in 1 255) 12] 
               [angle real? 0])
         pict?]{

Creates a pict that draws text. For creating text picts within a slide
presentation, see @racket[t]. The size of the resulting pict may
depend on the value of @racket[dc-for-text-size].

The @racket[style] argument must be one of the following:

@itemize[

 @item{@racket[null] --- the default, same as @racket['default]}

 @item{a @racket[font%] object}

 @item{a font family symbol, such a @racket['roman] (see @racket[font%])}

 @item{a font face string, such as @racket["Helvetica"] (see @racket[font%])}

 @item{@racket[(cons _str _sym)] combining a face string and a font
       family (in case the face is unavailable; see @racket[font%])}

 @item{@racket[(cons 'bold style)] for a valid @racket[style]}

 @item{@racket[(cons 'italic style)]}
 @item{@racket[(cons 'subscript style)]}
 @item{@racket[(cons 'superscript style)]}
 @item{@racket[(cons 'caps style)]}

 @item{@racket[(cons 'combine style)] --- allows kerning and ligatures
      (the default, unless the @racket['modern] family is specified)}

 @item{@racket[(cons 'no-combine style)] --- renders characters individually}
]

If both @racket['combine] and @racket['no-combine] are specified, the
first one takes precedence. If caps is specified, the angle must be
zero.

The given @racket[size] is in pixels, but it is ignored if a
@racket[font%] object is provided in the text-style.

The @racket[angle] is in radians, and positive values rotate
counter-clockwise. For a non-zero @racket[angle], the resulting
pict's bounding box covers the rotated text, and the descent is zero
and the ascent is the height.}


@defproc*[([(hline [w real?] [h real?] 
                   [#:segment seg-length (or/c #f real?) #f]) pict?]
           [(vline [w real?] [h real?] 
                   [#:segment seg-length (or/c #f real?) #f]) pict?])]{

Straight lines, centered within their bounding boxes.}

@defproc[(frame [pict pict?]
                [#:segment seg-length (or/c #f real?) #f]
                [#:color color (or/c #f string? (is-a?/c color<%>)) #f]
                [#:line-width width (or/c #f real?) #f])
          pict?]{

Frames a given pict. If the color or line width are provided, the
override settings supplied by the context.}

@defproc*[([(ellipse [w real?] [h real?]) pict?]
           [(circle [diameter real?]) pict?]
           [(filled-ellipse [w real?] [h real?]) pict?]
           [(disk [diameter real?]) pict?])]{

Unfilled and filled ellipses.}

@defproc*[([(rectangle [w real?] [h real?]) pict?]
           [(filled-rectangle [w real?]
                              [h real?] 
                              [#:draw-border? draw-border? any/c #t])
            pict?])]{

Unfilled and filled rectangles.

If @racket[draw-border?] is @racket[#f], then the pen is set to be transparent
before drawing the rectangle.
}

@defproc*[([(rounded-rectangle [w real?] [h real?] 
                               [corner-radius real? -0.25]
                               [#:angle angle real? 0])
            pict?]
           [(filled-rounded-rectangle [w real?] [h real?]
                                      [corner-radius real? -0.25]
                                      [#:angle angle real? 0])
            pict?])]{

Unfilled and filled rectangles with rounded corners.  The
@racket[corner-radius] is used to determine how much
rounding occurs in the corners. If it is a positive number,
then it determines the radius of a circle touching the edges
in each corner, and the rounding of the rectangle follow the
edge of those circles. If it is a negative number, then the
radius of the circles in the corners is the absolute value of the
@racket[corner-radius] times the smaller of @racket[width]
and @racket[height].

The @racket[angle] determines how much the rectangle is
rotated, in radians.
}

@defproc[(bitmap [img (or/c path-string? (is-a?/c bitmap%))])
         pict]{

A pict that display a bitmap. When a path is provided, the image is
loaded with the @racket['unknown/mask] flag, which means that a mask
bitmap is generated if the file contains a mask.

If the bitmap cannot be loaded, if the given @racket[bitmap%] object
is not valid, or if the @racket[bitmap-draft-mode] parameter is set to
@racket[#t], the result pict draws the word ``bitmap failed''.}

@defproc*[([(arrow [size real?] [radians real?]) pict?]
           [(arrowhead [size real?] [radians real?]) pict?])]{

Creates an arrow or arrowhead in the specific direction within a
@racket[size] by @racket[size] pict. Points on the arrow may extend
slightly beyond the bounding box.}

@defproc*[([(pip-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrow-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrows-line [dx real?] [dy real?] [size real?]) pict?])]{

Creates a line (with some number of arrowheads) as a zero-sized pict
suitable for use with @racket[pin-over]. The 0-sized picture contains
the starting point.

The @racket[size] is used for the arrowhead size. Even though
@racket[pip-line] creates no arrowheads, it accepts the @racket[size]
argument for consistency with the other functions.}

@defproc*[([(pin-line [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:under? under? any/c #f])
            pict?]
           [(pin-arrow-line [arrow-size real?] [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:under? under? any/c #f]
                      [#:solid? solid? any/c #t]
		      [#:hide-arrowhead? any/c #f])
            pict?]
           [(pin-arrows-line [arrow-size real?] [pict pict?]
                      [src pict-path?]
                      [find-src (pict? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:under? under? any/c #f]
                      [#:solid? solid? any/c #t]
		      [#:hide-arrowhead? any/c #f])
            pict?])]{

Adds a line or line-with-arrows onto @racket[pict], using one of the
pict-finding functions (e.g., @racket[lt-find]) to extract the source
and destination of the line.

If @racket[under?] is true, then the line and arrows are added under
the existing @racket[pict] drawing, instead of on top. If
@racket[solid?] is false, then the arrowheads are hollow instead of
filled.

The @racket[start-angle], @racket[end-angle], @racket[start-pull], and
@racket[end-pull] arguments control the curve of the line:

@itemize[

 @item{The @racket[start-angle] and @racket[end-angle] arguments
       specify the direction of curve at its start and end positions;
       if either is @racket[#f], it defaults to the angle of a
       straight line from the start position to end position.}

 @item{The @racket[start-pull] and @racket[end-pull] arguments specify
       a kind of momentum for the starting and ending angles; larger
       values preserve the angle longer.}

]

When the @racket[hide-arrowhead?] argument is a true value, then 
space for the arrowhead is left behind, but the arrowhead itself 
is not drawn.

The defaults produce a straight line.}

@defthing[text-style/c contract?]{

A contract that matches the second argument of @racket[text].}

@defboolparam[bitmap-draft-mode on?]{

A parameter that determines whether @racket[bitmap] loads/uses a
bitmap.}


@; ------------------------------------------------------------------------

@section{Pict Combiners}

@defproc*[([(vl-append [d real? 0.0] [pict pict?] ...) pict?]
           [(vc-append [d real? 0.0] [pict pict?] ...) pict?]
           [(vr-append [d real? 0.0] [pict pict?] ...) pict?]
           [(ht-append [d real? 0.0] [pict pict?] ...) pict?]
           [(htl-append [d real? 0.0] [pict pict?] ...) pict?]
           [(hc-append [d real? 0.0] [pict pict?] ...) pict?]
           [(hbl-append [d real? 0.0] [pict pict?] ...) pict?]
           [(hb-append [d real? 0.0] [pict pict?] ...) pict?])]{

Creates a new pict as a column (for @racket[v...-append]) or row (for
@racket[h...-append]) of other picts. The optional @racket[d] argument
specifies amount of space to insert between each pair of pictures in
making the column or row.

Different procedures align pictures in the orthogonal direction in
different ways. For example, @racket[vl-append] left-aligns all of the
pictures.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline. If at least one @racket[pict] is supplied,
then the last element (as reported by @racket[pict-last]) for the
result is @racket[(or (pict-last pict) pict)] for the using last
supplied @racket[pict].}

@defproc*[([(lt-superimpose [pict pict?] ...) pict?]
           [(ltl-superimpose [pict pict?] ...) pict?]
           [(lc-superimpose [pict pict?] ...) pict?]
           [(lbl-superimpose [pict pict?] ...) pict?]
           [(lb-superimpose [pict pict?] ...) pict?]
           [(ct-superimpose [pict pict?] ...) pict?]
           [(ctl-superimpose [pict pict?] ...) pict?]
           [(cc-superimpose [pict pict?] ...) pict?]
           [(cbl-superimpose [pict pict?] ...) pict?]
           [(cb-superimpose [pict pict?] ...) pict?]
           [(rt-superimpose [pict pict?] ...) pict?]
           [(rtl-superimpose [pict pict?] ...) pict?]
           [(rc-superimpose [pict pict?] ...) pict?]
           [(rbl-superimpose [pict pict?] ...) pict?]
           [(rb-superimpose [pict pict?] ...) pict?])]{

Creates a new picture by superimposing a set of pictures. The name
prefixes are alignment indicators: horizontal alignment then vertical
alignment.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline. The last element (as reported by
@racket[pict-last]) for the result is the lowest, right-most among the
last-element picts of the @racket[pict] arguments, as determined by
comparing the last-element bottom-right corners.}


@defproc*[([(pin-over [base pict?] [dx real?] [dy real?] [pict pict?])
            pict?]
           [(pin-over [base pict?] 
                      [find-pict pict-path?]
                      [find (pict? pict-path? . -> . (values real? real?))]
                      [pict pict?])
            pict?])]{

Creates a pict with the same bounding box, ascent, and descent as
@racket[base], but with @racket[pict] placed on top.  The @racket[dx]
and @racket[dy] arguments specify how far right and down the second
pict's corner is from the first pict's corner.  Alternately, the
@racket[find-pict] and @racket[find] arguments find a point in
@racket[base] for @racket[find-pict]; the @racket[find] procedure
should be something like @racket[lt-find].}


@defproc*[([(pin-under [base pict?] [dx real?] [dy real?] [pict pict?])
            pict?]
           [(pin-under [base pict?] 
                       [find-pict pict?]
                       [find (pict? pict? . -> . (values real? real?))]
                       [pict pict?])
            pict?])]{

Like @racket[pin-over], but @racket[pict] is drawn before
@racket[base] in the resulting combination.}


@defproc[(table [ncols exact-positive-integer?]
                [picts (listof pict?)]
                [col-aligns (table-list-of (pict? pict? -> pict?))]
                [row-aligns (table-list-of (pict? pict? -> pict?))]
                [col-seps (table-list-of real?)]
                [row-seps (table-list-of real?)])
         pict?]{

Creates a table given a list of picts. The @racket[picts] list is a
concatenation of the table's rows (which means that a Racket
@racket[list] call can be formatted to reflect the shape of the output
table).
  
The @racket[col-aligns], @racket[row-aligns], @racket[col-seps], and
@racket[row-seps] arguments are ``lists'' specifying the row and
columns alignments separation between rows and columns.  For @math{c}
columns and @math{r} rows, the first two should have @math{c} and
@math{r} superimpose procedures, and the last two should have
@math{c-1} and @math{r-1} numbers, respectively. The lists can be
``improper'' (i.e., ending in a number instead of an empty list), in
which case the non-pair cdr is used as the value for all remaining
list items that were expected. The @racket[col-aligns] and
@racket[row-aligns] procedures are used to superimpose all of the
cells in a column or row; this superimposition determines the total
width or height of the column or row, and also determines the
horizontal or vertical placement of each cell in the column or row.}

@; ------------------------------------------------------------------------

@section{Pict Drawing Adjusters}

@defproc*[([(scale [pict pict?] [factor real?]) pict?]
           [(scale [pict pict?] [w-factor real?] [h-factor real?]) pict?])]{

Scales a pict drawing, as well as its @tech{bounding-box}. The drawing
is scaled by adjusting the destination @racket[dc<%>]'s scale while
drawing the original @racket[pict].}

@defproc[(ghost [pict pict?]) pict?]{

Creats a container picture that doesn't draw the child picture,
but uses the child's size.}

@defproc[(linewidth [w (or/c real? #f)] [pict pict?]) pict?]{

Selects a specific pen width for drawing, which applies to pen drawing
for @racket[pict] that does not already use a specific pen width.
A @racket[#f] value for @racket[w] makes the pen transparent (in contrast
to a zero value, which means ``as thin as possible for the target device'').}

@defproc[(colorize [pict pict?] [color (or/c string? 
                                             (is-a?/c color%)
                                             (list (integer-in 0 255)
                                                   (integer-in 0 255)
                                                   (integer-in 0 255)))])
         pict?]{

Selects a specific color drawing, which applies to drawing in
@racket[pict] that does not already use a specific color. The
@racket[black-and-white] parameter causes all non-white colors to be
converted to black.}

@defproc[(cellophane [pict pict?] [opacity (real-in 0 1)])
         pict?]{

Makes the given @racket[pict] semi-transparent, where an opacity of
@racket[0] is fully transparent, and an opacity of @racket[1] is fully
opaque.  See @method[dc<%> set-alpha] for information about the
contexts and cases when semi-transparent drawing works.}

@defproc[(clip [pict pict?]) pict]{

Clips a pict's drawing to its @tech{bounding box}.}

@defproc*[([(inset/clip [pict pict?] [amt real?]) pict?]
           [(inset/clip [pict pict?] [h-amt real?] [v-amt real?]) pict?]
           [(inset/clip [pict pict?] [l-amt real?] [t-amt real?] 
                        [r-amt real?] [b-amt real?]) pict?])]{

Insets and clips the pict's drawing to its @tech{bounding
box}. Usually, the inset amounts are negative.}

@defform*[[(scale/improve-new-text pict-expr scale-expr)
           (scale/improve-new-text pict-expr x-scale-expr y-scale-expr)]]{

Like the @racket[scale] procedure, but also sets
@racket[current-expected-text-scale] while evaluating @racket[pict-expr].}

@defboolparam[black-and-white on?]{

A parameter that determines whether @racket[colorize] uses color or
black-and-white colors.}

@; ------------------------------------------------------------------------

@section{Bounding-Box Adjusters}

@defproc*[([(inset [pict pict?] [amt real?]) pict?]
           [(inset [pict pict?] [h-amt real?] [v-amt real?]) pict?]
           [(inset [pict pict?] [l-amt real?] [t-amt real?] 
                   [r-amt real?] [b-amt real?]) pict?])]{

Extends @racket[pict]'s @tech{bounding box} by adding the given amounts
to the corresponding sides; ascent and descent are extended, too.}


@defproc[(clip-descent [pict pict?]) pict?]{

Truncates @racket[pict]'s @tech{bounding box} by removing the descent part.}


@defproc[(lift-above-baseline [pict pict?] [amt real?]) pict?]{

Lifts @racket[pict] relative to its baseline, extending the
@tech{bounding-box} height if necessary.}

@defproc[(drop-below-ascent [pict pict?] [amt real?]) pict?]{

Drops @racket[pict] relative to its ascent line, extending the
@tech{bounding-box} height if necessary.}

@defproc[(baseless [pict pict?]) pict?]{

Makes the descent @racket[0] and the ascent the same as the height.}

@defproc[(refocus [pict pict?] [sub-pict pict?]) pict?]{

Assuming that @racket[sub-pict] can be found within @racket[pict],
shifts the overall bounding box to that of @racket[sub-pict] (but
preserving all the drawing of @racket[pict]). The last element, as
reported by @racket[pict-last] is also set to @racket[(or (pict-last
sub-pict) sub-pict)].}

@defproc[(panorama [pict pict?]) pict?]{

Shifts the given pict's bounding box to enclose the bounding boxes of
all sub-picts (even @racket[launder]ed picts).}

@defproc[(use-last [pict pict?] [sub-pict pict-path?]) pict?]{

Returns a pict like @racket[pict], but with the last element (as
reported by @racket[pict-last]) set to @racket[sub-pict]. The
@racket[sub-pict] must exist as a sub-pict (or path of sub-picts)
within @racket[pict].}

@defproc[(use-last* [pict pict?] [sub-pict pict-path?]) pict?]{

Propagates the last element of @racket[sub-pict] to @racket[pict].

That is, @racket[use-last*] is like @racket[use-last], but the last
element of @racket[sub-pict] is used as the new last element for
@racket[pict], instead of @racket[sub-pict] itself---unless
@racket[(pict-last sub-pict)] is @racket[#f], in which case
@racket[sub-pict] is used as the last element of @racket[pict].}

@; ------------------------------------------------------------------------

@section{Pict Finders}

@defproc*[([(lt-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ltl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(lb-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ct-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(ctl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(cb-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rt-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rtl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rc-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rbl-find [pict pict?] [find pict-path?]) (values real? real?)]
           [(rb-find [pict pict?] [find pict-path?]) (values real? real?)])]{

Locates a pict designated by @racket[find] is within @racket[pict]. If
@racket[find] is a pict, then the @racket[pict] must have been created
as some combination involving @racket[find].

If @racket[find] is a list, then the first element of @racket[find]
must be within @racket[pict], the second element of @racket[find] must
be within the second element, and so on.}

@defproc[(pict-path? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[pict] or a non-empty
list of @racket[pict]s.}

@defproc[(launder [pict pict?]) pict?]{

Creates a pict that has the same drawing and bounding box of
@racket[pict], but which hides all of its sub-picts so that they
cannot be found with functions like @racket[lt-find]. If @racket[pict]
has a last-line pict, then the laundered pict has a fresh last-line
pict with the same shape and location.}

@; ------------------------------------------------------------------------

@section{More Pict Constructors}

@; ----------------------------------------

@subsection{Dingbats}

@defproc[(cloud [w real?]
                [h real?] 
                [color (or/c string? (is-a?/c color%)) "gray"])
         pict?]{

Creates a fluffy cloud.}

@defproc[(file-icon [w real?]
                    [h real?] 
                    [color (or/c string? (is-a?/c color%) any/c)]
                    [shaded? any/c #f])
         pict?]{

Creates a Mac-like file icon, optionally shaded. If @racket[color] is
not a string or @racket[color%] object, it is treated as a boolean, in
which case true means @racket["gray"] and false means
@racket["white"].}

@defproc[(standard-fish [w real?]
                        [h real?] 
                        [#:direction direction (or/c 'left 'right) 'left] 
                        [#:color color (or/c string? (is-a?/c color%)) "blue"] 
                        [#:eye-color eye-color (or/c string? (is-a?/c color%) #f) "black"]
                        [#:open-mouth open-mouth (or/c boolean? real?) #f])
         pict?]{

Creates a fish swimming either @racket['left] or @racket['right].
If @racket[eye-color] is @racket[#f], no eye is drawn.

The @racket[open-mouth] argument can be either @racket[#f] (mouth
closed), @racket[#t] (mouth fully open), or a number: @racket[0.0] is
closed, @racket[1.0] is fully open, and numbers in between are
partially open.}

@defproc[(jack-o-lantern [size real?]
                         [pumpkin-color (or/c string? (is-a?/c color%)) "orange"] 
                         [face-color (or/c string? (is-a?/c color%)) "black"])
         pict?]{

Creates a jack-o-lantern; use the same pumpkin and face color to get a
plain pumpkin. The @racket[size] determines the width.}

@defproc[(angel-wing [w real?]
                     [h real?] 
                     [left? any/c])
         pict?]{

Creates an angel wing, left or right, or any size.  The color and pen
width for drawing the wing outline is the current one.}

@defproc[(desktop-machine [scale real?]
                          [style (listof symbol?) null])
         pict?]{

Produces a picture of ancient desktop computer. The @racket[scale]
argument scales the size relative to the base size of 120 by 115. 

The @racket[style] can include any of the following:

@itemlist[

 @item{@racket['plt] --- include a Racket logo on the machine's screen}

 @item{@racket['binary] --- put 1s and 0s on the machine's screen}

 @item{@racket['devil] --- like @racket['binary], and also give the machine 
                           horns and a tail}

]}

@; ----------------------------------------

@subsection{Balloon Annotations}

@defmodule[slideshow/balloon]{The @racketmodname[slideshow/balloon]
library provides functions for creating and placing cartoon-speech
balloons.}

@defproc[(wrap-balloon [pict pict?]
                       [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                       [dx real?]
                       [dy real?]
                       [color (or/c string? (is-a?/c color%)) balloon-color]
                       [corner-radius (and/c real? (not/c negative?)) 32])
         balloon?]{

Superimposes @racket[pict] on top of a balloon that wraps it.

The @racket[spike] argument indicates the corner from which a spike
protrudes from the balloon (i.e., the spike that points to whatever
the balloon is about). For example, @racket['n] means ``north,'',
which is a spike in the top middle of the balloon.

The @racket[dx] and @racket[dy] arguments specify how far the spike
should protrude.  For a @racket['w] spike, @racket[dx] should be
negative, etc.

The @racket[color] argument is the background color for the balloon.

The @racket[corner-radius] argument determines the radius of the cicle
used to roun the balloon's corners. As usual, if it is less than
@racket[1], then it acts as a ratio of the balloon's width or height.

The result is a balloon, not a pict. The @racket[balloon-pict]
function extracts a pict whose bounding box does not include the
spike, but includes the rest of the image, and the
@racket[balloon-point-x] and @racket[balloon-point-y] functions
extract the location of the spike point. More typically, the
@racket[pin-balloon] function is used to add a balloon to a pict.}

@defproc[(pip-wrap-balloon [pict pict?]
                           [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                           [dx real?]
                           [dy real?]
                           [color (or/c string? (is-a?/c color%)) balloon-color]
                           [corner-radius (and/c real? (not/c negative?)) 32])
         pict?]{

Like @racket[wrap-balloon], but produces a zero-sized pict suitable
for use with @racket[pin-over].}


@defproc*[([(pin-balloon [balloon balloon?]
                         [base pict?]
                         [x real?]
                         [y real?])
            pict?]
           [(pin-balloon [balloon balloon?]
                         [base pict?]
                         [at-pict pict-path?]
                         [find (pict? pict-path? . -> . (values real? real?))])
            pict?])]{

Superimposes the pict in @racket[balloon] onto @racket[base] to
produce a new pict. The balloon is positioned so that its spike points
to the location specified by either @racket[x] and @racket[y]
(numbers) or at the position determined by combining @racket[base] and
@racket[at-pict] with @racket[find]. The @racket[find] function uses
its arguments like @racket[lt-find].

The resulting pict has the same bounding box, descent, and ascent as
@racket[base], even if the balloon extends beyond the bounding box.}


@defproc[(balloon [w real?]
                  [h real?]
                  [corner-radius (and/c real? (not/c negative?))]
                  [spike (or/c 'n 's 'e 'w 'ne 'se 'sw 'nw)]
                  [dx real?]
                  [dy real?]
                  [color (or/c string? (is-a?/c color%)) balloon-color])
         balloon?]{

Creates a balloon, much like @racket[wrap-balloon] except that the balloon's
width is @racket[w] and its height is @racket[h].}

@defproc*[([(balloon? [v any/c]) boolean?]
           [(make-balloon [pict pict?] [x real?] [y real?]) balloon?]
           [(balloon-pict [balloon balloon?]) pict?]
           [(balloon-point-x [balloon balloon?]) real?]
           [(balloon-point-y [balloon balloon?]) real?])]{

A balloon encapsulates a pict and the position of the balloon's spike
relative to the balloon's top-left corner.}

@defthing[balloon-color (or/c string? (is-a?/c color%))]

The default background color for a balloon.

@; ----------------------------------------

@subsection{Face}

@defmodule[slideshow/face]{The @racketmodname[slideshow/face] library
provides functions for a kind of @as-index{Mr. Potatohead}-style face
library.}

@defthing[default-face-color (or/c string (is-a?/c color%))]{

Orange.}

@defproc[(face [mood symbol?]
               [color (or/c string (is-a?/c color%)) default-face-color])
         pict?]{

Returns a pict for a pre-configured face with the given base
color. The built-in configurations, selected by mood-symbol, are as
follows:

@itemize[

    @item{@racket['unhappy] --- @racket[(face* 'none 'plain #t default-face-color 6)]}
    @item{@racket['sortof-unhappy] --- @racket[(face* 'worried 'grimace #t default-face-color 6)]}
    @item{@racket['sortof-happy] --- @racket[(face* 'worried 'medium #f default-face-color 6)]}
    @item{@racket['happy] --- @racket[(face* 'none 'plain #f default-face-color 6)]}
    @item{@racket['happier] --- @racket[(face* 'none 'large #f default-face-color 3)]}
    @item{@racket['embarrassed] --- @racket[(face* 'worried 'medium #f default-face-color 3)]}
    @item{@racket['badly-embarrassed] --- @racket[(face* 'worried 'medium #t default-face-color 3)]}
    @item{@racket['unhappier] --- @racket[(face* 'normal 'large #t default-face-color 3)]}
    @item{@racket['happiest] --- @racket[(face* 'normal 'huge #f default-face-color 0 -3)]}
    @item{@racket['unhappiest] --- @racket[(face* 'normal 'huge #t default-face-color 0 -3)]}
    @item{@racket['mad] --- @racket[(face* 'angry 'grimace #t default-face-color 0)]}
    @item{@racket['mean] --- @racket[(face* 'angry 'narrow #f default-face-color 0)]}
    @item{@racket['surprised] --- @racket[(face* 'worried 'oh #t default-face-color -4 -3 2)]}

]}

@defproc[(face* [eyebrow-kind (or/c 'none 'normal 'worried 'angry)]
                [mouth-kind (or/c 'plain 'smaller 'narrow 'medium 'large 
                                  'huge 'grimace 'oh 'tongue)]
                [frown? any/c]
                [color (or/c string (is-a?/c color%))]
                [eye-inset real?]
                [eyebrow-dy real?]
                [pupil-dx real?]
                [pupil-dy real?]
                [#:eyebrow-shading? eyebrow-on? any/c #t]
                [#:mouth-shading? mouth-on? any/c #t]
                [#:eye-shading? eye-on? any/c #t]
                [#:tongue-shading? tongue-on? any/c #t]
                [#:face-background-shading? face-bg-on? any/c #t]
                [#:teeth? teeth-on? any/c #t])
         pict?]{

Returns a pict for a face:

@itemize[

 @item{@racket[eyebrow-kind] determines the eyebrow shape.}

 @item{@racket[mouth-kind] determines the mouth shape, combined with
       @racket[frown?].}

 @item{@racket[frown?] determines whether the mouth is up or down.}

 @item{@racket[color] determines the face color.}

 @item{@racket[eye-inset] adjusts the eye size; recommend values are
       between 0 and 10.}

 @item{@racket[eyebrow-dy] adjusts the eyebrows; recommend values:
       between -5 and 5.}

 @item{@racket[pupil-dx] adjusts the pupil; recommend values are
       between -10 and 10.}

 @item{@racket[pupil-dy] adjusts the pupil; recommend values are
       between -15 and 15.}

]

The @racket[#:eyebrow-shading?] through
@racket[#:face-background-shading?] arguments control whether a
shading is used for on a particular feature in the face (shading tends
to look worse than just anti-aliasing when the face is small). The
@racket[#:teeth?] argument controls the visibility of the teeth for
some mouth shapes.}

@; ----------------------------------------

@subsection{Flash}

@defmodule[slideshow/flash]

@defproc[(filled-flash [width real?]
                       [height real?]
                       [n-points exact-positive-integer? 10]
                       [spike-fraction (real-in 0 1) 0.25]
                       [rotation real? 0])
         pict?]{

Returns a pict for a ``flash'': a spiky oval, like the yellow
background that goes behind a ``new!'' logo on web pages or a box of
cereal.
  
The @racket[height] and @racket[width] arguments determine the size of
the oval in which the flash is drawn, prior to rotation. The actual
height and width may be smaller if @racket[points] is not a multiple
of 4, and the actual height and width will be different if the flash
is rotated.

The @racket[n-points] argument determines the number of points on the
flash.

The @racket[spike-fraction] argument determines how big the flash
spikes are compared to the bounding oval.

The @racket[rotation] argument specifies an angle in radians for
counter-clockwise rotation.

The flash is drawn in the default color.}

@defproc[(outline-flash [width real?]
                        [height real?]
                        [n-points exact-positive-integer? 10]
                        [spike-fraction (real-in 0 1) 0.25]
                        [rotation real? 0])
         pict?]{

Like @racket[filled-flash], but drawing only the outline.}

@; ------------------------------------------------------------------------

@section{Miscellaneous}

@defproc[(hyperlinkize [pict pict?])
         pict?]{

Adds an underline and blue color. The @racket[pict]'s height and
descent are extended.}


@defproc[(scale-color [factor real?]
                      [color (or/c string (is-a?/c color%))])
         (is-a?/c color%)]{

Scales a color, making it brighter or darker. If the factor is less
than 1, the color is darkened by multiplying the RGB components by the
factor. If the factor is greater tham 1, the color is lightened by
dividing the gap between the RGB components and 255 by the factor.}

@defproc[(color-series [dc (is-a?/c dc<%>)]
                       [max-step exact-nonnegative-integer?]
                       [step-delta (and/c exact? positive?)]
                       [start (or/c string? (is-a?/c color%))]
                       [end (or/c string? (is-a?/c color%))]
                       [proc (exact? . -> . any)]
                       [set-pen? any/c]
                       [set-brush? any/c])
          void?]{

Calls a @racket[proc] multiple times, gradually changing the pen
and/or brush color for each call. For the first call, the current pen
and/or brush color matches @racket[start]; for the last call, it
matches racket[end]; and for intermediate calls, the color is an
intermediate color.

The @racket[max-step] and @racket[step-delta] arguments should be
exact numbers; the procedure is called with each number from 0 to
@racket[max-step] inclusive using a @racket[step-delta] increment.}

@; ------------------------------------------------------------------------

@section{Rendering}

@defparam[dc-for-text-size dc (or/c #f (is-a?/c dc<%>))]{

A parameter that is used to determine the @tech{bounding box} of picts
created with @racket[text].

The drawing context installed in this parameter need not be the same
as the ultimate drawing context, but it should measure text in the same
way. Under normal circumstances, font metrics are the same for all
drawing contexts, so the default value of @racket[dc-for-text-size] is
a @racket[bitmap-dc%] that draws to a 1-by-1 bitmap.}


@defproc[(draw-pict [pict pict?]
                    [dc (is-a?/c dc<%>)]
                    [x real?]
                    [y real?])
         void?]{

Draws @racket[pict] to @racket[dc], with its top-left corner at offset
 (@racket[x], @racket[y]).}


@defproc[(make-pict-drawer [pict pict?])
         ((is-a?/c dc<%>) real? real? . -> . void?)]{

Generates a pict-drawer procedure for multiple renderings of
@racket[pict]. Using the generated procedure can be faster than
repeated calls to @racket[draw-pict].}


@defproc[(show-pict [pict pict?]
                    [w (or/c #f exact-nonnegative-integer?) #f] 
                    [h (or/c #f exact-nonnegative-integer?) #f])
         void?]{

Opens a frame that displays @racket[pict].  The frame adds one method,
@racket[set-pict], which takes a pict to display. The optional
@racket[w] and @racket[h] arguments specify a minimum size for the
frame's drawing area.}

@defparam[current-expected-text-scale scales (list real? real?)]{

A parameter used to refine text measurements to better match an
expected scaling of the image. The @racket[scale/improve-new-text]
form sets this parameter while also scaling the resulting pict.}
