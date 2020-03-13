#lang scribble/doc
@(require "common.rkt")

@gametitle["GCalc" "gcalc" "Visual λ-Calculus"]

@onscreen{GCalc} is a system for visually demonstrating the
λ-Calculus (not really a game).

See the following for the principles:

@centerline{@selflink{http://www.grame.fr/Research/GCalcul/Graphic_Calculus.html}}
@centerline{@selflink{ftp://ftp.grame.fr/pub/Documents/ICMC94LambdaCalc.pdf}}


@section{The Window Layout}

The window is divided into three working areas, each made of cells.
Cells hold cube objects, which can be dragged between cells (with a
few exceptions that are listed below).  The working areas are as
follows:

@itemize[

@item{The right side is the storage area.  This is used for saving
   objects -- drag any cube to/from here.  Note that cubes can be
   named for convenience.}

@item{The left side is a panel of basic color cubes.  These cells
   always contain a set of basic cubes that are used as the primitive
   building blocks all other values are made of.  They cannot be
   overwritten.  (Note that this includes a transparent cell.)}

@item{The center part is the working panel.  This is the main panel where
   new cubes are constructed.  The center cell is similar to a storage
   cell, and the surrounding eight cells all perform some operation on
   this cell.}

]

@section{User Interaction}

Right-click any cell except for the basic colors on the left panel, or
hit escape or F10 for a menu of operations.  The menu also includes
the keyboard shortcuts for these operations.


@section{Cube operations}

There are six simple operations that are considered part of the simple
graphic cube world.  The operations correspond to six of the operation
cells: a left-right composition is built using the left and the right
cells, a top-bottom using the top and the bottom, and a front-back
using the top-left and bottom-right.  Dragging a cube to one of these
cells will use the corresponding operator to combine it with the main
cell's cube.  Using a right mouse click on one of these cells can be
used to cancel dragging an object to that cell, this is not really an
undo feature: a right-click on the right cell always splits the main
cube to two halves and throws the right side.

The colored cubes and the six basic operators make this simple domain,
which is extended to form a λ-Calculus-like language by adding
abstractions and applications.  Right-clicking on a basic cube on the
left panel creates an abstraction which is actually a lambda
expression except that colors are used instead of syntactic variables.
For example, if the main cell contains @onscreen{R|G} (red-green on
the left and right), then right-clicking the green cube on the left
panel leaves us with @onscreen{λ G . R|G}, which is visualized
as @onscreen{R|G} with a green circle.  The last two operator cells
are used for application of these abstractions: drag a function to the
top-right to have it applied on the main cube, or to the bottom-left
to have the main cube applied to it.  As in the λ-Calculus,
all abstractions have exactly one variable, use currying for multiple
variables.

So far the result is a domain of colored cubes that can be used in the
same way as the simple λ-Calculus.  There is one last
extension that goes one step further: function cubes can themselves be
combined with other functions using the simple operations.  This
results in a form of "spatial functions" that behave differently in
different parts of the cube according to the construction.  For
example, a left-right construction of two functions @onscreen{f|g}
operates on a given cube by applying @onscreen{f} on its left part and
@onscreen{g} on its right part.  You can use the preferences dialog to
change a few aspects of the computation.

Use the @onscreen{Open Example} menu entry to open a sample file that
contains lots of useful objects: Church numerals, booleans, lists,
Y-combinator, etc.
