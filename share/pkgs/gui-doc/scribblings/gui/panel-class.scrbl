#lang scribble/doc
@(require "common.rkt")

@centered{@image[#:suffixes @list[".png"]]{image/panel}}

@defclass/title[panel% object% (area-container-window<%> subwindow<%>)]{

A panel is a both a container and a containee window. It serves mainly
 as a geometry management device, but the @racket['border] creates a
 container with a border. Unlike a @racket[pane%] object, a @racket[panel%]
 object can be hidden or disabled.

A @racket[panel%] object has a degenerate placement strategy for
 managing its children: it places each child as if it was the only
 child of the panel.  The @racket[horizontal-panel%] and
 @racket[vertical-panel%] classes provide useful geometry management
 for multiple children.

@history[#:changed "1.3" @elem{Changed the placement strategy to
                               stretch and align children, instead of
                               placing all children at the top-left
                               corner.}]

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (or/c 'border 'deleted
                                      'hscroll 'auto-hscroll
                                      'vscroll 'auto-vscroll)) null]
                 [enabled any/c #t]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(center center)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

If the @racket['border] style is specified, the window is created with
 a thin border (in which case the client size of the panel may be
 less than its total size). @DeletedStyleNote[@racket[style] @racket[parent]]{panel}

If the @racket['hscroll] or @racket['vscroll] style is specified, then
 the panel includes a scrollbar in the corresponding direction, and
 the panel's own size in the corresponding direction is not
 constrained by the size of its children subareas. The @racket['auto-hscroll]
 and @racket['auto-vscroll] styles are like @racket['hscroll] or
 @racket['vscroll], but they cause the corresponding scrollbar to
 disappear when no scrolling is needed in the corresponding direction;
 the @racket['auto-vscroll] and @racket['auto-hscroll] modes assume that
 children subareas are placed using the default algorithm for a @racket[panel%],
 @racket[vertical-panel%], or @racket[horizontal-panel%].

@WindowKWs[@racket[enabled]] @SubareaKWs[] @AreaContKWs[] @AreaKWs[]

}}

