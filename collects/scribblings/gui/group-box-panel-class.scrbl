#lang scribble/doc
@(require "common.ss")

@defclass/title[group-box-panel% vertical-panel% ()]{

A group-box panel arranges its subwindows in a single column, but also
 draws an optional label at the top of the panel and a border around
 the panel content.

Unlike most panel classes, a group-box panel's horizontal and vertical
 margins default to @scheme[2].


@defconstructor[([label label-string?]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%)
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [style (listof (one-of/c 'deleted)) null]
                 [font (is-a?/c font%) small-control-font]
                 [enabled any/c #t]
                 [vert-margin (integer-in 0 1000) 2]
                 [horiz-margin (integer-in 0 1000) 2]
                 [border (integer-in 0 1000) 0]
                 [spacing (integer-in 0 1000) 0]
                 [alignment (list/c (one-of/c 'left 'center 'right)
                                    (one-of/c 'top 'center 'bottom))
                            '(center top)]
                 [min-width (integer-in 0 10000) _graphical-minimum-width]
                 [min-height (integer-in 0 10000) _graphical-minimum-height]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

Creates a group pane whose title is @scheme[label].

@DeletedStyleNote{group panel}

@FontKWs[] @WindowKWs[] @SubareaKWs[] @AreaKWs[]


}}

