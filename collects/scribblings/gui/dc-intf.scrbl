#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@definterface/title[dc<%> ()]{

A @scheme[dc<%>] object is a drawing context for drawing graphics and
 text.  It represents output devices in a generic way; e.g., a canvas
 has a drawing context, as does a printer.

The drawing methods, such as @method[dc<%> draw-rectangle], accept
real number values as arguments, but the results are only well-defined
when the drawing coordinates are in the range @scheme[-16383] to
@scheme[16383]. This restriction applies to the coordinates both
before and after offsets and scaling factors are applied.

@defmethod[(clear)
           void?]{

Clears the drawing region (fills it with the current background color,
as determined by @method[dc<%> get-background]).

}

@defmethod[(draw-arc [x real?]
                     [y real?]
                     [width (and/c real? (not/c negative?))]
                     [height (and/c real? (not/c negative?))]
                     [start-radians real?]
                     [end-radians real?])
           void?]{

Draws a counter-clockwise circular arc, a part of the ellipse
 inscribed in the rectangle specified by @scheme[x] (left), @scheme[y]
 (top), @scheme[width], and @scheme[height]. The arc starts at the angle
 specified by @scheme[start-radians] (@scheme[0] is three o'clock and
 half-pi is twelve o'clock) and continues counter-clockwise to
 @scheme[end-radians]. If @scheme[start-radians] and @scheme[end-radians] are
 the same, a full ellipse is drawn.

The current pen is used for the arc. If the current brush is not
 transparent, it is used to fill the wedge bounded by the arc plus
 lines (not drawn) extending to the center of the inscribed ellipse.

If both the pen and brush are non-transparent, the wedge is filled
 with the brush before the arc is drawn with the pen. The wedge and
 arc meet so that no space is left between them, but the precise
 overlap between the wedge and arc is platform- and size-specific.
 Typically, the regions drawn by the brush and pen overlap.  More
 generally, the pen is centered over the outline of the arc, rounding
 toward the center in unsmoothed mode.

@|DrawSizeNote|

}


@defmethod[(draw-bitmap [source (is-a?/c bitmap%)]
                        [dest-x real?]
                        [dest-y real?]
                        [style (one-of/c 'solid 'opaque 'xor) 'solid]
                        [color (is-a?/c color%) (send the-color-database find-color "black")]
                        [mask (or/c (is-a?/c bitmap%) false/c) #f])
           boolean?]{

Displays a bitmap. The @scheme[dest-x] and @scheme[dest-y] arguments
 are in DC coordinates.

For color bitmaps, the drawing style and color arguments are
 ignored. For monochrome bitmaps, @method[dc<%> draw-bitmap] uses the
 style and color arguments in the same way that a brush uses its style
 and color settings to draw a monochrome stipple (see @scheme[brush%]
 for more information).

If a mask bitmap is supplied, it must have the same width and height
 as the bitmap to display, and its @method[bitmap% ok?] must return
 true, otherwise @|MismatchExn|. The bitmap to draw and the mask
 bitmap can be the same object, but if the drawing context is a
 @scheme[bitmap-dc%] object, both bitmaps must be distinct from the
 destination bitmap, otherwise @|MismatchExn|.

If the mask bitmap is monochrome, drawing occurs in the target
 @scheme[dc<%>] only where the mask bitmap contains black pixels.

If the mask bitmap is grayscale and the bitmap to draw is not
 monochrome, then the blackness of each mask pixel controls the
 opacity of the drawn pixel (i.e., the mask acts as an inverted alpha
 channel), at least on most platforms. (Non-monochrome masks
 are collapsed to monochrome under X when the RENDER extension is not
 available, and under Windows 95 and NT when @file{msing32.dll} is not
 available.) Other combinations involving a non-monochrome mask (i.e.,
 a non-grayscale mask or a monochrome bitmap to draw) produce
 platform-specific results.

The current brush, current pen, and current text settings for the DC
 have no effect on how the bitmap is drawn, but the bitmap is scaled
 if the DC has a scale.

For @scheme[post-script-dc%] output, the mask bitmap is currently
 ignored, and the @scheme['solid] style is treated the same as
 @scheme['opaque]. (However, mask bitmaps and @scheme['solid] drawing
 may become supported for @scheme[post-script-dc%] in the future.)

The result is @scheme[#t] if the bitmap is successfully drawn,
 @scheme[#f] otherwise (possibly because the bitmap's @method[bitmap%
 ok?] method returns @scheme[#f]).

See also @method[dc<%> draw-bitmap-section].

@|DrawSizeNote|

}

@defmethod[(draw-bitmap-section [source (is-a?/c bitmap%)]
                                [dest-x real?]
                                [dest-y real?]
                                [src-x real?]
                                [src-y real?]
                                [src-width (and/c real? (not/c negative?))]
                                [src-height (and/c real? (not/c negative?))]
                                [style (one-of/c 'solid 'opaque 'xor) 'solid]
                                [color (is-a?/c color%) (send the-color-database find-color "black")]
                                [mask (or/c (is-a?/c bitmap%) false/c) #f])
           boolean?]{

Displays part of a bitmap.

The @scheme[src-x], @scheme[src-y], @scheme[src-width], and
 @scheme[src-height] arguments specify a rectangle in the source
 bitmap to copy into this drawing context.

See @method[dc<%> draw-bitmap] for information about @scheme[dest-x],
 @scheme[dest-y], @scheme[style], @scheme[color], and @scheme[mask].

}

@defmethod[(draw-ellipse [x real?]
                         [y real?]
                         [width (and/c real? (not/c negative?))]
                         [height (and/c real? (not/c negative?))])
           void?]{

Draws an ellipse contained in a rectangle with the given top-left
 corner and size. The current pen is used for the outline, and the
 current brush is used for filling the shape.

If both the pen and brush are non-transparent, the ellipse is filled
 with the brush before the outline is drawn with the pen. The filling
 and outline meet so that no space is left between them, but the
 precise overlap between the filling and outline is platform- and
 size-specific.  Typically, the regions drawn by the brush and pen
 overlap.  More generally, the pen is centered over the outline of the
 ellipse, rounding toward the center in unsmoothed mode.

@|DrawSizeNote|

}

@defmethod[(draw-line [x1 real?]
                      [y1 real?]
                      [x2 real?]
                      [y2 real?])
           void?]{

Draws a line from one point to another.  The current pen is used for
 drawing the line.

In unsmoothed mode, the points correspond to pixels, and the line
 covers both the start and end points. For a pen whose scaled width is
 larger than @scheme[1], the line is drawn centered over the start and
 end points.

See also @method[dc<%> set-smoothing] for information on the
@scheme['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-lines [points (listof (is-a?/c point%))]
                       [xoffset real? 0]
                       [yoffset real? 0])
           void?]{

Draws lines using a list of @scheme[points], adding @scheme[xoffset]
 and @scheme[yoffset] to each point. The current pen is used for
 drawing the lines.

See also @method[dc<%> set-smoothing] for information on the
 @scheme['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-path [path (is-a?/c dc-path%)]
                      [xoffset real? 0]
                      [yoffset real? 0]
                      [fill-style (one-of/c 'odd-even 'winding) 'odd-even])
           void?]{

Draws the sub-paths of the given @scheme[dc-path%] object, adding
 @scheme[xoffset] and @scheme[yoffset] to each point. The current pen
 is used for drawing the path as a line, and the current brush is used
 for filling the area bounded by the path.

If both the pen and brush are non-transparent, the path is filled with
 the brush before the outline is drawn with the pen. The filling and
 outline meet so that no space is left between them, but the precise
 overlap between the filling and outline is platform- and
 size-specific.  Thus, the regions drawn by the brush and pen may
 overlap. More generally, the pen is centered over the path, rounding
 left and down in unsmoothed mode.

The @scheme[fill-style] argument specifies the fill rule:
 @scheme['odd-even] or @scheme['winding]. In @scheme['odd-even] mode, a
 point is considered enclosed within the path if it is enclosed by an
 odd number of sub-path loops. In @scheme['winding] mode, a point is
 considered enclosed within the path if it is enclosed by more or less
 clockwise sub-path loops than counter-clockwise sub-path loops. In
 unsmoothed mode, the @scheme['winding] fill rule is not supported
 under Mac OS X and it is not supported when @scheme[path] contains
 multiple sub-paths; the @scheme['winding] fill rules is always
 supported when smoothing is enabled (see
@method[dc<%> set-smoothing]).

See also @method[dc<%> set-smoothing] for information on the
 @scheme['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-point [x real?]
                       [y real?])
           void?]{

Plots a single point using the current pen.

@|DrawSizeNote|

}

@defmethod[(draw-polygon [points (listof (is-a?/c point%))]
                         [xoffset real? 0]
                         [yoffset real? 0]
                         [fill-style (one-of/c 'odd-even 'winding) 'odd-even])
           void?]{

Draw a filled polygon using a list of @scheme[points], adding
 @scheme[xoffset] and @scheme[yoffset] to each point. The polygon is
 automatically closed, so the first and last point can be
 different. The current pen is used for drawing the outline, and the
 current brush for filling the shape.

If both the pen and brush are non-transparent, the polygon is filled
 with the brush before the outline is drawn with the pen. The filling
 and outline meet so that no space is left between them, but the
 precise overlap between the filling and outline is platform- and
 shape-specific.  Thus, the regions drawn by the brush and pen may
 overlap. More generally, the pen is centered over the polygon lines,
 rounding left and down in unsmoothed mode.

The @scheme[fill-style] argument specifies the fill rule:
 @scheme['odd-even] or @scheme['winding]. In @scheme['odd-even] mode, a
 point is considered enclosed within the polygon if it is enclosed by
 an odd number of loops. In @scheme['winding] mode, a point is
 considered enclosed within the polygon if it is enclosed by more or
 less clockwise loops than counter-clockwise loops. The
 @scheme['winding] fill rule is not supported under Mac OS X,
 except when smoothing is enabled (see
@method[dc<%> set-smoothing]).

See also @method[dc<%> set-smoothing] for information on the
 @scheme['aligned] smoothing mode.

@|DrawSizeNote|

}


@defmethod[(draw-rectangle [x real?]
                           [y real?]
                           [width (and/c real? (not/c negative?))]
                           [height (and/c real? (not/c negative?))])
           void?]{

Draws a rectangle with the given top-left corner and size.  The
 current pen is used for the outline and the current brush for filling
 the shape.

If both the pen and brush are non-transparent, the rectangle is filled
 with the brush before the outline is drawn with the pen. In
 unsmoothed mode, when the pen is size 0 or 1, the filling precisely
 overlaps the entire outline. As a result, if a rectangle is drawn
 with a size-0 or size-1 @scheme['xor] @scheme[pen%] and an
 @scheme['xor] @scheme[brush%], the outline is xored twice (first by
 the brush, then by the pen), leaving it unchanged. More generally,
 the pen is centered over the outline of the rectangle, rounding
 toward the center in unsmoothed mode.

See also @method[dc<%> set-smoothing] for information on the
@scheme['aligned] smoothing mode.

@|DrawSizeNote|

}


@defmethod[(draw-rounded-rectangle [x real?]
                                   [y real?]
                                   [width (and/c real? (not/c negative?))]
                                   [height (and/c real? (not/c negative?))]
                                   [radius real? -0.25])
           void?]{

Draws a rectangle with the given top-left corner, and with the given
 size. The corners are quarter-circles using the given radius.  The
 current pen is used for the outline and the current brush for filling
 the shape.

If @scheme[radius] is positive, the value is used as the radius of the
 rounded corner. If @scheme[radius] is negative, the absolute value is
 used as the @italic{proportion} of the smallest dimension of the
 rectangle. 

If @scheme[radius] is less than @scheme[-0.5] or more than half of
 @scheme[width] or @scheme[height], @|MismatchExn|.

If both the pen and brush are non-transparent, the rectangle is filled
 with the brush before the outline is drawn with the pen. The filling
 and outline meet so that no space is left between them, but the
 precise overlap between the filling and outline is platform- and
 size-specific.  Thus, the regions drawn by the brush and pen may
 partially overlap. More generally, the pen is centered over the
 outline of the rounded rectangle, rounding toward the center in
 unsmoothed mode.

See also @method[dc<%> set-smoothing] for information on the
@scheme['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-spline [x1 real?]
                        [y1 real?]
                        [x2 real?]
                        [y2 real?]
                        [x3 real?]
                        [y3 real?])
           void?]{

Draws a spline from (@scheme[x1], @scheme[y1]) to (@scheme[x3], @scheme[y3])
 using (@scheme[x2], @scheme[y2]) as the control point.

See also @method[dc<%> set-smoothing] for information on the
@scheme['aligned] smoothing mode.

@|DrawSizeNote|

}

@defmethod[(draw-text [text string?]
                      [x real?]
                      [y real?]
                      [combine? any/c #f]
                      [offset nonnegative-exact-integer? 0]
                      [angle real? 0])
           void?]{

Draws a text string at a specified point, using the current text font,
 and the current text foreground and background colors. For unrotated
 text, the specified point is used as the starting top-left point for
 drawing characters (e.g, if ``W'' is drawn, the point is roughly the
 location of the top-left pixel in the ``W''). Rotated text is rotated
 around this point.

The @scheme[text] string is drawn starting from the @scheme[offset]
 character, and continuing until the end of @scheme[text] or the first
 null character.

If @scheme[combine?] is @scheme[#t], then @scheme[text] may be
 measured with adjacent characters combined to ligature glyphs, with
 Unicode combining characters as a single glyph, with kerning, with
 right-to-left rendering of characters, etc. If @scheme[combine?] is
 @scheme[#f], then the result is the same as if each character is
 measured separately, and Unicode control characters are ignored.

The string is rotated by @scheme[angle] radians counter-clockwise. If
 @scheme[angle] is not zero, then the text is always drawn in
 transparent mode (see @method[dc<%> set-text-mode]).

The current brush and current pen settings for the DC have no effect
 on how the text is drawn.

See @method[dc<%> get-text-extent] for information on the size of the
 drawn text.

See also @method[dc<%> set-text-foreground], @method[dc<%>
 set-text-background], and @method[dc<%> set-text-mode].

@|DrawSizeNote|

}

@defmethod[(end-doc)
           void?]{

Ends a document, relevant only when drawing to a printer or PostScript
 device (including to a PostScript file).

For printer or PostScript output, an exception is raised if
@scheme[end-doc] is called when the document is not started with
@method[dc<%> start-doc], when a page is currently started by
@method[dc<%> start-page] and not ended with @method[dc<%> end-page],
or when the document has been ended already.

}

@defmethod[(end-page)
           void?]{

Ends a single page, relevant only when drawing to a printer or
 PostScript device (including to a PostScript file).

For printer or PostScript output, an exception is raised if
@scheme[end-page] is called when a page is not currently started by
@method[dc<%> start-page].

}

@defmethod[(get-background)
           (is-a?/c color%)]{

Gets the color used for painting the background. See also
@method[dc<%> set-background].

}

@defmethod[(get-brush)
           (is-a?/c brush%)]{

Gets the current brush. See also @method[dc<%> set-brush].

}

@defmethod[(get-char-height)
           (and/c real? (not/c negative?))]{

Gets the height of a character using the current font.

Unlike most methods, this method can be called for a
 @scheme[bitmap-dc%] object without a bitmap installed.

}

@defmethod[(get-char-width)
           (and/c real? (not/c negative?))]{

Gets the average width of a character using the current font.

Unlike most methods, this method can be called for a
 @scheme[bitmap-dc%] object without a bitmap installed.

}

@defmethod[(get-clipping-region)
           (or/c (is-a?/c region%) false/c)]{

Gets the current clipping region, returning @scheme[#f] if the drawing
 context is not clipped (i.e., the clipping region is the entire
 drawing region).

}

@defmethod[(get-font)
           (is-a?/c font%)]{

Gets the current font. See also @method[dc<%> set-font].

}

@defmethod[(get-gl-context)
           (or/c (is-a?/c gl-context<%>) false/c)]{

Returns a @scheme[gl-context<%>] object for this drawing context
 if it supports OpenGL, @scheme[#f] otherwise.

See @scheme[gl-context<%>] for more information.

}

@defmethod[(get-origin)
           (values real? real?)]{

Returns the device origin, i.e., the location in device coordinates of
 @math{(0,0)} in logical coordinates.

See also @method[dc<%> set-origin].

}


@defmethod[(get-pen)
           (is-a?/c pen%)]{

Gets the current pen. See also @method[dc<%> set-pen].

}

@defmethod[(get-scale)
           (values real? real?)]{

Returns the scaling factor that maps logical coordinates to device
coordinates.

See also @method[dc<%> set-scale].

}

@defmethod[(get-size)
           (values nonnegative-real? nonnegative-real?)]{

Gets the size of the destination drawing area. For a @scheme[dc<%>]
 object obtained from a @scheme[canvas<%>], this is the (virtual
 client) size of the destination window; for a @scheme[bitmap-dc%]
 object, this is the size of the selected bitmap (or 0 if no bitmap is
 selected); for a @scheme[post-script-dc%] or @scheme[printer-dc%]
 drawing context, this gets the horizontal and vertical size of the
 drawing area.

}

@defmethod[(get-smoothing)
           (one-of/c 'unsmoothed 'smoothed 'aligned)]{

Returns the current smoothing mode. See @method[dc<%> set-smoothing].

}

@defmethod[(get-text-background)
           (is-a?/c color%)]{

Gets the current text background color. See also @method[dc<%>
set-text-background].

}

@defmethod[(get-text-extent [string string?]
                            [font (or/c (is-a?/c font%) false/c) #f]
                            [combine? any/c #f]
                            [offset nonnegative-exact-integer? 0])
           (values nonnegative-real? 
                   nonnegative-real?
                   nonnegative-real? 
                   nonnegative-real?)]{


Returns the size of @scheme[str] at it would be drawn in the drawing
 context, starting from the @scheme[offset] character of @scheme[str],
 and continuing until the end of @scheme[str] or the first null
 character.  The @scheme[font] argument specifies the font to use in
 measuring the text; if it is @scheme[#f], the current font of the
 drawing area is used. (See also @method[dc<%> set-font].)

The result is four real numbers:

@itemize{

 @item{the total width of the text (depends on both the font and the
 text);}

 @item{the total height of the font (depends only on the font);}

 @item{the distance from the baseline of the font to the bottom of the
 descender (included in the height, depends only on the font); and}

 @item{extra vertical space added to the font by the font designer
 (included in the height, and often zero; depends only on the font).}

}

The returned width and height define a rectangle is that guaranteed to
 contain the text string when it is drawn, but the fit is not
 necessarily tight. Some undefined number of pixels on the left,
 right, top, and bottom of the drawn string may be ``whitespace,''
 depending on the whims of the font designer and the platform-specific
 font-scaling mechanism.

If @scheme[combine?] is @scheme[#t], then @scheme[text] may be drawn
 with adjacent characters combined to ligature glyphs, with Unicode
 combining characters as a single glyph, with kerning, with
 right-to-left ordering of characters, etc. If @scheme[combine?] is
 @scheme[#f], then the result is the same as if each character is
 drawn separately, and Unicode control characters are ignored.

Unlike most methods, this method can be called for a
 @scheme[bitmap-dc%] object without a bitmap installed.

}


@defmethod[(get-text-foreground)
           (is-a?/c color%)]{

Gets the current text foreground color. See also @method[dc<%>
set-text-foreground].

}

@defmethod[(get-text-mode)
           (one-of/c 'solid 'transparent)]{
Reports how text is drawn; see
@method[dc<%> set-text-mode] .

}

@defmethod[(glyph-exists? [c char]
                          [font (or/c (is-a?/c font%) false/c) #f])
           boolean?]{

Returns @scheme[#t] if the given character has a corresponding glyph
 for this drawing context, @scheme[#f] otherwise.

Due to automatic font substitution when drawing or measuring text, the
 result of this method does not depend on the given font, which merely
 provides a hint for the glyph search. If the font is @scheme[#f], the
 drawing context's current font is used. The result depends on the
 type of the drawing context, but the result for @scheme[canvas%]
 @scheme[dc<%>] instances and @scheme[bitmap-dc%] instances is always
 the same for a given platform and a given set of installed fonts.

See also @method[font% screen-glyph-exists?] .

}

@defmethod[(ok?)
           boolean?]{

Returns @scheme[#t] if the drawing context is usable.

}

@defmethod[(set-background [color (is-a?/c color%)])
           void?]{

Sets the background color for drawing in this object (e.g., using
@method[dc<%> clear] or using a stippled @scheme[brush%] with the mode
@scheme['opaque]). For monochrome drawing, all non-black colors are
treated as white.

}

@defmethod*[([(set-brush [brush (is-a?/c brush%)])
              void?]
             [(set-brush [color (is-a?/c color%)]
                         [style (one-of/c 'transparent 'solid 'opaque 
                                          'xor 'hilite 'panel 
                                          'bdiagonal-hatch 'crossdiag-hatch
                                          'fdiagonal-hatch 'cross-hatch 
                                          'horizontal-hatch 'vertical-hatch)])
              void?]
             [(set-brush [color-name string?]
                         [style (one-of/c 'transparent 'solid 'opaque 
                                          'xor 'hilite 'panel
                                          'bdiagonal-hatch 'crossdiag-hatch
                                          'fdiagonal-hatch 'cross-hatch
                                          'horizontal-hatch 'vertical-hatch)])
              void?])]{

Sets the current brush for drawing in this object.  While a brush is
 selected into a drawing context, it cannot be modified. When a color
 and style are given, the arguments are as for @xmethod[brush-list%
 find-or-create-brush].

}


@defmethod[(set-clipping-rect [x real?]
                              [y real?]
                              [width (and/c real? (not/c negative?))]
                              [height (and/c real? (not/c negative?))])
           void?]{

Sets the clipping region to a rectangular region.

See also @method[dc<%> set-clipping-region] and @method[dc<%>
get-clipping-region].

@|DrawSizeNote|

}

@defmethod[(set-clipping-region [rgn (or/c (is-a?/c region%) false/c)])
           void?]{

Sets the clipping region for the drawing area, turning off all
 clipping within the drawing region if @scheme[#f] is provided.

The clipping region must be reset after changing a @scheme[dc<%>]
 object's origin or scale (unless it is @scheme[#f]); see
 @scheme[region%] for more information.

See also @method[dc<%> set-clipping-rect] and @method[dc<%>
 get-clipping-region].

}

@defmethod[(set-font [font (is-a?/c font%)])
           void?]{

Sets the current font for drawing text in this object.

}

@defmethod[(set-origin [x real?]
                       [y real?])
           void?]{

Sets the device origin, i.e., the location in device coordinates of
 @math{(0,0)} in logical coordinates.

Changing a @scheme[dc<%>] object's origin or scale does not affect
 @scheme[region%] objects that were previously created. See
@scheme[region%] for more information.
 

@|DrawSizeNote|

}

@defmethod*[([(set-pen [pen (is-a?/c pen%)])
              void?]
             [(set-pen [color (is-a?/c color%)]
                       [width (real-in 0 255)]
                       [style (one-of/c 'transparent 'solid 'xor 'hilite
                                        'dot 'long-dash 'short-dash 'dot-dash 
                                        'xor-dot 'xor-long-dash 'xor-short-dash 
                                        'xor-dot-dash)])
              void?]
             [(set-pen [color-name string?]
                       [width (real-in 0 255)]
                       [style (one-of/c 'transparent 'solid 'xor 'hilite 
                                        'dot 'long-dash 'short-dash 'dot-dash 
                                        'xor-dot 'xor-long-dash 'xor-short-dash 
                                        'xor-dot-dash)])
              void?])]{

Sets the current pen for this object. When a color, width, and style
 are given, the arguments are as for @xmethod[pen-list%
 find-or-create-pen].

The current pen does not affect text drawing; see also @method[dc<%>
 set-text-foreground].

While a pen is selected into a drawing context, it cannot be modified.

}

@defmethod[(set-scale [x-scale (and/c real? (not/c negative?))]
                      [y-scale (and/c real? (not/c negative?))])
           void?]{

Sets a scaling factor that maps logical coordinates to device coordinates.

Changing a @scheme[dc<%>] object's origin or scale does not affect
 @scheme[region%] objects that were previously created. See
 @scheme[region%] for more information.

@|DrawSizeNote|

}

@defmethod[(set-smoothing [mode (one-of/c 'unsmoothed 'smoothed 'aligned)])
           void?]{

Enables or disables anti-aliased smoothing of lines, curves,
 rectangles, rounded rectangles, ellipses, polygons, paths, and clear
 operations. (Text smoothing is not affected by this method, and is
 instead controlled through the @scheme[font%] object.)

Smoothing is supported under Windows only when Microsoft's
 @file{gdiplus.dll} is installed (which is always the case for Windows
 XP). Smoothing is supported under Mac OS X always. Smoothing is
 supported under X only when Cairo is installed when MrEd is compiled.
 Smoothing is never supported for black-and-white contexts. Smoothing
 is always supported (and cannot be disabled) for PostScript output.

The smoothing mode is either @scheme['unsmoothed], @scheme['smoothed],
 or @scheme['aligned]. Both @scheme['aligned] and @scheme['smoothed]
 are smoothing modes.

In @scheme['smoothed] mode for a canvas or bitmap drawing context,
 integer drawing coordinates correspond to the boundary between
 pixels, and pen-based drawing is centered over a given line or
 curve. Thus, drawing with pen width @scheme[1] from @math{(0, 10)} to
 @math{(10, 10)} draws a 2-pixel wide line with @math{50%} opacity.

The @scheme['aligned] smoothing mode is like @scheme['smoothed], but
 it paints pixels more like @scheme['unsmoothed] mode. Since it aligns
 shapes to pixel boundaries, @scheme['aligned] mode often produces
 better results than @scheme['smoothed], but the results depend on the
 application. The @scheme['aligned] mode is defined in terms of
 @scheme['smoothed] mode, except that drawing coordinates are rounded
 down (via @scheme[floor], after scaling and origin translation). For
 line drawing, coordinates are then shifted right and down by the
 @scheme[floor] of half a pen width.  In addition, for pen drawing
 through @method[dc<%> draw-rectangle], @method[dc<%> draw-ellipse],
 @method[dc<%> draw-rounded-rectangle], and @method[dc<%> draw-arc],
 the given width and height are each decreased by @math{1.0}.

In either smoothing mode, brush and pen stipples are ignored (except
 for PostScript drawing), and @scheme['hilite] and @scheme['xor]
 drawing modes are treated as @scheme['solid].  If smoothing is not
 supported, then attempting to set the smoothing mode to
 @scheme['smoothed] or @scheme['aligned] will have no effect, and
 @method[dc<%> get-smoothing] will always return
 @scheme['unsmoothed]. Similarly, @method[dc<%> get-smoothing] for a
 @scheme[post-script-dc%] always returns @scheme['smoothed].

}

@defmethod[(set-text-background [color (is-a?/c color%)])
           void?]{

Sets the current text background color for this object. The text
 background color is painted behind text that is drawn with
 @method[dc<%> draw-text], but only for the @scheme['solid] text mode
 (see @method[dc<%> set-text-mode]).

For monochrome drawing, all non-white colors are treated as black.

}

@defmethod[(set-text-foreground [color (is-a?/c color%)])
           void?]{

Sets the current text foreground color for this object, used for
 drawing text with
@method[dc<%> draw-text].

For monochrome drawing, all non-black colors are treated as
 white.

}

@defmethod[(set-text-mode [mode (one-of/c 'solid 'transparent)])
           void?]{

Determines how text is drawn:

@itemize{

 @item{@scheme['solid] --- Before text is drawn, the destination area
       is filled with the text background color (see @method[dc<%>
       set-text-background]).}

 @item{@scheme['transparent] --- Text is drawn directly over any
       existing image in the destination, as if overlaying text
       written on transparent film.}

}

}

@defmethod[(start-doc [message string?])
           boolean?]{

Starts a document, relevant only when drawing to a printer or
 PostScript device (including to a PostScript file).  For some
 platforms, the @scheme[message] string is displayed in a dialog until
 @method[dc<%> end-doc] is called.

For printer or PostScript output, an exception is raised if
 @scheme[start-doc] has been called already (even if @method[dc<%>
 end-doc] has been called as well). Furthermore, drawing methods raise
 an exception if not called while a page is active as determined by
 @method[dc<%> start-doc] and @method[dc<%> start-page].

}

@defmethod[(start-page)
           void?]{

Starts a page, relevant only when drawing to a printer or PostScript
 device (including to a PostScript file).

For printer or PostScript output, an exception is raised if
 @scheme[start-doc] is called when a page is already started, or when
 @method[dc<%> start-doc] has not been called, or when @method[dc<%>
 end-doc] has been called already. In addition, in the case of
 PostScript output, Encapsulated PostScript (EPS) cannot contain
 multiple pages, so calling @scheme[start-page] a second time for a
 @scheme[post-script-dc%] instance raises an exception; to create
 PostScript output with multiple pages, supply @scheme[#f] as the
 @scheme[as-eps] initialization argument for @scheme[post-script-dc%].

}

@defmethod[(try-color [try (is-a?/c color%)]
                      [result (is-a?/c color%)])
           void?]{

Determines the actual color used for drawing requests with the given
 color. The @scheme[result] color is set to the RGB values that are
 actually produced for this drawing context to draw the color
 @scheme[try].

}}
