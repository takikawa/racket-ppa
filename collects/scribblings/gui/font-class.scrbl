#lang scribble/doc
@(require "common.ss")

@defclass/title[font% object% ()]{

A @defterm{font} is an object which determines the appearance of text,
 primarily when drawing text to a device context. A font is determined
 by seven properties:

@itemize{

 @item{size --- The size of the text, either in points (the default)
                or logical drawing units, depending on the
                ``size-in-pixels?''\ property (see below).}

 @item{family --- A platform- and device-independent font
                  designation. The families are:

 @itemize{
 @item{@indexed-scheme['default]}
 @item{@indexed-scheme['decorative]}
 @item{@indexed-scheme['roman]}
 @item{@indexed-scheme['script]}
 @item{@indexed-scheme['swiss]}
 @item{@indexed-scheme['modern] (fixed width)}
 @item{@indexed-scheme['symbol] (Greek letters and more)}
 @item{@indexed-scheme['system] (used to draw control labels)}
 }

 The @scheme['modern] designation is special under Mac OS X and X with
 fontconfig/Xft; characters in the ASCII range 0-255 are converted to
 Unicode characters that match Adobe symbols. For example, @litchar{a} is
 converted to @litchar["\u03B1"].}

 @item{face --- A string face name, such as @scheme["Courier"] (under
                Windows and Mac OS X), @scheme["-*-courier"] (under
                X), or @scheme[" Luxi Sans"] (under X with
                fontconfig/Xft; note the leading space). The format
                and meaning of a face name is platform- and
                device-specific. If a font's face name is @scheme[#f],
                then the font's appearance depends only on the
                family. If a face is provided but no mapping is
                available for the face name (for a specific platform
                or device), then the face name is ignored and the
                family is used. See @scheme[font-name-directory<%>]
                for information about how face names are mapped for
                drawing text.}

@item{style --- The slant style of the font, one of:
 @itemize{
 @item{@indexed-scheme['normal]}
 @item{@indexed-scheme['slant] (Windows, Mac OS X: same as @scheme['italic]; X: tries @scheme['italic] if @scheme['slant] font does not exist)}
 @item{@indexed-scheme['italic] (X: tries @scheme['slant] if @scheme['italic] font does not exist)}
 }}

@item{weight --- The weight of the font, one of:
 @itemize{
 @item{@indexed-scheme['normal]}
 @item{@indexed-scheme['light]}
 @item{@indexed-scheme['bold]}
 }}

@item{underline? --- @scheme[#t] for underlined, @scheme[#f] for plain.}

@item{smoothing --- Amount of anti-alias smoothing, one of:
 @itemize{
 @item{@indexed-scheme['default] (platform-specific, sometimes user-configurable)}
 @item{@indexed-scheme['partly-smoothed] (Windows: TrueType when available; 
                                      Mac OS X: 4-bit, pixel-aligned smoothing;
                                      X: fontconfig/Xft when available)}
 @item{@indexed-scheme['smoothed] (Windows: ClearType when available, XP and up;
                               Mac OS X: Quartz smoothing;
                               X: fontconfig/Xft when available)}
 @item{@indexed-scheme['unsmoothed]}
 }
 Special case: @scheme['default] corresponds to
 @scheme['partly-smoothed] when used with the @scheme['modern] family
 and a font size between 9 and 13 (inclusive).}

@item{size-in-pixels? --- @scheme[#t] if the size of the font
 is in logical drawing units (i.e., pixels for an unscaled screen or
 bitmap drawing context), @scheme[#f] if the size of the font is in
 points (which can depend on screen resolution).}

}

To avoid creating multiple fonts with the same characteristics, use
 the global @scheme[font-list%] object @indexed-scheme[the-font-list].

See also
@scheme[font-name-directory<%>].



@defconstructor*/make[(()
                       ([size (integer-in 1 255)]
                        [family (one-of/c 'default 'decorative 'roman 'script 
                                          'swiss 'modern 'symbol 'system)]
                        [style (one-of/c 'normal 'italic 'slant) 'normal]
                        [weight (one-of/c 'normal 'bold 'light) 'normal]
                        [underline? any/c #f]
                        [smoothing (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                        [size-in-pixels? any/c #f])
                       ([size (integer-in 1 255)]
                        [face string?]
                        [family (one-of/c 'default 'decorative 'roman 'script 
                                          'swiss 'modern 'symbol 'system)]
                        [style (one-of/c 'normal 'italic 'slant) 'normal]
                        [weight (one-of/c 'normal 'bold 'light) 'normal]
                        [underline? any/c #f]
                        [smoothing (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                        [size-in-pixels? any/c #f]))]{

When no arguments are provided, creates an instance of the default
 font. If no face name is provided, the font is created without a face
 name. Otherwise, see @scheme[font-name-directory<%>] for information
 about the way @scheme[face] is interpreted for drawing text on
 various platforms and devices; when a platform- or device-specific
 interpretation of @scheme[face] is not available, the @scheme[family]
 is used to draw text.

See @scheme[font%] for information about @scheme[family],
 @scheme[style], and @scheme[weight].  @scheme[font-name-directory<%>].

}

@defmethod[(get-face)
           (or/c string? false/c)]{

Gets the font's face name, or @scheme[#f] if none is specified.

}

@defmethod[(get-family)
           (one-of/c 'default 'decorative 'roman 'script 
                     'swiss 'modern 'symbol 'system)]{

Gets the font's family. See @scheme[font%] for information about
families.

}

@defmethod[(get-font-id)
           (and/c exact? integer?)]{

Gets the font's ID, for use with a
@scheme[font-name-directory<%>]. The ID is determined by the font's
face and family specifications, only.

}

@defmethod[(get-point-size)
           (integer-in 1 255)]{

Gets the font's size (roughly the height). Despite the method name,
 the size may be in logical units instead of points, depending on the
 result of @method[font% get-size-in-pixels].

Due to space included in a font by a font designer, a font tends to
 generate text that is slightly taller than the nominal size.

}

@defmethod[(get-size-in-pixels)
           boolean?]{

Returns @scheme[#t] if the size reported by @method[font%
 get-point-size] is in logical drawing units, @scheme[#f] if it is in
 points.

For a size in points and a screen or bitmap drawing context, the
 logical height depends on the resolution of the screen.

}

@defmethod[(get-smoothing)
           (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed)]{

Gets the font's anti-alias smoothing mode. See @scheme[font%] for
 information about smoothing.

}

@defmethod[(get-style)
           (one-of/c 'normal 'italic 'slant)]{

Gets the font's slant style. See @scheme[font%] for information about
 styles.

}

@defmethod[(get-underlined)
           boolean?]{

Returns @scheme[#t] if the font is underlined or @scheme[#f]
otherwise.

}

@defmethod[(get-weight)
           (one-of/c 'normal 'bold 'light)]{

Gets the font's weight. See @scheme[font%] for information about
 weights.

}

@defmethod[(screen-glyph-exists? [c char]
                                 [for-label? any/c #f])
           boolean?]{

Returns @scheme[#t] if the given character has a corresponding glyph
 when drawing to the screen or a bitmap, @scheme[#f] otherwise.

If the second argument is true, the result indicates whether the glyph
 is available for control labels. Otherwise, it indicates whether the
 glyph is available for @scheme[dc<%>] drawing.

For @scheme[dc<%>] drawing, due to automatic font substitution when
 drawing or measuring text, the result of this method does not depend
 on this font's attributes (size, face, etc.). The font's attributes
 merely provide a hint for the glyph search.

See also @method[dc<%> glyph-exists?] .

}}
