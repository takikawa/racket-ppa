#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@definterface/title[style<%> ()]{

A @scheme[style<%>] object encapsulates drawing information (font,
 color, alignment, etc.) in a hierarchical manner. A @scheme[style<%>]
 object always exists within the context of a @scheme[style-list%]
 object and is never created except by a @scheme[style-list%] object.

See also @|stylediscuss|.


@defmethod[(get-alignment)
           (one-of/c 'top 'center 'bottom)]{

Returns the style's alignment: @scheme['top], @scheme['center], or
 @scheme['bottom].

}


@defmethod[(get-background)
           (is-a?/c color%)]{

Returns the style's background color.

}


@defmethod[(get-base-style)
           (or/c (is-a?/c style<%>) false/c)]{

Returns the style's base style. See @|stylediscuss| for more
 information. The return value is @scheme[#f] only for the basic style
 in the list.

}

@defmethod[(get-delta [delta (is-a?/c style-delta%)])
           void?]{

Returns the style's delta information if the style is not a join
 style. See @|stylediscuss| for more information.

}

@defmethod[(get-face)
           (or/c string? false/c)]{

Returns the style's face name. See @scheme[font%].

}


@defmethod[(get-family)
           (one-of/c 'default 'decorative 'roman 'script 
                     'swiss 'modern 'symbol 'system)]{

Returns the style's font family. See @scheme[font%].

}

@defmethod[(get-font)
           (is-a?/c font%)]{

Returns the style's font information.

}

@defmethod[(get-foreground)
           (is-a?/c color%)]{

Returns the style's foreground color.

}

@defmethod[(get-name)
           (or/c string? false/c)]{

Returns the style's name, or @scheme[#f] if it is unnamed. Style names
 are only set through the style's @scheme[style-list%] object.

}

@defmethod[(get-shift-style)
           (is-a?/c style<%>)]{

Returns the style's shift style if it is a join style. Otherwise, the
 root style is returned. See @|stylediscuss| for more information.

}

@defmethod[(get-size)
           (integer-in 0 255)]{

Returns the style's font size.

}

@defmethod[(get-size-in-pixels)
           boolean?]{

Returns @scheme[#t] if the style size is in pixels, instead of points,
 or @scheme[#f] otherwise.

}

@defmethod[(get-smoothing)
           (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed)]{

Returns the style's font smoothing. See @scheme[font%].

}

@defmethod[(get-style)
           (one-of/c 'normal 'italic 'slant)]{

Returns the style's font style. See @scheme[font%].

}

@defmethod[(get-text-descent [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the descent of text using this style in a given DC.

}

@defmethod[(get-text-height [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the height of text using this style in a given DC.

}

@defmethod[(get-text-space [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the vertical spacing for text using this style in a given DC.

}

@defmethod[(get-text-width [dc (is-a?/c dc<%>)])
           (and/c real? (not/c negative?))]{

Returns the width of a space character using this style in a given
DC.

}

@defmethod[(get-transparent-text-backing)
           boolean?]{

Returns @scheme[#t] if text is drawn without erasing the
 text background or @scheme[#f] otherwise.

}

@defmethod[(get-underlined)
           boolean?]{

Returns @scheme[#t] if the style is underlined or @scheme[#f]
 otherwise.

}

@defmethod[(get-weight)
           (one-of/c 'normal 'bold 'light)]{

Returns the style's font weight. See @scheme[font%].

}

@defmethod[(is-join?)
           boolean?]{

Returns @scheme[#t] if the style is a join style or @scheme[#f]
 otherwise. See @|stylediscuss| for more information.

}

@defmethod[(set-base-style [base-style (is-a?/c style<%>)])
           void?]{

Sets the style's base style and recomputes the style's font, etc. See
 @|stylediscuss| for more information.

}

@defmethod[(set-delta [delta (is-a?/c style-delta%)])
           void?]{

Sets the style's delta (if it is not a join style) and recomputes the
style's font, etc. See @|stylediscuss| for more information.

}

@defmethod[(set-shift-style [style (is-a?/c style<%>)])
           void?]{

Sets the style's shift style (if it is a join style) and recomputes
the style's font, etc. See @|stylediscuss| for more information.

}

@defmethod[(switch-to [dc (is-a?/c dc<%>)]
                      [old-style (or/c (is-a?/c style<%>) false/c)])
           void?]{

Sets the font, pen color, etc. of the given drawing context. If
 @scheme[oldstyle] is not @scheme[#f], only differences between the
 given style and this one are applied to the drawing context.

}}

