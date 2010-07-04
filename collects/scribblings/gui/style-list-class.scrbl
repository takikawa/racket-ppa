#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@defclass/title[style-list% object% ()]{

A @scheme[style-list%] object contains a set of @scheme[style<%>]
 objects and maintains the hierarchical relationships between them. A
 @scheme[style<%>] object can only be created through the methods of a
 @scheme[style-list%] object. There is a global style list object,
 @indexed-scheme[the-style-list], but any number of independent lists can be
 created for separate style hierarchies.  Each editor creates its own
 private style list.

See @|stylediscuss| for more information.



@defconstructor/make[()]{

The root style, named @scheme["Basic"], is automatically created.

}

@defmethod[(basic-style)
           (is-a?/c style<%>)]{

Returns the root style. Each style list has its own root style.

See also @|mrprefsdiscuss| for information about the
 @ResourceFirst{default-font-size}.

}


@defmethod[(convert [style (is-a?/c style<%>)])
           (is-a?/c style<%>)]{

Converts @scheme[style], which can be from another style list, to a style
in this list. If @scheme[style] is already in this list, then @scheme[ style]
is returned. If @scheme[style] is named and a style by that name is
already in this list, then the existing named style is returned.
Otherwise, the style is converted by converting its base style
(and shift style if @scheme[style] is a join style) and then creating
a new style in this list.

}

@defmethod[(find-named-style [name string?])
           (or/c (is-a?/c style<%>) false/c)]{

Finds a style by name. If no such style can be found, @scheme[#f] is
returned.

}

@defmethod[(find-or-create-join-style [base-style (is-a?/c style<%>)]
                                      [shift-style (is-a?/c style<%>)])
           (is-a?/c style<%>)]{

Creates a new join style, or finds an appropriate existing one. The
returned style is always unnamed.  See @|stylediscuss| for more
information. 

The @scheme[base-style] argument must be a style within this style
 list.

}


@defmethod[(find-or-create-style [base-style (is-a?/c style<%>)]
                                 [delta (is-a?/c style-delta%)])
           (is-a?/c style<%>)]{

Creates a new derived style, or finds an appropriate existing one.
The returned style is always unnamed.  See @|stylediscuss| for more
information. 

The @scheme[base-style] argument must be a style within this style list.

}


@defmethod[(forget-notification [key any/c])
           void?]{

See @method[style-list% notify-on-change].

The @scheme[key] argument is the value returned by @method[style-list%
notify-on-change].

}


@defmethod[(index-to-style [i nonnegative-exact-integer?])
           (or/c (is-a?/c style<%>) false/c)]{

Returns the style associated with the given index, or @scheme[#f] for
 a bad index. See also @method[style-list% style-to-index].

}


@defmethod[(new-named-style [name string?]
                            [like-style (is-a?/c style<%>)])
           (is-a?/c style<%>)]{

Creates a new named style, unless the name is already being used. 

If @scheme[name] is already being used, then @scheme[like-style] is
 ignored and the old style associated to the name is
 returned. Otherwise, a new style is created for @scheme[name] with
 the same characteristics (i.e., the same base style and same style
 delta or shift style) as @scheme[like-style].

The @scheme[like-style] style must be in this style list, otherwise
 the named style is derived from the basic style with an empty style
 delta.

}

@defmethod[(notify-on-change [f ((or/c (is-a?/c style<%> false/c)) . -> . any)])
           any/c]{

Attaches a callback to the style list. The callback is invoked
 whenever a style is modified.

Often, a change in one style will trigger a change in several other
 derived styles; to allow clients to handle all the changes in a
 batch, @scheme[#f] is passed in as the changing style after a set of
 styles has been processed.

The return value from @method[style-list% notify-on-change] is an
 opaque key to be used with @method[style-list% forget-notification].

}


@defmethod[(number)
           nonnegative-exact-integer?]{

Returns the number of styles in the list.

}

@defmethod[(replace-named-style [name string?]
                                [like-style (is-a?/c style<%>)])
           (is-a?/c style<%>)]{

Like @method[style-list% new-named-style], except that if the name is
 already mapped to a style, the existing mapping is replaced.

}

@defmethod[(style-to-index [style (is-a?/c style<%>)])
           (or/c nonnegative-exact-integer? false/c)]{

Returns the index for a particular style. The index for a style's base
 style (and shift style, if it is a join style) is guaranteed to be
 lower than the style's own index. (As a result, the root style's
 index is always @scheme[0].) A style's index can change whenever a new
 style is added to the list, or the base style or shift style of
 another style is changed.

If the given style is not in this list, @scheme[#f] is returned.

}}

