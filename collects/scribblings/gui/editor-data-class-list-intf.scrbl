#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@definterface[editor-data-class-list<%> ()]{

Each eventspace has an instance of @scheme[editor-data-class-list<%>],
 obtained with @scheme[(get-the-editor-data-class-list)]. New
 instances cannot be created directly. This list keeps a list of
 editor data classes; this list is needed for loading snips from a
 file. See also @|editordatadiscuss|.


@defmethod[(add [snipclass (is-a?/c editor-data-class%)])
           void?]{
Adds a snip data class to the list. If a class with the same name already
exists in the list, this one will not be added.

}

@defmethod[(find [name string?])
           (or/c (is-a?/c snip-class%) false/c)]{
Finds a snip data class from the list with the given name, returning
 @scheme[#f] if none can be found.

}

@defmethod[(find-position [class (is-a?/c editor-data-class%)])
           nonnegative-exact-integer?]{
Returns an index into the list for the specified class.

}

@defmethod[(nth [n nonnegative-exact-integer?])
           (or/c (is-a?/c editor-data-class%) false/c)]{
Returns the @scheme[n]th class in the list (counting from 0), returning
 @scheme[#f] if the list has @scheme[n] or less classes.

}

@defmethod[(number)
           nonnegative-exact-integer?]{

Returns the number of editor data classes in the list.

}}

