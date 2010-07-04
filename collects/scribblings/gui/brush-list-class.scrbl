#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[brush-list% object% ()]{

A @scheme[brush-list%] object maintains a list of @scheme[brush%]
 objects to avoid creating brushes repeatedly. A @scheme[brush%]
 object in a brush list cannot be mutated.

A global brush list, @scheme[the-brush-list], is created
 automatically.


@defconstructor/make[()]{

Creates an empty brush list.

}

@defmethod*[([(find-or-create-brush [color (is-a?/c color%)]
                                    [style (one-of/c 'transparent 'solid 'opaque
                                                     'xor 'hilite 'panel 
                                                     'bdiagonal-hatch 'crossdiag-hatch 
                                                     'fdiagonal-hatch 'cross-hatch 
                                                     'horizontal-hatch 'vertical-hatch)])
              (is-a?/c brush%)]
             [(find-or-create-brush [color-name string?]
                                    [style (one-of/c 'transparent 'solid 'opaque 
                                                     'xor 'hilite 'panel 
                                                     'bdiagonal-hatch 'crossdiag-hatch 
                                                     'fdiagonal-hatch 'cross-hatch 
                                                     'horizontal-hatch 'vertical-hatch)])
              (or/c (is-a?/c brush%) false/c)])]{

Finds a brush of the given specification, or creates one and adds it
 to the list. See @scheme[brush%] for a further explanation of the
 arguments, which are the same as @scheme[brush%]'s initialization
 arguments.

}}
