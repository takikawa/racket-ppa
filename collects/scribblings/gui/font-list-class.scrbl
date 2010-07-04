#lang scribble/doc
@(require "common.ss")

@defclass/title[font-list% object% ()]{

A @scheme[font-list%] object maintains a list of @scheme[font%]
 objects to avoid repeatedly creating fonts.

A global font list, @scheme[the-font-list], is created automatically.


@defconstructor/make[()]{

Creates an empty font list.

}

@defmethod*[([(find-or-create-font [size (integer-in 1 255)]
                                   [family (one-of/c 'default 'decorative 'roman 'script 
                                                     'swiss 'modern 'symbol 'system)]
                                   [style (one-of/c 'normal 'italic 'slant)]
                                   [weight (one-of/c 'normal 'bold 'light)]
                                   [underline? any/c #f]
                                   [smoothing (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                                   [size-in-pixels? any/c #f])
              (is-a?/c font%)]
             [(find-or-create-font [size (integer-in 1 255)]
                                   [face string?]
                                   [family (one-of/c 'default 'decorative 'roman 'script
                                                     'swiss 'modern 'symbol 'system)]
                                   [style (one-of/c 'normal 'italic 'slant)]
                                   [weight (one-of/c 'normal 'bold 'light)]
                                   [underline any/c #f]
                                   [smoothing (one-of/c 'default 'partly-smoothed 'smoothed 'unsmoothed) 'default]
                                   [size-in-pixels? any/c #f])
              (is-a?/c font%)])]{

Finds an existing font in the list or creates a new one (that is
 automatically added to the list). The arguments are the same as for
 creating a @scheme[font%] instance.

}}
