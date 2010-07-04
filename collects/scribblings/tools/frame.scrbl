#lang scribble/doc
@(require "common.ss")
@title{@tt{drscheme:frame}}

@defclass[drscheme:frame:name-message% canvas% ()]{

This class implements the little filename button in the top-right hand
side of drscheme's frame.



@defconstructor/make[([parent (is-a?/c area-container<%>)])]{}

@defmethod[(set-message [name (or/c string? false/c)]
                        [short-name string?])
           void?]{
@methspec{

Sets the names that the button shows.

}
@methimpl{

The string @scheme[short-name] is the name that is shown on the button
and @scheme[name] is shown when the button is clicked on, in a separate
window. If @scheme[name] is \#f, a message indicating that the file
hasn't been saved is shown.



}}}


@defmixin[drscheme:frame:mixin (drscheme:frame:basics<%> frame:text-info<%> frame:editor<%>) (drscheme:frame:<%>)]{

Provides an implementation of 
@scheme[drscheme:frame:<%>] 
}


@defmixin[drscheme:frame:basics-mixin (frame:standard-menus<%>) (drscheme:frame:basics<%>)]{

Use this mixin to establish some common menu items across various DrScheme windows.



@defmethod[#:mode override 
           (edit-menu:between-find-and-preferences)
           void?]{

Adds a
@scheme[separator-menu-item%]. Next, adds the
@scheme["Keybindings"] menu item to the edit menu. Finally,
if the 
@scheme[current-eventspace-has-standard-menus?] procedure returns @scheme[#f], creates another 
@scheme[separator-menu-item%].



}

@defmethod[#:mode override 
           (file-menu:between-open-and-revert [file-menu (is-a?/c @scheme[menu%])])
           void?]{

Adds an {\it Install .plt File...} menu item, which
downloads and installs .plt files from the web, or installs
them from the local disk. After that, calls the super
method.


}

@defmethod[#:mode override 
           (file-menu:between-print-and-close [file-menu (is-a?/c @scheme[menu%])])
           void?]{

Calls the super method. Then, creates a menu item for
multi-file searching. Finally,
adds a
@scheme[separator-menu-item%].


}

@defmethod[#:mode override 
           (file-menu:new-callback [item (is-a?/c menu-item%)]
                                   [evt (is-a?/c control-event%)])
           void?]{

Opens a new, empty DrScheme window.


}

@defmethod[#:mode override 
           (file-menu:new-string)
           string?]{

Returns the empty string.


}

@defmethod[#:mode override 
           (file-menu:open-callback [item (is-a?/c menu-item%)]
                                    [evt (is-a?/c control-event%)])
           void?]{

Calls 
@scheme[handler:edit-file].


}

@defmethod[#:mode override 
           (file-menu:open-string)
           string?]{

Returns the empty string.


}

@defmethod[(get-additional-important-urls)
           (listof (list string string))]{
@methspec{

Each string in the result of this method is added as a menu
item to DrScheme's ``Related Web Sites'' menu item. The
first string is the name of the menu item and the second
string is a url that, when the menu item is chosen, is sent
to the user's browser.

}
@methimpl{

Defaultly returns the empty list.


}}

@defmethod[#:mode override 
           (help-menu:about-callback [item (is-a?/c menu-item%)]
                                     [evt (is-a?/c control-event%)])
           void?]{

Opens an about box for DrScheme.


}

@defmethod[#:mode override 
           (help-menu:about-string)
           string?]{

Returns the string @scheme["DrScheme"].


}

@defmethod[#:mode override 
           (help-menu:before-about [help-menu (is-a?/c menu%)])
           void?]{

Adds the Help Desk menu item and the Welcome to DrScheme menu item.
}

@defmethod[#:mode override 
           (help-menu:create-about?)
           boolean?]{

Returns @scheme[#t].


}}


@definterface[drscheme:frame:basics<%> (frame:standard-menus<%>)]{

This interface is the result of the
\iscmmixin{drscheme:frame:basics-mixin} 

}


@definterface[drscheme:frame:<%> (frame:editor<%> frame:text-info<%> drscheme:frame:basics<%>)]{



@defmethod[(add-show-menu-items [show-menu (is-a?/c @scheme[menu%])])
           void?]{
@methspec{

This method is called during the construction of the view
menu.  This method is intended to be overridden. It is
expected to add other Show/Hide menu items to the show menu.

See also
@method[drscheme:frame:<%> get-show-menu].

}
@methimpl{

Does nothing.



}}

@defmethod[(get-show-menu)
           (is-a?/c menu%)]{
\index{View menu}

returns the view menu, for use by the
@method[drscheme:frame:<%> update-shown] method.

See also
@method[drscheme:frame:<%> add-show-menu-items].

The method (and others) uses the word @tt{show} to preserve
backwards compatibility from when the menu itself was named
the Show menu.

}

@defmethod[(not-running)
           void?]{
updates the status pane at the bottom of the window to show
that evaluation is not taking place in the user's program.

}

@defmethod[(running)
           void?]{
updates the status pane at the bottom of the window to show
that evaluation is taking place in the user's program.

}

@defmethod[(update-shown)
           void?]{
@methspec{

This method is intended to be overridden. It's job is to
update the @scheme["View"] menu to match the state of the
visible windows. In the case of the standard DrScheme
window, it change the menu items to reflect the visibility of
the definitions and interaction @scheme[editor-canvas%]s.

Call this method whenever the state of the show menu might
need to change.

See also
@method[drscheme:frame:<%> get-show-menu].

}
@methimpl{

Does nothing.


}}}

@(include-extracted (lib "tool-lib.ss" "drscheme") #rx"^drscheme:frame:")
