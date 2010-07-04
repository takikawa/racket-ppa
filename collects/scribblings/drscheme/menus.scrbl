#lang scribble/doc
@(require "common.ss"
          scribble/struct)

@(define (defmenuitem . s)
  (let ([mi (apply onscreen s)])
    @index*[(list (string-append (element->string mi) " menu item"))
            (list (elem mi " menu item"))]{@|mi| :}))

@(define lam-str "\u03BB")

@title{Menus}

@section{@onscreen{File}}

@itemize[
  
 @item{@defmenuitem{New} Creates a new DrScheme window.}

 @item{@defmenuitem{Open...} Opens a find-file dialog for choosing
  a file to load into a @tech{definitions window}.}
  
 @item{@defmenuitem{Open Recent} Lists recently opened
   files. Choosing one of them opens that file for editing.}
  
 @item{@defmenuitem{Install PLT File...} Opens a dialog asking for the
   location of the @filepath{.plt} file (either on the local disk or
   on the web) and installs the contents of the file.}

 @item{@defmenuitem{Revert} Re-loads the file that is currently in the
  @tech{definitions window}. All changes since the file was last saved
  will be lost.}
  
 @item{@defmenuitem{Save Definitions} Saves the program in the
  @tech{definitions window}. If the program has never been saved
  before, a save-file dialog appears.}
  
 @item{@defmenuitem{Save Definitions As...} Opens a save-file dialog for
  choosing a destination file to save the program in the definitions
  window. Subsequent saves write to the newly-selected file.}
  
 @item{@defmenuitem{Save Other} Contains these sub-items
 @itemize[

 @item{@defmenuitem{Save Definitions As Text...} Like @onscreen{Save
  Definitions As...}, but the file is saved in plain-text format (see
  @secref["drscheme-file-formats"]). Subsequent saves also write in
  plain-text format.}
  
 @item{@defmenuitem{Save Interactions} Saves the contents of the interactions
  window to a file. If the interaction constants have never been saved
  before, a save-file dialog appears.}
  
 @item{@defmenuitem{Save Interactions As...}  Opens a save-file dialog for
  choosing a destination file to save the contents of the interactions
  window. Subsequent saves write to the newly-selected file.}
  
 @item{@defmenuitem{Save Interactions As Text...}  Like @onscreen{Save
  Interactions As...}, but the file is saved in plain-text format (see
  @secref["drscheme-file-formats"]). Subsequent saves are write in
  plain-text format.}
  
 ]}
 
@item{@defmenuitem{Log Definitions and Interactions...} Starts a
  running of log of the text in the interactions and definitions
  windows, organized by executions. In a directory of your choosing,
  DrScheme saves files with the names @filepath{01-definitions},
  @filepath{01-interactions}, @filepath{02-definitions},
  @filepath{02-interactions}, @|etc| as you interact with various
  programs.}

 @item{@defmenuitem{Print Definitions...} Opens a dialog for printing
  the current program in the @tech{definitions window}.}

 @item{@defmenuitem{Print Interactions...} Opens a dialog for printing the
  contents of the @tech{interactions window}.}

@item{@defmenuitem{Search in Files...} Opens a dialog where you can
  specify the parameters of a multi-file search. The results of the
  search are displayed in a separate window.}

 @item{@defmenuitem{Close} Closes this DrScheme window. If this window
  is the only open DrScheme window, then DrScheme quits, except under
  Mac OS X.}

 @item{{@onscreen{Quit} or @onscreen{Exit}} Exits DrScheme. (Under Mac
  OS X, this menu item is in the Apple menu.)}

]

@; ----------------------------------------

@section{@onscreen{Edit}}

All @onscreen{Edit} menu items operate on either the definitions or
interactions window, depending on the location of the selection or
blinking caret. Each window maintains its own Undo and Redo history.

@itemize[

 @item{@defmenuitem{Undo} Reverses an editing action. Each window
  maintains a history of actions, so multiple @onscreen{Undo}
  operations can reverse multiple editing actions.}

 @item{@defmenuitem{Redo} Reverses an @onscreen{Undo} action. Each
  window (and boxed-subwindow) maintains its own history of
  @onscreen{Undo} actions, so multiple @onscreen{Redo} operations can
  reverse multiple @onscreen{Undo} actions.}

 @item{@defmenuitem{Cut} Copies the selected text to the clipboard and
  deletes it from the window.}

 @item{@defmenuitem{Copy} Copies the selected text to the clipboard.}

 @item{@defmenuitem{Paste} Pastes the current clipboard contents into the
  window.}

 @item{@defmenuitem{Delete} or @defmenuitem{Clear} Deletes the selected text.}

 @item{@defmenuitem{Select All} Highlights the entire text of the buffer.}

  @item{@defmenuitem{Wrap Text} Toggles between wrapped text and
  unwrapped text in the window.}

 @item{@defmenuitem{Find...} Opens an interactive search
 window at the bottom of the frame and moves the insertion
 point to the search string editor (or out of it, if the
 insertion point is already there).

 See also @secref["Searching"].}

 @item{@defmenuitem{Find Again} Finds the next occurrence of the text 
 in the search window.}
  
 @item{@defmenuitem{Find Again Backwards} Finds the next occurrence of the text 
 in the search window, but searching backwards.}
  
@item{@defmenuitem{Replace & Find Again} Replaces the selection with the
  replace string (if it matches the find string) and finds the next
  occurrence of the text that was last searched for, looking forwards.}

@item{@defmenuitem{Replace & Find Again Backwards} Replaces the selection with the
  replace string (if it matches the find string) and finds the next
  occurrence of the text that was last searched for, looking backwards.}

@item{@defmenuitem{Replace All} Replaces all occurrences of
the search string with the replace string.}

@item{@defmenuitem{Find Case Sensitive} Toggles between
case-sensitive and case-insensitive search.}
  
@item{@defmenuitem{Keybindings} 
@itemize[

@item{@defmenuitem{Show Active Keybindings} Shows all of the
  keybindings available in the current window.}

@item{@defmenuitem{Add User-defined Keybindings...} Choosing this menu
  item opens a file dialog where you can select a file containing
  Scheme-definitions of keybindings. See @secref["defining-shortcuts"]
  for more information.}

]}

@item{@defmenuitem{Complete Word} Completes the word at the
insertion point, using the manuals as a source of completions.}

 @item{@defmenuitem{Preferences...} Opens the preferences dialog. See
  @secref["prefs-explanation"]. (Under Mac OS X, this menu item is in
  the Apple menu.)}  ]

@; ----------------------------------------

@section[#:tag "menu:view"]{@onscreen{View}}

One each of the following show/hide pairs of menu items
appears at any time.

@itemize[

 @item{@defmenuitem{Show Definitions} Shows the definitions window.}

 @item{@defmenuitem{Hide Definitions} Hides the definitions window.}

 @item{@defmenuitem{Show Interactions} Shows interactions window.}

 @item{@defmenuitem{Hide Interactions} Hides interactions window.}

 @item{@defmenuitem{Show Program Contour} Shows a ``20,000 foot''
   overview window along the edge of the DrScheme
   window. Each pixel in this window corresponds to a letter
   in the program text.}

 @item{@defmenuitem{Hide Program Contour} Hides the contour window.}
   
 @item{@defmenuitem{Show Module Browser} Shows the module DAG rooted
   at the currently opened file in DrScheme.}

 @item{@defmenuitem{Hide Module Browser} Hides the module browser.}

 @item{@defmenuitem{Toolbar} 
@itemize[
@item{@defmenuitem{Toolbar on Left} Moves the tool bar (defaultly on the top of DrScheme's window) to the left-hand side, organized vertically.}
@item{@defmenuitem{Toolbar on Top} Moves the toolbar to the top of the DrScheme window.}
@item{@defmenuitem{Toolbar on Right} Moves the tool bar to the right-hand side, organized vertically.}
@item{@defmenuitem{Toolbar Hidden} Hides the toolbar entirely.}]}

 @item{@defmenuitem{Show Log} Shows the current log messages.}
 @item{@defmenuitem{Hide Log} Hides the current log messages.}

 @item{@defmenuitem{Show Profile} Shows the current profiling
   report. This menu is useful only if you have enabled profiling in
   the @onscreen{Choose Language...} dialog's @onscreen{Details}
   section. Profiling does not apply to all languages.}
   
 @item{@defmenuitem{Hide Profile} Hides any profiling
   information currently displayed in the DrScheme window.}

 @item{@defmenuitem{Dock Test Report} Like the dock button on the test report
   window, this causes all test report windows to merge with the appropriate
   DrScheme window at the bottom of the frame.}
  @item{@defmenuitem{Undock Test Report} Like the undock button on the test report
   window, this causes the test reports attached to appropriate DrScheme tabs
   to become separate windows.}
 
 @item{@defmenuitem{Show Tracing} Shows a trace of functions called since
   the last time @onscreen{Run} was clicked. This menu is useful only if
   you have enabled tracing in the @onscreen{Choose Language...} dialog's
   @onscreen{Details} section. Profiling does not apply to all languages.}
   
 @item{@defmenuitem{Hide Tracing} Hides the tracing display.}

 @item{@defmenuitem{Split} Splits the current window in half to
  allow for two different portions of the current window to
  be visible simultaneously.}

 @item{@defmenuitem{Collapse} If the window has been split before, this
   menu item becomes enabled, allowing you to collapse the split
   window.}

]

Note: whenever a program is run, the interactions window is made
 visible if it is hidden.

@; ----------------------------------------

@section{@onscreen{Language}}

@itemize[

 @item{@defmenuitem{Choose Language...} Opens a dialog for selecting
  the current evaluation language. Click @onscreen{Run} to make the
  language active in the interactions window. See
  @secref["choose-language"] for more information about the
  languages.}


 @item{@defmenuitem{Add Teachpack...} Opens a find-file dialog for
  choosing a teachpack to extend the current language. Click
  @onscreen{Run} to make the teachpack available in the interactions
  windows. See @secref["extending-drscheme"] for information on
  creating teachpacks.}

@item{@defmenuitem{Clear All Teachpacks} Clears all of the current
  teachpacks.  Click @onscreen{Run} to clear the teachpack from the
  interactions window.}

]

In addition to the above items, a menu item for each teachpack that
clears only the corresponding teachpack.

@; ----------------------------------------

@section[#:tag "menu:scheme"]{@onscreen{Scheme}}

@itemize[

 @item{@defmenuitem{Run} Resets the interactions window and runs the
  program in the definitions window.}

 @item{@defmenuitem{Break} Breaks the current evaluation.}

 @item{@defmenuitem{Kill} Terminates the current evaluation.}

@item{@defmenuitem{Limit Memory...} Allow you to specify a
limit on the amount of memory that a program running in
DrScheme is allowed to consume.}

@item{@defmenuitem{Clear Error Highlight} Removes the red
background that signals the source location of an error.}

 @item{@defmenuitem{Create Executable...} Creates a separate launcher
   for running your program. See @secref["create-exe"] for more
   info.}

@item{@defmenuitem{Module Browser...} Prompts for a file and
  then opens a window showing the module import structure
  for the module import DAG starting at the selected module.
  
  The module browser window contains a square for each
  module. The squares are colored based on the number of
  lines of code in the module. If a module has more lines of
  code, it gets a darker color.
  
  In addition, for each normal import, a blue line drawn is
  from the module to the importing module. Similarly, purple
  lines are drawn for each for-syntax import. In the initial
  module layout, modules to the left import modules to the
  right, but since modules can be moved around
  interactively, that property might not be preserved.

  To open the file corresponding to the module, right-click or
  control-click (Mac OS X) on the box for that module.}

 @item{@defmenuitem{Reindent} Indents the selected text according to
  the standard Scheme formatting conventions. (Pressing the Tab key
  has the same effect.)}
  
 @item{@defmenuitem{Reindent All} Indents all of the text in either
  the definitions or interactions window, depending on the location of
  the selection or blinking caret.}
  
 @item{@defmenuitem{Comment Out with Semicolons} Puts @litchar{;}
  characters at each of the beginning of each selected line of text.}

 @item{@defmenuitem{Comment Out with a Box} Boxes the selected
  text with a comment box.}
  
 @item{@defmenuitem{Uncomment} Removes all @litchar{;} characters at
  the start of each selected line of text or removes a comment box
  around the text. Uncommenting only removes a @litchar{;} if it
  appears at the start of a line and it only removes the first
  @litchar{;} on each line.}
           
 @item{@defmenuitem{Disable Tests} Stops tests written in the definitions
  window from evaluating when the program is Run. Tests can be enabled
  using the @onscreen{Enable Tests} menu item. Disabling tests freezes
  the contents of any existing test report window.
  }

 @item{@defmenuitem{Enable Tests} Allows tests written in the definitions
  window to evaluate when the program is Run. Tests can be disabled using
  the @onscreen{Disable Tests} menu item.
  }

]

@section{@onscreen{Insert}}

@itemize[

 @item{@defmenuitem{Insert Comment Box} Inserts a box that is ignored
  by DrScheme; use it to write comments for people who read your
  program.}

 @item{@defmenuitem{Insert Image...} Opens a find-file dialog for
  selecting an image file in GIF, BMP, XBM, XPM, PNG, or JPG
  format. The image is treated as a value.}

 @item{@defmenuitem{Insert Fraction...} Opens a dialog for a
   mixed-notation fraction, and inserts the given fraction into the
   current editor.}
  
 @item{@defmenuitem{Insert Large Letters...} Opens a dialog for a line of
   text, and inserts a large version of the text (using semicolons and
   spaces).}

 @item{@defmenuitem{Insert @|lam-str|} Inserts the symbol @|lam-str|
   (as a Unicode character) into the program. The @|lam-str| symbol is
   normally bound the same as @scheme[lambda].}

 @item{@defmenuitem{Insert Java Comment Box} Inserts a box that is
   ignored by DrScheme. Unlike the @onscreen{Insert Comment Box} menu
   item, this is designed for the ProfessorJ language levels. See
   @secref["profj"].}

 @item{@defmenuitem{Insert Java Interactions Box} Inserts a box that
   will allows Java expressions and statements within Scheme
   programs. The result of the box is a Scheme value corresponding to
   the result(s) of the Java expressions. At this time, Scheme values
   cannot enter the box. The box will accept one Java statement or
   expression per line.}

 @item{@defmenuitem{Insert XML Box} Inserts an XML; see
   @secref["xml-boxes"] for more information.}

 @item{@defmenuitem{Insert Scheme Box} Inserts a box to contain Scheme
   code, typically used inside an XML box; see @secref["xml-boxes"].}

 @item{@defmenuitem{Insert Scheme Splice Box} Inserts a box to contain Scheme
   code, typically used inside an XML box; see also @secref["xml-boxes"].}

 @item{@defmenuitem{Insert Pict Box} Creates a box for generating a
   Slideshow picture. Inside the pict box, insert and arrange Scheme
   boxes that produce picture values.}

]  
 
@; ----------------------------------------

@section{@onscreen{Windows}}

@itemize[

 @item{@defmenuitem{Bring Frame to Front...}  Opens a window that lists
   all of the opened DrScheme frames. Selecting one of them brings the
   window to the front.}

 @item{@defmenuitem{Most Recent Window} Toggles between the currently
   focused window and the one that most recently had the focus.}

]

Additionally, after the above menu items, this menu contains
an entry for each window in DrScheme. Selecting a menu item
brings the corresponding window to the front.

@; ----------------------------------------

@section{@onscreen{Help}}

@itemize[

 @item{@defmenuitem{Help Desk} Opens the Help Desk. This is the clearing 
 house for all documentation about DrScheme and its language.}
 
 @item{@defmenuitem{About DrScheme...} Shows the credits for DrScheme.}

 @item{@defmenuitem{Related Web Sites} Provides links to related web sites.}

 @item{@defmenuitem{Tool Web Sites} Provides links to web sites for
   installed tools.}

 @item{@defmenuitem{Interact with DrScheme in English} Changes DrScheme's
   interface to use English; the menu item appears only when the
   current language is not English. Additional menu items switch
   DrScheme to other languages.}

]

