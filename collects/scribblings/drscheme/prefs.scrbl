#lang scribble/doc
@(require "common.ss"
          scribble/bnf
          scribble/struct)

@(define (PrefItem . s) (apply onscreen s))

@title[#:tag "prefs-explanation"]{Preferences}

The preferences dialog consists of several panels.

@section{@onscreen{Font}}

 This panel controls the main font used by DrScheme.

@section{@onscreen{Colors}}

  The @onscreen{Colors} panel has several sub-panels that let you
  configure the colors that DrScheme uses for the editor background,
  for highlighting matching parentheses, for the online coloring for
  Scheme and Java modes, for @onscreen{Check Syntax}, and for the
  colors of the text in the @tech{interactions window}.

  It also has two buttons, @onscreen{White on Black} and
  @onscreen{Black on White}, which set a number of defaults for the
  color preferences and change a few other aspects of DrScheme's
  behavior to make DrScheme's colors look nicer for those two modes.

@section{@onscreen{Editing}}
  
  The @onscreen{Editing} panel consists of several sub-panels:

  @itemize{

   @item{@onscreen{Indenting}

         This panel controls which keywords DrScheme recognizes for
         indenting, and how each keyword is treated.}

 @item{@onscreen{Square bracket}

      This panel controls which keywords DrScheme uses to determine
      when to rewrite @litchar{[} to @litchar{(}. For
      @scheme[cond]-like keywords, the number in parenthesis indicates
      how many sub-expressions are skipped before square brackets are
      started.

      See @secref["editor"] for details on how the entries in the
      columns behave.}

 @item{@onscreen{General}

  @itemize{

   @item{@PrefItem{Number of recent items} --- controls the
     length of the @onscreen{Open Recent} menu (in the @onscreen{File}
     menu).} 

   @item{@PrefItem{Auto-save files} --- If checked, the editor
    generates autosave files (see @secref["drscheme-autosave-files"])
    for files that have not been saved after five minutes.}
    
  @item{@PrefItem{Backup files} --- If checked, when saving a file for
    the first time in each editing session, the original copy of the
    file is copied to a backup file in the same directory. The backup
    files have the same name as the original, except that they end in
    either @indexed-file{.bak} or @indexed-file{~}.}

   @item{@PrefItem{Map delete to backspace} --- If checked, the editor
    treats the Delete key like the Backspace key.}

   @item{@PrefItem{Show status-line} --- If checked, DrScheme shows a
    status line at the bottom of each window.}

   @item{@PrefItem{Count column numbers from one} --- If checked, the
    status line's column counter counts from one. Otherwise, it counts
    from zero.}

   @item{@PrefItem{Display line numbers in buffer; not character
    offsets} --- If checked, the status line shows a
    @nonterm{line}:@nonterm{column} display for the current selection
    rather than the character offset into the text.}

   @item{@PrefItem{Wrap words in editor buffers} --- If checked,
     DrScheme editors auto-wrap text lines by default. Changing this
     preference affects new windows only.}

   @item{@PrefItem{Use separate dialog for searching} --- If checked,
    then selecting the @onscreen{Find} menu item opens a separate
    dialog for searching and replacing. Otherwise, selecting
    @onscreen{Find} opens an interactive search-and-replace panel at
    the bottom of a DrScheme window.}

   @item{@PrefItem{Reuse existing frames when opening new files} ---
    If checked, new files are opened in the same DrScheme window,
    rather than creating a new DrScheme window for each new file.}

   @item{@PrefItem{Enable keybindings in menus} --- If checked, some
    DrScheme menu items have keybindings. Otherwise, no menu items
    have key bindings. This preference is designed for people who are
    comfortable editing in Emacs and find the standard menu
    keybindings interfere with the Emacs keybindings.}
    
  @item{@PrefItem{Color syntax interactively} --- If checked, DrScheme
    colors your syntax as you type.}

  @item{@PrefItem{Automatically print to PostScript file} --- If
    checked, printing will automatically save PostScript files. If
    not, printing will use the standard printing mechanisms for your
    computer.}

  @item{@PrefItem{Open files in separate tabs (not separate windows)}
    -- If checked, DrScheme will use tabs in the front-most window to
    open new files, rather than creating new windows for new files.}

  @item{@PrefItem{Automatically open interactions window when running
    a program} -- If checked, DrScheme shows the interactions window
    (if it is hidden) when a program is run.}

@item{@PrefItem{Put the interactions window beside the definitions
   window} -- If checked, DrScheme puts the interactions window to the
   right of the definitions window. By default, the interactions
   window is below the definitions window.}

@item{@PrefItem{Always show the #lang line in the Module language} --
  If checked, the module language always shows the the @hash-lang[]
  line (even when it would ordinarily be scrolled off of the page), assuming
  that the @hash-lang[] line is the first line in the file.
}

  }}

 @item{@onscreen{Scheme}

  @itemize{

   @item{@PrefItem{Highlight between matching parens} --- If checked, the
    editor marks the region between matching parenthesis with a gray
    background (in color) or a stipple pattern (in monochrome) when
    the blinking caret is next to a parenthesis.}

   @item{@PrefItem{Correct parens} --- If checked, the editor
    automatically converts a typed @litchar{)} to @litchar{]} to
    match @litchar{[}, or it converts a typed @litchar{]} to
    @litchar{)} to match @litchar{(}. Also, the editor changes
    typed @litchar{[} to match the context (as explained in
    @secref["editor"]).}

   @item{@PrefItem{Flash paren match} --- If checked, typing a closing
    parenthesis, square bracket, or quotation mark flashes the
    matching open parenthesis/bracket/quote.}

  }}

}

@section{@onscreen{Warnings}}

  @itemize{

   @item{@PrefItem{Ask before changing save format} --- If checked,
    DrScheme consults the user before saving a file in non-text format
    (see @secref["drscheme-file-formats"]).}

   @item{@PrefItem{Verify exit} --- If checked, DrScheme consults the
    user before exiting.}

   @item{@PrefItem{Only warn once when executions and interactions are
    not synchronized} --- If checked, DrScheme warns the user on the
    first interaction after the definitions window, language, or
    teachpack is changed without a corresponding click on
    @onscreen{Run}. Otherwise, the warning appears on every
    interaction.}

   @item{@PrefItem{Ask about clearing test coverage} --- If checked,
     when test coverage annotations are displayed DrScheme prompts
     about removing them. This setting only applies to the PLT
     languages. DrScheme never asks in the teaching languages.}

   @item{@PrefItem{Check for newer PLT Scheme versions} --- If
     checked, DrScheme periodically polls a server to determine
     whether a newer version of DrScheme is available.}

 }

@section{@onscreen{Profiling}}
  
  This preference panel configures the profiling report. The band of
  color shows the range of colors that profiled functions take
  on. Colors near the right are used for code that is not invoked
  often and colors on the right are used for code that is invoked
  often.
  
  If you are interested in more detail at the low end, choose the
  @onscreen{Square root} check box. If you are interested in more
  detail at the upper end, choose the @onscreen{Square} check box.

@section{@onscreen{Browser}}
  
  This preferences panel allows you to configure your HTTP
  proxy. Contact your system administrator for details.
