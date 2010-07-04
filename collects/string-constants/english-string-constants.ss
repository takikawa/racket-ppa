#|

When modifying the string constants files,
please adhere to these guidelines:

- All the entries in english-string-constants.ss have the same format
  (name string).  If the purpose of an entry you are adding to the
  file is not clear from just the name and string, put a comment next
  to the entry explaining what the string is going to be used for and
  in what context.
  That's especially true for strings that contain things like "~a".
  Systematically describe what the "~a" is going to be replaced with.
  When we have to translate strings like "deleting ~a", we translators
  need to know what "~a" is going to be (in particular, in some
  languages like French, we need to know whether the "~a" is going to
  be a masculine or feminine word, or whether it's going to be
  singular or plural, etc).

- When adding a bunch of new entries, put together in a section the
  entries that logically belong together.  Give a title to your
  section, so we have an idea of what the strings are for.  Don't mix
  in the same section strings that have nothing to do with each other,
  that's only going to confuse us.  Do not start a new section if
  there's already one that deals with the same thing.  Dumping all the
  new entries in random order at the end of the file is not a good way
  to have your new entries translated in a timely manner...

- Before adding strings for your new pet tool, check whether you can't
  re-use strings that already exist.  There's no need for yet another
  "Ok" string...

- If you modify an existing string in english-string-constants.ss, go
  through all the *-string-constants.ss files for the other languages,
  comment out the old version of the modified string in each of these
  files, and put a short comment there telling us the English string
  has changed and needs to be re-translated.  Do not erase the old
  version, it might help us translate the new one.  Do not move it
  either.  Just comment it out and add the short comment.  After the
  next svn update DrScheme will automatically tell us translators that
  a new string needs to be translated, we will find your comment in
  the file, and know what to do.
	Some evil evil people might think that, since DrScheme automatically
  informs us of new strings to be translated, an easier thing to do
  when modifying an existing string would be to simply rename it at
  the same time.  This works, except that if you do that, we
  translators will get two warnings from DrScheme:
		language english had but french does not:
		(new-name "New String")
		language french had but english does not:
		(old-name "Old String")
  then we translators will be left to wonder whether the two things
  are related or not, and whether we can safely base our translation
  of "New String" on the translation of "Old String" (since the two
  strings are likely to be close in meaning).  Worse, we might not
  even realize the two strings are related and translate "New String"
  from scratch, just to realize later that it's only a variation of
  "Old String".  I can tell you that nothing pisses off a translator
  more than having to translate pretty much the same string twice
  just because *you* were too lazy to inform us that it was just a
  slight modification to an existing string rather than an entirely
  new one.  Conclusion: do not change the name of a string unless you
  have some really good reason to do so.

- Please think hard before choosing a string and make sure it means
  what you want it to mean.  That way you won't have to change it
  later, and we won't have to retranslate it.

- Please think hard before choosing the name for a string.  Use
  meaningful names.  "error" or "ok" are not meaningful names.  Prefix
  all related names with a common prefix (the name of your tool or
  module).  String names are not the right place to save on typing.

- If, for some reason, you really have to change the name of a string
  (like, because the original name you gave it really sucked...),
  change the name of the string in *ALL* the *-string-constants.ss
  files.  That's a modification you can do without the help of a
  translator, so do it yourself.  It's not the job of the translators
  to clean up your naming mess for you.  Besides, you are the one who
  knows what you changed, so leaving the translators having to guess
  what you did is Not Nice(tm).

- If, for some reason, you need to remove a string (i.e. you changed
  your code and don't need the string anymore), remove the string in
  *ALL* the *-string-constants.ss files.  Again, you don't need the
  help of a translator to do that.  If you're not sure whether you
  might need the string in the future or not, just comment it out in
  *ALL* the files.

|#

(module english-string-constants "string-constant-lang.ss"
 ;;; when translating this constant, substitute name of actual langauge for `English'
 (is-this-your-native-language "Is English Your Native Language?")

 (are-you-sure-you-want-to-switch-languages
  "This will change the language of the GUI, which requires you to restart DrScheme. Are you sure?")

 (interact-with-drscheme-in-language "Interact with DrScheme in English")

 ;; these two should probably be the same in all languages excepet English.
 ;; they are the button labels (under macos and windows, respectively)
 ;; that go the with the string above.
 (accept-and-quit "Accept and Quit")
 (accept-and-exit "Accept and Exit")
 
 ;;; general purpose (DrScheme is hereby a word in every language, by decree of Robby :)
 (plt "PLT")
 (drscheme "DrScheme")
 (ok "OK")
 (cancel "Cancel")
 (abort "Abort")
 (untitled "Untitled")
 (untitled-n "Untitled ~a")
 (warning "Warning")
 (error "Error")
 (close "Close") ;; as in, close an open window. must match close-menu-item
                 ;; in the sense that, when the &s have been stripped from
                 ;; close-menu-item, it must be the same string as this.
 (stop "Stop")   
 (&stop "&Stop") ;; for use in button and menu item labels, with short cut.
 (are-you-sure-delete? "Are you sure you want to delete ~a?") ;; ~a is a filename or directory name
 (ignore "Ignore")
 (revert "Revert")

 ;; label for a generic check box, often supported on dialogs
 ;; that ask a binary choice of the user. If checked, the
 ;; dialog isn't going to be shown again.
 ;; One version for always using the current choice:
 (dont-ask-again-always-current "Do not ask again (always use current choice)")
 ;; One generic version (ie, on the Quit DrScheme dialog)
 (dont-ask-again                "Do not ask again")

 ;;; important urls
 (web-materials "Related Web Sites") ;; menu item title
 (tool-web-sites "Tool Web Sites")   ;; menu item title
 (drscheme-homepage "DrScheme")
 (plt-homepage "PLT")
 (how-to-use-scheme "How to Use Scheme") ;; title of a book.
 (teachscheme!-homepage "TeachScheme!") ;; probably this should be a `word' in all languages

 ;;; bug report form
 (cancel-bug-report? "Cancel Bug Report?")
 (are-you-sure-cancel-bug-report?
  "Are you sure that you want to cancel sending this bug report?")
 (bug-report-form "Bug Report Form")
 (bug-report-field-name "Name")
 (bug-report-field-email "Email")
 (bug-report-field-summary "Summary")
 (bug-report-field-severity "Severity")
 (bug-report-field-class "Class")
 (bug-report-field-description "Description")
 (bug-report-field-reproduce1 "Steps to")
 (bug-report-field-reproduce2 "Reproduce")
 (bug-report-field-environment "Environment")
 (bug-report-field-docs-installed "Docs Installed")
 (bug-report-field-collections "Collections")
 (bug-report-field-human-language "Human Language")
  (bug-report-field-memory-use "Memory Use")
 (bug-report-field-version "Version")
 (bug-report-synthesized-information "Synthesized Information")  ;; dialog title
 (bug-report-show-synthesized-info "Show Synthesized Info")
 (bug-report-submit "Submit")
 (bug-report-submit-menu-item "Submit Bug Report") ;; in Help Menu (drs & help desk)
 (error-sending-bug-report "Error Sending Bug Report")
 (error-sending-bug-report-expln "An error occurred when sending this bug report. If your internet connection is otherwise working fine, please visit:\n\n    http://bugs.plt-scheme.org/\n\nand submit the bug via our online web-form. Sorry for the difficulties.\n\nThe error message is:\n~a")
 (illegal-bug-report "Illegal Bug Report")
 (pls-fill-in-field "Please fill in the \"~a\" field")
 (malformed-email-address "Malformed email address")
 (pls-fill-in-either-description-or-reproduce "Please fill in either the Description field or the Steps to Reproduce field.")

 ;;; check syntax
 (check-syntax "Check Syntax")
 (cs-italic "Italic")
 (cs-bold "Bold")
 (cs-underline "Underline")
 (cs-change-color "Change Color")
 (cs-tack/untack-arrow "Tack/Untack Arrow(s)")
 (cs-jump-to-next-bound-occurrence "Jump to Next Bound Occurrence")
 (cs-jump-to-binding "Jump to Binding Occurrence")
 (cs-jump-to-definition "Jump to Definition")
 (cs-error-message "Error Message")
 (cs-open-file "Open ~a")
 (cs-rename-var "Rename ~a")
 (cs-rename-id "Rename Identifier")
 (cs-rename-var-to "Rename ~a to:")
 (cs-name-duplication-error "The new name you have chosen, ~s, conflicts with an already established name in this scope.")
 (cs-rename-anyway "Rename Anyway")
 (cs-status-init "Check Syntax: Initializing environment for user code")
 (cs-status-coloring-program "Check Syntax: coloring expression")
 (cs-status-eval-compile-time "Check Syntax: eval compile time")
 (cs-status-expanding-expression "Check Syntax: expanding expression")
 (cs-status-loading-docs-index "Check Syntax: loading documentation index")
 (cs-mouse-over-import "binding ~s imported from ~s")
 (cs-view-docs "View documentation for ~a")
  
 (cs-lexical-variable "lexical variable")
 (cs-imported-variable "imported variable")

 ;;; info bar at botttom of drscheme frame
 (collect-button-label "GC")
  (read-only "Read only")
 (auto-extend-selection "Auto-extend")
 (overwrite "Overwrite")
 (running "running")
 (not-running "not running")
 
 ;;; misc
 (welcome-to-something "Welcome to ~a")
 
 ; this appears in the drscheme about box.
 (welcome-to-drscheme-version/language "Welcome to DrScheme, version ~a, ~a")

 ; these appear on subsequent lines in the `Help|Welcome to DrScheme' dialog.
 (welcome-to-drscheme "Welcome to DrScheme")

 (goto-line "Goto line")
 (goto-line-invalid-number
  "~a is not a valid line number. It must be an integer between 1 and ~a")
 (goto-position "Goto Position")
 (no-full-name-since-not-saved
  "The file does not have a full name because it has not yet been saved.")
 (cannot-open-because-dne "Cannot open ~a because it does not exist.")

  (needs-execute-language-changed
   "WARNING: The language has changed. Click Run.")
  (needs-execute-teachpack-changed
   "WARNING: The teachpacks have changed. Click Run.")
  (needs-execute-defns-edited
   "WARNING: The definitions window has changed. Click Run.")
  
 (file-is-not-saved "The file \"~a\" is not saved.")
 (save "Save")
 (close-anyway "Close Anyway")
 (dont-save "Don't Save")
 (clear-anyway "Clear Anyway")

 ;; menu item title
 (log-definitions-and-interactions "Log Definitions and Interactions...")
 (stop-logging "Stop Logging")
 (please-choose-a-log-directory "Please choose a log directory")
 (logging-to "Logging to: ")
 (erase-log-directory-contents "Erase contents of log directory: ~a?")
 (error-erasing-log-directory "Error erasing log directory contents.\n\n~a\n")

 ;; modes
 (mode-submenu-label "Modes")
 (scheme-mode "Scheme mode")
 (text-mode "Text mode")

 (scheme-mode-color-symbol "Symbol")
 (scheme-mode-color-keyword "Keyword")
 (scheme-mode-color-comment "Comment")
 (scheme-mode-color-string "String")
 (scheme-mode-color-constant "Constant")
 (scheme-mode-color-parenthesis "Parenthesis")
 (scheme-mode-color-error "Error")
 (scheme-mode-color-other "Other")
 ;; the ~a is filled in with one of the above (scheme-mode-*)
 (syntax-coloring-choose-color "Choose a color for ~a")
 (preferences-colors "Colors") ;; used in the preferences dialog
 
  ;; parenthesis color scheme string constants
  (parenthesis-color-scheme "Parenthesis color scheme") ;; label for the choice% menu in the preferences dialog
  (paren-color-basic-grey "Basic grey")
  (paren-color-shades-of-gray "Shades of grey")
  (paren-color-shades-of-blue "Shades of blue")
  (paren-color-spring "Spring")
  (paren-color-fall "Fall")
  (paren-color-winter "Winter")

  
 (url: "URL:")
 (open-url... "Open URL...")
 (open-url "Open URL")
 (browse... "Browse...")
 (bad-url "Bad URL")
 (bad-url:this "Bad URL: ~a")
 
 ;; Help Desk
 (help "Help")
 (help-desk "Help Desk")
 (plt:hd:search "Search")
 (plt:hd:feeling-lucky "Feeling Lucky")
 (plt:hd:home "Help Desk home") 
 ; next 3 are popup menu choices in help desk search frame
 (plt:hd:search-for-keyword "Keyword entry")
 (plt:hd:search-for-keyword-or-index "Keyword or index entry")
 (plt:hd:search-for-keyword-or-index-or-text "Keyword, index entry, or text")
 (plt:hd:exact-match "Exact match")
 (plt:hd:containing-match "Containing match")
 (plt:hd:regexp-match "Regexp match")
 (plt:hd:find-docs-for "Find docs for:")
 (plt:hd:search-stopped-too-many-matches "[Search aborted: too many matches]")
 (plt:hd:nothing-found-for "Nothing found for ~a")
 (plt:hd:and "and")
 (plt:hd:refresh "refresh")
 (plt:hd:refresh-all-manuals "refresh all manuals")
 (plt:hd:manual-installed-date "(installed ~a)")
 ; Help Desk configuration
 ;; refreshing manuals
 (plt:hd:refreshing-manuals "Re-downloading Manuals")
 (plt:hd:refresh-downloading... "Downloading ~a...")
 (plt:hd:refresh-deleting... "Deleting old version of ~a...")
 (plt:hd:refresh-installing... "Installing new version of ~a...")
 (plt:hd:refresh-clearing-indicies "Clearing cached indices")
 (plt:hd:refreshing-manuals-finished "Finished.")
 (plt:hd:about-help-desk "About Help Desk")
 (plt:hd:help-desk-about-string
  "Help Desk is a complete source of information about PLT software, including DrScheme, MzScheme, and MrEd.\n\nVersion ~a\nCopyright (c) ~a-~a PLT")
 (plt:hd:help-on-help "Help on Help")
 (plt:hd:help-on-help-details "For help on using Help Desk, follow the first link `Help Desk' on Help Desk's home page. (To get to the home page if you're not already there, click the `Home' button at the top of the Help Desk window.)")
  (reload "Reload") ;; refresh the page in a web browser
  (plt:hd:ask-about-separate-browser
   "You have selected a link to content from the world-wide web. Would you like to view it in the Help Desk browser, or would you like to use a separate browser program to view it?")
  (plt:hd:homebrew-browser "Help Desk Browser") ;; choice for the above string (in a button)
  (plt:hd:separate-browser "Separate Browser") ;; other choice for the above string (also in a button)
  (plt:hd:external-link-in-help "External URLs in Help")
  (plt:hd:use-homebrew-browser "Use Help Desk browser for external URLs")
  (plt:hd:new-help-desk "New Help Desk")

  ;; in the Help Desk language dialog, title on the right.
  (plt:hd:manual-search-ordering "Manual Search Order")
  
  ;; in the help-desk standalone font preference dialog, on a check box
  (use-drscheme-font-size "Use DrScheme's font size")
  
  ;; in the preferences dialog in drscheme there is example text for help desk font size.
  ;; clicking the links in that text produces a dialog with this message
  (help-desk-this-is-just-example-text
   "This is just example text for setting the font size. Open Help Desk proper (from Help menu) to follow these links.")

  ;; this appears in the bottom part of the frame the first time the user hits `f1' 
  ;; (assuming nothing else has loaded the documentation index first)
  ;; see also: cs-status-loading-docs-index
  (help-desk-loading-documentation-index "Help Desk: loading documentation index")
  
 ;; Help desk htty proxy
 (http-proxy "HTTP Proxy")
 (proxy-direct-connection "Direct connection")
 (proxy-use-proxy "Use proxy:")
 (proxy-host "Host")
 (proxy-port "Port")
 (proxy-bad-host "Bad Proxy Host")

 ;; browser
 (rewind-in-browser-history "Rewind")
 (forward-in-browser-history "Forward")
 (home "Home")
 (browser "Browser")
 (external-browser-choice-title "External Browser") ; title for radio-button set
 (browser-command-line-label "Command Line:") ; label for radio button that is followed by text boxes
 (choose-browser "Choose a Browser")
 (no-browser "Ask Later")
 (browser-cmdline-expl-line-1 "(Command line formed by concatenating pre-text, URL,") ; explanatory text for dialog, line 1
 (browser-cmdline-expl-line-2 "and post-text, with no extra spaces between them.)") ; ... line 2. (Anyone need more lines?)
 (install? "Install?")  ;; if a .plt file is found (title of dialog)
 (you-have-selected-an-installable-package "You have selected an installable package.")
 (do-you-want-to-install-it? "Do you want to install it?")
 (paren-file-size "(The file is ~a bytes)")
 (download-and-install "Download && Install") ;; button label
 (download "Download") ;; button label
 (save-downloaded-file/size "Save downloaded file (~a bytes) as") ;; label for get-file dialog
 (save-downloaded-file "Save downloaded file as")  ;; label for get-file dialog
 (downloading "Downloading") ;; dialog title
 (downloading-file... "Downloading file...")
 (package-was-installed "The package was installed.")
 (download-was-saved "The downloaded file was saved.")

 (install-plt-file-menu-item... "Install .plt File...")
 (install-plt-file-dialog-title "Install .plt File")
 (install-plt-web-tab "Web")
 (install-plt-file-tab "File")
 (install-plt-filename "Filename:")
 (install-plt-url "URL:")
 
 ;; install plt file when opened in drscheme strings
 (install-plt-file "Install ~a or open for editing?")
 (install-plt-file/yes "Install")
 (install-plt-file/no "Edit")

 (plt-installer-progress-window-title "Installer Progress") ;; frame title
 (plt-installer-abort-installation "Abort Installation") ;; button label
 (plt-installer-aborted "Aborted.") ;; msg that appears in the installation window when installation is aborted

 ;;; about box
 (about-drscheme-frame-title "About DrScheme")
 
 ;;; save file in particular format prompting.
 (save-as-plain-text "Save this file as plain text?")
 (save-in-drs-format "Save this file in drscheme-specific non-text format?")
 (yes "Yes")
 (no "No")
 
 ;;; preferences
 (preferences "Preferences")
 (error-saving-preferences "Error saving preferences: ~a")
 (error-saving-preferences-title "Error saving preferences")
 (steal-the-lock-and-retry "Steal the lock && retry") ;; in the preferences error dialog; this happens when the lockfile exists (after 3 pref writes). 
 (error-reading-preferences "Error reading preferences")
 (prefs-file-locked "The preferences file is locked (because the file ~a exists), so your preference change could not be saved. Cancel preference change?")
 (try-again "Try again") ;; button label
 (prefs-file-still-locked "The preferences file is still locked (because the file ~a exists), so your preference change will not be saved.")
 (scheme-prefs-panel-label "Scheme")
 (warnings-prefs-panel-label "Warnings")
 (editor-prefs-panel-label "Editing")
 (general-prefs-panel-label "General")
 (highlight-parens "Highlight between matching parens")
 (fixup-open-brackets "Automatically adjust opening square brackets")
 (fixup-close-parens "Automatically adjust closing parens")
 (flash-paren-match "Flash paren match")
 (auto-save-files "Auto-save files")
 (backup-files "Backup files")
 (map-delete-to-backspace "Map delete to backspace")
 (verify-exit "Verify exit")
 (ask-before-changing-format "Ask before changing save format")
 (wrap-words-in-editor-buffers "Wrap words in editor buffers")
 (show-status-line "Show status-line")
 (count-columns-from-one "Count column numbers from one")
 (display-line-numbers "Display line numbers in buffer; not character offsets")
 (show-line-and-column-numbers "Show line & column numbers") ; used for popup menu; right click on line/column box in bottom of drs window
 (show-character-offsets "Show character offsets") ; used for popup menu; right click on line/column box in bottom of drs window
 (enable-keybindings-in-menus "Enable keybindings in menus")
 (automatically-to-ps "Automatically print to PostScript file")
 (command-as-meta "Treat command key as meta") ;; macos/macos x only
 (separate-dialog-for-searching "Use separate dialog for searching")
 (reuse-existing-frames "Reuse existing frames when opening new files")
 (default-fonts "Default Fonts")
 (basic-gray-paren-match-color "Basic gray parenthesis highlight color") ; in prefs dialog
 (online-coloring-active "Color syntax interactively")
 (open-files-in-tabs "Open files in separate tabs (not separate windows)")
 (show-interactions-on-execute "Automatically open interactions window when running a program")
  (switch-to-module-language-automatically "Automatically switch to the module language when opening a module")
  (interactions-beside-definitions "Put the interactions window beside the definitions window") ;; in preferences, below the checkbox one line above this one
 (limit-interactions-size "Limit interactions size")
 (background-color "Background Color")
 (default-text-color "Default text") ;; used for configuring colors, but doesn't need the word "color"
 (choose-a-background-color "Please choose a background color")
 (revert-to-defaults "Revert to Defaults")
  
  (black-on-white-color-scheme "Black on White") ;; these two appear in the color preferences dialog on butttons
  (white-on-black-color-scheme "White on Black") ;; clicking the buttons changes teh color schemes to some defaults that've been set up.
  
 ; title of the color choosing dialog

 ; should have entire alphabet
 (font-example-string "The quick brown fox jumped over the lazy dogs.") 

 (change-font-button-label "Change")
 (fonts "Fonts")
 (other... "Other...") ;; used in the font choice menu item

 ; filled with type of font, eg modern, swiss, etc.
 (choose-a-new-font "Please choose a new \"~a\" font")

 (font-size-slider-label "Size")
 (restart-to-see-font-changes "Restart to see font changes")

 (font-prefs-panel-title "Font")
 (font-name "Font Name")
 (font-size "Font Size")
 (set-font "Set Font...")
 (font-smoothing-label  "Font smoothing")
 (font-smoothing-none "None")
 (font-smoothing-some "Some")
 (font-smoothing-all "All")
 (font-smoothing-default "Use system-wide default")
 (select-font-name "Select Font Name")
 (example-text "Example Text:")
 (only-warn-once "Only warn once when definitions and interactions are not synchronized")
 
 ; warning message when lockfile is around
 (waiting-for-pref-lock "Waiting for the preferences lockfile...")
 (pref-lock-not-gone
  "The preferences lockfile:\n\n   ~a\n\nprevents the preferences from being saved. Ensure that no PLT software is running and delete this file.")
 (still-locked-exit-anyway? "The preferences were not saved sucessfully. Exit anyway?")
 
 ;;; indenting preferences panel
 (indenting-prefs-panel-label "Indenting")
 (indenting-prefs-extra-regexp "Extra regexp")

 (square-bracket-prefs-panel-label "Square Bracket")
  
 ; filled with define, lambda, or begin
 (enter-new-keyword "Enter new ~a-like keyword:")
 (x-keyword "~a Keyword")
 (x-like-keywords "~a-like Keywords")

 ; used in Square bracket panel
 (skip-subexpressions "Number of sub-expressions to skip")

 (expected-a-symbol "expected a symbol, found: ~a")
 (already-used-keyword "\"~a\" is already a specially indented keyword")
 (add-keyword "Add")
 (remove-keyword "Remove")
 
  ; repl color preferences
  (repl-colors "REPL")
  (repl-out-color "Output")
  (repl-value-color "Values")
  (repl-error-color "Errors")
  
 ;;; find/replace
 (find-and-replace "Find and Replace")
 (find "Find")
 (replace "Replace")
 (dock "Dock")
 (undock "Undock")
 (replace&find-again "Replace && Find Again") ;;; need double & to get a single &
 (replace-to-end "Replace to End")
 (forward "Forward")
 (backward "Backward")
 (hide "Hide")
 (find-case-sensitive "Case sensitive")  ;; the check box in both the docked & undocked search
  
 ;;; multi-file-search
 (mfs-multi-file-search-menu-item "Search in Files...")
 (mfs-string-match/graphics "String match (handles files with graphics)")
 (mfs-regexp-match/no-graphics "Regular Expression (only raw text files)")
 (mfs-searching... "Searching...")
 (mfs-configure-search "Configure Search") ;; dialog title
 (mfs-files-section "Files")   ;; section in config dialog
 (mfs-search-section "Search") ;; section in config dialog
 (mfs-dir "Dir")
 (mfs-recur-over-subdirectories "Recur over subdirectories")
 (mfs-regexp-filename-filter "Regexp filename filter")
 (mfs-search-string "Search string")
 (mfs-drscheme-multi-file-search "DrScheme - Multi File Search") ;; results window and error message title
 (mfs-not-a-dir "\"~a\" is not a directory")
 (mfs-open-file "Open File")
 (mfs-stop-search "Stop Search")
 (mfs-case-sensitive-label "Case sensitive")
 (mfs-no-matches-found "No matches found.")
 (mfs-search-interrupted "Search aborted.")
 
 ;;; reverting a file
 (are-you-sure-revert
  "Are you sure that you want to revert this file? This change cannot be undone.")
 (are-you-sure-revert-title
  "Revert?")
 
 ;;; saving a file
 ; ~a is filled with the filename
 (error-saving "Error Saving") ;; title of error message dialog
 (error-saving-file/name "There was an error saving ~a.")
 (error-loading "Error Loading")
 (error-loading-file/name "There was an error loading ~a.")
 (unknown-filename "<< unknown >>")

 ;;; finder dialog
 (must-specify-a-filename "You must specify a file name")
 (file-does-not-exist "The file \"~a\" does not exist.")
 (ask-because-file-exists "The file \"~a\" already exists. Replace it?")
 (dne-or-cycle "The file \"~a\" contains a nonexistent directory or a cycle.")
 (get-file "Get file")
 (put-file "Put file")
 (full-pathname "Full pathname")
 (show-dot-files "Show files and directories that begin with a dot.")
 (up-directory-button-label "Up directory")
 (add-button-label "Add") ;;; for multi-file selection
 (add-all-button-label "Add all") ;;; for multi-file selection
 (remove-button-label "Remove") ;;; for multi-file selection
 (file-wrong-form "That filename does not have the right form.")
 (select-files "Select files")
 (select-file "Select a file")
 (dir-dne "That directory does not exist.")
 (file-dne "That file does not exist.")
 (empty-filename "The filename must have some letters in it.")
 (that-is-dir-name "That is a directory name.")
 
 ;;; raw menu names -- these must match the 
 ;;; versions below, once the &s have been stripped.
 ;;; if they don't, DrScheme's menus will appear
 ;;; in the wrong order.
 (file-menu "File")
 (edit-menu "Edit")
 (help-menu "Help")
 (windows-menu "Windows")
 
 ;;; menus
 ;;; - in menu labels, the & indicates a alt-key based shortcut.
 ;;; - sometimes, things are stuck in the middle of 
 ;;; menu item labels. For instance, in the case of
 ;;; the "Save As" menu, you might see: "Save Definitions As". 
 ;;; be careful of spacing, follow the English, if possible.
 ;;; - the ellipses in the `after' strings indicates that
 ;;; more information is required from the user before completing
 ;;; the command.

 (file-menu-label "&File")

 (new-info  "Open a new file")
 (new-menu-item "&New")
 (new-...-menu-item "&New...")

 (open-info "Open a file from disk")
 (open-menu-item "&Open...")
 (open-here-menu-item "&Open Here...")

 (open-recent-info "A list of the recently opened files")
 (open-recent-menu-item "Open Recen&t")

 (revert-info "Revert this file to the copy on disk")
 (revert-menu-item "&Revert")

 (save-info "Save this file to disk")
 (save-menu-item "&Save")

 (save-as-info "Prompt for a filename and save this file to disk")
 (save-as-menu-item "Save &As...")

 (print-info "Send this file to a printer")
 (print-menu-item "&Print...")

 (page-setup-info "Configure printing parameters")
 (page-setup-menu-item "Page Setup...")

 (close-info "Close this file")
 (close-menu-item "&Close")

 (quit-info "Close all windows")
 (quit-menu-item-windows "E&xit")
 (quit-menu-item-others "&Quit")
 
 (edit-menu-label "&Edit")
 
 (undo-info "Undo the most recent action")
 (undo-menu-item "&Undo")

 (redo-info "Undo the most recent undo")
 (redo-menu-item "&Redo")

 (cut-info "Move the selected items to the clipboard for later pasting")
 (cut-menu-item "Cu&t")

 (copy-info "Copy the selected items to the clipboard for later pasting")
 (copy-menu-item "&Copy")

 (paste-info "Paste the most recently copied or cut items, in place of the selected items")
 (paste-menu-item "&Paste")

 (clear-info "Erase the selected items without affecting the clipboard or pasting")
 (clear-menu-item-windows "&Delete")

 (select-all-info "Select the entire document")
 (select-all-menu-item "Select A&ll")
 
 (find-info "Search for a string")
 (find-menu-item "Find...")

 (find-again-info "Search for the same string as before")
 (find-again-menu-item "Find Again")
 
 (replace-and-find-again-info "Replace the current text and search for the same string as before")
 (replace-and-find-again-menu-item "Replace && Find Again")

  (complete-word "Complete Word") ; the complete word menu item in the edit menu
  (no-completions "... no completions available") ; shows up in the completions menu when there are no completions (in italics)
  
 (preferences-info "Configure your preferences")
 (preferences-menu-item "Preferences...")

 (keybindings-info "Show the currently active keybindings")
 (keybindings-menu-item "Keybindings")
 (keybindings-show-active "Show Active Keybindings")
 (keybindings-frame-title "Keybindings")
 (keybindings-sort-by-name "Sort by Name")
 (keybindings-sort-by-key "Sort by Key")
 (keybindings-add-user-defined-keybindings "Add User-defined Keybindings...")
 (keybindings-add-user-defined-keybindings/planet "Add User-defined Keybindings from PLaneT...")
 (keybindings-menu-remove "Remove ~a")
 (keybindings-choose-user-defined-file "Please choose a file containing keybindings.")
 (keybindings-planet-malformed-spec "The PLaneT spec is malformed: ~a") ; the string will be what the user typed in
 (keybindings-type-planet-spec "Please enter a PLaneT require spec (without the `require')")
  
 ; first ~a will be a string naming the file or planet package where the keybindings come from;
 ; second ~a will be an error message
 (keybindings-error-installing-file "Error when installing the keybindings ~a:\n\n~a")
  
 (user-defined-keybinding-error "Error running keybinding ~a\n\n~a")
 (user-defined-keybinding-malformed-file "The file ~a does not contain a module written in the (lib \"keybinding-lang.ss\" \"framework\") language.")  
  
 ;; menu items in the "special" menu
 (insert-text-box-item "Insert Text Box")
 (insert-image-item "Insert Image...")
 (insert-comment-box-menu-item-label "Insert Comment Box")
 (insert-lambda "Insert λ")

 (wrap-text-item "Wrap Text")

 (windows-menu-label "&Windows")
 (minimize "Minimize") ;; minimize and zoom are only used under mac os x
 (zoom "Zoom")
 (bring-frame-to-front "Bring Frame to Front")       ;;; title of dialog
 (bring-frame-to-front... "Bring Frame to Front...") ;;; corresponding title of menu item
 (most-recent-window "Most Recent Window")

 (view-menu-label "&View")
 (show-overview "Show Program Contour") 
 (hide-overview "Hide Program Contour")
 (show-module-browser "Show Module Browser")
 (hide-module-browser "Hide Module Browser")

 (help-menu-label "&Help")
 (about-info "Credits and details for this application")
 (about-menu-item "About...")
 
 ;; open here's new menu item
 (create-new-window-or-clear-current
  "Would you like to create a new window, or clear out the current one?")
 (clear-current "Clear Current")
 (new-window "New Window")

 ;;; exiting and quitting ``are you sure'' dialog
 ;;; exit is used on windows, quit on macos, in English. Other
 ;;; languages probably use the same word on both platforms.
 (exit "Exit")
 (quit "Quit")
 (are-you-sure-exit "Are you sure you want to exit?")
 (are-you-sure-quit "Are you sure you want to quit?")
  ; these next two are only used in the quit/exit dialog
  ; on the button whose semantics is "dismiss this dialog".
  ; they are there to provide more flexibility for translations
  ; in English, they are just cancel.
 (dont-exit "Cancel") 
 (dont-quit "Cancel")
  
 ;;; autosaving
 (error-autosaving "Error autosaving \"~a\".") ;; ~a will be a filename
 (autosaving-turned-off "Autosaving is turned off\nuntil the file is saved.")
 (recover-autosave-files-frame-title "Recover Autosaved Files")
 (autosave-details "Details")
 (autosave-recover "Recover")
 (autosave-unknown-filename "<<unknown>>")
  
  ;; these are labels in a dialog that drscheme displays
  ;; if you have leftover autosave files. to see the dialog,
  ;; start up drscheme and modify (but don't save) a file
  ;; (also, do this with an unsaved file). Wait for the autosave
  ;; files to appear (typically 5 minutes). Kill DrScheme
  ;; and restart it. You'll see the dialog
  (autosave-autosave-label: "Autosave file:")
  (autosave-original-label: "Original file:")
  (autosave-autosave-label "Autosave file")
  (autosave-original-label "Original file")
  (autosave-compare-files "Compare autosave files")

  (autosave-show-autosave "Autosave file") ;; title of a window showing the autosave file

  (autosave-explanation "DrScheme found autosave files, which may contain your unsaved work.")

  (autosave-recovered! "Recovered!") ;; status of an autosave file
  (autosave-deleted "Deleted")       ;; status of an autosave file

  (autosave-error-deleting "Error deleting ~a\n\n~a") ;; first is a filename, second is an error message from mz.
  (autosave-delete-button "Delete")
  (autosave-delete-title "Delete")  ;; title of a dialog asking for deletion confirmation
  (autosave-done "Done")
  
  ;; appears in the file dialog
  (autosave-restore-to-where? "Choose a place to save the autosave file.")
  
  
 ;;; file modified warning
 (file-has-been-modified
  "The file has been modified since it was last saved. Overwrite the modifications?")
 (overwrite-file-button-label "Overwrite")
 
 (definitions-modified 
  "The definitions text has been modified in the file-system; please save or revert the definitions text.")
 (drscheme-internal-error "DrScheme Internal Error")
 
 ;;; tools
 (invalid-tool-spec "The tool specification in collection ~a's info.ss file is invalid. Expected either a string or a non-empty list of strings, got: ~e")
 (error-invoking-tool-title "Error invoking tool ~s;~s")
 (tool-tool-names-same-length
  "expected `tool-names' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-icons-same-length
  "expected `tool-icons' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (tool-tool-urls-same-length
  "expected `tool-urls' and `tools' to be lists of the same length, in info.ss file for ~s, got ~e and ~e")
 (error-getting-info-tool
  "error loading info.ss file for ~s")
 (tool-error-phase1 "Error in phase 1 for tool ~s; ~s")
 (tool-error-phase2 "Error in phase 2 for tool ~s; ~s")


 ;;; define popup menu
 (end-of-buffer-define "<< end of buffer >>")
 (sort-by-name "Sort by name")
 (sort-by-position "Sort by position in file")
 (no-definitions-found "<< no definitions found >>")
 (jump-to-defn "Jump to definition of ~a")

 (recent-items-sort-by-age "Sort by Age")
 (recent-items-sort-by-name "Sort by Name")
 
 ;;; view menu
 (hide-definitions-menu-item-label "Hide &Definitions")
 (show-definitions-menu-item-label "Show &Definitions")
 (definitions-menu-item-help-string "Show/Hide the definitions window")
 (show-interactions-menu-item-label "Show &Interactions")
 (hide-interactions-menu-item-label "Hide &Interactions")
 (interactions-menu-item-help-string "Show/Hide the interactions window")
 (toolbar "Toolbar")
 (toolbar-on-top "Toolbar On Top")
 (toolbar-on-left "Toolbar On Left")
 (toolbar-on-right "Toolbar On Right")
 (toolbar-hidden "Toolbar Hidden")

 ;;; file menu
 (save-definitions-as "Save Definitions &As...")
 (save-definitions "Save Definitions")
 (print-definitions "Print Definitions...")
 (about-drscheme "About DrScheme")
 (save-other "Save Other")
 (save-definitions-as-text "Save Definitions As Text...")
 (save-interactions "Save Interactions")
 (save-interactions-as "Save Interactions As...")
 (save-interactions-as-text "Save Interactions As Text...")
 (print-interactions "Print Interactions...")
 (new-tab "New Tab")
 (close-tab "Close Tab") ;; must not have any &s in it.
 (close-tab-amp "&Close Tab") ;; like close-tab, but with an ampersand on the same letter as the one in close-menu-item
  
 ;;; edit-menu
 (split-menu-item-label "&Split")
 (collapse-menu-item-label "C&ollapse")
 
 ;;; language menu
 (language-menu-name "&Language")
 
 ;;; scheme-menu
 (scheme-menu-name "S&cheme")
 (execute-menu-item-label "Run")
 (execute-menu-item-help-string "Restart the program in the definitions window")
 (break-menu-item-label "Stop")
 (break-menu-item-help-string "Break the current evaluation")
 (kill-menu-item-label "Kill")
 (kill-menu-item-help-string "Kill the current evaluation")
 (limit-memory-menu-item-label "Limit memory...")
 (limit-memory-msg-1 "The limit will take effect the next time the program")
 (limit-memory-msg-2 "is Run, and it must be at least 100 megabytes.")
 (limit-memory-unlimited "Unlimited")
 (limit-memory-limited "Limited")
 (limit-memory-megabytes "Megabytes")
 (clear-error-highlight-menu-item-label "Clear Error Highlight")
 (clear-error-highlight-item-help-string "Removes the pink error highlighting")
 (reindent-menu-item-label "&Reindent")
 (reindent-all-menu-item-label "Reindent &All")
 (semicolon-comment-out-menu-item-label "&Comment Out with Semicolons")
 (box-comment-out-menu-item-label "&Comment Out with a Box")
 (uncomment-menu-item-label "&Uncomment")

 (convert-to-semicolon-comment "Convert to Semicolon Comment")
 
 ;;; executables
 (create-executable-menu-item-label "Create Executable...")
 (create-executable-title "Create Executable")
 (must-save-before-executable "You must save your program before creating an executable.")
 (save-a-mred-launcher "Save a MrEd Launcher")
 (save-a-mzscheme-launcher "Save a MzScheme Launcher")
 (save-a-mred-stand-alone-executable "Save a MrEd Stand-alone Executable")
 (save-a-mzscheme-stand-alone-executable "Save a MzScheme Stand-alone Executable")
 (save-a-mred-distribution "Save a MrEd Distribution")
 (save-a-mzscheme-distribution "Save a MzScheme Distribution")

 (definitions-not-saved "The definitions window has not been saved. The executable will use the latest saved version of the definitions window. Continue?")
 ;; The "-explanatory-label" variants are the labels used for the radio buttons in
 ;;  the "Create Executable..." dialog for the "(module ...)" language.
 (launcher "Launcher")
 (launcher-explanatory-label "Launcher (for this machine only, runs from source)")
 (stand-alone "Stand-alone")
 (stand-alone-explanatory-label "Stand-alone (for this machine only, run compiled copy)")
 (distribution "Distribution")
 (distribution-explanatory-label "Distribution (to install on other machines)")
 (executable-type "Type")
 (executable-base "Base")
 (filename "Filename: ")
 (create "Create")
 (please-specify-a-filename "Please specify a filename to create.")
 (~a-must-end-with-~a
  "The ~a filename\n\n  ~a\n\nis illegal. The filename must end with \".~a\".")
 (macosx-executables-must-end-with-app
  "The filename\n\n  ~a\n\nis illegal. Under MacOS X, an executable must be a directory whose name ends with .app.")
 (warning-directory-will-be-replaced
  "WARNING: the directory:\n\n  ~a\n\nwill be replaced. Proceed?")
 
 (distribution-progress-window-title "Distribution Progress")
 (creating-executable-progress-status "Creating executable for distribution...")
 (assembling-distribution-files-progress-status "Assembling files for distribution...")
 (packing-distribution-progress-status "Packing distribution...")

 (create-servlet "Create Servlet...")

 ; the ~a is a language such as "module" or "algol60"
 (create-servlet-unsupported-language
  "Create Servlet does not work with the ~a language.")
  
 ;;; buttons
 (execute-button-label "Run") 
 (save-button-label "Save")
 (break-button-label "Stop")
 
 ;;; search help desk popup menu
 (search-help-desk-for "Search in Help Desk for \"~a\"")
 (exact-lucky-search-help-desk-for "Exact lucky search in Help Desk for \"~a\"")

 ;; collapse and expand popup menu items
 (collapse-sexp "Collapse S-expression")
 (expand-sexp "Expand S-expression")
 
 ;;; fraction dialog
 (enter-fraction "Enter Fraction")
 (whole-part "Whole Part")
 (numerator "Numerator")
 (denominator "Denominator")
 (invalid-number "Invalid number: must be an exact, real, non-integral number.")
 (insert-fraction-menu-item-label "Insert Fraction...")

 ;; number snip popup menu
 (show-decimal-expansion "View decimal expansion")
 (show-mixed-fraction-view "View as mixed fraction")
 (show-improper-fraction-view "View as improper fraction")
 (show-more-decimal-places "Show more decimal places")
 
 ;;; Teachpack messages
 (select-a-teachpack "Select a Teachpack")
 (clear-teachpack "Clear ~a Teachpack")
 (teachpack-error-label "DrScheme - Teachpack error")
 (teachpack-didnt-load "The teachpack file ~a did not load properly.")
 (add-teachpack-menu-item-label "Add Teachpack...")
 (clear-all-teachpacks-menu-item-label "Clear All Teachpacks")
 (drscheme-teachpack-message-title "DrScheme Teachpack")
 (already-added-teachpack "Already added ~a teachpack")
  
  ; ~a is filled with the teachpack's name; the message appears in the teachpack selection dialog when a user installs a new teachpack
  (compiling-teachpack "Compiling ~a teachpack ...")
  (teachpack-pre-installed "Preinstalled Teachpacks")
  (teachpack-user-installed "User-installed Teachpacks")
  (add-teachpack-to-list... "Add Teachpack to List...")
  (teachpack-already-installed "A teachpack with the name '~a' has already been installed. Overwrite it?")
  ; ~a is filled with a list of language names. Each name is separated by a newline and is indented two spaces (no commas, no 'and')
  (teachpacks-only-in-languages "Teachpacks are only available in these languages: ~a")
  
  
 ;;; Language dialog
 (introduction-to-language-dialog
  "Please select a language. Students in most introductory courses should use the default language.")
 (language-dialog-title "Choose Language")
 (case-sensitive-label "Case sensitive")
 (output-style-label "Output Style")
 (constructor-printing-style "Constructor")
 (quasiquote-printing-style "Quasiquote")
 (write-printing-style "write")
 (print-printing-style "current-print")
 (sharing-printing-label "Show sharing in values")
 (use-pretty-printer-label "Insert newlines in printed values")
 (input-syntax "Input Syntax")
 (dynamic-properties "Dynamic Properties")
 (output-syntax "Output Syntax")
  (teachpacks "Teachpacks") ;; label in the language dialog for the teaching languages
  (teachpacks-none "<< none >>") ;; shows up under the previous string, when there are no teachpacks
 (no-debugging-or-profiling "No debugging or profiling")
 (debugging "Debugging")
 (debugging-and-profiling "Debugging and profiling")
 (test-coverage "Syntactic test suite coverage")
 (show-details-button-label "Show Details")
 (hide-details-button-label "Hide Details")
 (choose-language-menu-item-label "Choose Language...")
 (revert-to-language-defaults "Revert to Language Defaults")
 (fraction-style "Fraction Style")
 (use-mixed-fractions "Mixed fractions")
 (use-repeating-decimals "Repeating decimals")
 (decimal-notation-for-rationals "Use decimal notation for rationals")
 (enforce-primitives-group-box-label "Initial Bindings")
 (enforce-primitives-check-box-label "Disallow redefinition of initial bindings")

  ; used in the bottom left of the drscheme frame 
  ; used the popup menu from the just above; greyed out and only
  ; visible when some languages are in the history
  (recent-languages "Recent languages:")
  ; shows up in bottom-left programming language menu popup, when no langs are recorded
  (no-recently-chosen-languages "no recently chosen languages") 
  
 ;; startup wizard screen language selection section
 (please-select-a-language "Please select a language")
  
  
 ;;; languages
 (beginning-student "Beginning Student")
 (beginning-one-line-summary "define, cond, structs, constants, and primitives")
 (beginning-student/abbrev "Beginning Student with List Abbreviations")
 (beginning/abbrev-one-line-summary "Beginner, with list style printing in the REPL")
 (intermediate-student "Intermediate Student")
 (intermediate-one-line-summary "Beginner plus lexical scope")
 (intermediate-student/lambda "Intermediate Student with lambda")
 (intermediate/lambda-one-line-summary "Intermediate plus higher-order functions")
 (advanced-student "Advanced Student")
 (advanced-one-line-summary "Intermediate plus lambda and mutation")
 (how-to-design-programs "How to Design Programs") ;; should agree with MIT Press on this one...
 (pretty-big-scheme "Pretty Big (includes MrEd and Advanced Student)")
 (pretty-big-scheme-one-line-summary "Adds syntax and functions from the HtDP languages")
 (r5rs-language-name "R5RS")
 (r5rs-one-line-summary "R5RS, with no frills")
 (expander "Expander")
 (expander-one-line-summary "Expands, rather than evaluates, expressions")
 (legacy-languages "Legacy Languages")
 (teaching-languages "Teaching Languages")
 (experimental-languages "Experimental Languages")
  (initial-language-category "Initial language")
  (no-language-chosen "No language chosen")
 
 (module-language-one-line-summary "Run creates a REPL in the context of the module, including the module's declared language")
  
  ;;; from the `not a language language' used initially in drscheme.
  (must-choose-language "DrScheme cannot process programs until you choose a programming language.")
  
  ; next two appear before and after the name of a text book (which will be in italics)
  (using-a-textbook-before "Using ")
  (using-a-textbook-after "?")
  
  ; next two are before and after a language
  (start-with-before "Start with ")
  (start-with-after "")
  
  (seasoned-plt-schemer? "Seasoned PLT Schemer?")
  (looking-for-standard-scheme? "Looking for standard Scheme?")

  ; the three string constants are concatenated together and the middle
  ; one is hyperlinked to the dialog that suggests various languages
  (get-guidance-before "Either select the “Choose Language...” item in the “Language” menu, or ")
  (get-guidance-during "get guidance")
  (get-guidance-after ".")
    
 ;;; debug language
 (unknown-debug-frame "[unknown]")
 (backtrace-window-title "Backtrace - DrScheme")
 (files-interactions "~a's interactions") ;; filled with a filename
 (current-interactions "interactions")
 (current-definitions "definitions")
 (mzscheme-w/debug "Textual (MzScheme, includes R5RS)")
 (mzscheme-one-line-summary "PLT's implementation Scheme")
 (mred-w/debug "Graphical (MrEd, includes MzScheme)")
 (mred-one-line-summary "Adds GUI support to MzScheme")

 ;; profiling
 (profiling-low-color "Low")
 (profiling-high-color "High")
 (profiling-choose-low-color "Please select a low color")
 (profiling-choose-high-color "Please select a high color")
 (profiling "Profiling")
 (profiling-example-text "(define (whee) (whee))")
 (profiling-color-config "Profiling Color Range") 
 (profiling-scale "Profiling Color Scale")
 (profiling-sqrt "Square root")
 (profiling-linear "Linear")
 (profiling-square "Square")
 (profiling-number "Number of Calls")
 (profiling-time "Cumulative Time")
 (profiling-update "Update Profile")
 (profiling-col-percent-time "% Time")
 (profiling-col-function "Function")
 (profiling-col-time-in-msec "Msec")
 (profiling-col-calls "Calls")
 (profiling-show-profile "Show Profile")
 (profiling-hide-profile "Hide Profile")
 (profiling-unknown-src "<< unknown >>")
 (profiling-no-information-available "There is no profiling information available. Please be sure that profiling is enabled in your language and you have run your program.")
 (profiling-clear? "Changing the definitions window invalidates the profiling information. Continue?")
 
 ;; test coverage
 (test-coverage-clear? "Changing the definitions window invalidates the test coverage information. Continue?")
 (test-coverage-clear-and-do-not-ask-again "Yes, and don't ask again")
 (test-coverage-ask? "Ask about clearing test coverage")
  
 ;; tracing
 (tracing-enable-tracing "Enable tracing")
 (tracing-show-tracing-window "Show Tracing")
 (tracing-hide-tracing-window "Hide Tracing")
 (tracing-tracing-nothing-to-show "No tracing results are available, yet. (Make sure that your language supports tracing and that tracing is enabled.)")

 ;;; repl stuff
 (evaluation-terminated "Evaluation Terminated")
 (evaluation-terminated-explanation
  "The evaluation thread is no longer running, so no evaluation can take place until the next execution.")
  
  ; The next three constants show up in the same dialog as the above evaluation-terminated string
  ; constants.
  ; The first two show up only when the user calls 'exit' (possibly with a status code).
  ; The third shows up when the program runs out of memory.
  (exited-successfully "Exited successfully.")
  (exited-with-error-code "Exited with error code ~a.") ;; ~a is filled in with a number between 1 and 255
  (program-ran-out-of-memory "The program ran out of memory.")
 (last-stack-frame "show the last stack frame")
 (last-stack-frames "show the last ~a stack frames")
 (next-stack-frames "show the next ~a stack frames")
 
 ;;; welcoming message in repl
 (language "Language")
 (custom "custom")
 (teachpack "Teachpack")
 (welcome-to "Welcome to")
 (version "version")
 
 ;;; kill evaluation dialog
 (kill-evaluation? "Do you want to kill the evaluation?")
 (just-break "Just Break")
 (kill "Kill")
 (kill? "Kill?")

 ;;; version checker
 (version:update-menu-item   "Check for Updates...")
 (version:update-check       "Update check") ; dialog title, with the next line
 (version:connecting-server  "Connecting to PLT version server")
 (version:results-title      "PLT Version Check")
 (version:do-periodic-checks "Periodically check for newer PLT Scheme versions")
 (version:take-me-there      "Take Me There") ; ...to the download website
 ;; the next one can appear alone, or followed by a comma and the one after that
 (version:plt-up-to-date     "Your PLT version is up-to-date")
 (version:but-newer-alpha    "but note that there is a newer alpha-release")
 ;; This is used in this context: "PLT Scheme vNNN <<<*>>> http://download..."
 (version:now-available-at   "is now available at")

 ;; insert menu
 (insert-menu "&Insert")
 
 ;; large semi colon letters
 (insert-large-letters... "Insert Large Letters...")
 (large-semicolon-letters "Large Semicolon Letters")
 (text-to-insert "Text to insert")

 (module-browser-filename-format "Full Filename: ~a (~a lines)")
 (module-browser-root-filename "Root Filename: ~a")
 (module-browser-font-size-gauge-label "Font Size")
 (module-browser-progress-label "Module overview progress")
 (module-browser-adding-file "Adding file: ~a...")
 (module-browser-laying-out-graph-label "Laying out graph")
 (module-browser-open-file-format "Open ~a")
 (module-browser "Module Browser") ;; frame title
 (module-browser... "Module Browser...") ;; menu item title
 (module-browser-error-expanding "Error expanding the program:\n\n~a")
 (module-browser-show-lib-paths "Show files loaded by (lib ..) paths")
 (module-browser-progress "Module Browser: ~a") ;; prefix in the status line
 (module-browser-compiling-defns "Module Browser: compiling definitions")
 (module-browser-show-lib-paths/short "Follow lib requires") ;; check box label in show module browser pane in drscheme window.
 (module-browser-show-planet-paths/short "Follow planet requires") ;; check box label in show module browser pane in drscheme window.
 (module-browser-refresh "Refresh") ;; button label in show module browser pane in drscheme window.
 (module-browser-only-in-plt-and-module-langs
  "The module browser is only available for programs in the PLT languages and in the module language (and only for programs that have modules in them).")
 (module-browser-name-length "Name length")
 (module-browser-name-short "Short")
 (module-browser-name-medium "Medium")
 (module-browser-name-long "Long")
 (module-browser-open-all "Open all files shown here")

 (happy-birthday-matthias "Happy Birthday, Matthias!")
 (happy-birthday-matthew "Happy Birthday, Matthew!")
 (happy-birthday-shriram "Happy Birthday, Shriram!")

 (mrflow-using-default-language-title "Default Language Used")
 (mrflow-using-default-language "The language currently used does not have a type table defined for its primitives. Using R5RS Scheme instead.")
 (mrflow-button-title "Analyze")
 ;(mrflow-unknown-style-delta-error-title "Unknown Box Style Delta")
 ;(mrflow-unknown-style-delta-error "Unknown box style delta: ~a")
 (mrflow-popup-menu-show-type "Show Type")
 (mrflow-popup-menu-hide-type "Hide Type")
 (mrflow-popup-menu-show-errors "Show Errors")
 (mrflow-popup-menu-hide-errors "Hide Errors")
 ;(mrflow-read-exception-title "Read Exception")
 ;(mrflow-read-exception "Read exception: ~a")
 ;(mrflow-syntax-exception-title "Syntax Exception")
 ;(mrflow-syntax-exception "Syntax exception: ~a")
 ;(mrflow-unknown-exception-title "Unknown Exception")
 ;(mrflow-unknown-exception "Unknown exception: ~a")
 ;(mrflow-language-primitives-error-title "Language Primitives Error")
 ;(mrflow-language-primitives-error "Wrong filename for language primitives types table: ~a")
  
 (snips-and-arrows-popup-menu-tack-all-arrows "Tack All Arrows")
 (snips-and-arrows-popup-menu-untack-all-arrows "Untack All Arrows")
 (snips-and-arrows-user-action-disallowed-title "User Changes Currently Disallowed")
 (snips-and-arrows-user-action-disallowed "User changes are disallowed in editors that contain tool-inserted snips.  Hide all snips before modifying the content of the editor.")
 ;(snips-and-arrows-changing-terms-warning-title "Changing terms will be undoable")
 ;(snips-and-arrows-changing-terms-warning "Changing terms in an editor containing snips cannot be undone.  You can either cancel this action, remove the snips, and try the change again, or you can continue with the change, in which case the change will not be undoable (all others changes made before and afterward will still be undoable though).")
 (snips-and-arrows-hide-all-snips-in-editor "Hide all snips in editor")

 (xml-tool-insert-xml-box "Insert XML Box")
 (xml-tool-insert-scheme-box "Insert Scheme Box")
 (xml-tool-insert-scheme-splice-box "Insert Scheme Splice Box")
 (xml-tool-xml-box "XML Box")
 (xml-tool-scheme-box "Scheme Box")
 (xml-tool-scheme-splice-box "Scheme Splice Box")
 (xml-tool-switch-to-scheme "Switch to Scheme box")
 (xml-tool-switch-to-scheme-splice "Switch to Scheme splice box")
 (xml-tool-eliminate-whitespace-in-empty-tags
  "Eliminiate whitespace in empty tags")
 (xml-tool-leave-whitespace-alone
  "Leave whitespace alone")
 
 (show-recent-items-window-menu-item "Show Recently Opened Files in Separate Window")
 (show-recent-items-window-label "Recently Opened Files")
 (number-of-open-recent-items "Number of recent items")
 (switch-anyway "Switch File Anyway")

 (stepper-program-has-changed "WARNING: The program has changed.")
 (stepper-program-window-closed "WARNING: The program window is gone.")

 (stepper-name "Stepper")
 (stepper-language-level-message "The stepper does not work for language \"~a\".")
 (stepper-button-label "Step")
 (stepper-home "Home")
 (stepper-previous-application "|< Application")
 (stepper-previous "< Step")
 (stepper-next "Step >")
 (stepper-next-application "Application >|")
 (stepper-jump-to-end "End")
 
 (debug-tool-button-name "Debug")

 (dialog-back "Back")

 ;; warnings about closing a drscheme frame when the program
 ;; might still be doing something interesting
 (program-is-still-running "The program in the definitions window is still running. Close anyway?")
  (program-has-open-windows "The program in the definitions window has open windows. Close this window anyway?")
 
  ;; ml-command-line-arguments is for the command line arguments
  ;; label in the module language details in the language dialog.
  (ml-command-line-arguments "Command-line arguments as a vector of strings, in read syntax")

  ;; ml-cp names are all for the module language collection path
  ;; configuration. See the details portion of the language dialog
  ;; for the module language (at the bottom).
  (ml-cp-default-collection-path "<<default collection paths>>")

  ;; in std get-directory 
  (ml-cp-choose-a-collection-path "Please choose a collection path")

  ;; err msg when adding default twice
  (ml-cp-default-already-present
   "Default collection paths are already present")
  
  ;; title of this section of the dialog (possibly the word
  ;; `Collection' should not be translated)
  (ml-cp-collection-paths "Collection Paths")

  ;; button labels
  (ml-cp-add "Add")
  (ml-cp-add-default "Add Default")
  (ml-cp-remove "Remove")
  (ml-cp-raise "Raise")
  (ml-cp-lower "Lower")

  ;; Profj
  (profj-java "Java")
  (profj-java-mode "Java mode")

  (profj-beginner-lang "Beginner")
  (profj-beginner-lang-one-line-summary "Java-like Beginner teaching language")
  (profj-full-lang "Full")
  (profj-full-lang-one-line-summary "Like Java 1.0 (some 1.1)")
  (profj-advanced-lang "Advanced")
  (profj-advanced-lang-one-line-summary "Java-like Advanced teaching language")
  (profj-intermediate-lang "Intermediate")
  (profj-intermediate-lang-one-line-summary "Java-like Intermediate teaching language")
  (profj-intermediate-access-lang "Intermediate + access")
  (profj-intermediate-access-lang-one-line-summary "Java-like Intermediate teaching language, with access modifiers")
  (profj-dynamic-lang "Java+dynamic")
  (profj-dynamic-lang-one-summary "Java with dynamic typing capabilities")

  (profj-java-mode-color-heading "Edit Colors") ; Heading for preference to choose editing colors  
  (profj-java-mode-color-keyword "keyword")
  (profj-java-mode-color-string "string")
  (profj-java-mode-color-literal "literal")
  (profj-java-mode-color-comment "comment")
  (profj-java-mode-color-error "error")
  (profj-java-mode-color-identifier "identifier")
  (profj-java-mode-color-prim-type "primitive type") ; Example text for built-in Java types
  (profj-java-mode-color-default "default")

  (profj-coverage-color-heading "Coverage Colors") ; Heading for preference to choose coverage colors
  (profj-coverage-color-covered "covered expression") 
  
  (profj-language-config-display-preferences "Display Preferences") ; Heading for preferences controlling printing
  (profj-language-config-display-style "Display style")
  (profj-language-config-display-field "Class + Fields")
  (profj-language-config-class "Class")
  (profj-language-config-display-array "Print entire contents of arrays?")
  (profj-language-config-testing-preferences "Testing Preferences") ; Heading for preferences controlling test behavior
  ;(profj-language-config-testing-enable "Display testing results on Run?") ; Run should be the word found on the Run button
  (profj-language-config-testing-coverage "Collect coverage information for tests?")
  (profj-language-config-support-test-language "Support test Language extension?")
  (profj-language-config-testing-check "Allow check expression?") ; check should not be translated
  (profj-language-config-classpath "Classpath")
  (profj-language-config-choose-classpath-directory "Choose the directory to add to class path")
  (profj-language-config-classpath-display "Show current") ; Button label to print the current classpath

  (profj-test-name-close-to-example "Class ~a's name contains a phrase close to Example.")
  (profj-test-name-example-miscapitalized "Class ~a's name contains a miscapitalized example.")
  
   ;; Close testing window and do not run test cases any more
  ;(profj-test-results-close-and-disable "Close and Disable Testing")
  ;; Hide docked testing window and do not run test cases any more
  ;(profj-test-results-hide-and-disable "Hide and Disable Testing")
  ;Renamed below
  ;(profj-test-results-window-title "Test Results")
  
  (profj-unsupported "Unsupported")
  (profj-executables-unsupported "Sorry - executables are not supported for Java at this time")

  (profj-convert-to-text-comment "Convert to text comment")
  (profj-convert-to-comment "Convert to comment")

  (profj-executing-main "executing main")

  (profj-insert-java-comment-box "Insert Java Comment Box")
  (profj-insert-java-interactions-box "Insert Java Interactions Box")

  ;;The Test engine tool
  ;;
  (test-engine-window-title "Test Results")
  ;;Following two appear in View menu, attach and free test report window from DrScheme frame
  (test-engine-dock-report "Dock Test Report")
  (test-engine-undock-report "Undock Test Report")
  ;;Following two appear in Scheme (Java, etc) menu, cause Tests to be Run automatically or not
  (test-engine-enable-tests "Enable Tests")
  (test-engine-disable-tests "Disable Tests")
  
  (profjWizward-insert-java-class "Insert Java Class")
  (profjWizard-insert-java-union "Insert Java Union")
  
  ;; The Test Suite Tool
  ;; Errors
  (test-case-empty-error "Empty test case")
  (test-case-too-many-expressions-error "Too many expressions in a test case.")
  ;; Dr. Scheme window menu items
  (test-case-insert "Insert Test Case")
  (test-case-disable-all "Disable all Test Cases")
  (test-case-enable-all "Enable all Test Cases")
  
  ;; NOTE: The following string constants are labels of the test-case fields. The width
  ;;       of the field is determined by the length of the longest of the following three words.
  ;;       if the words are too long the test case will take up too much horizontal room and
  ;;       not look very good.
  ;; This string is the label of the expression that is being tested in a test case.
  (test-case-to-test "Test")
  ;; This string is the label of the expression that is the expected value of the to-test expression.
  (test-case-expected "Should be")
  ;; This string is the label of the actual result of the to test expression.
  (test-case-actual "Actual")
  (test-case-predicate "Predicate")
  (test-case-should-raise "Should Raise")
  ;; The label of a field of the test-case that describes the expected error message of a test case
  (test-case-error-message "Error Message")

  (test-case-menu-title "Test Case")
  (test-case-switch-to-error-box "Switch to Error Test Box")
  (test-case-switch-to-nonerror-box "Switch to Nonerror Test box")
  (test-case-collapse "Collapse Test Case")
  (test-case-show-actual "Show Actual Value")
  (test-case-enable "Enable Test Case")
  (test-case-show-predicate "Show Predicate")
  (test-case-show-error-message "Show Error Message")
  (test-case-convert-to-text "Convert to text")
  
  ;; Profj Boxes
  (profjBoxes-empty-error "Empty interaction")
  (profjBoxes-too-many-expressions-error "Too many expressions in a box")
  (profjBoxes-interactions-label "Interactions")
  (profjBoxes-bad-java-id-error "Malformed Java ID")
  (profjBoxes-examples-label "Examples")
  (profjBoxes-add-new-example-button "Add New Example")
  (profjBoxes-type "Type")
  ;; The Java identifier of an example of data
  (profjBoxes-name "Name")
  (profjBoxes-value "Value")
  (profjBoxes-insert-java-examples "Insert Java Examples")
  (profjBoxes-insert-java-interactions "Insert Java Interactions")

  ;; Slideshow
  (slideshow-hide-picts "Show Nested Boxes")
  (slideshow-show-picts "Show Picts")
  (slideshow-cannot-show-picts "Cannot show picts; run program to cache sizes first")
  (slideshow-insert-pict-box "Insert Pict Box") 

  ;; GUI Tool
  (gui-tool-heading "GUI Tool")
  (gui-tool-before-clicking-message "Before clicking a tool icon, use \"Insert GUI\" from the \"Special\" menu to insert a root GUI item, or select an already inserted GUI.")
  (gui-tool-show-gui-toolbar "Show GUI Toolbar")
  (gui-tool-hide-gui-toolbar "Hide GUI Toolbar")
  (gui-tool-insert-gui "Insert GUI")
  )
