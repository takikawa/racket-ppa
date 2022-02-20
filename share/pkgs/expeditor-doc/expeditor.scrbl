#lang scribble/manual
@(require racket/list
          (for-syntax racket/base)
          (for-label racket/contract/base
                     racket/base
                     expeditor
                     (submod expeditor configure)
                     racket/file))

@(define (onekey s) (regexp-replace* #rx"\\^" (regexp-replace #rx"Esc-" s "Meta-") "Ctl-"))
@(define (binding-table . keys) (apply itemlist  keys))
@(define (key* keys proc . content)
   (item (elemtag
          `("key" ,(if (string? keys) keys (car keys)))
          (if (string? keys)
              (onekey keys)
              (add-between (map onekey keys) " or ")))
         " --- "
         content
         " (Implementation: " proc ")"))
@(define-syntax-rule (key keys proc . content) (key* 'keys @racket[proc] . content))

@(define (subsection* s) (subsection #:style '(unnumbered) s))

@(define (see-dr tag)
   @list{See @seclink[tag #:doc '(lib "scribblings/tools/tools.scrbl") #:indirect? #t]{the DrRacket manual}
         for more information.})

@title{Expeditor: Terminal Expression Editor}

@defmodule[expeditor]

The expeditor (the @bold{exp}ression @bold{editor}) supports
multi-line editing with indentation and completion support within a
terminal. It's based on Chez Scheme's expression editor, but adapts to
Racket languages using the same hooks and APIs as DrRacket. Normally,
the expeditor is run automatically by @racketmodname[xrepl
#:indirect], which is turn loaded by the @exec{racket} command-line
executable.

For customization of keybindings, see @secref["customization"]. To
disable or enable color in expeditor, you can use xrepl's
@litchar{,color} command.

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "key-bindings"]{Default Key Bindings}

In the keybinding descriptions below, a ``Meta-'' combination usually
means ``Alt-'' or ``Option-'', depending on your keyboard. It also can
be typed as the Esc key (pressed then released) followed by the rest
of the combination; a terminal will typically report a combination
using the Alt or Option key as that Esc sequence. In a ``Ctl-''
combination, the letter case of the key doesn't matter (i.e., doesn't
require holding the Shift key).

@subsection*{Evaluation, Indentation, and Completion}

@binding-table[

  @key[("Return" "^J") ee-newline/accept]{Reads and evaluates the
       current entry, if it is complete, and otherwise inserts a
       newline and auto-indents. The notion of ``complete'' depends on
       a language, but typically includes requirements like no
       unbalanced parentheses.}

  @key[("Esc-Return") ee-newline]{Inserts a newline and indents.}
       
  @key[("Esc-^J") ee-accept]{Reads and evaluates the current entry,
       even if it is not otherwise recognized as complete.}

  @key["^O" ee-open-line]{Creates a new line for input, similar to a
       non-accepting @onekey{Return}, but does not move the cursor to
       the new line or indent.}

  @key["Tab" ee-id-completion/indent]{Either indents or completes,
       depending on the text before the cursor. If no text is present
       before the cursor on the same line, then the line is indented
       or cycled to the next possible indentation. If the cursor is
       after an identifier, it's completed or a list of possible
       completions is shown. Completion depends on the language, but
       it is typically drawn from the set of available top-level
       bindings.}

  @key["^R" ee-next-id-completion]{Steps through the next possible
       completion when there are multiple possible completions.}

  @key["Esc-Tab" ee-indent]{Indents the current line or cycles though
       possible indentations. The cursor is moved to just after the
       indentation before the rest of the line content.}

  @key[("Esc-q" "Esc-Q" "Esc-^Q") ee-indent-all]{Reindents the full editor
       region.}

]

@subsection*{Navigation}

@binding-table[

  @key[("Left" "^B") ee-backward-char]{Moves the cursor back
       one character.}
       
  @key[("Right" "^F") ee-forward-char]{Moves the cursor forward
       one character.}
       
  @key[("Up" "^P") ee-previous-line]{Moves the cursor up to the
       previous line---unless the cursor is at the start of the
       editor-region, in which case replaces the editor region with the
       previous history entry.}

  @key[("Down" "^N") ee-next-line]{Moves the cursor down to the next
       line---unless the cursor is at the end of the editor region, in
       which case replaces the editor region with the next history
       entry.}
       
  @key[("Home" "^A") ee-beginning-of-line]{Moves the cursor to the
       start of the current line.}
       
  @key[("End" "^E") ee-end-of-line]{Moves the cursor to the
       end of the current line.}

  @key[("PageUp" "^X [") ee-backward-page]{Moves the cursor up to the
       previous page.}
       
  @key[("PageDown" "^X ]") ee-backward-page]{Moves the cursor down to the
       next page.}

  @key["Esc-<" ee-beginning-of-entry]{Moves the cursor to the
       start of the editor region.}
       
  @key["Esc->" ee-end-of-entry]{Moves the cursor to the
       end of the editor region.}

  @key[("^Right" "Esc-f" "Esc-F") ee-forward-word]{Moves the cursor
       forward one whitespace-delimited word.}
       
  @key[("^Left" "Esc-b" "Esc-B") ee-backward-word]{Moves the cursor
       backward one whitespace-delimited word.}

  @key["Esc-]" ee-goto-matching-delimiter]{Moves the cursor to the
       opener or closer opposite the one under the cursor.}
       
  @key["^]" ee-flash-matching-delimiter]{Flashes the cursor on the
       opener or closer opposite the one under the cursor.}

  @key[("Esc-^Right" "Esc-^F") ee-forward-exp]{Moves the cursor forward
       one expression, where the definition of ``expression'' is
       language-specific.}
       
  @key[("Esc-^Left" "Esc-^B") ee-backward-exp]{Moves the cursor backward
       one language-specific expression.}

  @key[("Esc-^U") ee-upward-exp]{Moves the cursor upward/outward
       one language-specific expression.}
       
  @key[("Esc-^D") ee-downward-exp]{Moves the cursor downward/inward
       one language-specific expression.}

  @key["^X ^X" ee-exchange-point-and-mark]{Moves the cursor to the
       location of the @tech{mark} while setting the @tech{mark} to
       the cursor's current position.}

]

@subsection*{History}

@binding-table[

  @key[("Esc-Up" "Esc-^P") ee-history-bwd]{Replaces the editor region with the
       previous history entry.}

  @key[("Esc-Down" "Esc-^N") ee-history-fwd]{Replaces the editor region with the
       next history entry.}
  
  @key[("Esc-p") ee-history-bwd-prefix]{Replaces the editor region
       with the previous history entry that starts the same as the current
       editor content.}

  @key[("Esc-P") ee-history-bwd-contains]{Replaces the editor region
       with the previous history entry that includes the same as the
       current editor content.}
       
  @key[("Esc-n") ee-history-fwd-prefix]{Replaces the editor region
       with the next history entry that starts the same as the current
       editor content.}

  @key[("Esc-N") ee-history-fwd-contains]{Replaces the editor region
       with the next history entry that includes the same as the
       current editor content.}

]

@subsection*{Deletion, Insertion, and Transposition}

@binding-table[

  @key[("Backspace" "^H") ee-backward-delete-char]{Deletes the previous
       character, if any.}

  @key["^D" ee-eof/delete-char]{Deletes the next character, if
       any---unless the editor region is empty, in which case returns
       an end-of-file as the input.}

  @key["Delete" ee-delete-char]{Deletes the next character, if
       any.}

  @key["^U" ee-delete-line]{Deletes the content of the current line,
       no matter where the cursor is within the line.}
  
  @key[("^K" "Esc-k") ee-delete-to-eol]{Deletes the content of the
       current line following the cursor, or merges the next line with
       the current one if the cursor is at the end of the line.}
  
  @key[("^G") ee-delete-entry]{Deletes the full content of the editor
       region.}
  
  @key[("^C") ee-reset-entry/break]{Deletes the full content of the editor
       region, and also moves to the end of the history---unless the editor
       region is empty, in which case sends a break signal to the current
       thread.}

  @key[("Esc-d") ee-delete-word]{Deletes one whitespace-delimited word
       after the cursor.}

  @key[("Esc-Delete" "Esc-^K") ee-delete-exp]{Deletes one expression
       after the cursor, where the definition of ``expression'' is
       language-specific.}

  @key[("Esc-Backspace" "Esc-^H") ee-backward-delete-exp]{Deletes one
       expression before the cursor.}

  @key[("^@" "^^") ee-set-mark]{Set the @tech{mark} to be the same
       position as the cursor. The @deftech{mark} is a kind of second
       cursor, but invisible, that is used by various editing
       operations.}

  @key["^W" ee-delete-between-point-and-mark-or-backward]{Deletes content between
       the cursor and the @tech{mark}. When no mark is set, deletes
       one expression before the cursor.}

  @key["^Y" ee-yank-kill-buffer]{Inserts content previously deleted,
       where multiple consecutive deletions accumulate to one set of
       content to insert.}

  @key["^V" ee-yank-selection]{Inserts the content of the system
       clipboard.}

  @key["^T" ee-transpose-char]{Transposes characters to left and right
       of the cursor---unless the cursor is at the end of a line, in
       which case transposes the previous two characters.}

  @key["Esc-t" ee-transpose-word]{Transposes space-delimited words to the
       left and right of the cursor.}

  @key["Esc-^T" ee-transpose-exp]{Transposes language-specific
       expressions to the left and right of the cursor.}

]

@subsection*{Process Control}

@binding-table[

  @key["^L" ee-redisplay]{Refreshes the editor region's display.}

  @key["^Z" ee-suspend-process]{Suspends the current process.}

]

@; ----------------------------------------

@section[#:tag "customization"]{Customizing Expeditor}

@(define-syntax (keyproc stx)
   (syntax-case stx ()
     [(_ id body ...)
      (with-syntax ([ee (datum->syntax #'id 'ee)]
                    [entry (datum->syntax #'id 'entry)]
                    [c (datum->syntax #'id 'c)])
        #`@defproc[(id [ee eestate?]
                       [entry entry?]
                       [c char?])
                   entry?
                   body ...])]))

@defmodule[(submod expeditor configure)]

When @racket[expeditor-configure] is called---such as when xrepl
initializes the expeditor, which is the default behavior when running
@exec{racket} at the command line---it dynamically requires the module
file reported by @racket[(expeditor-init-file-path)], if that file
exists. The module file can import @racketmodname[(submod expeditor
configure)] to configure key bindings and colors.

For example, the following module as
@racket[(expeditor-init-file-path)] changes the behavior of
@onekey["^J"] and changes the color of literals from green to magenta:

@racketblock[
@#,hash-lang[] racket/base
(require (submod expeditor configure))

(expeditor-bind-key! "^J" ee-newline)
(expeditor-set-syntax-color! 'literal 'magenta)
]

@; ----------------------------------------

@subsection{Key-Handling Functions}

A key-handling function accepts three values: a representation of the
terminal state, a representation of the current editor region, and a
character. The result is a representation of the editor region
(usually the one passed in) or @racket[#f] to indicate that the
current editor region should be accepted as input.

@(define (see-key key)
   @elem{Implements the behavior described for @elemref[`("key" ,key)]{@onekey[key]}.})

@defproc[(expeditor-bind-key! [key string?]
                              [handler (eestate? entry? char . -> . (or/c #f entry?))])
         void?]{

Binds the action of @racket[key] to @racket[handler], where
@racket[handler] is typically one of the @racketidfont{ee-} functions
described below.

The @racket[key] string encodes either a single key or a sequence of
keys:

@itemlist[

 @item{The sequence @litchar{\e} (so, in a literal string as
       @racket["\\e"]) is treated as Escape, which at the start of a
       sequence is normally the way terminals report ``Meta-'' key
       combinations.}

 @item{A @litchar{^} prefix on a character implies a ``Ctl-''
       combination, like @racket["^a"] for Ctl-A.}

 @item{The sequence @litchar{\\} is a backslash (so, in a literal
       string as @racket["\\\\"]).}

 @item{The sequence @litchar{\^} is the character character.}

 @item{Anything else stands for itself.}

]

As examples, here are a few bindings from the default set:

@racketblock[
(expeditor-bind-key! "^B" ee-backward-char) (code:comment "Ctl-B")
(expeditor-bind-key! "\\ef" ee-forward-word) (code:comment "Esc-f")
(expeditor-bind-key! "\\e[C" ee-forward-char) (code:comment "Right")
(expeditor-bind-key! "\\e[1;5C" ee-forward-word) (code:comment "Ctl-Right")
]

The Right and Ctl-Right bindings are derived from a typical sequence
that a terminal generates for those key combinations. In your
terminal, the @exec{od} program may be helpful in figuring how key
presses turn into program input.}


@keyproc[ee-insert-self/paren]{

This function is the default operation for unmapped keys.

Like @racket[ee-insert-self], but if @racket[c] is a ``parenthesis''
character, flashes its match like
@racket[ee-flash-matching-delimiter]. Furthermore, if @racket[c] is a
closing ``parenthesis'', it may be corrected automatically based on
its apparently intended match.}

@keyproc[ee-insert-self]{

Inserts @racket[c], as long as it is not a control character.}

@defproc[((make-ee-insert-string [s string?])
          [ee eestate?]
          [entry entry?]
          [c char?])
         entry?]{

Creates a key-handling function that inserts @racket[s].}

@defproc[(ee-newline/accept [ee eestate?]
                            [entry entry?]
                            [c char?])
         (or/c entry? #f)]{

@see-key["Return"] Note that the return value is @racket[#f] in the
case that the input should be accepted.}

@defproc[(ee-accept [ee eestate?]
                    [entry entry?]
                    [c char?])
         (or/c #f entry?)]{

@see-key["Esc-^J"] Note that the return value is @racket[#f] in the
case that the input should be accepted.}

@keyproc[ee-newline]{@see-key["Esc-Return"]}

@keyproc[ee-open-line]{@see-key["^O"]}

@keyproc[ee-indent]{@see-key["Esc-Tab"]}
@keyproc[ee-indent-all]{@see-key["Esc-q"]}

@keyproc[ee-id-completion/indent]{@see-key["Tab"]}

@keyproc[ee-id-completion]{

Like @racket[ee-id-completion], but always attempts completion instead
of tabbing.}

@keyproc[ee-next-id-completion]{@see-key["^R"]}

@keyproc[ee-backward-char]{@see-key["Left"]}
@keyproc[ee-forward-char]{@see-key["Right"]}
@keyproc[ee-next-line]{@see-key["Down"]}
@keyproc[ee-previous-line]{@see-key["Up"]}
@keyproc[ee-forward-word]{@see-key["^Right"]}
@keyproc[ee-forward-exp]{@see-key["Esc-^Right"]}
@keyproc[ee-backward-word]{@see-key["^Left"]}
@keyproc[ee-backward-exp]{@see-key["Esc-^Left"]}
@keyproc[ee-upward-exp]{@see-key["Esc-^U"]}
@keyproc[ee-downward-exp]{@see-key["Esc-^D"]}
@keyproc[ee-backward-page]{@see-key["PageUp"]}
@keyproc[ee-forward-page]{@see-key["PageDown"]}

@keyproc[ee-beginning-of-line]{@see-key["Home"]}
@keyproc[ee-end-of-line]{@see-key["End"]}
@keyproc[ee-beginning-of-entry]{@see-key["Esc-<"]}
@keyproc[ee-end-of-entry]{@see-key["Esc->"]}
           
@keyproc[ee-goto-matching-delimiter]{@see-key["Esc-]"]}
@keyproc[ee-flash-matching-delimiter]{@see-key["^]"]}

@keyproc[ee-transpose-char]{@see-key["^T"]}
@keyproc[ee-transpose-word]{@see-key["Esc-t"]}
@keyproc[ee-transpose-exp]{@see-key["Esc-^T"]}

@keyproc[ee-set-mark]{@see-key["^@"]}
@keyproc[ee-exchange-point-and-mark]{@see-key["^X ^X"]}

@keyproc[ee-delete-char]{@see-key["Delete"]}
@keyproc[ee-backward-delete-char]{@see-key["Backspace"]}
@keyproc[ee-delete-line]{@see-key["^U"]}
@keyproc[ee-delete-to-eol]{@see-key["^K"]}
@keyproc[ee-delete-between-point-and-mark-or-backward]{@see-key["^W"]}
@keyproc[ee-delete-entry]{@see-key["^G"]}

@keyproc[ee-reset-entry/break]{@see-key["^C"]}
@keyproc[ee-reset-entry]{Like @racket[ee-reset-entry/break], but never sends a break signal.}

@keyproc[ee-delete-word]{@see-key["Esc-d"]}
@keyproc[ee-delete-exp]{@see-key["Esc-Delete"]}
@keyproc[ee-backward-delete-exp]{@see-key["Esc-Backspace"]}
@keyproc[ee-yank-selection]{@see-key["^V"]}
@keyproc[ee-yank-kill-buffer]{@see-key["^Y"]}

@keyproc[ee-eof/delete-char]{@see-key["^D"]}
@keyproc[ee-eof]{Like @racket[ee-eof/delete-char], but always return an end-of-file.}

@keyproc[ee-redisplay]{@see-key["^L"]}

@keyproc[ee-history-bwd]{@see-key["Esc-Up"] See also @racket[current-ee-backward-history-point].}
@keyproc[ee-history-fwd]{@see-key["Esc-Down"] See also @racket[current-ee-forward-history-point].}
@keyproc[ee-history-bwd-prefix]{@see-key["Esc-p"] See also @racket[current-ee-backward-history-point].}
@keyproc[ee-history-bwd-contains]{@see-key["Esc-P"] See also @racket[current-ee-backward-history-point].}
@keyproc[ee-history-fwd-prefix]{@see-key["Esc-n"] See also @racket[current-ee-forward-history-point].}
@keyproc[ee-history-fwd-contains]{@see-key["Esc-N"] See also @racket[current-ee-forward-history-point].}

@keyproc[ee-command-repeat]{

Accumulates @racket[c] into a repeat count if it is a digit.
Otherwise, performs the command associated with @racket[c] the number
of times set up for repeating.}

@keyproc[ee-suspend-process]{@see-key["^Z"]}


@defproc[(eestate? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a representation of the terminal
state, @racket[#f] otherwise.}

@defproc[(entry? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a representation of the current
editor region, @racket[#f] otherwise.}

@; ----------------------------------------

@subsection{Colors}

@defproc[(expeditor-set-syntax-color! [category (or/c 'error
                                                      'paren
                                                      'literal
                                                      'identifier
                                                      'comment)]
                               [color (or/c 'default
                                            'black
                                            'white
                                            'red
                                            'green
                                            'blue
                                            'yellow
                                            'cyan
                                            'magenta
                                            'dark-gray
                                            'light-gray
                                            'light-red
                                            'light-green
                                            'light-blue
                                            'light-yellow
                                            'light-cyan
                                            'light-magenta)])
            void?]{

Sets the color used for a syntactic category when coloring is enabled.
The @racket['error] color is used by @racket[expeditor-error-display]
in addition to being used for invalid tokens.}

@; ----------------------------------------

@subsection{History Navigation}

@defparam[current-ee-backward-history-point start-at (or/c 'start 'top 'bottom 'end)]{

A parameter that determines where the cursor starts when the editor
content is changed to an earlier entry in the history via
@racket[ee-history-bwd] and similar functions:

@itemlist[

 @item{@racket['start] --- at the start of the entry}

 @item{@racket['top] --- at the end of the first line of the entry}

 @item{@racket['bottom] or @racket['end] --- at the end of the last line of the entry}

]

The default is @racket['top].}


@defparam[current-ee-forward-history-point start-at (or/c 'start 'top 'bottom 'end)]{

Like @racket[current-ee-backward-history-point], but used when the
editor content is changed to a later entry in the history via
@racket[ee-history-fwd] and similar functions.

The default is @racket['bottom].}

@; ----------------------------------------

@section{Expeditor API}

@defproc[(expeditor-open [history (listof string?)]) (or/c eestate? #f)]{

Attempts to start the expeditor. On success, which requires that
@racket[(current-input-port)] and @racket[(current-output-port)] are
terminal ports and the terminal configuration is recognized, the
result is a representation of the terminal state. The result is
@racket[#f] if the expeditor cannot be initialized.

The @racket[history] argument provides the }

@defproc[(expeditor-close [ee estate?]) (listof string?)]{

Closes the expeditor, relinquishing terminal configuration and
resources, if any. The result is the expeditor's history as
initialized by @racket[expeditor-open] and updated by
@racket[expeditor-read] calls.}

@defproc[(expeditor-read [ee estate?]) any/c]{

Reads input from the terminal. The @racket[ee] argument holds terminal
state as well as history that is updated during
@racket[expeditor-read].}

@defproc[(call-with-expeditor [proc ((-> any/c) -> any)]) any]{

Combines @racket[expeditor-open], a call to @racket[proc], and
@racket[expeditor-close], where the reading procedure passed to
@racket[proc] can be called any number of times to read input.

Expeditor history is initialized from
@racket[current-expeditor-history] on open, and the value of
@racket[current-expeditor-history] is updated with the new history on
close.}


@defproc[(expeditor-configure) void?]{

Sets expeditor parameters based on @racket[current-interaction-info],
the user's preferences file, and @racket[(expeditor-init-file-path)].

The @racket[current-expeditor-reader] parameter is first set to use
@racket[current-read-interaction].

then, @racket[expeditor-configure] checks for information via
@racket[current-interaction-info], currently checking for the
following keys:

@itemlist[

 @item{@racket['color-lexer] --- Sets
       @racket[current-expeditor-lexer]. If @racket['color-lexer] is
       not provided and @racketmodname[syntax-color/racket-lexer
       #:indirect] is available, then the Racket lexer is installed.}

 @item{@racket['drracket:submit-predicate] --- Sets
       @racket[current-expeditor-ready-checker].}

 @item{@racket['drracket:paren-matches] --- Sets
       @racket[current-expeditor-parentheses].}

 @item{@racket['drracket:grouping-position] --- Sets
       @racket[current-expeditor-grouper].}

 @item{@racket['drracket:indentation] and
       @racket['drracket:range-indentation] --- Sets
       @racket[current-expeditor-indenter] based on a combination of
       both values.}

]

The @racket['expeditor-color-enabled] preference (via
@racket[get-preference]) determines
@racket[current-expeditor-color-enabled].

Finally, if the file named by @racket[(expeditor-init-file-path)],
it is @racket[dynamic-require]d.}

@defproc[(expeditor-init-file-path) path?]{

Returns a path that is used by @racket[expeditor-configure].

If @racket[(find-system-path 'init-dir)] produces a different result
than @racket[(find-system-path 'home-dir)], then the result is
@racket[(build-path (find-system-path 'init-dir) "expeditor.rkt")].
Otherwise, the result is @racket[(build-path (find-system-path
'home-dir) ".expeditor.rkt")].}


@defparam[current-expeditor-reader proc (input-port? . -> . any/c)]{

A parameter that determines the reader used to parse input when an
entry is accepted. The default function uses @racket[read].}


@defparam[current-expeditor-post-skipper proc (input-port? . -> . any/c)]{

A parameter that determines a function used to consume extra
whitespace after a reader consumes from an accepted entry. The default
function consumes whitespace.}


@defparam[current-expeditor-lexer proc procedure?]{

A parameter that determines the lexer used for syntax coloring and
parenthesis matching. @see-dr["Syntax_Coloring"] The default function
simply recognizes common parenthesis-like characters.}


@defparam[current-expeditor-ready-checker proc procedure?]{

A parameter that determines how expeditor entry is treated as ready to
accept or not. @see-dr["REPL_Submit_Predicate"] The default function
attempts to @racket[read] all input, returning @racket[#f] only if
@racket[exn:fail:read:eof] is raised.}

@defparam[current-expeditor-parentheses pairs (listof (list/c symbol? symbol?))]{

A parameter that determines character sequences that are considered
matching opener and closer pairs. @see-dr["Indentation"]
The default is @racket['((|(| |)|) (|[| |]|) (|{| |}|))].}

@defparam[current-expeditor-grouper proc procedure?]{

A parameter that determines how expression-based navigation operators
work. @see-dr["Keystrokes"]}

@defparam[current-expeditor-indenter proc procedure?]{

A parameter that determines how automatic indentation works.
@see-dr["Indentation"].}

@defparam[current-expeditor-color-enabled on? boolean?]{

A parameter that determines whether syntax and error coloring are
enabled.}

@defparam[current-expeditor-history strs (listof string?)]{

Expeditor history as consumed and produced by
@racket[call-with-expeditor].}

@defboolparam[current-expeditor-history-whitespace-trim-enabled on?]{

A parameter that determines whether tailing whitespace is trimmed from
input before recording it as history. The default is @racket[#t].}


@defproc[(expeditor-error-display [s string?]) void?]{

Similar to @racket[display] of @racket[s], but when color is enabled,
the string is printed in the error color.}
