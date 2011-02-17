#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Color}

@definterface[color:text<%> (text:basic<%>)]{
  This interface describes how coloring is stopped and started for text
  that knows how to color itself.  It also describes how to query the
  lexical and s-expression structure of the text.
  @defmethod*[(((start-colorer (token-sym->style (-> symbol? string?)) 
                               (get-token (or/c (-> input-port? 
                                                    (values any/c 
                                                            symbol? 
                                                            (or/c false? symbol?) 
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?))
                                                (-> input-port? 
                                                    exact-nonnegative-integer?
                                                    any/c
                                                    (values any/c 
                                                            symbol? 
                                                            (or/c false? symbol?) 
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?
                                                            any/c))))
                               (pairs (listof (list/p symbol? symbol?)))) void))]{
    Starts tokenizing the buffer for coloring and parenthesis matching.

    The @scheme[token-sym->style] argument will be passed the first return symbol from @scheme[get-token],
    and it should return the style-name that the token should be colored.

    The @scheme[get-token] argument takes an input port and optionally an offset and mode value.
    When it accepts just an input port, @scheme[get-token] returns the next token as 5 values:

    @itemize[
    @item{
    An unused value.  This value is intended to represent the textual
    component of the token and may be used as such in the future.}
    @item{
    A symbol describing the type of the token.  This symbol is transformed
    into a style-name via the @scheme[token-sym->style] argument.  The symbols
    @scheme['white-space] and @scheme['comment] have special meaning and should always be
    returned for white space and comment tokens respectively.  The symbol
    @scheme['no-color] can be used to indicate that although the token is not white
    space, it should not be colored.  The symbol @scheme['eof] must be used to
    indicate when all the tokens have been consumed.}
    @item{
    A symbol indicating how the token should be treated by the paren
    matcher or @scheme[#f].  This symbol should be in the pairs argument.}
    @item{
    The starting position of the token.}
    @item{
    The ending position of the token.}]

    When @scheme[get-token] accepts an offset and mode value in addition to an
    input port, it must also return two extra results, which are a backup
    distance and new mode. The offset given to @scheme[get-token] can be added
    to the position of the input port to obtain absolute coordinates within a 
    text stream. The mode argument allows @scheme[get-token] to communicate information
    from earlier parsing to later.
    When @scheme[get-token] is called for the beginning on a
    stream, the mode argument is @scheme[#f]; thereafter, the mode
    returned for the previous token is provided to @scheme[get-token]
    for the next token. The mode should not be a mutable value; if
    part of the stream is re-tokenized, the mode saved from the
    immediately preceding token is given again to the
    @scheme[get-token] function. The backup distance returned by @scheme[get-token]
    indicates the maximum number of characters to back up (counting from the start of the token)
    and for re-parsing after a change to the editor within the token's region.

    The @scheme[get-token] function is usually be implemented with a lexer using the 
    @scheme[parser-tools/lex] library. The
    @scheme[get-token] function must obey the following invariants:
    @itemize[
    @item{
    Every position in the buffer must be accounted for in exactly one
    token, and every token must have a non-zero width.}
    @item{
    The token returned by @scheme[get-token] must rely only on the contents of the
    input port argument plus the mode argument. This constraint  means that the
    tokenization of some part of the input cannot depend on earlier parts of the 
    input except through the mode (and implicitly through the starting positions
    for tokens).}
    @item{
    A change to the stream must not change the tokenization of the stream prior
    to the token immediately preceding the change plus the backup distance.  In the following
    example, this invariant does not hold for a zero backup distance: If the buffer contains
    @verbatim[#:indent 2]{" 1 2 3}
    and the tokenizer treats the unmatched " as its own token (a string
    error token), and separately tokenizes the 1 2 and 3, an edit to make
    the buffer look like
    @verbatim[#:indent 2]{" 1 2 3"}
    would result in a single string token modifying previous tokens.  To
    handle these situations, @scheme[get-token] can treat the first line as a
    single token, or it can precisely track backup distances.}]

    The @scheme[pairs] argument is a list of different kinds of matching parens.  The second
    value returned by @scheme[get-token] is compared to this list to see how the
    paren matcher should treat the token.  An example: Suppose pairs is
    @scheme['((|(| |)|) (|[| |]|) (begin end))].  This means that there
    are three kinds of parens.  Any token which has @scheme['begin] as its second
    return value will act as an open for matching tokens with @scheme['end].
    Similarly any token with @scheme['|]|] will act as a closing match for
    tokens with @scheme['|[|].  When trying to correct a mismatched
    closing parenthesis, each closing symbol in pairs will be converted to
    a string and tried as a closing parenthesis.
  }
  @defmethod*[(((stop-colorer (clear-colors boolean #t)) void))]{
    Stops coloring and paren matching the buffer.


    If @scheme[clear-colors] is true all the text in the buffer will have its
    style set to Standard.
  }
  @defmethod*[(((force-stop-colorer (stop? boolean?)) void))]{
    Causes the entire tokenizing/coloring system to become inactive.
    Intended for debugging purposes only.


    @scheme[stop?] determines whether the system is being forced to stop or allowed
    to wake back up.
  }
  @defmethod*[(((is-stopped?) boolean?))]{
    Indicates if the colorer for this editor has been stopped, or not.

  }
  @defmethod*[(((is-frozen?) boolean?))]{
    Indicates if this editor's colorer is frozen. See also
    @method[color:text<%> freeze-colorer]
    and
    @method[color:text<%> thaw-colorer].

  }
  @defmethod*[(((freeze-colorer) void))]{
    Keep the text tokenized and paren matched, but stop altering the colors.

    @scheme[freeze-colorer] will not return until the coloring/tokenization of the
    entire text is brought up-to-date.  It must not be called on a locked
    text.
  }
  @defmethod*[(((thaw-colorer (recolor boolean #t) (retokenize boolean #f)) void))]{
    Start coloring a frozen buffer again.


    If @scheme[recolor?] is @scheme[#t], the text is re-colored.  If it is
    @scheme[#f] the text is not recolored.  When @scheme[recolor?] is @scheme[#t],
    @scheme[retokenize?] controls how the text is recolored.  @scheme[#f] causes
    the text to be entirely re-colored before thaw-colorer returns using
    the existing tokenization.  @scheme[#t] causes the entire text to be
    retokenized and recolored from scratch.  This will happen in the
    background after the call to @scheme[thaw-colorer] returns.

  }
  @defmethod*[(((reset-region (start natural-number?) (end (union (quote end) natural-number?))) void))]{
    Set the region of the text that is tokenized.

  }
  @defmethod*[(((reset-regions (regions (listof (list number (union (quote end) number))))) void))]{

    Sets the currently active regions to be @scheme[regions].
  }
  @defmethod*[(((get-regions) (listof (list number (union (quote end) number)))))]{
    This returns the list of regions that are currently being colored in the editor.

  }
  @defmethod*[(((skip-whitespace (position natural-number?) (direction (symbols (quote forward) (quote backward))) (comments? boolean?)) natural-number?))]{
    Returns the next non-whitespace character.

    Starts from position and skips whitespace in the direction indicated
    by direction.  If @scheme[comments?] is true, comments are skipped as well as
    whitespace.  skip-whitespace determines whitespaces and comments by
    comparing the token type to @scheme['white-space] and @scheme['comment].

    Must only be called while the tokenizer is started.
  }
  @defmethod*[(((backward-match (position natural-number?) (cutoff natural-number?)) (union natural-number? false?)))]{

    Skip all consecutive whitespaces and comments (using @scheme[skip-whitespace])
    immediately preceding the position.  If the token at this position is
    a close, return the position of the matching open, or @scheme[#f] if
    there is none.  If the token was an open, return @scheme[#f].  For any
    other token, return the start of that token.

    Must only be called while the tokenizer is started.
  }
  @defmethod*[(((backward-containing-sexp (position natural-number?) (cutoff natural-number?)) (union natural-number? false?)))]{

    Return the starting position of the interior of the (non-atomic)
    s-expression containing position, or @scheme[#f] is there is none.

    Must only be called while the tokenizer is started.
  }
  @defmethod*[(((forward-match (position natural-number?) (cutoff natural-number?)) (union natural-number? false?)))]{

    Skip all consecutive whitespaces and comments (using @scheme[skip-whitespace])
    immediately following position.  If the token at this position is an
    open, return the position of the matching close, or @scheme[#f] if
    there is none.  For any other token, return the end of that token.

    Must only be called while the tokenizer is started.
  }
  @defmethod*[(((insert-close-paren (position natural-number?) (char char?) (flash? boolean?) (fixup? boolean?)) void))]{

    The @scheme[position] is the place to put the parenthesis, and @scheme[char] is the
    parenthesis to be added (e.g., that the user typed).  If @scheme[fixup?] is true, the right kind of closing
    parenthesis will be chosen from the set previously passed to
    @scheme[start-colorer]---but only if an inserted @scheme[char] would be colored
    as a parenthesis (i.e., with the @scheme['parenthesis] classification).
    Otherwise, @scheme[char] will be inserted, even if it is not the
    right kind.  If @scheme[flash?] is true, the matching open parenthesis will be
    flashed.
  }
  @defmethod*[(((classify-position (position exact-nonnegative-integer?)) symbol?))]{


    Return a symbol for the lexer-determined token type for the token that
     contains the item after @scheme[position].

    Must only be called while the tokenizer is started.
  }
  
  @defmethod[(get-token-range [position exact-nonnegative-integer?]) 
             (values (or/c #f exact-nonnegative-integer?)
                     (or/c #f exact-nonnegative-integer?))]{
    
    Returns the range of the token surrounding @racket[position], if there is a token there.
    
    This method must be called only when the tokenizer is started.
    
  }
  
  @defmethod[#:mode augment (on-lexer-valid [valid? boolean?]) any]{
    This method is an observer for when the lexer is working.
    It is called when the lexer's state changes from valid to invalid (and back). 
    The @racket[valid?] argument indicates if the lexer has finished running over the editor (or not).
    
    The default method just returns @racket[(void)].
  }
  
  @defmethod[#:mode public-final (is-lexer-valid?) boolean?]{
    Indicates if the lexer is currently valid for this editor.
  }
  
}
@defmixin[color:text-mixin (text:basic<%>) (color:text<%>)]{
  Adds the functionality needed for on-the-fly coloring and parenthesis
  matching based on incremental tokenization of the text.
  @defmethod*[#:mode override (((lock) void))]{
  }
  @defmethod*[#:mode override (((on-focus) void))]{
  }
  @defmethod*[#:mode augment (((after-edit-sequence) void))]{
  }
  @defmethod*[#:mode augment (((after-set-position) void))]{
  }
  @defmethod*[#:mode augment (((after-change-style) void))]{
  }
  @defmethod*[#:mode augment (((on-set-size-constraint) void))]{

  }
  @defmethod*[#:mode augment (((after-insert) void))]{
  }
  @defmethod*[#:mode augment (((after-delete) void))]{
  }
}
@defclass[color:text% (color:text-mixin text:keymap%) ()]{}
@definterface[color:text-mode<%> ()]{
}
@defmixin[color:text-mode-mixin (mode:surrogate-text<%>) (color:text-mode<%>)]{
  This mixin adds coloring functionality to the mode.

  @defconstructor[((get-token lexer default-lexer) 
                   (token-sym->style (symbol? . -> . string?) (λ (x) "Standard")) 
                   (matches (listof (list/c symbol? symbol?)) null))]{

    The arguments are passed to 
    @method[color:text<%> start-colorer].
  }
  @defmethod*[#:mode override (((on-disable-surrogate) void))]{
  }
  @defmethod*[#:mode override (((on-enable-surrogate) void))]{
  }
}
@defclass[color:text-mode% (color:text-mode-mixin mode:surrogate-text%) ()]{}

@(include-previously-extracted "main-extracts.ss" #rx"^color:")
