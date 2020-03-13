#lang racket/base
(require "text-sig.rkt"
         "sig.rkt"
         "autocomplete.rkt"
         "../preferences.rkt"
         string-constants
         racket/list
         racket/unit
         racket/class
         mred/mred-sig
         scribble/xref
         scribble/manual-struct)

(provide text-autocomplete@)

(define-unit text-autocomplete@
  (import mred^
          [prefix editor: framework:editor^])
  (export text-autocomplete^)

  
  #| 
=== AUTOCOMPLETE ===

This module defines autocomplete-mixin, a mixin for editors that adds an
unintrusive autocompletion menu when a keystroke is pressed.

By default, the system works by reading the prefix whenever the autcomplete
keystroke is pressed, and then constructing a list of possible completions
by searching through the contents of the autocomplete-word-list parameter for all words
that share that prefix; when the user types another character or deletes a
character autocomplete-word-list is consulted again. This seems to be fast enough for
all but very large completion lists. However, the code has been designed
to allow more efficient implementations if that becomes necessary --- 
all autocomplete-word-list manipulation functions are isolated to the autocompletion-cursor<%>
interface, which implements two main methods, narrow and widen, to add or subtract
a character from the current prefix, respectively. A trie-based implementation,
for instance, could implement narrow and widen in constant-time at the cost of more
memory and more time to build the initial data structure.

===

autocomplete<%>

=new methods=

get-all-words : -> (listof string)
returns a list of all of the possible words that the completion should choose from

get-autocomplete-border-color : -> color string
returns the color for the border of the autocompletion menu

get-autocomplete-background-color : -> color string
returns the background color for the autocompletion menu

get-autocomplete-selected-color : -> color string
returns the selected color for the autocompletion menu

===

autocomplete-mixin: mixin (editor<%> -> editor<%>)

The autocomplete-text mixin produces a class that implements
editor<%> and provides the following extra public methods:

=overridden methods=

on-paint
overridden to draw the autocompletion menu as necessary.

on-char
overridden to intercept keypress events to control the completions
menu.

====

autocompletion-cursor<%>

An autocompletion-cursor<%> abstracts over a set of completions 
for a particular prefix. Typically an autocompletion-cursor<%>
implementation will be created with a particular initial prefix; 
from then on the autocomplete-text system will manipulate it
using the narrow and widen methods in response to user input.

The autocompletion-cursor<%> interface defines the following
methods:

get-completions : -> (listof string) 
Produces a list of all possible completions.

get-length      : -> int
Produces the number of possible completions.

empty?          : -> boolean
Determines if there are any completions in the given cursor.

narrow          : char -> autocompletion-cursor<%>
Yields a new cursor that represents the subset of
the completions held by this cursor that are also
completions of this cursor's prefix followed by the
given character.

widen           : -> autocompletion-cursor<%> | #f
Yields a new cursor that represents the completions
of this cursor's prefix with the last character
removed.

===
autocompletion-cursor%

The implementation of autcompletion-cursor<%> used
by the default get-completions method.

===

scrolling-cursor : mixin (autocompletion-cursor<%> -> scrolling-cursor<%>)

scrolling-cursor is a mixin that takes classes that implement
autocompletion-cursor<%> to classes that implement scrolling-cursor<%>
(not provided).

===
configuration parameters

These configuration parameters customize autocompletion behavior.

autocomplete-append-after : string parameter
designates text to insert after a completion. Default: ""

autocomplete-limit : positive int parameter
designates the maximum number of completions to show at a time. Default: 15

completion-mode-key : character parameter
designates the character that triggers autocompletion

|#

  (define autocomplete<%>
    (interface ((class->interface text%))
      auto-complete
      get-autocomplete-border-color
      get-autocomplete-background-color
      get-autocomplete-selected-color
      completion-mode-key-event?
      get-all-words
      get-word-at))

  ;; ============================================================
  ;; auto-complete-text (mixin) implementation

  (define selected-color (make-object color% 204 153 255))

  (define autocomplete-mixin
    (mixin ((class->interface text%)) (autocomplete<%>)
    
      (inherit invalidate-bitmap-cache get-dc get-start-position get-end-position
               find-wordbreak get-text position-location insert dc-location-to-editor-location)
    
      ; get-autocomplete-border-color : -> string
      ; the color of text in the autocomplete menu
      (define/public (get-autocomplete-border-color) "black")
    
      ; get-background-color : -> string
      ; background color in the autocomplete menu
      (define/public (get-autocomplete-background-color) "lavender")
    
      ; get-autocomplete-selected-color : -> string
      ; selected option background color in the autocomplete menu
      (define/public (get-autocomplete-selected-color) selected-color)
    
      (define/public (completion-mode-key-event? key-event)
        (cond
          [(and (eq? (send key-event get-key-code) #\.)
                (send key-event get-control-down))
           (or (eq? (system-type) 'macosx)
               (not (preferences:get 'framework:menu-bindings)))]
          [else
           #f]))
    
      (define/public (get-all-words) (get-completions/manuals #f))
    
      (define completions-box #f) ; completions-box% or #f if no completions box is active right now
      (define word-start-pos #f)  ; start pos of that word, or #f if no autocompletion
      (define word-end-pos #f)    ; end pos of that word, or #f if none
    
      ; string -> scrolling-cursor<%>       given a prefix, returns the possible completions
      ; given a word, produces a cursor that describes
      ; all possible completions. The default implementation of autocompletion-cursor%
      ; returns all strings from the get-all-words method (below)
      ; that have the given string as a prefix; it performs a 
      ; linear-search at every narrow/widen.
      (define/private (get-completions word) 
        (new autocompletion-cursor% 
             [word word] 
             [all-words (get-all-words)]))
    
      (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
        (super on-paint before? dc left top right bottom dx dy draw-caret)
        (when (and completions-box (not before?))
          (send completions-box draw dc dx dy)))
    
      ;; (-> void)
      ;; Check for possible completions of the current word and give the user a menu for them.
      (define/public-final (auto-complete)
        (when (equal? (get-start-position) (get-end-position))
          (let* ([end-pos (get-end-position)]
                 [word (get-word-at end-pos)]
                 [completion-cursor (get-completions word)])
            (let ([start-pos (- end-pos (string-length word))])
              (set! word-start-pos start-pos)
              (set! word-end-pos end-pos)
              (show-options word start-pos end-pos completion-cursor)))))
    
      ;; Number -> String
      ;; The word that ends at the current position of the editor
      (define/public (get-word-at current-pos)
        (let ([start-pos (box current-pos)]) 
          (find-wordbreak start-pos #f 'caret)
          (get-text (unbox start-pos) current-pos)))
    
      ;; String Number Number scrolling-cursor<%> -> void
      ;; Popup a menu of the given words at the location of the end-pos. Each menu item
      ;; should change the current word to the word in the list.
      (define/private (show-options word start-pos end-pos cursor)
        (let ([x (box 0)]
              [yb (box 0)]
              [yt (box 0)])
          (position-location start-pos x yb #f)
          (position-location start-pos #f yt #t)
          (set! completions-box (new completion-box%
                                     [completions (new scroll-manager% [cursor cursor])]
                                     [line-x (unbox x)]
                                     [line-y-above (unbox yt)]
                                     [line-y-below (unbox yb)]
                                     [editor this]))
          (send completions-box redraw)))
    
      (define/augment (after-set-position)
        (when completions-box
          (destroy-completions-box)
          (auto-complete))
        (inner (void) after-set-position))
    
      ;; on-char must handle inputs for two modes: normal text mode and in-the-middle-of-autocompleting
      ;; mode perhaps it would be better to handle this using the state machine pattern
      (define/override (on-char key-event)
        (cond
          [completions-box
           (let ([code (send key-event get-key-code)]
                 [full? (not (send completions-box empty?))])
             (cond
               [(and full? (memq code '(up wheel-up)))
                (send completions-box prev-item)]
               [(and full? 
                     (or (memq code '(down wheel-down))
                         (completion-mode-key-event? key-event)))
                (send completions-box next-item)]
               [(and full? (eq? code 'prior)) (send completions-box scroll-display-up)]
               [(and full? (eq? code 'next))  (send completions-box scroll-display-down)]
               [(eq? code 'release)
                (void)]
               [(eq? code #\backspace)
                (widen-possible-completions)
                (super on-char key-event)]
               [(eq? code #\return)
                (when full?
                  (insert-currently-selected-string))
                (destroy-completions-box)]
               [(and (char? code) (char-graphic? code))
                (super on-char key-event)
                (constrict-possible-completions code)]
               [else
                (destroy-completions-box)
                (super on-char key-event)]))]
          [(completion-mode-key-event? key-event)
           (auto-complete)]
          [else 
           (super on-char key-event)]))
    
      ;; on-event controls what happens with the mouse
      (define/override (on-event mouse-event)
        (cond
          [completions-box
           (let*-values ([(x) (send mouse-event get-x)]
                         [(y) (send mouse-event get-y)]
                         [(mouse-x mouse-y) (dc-location-to-editor-location x y)])
             (if (and (send completions-box point-inside-menu? mouse-x mouse-y)
                      (not (send completions-box empty?)))
                 (cond
                   [(send mouse-event moving?)
                    (send completions-box handle-mouse-movement mouse-x mouse-y)
                    (super on-event mouse-event)]
                   [(send mouse-event button-down?)
                    (insert-currently-selected-string)
                    (destroy-completions-box)]
                   [else
                    (super on-event mouse-event)])
                 (super on-event mouse-event)))]
          [else (super on-event mouse-event)]))
    
      (define/private (constrict-possible-completions char)
        (set! word-end-pos (add1 word-end-pos))
        (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
          (send completions-box narrow char)
          (let-values ([(_ __ x1p y1p) (send completions-box get-menu-coordinates)])
            (invalidate-bitmap-cache x0 y0 (max x1 x1p) (max y1 y1p)))))
    
      (define/private (widen-possible-completions)
        (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
          (let ([reasonable? (send completions-box widen)])
            (cond
              [reasonable?
               (set! word-end-pos (sub1 word-end-pos))
               (let-values ([(_ __ x1p y1p) (send completions-box get-menu-coordinates)])
                 (invalidate-bitmap-cache x0 y0 (max x1 x1p) (max y1 y1p)))]
              [else
               (set! completions-box #f)
               (invalidate-bitmap-cache x0 y0 x1 y1)]))))
    
      ;; destroy-completions-box : -> void
      ;; eliminates the active completions box
      (define/private (destroy-completions-box)
        (let-values ([(x0 y0 x1 y1) (send completions-box get-menu-coordinates)])
          (set! completions-box #f)
          (invalidate-bitmap-cache x0 y0 x1 y1)))
    
      ;; insert-currently-selected-string : -> void
      ;; inserts the string that is currently being autoselected
      (define/private (insert-currently-selected-string)
        (let ([css (send completions-box get-current-selection)])
          (insert (string-append css (autocomplete-append-after)) word-start-pos word-end-pos)))
    
      (super-new)))

  (define scrolling-cursor<%>
    (interface (autocompletion-cursor<%>)
      items-are-hidden?
      get-visible-completions
      get-visible-length
      scroll-down
      scroll-up))

  (define scroll-manager% 
    (class* object% ()
      (init-field cursor)
    
      (define all-completions #f)
      (define all-completions-length #f)
      (define visible-completions #f)
      (define visible-completions-length #f)
      (define hidden? #f)
    
      (define/private (initialize-state!)
        (cond
          [(<= (send cursor get-length) (autocomplete-limit))
           (set! hidden? #f)
           (set! all-completions (send cursor get-completions))
           (set! all-completions-length (send cursor get-length))
           (set! visible-completions all-completions)
           (set! visible-completions-length all-completions-length)]
          [else
           (set! hidden? #t)
           (set! all-completions (send cursor get-completions))
           (set! all-completions-length (send cursor get-length))
           (set! visible-completions (take (send cursor get-completions) (autocomplete-limit)))
           (set! visible-completions-length (autocomplete-limit))]))
    
      (define/public (get-completions) all-completions)
      (define/public (get-length) all-completions-length)
      (define/public (empty?) (send cursor empty?))
    
      (define/public (get-visible-length) visible-completions-length)
      (define/public (get-visible-completions) visible-completions)
    
      (define/public (items-are-hidden?) hidden?)
    
      (define/public (scroll-down)
        (when hidden?
          (set! all-completions (append (drop all-completions (autocomplete-limit))
                                        visible-completions))
          (set! visible-completions (take all-completions (autocomplete-limit)))))
    
      (define/public (scroll-up)
        (when hidden?
          (let ([n (- all-completions-length (autocomplete-limit))])
            (set! all-completions (append (drop all-completions n) (take all-completions n)))
            (set! visible-completions (take all-completions (autocomplete-limit))))))
    
      (define/public (narrow char)
        (let ([new-cursor (send cursor narrow char)])
          (set! cursor new-cursor)
          (initialize-state!)))
    
      (define/public (widen)
        (let ([new-cursor (send cursor widen)])
          (cond
            [new-cursor
             (set! cursor new-cursor)
             (initialize-state!)
             #t]
            [else #f])))
    
      (initialize-state!)
      (super-new)))

  ;; ============================================================
  ;; completion-box<%> implementation

  (define menu-padding-x 4)
  (define menu-padding-y 0)

  (define completion-box<%>
    (interface ()
      draw                   ; dc<%> int int -> void
      redraw                 ; -> void
      get-menu-coordinates   ; -> (values int int int int)
      next-item              ; -> void
      prev-item              ; -> void
      scroll-display-up      ; -> void
      scroll-display-down    ; -> void
      get-current-selection  ; -> string
      narrow                 ; char -> boolean
      widen                  ;      -> boolean
      empty?))               ; -> boolean


  (define hidden-completions-text "⋮")
  (define-struct geometry (menu-x
                           menu-y
                           menu-width
                           menu-height
                           mouse->menu-item-vector))

  (define completion-box% 
    (class* object% (completion-box<%>)
    
      (init-field completions       ; scroll-manager%      
                  ;                   the possible completions (all of which have base-word as a prefix)
                  line-x            ; int                  
                  ;                   the x coordinate of the line where the menu goes
                  line-y-above      ; int                   
                  ;                   the y coordinate of the top of the line where the menu goes
                  line-y-below      ; int                  
                  ;                   the y coordinate of the bottom of the line where the menu goes
                  editor            ; editor<%>             
                  ;                   the owner of this completion box
                  )
    
      (define/public (empty?) (send completions empty?))
    
      (define/private (compute-geometry)
      
        (define vec #f)
        (define (initialize-mouse-offset-map! coord-map)
          (cond
            [(null? coord-map) (void)] ; is this possible?
            [else
             (let* ([last-index (cadr (car coord-map))]
                    [v (make-vector (add1 last-index))])
               (for-each
                (λ (elt)
                  (let ([first (car elt)]
                        [last  (cadr elt)]
                        [val   (caddr elt)])
                    (let loop ([n first])
                      (when (<= n last)
                        (vector-set! v n val)
                        (loop (add1 n))))))
                coord-map)
               (set! vec v))]))
      
        (define-values (editor-width editor-height)
          (let* ([wb (box 0)]
                 [hb (box 0)]
                 [admin (send editor get-admin)])
            (if admin
                (begin
                  (send admin get-view #f #f wb hb)
                  (values (unbox wb)
                          (unbox hb)))
                (values 10 10))))
      
        (let* ([num-completions (send completions get-length)]
               [shown-completions (send completions get-visible-completions)])
          (define-values (w h)
            (let ([dc (send editor get-dc)])
              (cond
                [(zero? num-completions)
                 (let-values ([(tw th _1 _2) (send dc get-text-extent (string-constant no-completions) 
                                                   (get-mt-font))])
                   (values (+ menu-padding-x tw menu-padding-x)
                           (+ menu-padding-y th menu-padding-y)))]
                [else
                 (let loop ([pc shown-completions]
                            [w 0]
                            [h 0]
                            [coord-map '()]
                            [n 0])
                   (cond
                     [(null? pc)
                      (let-values ([(hidden?) (send completions items-are-hidden?)] 
                                   [(tw th _1 _2) (send dc get-text-extent
                                                        hidden-completions-text
                                                        (get-reg-font))])
                        (let ([w (if hidden? (max tw w) w)]
                              [h (if hidden? (+ th h) h)])
                          (initialize-mouse-offset-map! coord-map)
                          (let ([offset-h menu-padding-y]
                                [offset-w (* menu-padding-x 2)])
                            (values (+ offset-w w)
                                    (+ offset-h h)))))]
                     [else 
                      (let ([c (car pc)])
                        (let-values ([(tw th _1 _2) (send dc get-text-extent c (get-reg-font))])
                          (loop (cdr pc)
                                (max tw w)
                                (+ th h)
                                (cons (list (inexact->exact h) (inexact->exact (+ h th)) n) coord-map)
                                (add1 n))))]))])))
        
          (let ([final-x (cond
                           [(< (+ line-x w) editor-width)
                            line-x]
                           [(> editor-width w)
                            (- editor-width w)]
                           [else line-x])]
                [final-y (cond
                           [(< (+ line-y-below 2 h) editor-height)
                            (+ line-y-below 2)]
                           [(> (- line-y-above h) 0)
                            (- line-y-above h)]
                           [else
                            (+ line-y-below 2)])])
          
            (make-geometry final-x final-y w h vec))))
    
      ;; geometry records the menu's current width and height and
      ;; a vector associating mouse location with a selected item
      (define geometry (compute-geometry))
    
      (define highlighted-menu-item 0) ; the currently-highlighted menu item
    
      ;; draw : dc<%> int int -> void
      ;; draws the menu to the given drawing context at offset dx, dy
      (define/public (draw dc dx dy)
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)]
              [font (send dc get-font)])
          (define-values (mx my tw th) (get-menu-coordinates))
          (send dc set-pen (send editor get-autocomplete-border-color) 1 'solid)
          (send dc set-brush (send editor get-autocomplete-background-color) 'solid)
          (send dc draw-rectangle (+ mx dx) (+ my dy) tw th)
        
          (cond
            [(send completions empty?)
             (let ([font (send dc get-font)])
               (send dc set-font (get-mt-font))
               (send dc draw-text 
                     (string-constant no-completions) 
                     (+ mx dx menu-padding-x)
                     (+ menu-padding-y my dy))
               (send dc set-font font))]
            [else
             (send dc set-font (get-reg-font))
             (let loop ([item-number 0] [y my] [pc (send completions get-visible-completions)])
               (cond
                 [(null? pc) 
                  (when (send completions items-are-hidden?)
                    (let-values ([(hw _1 _2 _3) (send dc get-text-extent hidden-completions-text)])
                      (send dc draw-text 
                            hidden-completions-text 
                            (+ mx dx (- (/ tw 2) (/ hw 2)))
                            (+ menu-padding-y y dy))))]
                 [else
                  (let ([c (car pc)])
                    (let-values ([(w h d a) (send dc get-text-extent c)])
                      (when (= item-number highlighted-menu-item)
                        (send dc set-pen "black" 1 'transparent)
                        (send dc set-brush (send editor get-autocomplete-selected-color) 'solid)
                        (send dc draw-rectangle (+ mx dx 1) (+ dy y menu-padding-y 1) (- tw 2) (- h 1)))
                      (send dc draw-text c (+ mx dx menu-padding-x) (+ menu-padding-y y dy))
                      (loop (add1 item-number) (+ y h) (cdr pc))))]))])
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)
          (send dc set-font font)))
    
      (define/private (get-mt-font)
        (send the-font-list find-or-create-font
              (editor:get-current-preferred-font-size)
              'default
              'italic
              'normal))
    
      (define/private (get-reg-font)
        (send the-font-list find-or-create-font
              (editor:get-current-preferred-font-size)
              'default
              'normal
              'normal))
    
      ;; redraw : -> void
      ;; tells the parent to refresh enough of itself to redraw this menu
      (define/public (redraw)
        (let-values ([(x y w h) (get-menu-coordinates)])
          (send editor invalidate-bitmap-cache x y w h)))
    
      ;; get-menu-coordinates : -> (values int int int int)
      ;; get the menu's x, y, w, h coordinates with respect to its parent
      (define/public (get-menu-coordinates)
        (values (geometry-menu-x geometry)
                (geometry-menu-y geometry)
                (geometry-menu-width geometry)
                (geometry-menu-height geometry)))
    
      ;; next-item : -> void
      ;; tells the menu that the next item is selected
      (define/public (next-item)
        (cond
          [(and (= highlighted-menu-item (sub1 (autocomplete-limit)))
                (send completions items-are-hidden?))
           (set! highlighted-menu-item 0)
           (scroll-display-down)]
          [else
           (set! highlighted-menu-item (modulo (add1 highlighted-menu-item)
                                               (send completions get-visible-length)))
           (redraw)]))
    
      ;; prev-item : -> void
      ;; tells the menu that the previous item is selected
      (define/public (prev-item)
        (cond
          [(and (= highlighted-menu-item 0)
                (send completions items-are-hidden?))
           (set! highlighted-menu-item
                 (sub1 (send completions get-visible-length)))
           (scroll-display-up)]
          [else
           (set! highlighted-menu-item (modulo (sub1 highlighted-menu-item)
                                               (send completions get-visible-length)))
           (redraw)]))
    
      ;; scroll-display-down : -> void
      ;; shows the next page possible completions
      (define/private (scroll-display do-it!)
        (let*-values ([(old-x1 old-y1 old-w old-h) (get-menu-coordinates)]
                      [(_) (do-it!)]
                      [(_) (set! geometry (compute-geometry))]
                      [(new-x1 new-y1 new-w new-h) (get-menu-coordinates)])
          (let ([old-x2 (+ old-x1 old-w)]
                [old-y2 (+ old-y1 old-h)]
                [new-x2 (+ new-x1 new-w)]
                [new-y2 (+ new-y1 new-h)])
            (let ([composite-x1 (min old-x1 new-x1)]
                  [composite-y1 (min old-y1 new-y1)]
                  [composite-x2 (max old-x2 new-x2)]
                  [composite-y2 (max old-y2 new-y2)])
              (send editor invalidate-bitmap-cache 
                    composite-x1
                    composite-y1
                    (- composite-x2 composite-x1)
                    (- composite-y2 composite-y1))))))
    
      (define/public (scroll-display-down)
        (scroll-display (λ () (send completions scroll-down))))
    
      (define/public (scroll-display-up)
        (scroll-display (λ () (send completions scroll-up))))
    
      ;; point-inside-menu? : nat nat -> boolean
      ;; determines if the given x,y editor coordinate is inside
      ;; the drawn window or not
      (define/public (point-inside-menu? x y)
        (let*-values ([(mx my w h) (get-menu-coordinates)])
          (and (<= mx x (+ mx w))
               (<= my y (+ my h)))))
    
      ;; handle-mouse-movement : int int -> bool
      ;; takes an editor coordinate, returns whether it has intercept
      (define/public (handle-mouse-movement x y)
        (define-values (mx my w h) (get-menu-coordinates))
        (define index (floor (inexact->exact (- y my))))
        (when (and (<= mx x (+ mx w))
                   (< menu-padding-y
                      index
                      (vector-length (geometry-mouse->menu-item-vector geometry))))
          (set! highlighted-menu-item (vector-ref (geometry-mouse->menu-item-vector geometry)
                                                  index))
          (redraw)))
    
      ;; get-current-selection : -> string
      ;; returns the selected string
      (define/public (get-current-selection)
        (list-ref (send completions get-visible-completions) highlighted-menu-item))
    
      ;; narrow : char -> boolean
      ;; narrows the given selection given a new character (faster than recomputing the whole thing)
      (define/public (narrow char)
        (send completions narrow char)
        (set! highlighted-menu-item 0)
        (set! geometry (compute-geometry))
        (not (send completions empty?)))
    
      ;; widen : -> boolean
      ;; attempts widens the selection by eliminating the last character from the word.
      ;; returns #f if that cannot be done (because there are no characters left); #t otherwise
      (define/public (widen)
        (let ([successfully-widened? (send completions widen)])
          (cond
            [successfully-widened?
             (set! highlighted-menu-item 0)
             (set! geometry (compute-geometry))
             #t]
            [else #f])))
    
      (super-new)))

  ;; ============================================================
  ;; configuration parameters

  (define (make-guarded-parameter name description default okay?)
    (make-parameter
     default
     (λ (v)
       (cond
         [(okay? v) v]
         [else
          (raise (make-exn:fail:contract
                  (string->immutable-string
                   (format "parameter ~a: expected ~a, given: ~e" name description v))
                  (current-continuation-marks)))]))))

  (define autocomplete-append-after
    (make-guarded-parameter 'append-after "string" "" string?))
  (define autocomplete-limit
    (make-guarded-parameter 'limit "positive integer" 15 (λ (x) (and (integer? x) (> x 0)))))

  ;; ============================================================
  ;; read keywords from manuals

  (define xref #f)

  (define (get-completions/manuals manuals)
    (let* ([sym->mpi (λ (mp) (module-path-index-resolve (module-path-index-join mp #f)))]
           [manual-mpis (and manuals (map sym->mpi manuals))])
    
      (unless xref
        (let ([load-collections-xref
               ;; Make the dependency on `setup/xref' indirect, so that a
               ;; GUI does not depend on having documentation installed:
               (with-handlers ([exn:missing-module? (lambda (exn)
                                                      (lambda ()
                                                        (load-xref null)))])
                 (dynamic-require 'setup/xref 'load-collections-xref))])
          (set! xref (load-collections-xref))))
    
      (let ([ht (make-hash)])
        (for-each
         (λ (entry)
           (let ([desc (entry-desc entry)])
             (when (exported-index-desc? desc)
               (let ([name (exported-index-desc-name desc)])
                 (when name
                   (when (or (not manual-mpis)
                             (ormap (λ (from-lib) (memq from-lib manual-mpis))
                                    (map sym->mpi (exported-index-desc-from-libs desc))))
                     (hash-set! ht (symbol->string name) #t)))))))
         (xref-index xref))
        (sort (hash-map ht (λ (x y) x)) string<?))))

  ;; ============================================================
  ;; auto complete example code

  #;
  (begin
    (define all-words (get-completions/manuals #f))
  
    (let* ([f (new frame% (label "Test") (height 400) (width 400))]
           [e (new (autocomplete-mixin text%))]
           [c (new editor-canvas% (editor e) (parent f))])
      (send c focus)
      (send e insert "\n\n     get")
      (send e set-position (send e last-position) (send e last-position))
      (send f show #t))))
