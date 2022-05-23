#lang racket/base
(require racket/fixnum
         racket/symbol
         racket/vector
         "port.rkt"
         "public.rkt"
         "screen.rkt"
         "pos.rkt"
         "assert.rkt"
         "param.rkt"
         "token.rkt"
         "object.rkt"
         "color.rkt"
         "wstring.rkt")

;; See "../main.rkt"

(provide
 (protect-out string->entry
              entry->string
              string->lines
              (struct-out eestate) make-eestate
              ; primtiive and derived record accessors and mutators: no ee argument
              entry?
              entry-col
              entry-nsr
              entry-row
              entry-mark
              entry-point
              null-entry?
              entry-mark-set!
              entry-row-set!
              set-entry-col!
              ; normal entry procedures: first two arguments are ee and entry
              add-char
              beginning-of-line?
              clear-entry
              id-completions
              correct&flash-matching-delimiter
              yank-entry
              delete-char
              delete-forward
              delete-to-eol
              echo-entry
              end-of-line?
              find-matching-delimiter
              find-next-exp-backward
              find-next-exp-forward
              find-next-exp-upward
              find-next-exp-downward
              find-next-word
              find-previous-word
              find-whitespace-start
              find-whitespace-end
              first-line?
              flash
              goto
              handle-winch
              indent
              indent-all
              insert-string-before
              insert-strings-before
              join-rows
              last-line?
              last-line-displayed?
              move-bol
              move-down
              move-eoe
              move-eol
              move-left
              move-right
              move-up
              move-forward
              only-whitespace-left?
              page-down
              page-up
              redisplay
              should-auto-indent?
              update-entry-colors))

(define (fxzero? n) (fx= n 0))
(define (fx1- n) (fx- n 1))
(define (fx1+ n) (fx+ n 1))

(define lpchar #\()
(define rpchar #\))
(define lbchar #\[)
(define rbchar #\])
(define lcchar #\{)
(define rcchar #\})

;;; eestate holds the state of the expression editor.

(struct eestate (last-op
                 rt-last-op
                 prompt
                 repeat-count
                 killbuf
                 histnew
                 histbwd
                 histnow
                 histfwd
                 histkey
                 last-suffix*
                 cc?)
  #:mutable
  #:authentic
  #:sealed)

(define (make-eestate)
  (eestate #f         ; last-op
           '(0 . 0)   ; rt-last-op
           ""         ; prompt
           1          ; repeat-count
           '()        ; killbuf
           0          ; histnew
           '()        ; histbwd
           ""         ; histnow
           '()        ; hisfwd
           ""         ; histkey
           '()        ; last-suffix*
           #f))       ; cc?

; NB. top-line and bot-line aren't really positions.
; the row does identify the logical row, but the col identifies the
; physical row of the logical row, i.e., 0 for the first physical
; row, 1 for the second, etc.

(struct entry (lns                 ; logical lines
               row                 ; point (logical cursor) row
               col                 ; point (logical cursor) column
               screen-cols         ; cached screen columns
               screen-rows         ; cached screen rows
               top-line            ; first displayed line
               bot-line            ; last displayed line
               mark)               ; current mark pos
  #:mutable
  #:authentic
  #:sealed)

(define entry-mark-set! set-entry-mark!)
(define entry-row-set! set-entry-row!)

(define (make-entry lns)
  (entry lns 0 0 (screen-cols) (screen-rows)
         (make-pos 0 0) (make-pos 0 0) #f))

(define (entry-point entry)
  (make-pos (entry-row entry) (entry-col entry)))

;;; an lns is a nonempty list of logical lines, each of which may span
;;; multiple screen lines.  each line consists of an integer that records
;;; the number of screen rows spanned along with a string containing
;;; the text of the line.  lines are implicitly separated by newlines; no
;;; newlines appear in the strings themselves.
;;; each line also has a vector of colors for the characters in the
;;; line's srring.
;;; each character spans a logcal column, but it may span multiple
;;; physical columns.
;;;
;;; lns   := (ln ln ...)  ;;; list of "ln"s
;;; ln    := [nsr, str]
;;; nsr   := integer      ;;; number of physical rows occupied by the line
;;; str   := string       ;;; contents of the line

;; arrange for nsr to be updated whenever str is changed
(struct ln (str nsr colors unicell?)
  #:mutable
  #:authentic
  #:sealed)
(define (make-ln ee str)
  (ln str (str->nsr ee str) (str->colors str) (string-unicell? str)))
(define (ln-str-set! ee ln str [colors (str->colors str)])
  (set-ln-str! ln str)
  (set-ln-nsr! ln (str->nsr ee str))
  (set-ln-colors! ln colors)
  (set-ln-unicell?! ln (string-unicell? str)))
(define ln-nsr-set! set-ln-nsr!)

(define (str->colors str [color default-color])
  (make-vector (string-length str) color))
    
; extract nsr or str from selected row of lns
(define (lns->nsr lns row) (ln-nsr (list-ref lns row)))
(define (lns->str lns row) (ln-str (list-ref lns row)))
(define (lns->colors lns row) (ln-colors (list-ref lns row)))

; replace str in selected row of lns
(define (lns->str! ee lns row str)
  (ln-str-set! ee (list-ref lns row) str))

(define (lns->char lns row col)
  (let ([str (lns->str lns row)])
    (if (fx< col (string-length str))
        (string-ref str col)
        #f)))

(define (yank-entry ee entry)
  (map ln-str (entry-lns entry)))

(define (entry->string entry
                       #:from-row [from-row 0]
                       #:from-col [from-col 0]
                       #:up-to-row [up-to-row #f]
                       #:up-to-col [up-to-col #f])
  (let* ([lns (entry-lns entry)] [n (length lns)])
    (let ([op (open-output-string)])
      (let loop ([i from-row] [sep ""])
        (unless (fx= i n)
          (let ([str (lns->str lns i)])
            (cond
              [(and (fx= i from-row) (fx> from-col 0))
               (define up-to-row? (and up-to-row (fx= i up-to-row)))
               (fprintf op "~a~a" sep (substring str from-col
                                                 (if up-to-row?
                                                     up-to-col
                                                     (string-length str))))
               (unless up-to-row?
                 (loop (fx+ i 1) "\n"))]
              [(and up-to-row (fx= i up-to-row))
               (fprintf op "~a~a" sep (substring str 0 up-to-col))]
              [else
               (fprintf op "~a~a" sep str)
               (loop (fx+ i 1) "\n")]))))
      (get-output-string op))))

(define (editor-object e row col)
  (define s (entry->string e))
  (values (new-object s)
          (let loop ([offset 0] [row row])
            (cond
              [(fxzero? row) (+ offset col)]
              [(= offset (string-length s)) offset]
              [(eqv? #\newline (string-ref s offset))
               (loop (fx+ offset 1) (fx- row 1))]
              [else
               (loop (fx+ offset 1) row)]))
          (lambda (dest-offset)
            (and dest-offset
                 (index->pos s dest-offset 0 0)))))

(define (echo-entry ee entry tp)
  (write-string (eestate-prompt ee) tp)
  (let ([lns (entry-lns entry)])
    (fprintf tp "~a\n" (ln-str (car lns)))
    (for-each
     (let ([pad (phantom-prompt ee)])
       (lambda (ln) (fprintf tp "~a~a\n" pad (ln-str ln))))
     (cdr lns)))
  (flush-output tp))

(define (string->lines s)
  ; break string into list of lines while expanding tabs
  (let ([n (string-length s)] [op (open-output-string)])
    (let f ([i 0] [col 0])
      (if (fx= i n)
          (list (get-output-string op))
          (let ([c (string-ref s i)])
            (case c
              [(#\newline)
               (let ([line (bytes->string/utf-8 (get-output-bytes op #t))])
                 (cons line (f (fx+ i 1) 0)))]
              [(#\tab)
               (do ([i (fx- 8 (fxmodulo col 8)) (fx- i 1)])
                   ((fx= i 0))
                 (write-char #\space op))
               (f (fx+ i 1) 0)]
              [(#\return) (f (fx+ i 1) col)]
              [else (write-char c op) (f (fx+ i 1) (fx+ col 1))]))))))

(define (string->entry ee s)
  (let ([ln* (map (lambda (str) (make-ln ee str)) (string->lines s))])
    (make-entry (if (null? ln*) (list (make-ln ee "")) ln*))))

(define (null-entry? entry)
  (let ([lns (entry-lns entry)])
    (and (fx= (length lns) 1)
         (equal? (ln-str (car lns)) ""))))

(define (entry-nsr entry) (apply fx+ (map ln-nsr (entry-lns entry))))

;;; split a logical row into a list of strings each of which will fit
;;; on a screen line, starting at logical column col which resides at
;;; physical column pcol
(define split-string
  (lambda (ee str col pcol)
    (let ([str-len (string-length str)])
      (let f ([col col] [width (fx- (screen-cols) pcol)])
        (define n (string-fits-width str col width))
        (if (fx= (fx- str-len col) n)
            (cons (substring str col)
                  (if (or (n . fx< . (fx- (string-length str) col))
                          (fx= width (string-width str col)))
                      (list "")
                      null))
            (cons (substring str col (fx+ col n))
                  (f (fx+ col n) (screen-cols))))))))

(define split-vector-like
  (lambda (ee vec col strs)
    (let ([vec-len (vector-length vec)])
      (let f ([col col] [strs strs])
        (cond
          [(null? strs) null]
          [else
           (define len (string-length (car strs)))
           (cons (vector-copy vec col (fx+ col len))
                 (f (fx+ col len) (cdr strs)))])))))

(define (screen-lines-between ee entry toprow topoff nextrow nextoff)
  ; returns distance in physical screen lines between physical line
  ; topoff of toprow and nextoff of nextrow
  (max (let ([lns (entry-lns entry)])
         (let f ([i toprow] [off topoff] [lns (list-tail lns toprow)])
           (if (fx= i nextrow)
               (fx- nextoff off)
               (fx+ (fx- (ln-nsr (car lns)) off)
                    (f (fx+ i 1) 0 (cdr lns))))))
       ;; shouldn't go negative, bust just in case:
       0))

(define (prompt-width ee)
  (string-length (eestate-prompt ee)))

(define (str->nsr ee str)
  ;; we can't just divide by the screen width, because a double-wide
  ;; character might be at a line boundary
  (let loop ([i 0] [prompt-len (prompt-width ee)])
    (define w (fx- (screen-cols) prompt-len))
    (define n (string-fits-width str i w))
    (if (fx= n (fx- (string-length str) i))
        (if (fx= w (string-width str i))
            2
            1)
        (fx+ 1 (loop (fx+ i n) 0)))))

;;; return the line offset based on the column and screen size;
;;; we have to walk the string to handle multicolumn characters
(define col->line-offset
  (lambda (ee entry row col)
    (define ln (list-ref (entry-lns entry) row))
    (cond
      [(ln-unicell? ln)
       (fxquotient (fx+ (string-length (eestate-prompt ee)) col) (screen-cols))]
      [else
       (define str (ln-str ln))
       (let loop ([i 0] [col col] [offset 0] [prompt-len (prompt-width ee)])
         (define w (fx- (screen-cols) prompt-len))
         (define n (string-fits-width str i w))
         (cond
           [(fx<= col n)
            (if (and (fx= col n)
                     (or (n . fx< . (fx- (string-length str) i))
                         (fx= w (string-width str i))))
                (fx+ offset 1)
                offset)]
           [else
            (loop (fx+ i n) (fx- col n) (fx+ offset 1) 0)]))])))

;;; return the actual screen column based on the logical row column;
;;; we have to walk the string to handle multicolumn characters
(define col->screen-col
  (lambda (ee entry row col)
    (define ln (list-ref (entry-lns entry) row))
    (cond
      [(ln-unicell? ln)
       (fxremainder (fx+ col (string-length (eestate-prompt ee))) (screen-cols))]
      [else
       (define str (ln-str ln))
       (let loop ([i 0] [col col] [prompt-len (prompt-width ee)])
         (define w (fx- (screen-cols) prompt-len))
         (define n (string-fits-width str i w))
         (cond
           [(fx<= col n)
            (if (and (fx= col n)
                     (or (n . fx< . (fx- (string-length str) i))
                         (fx= w (string-width str i))))
                0
                (let loop ([i i] [col col] [pcol prompt-len])
                  (if (fx= col 0)
                      pcol
                      (loop (fx+ i 1) (fx- col 1) (fx+ pcol (char-width (string-ref str i)))))))]
           [else (loop (fx+ i n) (fx- col n) 0)]))])))

(define (clear-entry ee entry)
  ; like clear-screen, but clears only from top line of entry
  (if (visible? ee entry 0 0)
      (begin
        (carriage-return)
        (move-cursor-up
         (let ([top-line (entry-top-line entry)])
           (screen-lines-between ee entry
                                 (pos-row top-line) (pos-col top-line)
                                 (entry-row entry) (col->line-offset ee entry (entry-row entry) (entry-col entry)))))
        (clear-eos))
      (clear-screen)))

;;; given bottom line displayed, determines top line that will fill
;;; the screen to the extent possible
(define (calc-top-line-displayed entry last-row-pos [nrows (screen-rows)])
  (let ([lns (entry-lns entry)])
    (let loop ([n nrows]
               [r (pos-row last-row-pos)]
               [off (pos-col last-row-pos)])
      (if (or (fx= n 1) (and (fx= r 0) (fx= off 0)))
          (make-pos r off)
          (if (fx= off 0)
              (loop (fx- n 1) (fx- r 1) (fx- (lns->nsr lns (fx- r 1)) 1))
              (loop (fx- n 1) r (fx- off 1)))))))

;;; given first line displayed, determines bottom line that will fill
;;; the screen to the extent possible
(define (calc-bot-line-displayed entry first-row-pos [nrows (screen-rows)])
  (let* ([lns (entry-lns entry)]
         [last-row (fx- (length lns) 1)]
         [last-off (fx- (lns->nsr lns last-row) 1)]
         [first-row (pos-row first-row-pos)])
    (let loop ([n nrows]
               [r first-row]
               [off (pos-col first-row-pos)]
               [off-max (fx- (lns->nsr lns first-row) 1)])
      (if (or (fx= n 1) (and (fx= r last-row) (fx= off last-off)))
          (make-pos r off)
          (if (fx= off off-max)
              (loop (fx- n 1) (fx+ r 1) 0 (fx- (lns->nsr lns (fx+ r 1)) 1))
              (loop (fx- n 1) r (fx+ off 1) off-max))))))

; NB.  the macos x terminal app distinguishes between empty screen
; positions (e.g., after clr_eos or clr_eol) and screen positions filled
; with spaces.  attempts to move past and clear after the former result
; in strange behavior.  (For example, the sequence clr_eos, cursor_right,
; clr_eol, 'a', clr_eol, and 'b' doesn't print the b but does cause the
; terminal to send back some characters.  Using ' ' in place of the
; cursor_right works as expected.)  For this reason, we display spaces
; and avoid using move-cursor-right to pad the front of each row after
; the first, which gets the actual prompt.

(define (phantom-prompt ee)
  (make-string (string-length (eestate-prompt ee)) #\space))

(define (ee-display-string/colors str colors current-color)
  (cond
    [(current-expeditor-color-enabled)
     (let loop ([i 0] [current-color current-color])
       (cond
         [(= i (string-length str))
          current-color]
         [(eqv? current-color (vector-ref colors i))
          (define j (let loop ([j (add1 i)])
                      (cond
                        [(= j (string-length str)) j]
                        [(eqv? current-color (vector-ref colors j)) (loop (add1 j))]
                        [else j])))
          (ee-display-string (substring str i j))
          (loop j current-color)]
         [else
          (define color (vector-ref colors i))
          (set-fg-color color)
          (loop i color)]))]
    [else
     (ee-display-string str)
     current-color]))

(define (finish-color current-color)
  (unless (eqv? current-color default-color)
    (set-fg-color default-color)))

(define-public (display-rest/goto)
  (define (display-rest-of-line ee entry row col pcol clear?)
    ; display as much of the rest of row as will fit on the screen
    (let ([lns (entry-lns entry)] [bot-line (entry-bot-line entry)])
      ; n = number of lines to display beyond the first
      (define row-str (lns->str lns row))
      (define strs (split-string ee row-str col pcol))
      (let loop ([n (fx- (if (fx= row (pos-row bot-line))
                             (pos-col bot-line)
                             (fx- (lns->nsr lns row) 1))
                         (col->line-offset ee entry row col))]
                 [str-lst strs]
                 [colors-lst (split-vector-like ee (lns->colors lns row) col strs)]
                 [current-color default-color]
                 [new-col col]
                 [new-pcol pcol])
        (when clear? (clear-eol))
        (let ([str (car str-lst)]
              [colors (car colors-lst)])
          (let ([current-color (ee-display-string/colors str colors current-color)])
            (let ([new-col (fx+ new-col (string-length str))]
                  [new-pcol (fx+ new-pcol (string-width str))])
              (if (fx= n 0)
                  (begin
                    (finish-color current-color)
                    (values new-col new-pcol))
                  (begin
                    (carriage-return)
                    (line-feed)
                    (loop (fx- n 1) (cdr str-lst) (cdr colors-lst) current-color new-col new-pcol)))))))))
  
  (define (display-rest-of-entry ee entry)
    (let ([row (entry-row entry)]
          [col (entry-col entry)]
          [bot-row (pos-row (entry-bot-line entry))])
      (define pcol (col->screen-col ee entry row col))
      (let loop ([new-row row] [start-col col] [start-pcol pcol])
        (let-values ([(new-col new-pcol) (display-rest-of-line ee entry new-row start-col start-pcol #f)])
          (if (fx= new-row bot-row)
              (values new-row new-col new-pcol)
              (begin
                (carriage-return)
                (line-feed)
                (ee-display-string (phantom-prompt ee))
                (loop (fx+ new-row 1) 0 0)))))))
  
  (define (display-rest/goto ee entry just-row? clear? to-row to-col)
    ; display rest of entry and go directly from there to (to-row, to-col)
    ; just-row? => only remainder of current logical row needed by displayed
    ; clear? => clear-eos or clear-eol needed
    (let-values ([(cur-row cur-col cur-pcol)
                  (if just-row?
                      (let-values ([(col pcol) (display-rest-of-line
                                                ee entry
                                                (entry-row entry) (entry-col entry)
                                                (col->screen-col ee entry (entry-row entry) (entry-col entry))
                                                clear?)])
                        (values (entry-row entry) col pcol))
                      (begin
                        (set-entry-bot-line! entry
                                             (calc-bot-line-displayed entry
                                                                      (entry-top-line entry)))
                        (when clear? (clear-eos))
                        (display-rest-of-entry ee entry)))])
      (unless (and (fx= cur-row (entry-row entry))
                   (fx= cur-col (entry-col entry)))
        (set-entry-row! entry cur-row)
        ; if the last character written was in the last column of a screen
        ; line, move back one so that the cursor is pointing at that character
        ; to avoid returning a column value that would wrongly indicate that
        ; the cursor is at the start of the next screen line
        (cond
          [(and (fx> cur-col 0) (fx= cur-pcol 0))
           (define adj-pcol (col->screen-col ee entry cur-row (fx- cur-col 1)))
           (move-cursor-left (- cur-pcol adj-pcol))
           (set-entry-col! entry (fx- cur-col 1))]
          [else
           (set-entry-col! entry cur-col)]))
      (goto ee entry (make-pos to-row to-col)))))

(define-public (display-partial-entry)
  (define (display-partial-row ee row str colors start end)
    ; displays physical lines of str from start (inclusive) to end (inclusive)
    ; assumes cursor is at column zero of start line; leaves cursor at
    ; column zero of end line
    (let* ([ls-all (split-string ee str 0 (prompt-width ee))]
           [ls (list-tail ls-all start)]
           [cs (list-tail (split-vector-like ee colors 0 ls-all) start)])
      (when (fx= start 0)
        (ee-display-string
         (if (fx= row 0)
             (eestate-prompt ee)
             (phantom-prompt ee))))
      (define current-color (ee-display-string/colors (car ls) (car cs) default-color))
      (carriage-return)
      (let loop ([i start] [ls (cdr ls)] [cs (cdr cs)] [current-color current-color])
        (cond
          [(fx= i end) (finish-color current-color)]
          [else
           (line-feed)
           (define next-color (ee-display-string/colors (car ls) (car cs) current-color))
           (carriage-return)
           (loop (fx+ i 1) (cdr ls) (cdr cs) next-color)]))))

  (define (display-partial-entry ee entry toprow topoff botrow botoff)
    ; displays physical screen lines between physical line topoff of
    ; toprow (inclusive) and botoff of botrow (inclusive)
    ; assumes cursor is at column zero of first physical line to be displayed;
    ; leaves cursor at column zero of last line displayed
    (let ([lns (entry-lns entry)])
      (let loop ([r toprow] [start topoff] [lns (list-tail lns toprow)])
        (display-partial-row ee r (ln-str (car lns)) (ln-colors (car lns)) start
                             (if (fx= r botrow) botoff (fx- (ln-nsr (car lns)) 1)))
        (unless (fx= r botrow)
          (line-feed)
          (loop (fx+ r 1) 0 (cdr lns)))))))

(define (goto-backward ee entry new-row new-col)
  (assert* (fx>= new-row 0)
           (fx>= new-col 0)
           (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
  (let* ([lns (entry-lns entry)]
         [row (entry-row entry)]
         [col (entry-col entry)]
         [top-line (entry-top-line entry)]
         [new-str (lns->str lns new-row)]
         [new-len (string-length new-str)]
         [new-row-offset (col->line-offset ee entry new-row new-col)]
         [new-row-pos (make-pos new-row new-row-offset)]
         [new-bot-line (calc-bot-line-displayed entry new-row-pos)])
    (cond
      ; case 1: destination on screen, no scrolling necessary
      ;  ------------------
      ; | (define fact     | <--top-line
      ; |   (lambda (n)    | <--new-row
      ; |     (if (zero? n)| <--point
      ; |         1        |
      ; |         (* n (fac|
      ; |t (sub1 n))))))   | <--bot-line
      ; |                  |
      ;  ------------------
      [(pos>=? new-row-pos (entry-top-line entry))
       (move-cursor-up
        (screen-lines-between ee entry
                              new-row new-row-offset
                              (entry-row entry)
                              (col->line-offset ee entry (entry-row entry) (entry-col entry))))
       (let ([screen-col (col->screen-col ee entry row col)]
             [new-screen-col (col->screen-col ee entry new-row new-col)])
         (cond
           [(fx> new-screen-col screen-col)
            (move-cursor-right (fx- new-screen-col screen-col))]
           [(fx< new-screen-col screen-col)
            (move-cursor-left (fx- screen-col new-screen-col))]))
       (set-entry-row! entry new-row)
       (set-entry-col! entry new-col)]

      ; case 2: a portion of the old screen overlaps the new screen.
      ;         we will scroll down and keep the overlap instead of
      ;         redrawing
      ;         + = new screen border
      ;         - = old-screen border
      ;  ++++++++++++++++++
      ; | (define f        | <--new-row        0         }extra-top-
      ; |   (lambda (n)    |                             }lines
      ;  ------------------
      ; |     (if (zero? n)| <--top-line       (2 . 0)
      ; |         1        | <--point          (row . col)
      ; |         (* n     | <--new-bot-line   (4 . 0)
      ;  ++++++++++++++++++
      ; |            (f    | <--bot-line       (5 . 0)
      ; |              (1- |
      ;  ------------------
      ;                  n))))))
      [(pos>? new-bot-line (entry-top-line entry))
       ; move cursor to physical column 0 of top screen line
       (move-cursor-up
        (screen-lines-between ee entry
                              (pos-row top-line) (pos-col top-line)
                              row (col->line-offset ee entry row col)))
       (carriage-return)
       (let ([extra-top-lines
              (screen-lines-between ee entry
                                    new-row new-row-offset
                                    (pos-row top-line) (pos-col top-line))])
         ; reverse scroll to open up space at the top
         ; if we're not actually at the top of the physical display, e.g.,
         ; if we only partially displayed the entry after an error or tab-tab,
         ; we hope that this goes up a line and clears to end of line.  if
         ; this ever gives us problems, we'll have avoid getting into this
         ; case when less than a screenful of lines has been displayed.
         (scroll-reverse extra-top-lines)
         ; display the extra lines
         (let ([r (pos-row top-line)] [off (fx- (pos-col top-line) 1)])
           (if (fx>= off 0)
               (display-partial-entry ee entry new-row new-row-offset r off)
               (display-partial-entry ee entry new-row new-row-offset
                                      (fx- r 1) (fx- (lns->nsr lns (fx- r 1)) 1))))
         ; move cursor back to top
         (move-cursor-up (fx- extra-top-lines 1)))
       (move-cursor-right (col->screen-col ee entry new-row new-col))
       (set-entry-col! entry new-col)
       (set-entry-row! entry new-row)
       (set-entry-top-line! entry new-row-pos)
       (when (pos<? new-bot-line (entry-bot-line entry))
         (set-entry-bot-line! entry new-bot-line))]

      ; case 3: no overlap between old screen area and new screen
      ;         area. we will redraw the entire screen
      ; + = new screen border
      ; - = old-screen border
      ;  ++++++++++++++++++
      ; | (define f        | <--new-row        0
      ; |   (lambda (n)    |
      ; |     (if (zero? n)| <--new-bot-line   (4 . 0)
      ;  ++++++++++++++++++
      ;  ------------------
      ; |         1        | <--top-line       (2 . 0)
      ; |         (* n     |
      ; |            (f    | <--bot-line       (4 . 1)
      ;  ------------------
      ;                (1-
      ;                  n))))))
      [else
       (clear-screen)
       (display-partial-entry ee entry
                              new-row new-row-offset
                              (pos-row new-bot-line) (pos-col new-bot-line))
       (move-cursor-up
        (screen-lines-between ee entry
                              new-row new-row-offset
                              (pos-row new-bot-line) (pos-col new-bot-line)))
       (move-cursor-right (col->screen-col ee entry new-row new-col))
       (set-entry-col! entry new-col)
       (set-entry-row! entry new-row)
       (set-entry-top-line! entry new-row-pos)
       (set-entry-bot-line! entry new-bot-line)])))

(define (goto-forward ee entry new-row new-col)
  (assert* (fx< new-row (length (entry-lns entry)))
           (fx>= new-col 0)
           (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
  (let* ([lns (entry-lns entry)]
         [row (entry-row entry)]
         [col (entry-col entry)]
         [bot-line (entry-bot-line entry)]
         [new-str (lns->str lns new-row)]
         [new-len (string-length new-str)]
         [new-row-offset (col->line-offset ee entry new-row new-col)]
         [new-row-pos (make-pos new-row new-row-offset)]
         [new-top-line (calc-top-line-displayed entry new-row-pos)])
    (cond
      ; case 1: destination on screen, no scrolling necessary
      ;  ------------------
      ; | (define fact     | <--top-line
      ; |   (lambda (n)    | <--point
      ; |     (if (zero? n)| <--new-row
      ; |         1        |
      ; |         (* n (fac|
      ; |t (sub1 n))))))   | <--bot-line
      ; |                  |
      ;  ------------------
      [(pos<=? new-row-pos bot-line)
       (move-cursor-down
        (screen-lines-between ee entry
                              row (col->line-offset ee entry row col)
                              new-row new-row-offset))
       (let ([screen-col (col->screen-col ee entry row col)]
             [new-screen-col (col->screen-col ee entry new-row new-col)])
         (cond
           [(fx> new-screen-col screen-col)
            (move-cursor-right (fx- new-screen-col screen-col))]
           [(fx< new-screen-col screen-col)
            (move-cursor-left (fx- screen-col new-screen-col))]))
       (set-entry-row! entry new-row)
       (set-entry-col! entry new-col)]

      ; case 2: a portion of the old screen overlaps the new screen.
      ;         we will scroll up and keep the overlap
      ;
      ;         + = new screen border
      ;         - = old-screen border
      ;  ------------------
      ; | (define f        | <--top-line       (0 . 0)
      ; |   (lambda (n)    |
      ;  ++++++++++++++++++
      ; |     (if (zero? n)| <--new-top-line               } scrn-
      ; |         1        | <--point          (row . col) } draw-
      ; |         (* n     | <--bot-line       (4 . 0)     } lines
      ;  ------------------
      ; |            (f    |
      ; |              (1- | <--new-row        6
      ;  ++++++++++++++++++
      ;                  n))))))
      [(pos>=? bot-line new-top-line)
       ; move cursor to physical col 0 of first line after old bot-line
       (move-cursor-down
        (screen-lines-between ee entry
                              row (col->line-offset ee entry row col)
                              (pos-row bot-line) (pos-col bot-line)))
       (carriage-return)
       (line-feed)
       (let ([r (pos-row bot-line)] [off (fx+ (pos-col bot-line) 1)])
         (if (fx< off (lns->nsr lns r))
             (display-partial-entry ee entry r off
                                    new-row new-row-offset)
             (display-partial-entry ee entry (fx+ r 1) 0
                                    new-row new-row-offset)))
       (move-cursor-right (col->screen-col ee entry new-row new-col))
       (set-entry-col! entry new-col)
       (set-entry-row! entry new-row)
       (when (pos>? new-top-line (entry-top-line entry))
         (set-entry-top-line! entry new-top-line))
       (set-entry-bot-line! entry new-row-pos)]

      ; case 3: no overlap between old screen area and new screen
      ;         area. we will redraw the entire screen
      ; + = new screen border
      ; - = old-screen border
      ;  ++++++++++++++++++
      ; | (define f        | <--top-line       (0 . 0)
      ; |   (lambda (n)    |
      ; |     (if (zero? n)| <--bot-line       (2 . 0)
      ;  ++++++++++++++++++
      ;  ------------------
      ; |         1        | <--new-top-line
      ; |         (* n     |
      ; |            (f    | <--new-row, new-row-offset
      ;  ------------------
      ;                (1-
      ;                  n))))))
      [else
       (clear-screen)
       (display-partial-entry ee entry
                              (pos-row new-top-line) (pos-col new-top-line)
                              new-row new-row-offset)
       (move-cursor-right (col->screen-col ee entry new-row new-col))
       (set-entry-col! entry new-col)
       (set-entry-row! entry new-row)
       (set-entry-top-line! entry new-top-line)
       (set-entry-bot-line! entry new-row-pos)])))

(define (goto ee entry p)
  (let ([new-row (pos-row p)] [new-col (pos-col p)])
    (assert* (fx< new-row (length (entry-lns entry)))
             (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
    (if (or (fx< new-row (entry-row entry))
            (and (fx= new-row (entry-row entry))
                 (fx< new-col (entry-col entry))))
        (goto-backward ee entry new-row new-col)
        (goto-forward ee entry new-row new-col))))

(define (move-up ee entry [n 1])
  (assert* (fx>= (fx- (entry-row entry) n) 0))
  (let ([new-row (fx- (entry-row entry) n)])
    (goto-backward ee entry new-row
                   (fxmin (entry-col entry)
                          (string-length (lns->str (entry-lns entry) new-row))))))

(define (move-down ee entry [n 1])
  (assert* (fx< (fx+ (entry-row entry) n) (length (entry-lns entry))))
  (let ([new-row (fx+ (entry-row entry) n)])
    (goto-forward ee entry new-row
                  (fxmin (entry-col entry)
                         (string-length (lns->str (entry-lns entry) new-row))))))

(define (move-left ee entry [n 1])
  (let ([new-col (fx- (entry-col entry) n)])
    (assert* (fx>= new-col 0))
    (goto-backward ee entry (entry-row entry) new-col)))

(define (move-right ee entry [n 1])
  (let ([new-col (fx+ (entry-col entry) n)])
    (assert* (fx<= new-col (string-length (lns->str (entry-lns entry) (entry-row entry)))))
    (goto-forward ee entry (entry-row entry) new-col)))

(define (move-forward ee entry n)
  (define dest
    (let loop ([n n] [row (entry-row entry)] [col (entry-col entry)])
      (cond
        [(zero? n) (make-pos row col)]
        [else
         (define len (- (string-length (lns->str (entry-lns entry) row))
                        col))
         (cond
           [(n . > . len)
            (loop (- n len 1) (add1 row) 0)]
           [else
            (make-pos row (+ col n))])])))
  (move-down ee entry (- (pos-row dest) (entry-row entry)))
  (define right (- (pos-col dest) (entry-col entry)))
  (if (right . < . 0)
      (move-left ee entry (- right))
      (move-right ee entry right)))

(define (page-down ee entry)
  (let* ([last-row (fx- (length (entry-lns entry)) 1)]
         [row (entry-row entry)]
         [col (entry-col entry)]
         [point-line-offset (col->line-offset ee entry row col)]
         [top-line (entry-top-line entry)]
         [bot-line (entry-bot-line entry)]
         [n (screen-lines-between ee entry
                                  (pos-row top-line) (pos-col top-line)
                                  (pos-row bot-line) (pos-col bot-line))])
    (let f ([r (fxmin (fx+ row n) last-row)])
      (if (fx= r row)
          (unless (fx= r last-row)
            (goto-forward ee entry (fx+ r 1)
                          (fxmin col (string-length (lns->str (entry-lns entry) (fx+ r 1))))))
          (let ([c (fxmin col (string-length (lns->str (entry-lns entry) r)))])
            (if (<= (screen-lines-between ee entry
                                          row point-line-offset
                                          r (col->line-offset ee entry r c))
                    n)
                (goto-forward ee entry r c)
                (f (fx- r 1))))))))

(define (page-up ee entry)
  (let* ([row (entry-row entry)]
         [col (entry-col entry)]
         [point-line-offset (col->line-offset ee entry row col)]
         [top-line (entry-top-line entry)]
         [bot-line (entry-bot-line entry)]
         [n (screen-lines-between ee entry
                                  (pos-row top-line) (pos-col top-line)
                                  (pos-row bot-line) (pos-col bot-line))])
    (let f ([r (max (fx- row n) 0)])
      (if (fx= r row)
          (unless (fx= r 0)
            (goto-backward ee entry (fx- r 1)
                           (fxmin col (string-length (lns->str (entry-lns entry) (fx- r 1))))))
          (let ([c (fxmin col (string-length (lns->str (entry-lns entry) r)))])
            (if (<= (screen-lines-between ee entry
                                          r (col->line-offset ee entry r c)
                                          row point-line-offset)
                    n)
                (goto-backward ee entry r c)
                (f (fx+ r 1))))))))

(define (move-eol ee entry)
  (move-right ee entry
              (fx- (string-length (lns->str (entry-lns entry) (entry-row entry)))
                   (entry-col entry))))

(define (move-bol ee entry)
  (move-left ee entry (entry-col entry)))

(define (move-eoe ee entry)
  (let ([lns (entry-lns entry)])
    (let ([r (fx- (length lns) 1)])
      (goto-forward ee entry r (string-length (lns->str lns r))))))

(define (move-to-col-pos ee entry new-col)
  (let ([col (entry-col entry)])
    (if (fx< new-col col)
        (move-left ee entry (fx- col new-col))
        (move-right ee entry (fx- new-col col)))))

(define (adjust-mark/delete ee entry r1 c1 r2 c2)
  (let ([mark (entry-mark entry)])
    (when mark
      (let ([mrow (pos-row mark)] [mcol (pos-col mark)])
        (unless (or (fx< mrow r1) (and (fx= mrow r1) (fx< mcol c1)))
          (set-entry-mark! entry
                           (and (not (or (fx< mrow r2) (and (fx= mrow r2) (fx< mcol c2))))
                                (make-pos
                                 (fx- mrow (fx- r2 r1))
                                 (if (fx= mrow r2) (fx+ c1 (fx- mcol c2)) mcol)))))))))

(define (adjust-mark/insert ee entry r1 c1 r2 c2)
  (let ([mark (entry-mark entry)])
    (when mark
      (let ([mrow (pos-row mark)] [mcol (pos-col mark)])
        (unless (or (fx< mrow r1) (and (fx= mrow r1) (fx< mcol c1)))
          (set-entry-mark! entry
                           (make-pos
                            (fx+ mrow (fx- r2 r1))
                            (if (fx= mrow r1) (fx+ c2 (fx- mcol c1)) mcol))))))))

(define (delete-forward ee entry r2 c2)
  ; deletes from point, aka r1, c1 (inclusive) to r2, c2 (exclusive)
  ; and returns the deleted content as a list of strings
  (let ([r1 (entry-row entry)] [c1 (entry-col entry)])
    (assert* (or (fx< r1 r2) (and (fx= r1 r2) (fx<= c1 c2))))
    (adjust-mark/delete ee entry r1 c1 r2 c2)
    (if (fx= r1 r2)
        (let* ([ln (list-ref (entry-lns entry) r1)]
               [s (ln-str ln)]
               [colors (ln-colors ln)]
               [old-nsr (ln-nsr ln)])
          (ln-str-set! ee ln
                       (string-append
                        (substring s 0 c1)
                        (substring s c2 (string-length s)))
                       (vector-append
                        (vector-copy colors 0 c1)
                        (vector-copy colors c2 (string-length s))))
          (display-rest/goto ee entry (fx= (ln-nsr ln) old-nsr) #t r1 c1)
          (list (substring s c1 c2)))
        (let* ([lns (entry-lns entry)]
               [ls1 (list-tail lns r1)]
               [ls2 (list-tail ls1 (fx- r2 r1))]
               [s1 (ln-str (car ls1))]
               [s2 (ln-str (car ls2))]
               [colors1 (ln-colors (car ls1))]
               [colors2 (ln-colors (car ls2))])
          (ln-str-set! ee (car ls1)
                       (string-append
                        (substring s1 0 c1)
                        (substring s2 c2 (string-length s2)))
                       (vector-append
                        (vector-copy colors1 0 c1)
                        (vector-copy colors2 c2 (string-length s2))))
          (let ([deleted
                 (cons (substring s1 c1 (string-length s1))
                       (let f ([ls (cdr ls1)])
                         (if (eq? ls ls2)
                             (list (substring s2 0 c2))
                             (cons (ln-str (car ls)) (f (cdr ls))))))])
            (set-entry-lns! entry (replace-cdr lns ls1 (cdr ls2)))
            (display-rest/goto ee entry #f #t r1 c1)
            deleted)))))

(define (update-entry-colors ee entry new-colors)
  (define init-cursor-row (entry-row entry))
  (define init-cursor-col (entry-col entry))
  (let loop ([lns (entry-lns entry)]
             [i 0]
             [row 0]
             [current-color default-color])
    (cond
      [(null? lns)
       (finish-color current-color)
       (goto ee entry (make-pos init-cursor-row init-cursor-col))]
      [else
       (define str (ln-str (car lns)))
       (define colors (ln-colors (car lns)))
       (define len (string-length str))
       (define-values (next-color pcol)
         (for/fold ([current-color current-color] [pcol #f]) ([j (in-range len)])
           (define new-color (vector-ref new-colors (+ i j)))
           (cond
             [(= new-color (vector-ref colors j))
              (values current-color #f)]
             [else
              (vector-set! colors j new-color)
              (goto ee entry (make-pos row j))
              (unless (eqv? current-color new-color)
                (set-fg-color new-color))
              (define w (ee-write-char (string-ref str j)))
              (define new-pcol
                (if pcol
                    (+ pcol w)
                    (+ (col->screen-col ee entry row j) w)))
              (cond
                [(new-pcol . fx>= . (entry-screen-cols entry))
                 ;; avoid end-of-line edge case by moving to start of character
                 (move-cursor-left w)
                 (values new-color #f)]
                [else
                 (set-entry-col! entry (add1 (entry-col entry)))
                 (values new-color new-pcol)])])))
       (loop (cdr lns) (+ i len 1) (add1 row) next-color)])))

(define (replace-cdr l lh lt)
  (if (eq? l lh)
      (cons (car lh) lt)
      (cons (car l) (replace-cdr (cdr l) lh lt))))

(define (delete-char ee entry)
  (assert* (not (end-of-line? ee entry)))
  (let ([row (entry-row entry)] [col (entry-col entry)])
    (delete-forward ee entry row (fx+ col 1))))

(define (delete-to-eol ee entry)
  (let ([row (entry-row entry)])
    (delete-forward ee entry row
                    (string-length (lns->str (entry-lns entry) row)))))

(define (join-rows ee entry)
  (assert* (end-of-line? ee entry) (not (last-line? ee entry)))
  (delete-forward ee entry (fx+ (entry-row entry) 1) 0))

(define (insert-string-before ee entry new-str)
  (let* ([row (entry-row entry)]
         [col (entry-col entry)]
         [lns (entry-lns entry)]
         [ln (list-ref lns row)]
         [str (ln-str ln)]
         [str-len (string-length str)]
         [colors (ln-colors ln)]
         [new-col (fx+ col (string-length new-str))]
         [nsr (ln-nsr ln)]
         [eoe? (end-of-entry? ee entry)])
    ;; beware of inserting just before a multicell character that has
    ;; moved to the next line leaving a blank cell at the end of a
    ;; row, in which case inserting could shift the logical->physical
    ;; calculation for the point
    (let ([pcol (col->screen-col ee entry row col)])
      (when (and (col . fx> . 0) (fx= 0 pcol))
        (move-left ee entry 1)
        (set-entry-col! entry (fx- col 1))))
    (ln-str-set! ee ln
                 (string-append
                  (substring str 0 col)
                  new-str
                  (substring str col (string-length str)))
                 (vector-append
                  (vector-copy colors 0 col)
                  (str->colors new-str (cond
                                          [(col . > . 0)
                                           (vector-ref colors (sub1 col))]
                                          [(col . < . (string-length str))
                                           (vector-ref colors col)]
                                          [else
                                           default-color]))
                  (vector-copy colors col (string-length str))))
    (let ([just-row? (fx= (ln-nsr ln) nsr)])
      (display-rest/goto ee entry just-row?
                         ; avoid clear-eol/eos if insertion takes place at end of entry or
                         ; if rewriting just the current row
                         (and (not eoe?) (not just-row?))
                         row new-col))
    (adjust-mark/insert ee entry row col row new-col)))

(define (add-char ee entry c)
  ; add character after point, then move point forward one character
  (assert* (char? c))
  (insert-string-before ee entry (string c)))

(define (insert-strings-before ee entry strs)
  (unless (null? strs)
    (if (fx= (length strs) 1)
        (insert-string-before ee entry (car strs))
        (let* ([row (entry-row entry)]
               [col (entry-col entry)]
               [lns (entry-lns entry)]
               [ls (list-tail lns row)]
               [ln (car ls)]
               [point-str (ln-str ln)]
               [point-colors (ln-colors ln)]
               [eoe? (end-of-entry? ee entry)])
          (ln-str-set! ee ln
                       (string-append (substring point-str 0 col) (car strs))
                       (vector-append (vector-copy point-colors 0 col)
                                      (str->colors (car strs) default-color)))
          (define new-cdr
            (let f ([str (cadr strs)] [strs (cddr strs)])
              (if (null? strs)
                  (cons (make-ln ee
                                 (string-append str
                                                (substring point-str col
                                                           (string-length point-str))))
                        (cdr ls))
                  (cons (make-ln ee str)
                        (f (car strs) (cdr strs))))))
          (set-entry-lns! entry (replace-cdr lns ls new-cdr))
          (let* ([n (fx- (length strs) 1)]
                 [new-row (fx+ row n)]
                 [new-col (string-length (list-ref strs n))])
            (display-rest/goto ee entry #f (not eoe?) new-row new-col)
            (adjust-mark/insert ee entry row col new-row new-col))))))

(define (first-line? ee entry) (fxzero? (entry-row entry)))

(define (last-line? ee entry)
  (fx= (entry-row entry) (fx1- (length (entry-lns entry)))))

(define (last-line-displayed? ee entry)
  (define row (entry-row entry))
  (pos=? (make-pos row (col->line-offset ee entry row (entry-col entry)))
         (entry-bot-line entry)))

(define (visible? ee entry row col)
  (let ([line (make-pos row (col->line-offset ee entry row col))])
    (and (pos<=? (entry-top-line entry) line)
         (pos<=? line (entry-bot-line entry)))))

(define (end-of-line? ee entry)
  (fx= (entry-col entry)
       (string-length (lns->str (entry-lns entry) (entry-row entry)))))

(define (end-of-entry? ee entry)
  (and (fx= (entry-row entry) (fx- (length (entry-lns entry)) 1))
       (end-of-line? ee entry)))

(define (beginning-of-line? ee entry) (fx= (entry-col entry) 0))

; returns #t iff only spaces and newlines are left after point
(define (only-whitespace-left? ee entry)
  (let f ([ls (list-tail (entry-lns entry) (entry-row entry))]
          [col (entry-col entry)])
    (or (null? ls)
        (let* ([s (ln-str (car ls))] [n (string-length s)])
          (let g ([col col])
            (if (fx= col n)
                (f (cdr ls) 0)
                (and (char=? (string-ref s col) #\space)
                     (g (fx+ col 1)))))))))

(define (handle-winch ee entry)
  (screen-resize!)
  (unless (and (fx= (screen-rows) (entry-screen-rows entry))
               (fx= (screen-cols) (entry-screen-cols entry)))
    (clear-entry ee entry)
    (redisplay ee entry)))

(define-public (redisplay)
  (define (set-screen-size! ee entry)
    (screen-resize!)
    (unless (and (fx= (entry-screen-cols entry) (screen-cols))
                 (fx= (entry-screen-rows entry) (screen-rows)))
      (for-each
       (lambda (ln) (ln-nsr-set! ln (str->nsr ee (ln-str ln))))
       (entry-lns entry))
      (set-entry-screen-cols! entry (screen-cols))
      (set-entry-screen-rows! entry (screen-rows))))

  (define (redisplay ee entry [nrows #f])
    (set-screen-size! ee entry)
    (let* ([nrows (or nrows (screen-rows))] ; want new screen-rows
           [row (entry-row entry)]
           [col (entry-col entry)]
           [point-line (make-pos row (col->line-offset ee entry row col))])
      (set-entry-bot-line! entry
                           (calc-bot-line-displayed entry (entry-top-line entry) nrows))
      (when (pos>? point-line (entry-bot-line entry))
        (set-entry-bot-line! entry point-line))
      (set-entry-top-line! entry
                           (calc-top-line-displayed entry (entry-bot-line entry) nrows))
      (when (pos<? point-line (entry-top-line entry))
        (set-entry-top-line! entry point-line)
        (set-entry-bot-line! entry
                             (calc-bot-line-displayed entry (entry-top-line entry) nrows)))
      (let ([top-line (entry-top-line entry)] [bot-line (entry-bot-line entry)])
        (display-partial-entry ee entry
                               (pos-row top-line) (pos-col top-line)
                               (pos-row bot-line) (pos-col bot-line))
        (move-cursor-up
         (screen-lines-between ee entry
                               (pos-row point-line) (pos-col point-line)
                               (pos-row bot-line) (pos-col bot-line)))
        (move-cursor-right (col->screen-col ee entry row col))))))

(define (flash ee entry mpos)
  (let ([point-pos (entry-point entry)])
    (cond
      [(visible? ee entry (pos-row mpos) (pos-col mpos))
       (goto ee entry mpos)
       (ee-flush)
       (wait (ee-paren-flash-delay))
       (goto ee entry point-pos)]
      [(pos<? mpos point-pos)
       (let ([nlines
              (screen-lines-between ee entry
                                    (pos-row (entry-top-line entry))
                                    (pos-col (entry-top-line entry))
                                    (pos-row point-pos)
                                    (col->line-offset ee entry (pos-row point-pos) (pos-col point-pos)))]
             [ncols (col->screen-col ee entry (pos-row point-pos) (pos-col point-pos))])
         (move-cursor-left ncols)
         (move-cursor-up nlines)
         (ee-flush)
         (wait (ee-paren-flash-delay))
         (move-cursor-down nlines)
         (move-cursor-right ncols))]
      [else
       (let ([nlines
              (screen-lines-between ee entry
                                    (pos-row point-pos)
                                    (col->line-offset ee entry (pos-row point-pos) (pos-col point-pos))
                                    (pos-row (entry-bot-line entry))
                                    (pos-col (entry-top-line entry)))]
             [ncols (col->screen-col ee entry (pos-row point-pos) (pos-col point-pos))])
         (move-cursor-left ncols)
         (move-cursor-down nlines)
         (ee-flush)
         (wait (ee-paren-flash-delay))
         (move-cursor-up nlines)
         (move-cursor-right ncols))])))

(define (correct&flash-matching-delimiter ee entry on-correct)
  (define (expected left)
    (for/or ([p (in-list (current-expeditor-parentheses))])
      (define l (symbol->immutable-string (car p)))
      (and (= (string-length l) 1)
           (eqv? left (string-ref l 0))
           (let ([r (symbol->immutable-string (cadr p))])
             (and (= (string-length r) 1)
                  (string-ref r 0))))))
  (move-left ee entry 1) ; move over delim
  (let ([lns (entry-lns entry)])
    (let* ([row (entry-row entry)]
           [col (entry-col entry)]
           [str (lns->str lns row)]
           [colors (lns->colors lns row)]
           [c (string-ref str col)])
      (cond
        [(and (for/or ([p (in-list (current-expeditor-parentheses))])
                (define l (symbol->immutable-string (car p)))
                (define r (symbol->immutable-string (cadr p)))
                (and (= (string-length l) 1)
                     (= (string-length r) 1)
                     (eqv? c (string-ref r 0))))
              (find-matching-delim-backward ee entry row col
                                            (ee-auto-paren-balance)))
         => (lambda (mpos)
              (define ln (list-ref lns (pos-row mpos)))
              (let ([cexp (expected (string-ref (ln-str ln) (pos-col mpos)))])
                (unless (or (not cexp) (eqv? c cexp))
                  (string-set! str col cexp)
                  (define color (vector-ref colors col))
                  (unless (eqv? color default-color)
                    (set-fg-color color))
                  (ee-write-char cexp)
                  (unless (eqv? color default-color)
                    (set-fg-color default-color))
                  (move-cursor-left 1)
                  (on-correct)))
              (when (ee-flash-parens) (flash ee entry mpos)))]
        [else
         (when (ee-flash-parens)
           (cond
             [(find-matching-delim-forward ee entry row col #f)
              => (lambda (mpos) (flash ee entry mpos))]))])))
  (move-right ee entry 1))

(define (find-matching-delimiter ee entry)
  (let ([row (entry-row entry)]
        [col (entry-col entry)]
        [str (lns->str (entry-lns entry) (entry-row entry))])
    (and (fx< col (string-length str))
         (let ([c (string-ref str col)])
           (if (or (char=? c lpchar) (char=? c lbchar))
               (find-matching-delim-forward ee entry row col #f)
               (and (or (char=? c rpchar) (char=? c rbchar))
                    (find-matching-delim-backward ee entry row col #f)))))))

(define (find-matching-delim-backward ee entry row col lax?)
  ; 1. create string representing current entry through row, col
  ; 2. search forward, stacking left/right delimiters and their indices
  ; 3. if matching delimiter found, convert string index to pos
  (let* ([s (entry->string entry #:up-to-row row #:up-to-col (fx+ col 1))]
         [ip (open-input-string/count s)])
    (let loop ([stack '()] [state #f])
      (with-handlers* ([exn:fail? (lambda (exn) (loop '() state))])
        (let-values ([(type value start end new-state) (read-token ip state)])
          (case type
            [(eof fasl) #f]
            [(opener)
             (loop (cons (cons (opener->closer value) end) stack) new-state)]
            [(closer)
             (if (= end (string-length s))
                 (and (not (null? stack))
                      (or lax? (eq? (caar stack) value))
                      (index->pos s (fx- (cdar stack) 1) 0 0))
                 (if (and (not (null? stack)) (eq? (caar stack) value))
                     (loop (cdr stack) new-state)
                     (loop '() new-state)))]
            [else (loop stack new-state)]))))))

(define (find-matching-delim-forward ee entry row col lax?)
  (let ([lns (entry-lns entry)])
    ; should be sitting on left paren or bracket...
    ; 1. create string representing current entry starting at col, row
    ; 2. search forward until matching delimiter, eof, or error
    ; 3. if matching delimiter found, convert string index to pos
    (let* ([s (entry->string entry #:from-row row #:from-col col)]
           [ip (open-input-string/count s)])
      (with-handlers ([exn:fail? (lambda (exn) #f)])
        (let loop ([stack '()] [state #f])
          (let-values ([(type value start end new-state) (read-token ip state)])
            (case type
              [(eof fasl) #f]
              [(opener)
               (loop (cons (opener->closer value) stack) new-state)]
              [(closer)
               (if (fx= (length stack) 1)
                   (and (or lax? (eq? (car stack) value))
                        (index->pos s start row col))
                   (and (eq? (car stack) value) (loop (cdr stack) new-state)))]
              [else (loop stack new-state)])))))))

(define (reader-prefix? type value)
  (and (memq type '(constant other))
       (member value '("'" "`" "," ",@" "#'" "#`" "#," "#,@"))))

(define (find-next-exp-backward ee entry row col)
  (define-values (obj offset offset->pos) (editor-object entry row col))
  (define new-offset ((current-expeditor-grouper) obj offset 0 'backward))
  (cond
    [(eq? new-offset #t)
     (let* ([s (entry->string entry #:up-to-row row #:up-to-col col)]
            [ip (open-input-string/count s)])
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         (let loop ([stack '()] [last-start 0] [state #f])
           (let-values ([(type value start end new-state) (read-token ip state)])
             (case type
               [(eof) (and last-start (index->pos s last-start 0 0))]
               [(opener)
                (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                    (loop (cons (cons (opener->closer value) (cdar stack)) (cdr stack)) #f new-state)
                    (loop (cons (cons (opener->closer value) start) stack) #f new-state))]
               [(closer)
                (if (and (not (null? stack)) (eq? (caar stack) value))
                    (loop (cdr stack) (cdar stack) new-state)
                    (loop '() #f new-state))]
               [else
                ;; 'qubx it meant to be a quote, unquote, box prefix, etc.
                (cond
                  [(and (not (null? stack)) (eq? (caar stack) 'qubx))
                   (loop (cdr stack) (cdar stack) new-state)]
                  [(reader-prefix? type value)
                   (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                       (loop stack #f new-state)
                       (loop (cons (cons 'qubx start) stack) #f new-state))]
                  [else
                   (loop stack start new-state)])])))))]
    [else (offset->pos new-offset)]))

(define (find-next-exp-forward ee entry row col [ignore-whitespace? #f])
  ; ordinarily stops at first s-expression if it follows whitespace (or
  ; comments), but always moves to second if ignore-whitespace? is true
  (define-values (obj offset offset->pos) (editor-object entry row col))
  (define new-offset ((current-expeditor-grouper) obj offset 0 'forward))
  (cond
    [(eq? new-offset #t)
     (let* ([s (entry->string entry #:from-row row #:from-col col)]
            [ip (open-input-string/count s)])
       (define (skip start state)
         (index->pos s
                     (with-handlers ([exn:fail? (lambda (exn) start)])
                       (let loop ([state state])
                         (let-values ([(type value start end new-state) (read-token ip state)])
                           (if (eq? type 'white-space)
                               (loop new-state)
                               start))))
                     row col))
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         (let loop ([stack '()] [first? #t] [ignore? #f] [state #f])
           (let-values ([(type value start end new-state) (read-token ip state)])
             (if (and first? (not ignore-whitespace?) (fx> start 0))
                 (and (not ignore?) (index->pos s start row col))
                 (case type
                   [(eof fasl) #f]
                   [(opener) (loop (cons (opener->closer value) stack) #f ignore? new-state)]
                   [(closer)
                    (and (not (null? stack))
                         (eq? (car stack) value)
                         (let ([stack (cdr stack)])
                           (if (null? stack)
                               (and (not ignore?) (skip start new-state))
                               (loop stack #f ignore? new-state))))]
                   [else
                    (cond
                      [(reader-prefix? type value)
                       (loop stack #f ignore? new-state)]
                      [(null? stack)
                       (and (not ignore?) (skip start new-state))]
                      [else
                       (loop stack #f ignore? new-state)])]))))))]
    [else (offset->pos new-offset)]))

(define (find-next-exp-upward ee entry row col)
  (define-values (obj offset offset->pos) (editor-object entry row col))
  (define new-offset ((current-expeditor-grouper) obj offset 0 'up))
  (cond
    [(eq? new-offset #t)
     (let* ([s (entry->string entry #:up-to-row row #:up-to-col col)]
            [ip (open-input-string/count s)])
       (let loop ([stack '()] [state #f])
         (let-values ([(type value start end new-state) (read-token ip state)])
           (case type
             [(eof)
              (and (pair? stack)
                   (index->pos s (car stack) 0 0))]
             [(opener) (loop (cons start stack) new-state)]
             [(closer) (loop (if (pair? stack) (cdr stack) null) new-state)]
             [else (loop stack new-state)]))))]
    [else (offset->pos new-offset)]))

(define (find-next-exp-downward ee entry row col)
  (define-values (obj offset offset->pos) (editor-object entry row col))
  (define new-offset ((current-expeditor-grouper) obj offset 0 'down))
  (cond
    [(eq? new-offset #t)
     (let* ([s (entry->string entry #:from-row row #:from-col col)]
            [ip (open-input-string/count s)])
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         (let loop ([state #f])
           (let-values ([(type value start end new-state) (read-token ip state)])
             (case type
               [(eof closer) #f]
               [(opener) (index->pos s end row col)]
               [else (loop new-state)])))))]
    [else (offset->pos new-offset)]))

(define-public (find-next-word find-previous-word)
  (define separator?
    (lambda (c)
      (memq c '(#\space #\; #\( #\) #\[ #\] #\" #\' #\`))))

  (define (find-next-word ee entry row col)
    ; always returns a position
    (let ([lns (entry-lns entry)])
      ; skip past separators
      (let loop ([row row] [col col])
        (cond
          [(fx= col (string-length (lns->str lns row)))
           (if (fx= row (fx1- (length lns)))
               (make-pos row col)
               (loop (fx1+ row) 0))]
          [(separator? (lns->char lns row col))
           (loop row (fx1+ col))]
          ; now we are past initial separators, find next separator
          [else
           (let loop ([col col])
             (cond
               [(or (fx= col (string-length (lns->str lns row)))
                    (separator? (lns->char lns row col)))
                (make-pos row col)]
               [else (loop (fx1+ col))]))]))))

  (define (find-previous-word ee entry row col)
    ; always returns a position
    (let ([lns (entry-lns entry)])
      ; skip past separators space (starts at char left of current)
      (let loop ([row row] [col col])
        (cond
          [(fx= col 0)
           (if (fx= row 0)
               (make-pos row col)
               (loop
                (fx1- row)
                (string-length (lns->str lns (fx1- row)))))]
          [(separator? (lns->char lns row (fx1- col)))
           (loop row (fx1- col))]
          ; now we are past initial separators, find next separator
          [else
           (let loop ([col col])
             (cond
               [(or (fx= col 0)
                    (separator? (lns->char lns row (fx1- col))))
                (make-pos row col)]
               [else (loop (fx1- col))]))])))))

(define-public (find-whitespace-start find-whitespace-end)
  (define (find-whitespace-end ee entry row col)
    ; always returns a position
    (let ([lns (entry-lns entry)])
      ; skip past whitespace
      (let loop ([row row] [col col])
        (cond
          [(fx= col (string-length (lns->str lns row)))
           (if (fx= row (fx1- (length lns)))
               (make-pos row col)
               (loop (fx1+ row) 0))]
          [(char-whitespace? (lns->char lns row col))
           (loop row (fx1+ col))]
          [else (make-pos row col)]))))

  (define (find-whitespace-start ee entry row col)
    ; always returns a position
    (let ([lns (entry-lns entry)])
      ; skip past separators space (starts at char left of current)
      (let loop ([row row] [col col])
        (cond
          [(fx= col 0)
           (if (fx= row 0)
               (make-pos row col)
               (loop
                (fx1- row)
                (string-length (lns->str lns (fx1- row)))))]
          [(char-whitespace? (lns->char lns row (fx1- col)))
           (loop row (fx1- col))]
          [else (make-pos row col)])))))
           

(define-public (indent indent-all)
  (define (calc-indent ee entry row)
    
    (define (find-unmatched-left-delim row)
      (let* ([ln (list-ref (entry-lns entry) row)]
             [s (ln-str ln)]
             [colors (ln-colors ln)])
        (ln-str-set! ee ln (string rpchar))
        (let ([pos (find-matching-delim-backward ee entry row 0 #t)])
          (ln-str-set! ee ln s colors)
          pos)))
    (let ([lns (entry-lns entry)])
      (cond
        [(find-unmatched-left-delim row)
         => (lambda (mpos)
              (let ([mrow (pos-row mpos)] [mcol (pos-col mpos)])
                (or
                 ; if some intervening line has same unmatched left
                 ; delimiter, use its indentation
                 (let f ([xrow (fx- row 1)])
                   (and (not (fx= xrow mrow))
                        (cond
                          [(find-unmatched-left-delim xrow)
                           => (lambda (xmpos)
                                (if (pos=? xmpos mpos)
                                    (current-indent lns xrow)
                                    (f (fx- xrow 1))))]
                          [else (f (fx- xrow 1))])))
                 ; otherwise, if left paren is followed by a symbol,
                 ; indent under second item or use standard indent if
                 ; second item is too far out or not present
                 (let ([ip (open-input-string/count
                            (let ([s (lns->str lns mrow)])
                              (substring s mcol (string-length s))))])
                   (with-handlers ([exn:fail? (lambda (exn) #f)])
                     (and (char=? (read-char ip) lpchar)
                          (let-values ([(t1 v1 s1 e1 state) (read-token ip #f)])
                            (and (eq? t1 'symbol)
                                 (let-values ([(t2 v2 s2 e2 state2) (read-token ip state)])
                                   (if (and (not (eq? t2 'eof))
                                            (fx< s2 6)
                                            ; use standard indent for let and rec
                                            (not (memq v1 '(let rec))))
                                       (fx+ mcol s2)
                                       (fx+ mcol (ee-standard-indent)))))))))
                 ; otherwise, indent one space in.  this handles, among
                 ; other things, bracketed let bindings and cond clauses.
                 (fx+ mcol 1))))]
        [else 0])))

  (define current-indent
    (lambda (lns row)
      (let* ([s (lns->str lns row)]
             [n (string-length s)])
        (let f ([i 0])
          (if (and (fx< i n) (char=? (string-ref s i) #\space))
              (f (fx+ i 1))
              i)))))

  (define (indent-row! ee entry row n)
    (cond
      [(fx< n 0)
       (adjust-mark/delete ee entry row 0 row (fx- n))
       (let ([lns (entry-lns entry)])
         (lns->str! ee lns row
                    (let ([s (lns->str lns row)])
                      (substring s (fx- n) (string-length s)))))]
      [(fx> n 0)
       (adjust-mark/insert ee entry row 0 row n)
       (let ([lns (entry-lns entry)])
         (lns->str! ee lns row
                    (string-append
                     (make-string n #\space)
                     (lns->str lns row))))]))

  (define (indent ee entry [auto? #f])
    (define-values (obj offset offset->pos) (editor-object entry
                                                           (entry-row entry)
                                                           (entry-col entry)))
    (define amt ((current-expeditor-indenter) obj offset auto?))
    (define row (entry-row entry))
    (define lns (entry-lns entry))
    (define ln (list-ref lns row))
    (define nsr (ln-nsr ln))
    (define (finish n)
      (indent-row! ee entry row n)
      (move-bol ee entry)
      (let ([just-row? (fx= (ln-nsr ln) nsr)])
        (define eoe? (end-of-entry? ee entry))
        (display-rest/goto ee entry just-row?
                           ; avoid clear-eol/eos if inserting and either at end of entry or
                           ; rewriting just the current row
                           (or (fx< n 0) (and (not eoe?) (not just-row?)))
                           row (fxmax (fx+ (entry-col entry) n) 0))))
    (cond
      [(not amt)
       (let* ([n (fx- (calc-indent ee entry row) (current-indent lns row))])
         (unless (fx= n 0)
           (finish n)))]
      [else
       (define row (entry-row entry))
       (define cur-amt (current-indent lns row))
       (cond
         [(equal? amt '(0 "")) (void)]
         [(number? amt)
          (finish (- amt cur-amt))]
         [else
          (finish (- (string-length (cadr amt))
                     (car amt)))])]))

  (define (indent-all ee entry)
    (let* ([lns (entry-lns entry)]
           [row (entry-row entry)]
           [col (entry-col entry)]
           [top-line (entry-top-line entry)]
           [point-ln (list-ref lns row)]
           [point-strlen (string-length (ln-str point-ln))]
           [lines-to-top ; compute before we muck with indentation
            (screen-lines-between ee entry
                                  (pos-row top-line) (pos-col top-line)
                                  row (col->line-offset ee entry row col))])
      (let loop ([ls lns] [i 0] [firstmod (length lns)] [lastmod -1])
        (if (null? ls)
            (unless (and (fx< lastmod (pos-row top-line))
                         (fx> firstmod (pos-row (entry-bot-line entry))))
              ; move to first physical column of first displayed line
              (move-cursor-up lines-to-top)
              (carriage-return)
              (clear-eos)
              (set-entry-col! entry
                              (fxmax 0
                                     (fx+ col
                                          (fx- (string-length (ln-str point-ln))
                                               point-strlen))))
              (redisplay ee entry))
            (let ([n (fx- (calc-indent ee entry i) (current-indent lns i))])
              (if (fx= n 0)
                  (loop (cdr ls) (fx+ i 1) firstmod lastmod)
                  (begin
                    (indent-row! ee entry i n)
                    (loop (cdr ls) (fx+ i 1)
                          (fxmin i firstmod)
                          (fxmax i lastmod)))))))))
  )

(define (id-completions ee entry)
  (define (idstring<? prefix common scheme-syms)
    (lambda (s1 s2)
      (let ([x1 (string->symbol (string-append prefix s1))]
            [x2 (string->symbol (string-append prefix s2))])
        ; prefer common
        (let ([m1 (memq x1 common)] [m2 (memq x2 common)])
          (if m1
              (or (not m2) (< (length m2) (length m1)))
              (and (not m2)
                   ; prefer user-defined
                   (let ([u1 (not (memq x1 scheme-syms))]
                         [u2 (not (memq x2 scheme-syms))])
                     (if u1
                         (or (not u2) (string<? s1 s2))
                         (and (not u2) (string<? s1 s2))))))))))
  (define (completion str1 str2)
    (let ([n1 (string-length str1)] [n2 (string-length str2)])
      (and (fx>= n2 n1)
           (string=? (substring str2 0 n1) str1)
           (substring str2 n1 n2))))
  (define (fn-completions prefix)
    (values prefix
            '()
            #;
            (sort string<?
                  (foldl
                   (let ([last (path-last prefix)])
                     (lambda (suffix* s)
                       (cond
                         [(completion last s) =>
                                              (lambda (suffix) 
                                                (cons (if (file-directory? (string-append prefix suffix))
                                                          (string-append suffix (string (directory-separator)))
                                                          suffix)
                                                      suffix*))]
                         [else suffix*])))
                   '()
                   (on-error '()
                             (directory-list
                              (let ([dir (path-parent prefix)])
                                (if (string=? dir "") "." dir))))))))
  (let loop ([c 0])
    (if (fx>= c (entry-col entry))
        (values #f '())
        (let ([s (let ([s (lns->str (entry-lns entry) (entry-row entry))])
                   (substring s c (string-length s)))])
          ((with-handlers* ([exn:fail? (lambda (exn)
                                         (lambda ()
                                           (if (and (fx> (string-length s) 0) (char=? (string-ref s 0) #\"))
                                               (fn-completions (substring s 1 (string-length s)))
                                               (loop (fx+ c 1)))))])
             (let-values ([(type value start end new-state) (read-token (open-input-string/count s) #f)])
               (lambda ()
                 (cond
                   [(and (fx= (fx+ c end) (entry-col entry))
                         (eq? type 'symbol))
                    (let ([prefix (symbol->string value)])
                      (let-values ([(syms common) ((current-expeditor-completer) prefix)])
                        (values prefix
                                (sort (foldl (lambda (x suffix*)
                                               (cond
                                                 [(completion prefix (symbol->string x))
                                                  => (lambda (suffix) (cons suffix suffix*))]
                                                 [else suffix*]))
                                             '()
                                             syms)
                                      (idstring<? prefix common '())))))]
                   [(and (fx= (fx+ c end -1) (entry-col entry))
                         (eq? type 'string)
                         (string? value))
                    (fn-completions value)]
                   [else (loop (fx+ c end))])))))))))

(define (should-auto-indent? ee)
  (and (ee-auto-indent)
       ; don't autoindent if the characters are coming so fast that we're
       ; probably dealing with paste input
       (> (- (current-milliseconds) (car (eestate-rt-last-op ee))) 50)))
