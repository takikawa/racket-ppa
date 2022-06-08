#lang racket/base
(require racket/fixnum
         racket/file
         racket/symbol
         racket/interaction-info
         "private/port.rkt"
         "private/public.rkt"
         "private/screen.rkt"
         "private/pos.rkt"
         "private/param.rkt"
         "private/ee.rkt"
         "private/history.rkt"
         "private/token.rkt"
         "private/color.rkt")

(provide call-with-expeditor
         expeditor-open
         expeditor-close
         expeditor-read

         expeditor-configure
         expeditor-init-file-path

         expeditor-error-display

         current-expeditor-history

         current-expeditor-lexer
         current-expeditor-ready-checker
         current-expeditor-reader
         current-expeditor-post-skipper
         current-expeditor-parentheses
         current-expeditor-grouper
         current-expeditor-indenter
         current-expeditor-color-enabled
         current-expeditor-history-whitespace-trim-enabled)

(module+ configure
  (provide expeditor-set-syntax-color!
           expeditor-bind-key!

           eestate?
           entry?

           ee-insert-self
           ee-insert-self/paren
           make-ee-insert-string

           ee-accept

           ee-newline
           ee-newline/accept
           ee-open-line

           ee-indent
           ee-indent-all

           ee-id-completion
           ee-id-completion/indent
           ee-next-id-completion

           ee-backward-char
           ee-forward-char
           ee-next-line
           ee-previous-line
           ee-forward-word
           ee-forward-exp
           ee-backward-word
           ee-backward-exp
           ee-upward-exp
           ee-downward-exp
           ee-backward-page
           ee-forward-page

           ee-beginning-of-line
           ee-end-of-line
           ee-beginning-of-entry
           ee-end-of-entry
           
           ee-goto-matching-delimiter
           ee-flash-matching-delimiter

           ee-transpose-char
           ee-transpose-word
           ee-transpose-exp

           ee-exchange-point-and-mark
           ee-set-mark

           ee-delete-char
           ee-backward-delete-char
           ee-delete-line
           ee-delete-to-eol
           ee-delete-between-point-and-mark-or-backward
           ee-delete-entry
           ee-reset-entry
           ee-reset-entry/break
           ee-delete-word
           ee-delete-exp
           ee-backward-delete-exp
           ee-yank-selection
           ee-yank-kill-buffer
           ee-eof
           ee-eof/delete-char

           ee-redisplay

           ee-history-bwd
           ee-history-fwd
           ee-history-bwd-prefix
           ee-history-bwd-contains
           ee-history-fwd-prefix
           ee-history-fwd-contains

           ee-command-repeat
           ee-suspend-process

           current-ee-backward-history-point
           current-ee-forward-history-point))

;;; Based on:
;;;
;;; expeditor.ss
;;; R. Kent Dybvig
;;; August 2007

;;; This code is based on David Boyer's command-line editor, which has the
;;; following copyright:
;;;
;;;   Copyright (c) 1989, 1993, 1994 C. David Boyer
;;;
;;;   Permission to copy this software, in whole or in part, to use this
;;;   software for any lawful purpose, and to redistribute this software is
;;;   granted subject to the restriction that all copies made of this software
;;;   must include this copyright notice in full.
;;;
;;; The present implementation retains some of the basic design but little
;;; of the original code.

;;; The expression editor module is organized into sections:
;;;
;;;  1. screen-management routines
;;;  2. exported parameters
;;;  3. eestate and pos record definitions
;;;  4. current entry management routines
;;;  5. the reader and prompt-and-reader
;;;  6. history management routines
;;;  7. key function definitions
;;;  8. key binding code
;;;
;;; Also contained within this file are a few system entry points:
;;; the $enable-expeditor and $expeditor-history-file parameters and
;;; the main entry point into the expression editor, $expeditor.
;;;
;;; Pieces of the implementation have been moved to separate files:
;;  "private/screen.rkt", "private/pos.rkt", "private/ee.rkt", etc.

(define $enable-expeditor (make-parameter #f))
(define $expeditor-history-file
  (make-parameter #f
    (lambda (s)
      (cond
        [(not s) s]
        [(string? s)
         (if (string=? s "")
             (build-path (find-system-path 'pref-dir) ".expeditor_history")
             s)]
        [else (error '$expeditor-history-file "~s is not #f or a string" s)]))))

(define beep
  (lambda (str . arg*)
    #;(with-output-to-file "/tmp/ee.log"
        (lambda () (apply printf str arg*) (newline))
        'append)
    (when (ee-noisy) (bell))))

(define-public (ee-read)
  (define bars-around-read-errors? #t)
  
  (define (accept ee entry kf #:newline? [newline? #t])
    (let* ([str (entry->string entry)]
           [sip (open-input-string/count str)])
      (define (fail exn)
        (define (report sop)
          (display (exn-message exn) sop))
        ;; clear entry before report has a chance to muck with point position
        (clear-entry ee entry)
        (when bars-around-read-errors?
          (ee-display-string (make-string (screen-cols) #\-))
          (carriage-return)
          (line-feed))
        (when (current-expeditor-color-enabled)
          (set-fg-color error-color))
        (let* ([s (let ([sop (open-output-string)])
                    (report sop)
                    (get-output-string sop))]
               [n (string-length s)])
          (let loop ([i 0] [msg-lines 0])
            (if (= i n)
                (begin
                  (when (current-expeditor-color-enabled)
                    (set-fg-color default-color))
                  (when bars-around-read-errors?
                    (unless (fx< (screen-rows) 3)
                      (ee-display-string (make-string (screen-cols) #\-))
                      (carriage-return)
                      (line-feed)))
                  (redisplay ee entry (max (fx- (screen-rows) msg-lines 2) 1)))
                (let ([m (min (fx+ i (screen-cols)) n)])
                  (ee-display-string (substring s i m))
                  (when (fx< (screen-rows) 2) (wait 2000))
                  (carriage-return)
                  (line-feed)
                  (loop m (fx+ msg-lines 1))))))
        (kf))
      (define (succeed result)
        (move-eoe ee entry)
        (no-raw-mode)
        (when newline? ; skip a newline on EOF for consistency with plain input
          (ee-write-char #\newline))
        (ee-flush)
        (update-history! ee entry)
        ;; skip close delimiters, whitespace, and comments?
        (define fp (input-port-position sip))
        (define skip ((current-expeditor-post-skipper) sip))
        ;; save remainder of entry, if any, as histnow
        (set-eestate-histnow! ee (substring str (fx+ fp skip) (string-length str)))
        (set-eestate-last-op! ee #f)
        result)
      ((with-handlers ([exn:fail? (lambda (exn) (lambda () (fail exn)))])
         (let ([x ((current-expeditor-reader) sip)])
           (lambda () (succeed x)))))))

  (define (dispatch ee entry table)
    (if (ee-winch?)
        (begin
          (handle-winch ee entry)
          (dispatch ee entry table))
        (let ([c (ee-read-char)])
          (let ([x (if (eof-object? c)
                       (lambda (ee entry c) eof)
                       (hash-ref table c (lambda () ee-insert-self/paren)))])
            (cond
              [(procedure? x)
               (let ([n (eestate-repeat-count ee)])
                 (set-eestate-repeat-count! ee 1)
                 (if (= n 0)
                     (dispatch ee entry base-dispatch-table)
                     (let loop ([n n] [entry entry])
                       (define result (x ee entry c))
                       (cond
                         [(entry? result)
                          (define entry result)
                          (if (> n 1)
                              (loop (- n 1) entry)
                              (begin
                                (set-eestate-rt-last-op! ee (cons (cdr (eestate-rt-last-op ee))
                                                                  (current-milliseconds)))
                                (set-eestate-last-op! ee x)
                                (dispatch ee entry base-dispatch-table)))]
                         [else
                          (accept ee entry
                                  #:newline? (not (eof-object? result))
                                  (lambda ()
                                    (dispatch ee entry base-dispatch-table)))]))))]
              [(dispatch-table? x) (dispatch ee entry x)]
              [else
               (set-eestate-repeat-count! ee 1)
               (set-eestate-last-op! ee #f)
               (beep "unbound key")
               (dispatch ee entry base-dispatch-table)])))))

  (define (ee-read ee)
    (screen-resize!)
    (let ([entry (let ([s (eestate-histnow ee)])
                  ; set to "" so that entry will appear modified if nonempty,
                  ; i.e., if a partial entry is left over from last read
                   (set-eestate-histnow! ee "")
                   (string->entry ee s))])
      (raw-mode)
      (carriage-return)
      (redisplay ee entry)
      (move-eol ee entry)
      (recolor ee entry)
      (with-handlers* ([exn:fail? (lambda (exn)
                                    (carriage-return)
                                    (line-feed)
                                    (clear-eos)
                                    (ee-flush)
                                    (no-raw-mode)
                                    #;((error-display-handler) (exn-message exn) exn)
                                    (ee-display-string (exn-message exn))
                                    (ee-write-char #\newline)
                                    (update-history! ee entry)
                                    (void))]
                       [exn:break? (lambda (exn)
                                     (ee-flush)
                                     (no-raw-mode)
                                     (raise exn))])
        (dispatch ee entry base-dispatch-table)))))

(define (ee-prompt-and-read ee n wps)
  (unless (and (integer? n) (>= n 0))
    (error 'ee-prompt-and-read
           "nesting level ~s is not a positive integer"
           n))
  (if (and (terminal-port? (current-input-port))
           (terminal-port? (current-output-port)))
      (begin
        ; fresh-line doesn't take into account output written to the console
        ; through some other port or external means, so this might not emit a
        ; fresh line when one is needed, but the user can always redisplay
        (fresh-line (current-output-port))
        (flush-output (current-output-port))
        (set-eestate-prompt! ee
          (let ([wps wps])
            (if (string=? wps "")
                ""
                (string-append
                  (apply string-append (for/list ([i (in-range n)]) wps))
                  " "))))
        (ee-read ee))
      (default-prompt-and-read n)))

(define (fresh-line op)
  (flush-output op)
  (define-values (line col pos) (port-next-location op))
  (unless (or (not col) (eq? col 0)) (newline op)))

(define (default-prompt-and-read n)
  ((current-prompt-read)))


;;; editing functions

(define-public (ee-next-id-completion ee-next-id-completion/indent)
  (define complete
    (lambda (ee entry suffix*)
      (set-eestate-last-suffix*! ee suffix*)
      (if (null? suffix*)
          (beep "id-completion: no completion found")
          (insert-string-before ee entry (car suffix*)))))

  (define next-completion
    (lambda (ee entry)
      (if (fx<= (length (eestate-last-suffix* ee)) 1)
          (beep "id-completion: no completion found")
          (let ([suffix (car (eestate-last-suffix* ee))])
            (let ([n (string-length suffix)])
              (move-left ee entry n)
              (delete-forward ee entry (entry-row entry) (fx+ (entry-col entry) n)))
            (complete ee entry
              (append (cdr (eestate-last-suffix* ee)) (list suffix)))))))

  (define ee-next-id-completion
    (lambda (ee entry c)
      (if (eq? (eestate-last-op ee) ee-next-id-completion)
          (next-completion ee entry)
          (let-values ([(prefix suffix*) (id-completions ee entry)])
            (if prefix
                (complete ee entry suffix*)
                (begin
                  (set-eestate-last-suffix*! ee '())
                  (beep "id-completion: no identifier to complete")))))
      entry))

  (define ee-next-id-completion/indent
    (lambda (ee entry c)
      (cond
        [(and (eq? (eestate-last-op ee) ee-next-id-completion/indent)
              (eestate-cc? ee))
         (next-completion ee entry)
         entry]
        [(and (or (eq? (eestate-last-op ee) ee-insert-self/paren)
                  (eq? (eestate-last-op ee) ee-next-id-completion/indent))
              (let-values ([(prefix suffix*) (id-completions ee entry)])
                (and prefix suffix*))) =>
         (lambda (suffix*)
           (set-eestate-cc?! ee #t)
           (complete ee entry suffix*)
           entry)]
        [else
         (set-eestate-cc?! ee #f)
         (set-eestate-last-suffix*! ee '())
         (ee-indent ee entry c)])))
)
  
(define-public (ee-id-completion ee-id-completion/indent)
  (define (display-completions prefix suffix*)
    (let* ([s* (map (lambda (suffix) (string-append prefix suffix)) suffix*)]
           [width (fx+ (apply fxmax (map string-length s*)) 2)]
           [tcols (fxmax 1 (fxquotient (screen-cols) width))]
           [trows (fxquotient (length s*) tcols)]
           [nlong (fxremainder (length s*) tcols)])
      (define (display-row v last)
        (let loop ([j 0])
          (let ([s (vector-ref v j)])
            (if (fx= j last)
                (ee-display-string s)
                (begin
                  (ee-display-string (let ([s (format "~a"  s)])
                                       (string-append
                                        s
                                        (make-string (max 0 (- width (string-length s))) #\space))))
                  (loop (fx+ j 1))))))
        (carriage-return)
        (line-feed))
      (set-fg-color identifier-color)
      (begin0
        (let ([v (make-vector (if (fx= nlong 0) trows (fx+ trows 1)))])
          (do ([i 0 (fx+ i 1)])
              ((fx= i (vector-length v)))
            (vector-set! v i (make-vector tcols #f)))
          (let f ([s* s*] [i 0] [j 0] [nlong nlong])
            (unless (null? s*)
              (if (fx= i (if (fx> nlong 0) (fx+ trows 1) trows))
                  (f s* 0 (fx+ j 1) (fx- nlong 1))
                  (begin
                    (vector-set! (vector-ref v i) j (car s*))
                    (f (cdr s*) (fx+ i 1) j nlong)))))
          (do ([i 0 (fx+ i 1)])
              ((fx= i trows))
            (display-row (vector-ref v i) (fx- tcols 1)))
          (unless (fx= nlong 0)
            (display-row (vector-ref v trows) (fx- nlong 1)))
          (if (fx= nlong 0) trows (fx+ trows 1)))
        (set-fg-color default-color))))

  (define (common-prefix s*)
    (let outer ([s1 (car s*)] [s* (cdr s*)])
      (if (null? s*)
          s1
          (let ([s2 (car s*)])
            (let ([n1 (string-length s1)] [n2 (string-length s2)])
              (let inner ([i 0])
                (if (or (fx= i n1)
                        (fx= i n2)
                        (not (char=? (string-ref s1 i) (string-ref s2 i))))
                    (outer (substring s1 0 i) (cdr s*))
                    (inner (fx+ i 1)))))))))

  (define ee-id-completion
    (lambda (ee entry c)
      (let-values ([(prefix suffix*) (id-completions ee entry)])
        (if prefix
            (if (not (null? suffix*))
                (if (eq? (eestate-last-op ee) ee-id-completion)
                    (begin
                      (clear-entry ee entry)
                      (ee-display-string (make-string (screen-cols) #\-))
                      (carriage-return)
                      (line-feed)
                      (let ([nrows (display-completions prefix suffix*)])
                        (ee-display-string (make-string (screen-cols) #\-))
                        (carriage-return)
                        (line-feed)
                        (redisplay ee entry (max (fx- (screen-rows) nrows 1) 1))))
                    (insert-string-before ee entry (common-prefix suffix*)))
                (beep "id-completion: no completions found"))
            (beep "id-completion: no identifier to complete")))
      entry))

  (define ee-id-completion/indent
    (lambda (ee entry c)
      (cond
        [(and (eq? (eestate-last-op ee) ee-id-completion/indent)
              (eestate-cc? ee))
         (let-values ([(prefix suffix*) (id-completions ee entry)])
           (if (not (null? suffix*))
               (begin
                 (clear-entry ee entry)
                 (ee-display-string (make-string (screen-cols) #\-))
                 (carriage-return)
                 (line-feed)
                 (let ([nrows (display-completions prefix suffix*)])
                   (ee-display-string (make-string (screen-cols) #\-))
                   (carriage-return)
                   (line-feed)
                   (redisplay ee entry (max (fx- (screen-rows) nrows 1) 1))))
               (beep "id-completion: no completions found")))
         entry]
        [(and (or (eq? (eestate-last-op ee) ee-insert-self/paren)
                  (eq? (eestate-last-op ee) ee-id-completion/indent))
              (let-values ([(prefix suffix*) (id-completions ee entry)])
                (and prefix suffix*))) =>
         (lambda (suffix*)
           (set-eestate-cc?! ee #t)
           (if (not (null? suffix*))
               (insert-string-before ee entry (common-prefix suffix*))
               (beep "id-completion: no completions found"))
           entry)]
        [else
         (set-eestate-cc?! ee #f)
         (ee-indent ee entry c)])))
  )

(define (recolor ee entry)
  (when (current-expeditor-color-enabled)
    (define str (entry->string entry))
    (unless (equal? str "")
      (define colors (make-vector (string-length str) default-color))
      (define sip (open-input-string/count str))
      (let loop ([state #f])
        (let-values ([(type value start end new-state) (read-token sip state)])
          (case type
            [(eof)
             (update-entry-colors ee entry colors)]
            [else
             (define color
               (case type
                 [(error) error-color]
                 [(opener closer hash-colon-keyword) paren-color]
                 [(string text constant) literal-color]
                 [(symbol) identifier-color]
                 [(comment) comment-color]
                 [else default-color]))
             (for ([i (in-range start end)])
               (vector-set! colors i color))
             (loop new-state)]))))))

(define ee-insert-self
  (lambda (ee entry c)
    (unless ((char->integer c) . < . 32) ; don't insert control characters
      (add-char ee entry c)
      (recolor ee entry))
    entry))

(define ee-insert-self/paren
  (lambda (ee entry c)
    (cond
      [(for/or ([p (in-list (current-expeditor-parentheses))])
         (or (eqv? c (string-ref (symbol->immutable-string (car p)) 0))
             (let ([s (symbol->immutable-string (cadr p))])
               (eqv? c (string-ref s (sub1 (string-length s)))))))
       (ee-insert-paren ee entry c)]
      [else
       (ee-insert-self ee entry c)])))

(define ee-command-repeat
  (lambda (ee entry c)
    (define (digit-value c) (fx- (char->integer c) (char->integer #\0)))
    (set-eestate-repeat-count! ee
      (let ([c (ee-peek-char)])
        (if (and (not (eof-object? c)) (char-numeric? c))
            (let loop ([n (digit-value (ee-read-char))])
              (let ([c (ee-peek-char)])
                (if (and (not (eof-object? c)) (char-numeric? c))
                    (loop (+ (* n 10) (digit-value (ee-read-char))))
                    n)))
            (ee-default-repeat))))
    entry))

(define-public (ee-history-bwd ee-history-fwd
                               ee-history-bwd-prefix ee-history-fwd-prefix
                               ee-history-bwd-contains ee-history-fwd-contains)
  (define contains?
    (lambda (key str)
      (let ([key-len (string-length key)]
            [str-len (string-length str)])
        (let loop ([idx 0])
          (cond
            [(fx> key-len (fx- str-len idx)) #f]
            [(string=? key (substring str idx (fx+ idx key-len))) #t]
            [else (loop (add1 idx))])))))

  (define prefix?
    (lambda (key str)
      (let ([nkey (string-length key)] [nstr (string-length str)])
       ; if key doesn't start with space, skip leading spaces in str
        (let ([i (if (or (fx= nkey 0) (char=? (string-ref key 0) #\space))
                     0
                     (let f ([i 0])
                       (if (or (fx= i nstr) (not (char=? (string-ref str i) #\space)))
                           i
                           (f (fx+ i 1)))))])
          (let ([n (fx+ nkey i)])
            (and (fx<= n nstr)
                 (string=? key (substring str i n))))))))

  (define new-entry
    (lambda (ee entry s dir)
      (clear-entry ee entry)
      (let ([entry (string->entry ee s)])
        (redisplay ee entry #f) ; Chez Scheme behavior is 1 instead of #f here
        (recolor ee entry)
        (case (if (eq? dir 'up)
                  (current-ee-backward-history-point)
                  (current-ee-forward-history-point))
          [(start) (void)]
          [(top) (move-eol ee entry)]
          [(bottom end) (move-eoe ee entry)])
        entry)))

  (define ee-history-bwd
    (lambda (ee entry c)
      (cond
        [(and (not (null-entry? entry)) (entry-modified? ee entry))
         (beep "cannot leave nonempty modified entry")
         entry]
        [(history-search-bwd ee (lambda (s) #t))
         => (lambda (s)
              ;; clear histkey when null as favor to search commands
              (when (null-entry? entry) (set-eestate-histkey! ee ""))
              (new-entry ee entry s 'up))]
        [else
         (beep "invalid history movement")
         entry])))

  (define ee-history-fwd
    (lambda (ee entry c)
      (cond
        [(and (not (null-entry? entry)) (entry-modified? ee entry))
         (beep "cannot leave nonempty modified entry")
         entry]
        [(history-search-fwd ee (lambda (s) #t)) =>
         (lambda (s)
          ; clear histkey when null as favor to search commands
           (when (null-entry? entry) (set-eestate-histkey! ee ""))
           (new-entry ee entry s 'down))]
        [else
         (beep "invalid history movement")
         entry])))

  (define history-search-bwd-key
    (lambda (ee entry match?)
      (if (or (entry-modified? ee entry) (null-entry? entry))
          (begin
            (history-fast-forward! ee)
            (set-eestate-histkey! ee (entry->string entry))
            (cond
              [(history-search-bwd ee
                 (lambda (s) (match? (eestate-histkey ee) s))) =>
               (lambda (s) (new-entry ee entry s 'up))]
              [else
               (beep "invalid history movement")
               entry]))
         ; if nonempty and unmodified, we must already have moved via one
         ; of the history commands, so eestate-histkey should be valid
          (cond
            [(history-search-bwd ee
               (lambda (s) (match? (eestate-histkey ee) s))) =>
             (lambda (s) (new-entry ee entry s 'up))]
            [else
             (beep "invalid history movement")
             entry]))))

  (define history-search-fwd-key
   ; similar to history-search-bwd-key but "finds" key at forward extreme
    (lambda (ee entry match?)
      (if (or (entry-modified? ee entry) (null-entry? entry))
          (begin
            (history-fast-forward! ee)
            (set-eestate-histkey! ee (entry->string entry))
            (cond
              [(history-search-fwd ee
                 (lambda (s) (prefix? (eestate-histkey ee) s))) =>
               (lambda (s) (new-entry ee entry s 'done))]
              [else
               (beep "invalid history movement")
               entry]))
         ; if nonempty and unmodified, we must already have moved via one
         ; of the history commands, so eestate-histkey should be valid
          (cond
            [(history-search-fwd ee
               (lambda (s) (match? (eestate-histkey ee) s))) =>
             (lambda (s) (new-entry ee entry s 'done))]
            [else
             (let ([entry (new-entry ee entry (eestate-histkey ee) 'down)])
               (history-fast-forward! ee)
               entry)]))))

  (define ee-history-fwd-prefix
    (lambda (ee entry c)
      (history-search-fwd-key ee entry prefix?)))

  (define ee-history-bwd-prefix
    (lambda (ee entry c)
      (history-search-bwd-key ee entry prefix?)))

  (define ee-history-fwd-contains
    (lambda (ee entry c)
      (history-search-fwd-key ee entry contains?)))

  (define ee-history-bwd-contains
    (lambda (ee entry c)
      (history-search-bwd-key ee entry contains?)))
)

(define ee-newline/accept
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) entry]
      [(and (only-whitespace-left? ee entry)
            ((current-expeditor-ready-checker)
             (open-input-string (entry->string entry))))
       (let loop ()
         (delete-to-eol ee entry)
         (unless (last-line? ee entry)
           (join-rows ee entry)
           (loop)))
       ;; #f result tells calling `ee-read` to return expr:
       #f]
      [else
       (insert-strings-before ee entry '("" ""))
       (when (should-auto-indent? ee)
         (indent ee entry #t)
         (recolor ee entry))
       entry])))

(define ee-newline
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) entry]
      [else
       (insert-strings-before ee entry '("" ""))
       (when (should-auto-indent? ee)
         (indent ee entry #t)
         (recolor ee entry))
       entry])))

(define ee-accept
  (lambda (ee entry c)
   ; force ee-read to attempt read even if not at end of expr and not balanced
    (with-handlers ([exn:fail? (lambda (exn) #f)])
      (let ([sip (open-input-string (entry->string entry))])
        (let loop ([state #f])
          (let-values ([(type value start end new-state) (read-token sip state)])
            (cond
              [(eq? type 'eof)
              ; entry contains only whitespace and comments.  pretend to accept
              ; but don't really, or ee-read will return eof, causing cafe to exit
               (update-history! ee entry)
               (move-eoe ee entry)
               (no-raw-mode)
               (ee-write-char #\newline)
               (ee-flush)
               (raw-mode)
               (let ([entry (string->entry ee "")])
                 (redisplay ee entry)
                 entry)]
              [(and (eq? type 'quote) (eq? value 'datum-comment))
               ((current-expeditor-reader) sip)
               (loop new-state)]
              [else #f])))))))

(define ee-open-line
  (lambda (ee entry c)
    (let ([point (entry-point entry)])
      (insert-strings-before ee entry '("" ""))
      (when (should-auto-indent? ee)
        (indent ee entry #t)
        (recolor ee entry))
      (goto ee entry point)
      entry)))

(define ee-indent
  (lambda (ee entry c)
    (indent ee entry)
    (recolor ee entry)
    entry))

(define ee-indent-all
  (lambda (ee entry c)
    (indent-all ee entry)
    (recolor ee entry)
    entry))

(define ee-backward-char
  (lambda (ee entry c)
    (if (beginning-of-line? ee entry)
        (unless (first-line? ee entry)
          (move-up ee entry)
          (move-eol ee entry))
        (move-left ee entry))
    entry))

(define ee-forward-char
  (lambda (ee entry c)
    (if (end-of-line? ee entry)
        (unless (last-line? ee entry)
          (move-down ee entry)
          (move-bol ee entry))
        (move-right ee entry))
    entry))

(define ee-next-line
  (lambda (ee entry c)
    (if (last-line? ee entry)
        (ee-history-fwd ee entry c)
        (begin
          (move-down ee entry)
          entry))))

(define ee-previous-line
  (lambda (ee entry c)
    (if (first-line? ee entry)
        (ee-history-bwd ee entry c)
        (begin
          (move-up ee entry)
          entry))))

(define ee-end-of-line
  (lambda (ee entry c)
    (move-eol ee entry)
    entry))

(define ee-beginning-of-line
  (lambda (ee entry c)
    (move-bol ee entry)
    entry))

(define ee-beginning-of-entry
  (lambda (ee entry c)
    (goto ee entry (make-pos 0 0))
    entry))

(define ee-end-of-entry
  (lambda (ee entry c)
    (move-eoe ee entry)
    entry))

(define ee-delete-to-eol
  (lambda (ee entry c)
    (if (end-of-line? ee entry)
        (unless (last-line? ee entry)
          (join-rows ee entry)
          (set-eestate-killbuf! ee
            (if (eq? (eestate-last-op ee) ee-delete-to-eol)
                (append (eestate-killbuf ee) '(""))
                '(""))))
        (set-eestate-killbuf! ee
          (let ([killbuf (delete-to-eol ee entry)])
            (if (eq? (eestate-last-op ee) ee-delete-to-eol)
               ; last addition must have been ("") representing newline
                (append (reverse (cdr (reverse (eestate-killbuf ee))))
                        killbuf)
                killbuf))))
    entry))

(define ee-delete-line
  (lambda (ee entry c)
    (if (and (first-line? ee entry)
             (not (last-line? ee entry))
             (last-line-displayed? ee entry))
        (ee-delete-entry ee entry c)
        (begin
          (move-bol ee entry)
          (let ([killbuf (delete-to-eol ee entry)])
             (unless (equal? killbuf '(""))
               (set-eestate-killbuf! ee killbuf)))
          (recolor ee entry)
          entry))))

(define ee-delete-between-point-and-mark
  (lambda (ee entry c)
    (let ([point (entry-point entry)] [mark (entry-mark entry)])
      (if mark
          (unless (pos=? mark point)
            (set-eestate-killbuf! ee
              (if (pos<? mark (entry-point entry))
                  (begin
                    (goto ee entry mark)
                    (delete-forward ee entry (pos-row point) (pos-col point)))
                  (delete-forward ee entry (pos-row mark) (pos-col mark))))
            (recolor ee entry))
          (beep "mark not set")))
    entry))

(define (ee-delete-between-point-and-mark-or-backward ee entry c)
  (if (entry-mark entry)
      (ee-delete-between-point-and-mark ee entry c)
      (ee-backward-delete-exp ee entry c)))

(define ee-set-mark
  (lambda (ee entry c)
    (entry-mark-set! entry (entry-point entry))
    entry))

(define ee-delete-entry
  (lambda (ee entry c)
    (unless (null-entry? entry)
      (set-eestate-killbuf! ee (yank-entry ee entry)))
    (clear-entry ee entry)
    (let ([entry (string->entry ee "")])
      (redisplay ee entry)
      entry)))

(define ee-reset-entry
  (lambda (ee entry c)
    (history-fast-forward! ee)
    (ee-delete-entry ee entry c)))

(define ee-reset-entry/break
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) (break-thread (current-thread))]
      [else (ee-reset-entry ee entry c)])))

(define-public (ee-transpose-char ee-transpose-word ee-transpose-exp)
  (define (do-transpose ee entry pre-pos pre-end-pos post-pos post-end-pos)
    (define delta (string-length (entry->string entry
                                                #:from-row (pos-row pre-end-pos)
                                                #:from-col (pos-col pre-end-pos)
                                                #:up-to-row (pos-row post-pos)
                                                #:up-to-col (pos-col post-pos))))
    (goto ee entry post-pos)
    (define post (delete-forward ee entry (pos-row post-end-pos) (pos-col post-end-pos)))
    (goto ee entry pre-pos)
    (define pre (delete-forward ee entry (pos-row pre-end-pos) (pos-col pre-end-pos)))
    (insert-strings-before ee entry post)
    (move-forward ee entry delta)
    (insert-strings-before ee entry pre)
    (recolor ee entry))
  
  (define ee-transpose-char
    (lambda (ee entry c)
      (define row (entry-row entry))
      (define col (entry-col entry))
      (define end? (end-of-line? ee entry))
      (cond
        [(and (= row 0) (= col 0))
         (beep "no previous character to transpose")]
        [(and end? (col . > . 1))
         ;; transpose two previous characters
         (do-transpose ee entry
                       (make-pos row (- col 2))
                       (make-pos row (- col 1))
                       (make-pos row (- col 1))
                       (make-pos row col))]
        [(and end? (= row 0))
         (beep "no previous character to transpose")]
        [(and end? (= col 0))
         (beep "no previous character on line to transpose")]
        [(or (= col 0)
             (and end? (= col 1)))
         ;; swap char and previous newline
         (when (= col 1) (move-left ee entry))
         (define post (delete-forward ee entry row 1))
         (move-up ee entry)
         (move-eol ee entry)
         (insert-strings-before ee entry post)
         (when (= col 1)
           (move-down ee entry)
           (move-bol ee entry))
         (recolor ee entry)]
        [else
         (do-transpose ee entry
                       (make-pos row (- col 1))
                       (make-pos row col)
                       (make-pos row col)
                       (make-pos row (+ col 1)))])
      entry))
  
  (define (make-transpose find-next-backward find-next-forward)
    (lambda (ee entry c)
      (let* ([pre-end-pos (find-whitespace-start ee entry
                                                 (entry-row entry) (entry-col entry))]
             [pre-pos (find-next-backward ee entry
                                          (pos-row pre-end-pos) (pos-col pre-end-pos))]
             [post-pos (find-whitespace-end ee entry
                                            (entry-row entry) (entry-col entry))]
             [post-end-pos (find-next-forward ee entry
                                              (pos-row post-pos) (pos-col post-pos))])
        (cond
          [(and pre-pos post-end-pos)
           (do-transpose ee entry pre-pos pre-end-pos post-pos post-end-pos)]
          [else
           (beep "start or end not found")]))
      entry))

  (define ee-transpose-word
    (make-transpose find-previous-word find-next-word))
  (define ee-transpose-exp
    (make-transpose find-next-exp-backward find-next-exp-forward)))

(define ee-delete-exp
  (lambda (ee entry c)
    (let ([pos (find-next-exp-forward ee entry
                 (entry-row entry) (entry-col entry) #f)])
      (cond
        [pos
         (set-eestate-killbuf! ee (delete-forward ee entry (pos-row pos) (pos-col pos)))
         (recolor ee entry)]
        [else
         (beep "end of expression not found")]))
    entry))

(define ee-backward-delete-exp
  (lambda (ee entry c)
    (let ([row (entry-row entry)] [col (entry-col entry)])
      (let ([pos (find-next-exp-backward ee entry row col)])
        (if pos
            (begin
              (goto ee entry pos)
              (set-eestate-killbuf! ee (delete-forward ee entry row col))
              (recolor ee entry))
          (beep "start of expression not found"))))
    entry))

(define (ee-delete-word ee entry c)
  (define pos
    (find-next-word
     ee entry
     (entry-row entry)
     (entry-col entry)))
  (delete-forward
   ee entry
   (pos-row pos)
   (pos-col pos))
  (recolor ee entry)
  entry)

(define ee-redisplay
  (lambda (ee entry c)
    (if (eq? (eestate-last-op ee) ee-redisplay)
        (clear-screen)
        (clear-entry ee entry))
    (redisplay ee entry)
    entry))

(define ee-yank-kill-buffer
  (lambda (ee entry c)
    (insert-strings-before ee entry (eestate-killbuf ee))
    (recolor ee entry)
    entry))

(define ee-yank-selection
  (lambda (ee entry c)
    (insert-strings-before ee entry
      (string->lines
        (let* ([s (get-clipboard)]
               [n (fx- (string-length s) 1)])
          (if (and (fx>= n 0) (char=? (string-ref s n) #\newline))
              (substring s 0 n)
              s))))
    (recolor ee entry)
    entry))

(define make-ee-insert-string
  (lambda (str)
    (lambda (ee entry c)
      (insert-string-before ee entry str)
      (recolor ee entry)
      entry)))

(define ee-eof
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) eof]
      [else (beep "eof ignored except in null entry")])))

(define ee-delete-char
  (lambda (ee entry c)
    (cond
      [(end-of-line? ee entry)
       (unless (last-line? ee entry)
         (join-rows ee entry)
         (recolor ee entry))
       entry]
      [else
       (delete-char ee entry)
       (recolor ee entry)
       entry])))

(define ee-eof/delete-char
  (lambda (ee entry c)
    (cond
      [(null-entry? entry)
       (if (eq? (eestate-last-op ee) ee-eof/delete-char)
           entry     ; assume attempt to continue deleting chars
           eof)]
      [(end-of-line? ee entry)
       (unless (last-line? ee entry)
         (join-rows ee entry)
         (recolor ee entry))
       entry]
      [else
       (delete-char ee entry)
       (recolor ee entry)
       entry])))

(define ee-backward-delete-char
  (lambda (ee entry c)
    (if (beginning-of-line? ee entry)
        (unless (first-line? ee entry)
          (move-up ee entry)
          (move-eol ee entry)
          (join-rows ee entry)
          (recolor ee entry))
        (begin
          (move-left ee entry)
          (delete-char ee entry)
          (recolor ee entry)))
    entry))

(define ee-insert-paren
  (lambda (ee entry c)
    (add-char ee entry c)
    (recolor ee entry)
    (when (or (ee-flash-parens) (ee-auto-paren-balance))
      (correct&flash-matching-delimiter ee entry
                                        (lambda ()
                                          (recolor ee entry))))
    entry))

(define ee-goto-matching-delimiter
  (lambda (ee entry c)
    (let ([pos (find-matching-delimiter ee entry)])
      (if pos
          (goto ee entry pos)
          (beep "matching delimiter not found")))
    entry))

(define ee-flash-matching-delimiter
  (lambda (ee entry c)
    (let ([pos (find-matching-delimiter ee entry)])
      (if pos
          (flash ee entry pos)
          (beep "matching delimiter not found")))
    entry))

(define ee-exchange-point-and-mark
  (lambda (ee entry c)
    (let ([mark (entry-mark entry)])
      (if mark
          (begin
            (entry-mark-set! entry (entry-point entry))
            (goto ee entry mark))
          (beep "mark not set")))
    entry))

(define (make-ee-move-exp get-pos)
  (lambda (ee entry c)
    (let ([pos (get-pos ee entry (entry-row entry) (entry-col entry))])
      (if pos
          (goto ee entry pos)
          (beep "target expression not found")))
    entry))

(define ee-backward-exp
  (make-ee-move-exp (lambda (ee entry row col)
                       (find-next-exp-backward ee entry row col))))

(define ee-forward-exp
  (make-ee-move-exp (lambda (ee entry row col)
                       (find-next-exp-forward ee entry row col #t))))

(define ee-upward-exp
  (make-ee-move-exp (lambda (ee entry row col)
                       (find-next-exp-upward ee entry row col))))

(define ee-downward-exp
  (make-ee-move-exp (lambda (ee entry row col)
                       (find-next-exp-downward ee entry row col))))

(define ee-forward-word
  (lambda (ee entry c)
    (goto ee entry
      (find-next-word ee entry
        (entry-row entry)
        (entry-col entry)))
    entry))

(define ee-backward-word
  (lambda (ee entry c)
    (goto ee entry
      (find-previous-word ee entry
        (entry-row entry)
        (entry-col entry)))
    entry))

(define ee-forward-page
  (lambda (ee entry c)
    (page-down ee entry)
    entry))

(define ee-backward-page
  (lambda (ee entry c)
    (page-up ee entry)
    entry))

(define ee-suspend-process
  (lambda (ee entry c)
    (carriage-return)
    (line-feed)
    (clear-eos)
    (ee-flush)
    (no-raw-mode)
    (pause)
    (raw-mode)
    (carriage-return)
    (clear-eos)
    (redisplay ee entry)
    entry))

(define (ee-compose . p*)
  (let ([ee-composition
         (lambda (ee entry c)
           (let f ([p* p*] [entry entry])
             (if (null? p*)
                 entry
                 (let ([entry ((car p*) ee entry c)])
                   (and entry (f (cdr p*) entry))))))])
    ee-composition))

;;; key bindings

;;; (expeditor-bind-key! key ee-xxx)

;;; key must evaluate to a <key>, where:
;;;
;;; <key> = <char> | <key-string>
;;;
;;; <key-string> -> "<key-char>+"
;;; <key-char> ->
;;;   \e             escape character
;;;   ^x             control is applied to character x
;;;   \\             backslash
;;;   \^             caret
;;;   <plain-char>   any character other than \ or ^
;;;
;;; <char> examples:
;;;
;;;   input key   description  byte sequence
;;;   ---------   -----------  -------------
;;;   #\a         letter 'a'   97
;;;   #\^         caret        94
;;;
;;; <key-string> examples:
;;;
;;;   input key   contents  description  byte sequence
;;;   ---------   --------  -----------  -------------
;;;   "\\ex"      \ex       Esc-x        27 120
;;;   "^a"        ^a        Ctrl-A       1
;;;   "\\\\"      \\        backslash    92
;;;   "\\^"       \^        caret        94
;;;   "a"         a         letter 'a'   97

(define-public (dispatch-table? base-dispatch-table expeditor-bind-key!)
  (define make-dispatch-table
    (lambda ()
      (make-hasheqv)))

  (define dispatch-table? hash?)

  (define expeditor-bind-key!
    (lambda (key proc)
      (unless (or (char? key)
                  (and (string? key) (fx> (string-length key) 0)))
        (error 'expeditor-bind-key! "~s is not a valid key (character or nonempty string)" key))
      (unless (procedure? proc)
        (error 'expeditor-bind-key! "~s is not a procedure" proc))

      (if (string? key)
          (let* ([n (string-length key)])
            (define (s0 table i)
              (let ([c (string-ref key i)])
                (case c
                  [(#\\) (s-backslash table (fx+ i 1))]
                  [(#\^) (s-caret table (fx+ i 1))]
                  [else (s-lookup table (fx+ i 1) c)])))
            (define (s-backslash table i)
              (when (fx= i n)
                (error 'expeditor-bind-key!
                  "malformed key ~s (nothing following \\)"
                  key))
              (let ([c (string-ref key i)])
                (case c
                  [(#\e) (s-lookup table (fx+ i 1) #\u1B)]
                  [(#\\ #\^) (s-lookup table (fx+ i 1) c)]
                  [else (error 'expeditor-bind-key!
                         "malformed key ~s (unexpected character following \\)"
                         key)])))
            (define (s-caret table i)
              (define (^char c)
                (integer->char (fxand (char->integer c) #b11111)))
              (when (fx= i n)
                (error 'expeditor-bind-key!
                  "malformed key ~s (nothing following ^)"
                  key))
              (s-lookup table (fx+ i 1) (^char (string-ref key i))))
            (define (s-lookup table i key)
              (let ([x (hash-ref table key #f)])
                (cond
                  [(fx= i n)
                   (when (dispatch-table? x)
                     (log-warning
                       "expeditor-bind-key!: definition for key ~s disables its use as a prefix"
                       key))
                   (hash-set! table key proc)]
                  [(dispatch-table? x) (s0 x i)]
                  [else
                   (when (procedure? x)
                     (log-warning
                       "expeditor-bind-key!: definition for key ~s disables its use as a prefix"
                       key))
                   (let ([x (make-dispatch-table)])
                     (hash-set! table key x)
                     (s0 x i))])))
            (s0 base-dispatch-table 0))
          (begin
            (when (dispatch-table? (hash-ref base-dispatch-table key #f))
              (log-warning
                "expeditor-bind-key!: definition for key ~s disables its use as a prefix"
                key))
            (hash-set! base-dispatch-table key proc)))))

  (define base-dispatch-table (make-dispatch-table))

 ; set up self-insertion for space and all printing characters
  (for-each
    (lambda (c) (expeditor-bind-key! c ee-insert-self/paren))
    (string->list " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
)

(let ([ebk expeditor-bind-key!])
 ; newline oper<ations
  (ebk #\return   ee-newline/accept)                  ; Return, ^M
  (ebk "^J"       ee-newline/accept)                  ; ^J

  (ebk "\\e^M"    ee-newline)                         ; Esc-^M
  (ebk "\\e^J"    ee-accept)                          ; Esc-^J

  (ebk "^O"       ee-open-line)                       ; ^O

 ; indenting operations
  (ebk "\\e\t"    ee-indent)                          ; Esc-Tab
  (ebk "\\eq"     ee-indent-all)                      ; Esc-q
  (ebk "\\eQ"     ee-indent-all)                      ; Esc-Q
  (ebk "\\e^Q"    ee-indent-all)                      ; Esc-^Q

 ; command completion
  (ebk "\t"       ee-id-completion/indent)            ; Tab
  (ebk "^R"       ee-next-id-completion)              ; ^R

 ; cursor movement keys
  (ebk "^B"       ee-backward-char)                   ; ^B
  (ebk "\\e[D"    ee-backward-char)                   ; Left       ; ]
  (ebk "^F"       ee-forward-char)                    ; ^F
  (ebk "\\e[C"    ee-forward-char)                    ; Right      ; ]
  (ebk "^N"       ee-next-line)                       ; ^N
  (ebk "\\e[B"    ee-next-line)                       ; Down
  (ebk "^P"       ee-previous-line)                   ; ^P
  (ebk "\\e[A"    ee-previous-line)                   ; Up

  (ebk "\\ef"     ee-forward-word)                    ; Esc-f
  (ebk "\\eF"     ee-forward-word)                    ; Esc-F
  (ebk "\\e[1;5C" ee-forward-word)                    ; Ctl-Right
  (ebk "\\e^F"    ee-forward-exp)                     ; Esc-^F
  (ebk "\\e\\e[C" ee-forward-exp)                     ; Esc-Ctl-Right
  (ebk "\\eb"     ee-backward-word)                   ; Esc-b
  (ebk "\\eB"     ee-backward-word)                   ; Esc-B
  (ebk "\\e[1;5D" ee-backward-word)                   ; Ctl-Left
  (ebk "\\e^B"    ee-backward-exp)                    ; Esc-^B
  (ebk "\\e\\e[D" ee-backward-exp)                    ; Esc-Ctl-Left

  (ebk "\\e^U"    ee-upward-exp)                      ; Esc-^U
  (ebk "\\e^D"    ee-downward-exp)                    ; Esc-^D

  (ebk "^X^X"     ee-exchange-point-and-mark)         ; ^X^X
  (ebk "^X["      ee-backward-page)                   ; ^X[
  (ebk "^X]"      ee-forward-page)                    ; ^X]
  (ebk "\\e[5~"   ee-backward-page)                   ; Page-Up
  (ebk "\\e[6~"   ee-forward-page)                    ; Page-Down

  (ebk "^E"       ee-end-of-line)                     ; ^E
  (ebk "\\e[F"    ee-end-of-line)                     ; End key
 ; terminals are supposed to default to "normal" (aka "cursor") rather than
 ; "application" mode and in normal mode send ANSI \\e[F and \\e[H for End
 ; and Home.  although gnome terminal apparently starts in normal mode, it
 ; sends the application-mode sequences for this.  we capitulate reluctantly,
 ; since by defining Esc-OF and Esc-OH to do End and Home we prevent people
 ; from binding Esc-O by itself to a command.
  (ebk "\\eOF"    ee-end-of-line)                     ; End key (gnome terminal)
  (ebk "\\e[4~"   ee-end-of-line)                     ; End key (cygwin)
  (ebk "^A"       ee-beginning-of-line)               ; ^A
  (ebk "\\e[H"    ee-beginning-of-line)               ; Home key
  (ebk "\\eOH"    ee-beginning-of-line)               ; Home key (gnome terminal)
  (ebk "\\e[1~"   ee-beginning-of-line)               ; Home key (cygwin)
  (ebk "\\e<"     ee-beginning-of-entry)              ; Esc-<
  (ebk "\\e>"     ee-end-of-entry)                    ; Esc->      ; [[
  (ebk "\\e]"     ee-goto-matching-delimiter)         ; Esc-]
  (ebk "^]"       ee-flash-matching-delimiter)        ; ^]

  (ebk "^T"       ee-transpose-char)                  ; ^T
  (ebk "\\et"     ee-transpose-word)                  ; Esc-T
  (ebk "\\e^T"    ee-transpose-exp)                   ; Esc-^T

 ; destructive functions
  (ebk "^U"       ee-delete-line)                     ; ^U
  (ebk "^K"       ee-delete-to-eol)                   ; ^K
  (ebk "\\ek"     ee-delete-to-eol)                   ; Esc-k
  (ebk "^W"       ee-delete-between-point-and-mark-or-backward)   ; ^W
  (ebk "^G"       ee-delete-entry)                    ; ^G
  (ebk "^C"       ee-reset-entry/break)               ; ^C
  (ebk "\\ed"     ee-delete-word)                     ; Esc-d
  (ebk "\\e^K"    ee-delete-exp)                      ; Esc-^K
  (ebk "\\e\\e[3~" ee-delete-exp)                     ; Esc-Delete
  (ebk "\\e\177"  ee-backward-delete-exp)             ; Esc-Backspace
  (ebk "\\e^H"    ee-backward-delete-exp)             ; Esc-^H
  (ebk "^V"       ee-yank-selection)                  ; ^V
  (ebk "^Y"       ee-yank-kill-buffer)                ; ^Y
  (ebk "^D"       ee-eof/delete-char)                 ; ^D
  (ebk #\rubout   ee-backward-delete-char)            ; Backspace (<--)
  (ebk "\\e[3~"   ee-delete-char)                     ; Delete 
  (ebk "^H"       ee-backward-delete-char)            ; ^H
  (ebk "^@"       ee-set-mark)                        ; ^@ (or ^Space)
  (ebk "^^"       ee-set-mark)                        ; ^^

 ; display functions
  (ebk "^L"       ee-redisplay)                       ; ^L

 ; string macros
 ; (ebk "\\ed"     (make-ee-insert-string "(define "))       ; Esc-d   ; )
 ; (ebk "\\el"     (make-ee-insert-string "(lambda "))       ; Esc-l   ; )

 ; history keys
  (ebk "\\e^P"    ee-history-bwd)                     ; Esc-^P
  (ebk "\\e\\e[A" ee-history-bwd)                     ; Esc-Up
  (ebk "\\e^N"    ee-history-fwd)                     ; Esc-^N
  (ebk "\\e\\e[B" ee-history-fwd)                     ; Esc-Down
  (ebk "\\ep"     ee-history-bwd-prefix)              ; Esc-p
  (ebk "\\eP"     ee-history-bwd-contains)            ; Esc-P
  (ebk "\\en"     ee-history-fwd-prefix)              ; Esc-n
  (ebk "\\eN"     ee-history-fwd-contains)            ; Esc-N

 ; misc
  ;(ebk "\\e^U"   ee-command-repeat)                   ; Esc-^U
  (ebk "^Z"      ee-suspend-process)                  ; ^Z
)

(define (expeditor-open history)
  (cond
    [(init-screen (current-input-port) (current-output-port))
     (define ee (make-eestate))
     (ee-set-history! ee history)
     ee]
    [else #f]))

(define (expeditor-close ee)
  (ee-get-history ee))

(define (expeditor-read ee #:prompt [prompt ">"])
  (unless (eestate? ee) (raise-argument-error 'expeditor-read "eestate?" ee))
  (unless (string? prompt) (raise-argument-error 'expeditor-read "string?" prompt))
  (ee-prompt-and-read ee 1 prompt))

(define call-with-expeditor
  (lambda (proc #:prompt [prompt ">"])
    (unless (string? prompt) (raise-argument-error 'call-with-expeditor "string?" prompt))
    (let ([ee #f])
      (define (expeditor-prompt-and-read n)
        (if (cond
              [(eestate? ee) #t]
              [(eq? ee 'failed) #f]
              [(expeditor-open (current-expeditor-history))
               => (lambda (new-ee)
                    (set! ee new-ee)
                    #t)]
              [else (set! ee 'failed) #f])
            (ee-prompt-and-read ee n prompt)
            (default-prompt-and-read n)))
      (let ([val* (call-with-values
                   (lambda ()
                     (proc (lambda ()
                             (expeditor-prompt-and-read 1))))
                   list)])
        (when (eestate? ee)
          (current-expeditor-history (expeditor-close ee)))
        (apply values val*)))))

(define (expeditor-configure)
  (define (collection-file? file coll)
    (define f (collection-file-path file coll #:fail (lambda (err) #f)))
    (and f (file-exists? f)))
  (define info (let ([vec (current-interaction-info)])
                 (if vec
                     ((dynamic-require (vector-ref vec 0)
                                       (vector-ref vec 1))
                      (vector-ref vec 2))
                     (lambda (key def-val) def-val))))
  (current-expeditor-reader
   (lambda (in) ((current-read-interaction) (object-name in) in)))
  (let ([lexer (or (info 'color-lexer #f)
                   (and (collection-file? "racket-lexer.rkt" "syntax-color")
                        (dynamic-require 'syntax-color/racket-lexer 'racket-lexer)))])
    (current-expeditor-lexer lexer))
  (let ([pred (info 'drracket:submit-predicate #f)])
    (when pred
      (current-expeditor-ready-checker (lambda (in)
                                         (pred in #t)))))
  (let ([parens (info 'drracket:paren-matches #f)])
    (when parens
      (current-expeditor-parentheses parens)))
  (let ([group (info 'drracket:grouping-position #f)])
    (when group
      (current-expeditor-grouper group)))
  (let* ([indent-range (info 'drracket:range-indentation #f)]
         [indent (or (info 'drracket:indentation #f)
                     (and (not indent-range)
                          (collection-file? "racket-indentation.rkt" "syntax-color")
                          (dynamic-require 'syntax-color/racket-indentation 'racket-amount-to-indent)))])
    (when (or indent indent-range)
      (current-expeditor-indenter
       (lambda (t pos auto?)
         (cond
           [(and auto? indent)
            (indent t pos)]
           [else
            (define r (and indent-range
                           (indent-range t pos pos)))
            (if r
                (if (null? r)
                    (list 0 "")
                    (car r))
                (and indent
                     (indent t pos)))])))))
  (current-expeditor-color-enabled (get-preference 'expeditor-color-enabled
                                                   (lambda () #t)))
  (define init-file (expeditor-init-file-path))
  (when (file-exists? init-file)
    (dynamic-require init-file #f)))

(define (expeditor-init-file-path)
  (define init-dir (find-system-path 'init-dir))
  (define home-dir (find-system-path 'home-dir))
  (cond
    [(equal? init-dir home-dir)
     (build-path init-dir ".expeditor.rkt")]
    [else
     (build-path init-dir "expeditor.rkt")]))

(define (expeditor-error-display obj)
  (when (current-expeditor-color-enabled)
    (set-fg-color error-color))
  (ee-display-string obj)
  (when (current-expeditor-color-enabled)
    (set-fg-color default-color)
    (ee-flush)))

(module+ main
  (port-count-lines! (current-input-port))
  (port-count-lines! (current-output-port))
  (current-namespace (make-base-namespace))
  (current-expeditor-lexer (dynamic-require 'syntax-color/racket-lexer 'racket-lexer))
  (current-expeditor-reader (lambda (in) (read-syntax (object-name in) in)))
  (define ee (expeditor-open (map bytes->string/utf-8
                                  (get-preference 'readline-input-history (lambda () null)))))
  (unless ee
    (error 'expeditor "initialization failed"))
  (current-prompt-read (lambda () (expeditor-read ee)))
  (exit-handler
   (let ([old (exit-handler)])
     (lambda (v)
       (define history (expeditor-close ee))
       (put-preferences '(readline-input-history) (list (map string->bytes/utf-8 history)))
       (old v))))
  (read-eval-print-loop))
