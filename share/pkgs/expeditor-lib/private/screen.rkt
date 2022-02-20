#lang racket/base
(require racket/fixnum
         ffi/unsafe/port
         "terminal.rkt"
         "param.rkt")

;; See "../main.rkt"

; screen initialization and manipulation routines

(provide
 (protect-out init-screen
              screen-resize! screen-rows screen-cols
              ee-winch? ee-char-ready? ee-peek-char ee-read-char
              ee-write-char ee-display-string
              wait
              set-fg-color)
 raw-mode no-raw-mode
 ee-flush
 move-cursor-up move-cursor-right move-cursor-left move-cursor-down
 scroll-reverse clear-eol clear-eos clear-screen
 carriage-return line-feed
 bell pause get-clipboard
 set-color)

; screen state
(define cols #f)
(define rows #f)
(define cursor-col #f)
(define the-unread-char #f)
(define winch #f)
(define input-port #f)

(define (fx1+ n) (fx+ n 1))

; we use terminfo routines directly, rather than going through curses,
; because curses requires initscr(), which clears the screen, discarding
; the current context.  this is a shell, not a full-screen user interface.

(define (screen-resize!)
  (let ([p (get-screen-size)])
    (set! rows (car p))
    (set! cols (cdr p))))

(define (screen-rows) rows)
(define (screen-cols) cols)

(define (init-screen in out)
  (define in-fd (unsafe-port->file-descriptor in))
  (define out-fd (unsafe-port->file-descriptor out))
  (and (init-term in-fd out-fd)
       (begin
         (set! cursor-col 0)
         (set! the-unread-char #f)
         (set! winch #f)
         (set! input-port in)
         #t)))

(define (clear-screen)
  ($clear-screen)
  (set! cursor-col 0))

(define ($ee-read-char in-port block?)
  (cond
    [block?
     (post-output-mode)
     (let loop ()
       (define r (sync in-port
                       ((current-get-interaction-evt))))
       (unless (eq? r in-port)
         (when (procedure? r)
           (signal-mode)
           (r)
           (no-signal-mode))
         (loop)))
     (no-post-output-mode)
     (or ($ee-read-char/blocking #f)
         ($ee-read-char in-port block?))]
    [else
     ($ee-read-char/blocking #f)]))

(define (ee-winch?)
  (and (not the-unread-char)
       (if winch
           (begin (set! winch #f) #t)
           (begin
             (ee-flush)
             (let ([c ($ee-read-char input-port #t)])
               (or (eq? c #t)
                   (begin (set! the-unread-char c) #f)))))))

(define (ee-char-ready?)
  (if the-unread-char
      #t
      (let f ()
        (ee-flush)
        (let ([c ($ee-read-char input-port #f)])
          (cond
            [(eq? c #f) #f]
            [(eq? c #t) (set! winch #t) (f)]
            [else (set! the-unread-char c) #t])))))

(define (ee-read-char)
  (if the-unread-char
      (let ([c the-unread-char]) (set! the-unread-char #f) c)
      (let f ()
        (ee-flush)
        (let ([c ($ee-read-char input-port #t)])
          (if (eq? c #t)
              (begin (set! winch #t) (f))
              c)))))

(define (ee-peek-char)
  (or the-unread-char
      (let ([c (ee-read-char)])
        (set! the-unread-char c)
        c)))

; we assume that ee-write-char receives only characters that occupy one
; screen cell.  it should never be passed #\return, #\newline, or #\tab.
; furthermore, ee-write-char should never be used to write past the end
; of a screen line.
(define (ee-write-char c)
  (define len
    (if (and cols (fx= (fx+ cursor-col 1) cols))
        (begin
          (exit-am-mode)
          (begin0
            ($ee-write-char c)
            (enter-am-mode)))
        ($ee-write-char c)))
  (set! cursor-col (fx+ cursor-col len))
  len)

; comments regarding ee-write-char above apply also to ee-display-string
(define (ee-display-string s)
  (let ([n (string-length s)])
    (do ([i 0 (fx+ i 1)])
        ((fx= i n))
      (ee-write-char (string-ref s i)))))

(define (carriage-return)
  (set! cursor-col 0)
  ($carriage-return))

(define (move-cursor-right n)
  (cond
    [(fx< (fx+ cursor-col n) cols)
     ($move-cursor-right n)
     (set! cursor-col (fx+ cursor-col n))]
    [else
     (move-cursor-down (quotient (fx+ cursor-col n) cols))
     (let ([new-cursor-col (remainder (fx+ cursor-col n) cols)])
       (if (fx>= new-cursor-col cursor-col)
           (move-cursor-right (fx- new-cursor-col cursor-col))
           (move-cursor-left (fx- cursor-col new-cursor-col))))]))

(define (move-cursor-left n)
  (when (and (fx= cursor-col cols) (fx> n 0))
    (set! n (fx- n 1))
    (set! cursor-col (fx- cursor-col 1)))
  (cond
    [(fx<= n cursor-col)
     ($move-cursor-left n)
     (set! cursor-col (fx- cursor-col n))]
    [else
     (move-cursor-up (fx1+ (quotient (fx- n cursor-col 1) cols)))
     (let ([new-cursor-col (remainder
                            (fx- cols (remainder (fx- n cursor-col) cols))
                            cols)])
       (if (fx>= new-cursor-col cursor-col)
           (move-cursor-right (fx- new-cursor-col cursor-col))
           (move-cursor-left (fx- cursor-col new-cursor-col))))]))

(define wait
  (lambda (ms)
    (unless (or (<= ms 0) (ee-char-ready?))
      (nanosleep 0 (* 10 1000 1000)) ; 10ms granularity is best we can assume
      (wait (- ms 10)))))

(define (set-fg-color color)
  (set-color color #f))
