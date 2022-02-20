#lang racket/base
(require ffi/unsafe/vm
         '#%terminal)

;; See "../main.rkt"

(provide
 (protect-out init-term
              $ee-read-char/blocking
              $ee-write-char
              char-width
              set-color
              ee-flush
              get-screen-size
              raw-mode
              no-raw-mode
              enter-am-mode
              exit-am-mode
              post-output-mode
              no-post-output-mode
              signal-mode
              no-signal-mode
              nanosleep
              pause
              get-clipboard

              move-cursor-up
              move-cursor-down
              $move-cursor-left
              $move-cursor-right
              clear-eol
              clear-eos
              $clear-screen
              scroll-reverse
              bell
              $carriage-return
              line-feed))

;; Cache character widths, especially for Windows, where
;; `terminal-char-width` can't report the right answer, but
;; `terminal-write-char` can report information about how
;; the cursor advanced.
(define char-widths (make-hasheqv))

(define init-term terminal-init)
(define $ee-read-char/blocking terminal-read-char)
(define $ee-write-char (lambda (c)
                         (define w (terminal-write-char c))
                         (hash-set! char-widths c w)
                         w))
(define char-width (lambda (c)
                     ;; we're only set up to handle characters
                     ;; that are non-negative sized, so we don't
                     ;; handle control characters
                     (max 0 (or (hash-ref char-widths c #f)
                                (terminal-char-width c)))))
(define set-color terminal-set-color)
(define ee-flush terminal-flush)
(define get-screen-size terminal-get-screen-size)
(define raw-mode (lambda () (terminal-raw-mode #t)))
(define no-raw-mode (lambda () (terminal-raw-mode #f)))
(define post-output-mode (lambda () (terminal-postoutput-mode #t)))
(define no-post-output-mode (lambda () (terminal-postoutput-mode #f)))
(define signal-mode (lambda () (terminal-signal-mode #t)))
(define no-signal-mode (lambda () (terminal-signal-mode #f)))
(define enter-am-mode (lambda () (terminal-automargin-mode #t)))
(define exit-am-mode (lambda () (terminal-automargin-mode #f)))
(define nanosleep terminal-nanosleep)
(define pause terminal-pause)
(define get-clipboard terminal-get-clipboard)

(define move-cursor-up (lambda (amt) (terminal-move-cursor 'up amt)))
(define move-cursor-down (lambda (amt) (terminal-move-cursor 'down amt)))
(define $move-cursor-left (lambda (amt) (terminal-move-cursor 'left amt)))
(define $move-cursor-right (lambda (amt) (terminal-move-cursor 'right amt)))
(define clear-eol (lambda () (terminal-clear 'eol)))
(define clear-eos (lambda () (terminal-clear 'eos)))
(define $clear-screen (lambda () (terminal-clear 'screen)))
(define scroll-reverse terminal-scroll-reverse)
(define bell terminal-bell)
(define $carriage-return terminal-carriage-return)
(define line-feed terminal-line-feed)
