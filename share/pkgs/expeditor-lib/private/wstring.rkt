#lang racket/base
(require "terminal.rkt")

(provide char-width
         string-width
         string-fits-width
         string-unicell?)

;; On Windows, we get more acurrate width information for a
;; character after writing it. Overall, our strategy is to
;; keep the screen content in string form and recompute
;; width information any time it is needed, and that works
;; in most cases with adpative width information.

(define (string-width s [start 0])
  (for/sum ([c (in-string s start)])
    (char-width c)))

(define (string-fits-width str i width)
  (define len (string-length str))
  (let loop ([i i] [width width] [n 0])
    (cond
      [(= i len) n]
      [(zero? width) n]
      [else
       (define w (char-width (string-ref str i)))
       (if (w . > . width)
           n
           (loop (add1 i) (- width w) (add1 n)))])))

(define (string-unicell? str)
  (cond
    [(eq? (system-type) 'windows)
     ;; since the width report may change after we try to draw,
     ;; conservatively check for just ASCII
     (for/and ([c (in-string str)])
       ((char->integer c) . < . 128))]
    [else
     (for/and ([c (in-string str)])
       (eqv? 1 (char-width c)))]))

  
