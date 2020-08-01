#lang racket/base
(require racket/class
         racket/format
         racket/list
         racket/snip
         racket/draw
         racket/math
         "base.rkt")
(provide tainted-icon-snip%
         snip-class)

(define tainted-icon-snip%
  (class one-cell-snip%
    (inherit get-style set-snipclass get/cache-extent get-text-pen-color draw-background)
    (super-new)
    (set-snipclass snip-class)
    (send (get-the-snip-class-list) add snip-class)

    (define/override (draw dc x y left top right bottom dx dy caret)
      (define-values (w h d _a) (get/cache-extent dc))
      (define (SX u) (* u w))
      (define (SY u) (* u (min w (- h d))))
      (define (X u) (+ x (SX (+ u 0.5))))
      (define (Y u) (+ y (SY (+ u 0.5)) (max 0 (- h d w))))
      (let ([saved-pen (send dc get-pen)]
            [saved-brush (send dc get-brush)]
            [fg-color (get-text-pen-color dc caret)])
        (draw-background dc x y w h caret)
        (send dc set-brush fg-color 'transparent)
        (define penw (max 2 (/ w 8)))
        (define (draw-symbol)
          (send dc draw-ellipse (X -0.5) (Y -0.5) (SX 1) (SY 1))
          (send dc draw-line
                (X (/ (cos (* 1/4 pi)) 2)) (Y (/ (sin (* 1/4 pi)) 2))
                (X (/ (cos (* 5/4 pi)) 2)) (Y (/ (sin (* 5/4 pi)) 2))))
        (send dc set-pen (make-pen #:color fg-color #:width (* 2 penw)))
        (draw-symbol)
        (send dc set-pen (make-pen #:color "red" #:width penw))
        (draw-symbol)
        (send dc set-brush saved-brush)
        (send dc set-pen saved-pen)))

    (define/override (get-whole-text) "💥")
    ))

(define tainted-icon-snip-class%
  (class snip-class%
    (inherit set-classname set-version)
    (super-new)

    (set-classname (~s '(lib "tainted.rkt" "macro-debugger" "syntax-browser" "icons")))
    (set-version 1)

    (define/override (read f)
      (new tainted-icon-snip%))
    ))

(define snip-class (new tainted-icon-snip-class%))
