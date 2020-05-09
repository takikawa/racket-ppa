#lang racket/base
(require racket/class
         racket/list
         racket/snip
         racket/draw
         racket/math)
(provide one-cell-snip%)

(define one-cell-snip%
  (class snip%
    (inherit get-style get-admin)
    (super-new)

    (define cached-extent #f)

    (define/private (cache-extent! dc)
      (cond [cached-extent
             (vector->values cached-extent)]
            [else
             (define font (send (get-style) get-font))
             (define-values (w h d a) (send dc get-text-extent "X" font))
             (set! cached-extent (vector w h d a))
             (values w h d a)]))

    (define/public (get/cache-extent dc)
      (if cached-extent
          (vector->values cached-extent)
          (cache-extent! dc)))

    (define/override (size-cache-invalid)
      (super size-cache-invalid)
      (set! cached-extent #f))

    (define/override (get-extent dc x y [wb #f] [hb #f] [db #f] [sp #f] [lsp #f] [rsp #f])
      (define-values (w h d a) (get/cache-extent dc))
      (when wb (set-box! wb w))
      (when hb (set-box! hb h))
      (when db (set-box! db d))
      (when sp (set-box! sp a))
      (when lsp (set-box! lsp 0))
      (when rsp (set-box! rsp 0)))

    (define/public (get-text-pen-color dc caret)
      (cond [(and (pair? caret)
                  (let ([admin (get-admin)])
                    (and admin (send admin get-selected-text-color))))
             => (lambda (fg) fg)]
            [else (send (send dc get-pen) get-color)]))

    ;; does not restore pen,brush
    (define/public (draw-background dc x y w h caret)
      (unless (pair? caret)
        (define bg-color (send (get-style) get-background))
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush bg-color 'solid)
        (send dc draw-rectangle x y w h)))

    (define/override (draw dc x y left top right bottom dx dy caret)
      (define-values (w h _d _a) (get/cache-extent dc))
      (define style (get-style))
      (define saved-pen (send dc get-pen))
      (define saved-brush (send dc get-brush))
      (let ([fg-color (get-text-pen-color dc caret)])
        (send dc set-pen fg-color 1 'solid)
        (send dc set-brush fg-color 'solid))
      (send dc draw-rectangle x y w h)
      (send dc set-brush saved-brush)
      (send dc set-pen saved-pen))

    (define/override (copy) (new this%))

    (define/override (get-text start count [flattened? #f])
      (define s (get-whole-text))
      (define len (string-length s))
      (substring s (min start len) (min (+ start count) len)))

    (define/public (get-whole-text) "")
    ))
