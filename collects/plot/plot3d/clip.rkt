#lang racket/base

;; Small library for clipping points, lines and polygons against axial planes.

(require racket/match racket/list)

(provide point-in-bounds? clip-line clip-polygon)

;; ===================================================================================================
;; Points

(define (point-in-bounds? v x-min x-max y-min y-max z-min z-max)
  (match-define (vector x y z) v)
  (and (x . >= . x-min) (x . <= . x-max) 
       (y . >= . y-min) (y . <= . y-max)
       (z . >= . z-min) (z . <= . z-max)))

;; ===================================================================================================
;; Lines

(define (clip-line-x start-in-bounds? x x1 y1 z1 x2 y2 z2)
  (define t (/ (- x x1) (- x2 x1)))
  (cond [start-in-bounds?  (values x1 y1 z1 x (+ y1 (* t (- y2 y1))) (+ z1 (* t (- z2 z1))))]
        [else              (values x (+ y1 (* t (- y2 y1))) (+ z1 (* t (- z2 z1))) x2 y2 z2)]))

(define (clip-line-y start-in-bounds? y x1 y1 z1 x2 y2 z2)
  (define t (/ (- y y1) (- y2 y1)))
  (cond [start-in-bounds?  (values x1 y1 z1 (+ x1 (* t (- x2 x1))) y (+ z1 (* t (- z2 z1))))]
        [else              (values (+ x1 (* t (- x2 x1))) y (+ z1 (* t (- z2 z1))) x2 y2 z2)]))

(define (clip-line-z start-in-bounds? z x1 y1 z1 x2 y2 z2)
  (define t (/ (- z z1) (- z2 z1)))
  (cond [start-in-bounds?  (values x1 y1 z1 (+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1))) z)]
        [else              (values (+ x1 (* t (- x2 x1))) (+ y1 (* t (- y2 y1))) z x2 y2 z2)]))

(define (clip-line-x-min x-min x1 y1 z1 x2 y2 z2)
  (cond [(and (x1 . >= . x-min) (x2 . < . x-min))  (clip-line-x #t x-min x1 y1 z1 x2 y2 z2)]
        [(and (x2 . >= . x-min) (x1 . < . x-min))  (clip-line-x #f x-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-x-max x-max x1 y1 z1 x2 y2 z2)
  (cond [(and (x1 . <= . x-max) (x2 . > . x-max))  (clip-line-x #t x-max x1 y1 z1 x2 y2 z2)]
        [(and (x2 . <= . x-max) (x1 . > . x-max))  (clip-line-x #f x-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-y-min y-min x1 y1 z1 x2 y2 z2)
  (cond [(and (y1 . >= . y-min) (y2 . < . y-min))  (clip-line-y #t y-min x1 y1 z1 x2 y2 z2)]
        [(and (y2 . >= . y-min) (y1 . < . y-min))  (clip-line-y #f y-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-y-max y-max x1 y1 z1 x2 y2 z2)
  (cond [(and (y1 . <= . y-max) (y2 . > . y-max))  (clip-line-y #t y-max x1 y1 z1 x2 y2 z2)]
        [(and (y2 . <= . y-max) (y1 . > . y-max))  (clip-line-y #f y-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-z-min z-min x1 y1 z1 x2 y2 z2)
  (cond [(and (z1 . >= . z-min) (z2 . < . z-min))  (clip-line-z #t z-min x1 y1 z1 x2 y2 z2)]
        [(and (z2 . >= . z-min) (z1 . < . z-min))  (clip-line-z #f z-min x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line-z-max z-max x1 y1 z1 x2 y2 z2)
  (cond [(and (z1 . <= . z-max) (z2 . > . z-max))  (clip-line-z #t z-max x1 y1 z1 x2 y2 z2)]
        [(and (z2 . <= . z-max) (z1 . > . z-max))  (clip-line-z #f z-max x1 y1 z1 x2 y2 z2)]
        [else                                      (values x1 y1 z1 x2 y2 z2)]))

(define (clip-line v1 v2 x-min x-max y-min y-max z-min z-max)
  (let/ec return
    ; early accept: both endpoints in bounds
    (when (and (point-in-bounds? v1 x-min x-max y-min y-max z-min z-max)
               (point-in-bounds? v2 x-min x-max y-min y-max z-min z-max))
      (return v1 v2))
    ; early reject: both endpoints on the outside of the same plane
    (match-define (vector x1 y1 z1) v1)
    (match-define (vector x2 y2 z2) v2)
    (when (or (and (x1 . < . x-min) (x2 . < . x-min)) (and (x1 . > . x-max) (x2 . > . x-max))
              (and (y1 . < . y-min) (y2 . < . y-min)) (and (y1 . > . y-max) (y2 . > . y-max))
              (and (z1 . < . z-min) (z2 . < . z-min)) (and (z1 . > . z-max) (z2 . > . z-max)))
      (return #f #f))
    (let*-values ([(x1 y1 z1 x2 y2 z2)  (clip-line-x-min x-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-x-max x-max x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-y-min y-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-y-max y-max x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-z-min z-min x1 y1 z1 x2 y2 z2)]
                  [(x1 y1 z1 x2 y2 z2)  (clip-line-z-max z-max x1 y1 z1 x2 y2 z2)])
      (values (vector x1 y1 z1) (vector x2 y2 z2)))))

;; ===================================================================================================
;; Polygons

(define ((make-clip-polygon idx test? clip-line) val vs)
  (reverse
   (for/fold ([res empty]) ([v1  (in-list (cons (last vs) vs))] [v2  (in-list vs)])
     (define v1-in-bounds? (test? (vector-ref v1 idx) val))
     (define v2-in-bounds? (test? (vector-ref v2 idx) val))
     (cond [(and v1-in-bounds? v2-in-bounds?)              (cons v2 res)]
           [(and (not v1-in-bounds?) (not v2-in-bounds?))  res]
            [else  (match-define (vector x1 y1 z1) v1)
                   (match-define (vector x2 y2 z2) v2)
                   (let-values ([(x1 y1 z1 x2 y2 z2) (clip-line v1-in-bounds? val x1 y1 z1 x2 y2 z2)])
                     (cond [v2-in-bounds?  (list* (vector x2 y2 z2) (vector x1 y1 z1) res)]
                           [else           (cons (vector x2 y2 z2) res)]))]))))

(define clip-polygon-x-min (make-clip-polygon 0 >= clip-line-x))
(define clip-polygon-x-max (make-clip-polygon 0 <= clip-line-x))
(define clip-polygon-y-min (make-clip-polygon 1 >= clip-line-y))
(define clip-polygon-y-max (make-clip-polygon 1 <= clip-line-y))
(define clip-polygon-z-min (make-clip-polygon 2 >= clip-line-z))
(define clip-polygon-z-max (make-clip-polygon 2 <= clip-line-z))

(define (clip-polygon vs x-min x-max y-min y-max z-min z-max)
  (let/ec return
    ; early reject: no polygon
    (when (empty? vs) (return empty))
    ; early accept: all endpoints in bounds
    (when (andmap (λ (v) (point-in-bounds? v x-min x-max y-min y-max z-min z-max)) vs)
      (return vs))
    (match-define (list (vector xs ys zs) ...) vs)
    ; early reject: all endpoints on the outside of the same plane
    (when (or (andmap (λ (x) (x . < . x-min)) xs) (andmap (λ (x) (x . > . x-max)) xs)
              (andmap (λ (y) (y . < . y-min)) ys) (andmap (λ (y) (y . > . y-max)) ys)
              (andmap (λ (z) (z . < . z-min)) zs) (andmap (λ (z) (z . > . z-max)) zs))
      (return empty))
    (let* ([vs  (clip-polygon-x-min x-min vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-x-max x-max vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-y-min y-min vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-y-max y-max vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-z-min z-min vs)]
           [_   (when (empty? vs) (return empty))]
           [vs  (clip-polygon-z-max z-max vs)])
      vs)))
