#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "../common/draw.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "renderer.rkt"
         "sample.rkt"
         "bounds.rkt")

(provide contours3d contour-intervals3d)

;; ===================================================================================================
;; Contour lines in 3D (using marching squares)

(define ((contours3d-render-proc f levels samples colors widths styles alphas label) area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zss) (f x-min x-max (animated-samples samples)
                                    y-min y-max (animated-samples samples)))
  (define zs
    (cond [(list? levels)      levels]
          [(eq? levels 'auto)  (auto-contour-zs z-min z-max)]
          [else  (linear-seq z-min z-max levels #:start? #f #:stop? #f)]))
  
  (define cs (maybe-apply/list colors zs))
  (define ws (maybe-apply/list widths zs))
  (define ss (maybe-apply/list styles zs))
  (define as (maybe-apply/list alphas zs))
  
  (for ([z  (in-list zs)]
        [color  (in-cycle cs)]
        [width  (in-cycle ws)]
        [style  (in-cycle ss)]
        [alpha  (in-cycle as)])
    (send area put-alpha alpha)
    (send area put-pen color width style)
    (for ([ya  (in-list ys)]
          [yb  (in-list (rest ys))]
          [zs0  (in-vector zss)]
          [zs1  (in-vector zss 1)]
          #:when #t
          [xa  (in-list xs)]
          [xb  (in-list (rest xs))]
          [z1  (in-vector zs0)]
          [z2  (in-vector zs0 1)]
          [z3  (in-vector zs1 1)]
          [z4  (in-vector zs1)])
      (define lines (heights->lines (exact->inexact z)
                                    (exact->inexact z1) (exact->inexact z2)
                                    (exact->inexact z3) (exact->inexact z4)))
      (for ([line  (in-list lines)])
        (match-define (vector x1 y1 x2 y2) (scale-normalized-line line xa xb ya yb))
        (send area put-line
              (vector x1 y1 z) (vector x2 y2 z)
              (center-coord (list (vector xa ya z1) (vector xb ya z2)
                                  (vector xa yb z3) (vector xb yb z4)))))))
  
  (cond [label  (line-legend-entries label zs colors widths styles)]
        [else   empty]))

(defproc (contours3d
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:colors colors plot-colors/c (contour-colors)]
          [#:widths widths pen-widths/c (contour-widths)]
          [#:styles styles plot-pen-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (renderer3d (contours3d-render-proc g levels samples colors widths styles alphas label)
              default-3d-ticks-fun
              (surface3d-bounds-fun g samples)
              x-min x-max y-min y-max z-min z-max))

;; ===================================================================================================
;; Contour intervals in 3D (using marching squares)

(define ((contour-intervals3d-render-proc
          f levels samples colors line-colors line-widths line-styles
          contour-colors contour-widths contour-styles alphas label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))  
  (match-define (list xs ys zss) (f x-min x-max (animated-samples samples)
                                    y-min y-max (animated-samples samples)))
  
  (define contour-zs
    (cond [(list? levels)      levels]
          [(eq? levels 'auto)  (auto-contour-zs z-min z-max)]
          [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
  
  (define zs (append (list z-min) contour-zs (list z-max)))
  (define cs (maybe-apply/list colors zs))
  (define lcs (maybe-apply/list line-colors zs))
  (define lws (maybe-apply/list line-widths zs))
  (define lss (maybe-apply/list line-styles zs))
  (define as (maybe-apply/list alphas zs))
  
  (for ([za  (in-list zs)]
        [zb  (in-list (rest zs))]
        [color  (in-cycle cs)]
        [line-color  (in-cycle lcs)]
        [line-width  (in-cycle lws)]
        [line-style  (in-cycle lss)]
        [alpha  (in-cycle as)])
    (send area put-alpha alpha)
    (send area put-pen line-color line-width line-style)
    (send area put-brush color 'solid)
    (for ([ya  (in-list ys)]
          [yb  (in-list (rest ys))]
          [zs0  (in-vector zss)]
          [zs1  (in-vector zss 1)]
          #:when #t
          [xa  (in-list xs)]
          [xb  (in-list (rest xs))]
          [z1  (in-vector zs0)]
          [z2  (in-vector zs0 1)]
          [z3  (in-vector zs1 1)]
          [z4  (in-vector zs1)])
      (for ([poly  (in-list (heights->mid-polys (exact->inexact za) (exact->inexact zb)
                                                (exact->inexact z1) (exact->inexact z2)
                                                (exact->inexact z3) (exact->inexact z4)))])
        (define rect (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4)))
        (send area put-polygon
              (cond [(equal? poly 'full)  rect]
                    [else  (scale-normalized-poly poly xa xb ya yb)])
              (center-coord rect)))))
  
  ((contours3d-render-proc f levels samples contour-colors contour-widths contour-styles alphas #f)
   area)
  
  (cond [label  (contour-intervals-legend-entries
                 label z-min z-max contour-zs colors '(solid) line-colors line-widths line-styles
                 contour-colors contour-widths contour-styles)]
        [else  empty]))

(defproc (contour-intervals3d
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:levels levels (or/c 'auto exact-positive-integer? (listof real?)) (contour-levels)]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:colors colors plot-colors/c (contour-interval-colors)]
          [#:line-colors line-colors plot-colors/c (contour-interval-line-colors)]
          [#:line-widths line-widths pen-widths/c (contour-interval-line-widths)]
          [#:line-styles line-styles plot-pen-styles/c (contour-interval-line-styles)]
          [#:contour-colors contour-colors plot-colors/c (contour-colors)]
          [#:contour-widths contour-widths pen-widths/c (contour-widths)]
          [#:contour-styles contour-styles plot-pen-styles/c (contour-styles)]
          [#:alphas alphas alphas/c (contour-interval-alphas)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (renderer3d (contour-intervals3d-render-proc g levels samples colors
                                               line-colors line-widths line-styles
                                               contour-colors contour-widths contour-styles
                                               alphas label)
              default-3d-ticks-fun
              (surface3d-bounds-fun g samples)
              x-min x-max y-min y-max z-min z-max))
