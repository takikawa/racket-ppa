#lang racket/base

(require racket/flonum racket/fixnum racket/list racket/match racket/unsafe/ops racket/contract
         unstable/latent-contract/defthing
         (for-syntax racket/base)
         "math.rkt"
         "utils.rkt"
         "marching-utils.rkt")

(provide heights->lines heights->polys
         heights->lines:doc heights->polys:doc)

#|
Z values are at these normalized coordinates:

(0,1)           (1,1)
      z4 --- z3
       |     |
       |     |
      z1 --- z2
(0,0)           (1,0)

A marching squares algorithm and all of its facet functions have this type:

    ftype m = real real real real real -> m

where 'm' is a use-specific type, such as the type of "list of lines". The
first argument is the contour value; the rest are z coordinates arranged as
above.
|#

(define-syntax-rule (unrotate-vec v)
  (match-let ([(vector x y z)  v])
    (vector (unsafe-fl- 1.0 y) x z)))

(define-syntax-rule (mirror-x-vec v)
  (match-let ([(vector x y z)  v])
    (vector (fl- 1.0 x) y z)))

(define-syntax-rule (mirror-y-vec v)
  (match-let ([(vector x y z)  v])
    (vector x (fl- 1.0 y) z)))

;; =============================================================================
;; Contour lines

;; Except for opposite-corner facets, every line-returning facet function is
;; identical to the facet for its bitwise complement.

;; -----------------------------------------------------------------------------
;; all corners left out or included

;(: lines0000 (FType Lines))
(define-syntax-rule (lines0000 z z1 z2 z3 z4) empty)
(define-syntax-rule (lines1111 z z1 z2 z3 z4) empty)

;; -----------------------------------------------------------------------------
;; one corner included or left out

;(: lines1000 (FType Lines))
(define-syntax-rule (lines1000 z z1 z2 z3 z4)
  (list (vector (unsafe-solve-t z z1 z2) 0.0
                0.0 (unsafe-solve-t z z1 z4))))

;(: lines0100 (FType Lines))
(define-syntax-rule (lines0100 z z1 z2 z3 z4)
  (list (vector (unsafe-solve-t z z1 z2) 0.0
                1.0 (unsafe-solve-t z z2 z3))))

;(: lines0010 (FType Lines))
(define-syntax-rule (lines0010 z z1 z2 z3 z4)
  (list (vector 1.0 (unsafe-solve-t z z2 z3)
                (unsafe-solve-t z z4 z3) 1.0)))

;(: lines0001 (FType Lines))
(define-syntax-rule (lines0001 z z1 z2 z3 z4)
  (list (vector 0.0 (unsafe-solve-t z z1 z4)
                (unsafe-solve-t z z4 z3) 1.0)))

(define-syntax-rule (lines0111 z z1 z2 z3 z4) (lines1000 z z1 z2 z3 z4))
(define-syntax-rule (lines1011 z z1 z2 z3 z4) (lines0100 z z1 z2 z3 z4))
(define-syntax-rule (lines1101 z z1 z2 z3 z4) (lines0010 z z1 z2 z3 z4))
(define-syntax-rule (lines1110 z z1 z2 z3 z4) (lines0001 z z1 z2 z3 z4))

;; -----------------------------------------------------------------------------
;; adjacent corners included or left out

;(: lines1100 (FType Lines))
(define-syntax-rule (lines1100 z z1 z2 z3 z4)
  (list (vector 0.0 (unsafe-solve-t z z1 z4)
                1.0 (unsafe-solve-t z z2 z3))))

;(: lines0110 (FType Lines))
(define-syntax-rule (lines0110 z z1 z2 z3 z4)
  (list (vector (unsafe-solve-t z z1 z2) 0.0
                (unsafe-solve-t z z4 z3) 1.0)))

(define-syntax-rule (lines0011 z z1 z2 z3 z4) (lines1100 z z1 z2 z3 z4))
(define-syntax-rule (lines1001 z z1 z2 z3 z4) (lines0110 z z1 z2 z3 z4))

;; -----------------------------------------------------------------------------
;; opposite corners left out / included

;(: lines-opposite ((Float Float -> Boolean) -> (FType Lines)))
(define-syntax-rule (lines-opposite test? z z1 z2 z3 z4)
  ; disambiguate using average of corners as guess for center value
  (let ([z5  (unsafe-flavg4 z1 z2 z3 z4)])
    (if (test? z5 z)
        (list (vector (unsafe-solve-t z z1 z2) 0.0
                      1.0 (unsafe-solve-t z z2 z3))
              (vector 0.0 (unsafe-solve-t z z1 z4)
                      (unsafe-solve-t z z4 z3) 1.0))
        (list (vector (unsafe-solve-t z z1 z2) 0.0
                      0.0 (unsafe-solve-t z z1 z4))
              (vector 1.0 (unsafe-solve-t z z2 z3)
                      (unsafe-solve-t z z4 z3) 1.0)))))

(define-syntax-rule (lines1010 z z1 z2 z3 z4) (lines-opposite unsafe-fl>= z z1 z2 z3 z4))
(define-syntax-rule (lines0101 z z1 z2 z3 z4) (lines-opposite unsafe-fl< z z1 z2 z3 z4))

(define (unsafe-heights->lines z z1 z2 z3 z4)
  (define p1 (z1 . unsafe-fl>= . z))
  (define p2 (z2 . unsafe-fl>= . z))
  (define p3 (z3 . unsafe-fl>= . z))
  (define p4 (z4 . unsafe-fl>= . z))
  (if p1
      (if p2
          (if p3
              (if p4
                  (lines1111 z z1 z2 z3 z4)
                  (lines1110 z z1 z2 z3 z4))
              (if p4
                  (lines1101 z z1 z2 z3 z4)
                  (lines1100 z z1 z2 z3 z4)))
          (if p3
              (if p4
                  (lines1011 z z1 z2 z3 z4)
                  (lines1010 z z1 z2 z3 z4))
              (if p4
                  (lines1001 z z1 z2 z3 z4)
                  (lines1000 z z1 z2 z3 z4))))
      (if p2
          (if p3
              (if p4
                  (lines0111 z z1 z2 z3 z4)
                  (lines0110 z z1 z2 z3 z4))
              (if p4
                  (lines0101 z z1 z2 z3 z4)
                  (lines0100 z z1 z2 z3 z4)))
          (if p3
              (if p4
                  (lines0011 z z1 z2 z3 z4)
                  (lines0010 z z1 z2 z3 z4))
              (if p4
                  (lines0001 z z1 z2 z3 z4)
                  (lines0000 z z1 z2 z3 z4))))))

(defproc (heights->lines [xa real?] [xb real?] [ya real?] [yb real?]
                         [z real?] [z1 real?] [z2 real?] [z3 real?] [z4 real?]
                         ) (listof (list/c (vector/c real? real? real?) (vector/c real? real? real?)))
  (cond [(all inexact-real? xa xb ya yb z z1 z2 z3 z4)
         (define lines (unsafe-heights->lines z z1 z2 z3 z4))
         (for/list ([line  (in-list lines)])
           (match-define (vector u1 v1 u2 v2) line)
           (list (vector (unsafe-unsolve-t xa xb u1) (unsafe-unsolve-t ya yb v1) z)
                 (vector (unsafe-unsolve-t xa xb u2) (unsafe-unsolve-t ya yb v2) z)))]
        [(find-failure-index real? xa xb ya yb z z1 z2 z3 z4)
         => (λ (i) (raise-type-error 'heights->liens "real number" i xa xb ya yb z z1 z2 z3 z4))]
        [(= z z1 z2 z3 z4)  empty]
        [else
         (let-map
          (z z1 z2 z3 z4) inexact->exact
          (define z-min (min z z1 z2 z3 z4))
          (define z-max (max z z1 z2 z3 z4))
          (define z-scale (- z-max z-min))
          (define lines
            (unsafe-heights->lines (exact->inexact (/ (- z z-min) z-scale))
                                   (exact->inexact (/ (- z1 z-min) z-scale))
                                   (exact->inexact (/ (- z2 z-min) z-scale))
                                   (exact->inexact (/ (- z3 z-min) z-scale))
                                   (exact->inexact (/ (- z4 z-min) z-scale))))
          (for/list ([line  (in-list lines)])
            (match-define (vector u1 v1 u2 v2) line)
            (list (vector (unsolve-t xa xb u1) (unsolve-t ya yb v1) z)
                  (vector (unsolve-t xa xb u2) (unsolve-t ya yb v2) z))))]))

;; =============================================================================
;; Isoband marching squares: polygonizes contour between two isoline values

(define ((rotate-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map (λ (v) (unrotate-vec v)) poly))
       (f za zb z2 z3 z4 z1)))

(define ((mirror-x-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map (λ (v) (mirror-x-vec v)) poly))
       (f za zb z2 z1 z4 z3)))

(define ((mirror-y-facet f) za zb z1 z2 z3 z4)
  (map (λ (poly) (map (λ (v) (mirror-y-vec v)) poly))
       (f za zb z4 z3 z2 z1)))

;; -----------------------------------------------------------------------------
;; all corners same

(define (polys0000 za zb z1 z2 z3 z4) empty)
(define (polys2222 za zb z1 z2 z3 z4) empty)
(define (polys1111 za zb z1 z2 z3 z4) (list 'full))

;; -----------------------------------------------------------------------------
;; single triangle

(define (polys1000 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys0100 (rotate-facet polys1000))
(define polys0010 (rotate-facet polys0100))
(define polys0001 (rotate-facet polys0010))

(define (polys1222 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys2122 (rotate-facet polys1222))
(define polys2212 (rotate-facet polys2122))
(define polys2221 (rotate-facet polys2212))

;; -----------------------------------------------------------------------------
;; single trapezoid

(define (polys2000 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys0200 (rotate-facet polys2000))
(define polys0020 (rotate-facet polys0200))
(define polys0002 (rotate-facet polys0020))

(define (polys0222 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys2022 (rotate-facet polys0222))
(define polys2202 (rotate-facet polys2022))
(define polys2220 (rotate-facet polys2202))

;; -----------------------------------------------------------------------------
;; single rectangle

(define (polys1100 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector 1.0 0.0 z2)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys0110 (rotate-facet polys1100))
(define polys0011 (rotate-facet polys0110))
(define polys1001 (rotate-facet polys0011))

(define (polys1122 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector 1.0 0.0 z2)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys2112 (rotate-facet polys1122))
(define polys2211 (rotate-facet polys2112))
(define polys1221 (rotate-facet polys2211))

(define (polys0022 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys2002 (rotate-facet polys0022))
(define polys2200 (rotate-facet polys2002))
(define polys0220 (rotate-facet polys2200))

;; -----------------------------------------------------------------------------
;; single pentagon

(define (polys0111 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys1011 (rotate-facet polys0111))
(define polys1101 (rotate-facet polys1011))
(define polys1110 (rotate-facet polys1101))

(define (polys2111 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector 0.0 1.0 z4)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys1211 (rotate-facet polys2111))
(define polys1121 (rotate-facet polys1211))
(define polys1112 (rotate-facet polys1121))

(define (polys1002 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys2100 (rotate-facet polys1002))
(define polys0210 (rotate-facet polys2100))
(define polys0021 (rotate-facet polys0210))

(define (polys1220 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys0122 (rotate-facet polys1220))
(define polys2012 (rotate-facet polys0122))
(define polys2201 (rotate-facet polys2012))

(define (polys1200 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys0120 (rotate-facet polys1200))
(define polys0012 (rotate-facet polys0120))
(define polys2001 (rotate-facet polys0012))

(define (polys1022 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys2102 (rotate-facet polys1022))
(define polys2210 (rotate-facet polys2102))
(define polys0221 (rotate-facet polys2210))

;; -----------------------------------------------------------------------------
;; single hexagon

(define (polys0112 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys2011 (rotate-facet polys0112))
(define polys1201 (rotate-facet polys2011))
(define polys1120 (rotate-facet polys1201))

(define (polys2110 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 0.0 z2)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define polys0211 (rotate-facet polys2110))
(define polys1021 (rotate-facet polys0211))
(define polys1102 (rotate-facet polys1021))

(define (polys0121 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 0.0 z2)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 1.0 z4)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define polys1012 (rotate-facet polys0121))
(define polys2101 (rotate-facet polys1012))
(define polys1210 (rotate-facet polys2101))

;; -----------------------------------------------------------------------------
;; 6-sided saddle

(define (polys10100 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))
        (list (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector 1.0 (unsafe-solve-t za z2 z3) za))))

(define (polys10101 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define (polys1010 za zb z1 z2 z3 z4)
  (define z5 (unsafe-flavg4 z1 z2 z3 z4))
  (cond [(z5 . unsafe-fl< . za)  (polys10100 za zb z1 z2 z3 z4)]
        ; (z5 . >= . zb) is impossible
        [else  (polys10101 za zb z1 z2 z3 z4)]))

(define polys0101 (rotate-facet polys1010))

(define (polys1212-2 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))
        (list (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb))))

(define (polys1212-1 za zb z1 z2 z3 z4)
  (list (list (vector 0.0 0.0 z1)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define (polys1212 za zb z1 z2 z3 z4)
  (define z5 (unsafe-flavg4 z1 z2 z3 z4))
  (cond [(z5 . unsafe-fl>= . zb)  (polys1212-2 za zb z1 z2 z3 z4)]
        ; (z5 . < . za) is impossible
        [else  (polys1212-1 za zb z1 z2 z3 z4)]))

(define polys2121 (rotate-facet polys1212))

;; -----------------------------------------------------------------------------
;; 7-sided saddle

(define (polys0212-1 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define (polys0212-2 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))
        (list (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb))))

(define (polys0212 za zb z1 z2 z3 z4)
  (define z5 (unsafe-flavg4 z1 z2 z3 z4))
  (cond [(z5 . unsafe-fl< . zb)  (polys0212-1 za zb z1 z2 z3 z4)]
        ; handling (z5 . < . za) separately results in a non-convex polygon
        [else  (polys0212-2 za zb z1 z2 z3 z4)]))

(define polys2021 (rotate-facet polys0212))
(define polys1202 (rotate-facet polys2021))
(define polys2120 (rotate-facet polys1202))

(define (polys2010-1 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define (polys2010-0 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))
        (list (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector 1.0 1.0 z3)
              (vector (unsafe-solve-t za z4 z3) 1.0 za))))

(define (polys2010 za zb z1 z2 z3 z4)
  (define z5 (unsafe-flavg4  z1 z2 z3 z4))
  (cond [(z5 . unsafe-fl>= . za)  (polys2010-1 za zb z1 z2 z3 z4)]
        ; handling (z5 . >= . zb) separately results in a non-convex polygon
        [else  (polys2010-0 za zb z1 z2 z3 z4)]))

(define polys0201 (rotate-facet polys2010))
(define polys1020 (rotate-facet polys0201))
(define polys0102 (rotate-facet polys1020))

;; -----------------------------------------------------------------------------
;; 8-sided saddle

(define (polys0202-0 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 (unsafe-solve-t za z2 z3) za))
        (list (vector 0.0 (unsafe-solve-t za z1 z4) za)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb))))

(define (polys0202-1 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))))

(define (polys0202-2 za zb z1 z2 z3 z4)
  (list (list (vector (unsafe-solve-t za z1 z2) 0.0 za)
              (vector (unsafe-solve-t zb z1 z2) 0.0 zb)
              (vector 0.0 (unsafe-solve-t zb z1 z4) zb)
              (vector 0.0 (unsafe-solve-t za z1 z4) za))
        (list (vector 1.0 (unsafe-solve-t zb z2 z3) zb)
              (vector 1.0 (unsafe-solve-t za z2 z3) za)
              (vector (unsafe-solve-t za z4 z3) 1.0 za)
              (vector (unsafe-solve-t zb z4 z3) 1.0 zb))))

(define (polys0202 za zb z1 z2 z3 z4)
  (define z5 (unsafe-flavg4 z1 z2 z3 z4))
  (cond [(z5 . unsafe-fl< . za)  (polys0202-0 za zb z1 z2 z3 z4)]
        [(z5 . unsafe-fl< . zb)  (polys0202-1 za zb z1 z2 z3 z4)]
        [else  (polys0202-2 za zb z1 z2 z3 z4)]))

(define polys2020 (rotate-facet polys0202))

#|
(printf "(define polys-dispatch-table~n")
(printf "  (vector ")
(for* ([t1  (in-range 3)]
       [t2  (in-range 3)]
       [t3  (in-range 3)])
  (printf "~n  ")
  (for ([t4  (in-range 3)])
    (printf " polys~a~a~a~a" t1 t2 t3 t4)))
(printf "))")
|#

(define polys-dispatch-table
  (vector polys0000 polys0001 polys0002 
          polys0010 polys0011 polys0012 
          polys0020 polys0021 polys0022 
          polys0100 polys0101 polys0102 
          polys0110 polys0111 polys0112 
          polys0120 polys0121 polys0122 
          polys0200 polys0201 polys0202 
          polys0210 polys0211 polys0212 
          polys0220 polys0221 polys0222 
          polys1000 polys1001 polys1002 
          polys1010 polys1011 polys1012 
          polys1020 polys1021 polys1022 
          polys1100 polys1101 polys1102 
          polys1110 polys1111 polys1112 
          polys1120 polys1121 polys1122 
          polys1200 polys1201 polys1202 
          polys1210 polys1211 polys1212 
          polys1220 polys1221 polys1222 
          polys2000 polys2001 polys2002 
          polys2010 polys2011 polys2012 
          polys2020 polys2021 polys2022 
          polys2100 polys2101 polys2102 
          polys2110 polys2111 polys2112 
          polys2120 polys2121 polys2122 
          polys2200 polys2201 polys2202 
          polys2210 polys2211 polys2212 
          polys2220 polys2221 polys2222))

(define (unsafe-heights->polys za zb z1 z2 z3 z4)
  (define t1 (if (z1 . unsafe-fl< . za) 0 (if (z1 . unsafe-fl<= . zb) 1 2)))
  (define t2 (if (z2 . unsafe-fl< . za) 0 (if (z2 . unsafe-fl<= . zb) 1 2)))
  (define t3 (if (z3 . unsafe-fl< . za) 0 (if (z3 . unsafe-fl<= . zb) 1 2)))
  (define t4 (if (z4 . unsafe-fl< . za) 0 (if (z4 . unsafe-fl<= . zb) 1 2)))
  (define facet-num
    (unsafe-fx+ (unsafe-fx+ (unsafe-fx+ (unsafe-fx* (unsafe-fx* (unsafe-fx* t1 3) 3) 3)
                                        (unsafe-fx* (unsafe-fx* t2 3) 3))
                            (unsafe-fx* t3 3))
                t4))
  (define f (vector-ref polys-dispatch-table facet-num))
  (f za zb z1 z2 z3 z4))

(defproc (heights->polys [xa real?] [xb real?] [ya real?] [yb real?]
                         [za real?] [zb real?]
                         [z1 real?] [z2 real?] [z3 real?] [z4 real?]
                         ) (listof (listof (vector/c real? real? real?)))
  (cond [(all inexact-real? xa xb ya yb za zb z1 z2 z3 z4)
         (define polys (unsafe-heights->polys za zb z1 z2 z3 z4))
         (for/list ([poly  (in-list polys)])
           (cond [(eq? poly 'full)  (list (vector xa ya z1) (vector xb ya z2)
                                          (vector xb yb z3) (vector xa yb z4))]
                 [else  (for/list ([uv  (in-list poly)])
                          (match-define (vector u v z) uv)
                          (vector (unsafe-unsolve-t xa xb u) (unsafe-unsolve-t ya yb v) z))]))]
        [(find-failure-index real? xa xb ya yb za zb z1 z2 z3 z4)
         => (λ (i) (raise-type-error 'heights->polys "real number" i xa xb ya yb za zb z1 z2 z3 z4))]
        [(= za zb z1 z2 z3 z4)  (list (list (vector xa ya z1) (vector xb ya z2)
                                            (vector xb yb z3) (vector xa yb z4)))]
        [else
         (let-map
          (za zb z1 z2 z3 z4) inexact->exact
          (define z-min (min za zb z1 z2 z3 z4))
          (define z-max (max za zb z1 z2 z3 z4))
          (define z-scale (- z-max z-min))
          (define polys
            (unsafe-heights->polys (exact->inexact (/ (- za z-min) z-scale))
                                   (exact->inexact (/ (- zb z-min) z-scale))
                                   (exact->inexact (/ (- z1 z-min) z-scale))
                                   (exact->inexact (/ (- z2 z-min) z-scale))
                                   (exact->inexact (/ (- z3 z-min) z-scale))
                                   (exact->inexact (/ (- z4 z-min) z-scale))))
          (for/list ([poly  (in-list polys)])
            (cond [(eq? poly 'full)  (list (vector xa ya z1) (vector xb ya z2)
                                           (vector xb yb z3) (vector xa yb z4))]
                  [else  (for/list ([uv  (in-list poly)])
                           (match-define (vector u v z) uv)
                           (vector (unsolve-t xa xb u)
                                   (unsolve-t ya yb v)
                                   (+ z-min (* (inexact->exact z) z-scale))))])))]))
