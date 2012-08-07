#;
(
TR opt: unboxed-let-functions8.rkt 14:67 x -- unbox float-complex
TR opt: unboxed-let-functions8.rkt 14:69 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions8.rkt 14:64 (+ x 2.0+4.0i) -- unboxed binary float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



(letrec: ((f : (Float-Complex -> Float-Complex)     (lambda (x) (+ x 2.0+4.0i)))
          (g : (Float-Complex -> Float-Complex)     f)) ; f escapes! can't unbox its args
  (f 1.0+2.0i))
