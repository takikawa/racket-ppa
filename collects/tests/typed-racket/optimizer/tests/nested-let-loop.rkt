#;
(
TR opt: nested-let-loop.rkt 50:38 r -- unbox float-complex
TR opt: nested-let-loop.rkt 50:40 s -- unbox float-complex
TR opt: nested-let-loop.rkt 50:35 (+ r s) -- unboxed binary float complex
TR opt: nested-let-loop.rkt 43:8 r -- unboxed var -> table
TR opt: nested-let-loop.rkt 41:6 loop1 -- fun -> unboxed fun
TR opt: nested-let-loop.rkt 45:10 r -- unboxed complex variable
TR opt: nested-let-loop.rkt 50:38 r -- leave var unboxed
TR opt: nested-let-loop.rkt 50:40 s -- unbox float-complex
TR opt: nested-let-loop.rkt 50:35 (+ r s) -- unboxed binary float complex
TR opt: nested-let-loop.rkt 48:18 s -- unboxed var -> table
TR opt: nested-let-loop.rkt 46:16 loop2 -- fun -> unboxed fun
TR opt: nested-let-loop.rkt 50:38 r -- leave var unboxed
TR opt: nested-let-loop.rkt 50:40 s -- leave var unboxed
TR opt: nested-let-loop.rkt 50:35 (+ r s) -- unboxed binary float complex
TR opt: nested-let-loop.rkt 50:20 (loop1 (cdr x) (+ r s)) -- unboxed call site
TR opt: nested-let-loop.rkt 50:27 (cdr x) -- pair
TR opt: nested-let-loop.rkt 50:20 (loop1 (cdr x) (+ r s)) -- call to fun with unboxed args
TR opt: nested-let-loop.rkt 51:38 s -- leave var unboxed
TR opt: nested-let-loop.rkt 51:40 (car x) -- unbox float-complex
TR opt: nested-let-loop.rkt 51:40 (car x) -- pair
TR opt: nested-let-loop.rkt 51:48 (car y) -- unbox float-complex
TR opt: nested-let-loop.rkt 51:48 (car y) -- pair
TR opt: nested-let-loop.rkt 51:35 (+ s (car x) (car y)) -- unboxed binary float complex
TR opt: nested-let-loop.rkt 51:20 (loop2 (cdr y) (+ s (car x) (car y))) -- unboxed call site
TR opt: nested-let-loop.rkt 51:27 (cdr y) -- pair
TR opt: nested-let-loop.rkt 51:20 (loop2 (cdr y) (+ s (car x) (car y))) -- call to fun with unboxed args
TR opt: nested-let-loop.rkt 48:38 0.0+0.0i -- unboxed literal
TR opt: nested-let-loop.rkt 46:10 (let: loop2 : Float-Complex ((y : (Listof Float-Complex) (quote (3.0+6.0i 4.0+8.0i))) (s : Float-Complex 0.0+0.0i)) (if (null? y) (loop1 (cdr x) (+ r s)) (loop2 (cdr y) (+ s (car x) (car y))))) -- unboxed call site
TR opt: nested-let-loop.rkt 46:16 loop2 -- unboxed let loop
TR opt: nested-let-loop.rkt 43:28 0.0+0.0i -- unboxed literal
TR opt: nested-let-loop.rkt 41:0 (let: loop1 : Float-Complex ((x : (Listof Float-Complex) (quote (1.0+2.0i 2.0+4.0i))) (r : Float-Complex 0.0+0.0i)) (if (null? x) r (let: loop2 : Float-Complex ((y : (Listof Float-Complex) (quote (3.0+6.0i 4.0+8.0i))) (s : Float-Complex 0.0+0.0i)) (if (null? y) (loop1 (cdr x) (+ r s)) (loop2 (cdr y) (+ s (car x) (car y))))))) -- unboxed call site
TR opt: nested-let-loop.rkt 41:6 loop1 -- unboxed let loop
20.0+40.0i
)

#lang typed/scheme
#:optimize

(let: loop1 : Float-Complex
      ((x : (Listof Float-Complex)   '(1.0+2.0i 2.0+4.0i))
       (r : Float-Complex   0.0+0.0i))
      (if (null? x)
          r
          (let: loop2 : Float-Complex
                ((y : (Listof Float-Complex)   '(3.0+6.0i 4.0+8.0i))
                 (s : Float-Complex   0.0+0.0i))
                (if (null? y)
                    (loop1 (cdr x) (+ r s))
                    (loop2 (cdr y) (+ s (car x) (car y)))))))
