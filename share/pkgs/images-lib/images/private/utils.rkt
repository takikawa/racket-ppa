#lang racket/base

(require racket/draw racket/class racket/match racket/list
         (for-syntax racket/base)
         "flomap.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Caching flomaps with a hash table of weak box values

(define total-time-saved 0)
(define total-time-spent 0)

;; For a weak-value hash, we can't simply wrap hash-ref! with
;; weak-box-value and thnk with make-weak-box, because
;; 1. If weak-box-value returns #f, we need to regenerate the value
;; 2. We need to keep a handle to the generated value while it's being
;;    stored in the hash
;; So we use a pair of tables. One table maps the key to the
;; value, but using a key whose pointer identity is private (so
;; it's never reachable). Another table maps the value to the
;; key, so the key is retained as long as the value is retained.
;; Ephemerons provide the fixedpoint that ensures that the value and
;; key are kept only if the value is reachable.

(struct weak-value-hash (by-key     ; hash for (box key) -> (ephemeron (box key) (cons value msecs))
                         by-value)) ; hash for value -> (ephemeron value (box key))
;; The `by-key` table is used to find a cached value, while
;; the `by-value` table ensures tha the boxed key in `by-key` is retained
;; as long as the value is retained.
;; We assume that that a value in the table is not reachable from its key.

(define (make-weak-value-hash)
  (weak-value-hash (make-weak-hash) (make-weak-hasheq)))

(define (weak-value-hash-ref! h k thnk)
  (define (cache-ref!)
    (define start (current-milliseconds))
    (define val (thnk))
    (define time (- (current-milliseconds) start))
    (set! total-time-spent (+ total-time-spent time))
    ;; (printf "total-time-spent = ~v~n" total-time-spent)
    (define bk (box k))
    (hash-set! (weak-value-hash-by-key h) bk (make-ephemeron bk (cons val time)))
    (hash-set! (weak-value-hash-by-value h) val (make-ephemeron val bk))
    val)
  (cond
   [(hash-ref (weak-value-hash-by-key h) (box k) #f)
    => (lambda (ephemeron)
         (define val+msecs (ephemeron-value ephemeron))
         (cond
          [val+msecs
           (set! total-time-saved (+ total-time-saved (cdr val+msecs)))
           ;; (printf "total-time-saved = ~v~n" total-time-saved)
           (car val+msecs)]
          [else (cache-ref!)]))]
   [else (cache-ref!)]))

(define flomap-cache (make-weak-value-hash))

(define (get-total-time-saved) total-time-saved)
(define (get-total-time-spent) total-time-spent)

(define (make-cached-flomap* name proc size . args)
  (define rendered-size (if (size . < . 32) 32 size))
  (define fm (weak-value-hash-ref! flomap-cache (list name rendered-size args)
                        (λ () (apply proc rendered-size args))))
  (flomap-scale fm (/ size rendered-size)))

(define-syntax (make-cached-flomap stx)
  (syntax-case stx ()
    [(_ (size args ...) expr0 expr ...)
     ;; for some reason, generate-temporaries doesn't work here
     (with-syntax ([name  (gensym)])
       (syntax/loc stx
         (make-cached-flomap* 'name (λ (size args ...) expr0 expr ...) size args ...)))]))

;; ===================================================================================================
;; Drawing

(define (->color% c)
  (match c
    [(list r g b)  (make-object color% r g b)]
    [(? (is-a?/c color%))  c]
    [(? string?)  (send the-color-database find-color c)]
    [else  (raise-type-error '->color% "list, color% or string" c)]))

(define (apply-path-commands p cmds)
  (let loop ([x 0] [y 0] [cmds cmds])
    (cond
      [(empty? cmds)  (values x y)]
      [else
       (define cmd (first cmds))
       (match cmd
         ;; absolute commands
         [`(M)  (loop x y (rest cmds))]
         [`(L)  (loop x y (rest cmds))]
         [`(C)  (loop x y (rest cmds))]
         [`(M ,ax ,ay ,as ...)  (send p move-to ax ay)
                                (loop ax ay (cons `(M ,@as) (rest cmds)))]
         [`(L ,ax ,ay ,as ...)  (send p line-to ax ay)
                                (loop ax ay (cons `(L ,@as) (rest cmds)))]
         [`(C ,ax1 ,ay1 ,ax2 ,ay2 ,ax ,ay ,as ...)
          (send p curve-to ax1 ay1 ax2 ay2 ax ay)
          (loop ax ay (cons `(C ,@as) (rest cmds)))]
         ;; relative commands
         [`(m)  (loop x y (rest cmds))]
         [`(l)  (loop x y (rest cmds))]
         [`(c)  (loop x y (rest cmds))]
         [`(m ,dx ,dy ,ds ...)  (send p move-to (+ x dx) (+ y dy))
                                (loop (+ x dx) (+ y dy) (cons `(m ,@ds) (rest cmds)))]
         [`(l ,dx ,dy ,ds ...)  (send p line-to (+ x dx) (+ y dy))
                                (loop (+ x dx) (+ y dy) (cons `(l ,@ds) (rest cmds)))]
         [`(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy ,ds ...)
          (send p curve-to (+ dx1 x) (+ dy1 y) (+ dx2 x) (+ dy2 y) (+ dx x) (+ dy y))
          (loop (+ x dx) (+ y dy) (cons `(c ,@ds) (rest cmds)))]
         [_  (error 'apply-path-commands "unknown path command ~e" cmd)])]))
  (void))

(define (draw-path-commands dc cmds x y)
  (define p (new dc-path%))
  (apply-path-commands p cmds)
  (define t (send dc get-transformation))
  (send dc translate x y)
  (send dc draw-path p)
  (send dc set-transformation t))

(define (list->pairs lst)
  (match lst
    [(list x y xs ...)  (cons (cons x y) (list->pairs xs))]
    [(list)  (list)]))

(define (scale-path-commands cmds sx sy)
  (match cmds
    [(list `(,sym ,xys ...) cmds ...)
     (cons
      `(,sym ,@(flatten (map (λ (xy)
                               (match-define (cons x y) xy)
                               (list (* x sx) (* y sy)))
                             (list->pairs xys))))
      (scale-path-commands cmds sx sy))]
    [(list)  (list)]))

(define (relativize-path-commands cmds)
  (let loop ([x 0] [y 0] [cmds cmds])
    (cond
      [(empty? cmds)  empty]
      [else
       (define cmd (first cmds))
       (match cmd
         ;; absolute commands
         [`(M)  (loop x y (rest cmds))]
         [`(L)  (loop x y (rest cmds))]
         [`(C)  (loop x y (rest cmds))]
         [`(M ,ax ,ay ,as ...)  (cons `(m ,(- ax x) ,(- ay y))
                                      (loop ax ay (cons `(M ,@as) (rest cmds))))]
         [`(L ,ax ,ay ,as ...)  (cons `(l ,(- ax x) ,(- ay y))
                                      (loop ax ay (cons `(L ,@as) (rest cmds))))]
         [`(C ,ax1 ,ay1 ,ax2 ,ay2 ,ax ,ay ,as ...)
          (cons `(c ,(- ax1 x) ,(- ay1 y) ,(- ax2 x) ,(- ay2 y) ,(- ax x) ,(- ay y))
                (loop ax ay (cons `(C ,@as) (rest cmds))))]
         ;; relative commands
         [`(m)  (loop x y (rest cmds))]
         [`(l)  (loop x y (rest cmds))]
         [`(c)  (loop x y (rest cmds))]
         [`(m ,dx ,dy ,ds ...)  (cons `(m ,dx ,dy) (loop (+ x dx) (+ y dy)
                                                         (cons `(m ,@ds) (rest cmds))))]
         [`(l ,dx ,dy ,ds ...)  (cons `(l ,dx ,dy) (loop (+ x dx) (+ y dy)
                                                         (cons `(l ,@ds) (rest cmds))))]
         [`(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy ,ds ...)
          (cons `(c ,dx1 ,dy1 ,dx2 ,dy2 ,dx ,dy)
                (loop (+ x dx) (+ y dy) (cons `(c ,@ds) (rest cmds))))]
         [_  (error 'apply-path-commands "unknown path command ~e" cmd)])])))

(define (get-text-size str font)
  (define bm (make-bitmap 1 1))
  (define dc (make-object bitmap-dc% bm))
  (define-values (w h _1 _2) (send dc get-text-extent str font #t))
  (values (inexact->exact (ceiling w))
          (inexact->exact (ceiling h))))
