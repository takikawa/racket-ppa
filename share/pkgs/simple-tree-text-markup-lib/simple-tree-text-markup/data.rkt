; Combinator library for simple markup, with
; - srcloc links
; - horizontal and vertical composition
; - framed markup
#lang racket/base
(require racket/contract)
(provide
 (contract-out (struct empty-markup ())
               (struct horizontal-markup
                 ((markups (listof markup?))))
               (struct vertical-markup
                 ((markups (listof markup?))))
               (struct srcloc-markup
                 ((srcloc srcloc?)
                  (markup markup?)))
               (struct framed-markup
                 ((markup markup?)))
               (struct image-markup
                 ;; can be anything, as long as it's understood by the backend
                 ;; typical choices:
                 ;; - image%
                 ;; - snip%
                 ;; - record-dc-datum
                 ((data any/c)
                  (alt-markup markup?)))
               (markup? (any/c . -> . boolean?))
               (struct record-dc-datum
                 ((datum any/c)
                  (width natural-number/c)
                  (height natural-number/c)))
               (struct number-markup
                 ((number number?)
                  (exact-prefix (or/c 'always 'never 'when-necessary))
                  (inexact-prefix (or/c 'always 'never 'when-necessary))
                  ;; for exact non-integers only
                  (fraction-view (or/c 'mixed 'improper 'decimal))))
               (markup-folder
                (procedure? any/c (listof pair?) . -> .
                            (markup? . -> . any/c)))
               (transform-markup ((listof pair?) markup? . -> . markup?))))


(define (markup? x)
  (or (string? x)
      (empty-markup? x)
      (horizontal-markup? x)
      (vertical-markup? x)
      (srcloc-markup? x)
      (framed-markup? x)
      (image-markup? x)
      (number-markup? x)))

(struct empty-markup
  ()
  #:transparent)

(struct horizontal-markup
  (markups)
  #:transparent)

(struct vertical-markup
  (markups)
  #:transparent)

(struct srcloc-markup
  (srcloc markup)
  #:transparent)

(struct framed-markup
  (markup)
  #:transparent)

(struct image-markup
  (data alt-markup)
  #:transparent)

(struct record-dc-datum
  (datum width height)
  #:transparent)

(struct number-markup (number exact-prefix inexact-prefix fraction-view)
  #:transparent)

(define (markup-folder combine identity extractors)
  (lambda (markup)
    (let fold ((markup markup))
      (define val
        (cond
          ((assf (lambda (predicate) (predicate markup)) extractors)
           => (lambda (pair)
                ((cdr pair) markup)))
          (else identity)))
      (cond
        ((string? markup) val)
        ((empty-markup? markup) val)
        ((horizontal-markup? markup)
         (combine val
                  (foldl combine identity
                         (map fold (horizontal-markup-markups markup)))))
        ((vertical-markup? markup)
         (combine val
                  (foldl combine identity
                         (map fold (vertical-markup-markups markup)))))
        ((srcloc-markup? markup)
         (combine val (fold (srcloc-markup-markup markup))))
        ((framed-markup? markup)
         (combine val (fold (framed-markup-markup markup))))
        ((image-markup? markup)
         (combine val (fold (image-markup-alt-markup markup))))
        ((number-markup? markup) val)
        (else identity)))))

(define (transform-markup mappers markup)
  (define (reconstruct markup constructor)
    (cond
      ((assf (lambda (predicate) (predicate markup)) mappers) => cdr)
      (else constructor)))
  (let recur ((markup markup))
    (cond
      ((string? markup) ((reconstruct markup values) markup))
      ((empty-markup? markup) ((reconstruct markup empty-markup)))
      ((horizontal-markup? markup)
       ((reconstruct markup horizontal-markup) (map recur (horizontal-markup-markups markup))))
      ((vertical-markup? markup)
       ((reconstruct markup vertical-markup) (map recur (vertical-markup-markups markup))))
      ((srcloc-markup? markup)
       ((reconstruct markup srcloc-markup) (srcloc-markup-srcloc markup) (recur (srcloc-markup-markup markup))))
      ((framed-markup? markup)
       ((reconstruct markup framed-markup) (recur (framed-markup-markup markup))))
      ((image-markup? markup)
       ((reconstruct markup image-markup) (image-markup-data markup) (recur (image-markup-alt-markup markup))))
      ((number-markup? markup)
       ((reconstruct markup number-markup) (number-markup-number markup)
                                           (number-markup-exact-prefix markup) (number-markup-inexact-prefix markup)
                                           (number-markup-fraction-view markup))))))
     

