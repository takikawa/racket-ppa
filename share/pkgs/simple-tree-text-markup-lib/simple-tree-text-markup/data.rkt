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
               (markup? (any/c . -> . boolean?))))

(define (markup? x)
  (or (string? x)
      (empty-markup? x)
      (horizontal-markup? x)
      (vertical-markup? x)
      (srcloc-markup? x)
      (framed-markup? x)))

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
  (srcloc markup))

(struct framed-markup
  (markup)
  #:transparent)



  
  
