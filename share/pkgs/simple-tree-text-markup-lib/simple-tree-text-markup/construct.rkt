; Smart constructors and better names for markup
#lang racket/base
(require racket/contract
         (only-in simple-tree-text-markup/data markup? record-dc-datum?))
(provide
 (contract-out
  (srcloc-markup (srcloc? markup? . -> . markup?))
  (framed-markup (markup? . -> . markup?))
  (image-markup (any/c markup? . -> . markup?))
  (record-dc-datum (any/c natural-number/c natural-number/c . -> . record-dc-datum?))
  (number (->* (number?) (#:exact-prefix (or/c 'always 'never 'when-necessary)
                          #:inexact-prefix (or/c 'always 'never 'when-necessary)
                          #:fraction-view (or/c #f 'mixed 'improper 'decimal))
               markup?))
  (empty-markup markup?)
  (empty-line markup?)
  (horizontal (markup? ... . -> . markup?))
  (vertical (markup? ... . -> . markup?))
  (markup-transform-image-data ((any/c . -> . any/c) markup? . -> . markup?))))
   
(require (rename-in simple-tree-text-markup/data
                    (empty-markup make-empty-markup)
                    (number-markup make-number-markup))
         (only-in racket/list splitf-at append-map))

(define empty-markup (make-empty-markup))

(define empty-line (horizontal-markup '()))

(define (number number
                #:exact-prefix [exact-prefix 'never] #:inexact-prefix [inexact-prefix 'never]
                #:fraction-view [fraction-view #f])
  (make-number-markup number exact-prefix inexact-prefix fraction-view))

; flatten out nested markup elements, merge adjacent strings
(define (normalize-horizontal markups)
  (let ((flattened
         (append-map (lambda (markup)
                       (cond
                         ((empty-markup? markup) '())
                         ((horizontal-markup? markup)
                          (horizontal-markup-markups markup))
                         (else
                          (list markup))))
                     markups)))
    (merge-adjacent-strings flattened)))

(define (merge-adjacent-strings markups)
  (call-with-values
   (lambda () (splitf-at markups string?))
   (lambda (strings after)
     (let ((after-merged
            (if (null? after)
                '()
                (cons (car after)
                      (merge-adjacent-strings (cdr after))))))
       (if (null? strings)
           after-merged
           (cons (apply string-append strings)
                 after-merged))))))

(define (horizontal . markups)
  (let ((markups (normalize-horizontal markups)))
    (cond
      ((null? markups) empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else (horizontal-markup (remove "" markups))))))

(define (flatten-vertical markups)
  (append-map (lambda (markup)
                (cond
                  ((empty-markup? markup) '())
                  ((vertical-markup? markup)
                   (vertical-markup-markups markup))
                  (else (list markup))))
              markups))

(define (vertical . markups)
  (let ((markups (flatten-vertical markups)))
    (cond
      ((null? markups)
       empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else
       (vertical-markup markups)))))


(define (markup-transform-image-data transform-image-data markup)
  (let recur ((markup  markup))
    (cond
      ((string? markup) markup)
      ((empty-markup? markup) markup)
      ((horizontal-markup? markup)
       (horizontal-markup
        (map recur (horizontal-markup-markups markup))))
      ((vertical-markup? markup)
       (vertical-markup
        (map recur (vertical-markup-markups markup))))
      ((srcloc-markup? markup)
       (srcloc-markup (srcloc-markup-srcloc markup)
                      (recur (srcloc-markup-markup markup))))
      ((framed-markup? markup)
       (framed-markup (recur (framed-markup-markup markup))))
      ((image-markup? markup)
       (image-markup (transform-image-data (image-markup-data markup))
                     (recur (image-markup-alt-markup markup))))
      ((number-markup? markup) markup))))
