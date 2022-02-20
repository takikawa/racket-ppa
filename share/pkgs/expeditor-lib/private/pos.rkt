#lang racket/base
(require racket/fixnum)

;; See "../main.rkt"

;;; pos is used for two different but related purposes: for row, col
;;; positions and for row, physical-line positions.  see the comment
;;; about the entry top-line and bot-line fields below.

(provide make-pos pos? pos-row pos-col pos=? pos<? pos<=? pos>? pos>=? index->pos)

(define-struct pos (row col)
  #:authentic
  #:sealed)

(define (pos=? p1 p2)
  (and (fx= (pos-row p1) (pos-row p2))
       (fx= (pos-col p1) (pos-col p2))))
(define (pos<? p1 p2)
  (or (fx< (pos-row p1) (pos-row p2))
      (and (fx= (pos-row p1) (pos-row p2))
           (fx< (pos-col p1) (pos-col p2)))))
(define (pos<=? p1 p2)
  (or (fx< (pos-row p1) (pos-row p2))
      (and (fx= (pos-row p1) (pos-row p2))
           (fx<= (pos-col p1) (pos-col p2)))))
(define (pos>? p1 p2)
  (or (fx> (pos-row p1) (pos-row p2))
      (and (fx= (pos-row p1) (pos-row p2))
           (fx> (pos-col p1) (pos-col p2)))))
(define (pos>=? p1 p2)
  (or (fx> (pos-row p1) (pos-row p2))
      (and (fx= (pos-row p1) (pos-row p2))
           (fx>= (pos-col p1) (pos-col p2)))))
(define (index->pos s n r c)
  ; convert index in single-string representation of entry
  ; into pos.  r and c are logical row and col at which string
  ; starts in the entry
  (let f ([i 0] [r r] [c c])
    (if (fx= i n)
        (make-pos r c)
        (if (char=? (string-ref s i) #\newline)
            (f (fx+ i 1) (fx+ r 1) 0)
            (f (fx+ i 1) r (fx+ c 1))))))
