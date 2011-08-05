#lang scheme/base

;;;
;;; <search.rkt> ---- List searching functions
;;; Time-stamp: <02/02/28 12:11:01 noel>
;;;
;;; Copyright (C) 2002 by Noel Welsh.
;;;
;;; This file is part of SRFI-1.

;;; This SRFI-1 implementation is distributed under the same terms as
;;; Racket.

;;; Author: Noel Welsh <noelwelsh@yahoo.com>

;; Commentary:

;; Based on the reference implementation by Olin Shiver and hence:

;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;     -Olin

;; Olin Shivers verified that he is fine with redistributing this code
;; under the LGPL.  (Verified personally by Eli Barzilay.)



(require srfi/optional
         "predicate.rkt"
         "util.rkt")

(provide (rename-out [my-member member])
         find
         find-tail
         any
         every
         list-index
         take-while (rename-out [take-while take-while!])
         drop-while
         span (rename-out [span span!])
         break (rename-out [break break!]))

;; Extended from R4RS to take an optional comparison argument.
(define [my-member x lis [= equal?]]
  (find-tail (lambda (y) (= x y)) lis))

;; find find-tail take-while drop-while span break any every list-index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find pred list)
  (check-arg procedure? pred 'find)
  (cond ((find-tail-internal pred list) => car)
        (else #f)))

(define (find-tail pred list)
  (check-arg procedure? pred 'find-tail)
  (find-tail-internal pred list))

(define (find-tail-internal pred list)
  (let lp ((list list))
    (and (not (null-list? list))
         (if (pred (car list)) list
             (lp (cdr list))))))

(define (take-while pred lis)
  (check-arg procedure? pred 'take-while)
  (let recur ((lis lis))
    (if (null-list? lis) '()
        (let ((x (car lis)))
          (if (pred x)
            (cons x (recur (cdr lis)))
            '())))))

(define (drop-while pred lis)
  (check-arg procedure? pred 'drop-while)
  (let lp ((lis lis))
    (cond ((null-list? lis) '())
          ((pred (car lis)) (lp (cdr lis)))
          (else lis))))

#; ; lists are immutable
(define (take-while! pred lis)
  (check-arg procedure? pred 'take-while!)
  (if (or (null-list? lis) (not (pred (car lis)))) '()
      (begin (let lp ((prev lis) (rest (cdr lis)))
               (if (pair? rest)
                 (let ((x (car rest)))
                   (if (pred x) (lp rest (cdr rest))
                       (set-cdr! prev '())))))
             lis)))

(define (span pred lis)
  (check-arg procedure? pred 'span)
  (let recur ((lis lis))
    (if (null-list? lis) (values '() '())
        (let ((x (car lis)))
          (if (pred x)
            (let-values ([(prefix suffix) (recur (cdr lis))])
              (values (cons x prefix) suffix))
            (values '() lis))))))

#; ; lists are immutable
(define (span! pred lis)
  (check-arg procedure? pred 'span!)
  (if (or (null-list? lis) (not (pred (car lis)))) (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
                      (if (null-list? rest) rest
                          (let ((x (car rest)))
                            (if (pred x) (lp rest (cdr rest))
                                (begin (set-cdr! prev '())
                                       rest)))))))
        (values lis suffix))))

(define (break pred lis) (span  (lambda (x) (not (pred x))) lis))
#; ; lists are immutable
(define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))

(define (any pred lis1 . lists)
  (check-arg procedure? pred 'any)
  (if (pair? lists)
    ;; N-ary case
    (let-values ([(heads tails) (%cars+cdrs (cons lis1 lists))])
      (and (pair? heads)
           (let lp ((heads heads) (tails tails))
             (let-values ([(next-heads next-tails) (%cars+cdrs tails)])
               (if (pair? next-heads)
                 (or (apply pred heads) (lp next-heads next-tails))
                 (apply pred heads)))))) ; Last PRED app is tail call.
    ;; Fast path
    (and (not (null-list? lis1))
         (let lp ((head (car lis1)) (tail (cdr lis1)))
           (if (null-list? tail)
             (pred head)                ; Last PRED app is tail call.
             (or (pred head) (lp (car tail) (cdr tail))))))))

;(define (every pred list)              ; Simple definition.
;  (let lp ((list list))                ; Doesn't return the last PRED value.
;    (or (not (pair? list))
;        (and (pred (car list))
;             (lp (cdr list))))))

(define (every pred lis1 . lists)
  (check-arg procedure? pred 'every)
  (if (pair? lists)
    ;; N-ary case
    (let-values ([(heads tails) (%cars+cdrs (cons lis1 lists))])
      (or (not (pair? heads))
          (let lp ((heads heads) (tails tails))
            (let-values ([(next-heads next-tails) (%cars+cdrs tails)])
              (if (pair? next-heads)
                (and (apply pred heads) (lp next-heads next-tails))
                (apply pred heads)))))) ; Last PRED app is tail call.
    ;; Fast path
    (or (null-list? lis1)
        (let lp ((head (car lis1))  (tail (cdr lis1)))
          (if (null-list? tail)
            (pred head) ; Last PRED app is tail call.
            (and (pred head) (lp (car tail) (cdr tail))))))))

(define (list-index pred lis1 . lists)
  (check-arg procedure? pred 'list-index)
  (if (pair? lists)
    ;; N-ary case
    (let lp ((lists (cons lis1 lists)) (n 0))
      (let-values ([(heads tails) (%cars+cdrs lists)])
        (and (pair? heads)
             (if (apply pred heads) n
                 (lp tails (+ n 1))))))
    ;; Fast path
    (let lp ((lis lis1) (n 0))
      (and (not (null-list? lis))
           (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))

;;; search.rkt ends here
