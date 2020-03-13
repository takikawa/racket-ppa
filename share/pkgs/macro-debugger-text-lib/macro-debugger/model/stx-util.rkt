#lang racket/base
(require syntax/stx)
(provide stx->datum
         stxd-car
         stxd-cdr
         syntaxish?
         stx-disarm
         resyntax
         restx
         syntax-armed?
         syntax-armed/tainted?
         syntax-unarmed?
         property:unlocked-by-expander
         property:artificial
         datum->artificial-syntax
         syntax-artificial?)

(define (stx->datum x)
  (syntax->datum (datum->syntax #f x)))

(define (stxd-car x) (stx-car (stx-disarm x)))
(define (stxd-cdr x) (stx-cdr (stx-disarm x)))

(define (syntaxish? x)
  (or (syntax? x)
      (null? x)
      (and (pair? x)
           (syntaxish? (car x))
           (syntaxish? (cdr x)))))

(define insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

(define (stx-disarm stx)
  (if (syntax? stx) (syntax-disarm stx insp) stx))

(define (resyntax v stx [dstx (stx-disarm stx)] #:rearm? [rearm? #t])
  (unless (and (syntax? stx) (syntax? dstx))
    (error 'resyntax "not syntax: ~e, ~e" stx dstx))
  (let* ([vstx (datum->artificial-inner-syntax v)]
         [vstx (datum->syntax dstx vstx stx stx)]
         [vstx (if rearm? (syntax-rearm vstx stx) vstx)])
    (if (eq? v vstx) vstx (mark-artificial vstx))))

(define (restx v stx [dstx (stx-disarm stx)] #:rearm? [rearm? #t])
  (if (syntax? stx) (resyntax v stx dstx #:rearm? rearm?) v))

(define (syntax-armed? stx)
  (and (not (syntax-tainted? stx))
       (syntax-tainted? (datum->syntax stx #f))))

(define (syntax-armed/tainted? stx)
  (syntax-tainted? (datum->syntax stx #f)))

(define (syntax-unarmed? stx)
  (not (syntax-armed/tainted? stx)))

;; Used to communicate with syntax-browser
(define property:unlocked-by-expander (gensym 'unlocked-by-expander))
(define property:artificial (gensym 'artificial))

(define (mark-artificial stx)
  (syntax-property stx property:artificial #t))

(define (datum->artificial-syntax x)
  (let loop ([x x])
    (cond [(pair? x) (mark-artificial (datum->syntax #f (datum->artificial-inner-syntax x)))]
          [(syntax? x) x]
          [else (mark-artificial (datum->syntax #f x))])))

(define (datum->artificial-inner-syntax x)
  (let tailloop ([x x])
    (cond [(pair? x) (cons (datum->artificial-syntax (car x)) (tailloop (cdr x)))]
          [(null? x) null]
          [else (datum->artificial-syntax x)])))

(define (syntax-artificial? stx)
  (syntax-property stx property:artificial))
