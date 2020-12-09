#lang racket/base

(provide count-meta-levels
         generate-key-imports
         generate-key-imports-at-phase)

(define base (variable-reference->module-base-phase (#%variable-reference)))

(define ((count-meta-levels phase) expr)
  (syntax-case expr ()
    [(bfs . exprs)
     (and (identifier? #'bfs) (free-identifier=? #'bfs #'begin-for-syntax phase base))
     (add1 (apply max 0 (map (count-meta-levels (add1 phase)) (syntax->list #'exprs))))]
    [(ds . _)
     (and (identifier? #'ds) (free-identifier=? #'ds #'define-syntaxes phase base))
     1]
    [(b . exprs)
     (and (identifier? #'b) (free-identifier=? #'b #'begin phase base))
     (apply max 0 (map (count-meta-levels phase) (syntax->list #'exprs)))]
    [_ 0]))


;; generate imports for errortrace/errortrace-key at (syntax-local-phase-level)
(define (generate-key-imports meta-depth)
  (generate-key-imports-at-phase meta-depth (syntax-local-phase-level)))

(define (generate-key-imports-at-phase meta-depth phase)
  (syntax-shift-phase-level
   (let loop ([meta-depth meta-depth])
     (let ([e ((make-syntax-introducer)
               #`(#%require (for-meta #,meta-depth
                                      errortrace/errortrace-key)))])
       (if (zero? meta-depth)
           e
           #`(begin #,e #,(loop (sub1 meta-depth))))))
   (- phase base)))
