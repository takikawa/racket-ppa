#lang scheme/base

(require "../utils/utils.ss" stxclass
         scheme/contract
         (rep type-rep)
         (private type-annotation))

(p/c [find-annotation (syntax? identifier? . -> . (or/c #f Type/c))])

(define-syntax-class lv-clause
  #:transparent
  (pattern [(v:id ...) e:expr]))

(define-syntax-class lv-clauses
  #:transparent
  (pattern (cl:lv-clause ...)
           #:with (e ...) #'(cl.e ...)
           #:with (vs ...) #'((cl.v ...) ...)))

(define-syntax-class core-expr
  #:literals (reverse letrec-syntaxes+values let-values #%plain-app
                      if letrec-values begin #%plain-lambda set! case-lambda
                      begin0 with-continuation-mark)
  #:transparent
  (pattern (let-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-values cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (letrec-syntaxes+values _ cls:lv-clauses body)
           #:with (expr ...) #'(cls.e ... body))
  (pattern (#%plain-app expr ...))
  (pattern (if expr ...))
  (pattern (with-continuation-mark expr ...))
  (pattern (begin expr ...))
  (pattern (begin0 expr ...))
  (pattern (#%plain-lambda _ e)
           #:with (expr ...) #'(e))
  (pattern (case-lambda [_ expr] ...))
  (pattern (set! _ e)
           #:with (expr ...) #'(e))
  (pattern _ 
           #:with (expr ...) #'()))

;; expr id -> type or #f
;; if there is a binding in stx of the form:
;; (let ([x (reverse name)]) e)
;; where x has a type annotation, return that annotation, otherwise #f
(define (find-annotation stx name)
  (define (find s) (find-annotation s name))
  (define (match? b)
    (syntax-parse b
      #:literals (#%plain-app reverse)
      [c:lv-clause
       #:with (#%plain-app reverse n:id) #'c.e
       #:with (v) #'(c.v ...) 
       #:when (free-identifier=? name #'n)
       (type-annotation #'v)]
      [_ #f]))
  (syntax-parse stx
    #:literals (let-values)
    [(let-values cls:lv-clauses body)
     (or (ormap match? (syntax->list #'cls))
	 (find #'body))]
    [e:core-expr
     (ormap find (syntax->list #'(e.expr ...)))]))
