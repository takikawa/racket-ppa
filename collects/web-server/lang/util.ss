#lang scheme/base
(require (for-template scheme/base)
         scheme/list
         scheme/contract)
(provide/contract
 [transformer? (parameter/c boolean?)]
 [recertify (syntax? syntax? . -> . syntax?)]
 [current-code-labeling (parameter/c (syntax? . -> . syntax?))]
 [generate-formal ((symbol?) ((or/c false/c syntax?)) . ->* . (values syntax? syntax?))]
 [formals-list (syntax? . -> . (listof syntax?))]
 [make-define-case ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [make-module-case ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [make-lang-module-begin ((bytes? . -> . (-> symbol?)) (syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [bound-identifier-member? (syntax? (listof syntax?) . -> . boolean?)])

(define transformer? (make-parameter #f))

(define (recertify old-expr expr)
  (syntax-recertify expr old-expr (current-code-inspector) #f))

(define current-code-labeling
  (make-parameter
   (lambda (stx)
     (datum->syntax stx 'error))))

(define (generate-formal sym-name [stx-base #f])
  (let ([name (datum->syntax stx-base (gensym sym-name))])
    (with-syntax ([(#%plain-lambda (formal) ref-to-formal)
                   (if (syntax-transforming?)
                       (local-expand #`(#%plain-lambda (#,name) #,name) 'expression empty)
                       #`(#%plain-lambda (#,name) #,name))])
      (values #'formal #'ref-to-formal))))

(define (formals-list stx)
  (syntax-case stx ()
    [v (identifier? #'v)
       (list #'v)]
    [(v ...)
     (syntax->list #'(v ...))]
    [(v ... . rv)
     (list* #'rv (syntax->list #'(v ...)))]))

(define ((make-define-case inner) stx)
  (recertify
   stx
   (syntax-case stx (define-values define-syntaxes define-values-for-syntax)
     [(define-values (v ...) ve)
      (let-values ([(nve) (inner #'ve)])
        (quasisyntax/loc stx
          (define-values (v ...) #,nve)))]
     [(define-syntaxes (v ...) ve)
      stx]      
     [(define-values-for-syntax (v ...) ve)
      stx]
     [(#%require spec ...)
      stx]
     [expr
      (inner #'expr)])))

(define ((make-module-case inner) stx)
  (recertify
   stx
   (syntax-case* stx (#%provide) free-identifier=?     
     [(#%provide spec ...)
      stx]
     [_
      (inner stx)])))

(define ((make-lang-module-begin make-labeling transform) stx)
  (recertify
   stx
   (syntax-case stx ()
     [(mb forms ...)
      (with-syntax ([(pmb body ...)
                     (local-expand (quasisyntax/loc stx
                                     (#%module-begin forms ...))
                                   'module-begin 
                                   empty)])
        (define base-labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax->datum stx)))))      
        (define new-defs 
          (parameterize ([current-code-labeling
                          (lambda (stx) (datum->syntax stx (base-labeling)))])
            (map transform (syntax->list #'(body ...)))))
        (quasisyntax/loc stx
          (pmb #,@new-defs)))])))

(define (bound-identifier-member? id ids)
  (ormap
   (lambda (an-id)
     (bound-identifier=? id an-id))
   ids))
