#lang scheme/base
(require (for-template scheme/base)
         scheme/pretty
         scheme/list
         scheme/contract
         syntax/kerncase)
(provide/contract
 [transformer? (parameter/c boolean?)]
 [recertify (syntax? syntax? . -> . syntax?)]
 [recertify* (syntax? (listof syntax?) . -> . (listof syntax?))]
 [recertify/new-defs (syntax? (-> (values syntax? (listof syntax?))) . -> . (values syntax? (listof syntax?)))]
 [current-code-labeling (parameter/c (syntax? . -> . syntax?))]
 [generate-formal ((symbol?) ((or/c false/c syntax?)) . ->* . (values syntax? syntax?))]
 [formals-list (syntax? . -> . (listof syntax?))]
 [make-define-case/new-defs ((syntax? . -> . (values syntax? (listof syntax?))) . -> . (syntax? . -> . (listof syntax?)))]
 [make-module-case/new-defs ((syntax? . -> . (listof syntax?)) . -> . (syntax? . -> . (listof syntax?)))]
 [make-lang-module-begin ((bytes? . -> . (-> symbol?)) (syntax? . -> . (listof syntax?)) . -> . (syntax? . -> . syntax?))]
 [bound-identifier-member? (syntax? (listof syntax?) . -> . boolean?)])

(define transformer? (make-parameter #f))

(define (recertify old-expr expr)
  (syntax-recertify expr old-expr (current-code-inspector) #f))

(define (recertify* old-expr exprs)
  (map (lambda (expr)
         (syntax-recertify expr old-expr (current-code-inspector) #f))
       exprs))

(define (recertify/new-defs old-expr thunk)
  (call-with-values
   thunk
   (lambda (expr new-defs)
     (values (recertify old-expr expr)
             (recertify* old-expr new-defs)))))

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

(define ((make-define-case/new-defs inner) stx)
  (recertify*
   stx
   (syntax-case stx (define-values define-syntaxes define-values-for-syntax)
     [(define-values (v ...) ve)
      (let-values ([(nve defs) (inner #'ve)])
        (append 
         defs
         (list (quasisyntax/loc stx
                 (define-values (v ...) #,nve)))))]
     [(define-syntaxes (v ...) ve)
      (list stx)]      
     [(define-values-for-syntax (v ...) ve)
      (list stx)]
     [(#%require spec ...)
      (list stx)]
     [expr
      (let-values ([(nexpr defs) (inner #'expr)])
        (append defs (list nexpr)))])))

(define ((make-module-case/new-defs inner) stx)
  (recertify*
   stx
   (syntax-case* stx (#%provide) free-identifier=?     
     [(#%provide spec ...)
      (list stx)]
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
            (apply append (map transform (syntax->list #'(body ...))))))
        #;(pretty-print (syntax->datum #`(pmb #,@new-defs)))
        (quasisyntax/loc stx
          (pmb #,@new-defs)))])))

(define (bound-identifier-member? id ids)
  (ormap
   (lambda (an-id)
     (bound-identifier=? id an-id))
   ids))
