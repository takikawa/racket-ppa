#lang scheme

(provide define-keywords function-with-arity expr-with-check except err 
         ->args
         ->kwds-in
         clauses-use-kwd)

(require 
 (for-template "syn-aux-aux.ss" 
               scheme
               (rename-in lang/prim (first-order->higher-order f2h))))

#|
  transform the clauses into the initial arguments specification 
  for a new expression that instantiates the appropriate class
  
  ensure that all clauses mention only keywords specified in AllSpec or PartSpec
  move the contracts from AppSpecl and PartSpec to the clauses 
  
  run ->rec? over all used keywords to discover the presence of special clauses
  
  if anything fails, use the legal keyword to specialize the error message
|#
(define (->args stx clauses AllSpec PartSpec ->rec? legal)
  (define msg (format "not a legal clause in a ~a description" legal))
  (define Spec (append AllSpec PartSpec))
  (define kwds (map (compose (curry datum->syntax stx) car) Spec))
  (define spec (clauses-use-kwd (syntax->list clauses) ->rec? msg (->kwds-in kwds)))
  (map (lambda (x) 
         (define kw (car x))
         (define-values (key coercion)
           (let loop ([kwds kwds][Spec Spec])
             (if (free-identifier=? (car kwds) kw)
                 (values (car kwds) (cadar Spec))
                 (loop (cdr kwds) (cdr Spec)))))
         (list key (coercion (cdr x))))
       spec))

(define (clauses-use-kwd stx:list ->rec? legal-clause kwd-in?)
  (map (lambda (stx)
         (syntax-case stx ()
           [(kw . E) (kwd-in? #'kw) (begin (->rec? #'kw #'E) (cons #'kw stx))]
           [_ (raise-syntax-error #f legal-clause stx)]))
       stx:list))

;; [Listof SyntaxIdentifier] -> (Syntax -> Boolean)
(define (->kwds-in kwds)
  (lambda (k)
    (and (identifier? k) (for/or ([n kwds]) (free-identifier=? k n)))))

(define-syntax-rule (define-keywords the-list (kw coerce) ...)
  (begin
    (provide kw ...)
    (define-syntax (kw x)
      (raise-syntax-error 'kw "used out of context" x))
    ...
    (define-for-syntax the-list (list (list #'kw (coerce ''kw)) ...))))

(define-syntax (expr-with-check stx)
  (syntax-case stx ()
    [(_ check> msg)
     #`(lambda (tag)
         (lambda (p)
           (syntax-case p ()
             [(_ x) #`(check> #,tag x)]
             [_ (err tag p msg)])))]))

(define-syntax function-with-arity 
  (syntax-rules (except)
    [(_ arity)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           [_ (err tag p)])))]
    [(_ arity except extra)
     (lambda (tag)
       (lambda (p)
         (syntax-case p ()
           [(_ x) #`(proc> #,tag (f2h x) arity)]
           extra
           [_ (err tag p)])))]))

(define (err spec p . xtras)
  (raise-syntax-error (cadr spec)
                      (if (null? xtras)
                          "illegal specification"
                          (string-append "illegal specification: " (car xtras)))
                      p))
