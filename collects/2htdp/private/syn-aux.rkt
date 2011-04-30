#lang racket

(provide define-keywords 
         DEFAULT
         ;; constraint: the first kw is the original one 
         ;; and it is also the name of the field in the class
         ->args
         function-with-arity expr-with-check except err 
         ->kwds-in
         clauses-use-kwd
         contains-clause?)

(require 
 (for-syntax "syn-aux-aux.rkt" syntax/parse)
 (for-template "syn-aux-aux.rkt"
               racket
               (rename-in lang/prim (first-order->higher-order f2h))))

(define-syntax (DEFAULT stx)
  (raise-syntax-error 'DEFAULT "used out of context" stx))

(define-syntax (define-keywords stx)
  (syntax-parse stx #:literals (DEFAULT) 
    [(_ the-list super-list define-create
        (kw:identifier 
         (~optional kw-alt:identifier #:defaults ((kw-alt #'kw)))
         (~optional (~seq DEFAULT default:expr))
         coerce:expr) ...)
     (let* ([defs (attribute default)])
       #`(begin
           ;; define and create list of keywords and associated values 
           (define-for-syntax the-list
             (list* (list #'kw #'kw-alt (coerce ''kw) default) ... super-list))
           ;; define and provide keywords
           (provide (rename-out (kw  kw-alt) ...))
           (provide kw ...) 
           (define-syntaxes (kw ...)
             (values (lambda (x)
                       (raise-syntax-error 'kw "used out of context" x))
                     ...))
           
           ;; a macro for creating functions that instantiate the proper object
           ;; (define-create para ...) :: additional parameters for the new func
           (define-syntax (define-create stx)
             (syntax-case stx ()
               [(_ para (... ...))
                (let*-values
                    ([(kwds defs)
                      (values (map car the-list) '())]
                     ;; the defaults list defs is no longer needed
                     [(args) (lambda (para*)
                               (append para* (foldr cons '() kwds)))]
                     [(body) (lambda (para*)
                               (map (lambda (x) `(,x ,x)) (append para* kwds)))])
                  (let ([para* (syntax->list #'(para (... ...)))])
                    #`(lambda (%)
                        (lambda #,(args para*)
                          (lambda ()
                            (new % #,@(body para*)))))))]))))]))

#|
  transform the clauses into the initial arguments specification 
  for a new expression that instantiates the appropriate class
  
  ensure that the initial state (state0) is not in the shape of a clause

  ensure that all clauses mention only keywords specified in AllSpec or PartSpec
  move the contracts from AppSpecl and PartSpec to the clauses 
  
  run ->rec? over all used keywords to discover the presence of special clauses
  
  if anything fails, use the legal keyword to specialize the error message
|#
(define (->args tag stx state0 clauses Spec ->rec? legal)
  (define msg (format "not a legal clause in a ~a description" legal))
  (define kwds (map (compose (curry datum->syntax stx) car) Spec))
  (define spec (clauses-use-kwd (syntax->list clauses) ->rec? msg kwds))
  (duplicates? tag spec)
  (not-a-clause tag stx state0 kwds)
  (map (lambda (s) 
         (define kw (first s))
         (define kw-alt (second s))
         (define r
           (let loop ([spec spec])
             (cond
               [(null? spec) #false]
               [(or (free-identifier=? (caar spec) kw)
                    (free-identifier=? (caar spec) kw-alt))
                (syntax->list (cdar spec))]
               [else (loop (cdr spec))])))
         (if r ((third s) r) (fourth s)))
       Spec))

(define (contains-clause? kw clause-list)
  (memf (lambda (clause) (free-identifier=? kw (car (syntax->list clause)))) clause-list))


;; Syntax -> Syntax 
;; eventually: convert syntax to keyword
(define (mk-kwd key)
  (define key:id (symbol->string (syntax-e key)))
  (define key:wd (string->keyword key:id))
  key:wd)

;; Symbol Syntax Syntax [Listof Kw] -> true
;; effect: if state0 looks like a clause, raise special error 
(define (not-a-clause tag stx state0 kwds)
  (syntax-case state0 ()
    [(kw . E) 
     ((->kwds-in kwds) #'kw) 
     (raise-syntax-error tag "missing initial state" stx)]
    [_ #t]))

;; Symbol [Listof kw] -> true
;; effect: raise syntax error about duplicated clause 
(define (duplicates? tag lox)
  (let duplicates? ([lox lox])
    (cond
      [(empty? lox) false]
      [else
       (let* ([f (caar lox)]
              [id (syntax-e f)]
              [x (memf (lambda (x) (free-identifier=? (car x) f)) (rest lox))])
         (if x 
             (raise-syntax-error tag (format "duplicate ~a clause" id) (cdar x))
             (duplicates? (rest lox))))])))

;; check whether rec? occurs, produce list of keyword x clause pairs 
(define (clauses-use-kwd stx:list ->rec? legal-clause kwds)
  (define kwd-in? (->kwds-in kwds))
  (define double (string-append legal-clause ", ~a has been redefined"))
  (map (lambda (stx)
         (syntax-case stx ()
           [(kw . E) (kwd-in? #'kw) (begin (->rec? #'kw #'E) (cons #'kw stx))]
           [(kw . E)
            (let ([kw (syntax-e #'kw)])
              (if (member kw (map syntax-e kwds))
                  (raise-syntax-error #f (format double kw) stx)
                  (raise-syntax-error #f legal-clause stx)))]
           [_ (raise-syntax-error #f legal-clause stx)]))
       stx:list))

;; [Listof SyntaxIdentifier] -> (Syntax -> Boolean)
(define (->kwds-in kwds)
  (lambda (k)
    (and (identifier? k) (for/or ([n kwds]) (free-identifier=? k n)))))

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
