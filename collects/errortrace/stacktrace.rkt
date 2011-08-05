#lang racket/base
(require racket/unit
         syntax/kerncase
         syntax/stx
         (for-template racket/base)
         (for-syntax racket/base)) ; for matching

(provide stacktrace@ stacktrace^ stacktrace-imports^)
(define-signature stacktrace-imports^
  (with-mark
   
   test-coverage-enabled
   test-covered
   initialize-test-coverage-point
   
   profile-key
   profiling-enabled
   initialize-profile-point
   register-profile-start
   register-profile-done))

(define-signature stacktrace^
  (annotate-top
   annotate
   make-st-mark
   st-mark-source
   st-mark-bindings))

(define-unit stacktrace@
  (import stacktrace-imports^)
  (export stacktrace^)
  
  (define (short-version v depth)
    (cond
      [(identifier? v) (syntax-e v)]
      [(null? v) null]
      [(vector? v) (if (zero? depth)
                       #(....)
                       (list->vector
                        (short-version (vector->list v) (sub1 depth))))]
      [(box? v) (if (zero? depth)
                    #&(....)
                    (box (short-version (unbox v) (sub1 depth))))]
      [(pair? v)
       (cond
         [(zero? depth) '(....)]
         [(memq (syntax-e (car v)) '(#%app #%top))
          (short-version (cdr v) depth)]
         [else
          (cons (short-version (car v) (sub1 depth))
                (short-version (cdr v) (sub1 depth)))])]
      [(syntax? v) (short-version (syntax-e v) depth)]
      [else v]))
  
  (define (make-st-mark stx)
    (unless (syntax? stx)
      (error 'make-st-mark
             "expected syntax object as argument, got ~e" stx))
    (cond
      [(syntax-source stx)
       #`(quote (#,(short-version stx 10)
                 #,(syntax-source stx)
                 #,(syntax-line stx)
                 #,(syntax-column stx)
                 #,(syntax-position stx)
                 #,(syntax-span stx)))]
      [else #f]))
  (define (st-mark-source src)
    (and src
         (datum->syntax #f (car src) (cdr src) #f)))
  
  (define (st-mark-bindings x) null)
  
  
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Test case coverage instrumenter
 
  ;; The next procedure is called by `annotate' and `annotate-top' to wrap
  ;; expressions with test suite coverage information.  Returning the
  ;; first argument means no tests coverage information is collected.
 
  ;; test-coverage-point : syntax syntax phase -> syntax
  ;; sets a test coverage point for a single expression
  (define (test-coverage-point body expr phase)
    (if (and (test-coverage-enabled)
             (zero? phase)
             (syntax-position expr))
      (begin (initialize-test-coverage-point expr)
             (let ([thunk (test-covered expr)])
               (cond [(procedure? thunk)
                      (with-syntax ([body body] [thunk thunk])
                        #'(begin (#%plain-app thunk) body))]
                     [(syntax? thunk)
                      (with-syntax ([body body] [thunk thunk])
                        #'(begin thunk body))]
                     [else body])))
      body))



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Profiling instrumenter
  
  ;; profile-point :
  ;;   (syntax[list of exprs] symbol-or-#f syntax boolean
  ;;    -> syntax[list of exprs])
  
  ;; This procedure is called by `annotate' and `annotate-top' to wrap
  ;; expressions with profile collecting information.  Returning the
  ;; first argument means no profiling information is collected.
  
  ;; The second argument is the point's inferred name, if any, the third
  ;; argument is the source expression, and the fourth argument is #t for
  ;; a transformer expression and #f for a normal expression.
  
  (define (profile-point bodies name expr phase)
    (let ([key (gensym 'profile-point)])
      (initialize-profile-point key name expr)
      (with-syntax ([key (datum->syntax #f key (quote-syntax here))]
                    [start (datum->syntax
                            #f (gensym) (quote-syntax here))]
                    [profile-key (datum->syntax
                                  #f profile-key (quote-syntax here))]
                    [register-profile-start register-profile-start]
                    [register-profile-done register-profile-done])
        (with-syntax ([rest
                       (insert-at-tail*
                        (syntax (#%plain-app register-profile-done 'key start))
                        bodies
                        phase)])
          (syntax
           (let ([start (#%plain-app register-profile-start 'key)])
             (with-continuation-mark 'profile-key 'key
               (begin . rest))))))))
  
  (define (insert-at-tail* e exprs phase)
    (let ([new
           (rebuild exprs
                    (let loop ([exprs exprs])
                      (if (stx-null? (stx-cdr exprs))
                          (list (cons (stx-car exprs)
                                      (insert-at-tail
                                       e (stx-car exprs) phase)))
                          (loop (stx-cdr exprs)))))])
      (if (syntax? exprs)
          (rearm exprs new)
          new)))
  
  (define (insert-at-tail se sexpr phase)
    (with-syntax ([expr sexpr]
                  [e se])
      (kernel-syntax-case/phase sexpr phase
        ;; negligible time to eval
        [id
         (identifier? sexpr)
         (syntax (begin e expr))]
        [(quote _) (syntax (begin e expr))]
        [(quote-syntax _) (syntax (begin e expr))]
        [(#%top . d) (syntax (begin e expr))]
        [(#%variable-reference . d) (syntax (begin e expr))]
        
        ;; No tail effect, and we want to account for the time
        [(#%plain-lambda . _) (syntax (begin0 expr e))]
        [(case-lambda . _) (syntax (begin0 expr e))]
        [(set! . _) (syntax (begin0 expr e))]
        
        [(let-values bindings . body)
         (insert-at-tail* se sexpr phase)]
        [(letrec-values bindings . body)
         (insert-at-tail* se sexpr phase)]
        
        [(begin . _)
         (insert-at-tail* se sexpr phase)]
        [(with-continuation-mark . _)
         (insert-at-tail* se sexpr phase)]
        
        [(begin0 body ...)
         (rearm sexpr (syntax (begin0 body ... e)))]
        
        [(if test then else)
         ;; WARNING: se inserted twice!
         (rearm
          sexpr
          (rebuild
           sexpr
           (list
            (cons #'then (insert-at-tail se (syntax then) phase))
            (cons #'else (insert-at-tail se (syntax else) phase)))))]
        
        [(#%plain-app . rest)
         (if (stx-null? (syntax rest))
             ;; null constant
             (syntax (begin e expr))
             ;; application; exploit guaranteed left-to-right evaluation
             (insert-at-tail* se sexpr phase))]
        
        [_else
         (error 'errortrace
                "unrecognized (non-top-level) expression form: ~.s"
                (syntax->datum sexpr))])))
  
  (define (profile-annotate-lambda name expr clause bodys-stx phase)
    (let* ([bodys (stx->list bodys-stx)]
           [bodyl (map (lambda (e) (annotate e phase))
                       bodys)])
      (rebuild clause
               (if (profiling-enabled)
                   (let ([prof-expr
                          (profile-point bodyl name expr phase)])
                     ;; Tell rebuild to replace first expressions with
                     ;; (void), and replace the last expression with
                     ;; prof-expr:
                     (let loop ([bodys bodys])
                       (if (null? (cdr bodys))
                           (list (cons (car bodys) prof-expr))
                           (cons (cons (car bodys) #'(#%plain-app void))
                                 (loop (cdr bodys))))))
                   ;; Map 1-to-1:
                   (map cons bodys bodyl)))))
  
  (define (keep-lambda-properties orig new)
    (let ([p (syntax-property orig 'method-arity-error)]
          [p2 (syntax-property orig 'inferred-name)])
      (let ([new (if p
                   (syntax-property new 'method-arity-error p)
                   new)])
        (if p2
            (syntax-property new 'inferred-name p2)
            new))))
  
  (define (annotate-let expr phase varss-stx rhss-stx bodys-stx)
    (let ([varss (syntax->list varss-stx)]
          [rhss  (syntax->list rhss-stx)]
          [bodys (syntax->list bodys-stx)])
      (let ([rhsl (map
                   (lambda (vars rhs)
                     (annotate-named
                      (syntax-case vars () [(id) (syntax id)] [_else #f])
                      rhs
                      phase))
                   varss
                   rhss)]
            [bodyl (map (lambda (body) (annotate body phase))
                        bodys)])
        (rebuild expr (append (map cons bodys bodyl)
                              (map cons rhss rhsl))))))
  
  (define (annotate-seq expr bodys-stx annotate phase)
    (let* ([bodys (syntax->list bodys-stx)]
           [bodyl (map (lambda (b) (annotate b phase)) bodys)])
      (rebuild expr (map cons bodys bodyl))))
  
  (define orig-inspector (current-code-inspector))
  
  (define (rearm orig new)
    (syntax-rearm new orig))
  
  (define (disarm orig)
    (syntax-disarm orig orig-inspector))
  
  (define (rebuild expr replacements)
    (let loop ([expr expr] [same-k (lambda () expr)] [diff-k (lambda (x) x)])
      (let ([a (assq expr replacements)])
        (cond
          [a (diff-k (cdr a))]
          [(pair? expr)
           (loop (car expr)
                 (lambda ()
                   (loop (cdr expr) same-k
                         (lambda (y) (diff-k (cons (car expr) y)))))
                 (lambda (x)
                   (loop (cdr expr)
                         (lambda () (diff-k (cons x (cdr expr))))
                         (lambda (y) (diff-k (cons x y))))))]
          [(vector? expr)
           (loop (vector->list expr) same-k
                 (lambda (x) (diff-k (list->vector x))))]
          [(box? expr)
           (loop (unbox expr) same-k (lambda (x) (diff-k (box x))))]
          [(syntax? expr)
           (if (identifier? expr)
             (same-k)
             (loop (syntax-e expr) same-k
                   (lambda (x) (diff-k (datum->syntax expr x expr expr)))))]
          [else (same-k)]))))
  
  (define (append-rebuild expr end)
    (cond
      [(syntax? expr)
       (datum->syntax expr
                      (append-rebuild (syntax-e expr) end)
                      expr
                      expr)]
      [(pair? expr)
       (cons (car expr) (append-rebuild (cdr expr) end))]
      [(null? expr)
       (list end)]
      [else
       (error 'append-rebuild "shouldn't get here")]))
  
  (define (one-name names-stx)
    (let ([l (syntax->list names-stx)])
      (and (pair? l)
           (null? (cdr l))
           (car l))))
  
  (define (make-annotate top? name)
    (lambda (expr phase)
      (define disarmed-expr (disarm expr))
      (test-coverage-point
       (kernel-syntax-case/phase disarmed-expr phase
         [_
          (identifier? expr)
          (let ([b (identifier-binding expr phase)])
            (cond
             [(eq? 'lexical b)
              ;; lexical variable - no error possile
              expr]
             [(and (pair? b) (let-values ([(base rel) (module-path-index-split (car b))])
                               (equal? '(quote #%kernel) base)))
              ;; built-in - no error possible
              expr]
             [else
              ;; might be undefined/uninitialized
              (with-mark expr expr)]))]
         
         [(#%top . id)
          ;; might be undefined/uninitialized
          (with-mark expr expr)]
         [(#%variable-reference . _)
          ;; no error possible
          expr]
         
         [(define-values names rhs)
          top?
          ;; Can't put annotation on the outside
          (let* ([marked 
                  (with-mark expr
                             (annotate-named
                              (one-name #'names)
                              (syntax rhs)
                              phase))]
                 [with-coverage
                  (let loop ([stx #'names]
                             [obj marked])
                    (cond
                     [(not (syntax? stx)) obj]
                     [(identifier? stx)
                      (test-coverage-point obj stx phase)]
                     [(pair? (syntax-e stx))
                      (loop (car (syntax-e stx))
                            (loop (cdr (syntax-e stx))
                                  obj))]
                     [else obj]))])
            (rearm
             expr
             (rebuild 
              disarmed-expr 
              (list (cons #'rhs with-coverage)))))]
         [(begin . exprs)
          top?
          (rearm
           expr
           (annotate-seq disarmed-expr
                         (syntax exprs)
                         annotate-top phase))]
         [(define-syntaxes (name ...) rhs)
          top?
          (let ([marked (with-mark expr
                                   (annotate-named
                                    (one-name #'(name ...))
                                    (syntax rhs)
                                    (add1 phase)))])
            (rearm
             expr
             (rebuild disarmed-expr (list (cons #'rhs marked)))))]
         
         [(define-values-for-syntax (name ...) rhs)
          top?
          (let ([marked (with-mark expr
                                   (annotate-named
                                    (one-name (syntax (name ...)))
                                    (syntax rhs)
                                    (add1 phase)))])
            (rearm
             expr
             (rebuild disarmed-expr (list (cons #'rhs marked)))))]
         
         [(module name init-import mb)
          (syntax-case (disarm #'mb) ()
            [(__plain-module-begin body ...)
             ;; Just wrap body expressions
             (let ([bodys (syntax->list (syntax (body ...)))])
               (let ([bodyl (map (lambda (b)
                                   (annotate-top b 0))
                                 bodys)]
                     [mb #'mb])
                 (rearm
                  expr
                  (rebuild
                   disarmed-expr
                   (list (cons
                          mb
                          (rearm
                           mb
                           (rebuild mb (map cons bodys bodyl)))))))))])]
         
         [(#%expression e)
          top?
          (rearm expr #`(#%expression #,(annotate (syntax e) phase)))]
         
         ;; No way to wrap
         [(#%require i ...) expr]
         ;; No error possible (and no way to wrap)
         [(#%provide i ...) expr]
         
         
         ;; No error possible
         [(quote _)
          expr]
         [(quote-syntax _)
          expr]
         
         ;; Wrap body, also a profile point
         [(#%plain-lambda args . body)
          (rearm
           expr
           (keep-lambda-properties
            expr
            (profile-annotate-lambda name expr expr (syntax body)
                                     phase)))]
         [(case-lambda clause ...)
          (with-syntax ([([args . body] ...)
                         (syntax (clause ...))])
            (let* ([clauses (syntax->list (syntax (clause ...)))]
                   [clausel (map
                             (lambda (body clause)
                               (profile-annotate-lambda
                                name expr clause body phase))
                             (syntax->list (syntax (body ...)))
                             clauses)])
              (rearm
               expr
               (keep-lambda-properties
                expr
                (rebuild disarmed-expr (map cons clauses clausel))))))]
         
         ;; Wrap RHSs and body
         [(let-values ([vars rhs] ...) . body)
          (with-mark expr
                     (rearm
                      expr
                      (annotate-let disarmed-expr phase
                                    (syntax (vars ...))
                                    (syntax (rhs ...))
                                    (syntax body))))]
         [(letrec-values ([vars rhs] ...) . body)
          (let ([fm (rearm
                     expr
                     (annotate-let disarmed-expr phase
                                   (syntax (vars ...))
                                   (syntax (rhs ...))
                                   (syntax body)))])
            (kernel-syntax-case/phase expr phase
              [(lv ([(var1) (#%plain-lambda . _)]) var2)
               (and (identifier? #'var2)
                    (free-identifier=? #'var1 #'var2))
               fm]
              [_
               (with-mark expr fm)]))]

         ;; Wrap RHS
         [(set! var rhs)
          (let ([new-rhs (annotate-named
                          (syntax var)
                          (syntax rhs)
                          phase)])
            ;; set! might fail on undefined variable, or too many values:
            (with-mark expr
                       (rearm
                        expr
                        (rebuild disarmed-expr (list (cons #'rhs new-rhs))))))]
         
         ;; Wrap subexpressions only
         [(begin e)
          ;; Single expression: no mark
          (rearm
           expr
           #`(begin #,(annotate (syntax e) phase)))]
         [(begin . body)
          (with-mark expr
                     (rearm
                      expr
                      (annotate-seq disarmed-expr #'body annotate phase)))]
         [(begin0 . body)
          (with-mark expr
                     (rearm
                      expr
                      (annotate-seq disarmed-expr #'body annotate phase)))]
         [(if tst thn els)
          (let ([w-tst (annotate (syntax tst) phase)]
                [w-thn (annotate (syntax thn) phase)]
                [w-els (annotate (syntax els) phase)])
            (with-mark expr
                       (rearm
                        expr
                        (rebuild disarmed-expr (list (cons #'tst w-tst)
                                                     (cons #'thn w-thn)
                                                     (cons #'els w-els))))))]
         [(if tst thn)
          (let ([w-tst (annotate (syntax tst) phase)]
                [w-thn (annotate (syntax thn) phase)])
            (with-mark expr
                       (rearm
                        expr
                        (rebuild disarmed-expr (list (cons #'tst w-tst)
                                                     (cons #'thn w-thn))))))]
         [(with-continuation-mark . body)
          (with-mark expr
                     (rearm
                      expr
                      (annotate-seq disarmed-expr (syntax body)
                                    annotate phase)))]
         
         ;; Wrap whole application, plus subexpressions
         [(#%plain-app . body)
          (cond
           [(stx-null? (syntax body))
            ;; It's a null:
            expr]
           [(syntax-case* expr (#%plain-app void)
                          (if (positive? phase)
                              free-transformer-identifier=?
                              free-identifier=?)
              [(#%plain-app void) #t]
              [_else #f])
            ;; It's (void):
            expr]
           [else
            (with-mark expr (rearm
                             expr
                             (annotate-seq disarmed-expr (syntax body)
                                           annotate phase)))])]
         
         [_else
          (error 'errortrace "unrecognized expression form~a: ~.s"
                 (if top? " at top-level" "")
                 (syntax->datum expr))])
       expr
       phase)))
  
  (define annotate (make-annotate #f #f))
  (define annotate-top (make-annotate #t #f))
  (define (annotate-named name expr phase)
    ((make-annotate #t name) expr phase)))
