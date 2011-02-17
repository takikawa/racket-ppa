#lang scheme/base

(require syntax/parse syntax/id-table scheme/dict unstable/syntax
         "../utils/utils.rkt" racket/unsafe/ops
         (for-template scheme/base scheme/math racket/flonum scheme/unsafe/ops)
         (types abbrev)
         (optimizer utils float))

(provide float-complex-opt-expr
         float-complex-arith-opt-expr
         unboxed-float-complex-opt-expr
         float-complex-call-site-opt-expr
         unboxed-vars-table unboxed-funs-table)


;; contains the bindings which actually exist as separate bindings for each component
;; associates identifiers to lists (real-binding imag-binding)
(define unboxed-vars-table (make-free-id-table))

;; associates the names of functions with unboxed args (and whose call sites have to
;; be modified) to the arguments which can be unboxed and those which have to be boxed
;; entries in the table are of the form:
;; ((unboxed ...) (boxed ...))
;; all these values are indices, since arg names don't make sense for call sites
;; the new calling convention for these functions have all real parts of unboxed
;; params first, then all imaginary parts, then all boxed arguments
(define unboxed-funs-table (make-free-id-table))

;; it's faster to take apart a complex number and use unsafe operations on
;; its parts than it is to use generic operations
;; we keep the real and imaginary parts unboxed as long as we stay within
;; complex operations
(define-syntax-class unboxed-float-complex-opt-expr
  #:commit

  (pattern (#%plain-app (~and op (~literal +))
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
           #:when (isoftype? this-syntax -FloatComplex)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary float complex" #'op)
                  #`(#,@(append (syntax->list #'(c1.bindings ... c2.bindings ... cs.bindings ... ...))
                                (let ()
                                   ;; we can skip the real parts of imaginaries (#f) and vice versa
                                   (define (skip-0s l)
                                     (let ((l (filter syntax->datum (syntax->list l))))
                                       (case (length l)
                                         ((0) #'0.0)
                                         ((1) (car l))
                                         (else
                                          (for/fold ((o (car l)))
                                              ((e (cdr l)))
                                            #`(unsafe-fl+ #,o #,e))))))
                                   (list
                                    #`((real-binding) #,(skip-0s #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                                    #`((imag-binding) #,(skip-0s #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))))))))
  
  (pattern (#%plain-app (~and op (~literal -))
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
           #:when (isoftype? this-syntax -FloatComplex)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary float complex" #'op)
                  #`(#,@(append (syntax->list #'(c1.bindings ... c2.bindings ... cs.bindings ... ...))
                                (let ()
                                  ;; unlike addition, we simply can't skip real parts of imaginaries
                                  (define (skip-0s l)
                                    (let* ((l1 (syntax-map (lambda (x) (if (syntax->datum x) x #'0.0)) l))
                                           ;; but we can skip all but the first 0
                                           (l2 (filter (lambda (x) (not (equal? (syntax->datum x) 0.0)))
                                                       (cdr l1))))
                                      (case (length l2)
                                        ((0) (car l1))
                                        (else
                                         (for/fold ((o (car l1)))
                                             ((e l2))
                                           #`(unsafe-fl- #,o #,e))))))
                                  (list
                                   #`((real-binding) #,(skip-0s #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                                   #`((imag-binding) #,(skip-0s #'(c1.imag-binding c2.imag-binding cs.imag-binding ...)))))))))
  
  (pattern (#%plain-app (~and op (~literal *))
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
           #:when (or (isoftype? this-syntax -FloatComplex) (isoftype? this-syntax -Number))
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary float complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-binding and imag-binding
                     #,@(let ((lr (syntax-map (lambda (x) (if (syntax->datum x) x #'0.0))
                                              #'(c1.real-binding c2.real-binding cs.real-binding ...)))
                              (li (syntax-map (lambda (x) (if (syntax->datum x) x #'0.0))
                                              #'(c1.imag-binding c2.imag-binding cs.imag-binding ...))))
                          (let loop ([o1 (car lr)]
                                     [o2 (car li)]
                                     [e1 (cdr lr)]
                                     [e2 (cdr li)]
                                     [rs (append (syntax-map (lambda (x) (unboxed-gensym "unboxed-real-"))
                                                             #'(cs.real-binding ...))
                                                 (list #'real-binding))]
                                     [is (append (syntax-map (lambda (x) (unboxed-gensym "unboxed-imag-"))
                                                             #'(cs.imag-binding ...))
                                                 (list #'imag-binding))]
                                     [res '()])
                            (if (null? e1)
                                (reverse res)
                                (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is)
                                      ;; complex multiplication, imag part, then real part (reverse)
                                      ;; we eliminate operations on the imaginary parts of reals
                                      (let ((o-real? (equal? (syntax->datum o2) 0.0))
                                            (e-real? (equal? (syntax->datum (car e2)) 0.0)))
                                        (list* #`((#,(car is))
                                                  #,(cond ((and o-real? e-real?) #'0.0)
                                                          (o-real? #`(unsafe-fl* #,o1 #,(car e2)))
                                                          (e-real? #`(unsafe-fl* #,o2 #,(car e1)))
                                                          (else
                                                           #`(unsafe-fl+ (unsafe-fl* #,o2 #,(car e1))
                                                                         (unsafe-fl* #,o1 #,(car e2))))))
                                               #`((#,(car rs))
                                                  #,(cond ((or o-real? e-real?)
                                                           #`(unsafe-fl* #,o1 #,(car e1)))
                                                          (else
                                                           #`(unsafe-fl- (unsafe-fl* #,o1 #,(car e1))
                                                                         (unsafe-fl* #,o2 #,(car e2))))))
                                             res)))))))))
  
  (pattern (#%plain-app (~and op (~literal /))
                        c1:unboxed-float-complex-opt-expr
                        c2:unboxed-float-complex-opt-expr
                        cs:unboxed-float-complex-opt-expr ...)
           #:when (or (isoftype? this-syntax -FloatComplex) (isoftype? this-syntax -Number))
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with reals (syntax-map (lambda (x) (if (syntax->datum x) x #'0.0))
                                    #'(c1.real-binding c2.real-binding cs.real-binding ...))
           #:with imags (syntax-map (lambda (x) (if (syntax->datum x) x #'0.0))
                                    #'(c1.imag-binding c2.imag-binding cs.imag-binding ...))
           #:with (bindings ...)
           (begin (log-optimization "unboxed binary float complex" #'op)
                  #`(c1.bindings ... c2.bindings ... cs.bindings ... ...
                     ;; we want to bind the intermediate results to reuse them
                     ;; the final results are bound to real-binding and imag-binding
                     #,@(let loop ([o1 (car (syntax->list #'reals))]
                                   [o2 (car (syntax->list #'imags))]
                                   [e1 (cdr (syntax->list #'reals))]
                                   [e2 (cdr (syntax->list #'imags))]
                                   [rs (append (syntax-map (lambda (x) (unboxed-gensym "unboxed-real-"))
                                                           #'(cs.real-binding ...))
                                               (list #'real-binding))]
                                   [is (append (syntax-map (lambda (x) (unboxed-gensym "unboxed-imag-"))
                                                           #'(cs.imag-binding ...))
                                               (list #'imag-binding))]
                                   [ds (syntax-map (lambda (x) (unboxed-gensym))
                                                   #'(c2.real-binding cs.real-binding ...))]
                                   [res '()])
                          (if (null? e1)
                              (reverse res)
                              (loop (car rs) (car is) (cdr e1) (cdr e2) (cdr rs) (cdr is) (cdr ds)
                                    ;; complex division, imag part, real part, then denominator (reverse)
                                    (let ((o-real? (equal? (syntax->datum o2) 0.0))
                                          (e-real? (equal? (syntax->datum (car e2)) 0.0)))
                                      (cond [(and o-real? e-real?)
                                             (list*
                                              #`((#,(car is)) 0.0) ; currently not propagated
                                              #`((#,(car rs)) (unsafe-fl/ #,o1 #,(car e1)))
                                              res)]
                                            [o-real?
                                             (list*
                                              #`((#,(car is))
                                                 (unsafe-fl/ (unsafe-fl- 0.0
                                                                         (unsafe-fl* #,o1 #,(car e2)))
                                                             #,(car ds)))
                                              #`((#,(car rs)) (unsafe-fl/ (unsafe-fl* #,o1 #,(car e1))
                                                                          #,(car ds)))
                                              #`((#,(car ds)) (unsafe-fl+ (unsafe-fl* #,(car e1) #,(car e1))
                                                                          (unsafe-fl* #,(car e2) #,(car e2))))
                                              res)]
                                            [e-real?
                                             (list*
                                              #`((#,(car is)) (unsafe-fl/ #,o2 #,(car e1)))
                                              #`((#,(car rs)) (unsafe-fl/ #,o1 #,(car e1)))
                                              res)]
                                            [else
                                             (list*
                                              #`((#,(car is))
                                                 (unsafe-fl/ (unsafe-fl- (unsafe-fl* #,o2 #,(car e1))
                                                                         (unsafe-fl* #,o1 #,(car e2)))
                                                             #,(car ds)))
                                              #`((#,(car rs))
                                                 (unsafe-fl/ (unsafe-fl+ (unsafe-fl* #,o1 #,(car e1))
                                                                         (unsafe-fl* #,o2 #,(car e2)))
                                                             #,(car ds)))
                                              #`((#,(car ds))
                                                 (unsafe-fl+ (unsafe-fl* #,(car e1) #,(car e1))
                                                             (unsafe-fl* #,(car e2) #,(car e2))))
                                              res)]))))))))

  (pattern (#%plain-app (~and op (~literal conjugate)) c:unboxed-float-complex-opt-expr)
           #:when (isoftype? this-syntax -FloatComplex)
           #:with real-binding #'c.real-binding
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary float complex" #'op)
                  #`(#,@(append (syntax->list #'(c.bindings ...))
                                (list #'((imag-binding) (unsafe-fl- 0.0 c.imag-binding)))))))
  
  (pattern (#%plain-app (~and op (~literal magnitude)) c:unboxed-float-complex-opt-expr)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary float complex" #'op)
                  #`(c.bindings ...
                     ((real-binding) (unsafe-flsqrt 
                                      (unsafe-fl+ (unsafe-fl* c.real-binding c.real-binding) 
                                                  (unsafe-fl* c.imag-binding c.imag-binding)))))))
  
  (pattern (#%plain-app (~and op (~or (~literal real-part) (~literal unsafe-flreal-part)))
                        c:unboxed-float-complex-opt-expr)
           #:with real-binding #'c.real-binding
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary float complex" #'op)
                  #'(c.bindings ...)))
  (pattern (#%plain-app (~and op (~or (~literal imag-part) (~literal unsafe-flimag-part)))
                        c:unboxed-float-complex-opt-expr)
           #:with real-binding #'c.imag-binding
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed unary float complex" #'op)
                  #'(c.bindings ...)))
  
  ;; special handling of reals inside complex operations
  ;; must be after any cases that we are supposed to handle
  (pattern e:float-coerce-expr
           #:with real-binding (unboxed-gensym 'unboxed-float-)
           #:with imag-binding #f
           #:when (log-optimization "float-coerce-expr in complex ops" #'e)
           #:with (bindings ...)
           #`(((real-binding) e.opt)))
    

  ;; we can eliminate boxing that was introduced by the user
  (pattern (#%plain-app (~and op (~or (~literal make-rectangular)
                                      (~literal unsafe-make-flrectangular)))
                        real:float-coerce-expr imag:float-coerce-expr)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "make-rectangular elimination" #'op)
                  #'(((real-binding) real.opt)
                     ((imag-binding) imag.opt))))
  (pattern (#%plain-app (~and op (~literal make-polar))
                        r:float-coerce-expr theta:float-coerce-expr)
           #:with magnitude    (unboxed-gensym)
           #:with angle        (unboxed-gensym)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "make-rectangular elimination" #'op)
                  #'(((magnitude)    r.opt)
                     ((angle)        theta.opt)
                     ((real-binding) (unsafe-fl* magnitude (unsafe-flcos angle)))
                     ((imag-binding) (unsafe-fl* magnitude (unsafe-flsin angle))))))

  ;; if we see a variable that's already unboxed, use the unboxed bindings
  (pattern v:id
           #:with unboxed-info (dict-ref unboxed-vars-table #'v #f)
           #:when (syntax->datum #'unboxed-info)
           #:with real-binding (car  (syntax->list #'unboxed-info))
           #:with imag-binding (cadr (syntax->list #'unboxed-info))
           #:with (bindings ...)
           (begin (log-optimization "leave var unboxed" #'v)
                  #'()))
  
  ;; else, do the unboxing here

  ;; we can unbox literals right away
  (pattern (quote n)
           #:when (let ((x (syntax->datum #'n)))
                    (and (number? x)
                         (not (eq? (imag-part x) 0))))
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unboxed literal" #'n)
                  (let ((n (syntax->datum #'n)))
                    #`(((real-binding) #,(datum->syntax
                                          #'here
                                          (exact->inexact (real-part n))))
                       ((imag-binding) #,(datum->syntax
                                          #'here
                                          (exact->inexact (imag-part n))))))))
  (pattern (quote n)
           #:when (real? (syntax->datum #'n))
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding #f
           #:with (bindings ...)
           (begin (log-optimization "unboxed literal" #'n)
                  #`(((real-binding) #,(datum->syntax
                                        #'here
                                        (exact->inexact (syntax->datum #'n)))))))
  
  (pattern e:expr
           #:when (isoftype? #'e -FloatComplex)
           #:with e* (unboxed-gensym)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unbox float-complex" #'e)
                  #`(((e*) #,((optimize) #'e))
                     ((real-binding) (unsafe-flreal-part e*))
                     ((imag-binding) (unsafe-flimag-part e*)))))
  (pattern e:expr
           #:when (isoftype? #'e -Number) ; complex, maybe exact, maybe not
           #:with e* (unboxed-gensym)
           #:with real-binding (unboxed-gensym "unboxed-real-")
           #:with imag-binding (unboxed-gensym "unboxed-imag-")
           #:with (bindings ...)
           (begin (log-optimization "unbox complex" #'e)
                  #`(((e*) #,((optimize) #'e))
                     ((real-binding) (exact->inexact (real-part e*)))
                     ((imag-binding) (exact->inexact (imag-part e*))))))
  (pattern e:expr
           #:with (bindings ...)
           (error "non exhaustive pattern match")
           #:with real-binding #f
           #:with imag-binding #f))

(define-syntax-class float-complex-unary-op
  #:commit
  (pattern (~or (~literal real-part) (~literal flreal-part)) #:with unsafe #'unsafe-flreal-part)
  (pattern (~or (~literal imag-part) (~literal flimag-part)) #:with unsafe #'unsafe-flimag-part))

(define-syntax-class float-complex-op
  #:commit
  (pattern (~or (~literal +) (~literal -) (~literal *) (~literal /) (~literal conjugate))))

(define-syntax-class float-complex->float-op
  #:commit
  (pattern (~or (~literal magnitude)
                (~literal real-part) (~literal flreal-part) (~literal unsafe-flreal-part)
                (~literal imag-part) (~literal flimag-part) (~literal unsafe-flimag-part))))

(define-syntax-class float-complex-expr
  #:commit
  (pattern e:expr
           #:when (isoftype? #'e -FloatComplex)
           #:with opt ((optimize) #'e)))

(define-syntax-class float-complex-opt-expr
  #:commit

  ;; we can optimize taking the real of imag part of an unboxed complex
  ;; hopefully, the compiler can eliminate unused bindings for the other part if it's not used
  (pattern (#%plain-app (~and op (~or (~literal real-part) (~literal flreal-part) (~literal unsafe-flreal-part)
                                      (~literal imag-part) (~literal flimag-part) (~literal unsafe-flimag-part)))
                        c:float-complex-expr)
           #:with c*:unboxed-float-complex-opt-expr #'c
           #:with opt
           (begin (log-optimization "unboxed float complex" #'op)
                  (reset-unboxed-gensym)
                  #`(let*-values (c*.bindings ...)
                      #,(if (or (free-identifier=? #'op #'real-part)
                                (free-identifier=? #'op #'flreal-part)
                                (free-identifier=? #'op #'unsafe-flreal-part))
                            #'c*.real-binding
                            #'c*.imag-binding))))
  
  (pattern (#%plain-app op:float-complex-unary-op n:float-complex-expr)
           #:with opt
           (begin (log-optimization "unary float complex" #'op)
                  #'(op.unsafe n.opt)))

  (pattern (#%plain-app (~and op (~literal make-polar)) r theta)
           #:when (isoftype? this-syntax -FloatComplex)
           #:with exp*:unboxed-float-complex-opt-expr this-syntax
           #:with opt
           (begin (log-optimization "make-polar" #'op)
                  (reset-unboxed-gensym)
                  #'(let*-values (exp*.bindings ...)
                      (unsafe-make-flrectangular exp*.real-binding
                                                 exp*.imag-binding))))

  (pattern (#%plain-app op:id args:expr ...)
           #:with unboxed-info (dict-ref unboxed-funs-table #'op #f)
           #:when (syntax->datum #'unboxed-info)
           #:with (~var e* (float-complex-call-site-opt-expr
                            #'unboxed-info #'op)) ; no need to optimize op
           this-syntax
           #:with opt
           (begin (log-optimization "call to fun with unboxed args" #'op)
                  #'e*.opt))
  
  (pattern e:float-complex-arith-opt-expr
           #:with opt #'e.opt))

(define-syntax-class float-complex-arith-opt-expr
  #:commit
  
  (pattern (#%plain-app op:float-complex->float-op e:expr ...)
           #:when (subtypeof? this-syntax -Flonum)
           #:with exp*:unboxed-float-complex-opt-expr this-syntax
           #:with real-binding #'exp*.real-binding
           #:with imag-binding #f
           #:with (bindings ...) #'(exp*.bindings ...)
           #:with opt
           (begin (log-optimization "unboxed float complex->float" this-syntax)
                  (reset-unboxed-gensym)
                  #'(let*-values (exp*.bindings ...)
                      real-binding)))

  (pattern (#%plain-app op:float-complex-op e:expr ...)
           #:when (isoftype? this-syntax -FloatComplex)
           #:with exp*:unboxed-float-complex-opt-expr this-syntax
           #:with real-binding #'exp*.real-binding
           #:with imag-binding #'exp*.imag-binding
           #:with (bindings ...) #'(exp*.bindings ...)
           #:with opt
           (begin (log-optimization "unboxed float complex" this-syntax)
                  (reset-unboxed-gensym)
                  #'(let*-values (exp*.bindings ...)
                      (unsafe-make-flrectangular exp*.real-binding exp*.imag-binding))))
  
  (pattern v:id
           #:with unboxed-info (dict-ref unboxed-vars-table #'v #f)
           #:when (syntax->datum #'unboxed-info)
           #:when (subtypeof? #'v -FloatComplex)
           #:with real-binding (car  (syntax->list #'unboxed-info))
           #:with imag-binding (cadr (syntax->list #'unboxed-info))
           #:with (bindings ...) #'()
           ;; unboxed variable used in a boxed fashion, we have to box
           #:with opt
           (begin (log-optimization "unboxed complex variable" #'v)
                  (reset-unboxed-gensym)
                  #'(unsafe-make-flrectangular real-binding imag-binding))))

;; takes as argument a structure describing which arguments will be unboxed
;; and the optimized version of the operator. operators are optimized elsewhere
;; to benefit from local information
(define-syntax-class (float-complex-call-site-opt-expr unboxed-info opt-operator)
  #:commit
  ;; call site of a function with unboxed parameters
  ;; the calling convention is: real parts of unboxed, imag parts, boxed
  (pattern (#%plain-app op:expr args:expr ...)
           #:with ((to-unbox ...) (boxed ...)) unboxed-info
           #:with opt
           (let ((args    (syntax->list #'(args ...)))
                 (unboxed (syntax->datum #'(to-unbox ...)))
                 (boxed   (syntax->datum #'(boxed ...))))
             (define (get-arg i) (list-ref args i))
             (syntax-parse (map get-arg unboxed)
               [(e:unboxed-float-complex-opt-expr ...)
                (log-optimization "unboxed call site" #'op)
                (reset-unboxed-gensym)
                #`(let*-values (e.bindings ... ...)
                    (#%plain-app #,opt-operator
                                 e.real-binding ...
                                 e.imag-binding ...
                                 #,@(map (lambda (i) ((optimize) (get-arg i)))
                                         boxed)))])))) ; boxed params
