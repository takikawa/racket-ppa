#lang scheme/base

(require (for-syntax scheme/base
                     "location.ss"))

(require srfi/1)

(require "base.ss"
         "check-info.ss"
         "format.ss"
         "location.ss")

(provide current-check-handler
         check-around
         current-check-around

         fail-check

         define-check
         define-binary-check
         define-simple-check

         check
         check-exn
         check-not-exn
         check-true
         check-false
         check-pred
         check-eq?
         check-eqv?
         check-equal?
         check-=
         check-not-false
         check-not-eq?
         check-not-equal?
         fail)

;; parameter current-check-handler : (-> exn any)
(define current-check-handler
  (make-parameter
   (lambda (e)
     (cond
      [(exn:test:check? e)
       (display-delimiter)
       (display-failure)(newline)
       (display-check-info-stack
        (exn:test:check-stack e))
       (display-delimiter)]
      [(exn? e)
       (display-delimiter)
       (display-error)(newline)
       (display-exn e)
       (display-delimiter)]))
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-check-handler "procedure" v)))))

;; check-around : ( -> a) -> a
(define check-around
  (lambda (thunk)
    (with-handlers
        ([exn? (current-check-handler)])
      (thunk))))

;; parameter current-check-around : (( -> a) -> a)
(define current-check-around
  (make-parameter
   check-around
   (lambda (v)
     (if (procedure? v)
         v
         (raise-type-error 'current-check-around "procedure" v)))))

(define-syntax fail-check
  (syntax-rules ()
    ((_)
     (raise
      (make-exn:test:check
       "Check failure"
       (current-continuation-marks)
       (check-info-stack))))))

(define-syntax fail-internal
  (syntax-rules ()
    ((_)
     (raise
      (make-exn:test:check:internal
       "Internal failure"
       (current-continuation-marks)
       (check-info-stack))))))

;; refail-check : exn:test:check -> (exception raised)
;;
;; Raises an exn:test:check with the contents of the
;; given parameter.  Useful for propogating internal
;; errors to the outside world.
(define (refail-check exn)
  (raise
   (make-exn:test:check "Check failure"
                        (exn-continuation-marks exn)
                        (exn:test:check-stack exn))))

(define-syntax (define-check stx)
  (syntax-case stx ()
    ((define-check (name formal ...) expr ...)
     (with-syntax ([reported-name
                    (symbol->string
                     (syntax->datum (syntax name)))]
                   [(actual ...)
                    (datum->syntax
                     stx
                     (map gensym
                          (syntax->datum (syntax (formal ...)))))]
                   [check-fn
                    (syntax
                     (lambda (formal ...
                                     [message #f]
                                     #:location [location 'unknown]
                                     #:expression [expression 'unknown])
                       ((current-check-around)
                        (lambda ()
                          (with-check-info*
                           (list* (make-check-name (quote name))
                                  (make-check-location location)
                                  (make-check-expression expression)
                                  (make-check-params (list formal ...))
                                  (if message
                                      (list (make-check-message message))
                                      null))
                             (lambda () (begin expr ...)))))))]
                   [check-secret-name (datum->syntax stx (gensym (syntax->datum (syntax name))))])
       (syntax/loc stx
         (begin
           ;; The distinction between formal and actual parameters
           ;; is made to avoid evaluating the check arguments
           ;; more than once.  This technique is based on advice
           ;; received from Ryan Culpepper.

           (define check-secret-name check-fn)
           
           (define-syntax (name stx)
             (with-syntax
                 ([loc (syntax->location stx)])
               (syntax-case stx ()
                 ((name actual ...)
                  (syntax/loc stx
                    (check-secret-name actual ...
                                       #:location (quote loc)
                                       #:expression (quote (name actual ...)))))

                 ((name actual ... msg)
                  (syntax/loc stx
                    (check-secret-name actual ... msg
                                       #:location (quote loc)
                                       #:expression (quote (name actual ...)))))
                    
                 (name
                  (identifier? #'name)
                  (syntax/loc stx
                    check-secret-name)))))
           ))))))

(define-syntax define-simple-check
  (syntax-rules ()
    ((_ (name param ...) expr ...)
     (define-check (name param ...)
       (let ((result (begin expr ...)))
         (if result
             result
             (fail-check)))))))

(define-syntax define-binary-check
  (syntax-rules ()
    ((_ (name expr1 expr2) expr ...)
     (define-check (name expr1 expr2)
       (with-check-info*
        (list (make-check-actual expr1)
              (make-check-expected expr2))
        (lambda ()
          (let ((result (begin expr ...)))
            (if result
                result
                (fail-check)))))))
    ((_ (name pred expr1 expr2))
     (define-check (name expr1 expr2)
       (with-check-info*
        (list (make-check-actual expr1)
              (make-check-expected expr2))
        (lambda ()
          (if (pred expr1 expr2)
              #t
              (fail-check))))))))

(define-check (check-exn pred thunk)
  (let/ec succeed
    (with-handlers
        (;; catch the exception we are looking for and
         ;; succeed
         [pred
          (lambda (exn) (succeed #t))]
         ;; rethrow check failures if we aren't looking
         ;; for them
         [exn:test:check?
          (lambda (exn)
            (refail-check exn))]
         ;; catch any other exception and raise an check
         ;; failure
         [exn:fail?
          (lambda (exn)
            (with-check-info*
             (list
              (make-check-message "Wrong exception raised")
              (make-check-info 'exn-message (exn-message exn))
              (make-check-info 'exn exn))
             (lambda () (fail-check))))])
      (thunk))
    (with-check-info*
     (list (make-check-message "No exception raised"))
     (lambda () (fail-check)))))

(define-check (check-not-exn thunk)
  (with-handlers
      ([exn:test:check?
        (lambda (exn) (refail-check exn))]
       [exn?
        (lambda (exn)
          (with-check-info*
           (list
            (make-check-message "Exception raised")
            (make-check-info 'exception-message (exn-message exn))
            (make-check-info 'exception exn))
           (lambda () (fail-check))))])
    (thunk)))

(define-simple-check (check operator expr1 expr2)
  (operator expr1 expr2))

(define-simple-check (check-pred predicate expr)
  (predicate expr))

(define-binary-check (check-eq? eq? expr1 expr2))

(define-binary-check (check-eqv? eqv? expr1 expr2))

(define-binary-check (check-equal? expr1 expr2)
  (equal? expr1 expr2))

(define-simple-check (check-= expr1 expr2 epsilon)
  (<= (abs (- expr1 expr2)) epsilon))

(define-simple-check (check-true expr)
  (eq? expr #t))

(define-simple-check (check-false expr)
  (eq? expr #f))

(define-simple-check (check-not-false expr)
  expr)

(define-simple-check (check-not-eq? expr1 expr2)
  (not (eq? expr1 expr2)))

(define-simple-check (check-not-equal? expr1 expr2)
  (not (equal? expr1 expr2)))

(define-simple-check (fail)
  #f)

