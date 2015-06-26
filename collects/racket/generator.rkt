#lang racket/base

(require (for-syntax racket/base)
         racket/control)

(provide yield generator generator-state in-generator infinite-generator
         sequence->generator sequence->repeated-generator
         generator?)

;; (require racket/stxparam racket/splicing)

;; (define-syntax-parameter yield
;;   (lambda (stx)
;;     (raise-syntax-error
;;      #f "yield is only bound inside a sequence generator")))

;; (define (procedure->generator proc)
;;   (define tag (make-continuation-prompt-tag))
;;   (define (cont)
;;     (reset-at tag
;;       (let ([r (proc (lambda (r) (shift-at tag k (set! cont k) r)))])
;;         ;; normal return:
;;         (set! cont (lambda () r))
;;         r)))
;;   (lambda () (cont)))

;; not using parameterization (old version, doesn't deal with multiple
;; inputs/outputs as the one below)
#;
(define-syntax-rule (generator body0 body ...)
  (let ([tag (make-continuation-prompt-tag)])
    (define yielder
      (let ([yield (lambda (value) (shift-at tag k (set! cont k) value))])
        yield))
    (splicing-syntax-parameterize ([yield (make-rename-transformer #'yielder)])
      (define (cont)
        (reset-at tag
          (let ([retval (begin body0 body ...)])
            ;; normal return:
            (set! cont (lambda () retval))
            retval))))
    (define (generator) (cont))
    generator))

(define current-yielder
  (make-parameter
   (lambda (v)
     (error 'yield "must be called in the context of a generator"))))

(define yield
  (case-lambda [()  ((current-yielder))]
               [(v) ((current-yielder) v)]
               [vs  (apply (current-yielder) vs)]))

(define yield-tag (make-continuation-prompt-tag 'yield))

(define-syntax (generator stx)
  (syntax-case stx ()
    [(_ formals body0 body ...) 
     (if (let loop ([formals #'formals])
           (cond
            [(null? formals) #t]
            [(identifier? formals) #t]
            [(syntax? formals) (loop (syntax-e formals))]
            [(pair? formals) (and (identifier? (car formals))
                                  (loop (cdr formals)))]
            [else #f]))
         #'(create-generator (let ([generator
                                    (lambda formals
                                      body0 body ...)])
                               generator))
         (raise-syntax-error
          #f
          "bad syntax for formal initial arguments"
          stx
          #'formals))]
    [_ (if (identifier? stx)
           (raise-syntax-error #f "bad syntax"  stx)
           (raise-syntax-error
            #f
            (format "use does not have the form (~a formals body ...)"
                    (syntax-e (car (syntax-e stx))))
            stx))]))

(define (create-generator start)
  (let ([state 'fresh])
    (define (yielder . vs)
      (set! state 'suspended)
      (call/cc (lambda (k)
                 (set! cont k)
                 (apply abort-current-continuation yield-tag vs))
               yield-tag))
    (define (cont . init-formals)
      (set! state 'running)
      (call-with-values
          (lambda ()
            (apply start init-formals))
        ;; get here only on at the end of the generator
        (lambda rs
          (set! cont (lambda () (set! state 'done) (apply values rs)))
          (cont))))
    (define (call-cont vs)
      (call-with-continuation-prompt
       (lambda ()
         (parameterize ([current-yielder yielder])
           (apply cont vs)))
       yield-tag
       values))
    (define (err [what "send a value to"])
      (raise-arguments-error 'generator 
                             (format "cannot ~a a ~a generator" what state)
                             "generator" self))
    (define generator
      (case-lambda
        [()  (if (eq? state 'running)
               (err "call")
               (begin (set! state 'running) (call-cont null)))]
        ;; yield-tag means return the state (see `generator-state' below)
        [(x) (cond [(eq? x yield-tag) state]
                   [(memq state '(suspended running fresh))
                    (set! state 'running)
                    (call-cont (list x))]
                   [else (err)])]
        [xs  (if (memq state '(suspended running fresh))
               (begin (set! state 'running) (call-cont xs))
               (err))]))
    (define self (make-generator generator))
    self))

(define-struct generator (proc)
  #:property prop:procedure 0
  #:omit-define-syntaxes)

;; Get the state -- this is a hack: uses yield-tag as a hidden value that makes
;; the generator return its state.  Protect against grabbing this tag (eg, with
;; (generator-state values)) by inspecting the result (so it can still be
;; deceived, but that will be harmless).
(define (generator-state g)
  (if (generator? g)
      (g yield-tag)
      (raise-argument-error 'generator-state "generator?" g)))

(define-syntax-rule (infinite-generator body0 body ...)
  (generator () (let loop () body0 body ... (loop))))

(define stop-value (gensym 'stop-value))

(begin-for-syntax
  (define (expand-in-generator arity arity-allowed? stx)
    (syntax-case stx ()

      [(_ #:arity n body0 body ...)
       (let ([new-arity (syntax-e #'n)])
         (unless (exact-nonnegative-integer? new-arity)
           (define message "expected a literal exact nonnegative integer")
           (raise-syntax-error #f message stx #'n))
         (unless arity-allowed?
           (define message "cannot specify arity more than once")
           (raise-syntax-error #f message stx #'n))
         (when (and arity (not (= arity new-arity)))
           (define message
             (format "arity mismatch, context expects a generator of arity ~a"
               arity))
           (raise-syntax-error #f message stx #'n))
         (define new-stx (syntax/loc stx (in-generator body0 body ...)))
         (expand-in-generator new-arity #f new-stx))]

      [(_ body0 body ...)
       (let ([real-arity (or arity 1)])
         (cond
           [(zero? real-arity)
            #'(let ([stop? #f])
                (in-producer
                  (generator () body0 body ... (set! stop? #t) (values))
                  (lambda () stop?)))]
           [else
            (define vars (generate-temporaries (build-list real-arity values)))
            (define stops (build-list real-arity (lambda (i) #'stop-value)))
            (with-syntax ([(x ...) vars]
                          [x0 (car vars)]
                          [(stop ...) stops])
              #'(in-producer
                  (generator () body0 body ... (values stop ...))
                  (lambda (x ...) (eq? x0 stop-value))))]))])))

(define-sequence-syntax in-generator
  (lambda (stx) (expand-in-generator #f #t stx))
  (lambda (stx)
    (syntax-case stx ()
      [((id ...) expr)
       (let ([arity (length (syntax->list #'(id ...)))])
         (with-syntax ([e (expand-in-generator arity #t #'expr)])
           #'[(id ...) e]))])))

(define (sequence->generator sequence)
  (generator () (for ([i sequence]) (yield i))))

(define (sequence->repeated-generator sequence)
  (sequence->generator (in-cycle sequence)))

#|
;; examples
(for/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]) i)
(for*/list ([i (in-generator (for-each yield '(1 2 3)) (yield 'four))]
            [j (in-generator (yield 'X) (yield '-))])
  (list i j))
|#
