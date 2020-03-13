#lang racket/base
(require racket/class
         racket/contract/base
         racket/class/iop
         macro-debugger/model/trace
         "view/interfaces.rkt"
         "view/view.rkt")

(define (create-stepper deriv)
  (define director (new macro-stepper-director%))
  (define stepper (send/i director director<%> new-stepper))
  (send/i director director<%> add-deriv deriv)
  (void))

(define (expand/step stx)
  (create-stepper (trace stx)))

(define (expand-module/step module-path)
  (create-stepper (trace-module module-path)))

(define (macro-stepper-repl [new-repl? #f]
                            #:eval? [eval? #t])
  (define-values (new-eval new-mnr)
    (make-handlers (current-eval) (current-module-name-resolver) (and eval? #t)))
  (cond [new-repl?
         (parameterize ((current-eval new-eval)
                        (current-module-name-resolver new-mnr))
           (read-eval-print-loop)
           (newline))]
        [else
         (current-eval new-eval)
         (current-module-name-resolver new-mnr)]))

(define (make-handlers original-eval original-module-resolver eval?)
  (define director (new macro-stepper-director/process%))
  (send director new-stepper)
  (define debugging? #t) ;; mutated
  (define (inner-eval stx) (when eval? (original-eval stx)))
  (define (call-without-debugging thunk)
    (let ([saved-eo (current-expand-observe)]
          [saved-debugging? debugging?])
      (dynamic-wind
        (lambda ()
          (set! debugging? #f)
          (when saved-eo (current-expand-observe void)))
        thunk
        (lambda ()
          (set! debugging? saved-debugging?)
          (when saved-eo (current-expand-observe saved-eo))))))
  (define (new-eval expr)
    (cond [(and debugging? (not (compiled-expression? expr)))
           (define-values (e-expr events derivp)
             (trace*
              (cond [(syntax? expr) expr]
                    [else (namespace-syntax-introduce (datum->syntax #f expr))])))
           (send director add-trace events)
           (if (syntax? e-expr)
               (inner-eval e-expr)
               (raise e-expr))]
          [else (original-eval expr)]))
  (define (new-module-resolver . args)
    (call-without-debugging
     (lambda () (apply original-module-resolver args))))
  (values new-eval new-module-resolver))


(provide/contract
 [expand/step
  (-> syntax? void?)]
 [expand-module/step
  (-> module-path? void?)]
 [macro-stepper-repl
  (->* [any/c] [#:eval? any/c] any)])

(module+ main
  (require racket/cmdline
           raco/command-name)
  (define mode 'auto)
  (command-line
   #:program (short-program+command-name)
   #:once-any
   [("-f" "--file") "Interpret arguments as file-paths"
    (set! mode 'file)]
   [("-m" "--module-path") "Interpret arguments as module-paths"
    (set! mode 'module-path)]
   #:args (module-to-expand)
   (let ()
     (define (->modpath x)
       (case mode
         [(auto)
          (if (file-exists? x)
              `(file ,x)
              (read (open-input-string x)))]
         [(file) `(file ,x)]
         [else (read (open-input-string x))]))
     (expand-module/step (->modpath module-to-expand)))))
