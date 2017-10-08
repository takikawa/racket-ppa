#lang racket/base
(require racket/pretty
         racket/class
         racket/class/iop
         "interfaces.rkt"
         macro-debugger/view/debug-format
         "view.rkt")
(provide debug-file)

(define (make-stepper)
  (define director (new macro-stepper-director%))
  (send director new-stepper))

(define (debug-file file)
  (let-values ([(events msg ctx) (load-debug-file file)])
    (pretty-print msg)
    (pretty-print ctx)
    (let* ([w (make-stepper)])
      (send/i w widget<%> add-trace events)
      w)))
