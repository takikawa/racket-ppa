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

(provide/contract
 [expand/step
  (-> syntax? void?)]
 [expand-module/step
  (-> module-path? void?)])

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
