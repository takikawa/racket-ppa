;; This is a wrapper around "rep-start.rkt" -- use it if we're using a terminal
#lang racket/base

(require racket/runtime-path racket/file)

(define-runtime-path rep-start "rep-start.rkt")

(provide install-readline!
         pre-readline-input-port)

(define pre-readline-input-port
  (let ([inp (current-input-port)] [outp (current-output-port)])
    (and (eq? 'stdin (object-name inp))
         (terminal-port? inp)
         (dynamic-require rep-start 'pre-readline-input-port))))

(define readline-init-expr
  '(require readline/rep))

(define (install-readline!)
  (define file (find-system-path 'init-file))
  (define (add! msg)
    (call-with-output-file* file #:exists 'append
      (lambda (o)
        (fprintf o "\n;; load readline support ~a\n~s\n"
                 "(added by `install-readline!')"
                 readline-init-expr)))
    (printf msg file))
  (cond [(not (file-exists? file))
         (add! "\"~a\" created and readline initialization added.\n")]
        [(with-handlers ([exn:fail?
                          (lambda (exn)
                            (error 'install-readline!
                                   "trouble reading existing ~e: ~a"
                                   file
                                   (exn-message exn)))])
           (not (member readline-init-expr (file->list file))))
         (add! "Readline initialization added to \"~a\".\n")]
        [else (printf "Readline already installed in \"~a\".\n" file)]))
