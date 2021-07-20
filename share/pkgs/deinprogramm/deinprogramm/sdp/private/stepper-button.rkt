; Provide SdP-specific configuration of the stepper
#lang racket/base
(provide sdp-stepper-drracket-button)
(require racket/class
         racket/pretty
         racket/match
         racket/snip
         mzlib/pconvert
         deinprogramm/sdp/private/convert-explicit
         lang/stepper-language-interface
         (only-in deinprogramm/signature/signature signature? signature-name)
         stepper/drracket-button
         (only-in framework number-snip:make-repeating-decimal-snip)
         (only-in mrlib/syntax-browser render-syntax/snip)
         deinprogramm/sdp/private/runtime)

(define (sdp-stepper-drracket-button level)
  (let ((settings (level->sdp-runtime-settings level)))
    (stepper-drracket-button (new sdp-stepper-language%) settings
                             (lambda () (configure/settings settings)))))

(define-logger stepper)

(define sdp-stepper-language%
  (class* object% (stepper-language<%>)
    (public stepper:supported?)
    (define (stepper:supported?) #t)

    (public stepper:enable-let-lifting?)
    (define (stepper:enable-let-lifting?) #f)

    (public stepper:show-lambdas-as-lambdas?)
    (define (stepper:show-lambdas-as-lambdas?) #t)

    (public stepper:show-inexactness?)
    (define (stepper:show-inexactness?) #t)

    (public stepper:print-boolean-long-form?)
    (define (stepper:print-boolean-long-form?) #f)

    (public stepper:show-consumed-and/or-clauses?)
    (define (stepper:show-consumed-and/or-clauses?) #t)

    (public stepper:render-to-sexp)
    (define (stepper:render-to-sexp val settings language-level)
      (when (boolean? val)
        (log-stepper-debug "render-to-sexp got a boolean: ~v\n" val))
      (or (and (procedure? val)
               (object-name val))
          (print-convert val)))

    (public render-value)
    (define (render-value val settings port)
      (parameterize ([print-value-columns +inf.0])
        (print val port)))
    
    (super-instantiate ())))
