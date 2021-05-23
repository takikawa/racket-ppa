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
         (only-in mrlib/syntax-browser render-syntax/snip))

(define (sdp-stepper-drracket-button level)
  (stepper-drracket-button (new sdp-stepper-language%) (level->settings level)))

(define-logger stepper)

(struct settings (insert-newlines writing-style))

(define (level->settings level)
  (case level
    ((beginner) (settings #t 'explicit))
    ((vanilla) (settings #t 'explicit))
    ((advanced) (settings #t 'datum))))

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
      (parameterize ([current-print-convert-hook (make-print-convert-hook settings)])
        (call-with-print-settings
         language-level
         settings
         (lambda () 
           (stepper-convert-value val settings)))))

    (public render-value)
    (define (render-value val settings port)
      (set-printing-parameters
       settings
       (lambda ()
         (teaching-language-render-value/format val settings port 'infinity))))

    (public set-printing-parameters)
    ;; set-printing-parameters : ( -> TST) -> TST
    (define (set-printing-parameters settings thunk)
      (parameterize ([booleans-as-true/false #f]
                     [abbreviate-cons-as-list #t]
                     [pretty-print-show-inexactness #t]
                     [pretty-print-exact-as-decimal #t]
                     [use-named/undefined-handler (lambda (x) #f)]
                     [named/undefined-handler
                      (lambda (x)
                        (string->symbol
                         (format "function:~a" (object-name x))))])
        (thunk)))
    
    (super-instantiate ())))

;; make-print-convert-hook:
;;   simple-settings -> (TST (TST -> TST) (TST -> TST) -> TST)
;; this code copied from various locations in language.rkt and rep.rkt
(define (make-print-convert-hook simple-settings)
  (lambda (exp basic-convert sub-convert)
    (cond
      [(is-a? exp snip%)
       (send exp copy)]
      [else (basic-convert exp)])))

(define (stepper-convert-value value settings)
  (define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
    (if (or (is-a? expr snip%)
            ;; FIXME: internal in language.rkt (to-snip-value? expr)
            )
        expr
        (sh expr basic-convert sub-convert)))
  ;; mflatt: MINOR HACK - work around temporary
  ;;         print-convert problems
  (define (stepper-print-convert v)
    (or (and (procedure? v) (object-name v))
        (print-convert v)))

  (let ((v (case (settings-writing-style settings)
	     [(explicit) (convert-explicit value)]
	     [(datum) (convert-quoted value)])))
    (or (and (procedure? v) (object-name v))
        v)))

; ultimately a hack, as it may display something quoted as the final result
(define (convert-quoted value)
  (if (or (symbol? value) (null? value) (pair? value))
      `(quote ,value)
      value))

;; teaching-language-render-value/format : TST settings port (union #f (snip% -> void)) (union 'infinity number) -> void
(define (teaching-language-render-value/format value settings port width)
  (let ([converted-value (simple-module-based-language-convert-value value settings)])
    (setup-printing-parameters
     (lambda ()
       (cond
         [(settings-insert-newlines settings)
          (if (number? width)
              (parameterize ([pretty-print-columns width])
                (pretty-print converted-value port))
              (pretty-print converted-value port))]
         [else
          (parameterize ([pretty-print-columns 'infinity])
            (pretty-print converted-value port))
          (newline port)]))
     settings
     width)))

;; setup-printing-parameters : (-> void) -> void
(define (setup-printing-parameters thunk settings width)
  (let ([use-number-snip?
         (lambda (x)
           (and (number? x)
                (exact? x)
                (real? x)
                (not (integer? x))))])
    (parameterize (;; these three handlers aren't used, but are set to override the user's settings
                   [pretty-print-print-line (lambda (line-number op old-line dest-columns) 
                                              (when (and (not (equal? line-number 0))
                                                         (not (equal? dest-columns 'infinity)))
                                                (newline op))
                                              0)]
                   [pretty-print-pre-print-hook (lambda (val port) (void))]
                   [pretty-print-post-print-hook (lambda (val port) (void))]
                   

                   [pretty-print-columns width]
                   [pretty-print-size-hook
                    (lambda (value display? port)
                      (cond
                        [(not (port-writes-special? port)) #f]
                        [(signature? value) (string-length (signature-output value))]
                        [(procedure? value) (string-length (procedure-output value))]
                        [(is-a? value snip%) 1]
                        [(use-number-snip? value) 1]
                        [(syntax? value) 1]
                        [(to-snip-value? value) 1]
                        [else #f]))]
                   [pretty-print-print-hook
                    (lambda (value display? port)
                      (cond
                        [(signature? value)
                         (write-special (signature-output value) port)]
                        [(procedure? value)
                         (write-special (procedure-output value) port)]
                        [(is-a? value snip%)
                         (write-special value port)
                         1]
                        [(use-number-snip? value)
                         (write-special
                          (number-snip:make-repeating-decimal-snip value #f)
                          port)
                         1]
                        [(syntax? value)
                         (write-special (render-syntax/snip value) port)]
                        [else (write-special (value->snip value) port)]))]
                   [print-graph #f])
      (thunk))))

(define (procedure-output proc)
  (cond
    ((object-name proc)
     => (lambda (name)
          (string-append "#<function:" (symbol->string name) ">")))
    (else "#<function>")))

(define (signature-output proc)
  (cond
    ((signature-name proc)
     => (lambda (name)
          (string-append "#<signature:" (symbol->string name) ">")))
    (else "#<signature>")))

(define (stepper-print-convert-hook exp basic-convert sub-convert)
  (cond
    [(is-a? exp snip%) (send exp copy)]
    [else (basic-convert exp)]))

;; set-print-settings ; settings ( -> TST) -> TST
(define (call-with-print-settings language simple-settings thunk)
  ;; this should succeed for the teaching languges, and fail otherwise.
  ;; if there's a way to directly check this, I should do it. As an approximation,
  ;; the else clause will be guarded by a check for PLTSTEPPERUNSAFE
  (if (method-in-interface? 'set-printing-parameters (object-interface language))
      (send language set-printing-parameters simple-settings thunk)
      ;; should only wind up here for non-teaching-languages:
      (cond [(getenv "PLTSTEPPERUNSAFE") (thunk)]
            [else
             (thunk)
             ;; this error occurs in htdp/bsl etc.
             #;(error
              'stepper-tool
              "language object does not contain set-printing-parameters method")])))

(define (simple-module-based-language-convert-value value settings)
  (parameterize ([constructor-style-printing #t]
                 [show-sharing #f]
                 [current-print-convert-hook (leave-snips-alone-hook (current-print-convert-hook))])
    (print-convert (case (settings-writing-style settings)
                     [(explicit) (convert-explicit value)]
                     [(datum) value]))))

;; leave-snips-alone-hook : any? (any? -> printable) any? -> printable
(define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
  (if (is-a? expr snip%)
      expr
      (sh expr basic-convert sub-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  snip/value extensions
;;

(define to-snips null)
(define-struct to-snip (predicate? >value))
(define (add-snip-value predicate constructor)
  (set! to-snips (cons (make-to-snip predicate constructor) to-snips)))

(define (value->snip v)
  (ormap (lambda (to-snip) (and ((to-snip-predicate? to-snip) v)
                                ((to-snip->value to-snip) v)))
         to-snips))
(define (to-snip-value? v)
  (ormap (lambda (to-snip) ((to-snip-predicate? to-snip) v)) to-snips))


