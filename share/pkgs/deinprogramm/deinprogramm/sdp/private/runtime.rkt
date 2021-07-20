#lang racket/base
(provide configure configure/settings
         level->sdp-runtime-settings
         (struct-out sdp-runtime-settings)
         sdp-render-value/format)

(require mzlib/pconvert
         racket/pretty
         lang/private/set-result
         mrlib/image-core
         racket/snip
         racket/class
         (only-in test-engine/test-markup get-rewritten-error-message-parameter render-value-parameter)
         deinprogramm/sdp/private/rewrite-error-message
         (only-in test-engine/syntax report-signature-violation!)
         (only-in deinprogramm/signature/signature signature-violation-proc)
         deinprogramm/sdp/private/convert-explicit
         (only-in simple-tree-text-markup/construct number)
         simple-tree-text-markup/text)

(struct sdp-runtime-settings
  (printing-style ; constructor, quasiquote, write, print
   writing-style  ; explicit datum
   fraction-style ; mixed-fraction, mixed-fraction-e, repeating-decimal, repeating-decimal-e
   show-sharing?
   insert-newlines?
   tracing?)) ; unclear if this should be here

(define (level->sdp-runtime-settings level)
  (case level
    ((beginner)
     (sdp-runtime-settings 'write 'explicit 'repeating-decimal #f #t #f))
    ((vanilla)
     (sdp-runtime-settings 'write 'explicit 'repeating-decimal #f #t #f))
    ((advanced)
     (sdp-runtime-settings 'write 'datum    'repeating-decimal #f #t #f))))

(define insert-newlines (make-parameter #t))

(define (configure level)
  (configure/settings (level->sdp-runtime-settings level)))
         
; level is beginner, vanilla, advanced
(define (configure/settings settings)
  ;; Set print-convert options:
  (booleans-as-true/false #f)
  (print-boolean-long-form #f)
  (constructor-style-printing
   (case (sdp-runtime-settings-printing-style settings)
     [(quasiquote) #f]
     [else #t]))
  (add-make-prefix-to-constructor #f)
  (abbreviate-cons-as-list #t)
  (current-print-convert-hook
   (lambda (val basic sub)
     (case (sdp-runtime-settings-writing-style settings)
       ((explicit) (convert-explicit val))
       (else val))))
  (use-named/undefined-handler
   (lambda (x)
     (object-name x)))
  (named/undefined-handler
   (lambda (x)
     (string->symbol
      (format "function:~a" (object-name x)))))
  ; sharing done by print-convert
  (show-sharing (sdp-runtime-settings-show-sharing? settings))
  ; sharing done by write
  (print-graph (and (sdp-runtime-settings-show-sharing? settings)
                    ;; print-convert takes care of this also, so only do it when that doesn't happen
                    (case (sdp-runtime-settings-printing-style settings)
                      ([trad-write write] #t)
                      (else #f))))

  (define fraction-view
    (case (sdp-runtime-settings-fraction-style settings)
      [(mixed-fraction mixed-fraction-e) 'mixed]
      [(repeating-decimal repeating-decimal-e) 'decimal]))

  ;; Set pretty-print options:
  (pretty-print-show-inexactness #t)
  (pretty-print-exact-as-decimal (eq? fraction-view 'decimal))

  (define img-str "#<image>")
  (define (is-image? val)
    (or (is-a? val image%)         ;; 2htdp/image
        (is-a? val image-snip%)))  ;; literal image constant

    ;; exact fractions - slight hack as we know for what numbers DrRacket generates special snips
  (define (use-number-markup? x)
    (and (number? x)
         (exact? x)
         (real? x)
         (not (integer? x))))

  (pretty-print-print-hook
   (let ([oh (pretty-print-print-hook)])
     (λ (val display? port)
       (cond
        [(and (not (port-writes-special? port))
              (is-image? val))
         (display img-str port)]
        [(and (use-number-markup? val)
              (port-writes-special? port))
         (write-special (number val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view) port)]
        [(number? val)
         (display (number-markup->string val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view) port)]
        [else
         (oh val display? port)]))))

  (pretty-print-size-hook
   (let ([oh (pretty-print-size-hook)])
     (λ (val display? port)
       (cond
         [(and (not (port-writes-special? port))
               (is-image? val))
          (string-length img-str)]
        [(and (use-number-markup? val)
              (port-writes-special? port))
         1]
        [(number? val)
         (string-length (number-markup->string val #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view fraction-view))]
        [else
         (oh val display? port)]))))

  ; test-engine
  (get-rewritten-error-message-parameter get-rewriten-error-message)
  ; test-engine
  (render-value-parameter
   (lambda (value port)
     (parameterize ([print-value-columns 40])
       (print value port))))

  (error-display-handler
   (let ([o-d-h (error-display-handler)])
     (λ (msg exn)
       (define x (get-rewriten-error-message exn))
       (o-d-h x exn))))

  (global-port-print-handler
   (lambda (val port [depth 0])
     (define printing-style (sdp-runtime-settings-printing-style settings))
     (parameterize ([pretty-print-columns
                     (if (and (sdp-runtime-settings-insert-newlines? settings)
                              (exact-integer? (print-value-columns)))
                         (print-value-columns)
                         'infinity)])
       (let [(val (print-convert val))]
         (case printing-style
           [(print) (pretty-print val port depth)]
           [(write trad-write constructor) (pretty-write val port)]
           [(quasiquote) (pretty-write val port)])))))

  (signature-violation-proc
   (lambda (obj signature message blame)
     (report-signature-violation! obj signature message blame))))

(define (sdp-render-value/format value port width)
  (parameterize ([print-value-columns (if (eq? width 'infinity)
                                          +inf.0
                                          width)])
    (print value port)
    (unless (insert-newlines)
      (newline port))))

