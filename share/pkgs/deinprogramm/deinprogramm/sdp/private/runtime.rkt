#lang racket/base
(require mzlib/pconvert
         racket/pretty
         lang/private/set-result
         mrlib/image-core
         racket/snip
         racket/class
         (only-in test-engine/test-markup get-rewritten-error-message-parameter)
         deinprogramm/sdp/private/rewrite-error-message
         (only-in test-engine/syntax report-signature-violation!)
         (only-in deinprogramm/signature/signature signature-violation-proc)
         deinprogramm/sdp/private/convert-explicit
         htdp/bsl/print-width) ; FIXME: might want to have our own

(provide configure)

; level is beginner, vanilla, advanced
(define (configure level)
  ;; Set print-convert options:
  (booleans-as-true/false #f)
  (print-boolean-long-form #f)
  (constructor-style-printing #f)
  (add-make-prefix-to-constructor #f)
  (abbreviate-cons-as-list #t)
  (current-print-convert-hook
   (lambda (val basic sub)
     (case level
       ((beginner vanilla) (convert-explicit val))
       (else val))))
  (use-named/undefined-handler
   (lambda (x)
     (object-name x)))
  (named/undefined-handler
   (lambda (x)
     (string->symbol
      (format "function:~a" (object-name x)))))
  ;; Set pretty-print options:
  (pretty-print-show-inexactness #t)
  (pretty-print-exact-as-decimal #t)
  (define img-str "#<image>")
  (define (is-image? val)
    (or (is-a? val image%)         ;; 2htdp/image
        (is-a? val image-snip%)))  ;; literal image constant
  (show-sharing #f)

  ;; Set print handlers to use print-convert and pretty-write:
  (define (set-handlers thunk)
    (parameterize ([pretty-print-print-hook
                    (let ([oh (pretty-print-print-hook)])
                      (λ (val display? port)
                        (if (and (not (port-writes-special? port))
                                 (is-image? val))
                            (begin (display img-str port)
                                   (string-length img-str))
                            (oh val display? port))))]
                   [pretty-print-size-hook
                    (let ([oh (pretty-print-size-hook)])
                      (λ (val display? port)
                        (if (and (not (port-writes-special? port))
                                 (is-image? val))
                            (string-length img-str)
                            (oh val display? port))))])
      (thunk)))
  (get-rewritten-error-message-parameter get-rewriten-error-message)
  (error-display-handler
   (let ([o-d-h (error-display-handler)])
     (λ (msg exn)
       (define x (get-rewriten-error-message exn))
       (o-d-h x exn))))
  (let ([orig (global-port-print-handler)])
    (global-port-print-handler
     (lambda (val port [depth 0])
       (parameterize ([global-port-print-handler orig])
         (let ([val (print-convert val)])
           (set-handlers
            (λ ()
              (parameterize ([pretty-print-columns (htdp-print-columns)])
                (pretty-write val port)))))))))

  (signature-violation-proc
   (lambda (obj signature message blame)
     (report-signature-violation! obj signature message blame))))
