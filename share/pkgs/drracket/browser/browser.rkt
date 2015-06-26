#lang racket
(require racket/unit
         racket/gui
         mred/mred-sig
         setup/plt-installer-sig
         setup/plt-installer
         net/tcp-sig
         net/url-sig
         net/url
         "browser-sig.rkt"
         "browser-unit.rkt")

(provide-signature-elements browser^)

(define-values/invoke-unit/infer browser@)
