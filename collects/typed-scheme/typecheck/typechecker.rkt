#lang scheme/base

(require "../utils/utils.rkt")
(require (utils unit-utils)
         mzlib/trace
         (only-in scheme/unit
                  provide-signature-elements
                  define-values/invoke-unit/infer link)
         "signatures.rkt"
         "tc-if.rkt" "tc-lambda-unit.rkt" "tc-app.rkt"
         "tc-let-unit.rkt" "tc-apply.rkt"
         "tc-expr-unit.rkt" "check-subforms-unit.rkt")

(provide-signature-elements tc-expr^ check-subforms^)

(define-values/invoke-unit/infer
  (link tc-if@ tc-lambda@ tc-app@ tc-let@ tc-expr@ check-subforms@ tc-apply@))
