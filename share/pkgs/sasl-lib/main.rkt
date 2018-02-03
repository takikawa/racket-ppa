#lang racket/base
(require racket/contract/base
         "private/base.rkt")
(provide sasl-ctx?
         (contract-out
          [sasl-next-message    (-> sasl-ctx? any/c)]
          [sasl-receive-message (-> sasl-ctx? any/c any)]
          [sasl-state           (-> sasl-ctx? symbol?)])
         (struct-out exn:fail:sasl:fatal))
