#lang racket/base
(require racket/contract/base
         "private/base.rkt"
         "private/cram-md5.rkt")
(provide (contract-out
          [make-cram-md5-client-ctx
           (-> string? string? sasl-ctx?)]))
