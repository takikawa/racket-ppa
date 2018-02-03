#lang racket/base
(require racket/contract/base
         "private/saslprep.rkt")
(provide (contract-out
          [saslprep
           (->* [string?]
                [#:allow-unassigned? any/c]
                string?)]))
