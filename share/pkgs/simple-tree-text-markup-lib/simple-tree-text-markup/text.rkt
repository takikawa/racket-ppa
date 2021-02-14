; Render markup to text
#lang racket/base
(require racket/contract
         (only-in simple-tree-text-markup/data markup?))
(provide
 (contract-out (display-markup ((markup?) (output-port?) . ->* . any))))
(require simple-tree-text-markup/private/text)

