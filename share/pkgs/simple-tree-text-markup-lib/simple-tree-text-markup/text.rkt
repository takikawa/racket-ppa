; Render markup to text
#lang racket/base
(require racket/contract
         (only-in simple-tree-text-markup/data markup?))
(provide
 (contract-out (display-markup ((markup?) (output-port?) . ->* . any))
               (number-markup->string ((number?)
                                       (#:exact-prefix (or/c 'always 'never 'when-necessary)
                                        #:inexact-prefix (or/c 'always 'never 'when-necessary)
                                        #:fraction-view (or/c #f 'mixed 'improper 'decimal))
                                       . ->* .
                                       string?))))
(require simple-tree-text-markup/private/text)
