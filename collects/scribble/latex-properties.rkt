#lang scheme/base
(require "private/provide-structs.rkt"
         scheme/contract)

(provide-structs
 [tex-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)))])]
 [latex-defaults ([prefix (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                  [style (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                  [extra-files (listof (or/c path-string? (cons/c 'collects (listof bytes?))))])])
