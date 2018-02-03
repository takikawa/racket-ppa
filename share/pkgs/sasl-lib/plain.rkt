#lang racket/base
(require racket/contract/base
         "private/saslprep.rkt")
(provide (contract-out
          [plain-client-message
           (->* [safe-string? safe-string?]
                [#:authorization-id (or/c #f safe-string?)]
                string?)]))

;; References:
;; - https://tools.ietf.org/html/rfc4616

;; plain-client-message : String String/#f String -> String
(define (plain-client-message authcid passwd #:authorization-id [authzid #f])
  (let ([who 'plain-client-message])
    (string-append
     (if authzid (saslprep authzid #:who who) "")
     "\0" (saslprep authcid #:who who)
     "\0" (saslprep passwd #:who who))))

(define (safe-string? s)
  (and (string? s)
       (positive? (string-length s))
       (for/and ([c (in-string s)]) (not (eqv? c #\nul)))))
