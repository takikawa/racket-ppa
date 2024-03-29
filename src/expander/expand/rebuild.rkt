#lang racket/base
(require "../syntax/syntax.rkt")

(provide rebuild)

;; A helper for forms to reconstruct syntax while preserving source
;; locations, properties, and arming; if `track?` is #f, then don't keep
;; properties, because we've kept them in a surrounding form
(define (rebuild orig-s new
                 #:track? [track? #t])
  (datum->syntax orig-s new orig-s (and track? orig-s)))
