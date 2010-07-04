#lang racket
(require web-server/templates
         xml)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

(define (start initial-request)
  `(html (pre ,(include-template "static.html"))
         "versus"
         ,(make-cdata #f #f (include-template "static.html"))))
