#lang racket/base
(require rackunit/docs-complete)

(check-docs (quote deinprogramm/sdp/beginner) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
(check-docs (quote deinprogramm/sdp/vanilla) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
(check-docs (quote deinprogramm/sdp/advanced) #:skip #rx"(^#%)|(^\\.\\.)|(^contract$)|(^define-contract$)")
