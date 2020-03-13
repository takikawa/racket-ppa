#lang racket/base
(require rackunit/docs-complete)

(check-docs (quote deinprogramm/sdp/beginner) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)")
(check-docs (quote deinprogramm/sdp/vanilla) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)")
(check-docs (quote deinprogramm/sdp/advanced) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)")
