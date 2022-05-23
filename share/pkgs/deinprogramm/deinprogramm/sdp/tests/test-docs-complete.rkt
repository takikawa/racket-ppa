#lang racket/base
(require rackunit/docs-complete)

(check-docs (quote deinprogramm/sdp/beginner) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)|(^define-record-functions$)")
(check-docs (quote deinprogramm/sdp/vanilla) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)|(^nonempty-list-of$)|(^define-record-functions$)")
(check-docs (quote deinprogramm/sdp/advanced) #:skip #rx"(^#%)|(^\\.\\.)|(^one-of$)|(^nonempty-list-of$)|(^define-record-functions$)")
