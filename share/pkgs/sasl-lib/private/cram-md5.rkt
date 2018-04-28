#lang racket/base
(require racket/match
         file/sha1
         "saslprep.rkt"
         "base.rkt"
         "crypto.rkt")
(provide (all-defined-out))

;; References:
;; - https://tools.ietf.org/html/draft-ietf-sasl-crammd5-10

(struct cram-md5-client-ctx sasl-ctx (p-authcid p-password))

;; ============================================================

(define (make-cram-md5-client-ctx authcid password)
  (define p-authcid (saslprep authcid #:who 'make-cram-md5-client-ctx))
  (define p-password (string->bytes/utf-8 (saslprep password #:who 'make-cram-md5-client-ctx)))
  (cram-md5-client-ctx #f cram-md5-client-receive-1 p-authcid p-password))

(define (cram-md5-client-receive-1 ctx msg)
  (match-define (cram-md5-client-ctx _ _ p-authcid p-password) ctx)
  (define digest (bytes->hex-string (hmac 'md5 p-password msg)))
  (define response (format "~a ~a" p-authcid digest))
  (set-sasl! ctx response 'done))

(define (cram-md5-client-response authcid password challenge)
  (define p-authcid (saslprep authcid #:who 'cram-md5-client-response))
  (define p-password (string->bytes/utf-8 (saslprep password #:who 'cram-md5-client-response)))
  (define digest (bytes->hex-string (hmac 'md5 p-password challenge)))
  (format "~a ~a" p-authcid digest))
