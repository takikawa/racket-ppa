#lang racket/base
(require racket/match
         racket/string
         racket/list
         net/base64)
(provide (all-defined-out))

;; ----

(struct sasl-ctx (outbox k) #:mutable)
;; where outbox : String/Bytes or #f -- #f means no message available
;;       k : (self String/Bytes -> Void) | 'done | 'error
;; INV: if k = 'error then outbox = #f

(define (sasl-next-message ctx)
  (define msg (sasl-ctx-outbox ctx))
  (unless msg
    (error 'sasl-next-message
           "sequence violation: not allowed in current state\n  state: ~s\n  allowed in: ~s"
           (sasl-state ctx) '(send/receive send/done)))
  msg)

(define (sasl-receive-message ctx msg)
  (match (sasl-ctx-k ctx)
    [(? procedure? recv-k) (recv-k ctx msg)]
    [_ (error 'sasl-receive-message
              "sequence violation: not allowed in current state\n  state=~s\n  allowed in: ~s"
              (sasl-state ctx) '(receive send/receive))]))

(define (sasl-state ctx)
  (match (sasl-ctx-k ctx)
    [(? procedure?) (if (sasl-ctx-outbox ctx) 'send/receive 'receive)]
    ['done (if (sasl-ctx-outbox ctx) 'send/done 'done)]
    ['error 'error]))

;; ----

(define (set-sasl! ctx outbox k)
  (set-sasl-ctx-outbox! ctx outbox)
  (set-sasl-ctx-k! ctx k))

;; ----------------------------------------

(struct exn:fail:sasl:fatal exn:fail (msg))

(define (fatal ctx #:who [who0 #f] fmt . args)
  (when ctx
    (set-sasl-ctx-k! ctx 'error)
    (set-sasl-ctx-outbox! ctx #f))
  (define who (or who0 'sasl-receive-message))
  (define msg (apply format fmt args))
  (raise (exn:fail:sasl:fatal (format "~a: ~a" who msg) (current-continuation-marks) msg)))

;; ----------------------------------------

(define (->base64-bytes s [nl #""])
  (base64-encode (if (string? s) (string->bytes/utf-8 s) s) nl))
(define (->base64-string s [nl #""])
  (bytes->string/utf-8 (->base64-bytes s nl)))

;; FIXME: net/base64's decoder skips invalid chars; should validate encoding
(define (base64->bytes s)
  (base64-decode (if (string? s) (string->bytes/utf-8 s) s)))
(define (base64->string s)
  (bytes->string/utf-8 (base64->bytes s)))
