#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../file/identity.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "file-stream.rkt"
         "check.rkt")

(provide port-file-identity)

(define/who (port-file-identity p)
  (check who file-stream-port? p)
  (define cp (cond
               [(input-port? p) (->core-input-port p)]
               [else (->core-output-port p)]))
  (start-atomic)
  (check-not-closed who cp)
  (define fd (let ([pd (core-port-data cp)])
               ((file-stream-ref pd) pd)))
  (path-or-fd-identity who #:fd fd #:port p))
