#lang racket/base

(require net/tcp-sig
         racket/unit
         "unix-socket.rkt")

(provide
 make-unix-socket-tcp@)

(define (make-unix-socket-tcp@ path)
  (unit
    (import)
    (export tcp^)

    (define (tcp-abandon-port p)
      (error 'tcp-abandon-port "not supported"))

    (define (tcp-accept l)
      (unix-socket-accept l))

    (define (tcp-accept/enable-break l)
      (apply values (sync/enable-break
                     (unix-socket-accept-evt l))))

    (define (tcp-accept-ready? l)
      (and (sync/timeout 0 l) #t))

    (define (tcp-addresses _p [port-numbers? #f])
      (if port-numbers?
          (values "127.0.0.1" 1 "127.0.0.1" 0)
          (values "127.0.0.1" "127.0.0.1")))

    (define (tcp-close l)
      (unix-socket-close-listener l))

    (define (tcp-connect _hostname
                         _port-no
                         [_local-hostname #f]
                         [_local-port-no #f])
      (error 'tcp-connect "not supported"))

    (define (tcp-connect/enable-break _hostname
                                      _port-no
                                      [_local-hostname #f]
                                      [_local-port-no #f])
      (error 'tcp-connect/enable-break "not supported"))

    (define (tcp-listen _port-no
                        [backlog 4]
                        [_reuse? #f]
                        [_hostname #f])
      (unix-socket-listen path backlog))

    (define (tcp-listener? l)
      (unix-socket-listener? l))))
