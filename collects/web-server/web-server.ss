#lang scheme/base
(require mzlib/plt-match
         net/tcp-sig
         (prefix-in raw: net/tcp-unit)
         mzlib/unit
         scheme/contract
         "dispatchers/dispatch.ss"
         "private/dispatch-server-sig.ss"
         "private/dispatch-server-unit.ss"
         "web-config-sig.ss"
         "web-server-sig.ss"
         "web-server-unit.ss"
         (prefix-in http: "private/request.ss"))
(provide/contract
 [serve
  (->* (#:dispatch dispatcher/c)
       (#:tcp@ unit?
               #:port number?
               #:listen-ip (or/c false/c string?)
               #:max-waiting number?
               #:initial-connection-timeout number?)
       (-> void))]
 [serve/ports
  (->* (#:dispatch dispatcher/c)
       (#:tcp@ unit?
               #:ports (listof number?)
               #:listen-ip (or/c false/c string?)
               #:max-waiting number?
               #:initial-connection-timeout number?)
       (-> void))]
 [serve/ips+ports
  (->* (#:dispatch dispatcher/c)
       (#:tcp@ unit?
               #:ips+ports (listof (cons/c (or/c false/c string?) (listof number?)))
               #:max-waiting number?
               #:initial-connection-timeout number?)
       (-> void))]
 [do-not-return (-> void)]
 [serve/web-config@ (unit? . -> . (-> void?))])

(define (do-not-return)
  (semaphore-wait (make-semaphore 0)))

(define (serve
         #:dispatch dispatch
         #:tcp@ [tcp@ raw:tcp@]
         #:port [port 80]
         #:listen-ip [listen-ip #f]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define read-request http:read-request)
  (define-unit-binding a-tcp@
    tcp@ (import) (export tcp^))
  (define-compound-unit/infer dispatch-server@/tcp@
    (import dispatch-server-config^)
    (link a-tcp@ dispatch-server@)
    (export dispatch-server^))
  (define-values/invoke-unit
    dispatch-server@/tcp@
    (import dispatch-server-config^)
    (export dispatch-server^))
  
  (serve))

(define (serve/ports
         #:dispatch dispatch
         #:tcp@ [tcp@ raw:tcp@]
         #:ports [ports (list 80)]
         #:listen-ip [listen-ip #f]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define shutdowns
    (map (lambda (port)
           (serve #:dispatch dispatch
                  #:tcp@ tcp@
                  #:port port
                  #:listen-ip listen-ip
                  #:max-waiting max-waiting
                  #:initial-connection-timeout initial-connection-timeout))
         ports))
  (lambda ()
    (for-each apply shutdowns)))

(define (serve/ips+ports
         #:dispatch dispatch
         #:tcp@ [tcp@ raw:tcp@]
         #:ips+ports [ips+ports (list (cons #f (list 80)))]
         #:max-waiting [max-waiting 40]
         #:initial-connection-timeout [initial-connection-timeout 60])
  (define shutdowns
    (map (match-lambda
           [(list-rest listen-ip ports)
            (serve #:dispatch dispatch
                   #:tcp@ tcp@
                   #:ports ports
                   #:listen-ip listen-ip
                   #:max-waiting max-waiting
                   #:initial-connection-timeout initial-connection-timeout)])
         ips+ports))
  (lambda ()
    (for-each apply shutdowns)))

; serve/config@ : configuration -> (-> void)
(define (serve/web-config@ config@)
  (define-unit m@ (import web-server^) (export)
    (init-depend web-server^)
    (serve))
  (define-unit-binding c@ config@ (import) (export web-config^))
  (invoke-unit
   (compound-unit/infer
    (import)
    (link raw:tcp@ c@ web-server@ m@)
    (export))))
