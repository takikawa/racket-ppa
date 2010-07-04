#lang scheme/base
(require web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (parameterize ([current-servlet-continuation-expiration-handler
                  (lambda _
                    `(html (body "Expired")))])
    (send/suspend (lambda (k-url) `(html (a ([href ,k-url]) "Link"))))
    (send/forward (lambda (k-url) `(html (a ([href ,k-url]) "Link"))))
    (send/finish `(html (body "Done.")))))
