#lang racket
(require net/url
         racket/contract
         racket/serialize
         web-server/http
         web-server/managers/manager
         web-server/private/define-closure
         web-server/private/servlet       
         web-server/stuffers/stuffer
         web-server/lang/abort-resume
         web-server/lang/stuff-url)

(define-struct (stateless-servlet servlet) (stuffer))

(provide
 ;; Server Interface
 initialize-servlet 
 
 ;; Servlet Interface
 send/suspend
 send/suspend/dispatch
 send/suspend/hidden
 send/suspend/url
 send/suspend/url/dispatch
 redirect/get)

(provide/contract
 [make-stateless-servlet
  (custodian? namespace? manager? path-string? (request? . -> . response/c)
              (stuffer/c serializable? bytes?) . -> . stateless-servlet?)])

; These contracts interfere with the continuation safety marks
#;(provide/contract
   ;; Server Interface
   [initialize-servlet ((request? . -> . response/c) . -> . (request? . -> . response/c))]
   
   ;; Servlet Interface
   [send/suspend/hidden ((url? list? . -> . response/c) . -> . request?)]
   [send/suspend/url ((url? . -> . response/c) . -> . request?)]
   [send/suspend/url/dispatch ((((request? . -> . any/c) . -> . url?) . -> . response/c)
                               . -> . any/c)]
   [redirect/get (-> request?)])

;; initial-servlet : (request -> response) -> (request -> response/c)
(define (initialize-servlet start)
  (let ([params (current-parameterization)])
    (lambda (req0)
      (call-with-parameterization 
       params
       (lambda ()
         (dispatch
          (lambda (req1)
            (or (request->continuation req1)
                ; Try to decode a continuation from the request,
                ; or, use the start procedure to initialize a session
                (lambda (req2) (dispatch-start start req2))))
          req0))))))

;; send/suspend/hidden: (url input-field -> response) -> request
;; like send/suspend except the continuation is encoded in a hidden field
(define (send/suspend/hidden page-maker)
  (call-with-serializable-current-continuation
   (lambda (k)
     (define stuffer (stateless-servlet-stuffer (current-servlet)))
     (define p-cont ((stuffer-in stuffer) k))
     (page-maker
      (request-uri (execution-context-request (current-execution-context)))
      `(input ([type "hidden"] [name "kont"] [value ,(format "~s" p-cont)]))))))

;; send/suspend/url: (url -> response) -> request
;; like send/suspend except the continuation is encoded in the url
(define (send/suspend/url page-maker)
  (call-with-serializable-current-continuation
   (lambda (k)
     (define stuffer (stateless-servlet-stuffer (current-servlet)))
     (page-maker
      (stuff-url stuffer 
                 (request-uri (execution-context-request (current-execution-context)))
                 k)))))

(define (send/suspend page-maker)
  (send/suspend/url
   (lambda (k-url)
     (page-maker (url->string k-url)))))

(define-closure embed/url (proc) (k string?)
  (let ([url
         (stuff-url (stateless-servlet-stuffer (current-servlet))
                    (request-uri (execution-context-request (current-execution-context)))
                    (kont-append-fun k proc))])
    (if string?
        (url->string url)
        url)))

(define (send/suspend/url/dispatch response-generator)
  (call-with-serializable-current-continuation
   (lambda (k)
     (response-generator (make-embed/url (lambda () (values k #f)))))))
(define (send/suspend/dispatch response-generator)
  (call-with-serializable-current-continuation
   (lambda (k)
     (response-generator (make-embed/url (lambda () (values k #t)))))))

;; request->continuation: req -> continuation
;; decode the continuation from the hidden field of a request
(define (request->continuation req)
  (define stuffer (stateless-servlet-stuffer (current-servlet)))
  (or
   ; Look in url for c=<k>
   (let ([req-url (request-uri req)])
     (and (stuffed-url? req-url)
          (unstuff-url stuffer req-url)))
   ; Look in query for kont=<k>
   (match (bindings-assq #"kont" (request-bindings/raw req))
     [(struct binding:form (id kont))
      ((stuffer-out stuffer)
       (read (open-input-bytes kont)))]
     [_ #f]))) 

(define (redirect/get)
  (send/suspend/url (lambda (k-url) (redirect-to (url->string k-url) temporarily))))
