; Derived from plai/web/server, which was based on an older version of this
; Also derived from planet/untyped/instaservlet
#lang scheme
(require (prefix-in net: net/sendurl)
         scheme/contract
         scheme/list
         scheme/unit
         scheme/serialize
         net/tcp-unit
         net/tcp-sig
         net/ssl-tcp-unit)
(require web-server/web-server
         web-server/managers/lru
         web-server/managers/manager
         web-server/configuration/namespace
         web-server/http
         web-server/stuffers
         web-server/servlet/setup
         web-server/dispatchers/dispatch
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in servlets: web-server/dispatchers/dispatch-servlets))

(define send-url (make-parameter net:send-url))

(provide/contract
 [dispatch/servlet (((request? . -> . response/c))
                    (#:regexp regexp?
                     #:current-directory path-string?
                     #:namespace (listof module-path?)
                     #:stateless? boolean?
                     #:stuffer (stuffer/c serializable? bytes?)
                     #:manager manager?)
                    . ->* .
                    dispatcher/c)]
 [serve/launch/wait (((semaphore? . -> . dispatcher/c))
                     (#:launch-path (or/c false/c string?)
                      #:banner? boolean?
                      #:listen-ip (or/c false/c string?)
                      #:port number?
                      #:ssl-keys (or/c false/c (cons/c path-string? path-string?)))
                     . ->* .
                     void)])

(define (dispatch/servlet 
         start
         #:regexp
         [servlet-regexp #rx""]
         #:current-directory 
         [servlet-current-directory (current-directory)]
         #:namespace 
         [servlet-namespace empty]                  
         #:stateless? 
         [stateless? #f]
         #:stuffer
         [stuffer default-stuffer]
         #:manager
         [manager
          (make-threshold-LRU-manager
           (lambda (request)
             `(html (head (title "Page Has Expired."))
                    (body (p "Sorry, this page has expired. Please go back."))))
           (* 64 1024 1024))])
  (define servlet-box (box #f))
  (define make-servlet-namespace
    (make-make-servlet-namespace #:to-be-copied-module-specs servlet-namespace))
  (filter:make
   servlet-regexp
   (servlets:make
    (lambda (url)
      (or (unbox servlet-box)
          (let ([servlet
                 (parameterize ([current-custodian (make-custodian)]
                                [current-namespace
                                 (make-servlet-namespace
                                  #:additional-specs
                                  default-module-specs)])
                   (if stateless?
                       (make-stateless.servlet servlet-current-directory stuffer start)
                       (make-v2.servlet servlet-current-directory manager start)))])
            (set-box! servlet-box servlet)
            servlet))))))

(define (serve/launch/wait
         dispatcher
         
         #:launch-path
         [launch-path #f]          
         #:banner?
         [banner? #t]
         
         #:listen-ip
         [listen-ip "127.0.0.1"]
         #:port
         [port 8000]
         #:ssl-keys
         [ssl-keys #f])
  (define ssl? (pair? ssl-keys))
  (define server-url
    (string-append (if ssl? "https" "http")
                   "://localhost"
                   (if (and (not ssl?) (= port 80))
                     "" (format ":~a" port))))
  (define sema (make-semaphore 0))
  (define shutdown-server
    (serve #:dispatch (dispatcher sema)
           #:listen-ip listen-ip
           #:port port
           #:tcp@ (if ssl?
                    (let ()
                      (define-unit-binding ssl-tcp@
                        (make-ssl-tcp@
                         (car ssl-keys) (cdr ssl-keys)
                         #f #f #f #f #f)
                        (import) (export tcp^))
                      ssl-tcp@)
                    tcp@)))
  (when launch-path
    ((send-url) (string-append server-url launch-path) #t))
  (when banner?
    (printf "Your Web application is running at ~a.\n" 
            (if launch-path 
                (string-append server-url launch-path)
                server-url))
    (printf "Click 'Stop' at any time to terminate the Web Server.\n"))
  (let ([bye (lambda ()
               (when banner? (printf "\nWeb Server stopped.\n"))
               (shutdown-server))])
    (with-handlers ([exn:break? (lambda (exn) (bye))])
      (semaphore-wait/enable-break sema)
      ;; We can get here if a /quit url is visited
      (bye))))
