#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         mzlib/list
         net/url
         web-server/servlet/servlet-url
         web-server/private/request-structs)
(provide servlet-url-tests)

(define servlet-url-tests
  (test-suite
   "Servlet URLs"
   
   (test-case
    "Basic"
    (check-equal? (servlet-url->url-string/no-continuation
                   (request->servlet-url
                    (make-request 'get (string->url "http://localhost/servlets;1*1*65539753/examples/add.ss")
                                  empty empty #f
                                  "host" 80 "client")))
                  "http://localhost/servlets/examples/add.ss"))
   
   (test-case
    "Param"
    (check-equal? (servlet-url->url-string/no-continuation
                   (request->servlet-url
                    (make-request 'get (string->url "http://localhost/servlets;1*1*65539753/examples/add.ss;foo")
                                  empty empty #f
                                  "host" 80 "client")))
                  "http://localhost/servlets/examples/add.ss;foo"))))
