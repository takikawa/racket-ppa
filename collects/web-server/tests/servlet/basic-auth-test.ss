#lang scheme/base
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         web-server/private/request-structs
         web-server/servlet/basic-auth)
(provide basic-auth-tests)

(define basic-auth-tests
  (test-suite
   "BASIC Authentication"
   
   (test-case
    "Simple"
    (check-equal? (extract-user-pass (list (make-header #"Authorization" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")))
                  (cons #"Aladdin" #"open sesame")))
   
   (test-case
    "Value error"
    (check-false (extract-user-pass (list (make-header #"Authorization" #"Basic adfadQWxhZGRpb124134jpvcGVu=")))))
   
   (test-case
    "No header"
    (check-false (extract-user-pass (list))))
   
   (test-case
    "Case"
    (check-equal? (extract-user-pass (list (make-header #"AuthoRIZation" #"Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")))
                  (cons #"Aladdin" #"open sesame")))))
