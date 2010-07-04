#lang scheme/base
(require scheme/contract)
(require "../managers/manager.ss"
         web-server/http)

(define servlet-prompt (make-continuation-prompt-tag 'servlet))
(define-struct servlet (custodian namespace manager directory handler)
  #:mutable)
(define-struct execution-context (request))

(define current-servlet (make-parameter #f))
(define current-servlet-instance-id (make-parameter #f))
(define current-execution-context (make-parameter #f))

(define (current-servlet-manager)
  (servlet-manager (current-servlet)))

(provide/contract
 [servlet-prompt continuation-prompt-tag?]
 [struct servlet 
         ([custodian custodian?]
          [namespace namespace?]
          [manager manager?]
          [directory path?]
          [handler (request? . -> . response?)])]
 [struct execution-context 
         ([request request?])]
 [current-servlet (parameter/c (or/c false/c servlet?))]
 [current-servlet-instance-id (parameter/c (or/c false/c number?))]
 [current-execution-context (parameter/c (or/c false/c execution-context?))]
 [current-servlet-manager (-> manager?)])
