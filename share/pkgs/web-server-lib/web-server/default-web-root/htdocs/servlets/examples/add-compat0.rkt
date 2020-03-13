#lang racket/base
(require web-server/servlet
         web-server/compat/0/coerce)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)

; request-number : str -> num
(define (request-number which-number)
  (send/suspend/dispatch
   (lambda (embed/url)
     `(html (head (title "Enter a Number to Add"))
            (body ([bgcolor "white"])
                  (form ([action ,(embed/url
                                   (lambda (request)
                                     (string->number
                                      (extract-binding/single
                                       'number
                                       (request-bindings request)))))]
                         [method "post"])
                        "Enter the " ,which-number " number to add: "
                        (input ([type "text"] [name "number"] [value ""]))
                        (input ([type "submit"] [name "enter"] [value "Enter"]))))))))

(define (start initial-request)
  `(html (head (title "Sum"))
         (body ([bgcolor "white"])
               (p "The answer is "
                  ,(number->string (+ (request-number "first") (request-number "second")))))))
