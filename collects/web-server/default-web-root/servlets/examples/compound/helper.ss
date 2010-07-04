(module helper mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  
  ; get-number : string -> number
  ; to prompt the user for a number
  (define (get-number which-number)
    (let ask ([error-message null])
      (let* ([n-str
              (extract-binding/single
               'n
               (request-bindings
                (send/suspend
                 (let ([prompt (string-append "Enter " which-number ": ")])
                   (build-suspender
                    (list prompt)
                    `(,@error-message
                        (p ,prompt (input ([type "text"] [name "n"])))
                        (input ([type "submit"] [value "Okay"]))))))))]
             [n (string->number n-str)])
        (or n (ask `((p (font ([color "red"]) ,n-str) " is not a number.  Please enter a number."))))))))