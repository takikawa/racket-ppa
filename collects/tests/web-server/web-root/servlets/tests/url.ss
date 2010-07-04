(require mzlib/unitsig
         web-server/servlet-sig
         net/url)
(let ([count 0])
  (unit/sig ()
    (import servlet^)
    (set! count (add1 count))
    `(html (head (title "URL Test"))
           (body (p "The method requested is: " ,(format "~s" (request-method initial-request)))
                 (p "The URL requested is: " ,(url->string (request-uri initial-request)))
                 (p "count is: " ,(number->string count))))))
