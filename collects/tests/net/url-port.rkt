#lang racket
(require net/url
         mzlib/thread
         openssl
         tests/eli-tester)

(define (run-tests scheme wrap-ports skip-actual-redirect?)
  (define ((make-tester url->port) response . responses)
    (define first-port-no 9001)
    (define server-cust
      (make-custodian))
    (parameterize ([current-custodian server-cust])
      (for ([response (in-list (cons response responses))]
            [port-no (in-naturals first-port-no)])
        (thread 
         (λ ()
           (run-server port-no 
                       (lambda (ip op)
                         (let-values ([(ip op) (wrap-ports ip op)])
                           (regexp-match #rx"(\r\n|^)\r\n" ip)
                           (display response op)
                           (close-output-port op)
                           (close-input-port ip)))
                       +inf.0)))))
    (sleep 1)
    (dynamic-wind
     void
     (λ ()
       (call-with-values
        (λ ()
          (url->port
           (url scheme #f "localhost" first-port-no
                #t empty empty #f)))
        (λ vals (apply values (cons (port->string (car vals)) (cdr vals))))))
     (λ ()
       (custodian-shutdown-all server-cust))))

  (define get-pure
    (make-tester get-pure-port))
  (define get-impure
    (make-tester get-impure-port))
  (define get-pure/redirect
    (make-tester (λ (x) (get-pure-port x #:redirections 1))))
  (define get-pure/headers
    (make-tester get-pure-port/headers))
  (define get-pure/headers/redirect
    (make-tester (λ (x) (get-pure-port/headers x #:redirections 1))))
    
  (test
   (get-pure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "This is the data in the first chunk and this is the second one"
   
   (get-pure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   "This is the data in the first chunk and this is the second one"
   
   (get-pure
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   "This is the data in the first chand this is the second oneXXXXXXX"

   (get-pure/redirect
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   "This is the data in the first chunk and this is the second one"
   
   (get-pure/redirect
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   "This is the data in the first chunk and this is the second one"
   
   (get-pure/redirect
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   "This is the data in the first chand this is the second oneXXXXXXX"

   (get-impure
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n")
   =>
   "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one\r\n"
   
   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n")
   =>
   (values "This is the data in the first chunk and this is the second one"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\n")
   
   (get-pure/headers
    "HTTP/1.0 200 OK\r\nContent-Type: text/plain\r\n\r\nThis is the data in the first chunk and this is the second one")
   =>
   (values "This is the data in the first chunk and this is the second one"
           "Content-Type: text/plain\r\n")
   
   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   (values "This is the data in the first chand this is the second oneXXXXXXX"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\n")
   
   (get-pure/headers
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
   =>
   (values "This is the data in the first chand this is the second oneXXXXXXX"
           "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n")
   )
  
  (unless skip-actual-redirect?
    (test
     (get-pure/redirect
      "HTTP/1.1 301 Moved Permanently\r\nLocation: http://localhost:9002/whatever\r\n\r\nstuff"
      (string-append
       "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\n\r\n"
       "24\r\nThis is the data in the first chunk \r\n1A\r\nand this is the second one\r\n0\r\n"))
     
     (get-pure/headers/redirect
      "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n\r\n20\r\nThis is the data in the first ch\r\n21\r\nand this is the second oneXXXXXXX\r\n0\r\n")
     =>
     (values "This is the data in the first chand this is the second oneXXXXXXX"
             "Content-Type: text/plain\r\nTransfer-Encoding: chunked\r\nAnother-Header: ta-daa\r\n"))))

(run-tests "http" values #f)
(run-tests "https" (let ([ctx (ssl-make-server-context)])
                     (ssl-load-certificate-chain! ctx (collection-file-path "test.pem" "openssl"))
                     (ssl-load-private-key! ctx (collection-file-path "test.pem" "openssl"))
                     (lambda (in out)
                       (ports->ssl-ports in out #:mode 'accept #:context ctx)))
           #t)

