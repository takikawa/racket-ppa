(import (rumble)
        (io)
        (thread))

(define-syntax test
  (syntax-rules ()
    [(_ expect rhs)
     (let ([e expect]
           [v rhs])
       (unless (equal? e v)
         (error 'failed "~s: ~e" 'rhs v)))]))

;; ----------------------------------------

(test #t (directory-exists? "demo"))
(test #f (directory-exists? "no-such-demo"))

;; ----------------------------------------

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ([total 0])
         (define s (read-string 100 p))
         (unless (eof-object? s)
           (loop (+ total (string-length s)))))
       (loop (sub1 j))))))

(time
 (let loop ([j 10])
   (unless (zero? j)
     (let ()
       (define p (open-input-file "compiled/io.scm"))
       (port-count-lines! p)
       (let loop ()
         (unless (eof-object? (read-byte p))
           (loop)))
       (close-input-port p)
       (loop (sub1 j))))))

(time
 (let loop ([i 1000000] [v #f])
   (if (zero? i)
       v
       (loop (sub1 i)
             (bytes->string/utf-8 (string->bytes/utf-8 "ap\x3BB;ple"))))))


;; ----------------------------------------

(let ([c (make-custodian)])
  (with-continuation-mark
      parameterization-key
      (extend-parameterization (continuation-mark-set-first #f parameterization-key) current-custodian c)
    (let ()
      (define p (open-input-file "compiled/io.scm"))
      (define wb (make-weak-box p))
      (define we (make-will-executor))
      (will-register we p values)
      (set! p #f)
      (collect (collect-maximum-generation))
      (test #t (input-port? (will-try-execute we)))
      (collect (collect-maximum-generation))
      (test #f (weak-box-value wb))
      (custodian-shutdown-all c))))

;; ----------------------------------------

(call-in-main-thread
 (lambda ()
   (define root-logger (make-logger))

   (test 'none (log-max-level root-logger))
   (add-stderr-log-receiver! root-logger 'warning)

   (test 'warning (log-max-level root-logger))

   (log-message root-logger 'error "this should print to stderr" 5)

   (let ()
     (define demo1-logger (make-logger 'demo1 root-logger))
     (define demo2-logger (make-logger 'demo2 root-logger 'fatal))

     (log-message demo1-logger 'error "this should print to stderr, too" 5)
     (log-message demo2-logger 'error "this should not print to stderr" 5)

     (test 'warning (log-max-level demo1-logger))
     (test 'fatal (log-max-level demo2-logger))

     (let ()
       (define lr1 (make-log-receiver root-logger 'info 'cats))

       (test 'info (log-max-level demo1-logger))
       (test 'fatal (log-max-level demo2-logger))

       (test 'info (log-max-level demo1-logger 'cats))
       (test 'fatal (log-max-level demo2-logger 'cats))

       (test 'warning (log-max-level demo1-logger 'dogs))
       (test 'fatal (log-max-level demo2-logger 'dogs))

       (test #t (log-level? demo1-logger 'info 'cats))
       (test #f (log-level? demo1-logger 'debug 'cats))
       (test #f (log-level? demo1-logger 'info 'dogs))

       (let ()
         (define msg1 #f)
         (define th1 (thread (lambda () (set! msg1 (sync lr1)))))
         (sync (system-idle-evt))
         (test #f msg1)

         (log-message demo1-logger 'info 'cats "hello" 7)
         (sync (system-idle-evt))
         (test '#(info "cats: hello" 7 cats) msg1)

         (log-message demo1-logger 'info 'cats "goodbye" 9)
         (test '#(info "cats: goodbye" 9 cats) (sync lr1)))))))
