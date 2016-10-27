(module reader racket/base
  (require syntax/module-reader)

  (provide (rename-out [et-read read]
                       [et-read-syntax read-syntax]
                       [et-get-info get-info]))

  (define (wrap-reader p)
    (lambda args
      (let ([r (apply p args)])
        ;; Re-write module to use `errortrace':
        (if (syntax? r)
            (syntax-case r ()
              [(mod name lang (mod-beg . body))
               (quasisyntax/loc r
                 (mod name #,(datum->syntax r 'errortrace/lang/body) (#,(datum->syntax r '#%module-begin) lang . body)))])
            `(,(car r) ,(cadr r) errortrace/lang/body (#%module-begin ,(caddr r) . ,(cdr (cadddr r))))))))

  (define-values (et-read et-read-syntax et-get-info)
    (make-meta-reader
     'errortrace
     "language path"
     lang-reader-module-paths
     wrap-reader
     wrap-reader
     values)))
