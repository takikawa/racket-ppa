
(module stxparam-exptime '#%kernel
  (#%require "private/stx.ss" "private/stxcase-scheme.ss" 
             "private/small-scheme.ss" 
             "private/stxloc.ss" "private/stxparamkey.ss")

  (#%provide syntax-parameter-value
             make-parameter-rename-transformer)
  
  (define-values (syntax-parameter-value)
    (lambda (id)
      (let* ([v (syntax-local-value id (lambda () #f))]
             [v (if (set!-transformer? v)
                    (set!-transformer-procedure v)
                    v)])
        (unless (syntax-parameter? v)
          (raise-type-error 'syntax-parameter-value "syntax parameter" v))
        (let ([target (syntax-parameter-target v)])
          (syntax-parameter-target-value target)))))

  (define-values (make-parameter-rename-transformer)
    (lambda (id)
      (make-set!-transformer
       (lambda (stx)
         (let ([v (syntax-parameter-value (syntax-local-introduce id))])
           (apply-transformer v stx #'set!)))))))
