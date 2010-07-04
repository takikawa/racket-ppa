(module module-reader scheme/base

(provide (rename-out [provide-module-reader #%module-begin]
                     [wrap wrap-read-all])
         (except-out (all-from-out scheme/base) #%module-begin))

(require (for-syntax scheme/base))

(define-syntax (provide-module-reader stx)
  (define (err str [sub #f])
    (raise-syntax-error 'syntax/module-reader str sub))
  (define-syntax-rule (keywords body [kwd var default] ... [checks ...])
    (begin
      (define var #f) ...
      (set! body
        (let loop ([body body])
          (if (not (and (pair? body)
                        (pair? (cdr body))
                        (keyword? (syntax-e (car body)))))
            (datum->syntax stx body stx)
            (let* ([k (car body)] [k* (syntax-e k)] [v (cadr body)])
              (case k*
                [(kwd) (if var
                         (err (format "got two ~s keywords" k*) k)
                         (begin (set! var v) (loop (cddr body))))]
                ...
                [else (err "got an unknown keyword" (car body))])))))
      checks ...
      (set! var (or var default)) ...))
  (define (construct-reader lang body)
    (keywords body
              [#:language    ~lang        lang]
              [#:read        ~read        #'read]
              [#:read-syntax ~read-syntax #'read-syntax]
              [#:wrapper1    ~wrapper1    #'#f]
              [#:wrapper2    ~wrapper2    #'#f]
              [#:whole-body-readers? ~whole-body-readers? #'#f]
      [(when (equal? (and lang #t) (and ~lang #t))
         (err (string-append "must specify either a module path, or #:lang"
                             (if (and lang ~lang) ", not both" ""))))
       (unless (equal? (and ~read #t) (and ~read-syntax #t))
         (err "must specify either both #:read and #:read-syntax, or none"))
       (when (and ~whole-body-readers? (not (and ~read ~read-syntax)))
         (err "got a #:whole-body-readers? without #:read and #:read-syntax"))])
    (quasisyntax/loc stx
      (#%module-begin
       #,@body
       (#%provide (rename *read read) (rename *read-syntax read-syntax))
       (define-values (*read *read-syntax)
         (let* ([lang #,~lang]
                [rd  #,~read]
                [rds #,~read-syntax]
                [w1  #,~wrapper1]
                [w2  #,~wrapper2]
                [w2  (cond [(not w2) (lambda (in r _) (r in))]
                           [(procedure-arity-includes? w2 3) w2]
                           [else (lambda (in r _) (w2 in r))])]
                [whole? #,~whole-body-readers?])
           (values
            (lambda (in modpath line col pos)
              (w2 in
                  (lambda (in)
                    (wrap-internal lang in rd whole?
                                   w1 #f modpath #f line col pos))
                  #f))
            (lambda (src in modpath line col pos)
              (w2 in
                  (lambda (in)
                    (wrap-internal lang in (lambda (in) (rds src in)) whole?
                                   w1 #t modpath src line col pos))
                  #t))))))))
  (syntax-case stx ()
    [(_ lang body ...)
     (not (keyword? (syntax-e #'lang)))
     (construct-reader #''lang (syntax->list #'(body ...)))]
    [(_ body ...) (construct-reader #f (syntax->list #'(body ...)))]))

(define (wrap-internal lang port read whole? wrapper stx?
                       modpath src line col pos)
  (let* ([lang (if (procedure? lang)
                 (parameterize ([current-input-port port]) (lang))
                 lang)]
         [lang (if stx? (datum->syntax #f lang modpath modpath) lang)]
         [body (lambda ()
                 (if whole?
                   (read port)
                   (let loop ([a null])
                     (let ([v (read port)])
                       (if (eof-object? v) (reverse a) (loop (cons v a)))))))]
         [body (cond [(not wrapper) (body)]
                     [(procedure-arity-includes? wrapper 2) (wrapper body stx?)]
                     [else (wrapper body)])]
         [all-loc (vector src line col pos
                          (let-values ([(l c p) (port-next-location port)])
                            (and p (- p pos))))]
         [body (if (and stx? (not (syntax? body)))
                 (datum->syntax #f body all-loc)
                 body)]
         [p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol
                    (path->string (path-replace-suffix name #""))))
                 'page)]
         [tag-src (lambda (v)
                    (if stx?
                      (datum->syntax
                       #f v (vector src line col pos
                                    (- (or (syntax-position modpath) (add1 pos))
                                       pos)))
                      v))]
         [r `(,(tag-src 'module) ,(tag-src name) ,lang . ,body)])
    (if stx? (datum->syntax #f r all-loc) r)))

(define (wrap lang port read modpath src line col pos)
  (wrap-internal lang port read #f #f #f modpath src line col pos))

)
