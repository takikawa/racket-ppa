
;; Defines a language to be used by info.ss files

(module infotab mzscheme
  (define-syntax info-module-begin
    (lambda (stx)
      (syntax-case stx ()
	[(mod-beg defn ...)
	 (let ([names (let loop ([defns (syntax->list (syntax (defn ...)))]
                                 [r '()])
                        (if (null? defns)
                          (reverse! r)
                          (loop (cdr defns)
                                (syntax-case (car defns) (define)
                                  [(define var val)
                                   (cons (syntax var) r)]
                                  ;; In case it gets expanded:
                                  [(define-values (var) val)
                                   (cons (syntax var) r)]
                                  [(require lib) r] ; ignore these (see below)
                                  [_else (raise-syntax-error
                                          'infotab-module
                                          "not a well-formed definition"
                                          stx
                                          (car defns))]))))])
           (let ([dup (check-duplicate-identifier names)])
             (when dup
               (raise-syntax-error
                'infotab-module
                "duplicate definition"
                stx
                dup)))
           (with-syntax ([(name ...) names])
             (syntax
               (#%plain-module-begin
                defn ...
                (define #%info-lookup
                  (case-lambda
                   [(n) (#%info-lookup n (lambda () (error 'info.ss "no info for ~a" n)))]
                   [(n fail)
                    (unless (and (procedure? fail)
                                 (procedure-arity-includes? fail 0))
                      (error
                       'info.ss
                       "expected second argument to be a procedure that takes no arguments, got: ~e"
                       fail))
                    (case n
                      [(name) name]
                      ...
                      [else (fail)])]))
		(define (#%info-domain) '(name ...))
                (provide #%info-lookup #%info-domain)))))])))

  (define-syntax (limited-require stx)
    (syntax-case stx ()
      [(_ lib) (member (syntax-object->datum #'lib)
                       '((lib "string-constant.ss" "string-constants")))
       (syntax/loc stx (require lib))]))

  (provide (rename info-module-begin #%module-begin)
	   #%app #%datum #%top
	   define quote
	   list cons car cdr quasiquote unquote unquote-splicing
	   list* append reverse
           string-append
	   path->string build-path collection-path
	   system-library-subpath
           (rename limited-require require)))
