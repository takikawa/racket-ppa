(module modread mzscheme
  (require (lib "contract.ss"))
  
  (provide with-module-reading-parameterization)
  (provide/contract
   [check-module-form ((or/c syntax? eof-object?) symbol? (or/c string? path? false/c) . -> . any)])

  (define (with-module-reading-parameterization thunk)
    (parameterize ([read-case-sensitive #t]
                   [read-square-bracket-as-paren #t]
                   [read-curly-brace-as-paren #t]
                   [read-accept-box #t]
                   [read-accept-compiled #t]
                   [read-accept-bar-quote #t]
                   [read-accept-graph #t]
                   [read-decimal-as-inexact #t]
                   [read-accept-dot #t]
                   [read-accept-quasiquote #t]
                   [read-accept-reader #t]
                   [current-readtable #f])
      (thunk)))

  (define (raise-wrong-module-name filename expected-name name)
    (error 'load-handler
           "expected a `module' declaration for `~a' in ~s, found: ~a"
           expected-name filename name))

  (define (check-module-form exp expected-module filename)
    (cond [(or (eof-object? exp) (eof-object? (syntax-e exp)))
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found end-of-file"
                       expected-module filename))]
          [(compiled-module-expression? (syntax-e exp))
           (if (eq? (module-compiled-name (syntax-e exp)) expected-module)
             ;; It's fine:
             exp
             ;; Wrong name:
             (and filename (raise-wrong-module-name
                            filename expected-module
                            (module-compiled-name (syntax-e exp)))))]
          [(and (syntax? exp)
                (syntax-case exp ()
                  [(mod nm . _)
                   (and (eq? (syntax-e #'mod) 'module) (identifier? #'nm))]
                  [_else #f]))
           ;; It's ok; need to install a specific `module' binding:
           (with-syntax ([(mod nm . _) exp])
             (unless (eq? (syntax-e #'nm) expected-module)
               (raise-wrong-module-name filename expected-module
                                        (syntax-e #'nm)))
             (datum->syntax-object exp
                                   (cons #'module (cdr (syntax-e exp)))
                                   exp
                                   exp))]
          [else
           (and filename
                (error 'load-handler
                       "expected a `module' declaration for `~a' in ~s, but found something else"
                       expected-module filename))])))
