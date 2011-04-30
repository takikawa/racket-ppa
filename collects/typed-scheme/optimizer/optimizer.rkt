#lang scheme/base

(require syntax/parse unstable/syntax
         racket/pretty
         (for-template scheme/base)
         "../utils/utils.rkt"
         (optimizer utils number fixnum float float-complex vector string
                    pair sequence box struct dead-code apply unboxed-let))

(provide optimize-top)


(define-syntax-class opt-expr
  #:commit
  (pattern e:opt-expr*
           #:with opt (syntax-recertify #'e.opt this-syntax (current-code-inspector) #f)))

(define-syntax-class opt-expr*
  #:commit
  #:literal-sets (kernel-literals)

  ;; interesting cases, where something is optimized
  (pattern e:dead-code-opt-expr       #:with opt #'e.opt)
  (pattern e:unboxed-let-opt-expr     #:with opt #'e.opt)
  (pattern e:apply-opt-expr           #:with opt #'e.opt)
  (pattern e:number-opt-expr          #:with opt #'e.opt)
  (pattern e:fixnum-opt-expr          #:with opt #'e.opt)
  (pattern e:float-opt-expr           #:with opt #'e.opt)
  (pattern e:float-complex-opt-expr   #:with opt #'e.opt)
  (pattern e:vector-opt-expr          #:with opt #'e.opt)
  (pattern e:string-opt-expr          #:with opt #'e.opt)
  (pattern e:pair-opt-expr            #:with opt #'e.opt)
  (pattern e:sequence-opt-expr        #:with opt #'e.opt)
  (pattern e:box-opt-expr             #:with opt #'e.opt)
  (pattern e:struct-opt-expr          #:with opt #'e.opt)
  
  ;; boring cases, just recur down
  (pattern ((~and op (~or (~literal #%plain-lambda) (~literal define-values)))
            formals e:expr ...)
           #:with opt (syntax-track-origin (quasisyntax/loc this-syntax (op formals #,@(syntax-map (optimize) #'(e ...))))
                                           this-syntax
                                           #'op))
  (pattern (case-lambda [formals e:expr ...] ...)
           ;; optimize all the bodies
           #:with (opt-parts ...)
           (syntax-map (lambda (part)
                         (let ((l (syntax->list part)))
                           (cons (car l)
                                 (map (optimize) (cdr l)))))
                       #'([formals e ...] ...))
           #:with opt #'(case-lambda opt-parts ...))
  (pattern ((~and op (~or (~literal let-values) (~literal letrec-values)))
            ([ids e-rhs:expr] ...) e-body:expr ...)
           #:with (opt-rhs ...) (syntax-map (optimize) #'(e-rhs ...))
           #:with opt #`(op ([ids opt-rhs] ...)
                            #,@(syntax-map (optimize) #'(e-body ...))))
  (pattern (letrec-syntaxes+values stx-bindings
                                   ([(ids ...) e-rhs:expr] ...)
                                   e-body:expr ...)
           ;; optimize all the rhss
           #:with (opt-clauses ...)
           (syntax-map (lambda (clause)
                         (let ((l (syntax->list clause)))
                           (list (car l) ((optimize) (cadr l)))))
                       #'([(ids ...) e-rhs] ...))
           #:with opt #`(letrec-syntaxes+values
                         stx-bindings
                         (opt-clauses ...)
                         #,@(syntax-map (optimize) #'(e-body ...))))
  (pattern (kw:identifier expr ...)
           #:when 
	   (for/or ([k (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%app #'#%expression
			     #'#%variable-reference #'with-continuation-mark)])
	     (free-identifier=? k #'kw))
           ;; we don't want to optimize in the cases that don't match the #:when clause
           #:with opt #`(kw #,@(syntax-map (optimize) #'(expr ...))))
  (pattern other:expr
           #:with opt #'other))

(define (optimize-top stx)
  (let ((port (if (and *log-optimizations?*
                       *log-optimizatons-to-log-file?*)
                  (open-output-file *optimization-log-file*
                                    #:exists 'append)
                  (current-output-port))))
    (begin0
      (parameterize ([current-output-port port]
                     [optimize (syntax-parser
                                [e:expr
                                 #:when (and (not (syntax-property #'e 'typechecker:ignore))
                                             (not (syntax-property #'e 'typechecker:ignore-some))
                                             (not (syntax-property #'e 'typechecker:with-handlers)))
                                 #:with e*:opt-expr #'e
                                 #'e*.opt]
                                [e:expr #'e])])
        (let ((result ((optimize) stx)))
          (when *show-optimized-code*
            (pretty-print (syntax->datum result)))
          result))
      (when (and *log-optimizations?*
                 *log-optimizatons-to-log-file?*)
        (close-output-port port)))))
