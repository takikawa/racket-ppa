
;; Implements the syntactic forms for the HtDP teaching languages. The
;; reader-level aspects of the language (e.g., case-sensitivity) are
;; not implemented here, and the procedures are in a separate
;; module.

;; To a first approximation, this module is one big error-checking
;; routine. In other words, most of the syntax implementations are
;; straightforward, but error-checking is complex.

;; Error-message conventions:
;;  - Report errors, somewhat anthropomorphically, in terms of "expected"
;;    versus "found" syntax.
;;  - Report errors according to a left-to-right reading; e.g., the
;;    error in `(define 1 2 3)' is "expected an identifier, found 1",
;;    not "expected two parts after `define', but found three".
;;  - The error message should always explain what is wrong, not simply
;;    state a fact. For example, "f defined previously, so it cannot
;;    be re-defined here" is a good error message; in contrast, "found
;;    second definition of f here" doesn't say what's wrong with a second
;;    definition.

;; Left-to-right reporting sometimes requires an explicit expression
;; check before reporting some other error. For example, in the
;; expression (cond [true + 1 2]), the reported error should ideally
;; be for a misuse of "+", not that there are two extra parts in the
;; clause. This check requires local-expanding, so it doesn't work
;; when checking top-level forms like `define' (because not all of the
;; definitions are ready, yet). For other cases, ensure that the
;; expansion is in an expression position (not the top level) and use
;; the `local-expand-for-error' function instead of `local-expand' to
;; help declare that the expansion is merely for left-to-right error
;; reporting. As always, avoid using `local-expand' if there's no
;; error.

(module teach mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
	   (lib "math.ss")
           "set-result.ss")
  (require-for-syntax "teachhelp.ss"
		      (lib "kerncase.ss" "syntax")
		      (lib "stx.ss" "syntax")
		      (lib "struct.ss" "syntax")
		      (lib "context.ss" "syntax")
		      (lib "include.ss")
                      (lib "shared.ss" "stepper" "private"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; run-time helpers
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; verify-boolean is inserted to check for boolean results:
  (define (verify-boolean b where)
    (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
        (format "~a: question result is not true or false: ~e" where b)
        (current-continuation-marks)))))

  ;; Wrapped around uses of local-bound variables:
  (define (check-not-undefined name val)
    (if (eq? val undefined)
      (raise
       (make-exn:fail:contract:variable
        (format "local variable used before its definition: ~a" name)
        (current-continuation-marks)
        name))
      val))
  (define undefined (letrec ([x x]) x))

  ;; Wrapped around top-level definitions to disallow re-definition:
  (define (check-top-level-not-defined who id)
    (when (let ([b (identifier-binding id)])
	    ;; if it's not top-level, raise an exn
	    (if b
		#t
		;; At top-level, might be bound to syntax or value:
		(with-handlers ([exn:fail:contract:variable? (lambda (exn) #f)]
				[exn:fail:syntax? (lambda (exn) #t)])
		  (namespace-variable-value (syntax-e id) #t)
		  #t)))
      (error who "cannot redefine name: ~a" (syntax-e id))))

  ;; For quasiquote and shared:
  (require (rename "teachprims.ss" the-cons advanced-cons))
  (require (only   "teachprims.ss" cyclic-list?))

  ;; Referenced to ensure that evaluating `lambda' always
  ;; produces a new closure (instead of using a closure
  ;; that's allocated once)
  (define make-lambda-generative 5)
  
  ;; A consistent pattern for stepper-skipto:
  (define-for-syntax (stepper-ignore-checker stx)
    (stepper-syntax-property stx 'stepper-skipto '(syntax-e cdr syntax-e cdr car)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; syntax implementations
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax define-syntax-set/provide
    (lambda (stx)
      (syntax-case stx ()
	[(_ (id ...) defn ...)
	 (with-syntax ([(plain-id ...)
			(map (lambda (id)
			       (if (identifier? id)
				   id
				   (stx-car (stx-cdr id))))
			     (syntax->list (syntax (id ...))))]
		       [provided-identifiers
			(datum->syntax-object stx 'provided-identifiers)])
	   (syntax
	    (begin
	      (provide plain-id ...)
	      (define-syntax-set (id ...) 
		(define provided-identifiers (quote-syntax (id ...)))
		defn ...))))])))

  ;; The implementation of form X is defined below as X/proc. The
  ;; reason for this is to allow the implementation of Y to re-use the
  ;; implementation of X (expanding to a use of X would mangle syntax
  ;; error messages), while preserving the binding of X as the one for
  ;; the syntax definition (so that quasiquote can recognize unquote,
  ;; etc.).

  (define-syntax-set/provide (beginner-define
			      beginner-define-struct
			      beginner-lambda
			      beginner-app     beginner-app-continue
			      beginner-top     beginner-top-continue
			      beginner-cond
			      beginner-else
			      beginner-if
			      beginner-and
			      beginner-or
			      beginner-quote
                              beginner-require
			      
			      intermediate-define
			      intermediate-define-struct
			      intermediate-local
			      intermediate-letrec
			      intermediate-let
			      intermediate-let*
			      intermediate-recur
			      intermediate-lambda
			      intermediate-app
			      intermediate-quote
			      intermediate-quasiquote
			      intermediate-unquote
			      intermediate-unquote-splicing
			      intermediate-time

			      advanced-define
			      advanced-lambda
			      advanced-app
			      advanced-set!  advanced-set!-continue
			      advanced-when
			      advanced-unless
			      advanced-define-struct
			      advanced-let
			      advanced-recur
			      advanced-begin
			      advanced-begin0
			      advanced-case
			      advanced-shared
			      advanced-delay)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; compile-time helpers
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Raise a syntax error:
    (define (teach-syntax-error form stx detail msg . args)
      (let ([form (if (eq? form '|function call|)
		      form
		      #f)] ; extract name from stx
	    [msg (apply format msg args)])
	(if detail
	    (raise-syntax-error form msg stx detail)
	    (raise-syntax-error form msg stx))))
    
    (define (binding-in-this-module? b)
      (and (list? b)
	   (module-path-index? (car b))
	   (let-values ([(path base) (module-path-index-split (car b))])
	     (and (not path) (not base)))))
    
    ;; The syntax error when a form's name doesn't follow a "("
    (define (bad-use-error name stx)
      (teach-syntax-error
       name
       stx
       #f
       "found a use of `~a' that does not follow an open parenthesis"
       name))

    ;; Use for messages "expected ..., found <something else>"
    (define (something-else v)
      (let ([v (syntax-e v)])
	(cond
	 [(number? v) "a number"]
	 [(string? v) "a string"]
	 [else "something else"])))
    
    (define (ordinal n)
      (cond
       [(or (<= 11 n 13)
	    (zero? (modulo n 10))
	    (<= 4 (modulo n 10) 9))
	(format "~ath" n)]
       [(= 1 (modulo n 10))
	(format "~ast" n)]
       [(= 2 (modulo n 10))
	(format "~and" n)]
       [(= 3 (modulo n 10))
	(format "~ard" n)]))

    ;; At the top level, wrap `defn' to first check for
    ;;  existing definitions of the `names'. The `names'
    ;;  argument is a syntax list of identifiers.
    ;; In a module context, just check the binding
    ;;  at compile time.
    ;; In either context, if `assign?' is true, then
    ;;  generate an unevaluated assignment that makes
    ;;  the identifier mutable.
    (define (check-definitions-new who stx names defn assign)
      (cond
       [(eq? (syntax-local-context) 'top-level)
	(with-syntax ([defn defn]
		      [who who])
	  (with-syntax ([(check ...)
			 (map (lambda (name)
				(with-syntax ([name name])
                                  ;; Make sure each check has the
                                  ;; source location of the original
                                  ;; expression:
                                  (syntax/loc stx
                                    (check-top-level-not-defined 'who #'name))))
                              names)])
            (stepper-syntax-property 
	     (syntax/loc stx
	       (begin
		 check ...
		 defn)) 
             'stepper-skipto 
             (cons 'syntax-e
                   (let loop ([l names])
                     (if (null? l)
                         `(syntax-e cdr car)
                         (cons 'cdr (loop (cdr l)))))))))]
       [(memq (syntax-local-context) '(module module-begin))
	(for-each (lambda (name)
		    (let ([b (identifier-binding name)])
		      (when b
			(teach-syntax-error
			 'duplicate
			 name
			 #f
			 (if (binding-in-this-module? b)
			     "this name was defined previously and cannot be re-defined"
			     "this name has a built-in meaning and cannot be re-defined")))))
		  names)
        (if assign
            (with-syntax ([(name ...) (if (eq? assign #t)
                                          names
                                          assign)]
                          [make-up (gensym)]
                          [defn defn])
              (with-syntax ([made-up-defn (stepper-syntax-property 
                                           (syntax (define made-up (lambda () (advanced-set! name 10) ...)))
                                           'stepper-skip-completely
                                           #t)])
                (syntax/loc stx
                  (begin
                    made-up-defn ;; (define made-up (lambda () (advanced-set! name 10) ...))
                    defn))))
            defn)]
       [else defn]))
    
    ;; Same as above, but for one name
    (define (check-definition-new who stx name defn assign)
      (check-definitions-new who stx (list name) defn assign))

    ;; Check context for a `define' before even trying to
    ;; expand
    (define-struct expanding-for-intermediate-local ())
    (define (ok-definition-context)
      (let ([ctx (syntax-local-context)])
	(or (memq ctx '(top-level module module-begin))
	    (and (pair? ctx)
		 (expanding-for-intermediate-local? (car ctx))))))

    (define (local-expand-for-error stx ctx stops)
      ;; This function should only be called in an 'expression
      ;;  context. In case we mess up, avoid bogus error messages.
      (when (memq (syntax-local-context) '(expression))
	(local-expand stx ctx stops)))

    (define (ensure-expression stx k)
      (if (memq (syntax-local-context) '(expression))
	  (k)
	  (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))

    ;; Use to generate nicer error messages than direct pattern
    ;; matching. The `where' argument is an English description
    ;; of the portion of the larger expression where a single
    ;; sub-expression was expected.
    (define (check-single-expression who where stx exprs will-bind)
      (when (null? exprs)
	(teach-syntax-error
	 who
	 stx
	 #f
	 "expected an expression ~a, but nothing's there"
	 where))
      (unless (null? (cdr exprs))
	;; In case it's erroneous, to ensure left-to-right reading, let's
	;;  try expanding the first expression. We have to use
	;;  `will-bind' to avoid errors for unbound ids that will actually
	;;  be bound. Since they're used as stopping points, we may miss
	;;  some errors after all. It's worth a try, though. We also
	;;  have to stop at advanced-set!, in case it's used with
	;;  one of the identifiers in will-bind.
	(when will-bind
	  (local-expand-for-error (car exprs) 'expression (cons #'advanced-set!
								will-bind)))
	;; First expression seems ok, report an error for 2nd and later:
	(teach-syntax-error
	 who
	 stx
	 (cadr exprs)
	 "expected only one expression ~a, but found ~a extra part"
	 where
	 (if (null? (cddr exprs))
	     "one"
	     "at least one"))))

    (define (check-single-result-expr exprs where enclosing-expr will-bind)
      (check-single-expression where
			       "for the function body"
			       enclosing-expr
			       exprs
			       will-bind))

    (define keyword-list
      (append (syntax->list provided-identifiers)
	      (syntax->list (quote-syntax 
			     (#%datum
			      #%top
			      empty true false)))))

    (define (identifier/non-kw? stx)
      (and (identifier? stx)
	   (not (ormap (lambda (x) (module-identifier=? stx x))
		       keyword-list))))

    (define (something-else/kw stx)
      (if (identifier? stx)
	  "a keyword"
	  (something-else stx)))

    (define (make-name-inventer)
      ;; Normally we'd use (make-syntax-introducer) because gensyming makes
      ;;  identifiers that play badly with exporting. But we don't have
      ;;  to worry about exporting in the teaching languages, while we do
      ;;  have to worry about mangled names.
      (lambda (id)
	(datum->syntax-object id 
			      (string->uninterned-symbol (symbol->string (syntax-e id)))
			      id)))

    (define (wrap-func-definitions first-order? kinds names argcs k)
      (if first-order?
	  (let ([name2s (map (make-name-inventer) names)])
            (values (quasisyntax
                     (begin
                       #,@(map
                           (lambda (name name2 kind argc)
                             #`(define-syntax #,name 
                                 (make-first-order-function '#,kind 
                                                            #,argc
                                                            (quote-syntax #,name2) 
                                                            (quote-syntax #%app))))
                           names name2s kinds argcs)
                       #,(k name2s)))
                    name2s))
          (values (k names)
                  names)))

      
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define (beginner)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (define/proc first-order? assign? stx)

      (define (wrap-func-definition name argc k)
	(wrap-func-definitions first-order? 
			       '(procedure) (list name) (list argc)
			       (lambda (names)
				 (k (car names)))))

      (define (check-function-defn-ok stx)
	(when first-order?
	  (when (eq? 'top-level (syntax-local-context))
	    (teach-syntax-error
	     'define
	     stx
	     #f
	     "function definitions are not allowed in the interactions window; ~
              they must be in the definitions window"))))

      (unless (or (ok-definition-context)
		  (identifier? stx))
	(teach-syntax-error
	 'define
	 stx
	 #f
	 "found a definition that is not at the top level"))

      (syntax-case stx ()
	;; Constant or lambda def:
	[(_ name expr)
	 (identifier/non-kw? (syntax name))
	 (let ([lam (syntax expr)])
	   (check-defined-lambda lam)
	   (syntax-case (syntax expr) (beginner-lambda)
	     ;; Well-formed lambda def:
	     [(beginner-lambda arg-seq lexpr ...)
              (begin
                (check-function-defn-ok stx)
                (let-values ([(defn bind-names)
                              (wrap-func-definition
                               #'name
                               (length (syntax->list #'arg-seq))
                               (lambda (name)
                                 (with-syntax ([name name])
                                   (quasisyntax/loc 
                                    stx 
                                    (define name
                                      #,(stepper-syntax-property
                                         #`(lambda arg-seq 
                                             #,(stepper-syntax-property #`make-lambda-generative 
                                                                       'stepper-skip-completely #t) 
                                             lexpr ...)
                                         'stepper-define-type
                                         'lambda-define))))))])
                  (check-definition-new
                   'define
                   stx
                   #'name
                   defn
                   (and assign? bind-names))))]
	     ;; Constant def
	     [_else
              (check-definition-new
                'define
                stx
                (syntax name)
                (quasisyntax/loc stx (define name expr))
                (and assign? (list (syntax name))))]))]
	;; Function definition:
	[(_ name-seq expr ...)
	 (syntax-case (syntax name-seq) () [(name ...) #t][_else #f])
	 ;; name-seq is at least a sequence
	 (let ([names (syntax->list (syntax name-seq))])
	   (check-function-defn-ok stx)
	   (when (null? names)
	     (teach-syntax-error
	      'define
	      stx
	      names
	      "expected a function name for a definition, but the name is missing"))
	   (let loop ([names names][pos 0])
	     (unless (null? names)
	       (unless (identifier/non-kw? (car names))
		 (teach-syntax-error
		  'define
		  stx
		  (car names)
		  "expected a name for ~a, but found ~a"
		  (cond
		   [(zero? pos) "a function"]
		   [else (format "the function's ~a argument" (ordinal pos))])
		  (something-else/kw (car names))))
	       (loop (cdr names) (add1 pos))))
	   (when (null? (cdr names))
	     (teach-syntax-error
	      'define
	      stx
	      (syntax name-seq)
	      "expected at least one argument name after the function name, but found none"))
	   (let ([dup (check-duplicate-identifier (cdr names))])
	     (when dup
	       (teach-syntax-error
		'define
		stx
		dup
		"found an argument name that was used more than once: ~a"
		(syntax-e dup))))
	   (check-single-result-expr (syntax->list (syntax (expr ...)))
				     #f
				     stx
				     ;; can't local-expand function body, because
				     ;;  not all top-level defns are ready:
				     #f)
           
           (let-values ([(defn bind-names)
                         (wrap-func-definition
                          (car (syntax-e #'name-seq))
                          (length (cdr (syntax->list #'name-seq)))
                          (lambda (fn)
                            (with-syntax ([fn fn]
                                          [args (cdr (syntax-e #'name-seq))])
                              (quasisyntax/loc stx (define fn #,(stepper-syntax-property
                                                                 (stepper-syntax-property
                                                                  #`(lambda args expr ...)
                                                                  'stepper-define-type
                                                                  'shortened-proc-define)
                                                                 'stepper-proc-define-name
                                                                 #`fn))))))])
             (check-definition-new 
              'define
              stx
              (car names)
              defn
              (and assign? bind-names))))]
	;; Constant/lambda with too many or too few parts:
	[(_ name expr ...)
	 (identifier/non-kw? (syntax name))
	 (let ([exprs (syntax->list (syntax (expr ...)))])
	   (check-single-expression 'define
				    (format "after the defined name ~a"
					    (syntax-e (syntax name)))
				    stx
				    exprs
				    ;; can't local-expand RHS, because
				    ;;  not all top-level defns are ready:
				    #f))]
	;; Bad name/header:
	[(_ non-name expr ...)
	 (teach-syntax-error
	  'define
	  stx
	  (syntax non-name)
	  "expected a function name, constant name, or function header for `define', but found ~a"
	  (something-else/kw (syntax non-name)))]
	;; Missing name:
	[(_)
	 (teach-syntax-error
	  'define
	  stx
	  #f
	  "expected a function name, constant name, or function header after `define', ~
         but nothing's there")]
	[_else
	 (bad-use-error 'define stx)]))

    (define (beginner-define/proc stx)
      (define/proc #t #f stx))

    (define (intermediate-define/proc stx)
      (define/proc #f #f stx))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; lambda (beginner; only works with define)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (beginner-lambda/proc stx)
      (syntax-case stx ()
	[(_ . rest)
	 (teach-syntax-error
	  'lambda
	  stx
	  #f
	  "found a `lambda' expression that is not a function definition")]
	[_else
	 (bad-use-error 'lambda stx)]))


    (define (check-defined-lambda lam)
      (syntax-case lam (beginner-lambda)
	[(beginner-lambda arg-seq lexpr ...)
	 (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
	 (let ([args (syntax->list (syntax arg-seq))])
	   (for-each (lambda (arg)
		       (unless (identifier/non-kw? arg)
			 (teach-syntax-error
			  'lambda
			  lam
			  arg
			  "expected a name for a function argument, but found ~a"
			  (something-else/kw arg))))
		     args)
	   (when (null? args)
	     (teach-syntax-error
	      'lambda
	      lam
	      (syntax arg-seq)
	      "expected at least one argument name in the sequence after `lambda', but found none"))
	   (let ([dup (check-duplicate-identifier args)])
	     (when dup
	       (teach-syntax-error
		'lambda
		lam
		dup
		"found an argument name that was used more than once: ~a"
		(syntax-e dup))))
	   (check-single-result-expr (syntax->list (syntax (lexpr ...)))
				     #f
				     lam
				     args)
	   'ok)]
	;; Bad lambda because bad args:
	[(beginner-lambda args . _)
	 (teach-syntax-error
	  'lambda
	  lam
	  (syntax args)
	  "expected a sequence of function arguments after `lambda', but found ~a"
	  (something-else (syntax args)))]
	;; Bad lambda, no args:
	[(beginner-lambda)
	 (teach-syntax-error
	  'lambda
	  lam
	  (syntax args)
	  "expected a sequence of function arguments after `lambda', but nothing's there")]
	[_else 'ok]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define-struct (beginner)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (do-define-struct stx first-order? setters? struct-info-is-useful?)
      
      (unless (or (ok-definition-context)
		  (identifier? stx))
	(teach-syntax-error
	 'define-struct
	 stx
	 #f
	 "found a definition that is not at the top level"))
      
      (syntax-case stx ()
	;; First, check for a struct name:
	[(_ name . __)
	 (not (identifier/non-kw? (syntax name)))
	 (teach-syntax-error
	  'define-struct
	  stx
	  (syntax name)
	  "expected a structure type name after `define-struct', but found ~a"
	  (something-else/kw (syntax name)))]
	;; Main case (`rest' is for nice error messages):
	[(_ name_ (field_ ...) . rest)
	 (let ([name (syntax name_)]
	       [fields (syntax->list (syntax (field_ ...)))]
	       [ht (make-hash-table)])
	   (for-each
	    (lambda (field)
	      (unless (identifier? field)
		(teach-syntax-error
		 'define-struct
		 stx
		 field
		 "expected a structure field name, found ~a"
		 (something-else field)))
	      (let ([sym (syntax-e field)])
		(when (hash-table-get ht sym (lambda () #f))
		  (teach-syntax-error
		   'define-struct
		   stx
		   field
		   "found a field name that was used more than once: ~a"
		   sym))
		(hash-table-put! ht sym #t)))
	    fields)
	   (let ([rest (syntax->list (syntax rest))])
	     (unless (null? rest)
	       (teach-syntax-error
		'define-struct
		stx
		(car rest)
		"expected nothing after the field name sequence in `define-struct', ~
               but found ~a extra part"
		(if (null? (cdr rest))
		    "one"
		    "at least one"))))
	   (let* ([to-define-names (let ([l (build-struct-names name fields #f (not setters?) stx)])
				     (if struct-info-is-useful?
					 ;; All names:
					 l
					 ;; Skip `struct:' name:
					 (cdr l)))]
		  [proc-names (if struct-info-is-useful?
				  (cdr to-define-names)
				  to-define-names)])
	     (with-syntax ([compile-info (if struct-info-is-useful?
					     (build-struct-expand-info name fields #f (not setters?) #t null null)
					     (syntax
					      (lambda (stx)
						(raise-syntax-error
						 #f
						 "expected an expression, but found a structure name"
						 stx))))])
               (let-values ([(defn0 bind-names)
                             (wrap-func-definitions 
			      first-order? 
			      (list* 'constructor 
				     'predicate
				     (map (lambda (x) 'selector) (cddr proc-names)))
			      proc-names
			      (list* (- (length proc-names) 2)
				     1
				     (map (lambda (x) 1) (cddr proc-names)))
			      (lambda (def-proc-names)
				(with-syntax ([(def-proc-name ...) def-proc-names]
					      [(proc-name ...) proc-names])
				  (stepper-syntax-property #`(define-values (def-proc-name ...)
						       (let ()
							 (define-struct name_ (field_ ...) (make-inspector))
							 (values proc-name ...)))
						   'stepper-define-struct-hint
						   stx))))])
                 (let ([defn
                         (quasisyntax/loc stx
                           (begin
                             #,(stepper-syntax-property #`(define-syntaxes (name_) compile-info)
                                                        'stepper-skip-completely
                                                        #t)
                             #,defn0))])
                   (check-definitions-new 'define-struct
                                          stx 
                                          (cons #'name_ to-define-names)
                                          defn
                                          (and setters? bind-names)))))))]
	[(_ name_ something . rest)
	 (teach-syntax-error
	  'define-struct
	  stx
	  (syntax something)
	  "expected a sequence of field names after the structure type name in `define-struct', ~
         but found ~a"
	  (something-else (syntax something)))]
	[(_ name_)
	 (teach-syntax-error
	  'define-struct
	  stx
	  (syntax something)
	  "expected a sequence of field names after the structure type name in `define-struct', ~
         but nothing's there")]
	[(_)
	 (teach-syntax-error
	  'define-struct
	  stx
	  #f
	  "expected a structure type name after `define-struct', but nothing's there")]
	[_else (bad-use-error 'define-struct stx)]))

    (define (beginner-define-struct/proc stx)
      (do-define-struct stx #t #f #t))

    (define (intermediate-define-struct/proc stx)
      (do-define-struct stx #f #f #t))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; application (beginner and intermediate)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; For beginner:

    ;; #%app should never happen in beginner. Primitive operations and
    ;; defined functions turn into syntax that handle application
    ;; forms.  The only vaguely legitimate application would involve a
    ;; poorly implemented teachpack that exports functions instead of
    ;; primitive operators. Also, #%app is unavoidable in the REPL.

    ;; An #%app might happen "temporarily" if it appears at the top
    ;; level before the corresponding function definition. To provide
    ;; a good error message, we need to wait, and that's what
    ;; beginner-app-delay does.

    ;; For intermediate:

    ;; This application form disallows rator expressions that aren't
    ;; top-level identifiers or of the form `(check-not-undefined ...)'.

    ;; The latter is probably surprising. It turns out that every use of
    ;; a `local'-bound identifier gets converted to an undefined check,
    ;; and the call to `check-not-undefined' can't be forged by the
    ;; programmer. So the pattern-match effectively recognizes uses of
    ;; `local'-bound identifiers, which are legal as rator
    ;; expressions. (`let' and `letrec' get converted to `local'.)

    (define-values (beginner-app/proc intermediate-app/proc)
      (let ([mk-app
	     (lambda (lex-ok?)
	       (lambda (stx)
		 (syntax-case stx ()
		   [(_ rator rand ...)
		    (let* ([fun (syntax rator)]
			   [undef-check? (syntax-case fun (check-not-undefined)
					   [(check-not-undefined id)
					    #t]
					   [_else #f])]
			   [binding (and (identifier? fun)
					 (identifier-binding fun))]
			   [lex? (eq? 'lexical binding)]
			   [bad-app (lambda (what)
				      (teach-syntax-error
				       '|function call|
					 stx
					 fun
					 "expected a ~a after an open parenthesis, but found ~a"
					 (if lex-ok?
					     "name"
					     "defined name or a primitive operation name")
					 what))])
		      (unless (and (identifier? fun) (or lex-ok? undef-check? (not lex?)))
			(bad-app (if lex?
				     "a function argument name"
				     (something-else fun))))
		      ;; The following check disallows calling thunks.
		      ;; It's disabled because we need to allow calls to
		      ;; primitive thunks.
		      '(when (null? (syntax->list (syntax (rand ...))))
			 (teach-syntax-error
			  '|function call|
			    stx
			    #f
			    "expected an argument after the function name for a function call, ~
                             but nothing's there"))
		      (cond
		       [(and (not lex-ok?) (binding-in-this-module? binding))
			;; An application of something defined as a constant
			(bad-app "something else")]
		       [(or lex-ok? (and binding (not (binding-in-this-module? binding))))
			(syntax/loc stx (#%app rator rand ...))]
		       [else
			;; We don't know what rator is, yet, and it might be local:
			(quasisyntax/loc 
			 stx 
			 (#%app values #,(quasisyntax/loc
					  stx
					  (beginner-app-continue rator rand ...))))]))]
		   [(_)
		    (teach-syntax-error
		     '|function call|
		       stx
		       #f
		       (format
			"expected a ~a after an open parenthesis, but nothing's there"
			(if lex-ok?
			    "name"
			    "defined name or a primitive operation name")))]
		   [_else (bad-use-error '#%app stx)])))])
	(values (mk-app #f) (mk-app #t))))

    (define (beginner-app-continue/proc stx)
      (syntax-case stx ()
	[(_ rator rand ...)
	 (let* ([fun #'rator]
		[binding (identifier-binding fun)])
	   (if binding
	       ;; Now defined in the module:
	       (if (set!-transformer? (syntax-local-value fun (lambda () #f)))
		   ;; Something that takes care of itself:
		   (syntax/loc stx (rator rand ...))
		   ;; Something for which we probably need to report an error,
		   ;;  but let beginner-app take care of it:
		   (syntax/loc stx (beginner-app rator rand ...)))
	       ;; Something undefined; let beginner-top take care of it:
	       (syntax/loc stx (#%app rator rand ...))))]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; top-level variables (beginner)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Report errors for undefined names (but only in modules)

    (define (beginner-top/proc stx)
      (syntax-case stx ()
        [(_ . id)
	 ;; If we're in a module, we'll need to check that the name
	 ;;  is bound....
	 (if (and (not (identifier-binding #'id))
		  (syntax-source-module #'id))
	     ;; ... but it might be defined later in the module, so
	     ;; delay the check.
             (stepper-ignore-checker 
              (syntax/loc stx (#%app values (beginner-top-continue id))))
	     (syntax/loc stx (#%top . id)))]))

    (define (beginner-top-continue/proc stx)
      (syntax-case stx ()
        [(_ id)
	 ;; If there's still no binding, it's an "unknown name" error.
	 (if (not (identifier-binding #'id))
	     (teach-syntax-error
	      'unknown
	      #'id
	      #f
	      "name is not defined, not an argument, and not a primitive name")
	     ;; Don't use #%top here; id might have become bound to something
	     ;;  that isn't a value.
	     #'id)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; cond
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (beginner-cond/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_)
	    (teach-syntax-error
	     'cond
	     stx
	     #f
	     "expected a question--answer clause after `cond', but nothing's there")]
	   [(_ clause ...)
	    (let* ([clauses (syntax->list (syntax (clause ...)))]
		   [check-preceding-exprs
		    (lambda (stop-before)
		      (let/ec k
			(for-each (lambda (clause)
				    (if (eq? clause stop-before)
					(k #t)
					(syntax-case clause ()
					  [(question answer)
					   (begin
					     (unless (and (identifier? (syntax question))
							  (module-identifier=? (syntax question) #'beginner-else))
					       (local-expand-for-error (syntax question) 'expression null))
					     (local-expand-for-error (syntax answer) 'expression null))])))
				  clauses)))])
	      (let ([checked-clauses
		     (map
		      (lambda (clause)
			(syntax-case clause (beginner-else)
			  [(beginner-else answer)
			   (let ([lpos (memq clause clauses)])
			     (when (not (null? (cdr lpos)))
			       (teach-syntax-error
				'cond
				stx
				clause
				"found an `else' clause that isn't the last clause ~
                                    in its `cond' expression"))
			     (with-syntax ([new-test (stepper-syntax-property (syntax #t) 'stepper-else #t)])
			       (syntax/loc clause (new-test answer))))]
			  [(question answer)
			   (with-syntax ([verified (stepper-ignore-checker (syntax (verify-boolean question 'cond)))])
			     (syntax/loc clause (verified answer)))]
			  [()
			   (check-preceding-exprs clause)
			   (teach-syntax-error
			    'cond
			    stx
			    clause
			    "expected a question--answer clause, but found an empty clause")]
			  [(question?)
			   (check-preceding-exprs clause)
			   (teach-syntax-error
			    'cond
			    stx
			    clause
			    "expected a clause with a question and answer, but found a clause with only one part")]
			  [(question? answer? ...)
			   (check-preceding-exprs clause)
			   (let ([parts (syntax->list clause)])
			     ;; to ensure the illusion of left-to-right checking, make sure 
			     ;; the question and first answer (if any) are ok:
			     (unless (and (identifier? (car parts))
					  (module-identifier=? (car parts) #'beginner-else))
			       (local-expand-for-error (car parts) 'expression null))
			     (unless (null? (cdr parts))
			       (local-expand-for-error (cadr parts) 'expression null))
			     ;; question and answer (if any) are ok, raise a count-based exception:
			     (teach-syntax-error
			      'cond
			      stx
			      clause
			      "expected a clause with one question and one answer, but found a clause with ~a parts"
			      (length parts)))]
			  [_else
			   (teach-syntax-error
			    'cond
			    stx
			    clause
			    "expected a question--answer clause, but found ~a"
			    (something-else clause))]))
		      clauses)])
		;; Add `else' clause for error (always):
		(let ([clauses (append checked-clauses 
				       (list 
					(with-syntax ([error-call (syntax/loc stx (error 'cond "all question results were false"))])
					  (syntax [else error-call]))))])
		  (with-syntax ([clauses clauses])
		    (syntax/loc stx (cond . clauses))))))]
	   [_else (bad-use-error 'cond stx)]))))

    (define beginner-else/proc
      (make-set!-transformer
       (lambda (stx)
	 (define (bad expr)
	   (teach-syntax-error
	    'else
	    expr
	    #f
	    "not allowed here, because this is not an immediate question in a `cond' clause"))
	 (syntax-case stx (set! x)
	   [(set! e expr) (bad #'e)]
	   [(e . expr) (bad #'e)]
	   [e (bad stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; if
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (beginner-if/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ test then else)
	    (with-syntax ([new-test (stepper-ignore-checker (syntax (verify-boolean test 'if)))])
	      (syntax/loc stx
		(if new-test
		    then
		    else)))]
	   [(_ . rest)
	    (let ([n (length (syntax->list (syntax rest)))])
	      (teach-syntax-error
	       'if
	       stx
	       #f
	       "expected one question expression and two answer expressions, but found ~a expression~a"
	       (if (zero? n) "no" n)
	       (if (= n 1) "" "s")))]
	   [_else (bad-use-error 'if stx)]))))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; or, and
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define-values (beginner-or/proc beginner-and/proc)
      (let ([mk
	     (lambda (where)
               (let ([stepper-tag (case where
                                    [(or) 'comes-from-or]
                                    [(and) 'comes-from-and])])
		 (with-syntax ([swhere where])
		   (lambda (stx)
		     (ensure-expression
		      stx
		      (lambda ()
			(syntax-case stx ()
			  [(_ . clauses)
			   (let ([n (length (syntax->list (syntax clauses)))])
			     (when (n . < . 2)
			       (teach-syntax-error
				where
				stx
				#f
				"expected at least two expressions after `~a', but found ~a"
				where
				(if (zero? n) "no expressions" "only one expression")))
			     (let loop ([clauses-consumed 0]
					[remaining (syntax->list #`clauses)])
			       (if (null? remaining)
				   (case where
				     [(or) #`#f]
				     [(and) #`#t])
				   (stepper-syntax-property
				    (stepper-syntax-property
				     (quasisyntax/loc 
					 stx
				       (if #,(stepper-ignore-checker (quasisyntax/loc stx (verify-boolean #,(car remaining) 'swhere)))
					   #,@(case where
						[(or) #`(#t
							 #,(loop (+ clauses-consumed 1) (cdr remaining)))]
						[(and) #`(#,(loop (+ clauses-consumed 1) (cdr remaining))
							  #f)])))
				     'stepper-hint
				     stepper-tag)
				    'stepper-and/or-clauses-consumed
				    clauses-consumed))))]
			  [_else (bad-use-error where stx)])))))))])
	(values (mk 'or) (mk 'and))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; quote (symbols)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (beginner-quote/proc stx)
      (syntax-case stx ()
	[(_ expr)
	 (let ([sym (syntax expr)])
	   (unless (identifier? sym)
	     (teach-syntax-error
	      'quote
	      stx
	      #f
	      "expected a name after a ', found ~a"
	      (something-else sym)))
	   (syntax/loc stx (quote expr)))]
	[_else (bad-use-error 'quote stx)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; require
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (check-string-form stx s)
      (unless (regexp-match #rx#"^[-a-zA-Z0-9_. ]+(/+[-a-zA-Z0-9_. ]+)*$" (syntax-e s))
        (teach-syntax-error
         'require
         stx
         s
         (cond
          [(string=? "" (syntax-e s))
           "a module-naming string cannot be empty"]
          [(regexp-match #rx"^/" (syntax-e s))
           "a module-naming string cannot start with a slash"]
          [(regexp-match #rx"/$" (syntax-e s))
           "a module-naming string cannot end with a slash"]
          [else
           "a module-naming string can contain only a-z, A-Z, 0-9, -, _, ., space, and slash"]))))

    (define (version-number? n)
      (and (number? n) (exact? n) (integer? n) (n . >= . 0)))

    (define (beginner-require/proc stx)
      (when (identifier? stx)
        (bad-use-error 'require stx))
      (unless (memq (syntax-local-context) '(top-level module module-begin))
	(teach-syntax-error
	 'define
	 stx
	 #f
	 "found a module require that is not at the top level"))
      (syntax-case stx (lib planet)
        [(_ s)
         (string? (syntax-e #'s))
         (begin
           (check-string-form stx #'s)
           #'(require s))]
        [(_ (lib . rest))
         (let ([s (syntax->list #'rest)])
           (unless ((length s) . >= . 2)
             (teach-syntax-error
              'require
              stx
              #f
              "expected at least two strings with lib, found only ~a parts"
              (length s)))
           (for-each (lambda (v)
                       (unless (string? (syntax-e v))
                         (teach-syntax-error
                          'require
                          stx
                          v
                          "expected a string for a lib path, found ~a"
                          (something-else v)))
                       (check-string-form stx v))
                     s)
           ;; use the original `lib', so that it binds correctly:
           (syntax-case stx ()
             [(_ ms) #'(require ms)]))]
        [(_ (planet . rest))
         (syntax-case stx (planet)
           [(_ (planet s1 (s2 s3 n1 n2)))
            (and (string? (syntax-e #'s1))
                 (string? (syntax-e #'s2))
                 (string? (syntax-e #'s3))
                 (version-number? (syntax-e #'n1))
                 (version-number? (syntax-e #'n2)))
            (begin
              (check-string-form stx #'s1)
              (check-string-form stx #'s2)
              (check-string-form stx #'s3)
              ;; use the original `planet', so that it binds correctly:
              (syntax-case stx ()
                [(_ ms) #'(require ms)]))]
           [_else
            (teach-syntax-error
             'require
             stx
             #f
             "not a valid planet path; should be: (require (planet STRING (STRING STRING NUMBER NUMBER)))")])]
        [(_ thing)
         (teach-syntax-error
          'require
          stx
          #'thing
          "expected a module name as a string, a `lib' form, or a `planet' form, found ~a"
          (something-else #'thing))]
        [(_)
         (teach-syntax-error
          'require
          stx
          #f
          "expected a module name after `require', but found nothing")]
        [(_ . rest)
         (teach-syntax-error
          'require
          stx
          #f
          "expected a single module name after `require', but found ~a parts"
          (length (syntax->list #'rest)))]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; local
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (intermediate-local/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ (definition ...) . exprs)
	    (let ([defns (syntax->list (syntax (definition ...)))]
		  ;; The following context value lets teaching-language definition
		  ;;  forms know that it's ok to expand in this internal
		  ;;  definition context.
		  [int-def-ctx (build-expand-context (make-expanding-for-intermediate-local))])
	      (let* ([partly-expanded-defns 
		      (map (lambda (d)
			     (local-expand
			      d
			      int-def-ctx
			      (kernel-form-identifier-list (quote-syntax here))))
			   defns)]
		     [flattened-defns
		      (let loop ([l partly-expanded-defns][origs defns])
			(apply
			 append
			 (map (lambda (d orig)
				(syntax-case d (begin define-values define-syntaxes)
				  ;; we don't have to check for ill-formed `define-values'
				  ;; or `define-syntaxes', because only macros can generate
				  ;; them
				  [(begin defn ...)
				   (let ([l (syntax->list (syntax (defn ...)))])
				     (loop l l))]
				  [(define-values . _)
				   (list d)]
				  [(define-syntaxes . _)
				   (list d)]
				  [_else
				   (teach-syntax-error
				    'local
				    stx
				    orig
				    "expected only definitions within the definition sequence, but found ~a"
				    (something-else orig))]))
			      l origs)))]
		     [val-defns
		      (apply
		       append
		       (map (lambda (partly-expanded)
			      (syntax-case partly-expanded (define-values)
				[(define-values (id ...) expr)
				 (list partly-expanded)]
				[_else
				 null]))
			    flattened-defns))]
		     [stx-defns
		      (apply
		       append
		       (map (lambda (partly-expanded)
			      (syntax-case partly-expanded (define-syntaxes)
				[(define-syntaxes (id ...) expr)
				 (list partly-expanded)]
				[_else
				 null]))
			    flattened-defns))]
		     [get-ids (lambda (l)
				(apply
				 append
				 (map (lambda (partly-expanded)
					(syntax-case partly-expanded ()
					  [(_ (id ...) expr)
					   (syntax->list (syntax (id ...)))]))
				      l)))]
		     [val-ids (get-ids val-defns)]
		     [stx-ids (get-ids stx-defns)])
		(let ([dup (check-duplicate-identifier (append val-ids stx-ids))])
		  (when dup
		    (teach-syntax-error
		     'local
		     stx
		     dup
		     "found a name that was defined locally more than once: ~a"
		     (syntax-e dup)))
		  (let ([exprs (syntax->list (syntax exprs))])
		    (check-single-expression 'local
					     "after the local definition sequence"
					     stx
					     exprs
					     (append val-ids stx-ids)))
		  (with-syntax ([((d-v (def-id ...) def-expr) ...) val-defns]
				[(stx-def ...) stx-defns])
		    (with-syntax ([(((tmp-id def-id/prop) ...) ...)
				   ;; Generate tmp-ids that at least look like the defined
				   ;;  ids, for the purposes of error reporting, etc.:
				   (map (lambda (def-ids)
					  (map (lambda (def-id)
						 (list
						  (stepper-syntax-property
						   (datum->syntax-object
						    #f
						    (string->uninterned-symbol
						     (symbol->string (syntax-e def-id))))
						   'stepper-orig-name
						   def-id)
						  (syntax-property
						   def-id
						   'bind-as-variable
						   #t)))
					       (syntax->list def-ids)))
					(syntax->list (syntax ((def-id ...) ...))))])
		      (with-syntax ([(mapping ...)
				     (let ([mappers
					    (syntax->list
					     (syntax
					      ((define-syntaxes (def-id/prop ...)
						 (values
						  (make-undefined-check
						   (quote-syntax check-not-undefined)
						   (quote-syntax tmp-id))
						  ...))
					       ...)))])
				       (map syntax-track-origin
					    mappers
					    val-defns
					    (syntax->list (syntax (d-v ...)))))])
			(stepper-syntax-property
			 (quasisyntax/loc stx
			   (let ()
			     (define #,(gensym) 1) ; this ensures that the expansion of 'local' looks
					; roughly the same, even if the local has no defs.
			     mapping ...
			     stx-def ...
			     (define-values (tmp-id ...) def-expr)
			     ...
			     . exprs))
			 'stepper-hint
			 'comes-from-local)))))))]
	   [(_ def-non-seq . __)
	    (teach-syntax-error
	     'local
	     stx
	     (syntax def-non-seq)
	     "expected a parenthesized definition sequence after `local', but found ~a"
	     (something-else (syntax def-non-seq)))]
	   [(_)
	    (teach-syntax-error
	     'local
	     stx
	     #f
	     "expected a parenthesized definition sequence after `local', but nothing's there")]
	   [_else (bad-use-error 'local stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; letrec and let (intermediate)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; For the `let' forms, match the definitely correct patterns, and
    ;; put all error checking in `bad-let-form'.

    (define (intermediate-letrec/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ ([name rhs-expr] ...) expr)
	    (let ([names (syntax->list (syntax (name ...)))])
	      (and (andmap identifier/non-kw? names)
		   (not (check-duplicate-identifier names))))
	    (with-syntax ([(tmp-id ...)
			   ;; Generate tmp-ids that at least look like the defined
			   ;;  ids, for the purposes of error reporting, etc.:
			   (map (lambda (name)
				  (stepper-syntax-property
				   (datum->syntax-object
				    #f
				    (string->uninterned-symbol
				     (symbol->string (syntax-e name))))
				   'stepper-orig-name
				   name))
				(syntax->list #`(name ...)))]
			  [(rhs-expr ...) (map allow-local-lambda 
					       (syntax->list (syntax (rhs-expr ...))))])
	      (quasisyntax/loc stx
		(letrec-syntaxes+values ([(name) (make-undefined-check
						  (quote-syntax check-not-undefined)
						  (quote-syntax tmp-id))]
					 ...)
		    ([(tmp-id) rhs-expr] 
		     ...)
		  expr)))]
	   [_else (bad-let-form 'letrec stx stx)]))))
    
    (define (intermediate-let/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ ([name rhs-expr] ...) expr)
	    (let ([names (syntax->list (syntax (name ...)))])
	      (and (andmap identifier/non-kw? names)
		   (not (check-duplicate-identifier names))))
	    (with-syntax ([(tmp-id ...)
			   ;; Generate tmp-ids that at least look like the defined
			   ;;  ids, for the purposes of error reporting, etc.:
			   (map (lambda (name)
				  (stepper-syntax-property
				   (datum->syntax-object
				    #f
				    (string->uninterned-symbol
				     (symbol->string (syntax-e name))))
				   'stepper-orig-name
				   name))
				(syntax->list #`(name ...)))]
			  [(rhs-expr ...) (map allow-local-lambda 
					       (syntax->list (syntax (rhs-expr ...))))])
	      (quasisyntax/loc stx
                (let-values ([(tmp-id) rhs-expr] ...)
                  #,(stepper-syntax-property
                     #`(let-syntaxes ([(name) (make-undefined-check
                                               (quote-syntax check-not-undefined)
                                               (quote-syntax tmp-id))]
                                      ...)
                                     expr)
                     'stepper-skipto
                     (append
                      ;; body of let-values:
                      skipto/third
                      ;; body of let-values:
                      skipto/third)))))]
	   [_else (bad-let-form 'let stx stx)]))))

    (define (intermediate-let*/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ () expr)
	    (stepper-syntax-property
	     #`(let () expr)
	     'stepper-skipto
             skipto/third)]
	   [(_ ([name0 rhs-expr0] [name rhs-expr] ...) expr)
	    (let ([names (syntax->list (syntax (name0 name ...)))])
	      (andmap identifier/non-kw? names))
	    (with-syntax ([rhs-expr0 (allow-local-lambda (syntax rhs-expr0))])
	      (stepper-syntax-property
	       (quasisyntax/loc stx
		 (intermediate-let ([name0 rhs-expr0])
				   #,(quasisyntax/loc stx 
				       (intermediate-let* ([name rhs-expr]
							   ...)
							  expr))))
	       'stepper-hint
	       'comes-from-let*))]
	   [_else (bad-let-form 'let* stx stx)]))))

    ;; Helper function: allows `beginner-lambda' instead
    ;; of rejecting it:
    (define (allow-local-lambda stx)
      (syntax-case stx (beginner-lambda)
	[(beginner-lambda . rest)
	 (begin
	   (check-defined-lambda stx)
	   ;; pattern-match again to pull out the formals:
	   (syntax-case stx ()
	     [(_ formals . rest)
	      (quasisyntax/loc stx (lambda formals #,(stepper-syntax-property
                                                    #`make-lambda-generative
                                                    'stepper-skip-completely
                                                    #t) 
                                     . rest))]))]
	[_else stx]))

    ;; Helper function:
    (define (bad-let-form who stx orig-stx)
      (syntax-case stx ()
	[(_ (binding ...) . exprs)
	 (let ([bindings (syntax->list (syntax (binding ...)))])
	   (for-each (lambda (binding)
		       (syntax-case binding ()
			 [(name expr)
			  (let ([name (syntax name)])
			    (unless (identifier/non-kw? name)
			      (teach-syntax-error
			       who
			       orig-stx
			       name
			       "expected a name for a local binding, but found ~a"
			       (something-else/kw name))))]
			 [(name . exprs)
			  (identifier/non-kw? (syntax name))
			  (check-single-expression who
						   (format "after the name `~a'"
							   (syntax-e (syntax name)))
						   binding
						   (syntax->list (syntax exprs))
						   #f)]
			 [(something . exprs)
			  (teach-syntax-error
			   who
			   orig-stx
			   (syntax something)
			   "expected a name after the parenthesis for a ~a local definition, but found ~a"
			   who
			   (something-else/kw (syntax something)))]
			 [_else
			  (teach-syntax-error
			   who
			   orig-stx
			   binding
			   "expected a parenthesized name and expression for a ~a local definition, but found ~a"
			   who
			   (something-else binding))]))
		     bindings)
	   (unless (eq? who 'let*)
	     (let ([dup (check-duplicate-identifier (map (lambda (binding)
							   (syntax-case binding ()
							     [(name . _) (syntax name)]))
							 bindings))])
	       (when dup
		 (teach-syntax-error
		  who
		  orig-stx
		  dup
		  "found a name that was defined locally more than once: ~a"
		  (syntax-e dup)))))
	   (let ([exprs (syntax->list (syntax exprs))])
	     (check-single-expression who 
				      "after the name-defining sequence"
				      orig-stx
				      exprs
				      #f)))]
	[(_ binding-non-seq . __)
	 (teach-syntax-error
	  who
	  orig-stx
	  (syntax binding-non-seq)
	  "expected a parenthesized sequence of local name definitions after `~a', but found ~a"
	  who
	  (something-else (syntax binding-non-seq)))]
	[(_)
	 (teach-syntax-error
	  who
	  orig-stx
	  #f
	  "expected a sequence of local name definitions after `~a', but nothing's there"
	  who)]
	[_else
	 (bad-use-error who stx)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; recur (intermediate and advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; We can defer some error reporting to `bad-let-form',
    ;; but not all of it.

    (define-values (intermediate-recur/proc advanced-recur/proc)
      (let ([mk
	     (lambda (empty-ok?)
	       (lambda (stx)
		 (ensure-expression
		  stx
		  (lambda ()
		    (syntax-case stx ()
		      [(_ fname ([name rhs-expr] ...) expr)
		       (and (identifier/non-kw? (syntax fname))
			    (let ([names (syntax->list (syntax (name ...)))])
			      (and (andmap identifier/non-kw? names)
				   (or empty-ok? (pair? names))
				   (not (check-duplicate-identifier names)))))
		       (stepper-syntax-property
			(quasisyntax/loc stx
			  ((intermediate-letrec ([fname
						  #,(stepper-syntax-property
						     (stepper-syntax-property 
						      #`(lambda (name ...)
							  expr)
						      'stepper-define-type
						      'shortened-proc-define)
						     'stepper-proc-define-name
						     #`fname)])
						fname)
			   rhs-expr ...))
			'stepper-hint
			'comes-from-recur)]
		      [(_form fname empty-seq . rest)
		       (and (not empty-ok?)
			    (identifier/non-kw? (syntax fname))
			    (null? (syntax-e (syntax empty-seq))))
		       (teach-syntax-error
			'recur
			stx
			(syntax empty-seq)
			"expected a non-empty sequence of bindings after the function name, ~
                    but found an empty sequence")]
		      [(_form fname . rest)
		       (identifier/non-kw? (syntax fname))
		       (bad-let-form 'recur (syntax (_form . rest)) stx)]
		      [(_form fname . rest)
		       (teach-syntax-error
			'recur
			stx
			#f
			"expected a function name after `recur', but found ~a"
			(something-else/kw (syntax fname)))]
		      [(_form)
		       (teach-syntax-error
			'recur
			stx
			#f
			"expected a function name after `recur', but nothing's there")]
		      [_else
		       (bad-use-error 'recur stx)])))))])
	(values (mk #f) (mk #t))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; lambda (intermediate)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (intermediate-lambda/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ arg-seq lexpr ...)
	    (syntax-case (syntax arg-seq) () [(arg ...) #t][_else #f])
	    (let ([args (syntax->list (syntax arg-seq))])
	      (for-each (lambda (arg)
			  (unless (identifier/non-kw? arg)
			    (teach-syntax-error
			     'lambda
			     stx
			     arg
			     "expected a name for a function argument, but found ~a"
			     (something-else/kw arg))))
			args)
	      (when (null? args)
		(teach-syntax-error
		 'lambda
		 stx
		 (syntax arg-seq)
		 "expected at least one argument name in the sequence after `lambda', but found none"))
	      (let ([dup (check-duplicate-identifier args)])
		(when dup
		  (teach-syntax-error
		   'lambda
		   stx
		   dup
		   "found an argument name that is used more than once: ~a"
		   (syntax-e dup))))
	      (check-single-expression 'lambda
				       "within lambda"
				       stx
				       (syntax->list (syntax (lexpr ...)))
				       args)
	      (syntax/loc stx (lambda arg-seq lexpr ...)))]
	   ;; Bad lambda because bad args:
	   [(_ args . __)
	    (teach-syntax-error
	     'lambda
	     stx
	     (syntax args)
	     "expected a sequence of function arguments after `lambda', but found ~a"
	     (something-else (syntax args)))]
	   [(_)
	    (teach-syntax-error
	     'lambda
	     stx
	     #f
	     "expected a sequence of argument names after `lambda', but nothing's there")]
	   [_else
	    (bad-use-error 'lambda stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; quote (intermediate)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define intermediate-quote/proc
      (lambda (stx)
	(syntax-case stx ()
	  [(_ expr ...)
	   (begin
	     (check-single-expression 'quote
				      "after the `quote' keyword"
				      stx
				      (syntax->list (syntax (expr ...)))
				      ;; Don't expand expr!
				      #f)
	     (syntax (quote expr ...)))]
	  [_else (bad-use-error 'quote stx)])))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; quasiquote (intermediate)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; This quasiquote uses the right cons, and perhaps provides more
    ;; suitable error messages. The "right" cons is actually advanced-cons,
    ;; because it works with shared:

    (define (intermediate-quasiquote/proc stx)
      (let loop ([stx (syntax-case stx ()
			[(_ stx) (syntax stx)]
			[(_ . any)
			 (teach-syntax-error
			  'quasiquote
			  stx
			  #f
			  "misuse of `quasiquote'")]
			[_else (bad-use-error 'quasiquote stx)])]
		 [depth 0])
	(syntax-case stx (intermediate-unquote intermediate-unquote-splicing intermediate-quasiquote)
	  [(intermediate-unquote x)
	   (if (zero? depth)
	       (syntax x)
	       (with-syntax ([x (loop (syntax x) (sub1 depth))]
			     [uq (stx-car stx)])
		 (syntax/loc stx (list (quote uq) x))))]
	  [intermediate-unquote
	   (teach-syntax-error
	    'quasiquote
	    stx
	    #f
	    "misuse of `unquote' within a quasiquoting backquote")]
	  [((intermediate-unquote-splicing x) . rest)
	   (if (zero? depth)
	       (with-syntax ([rest (loop (syntax rest) depth)])
		 (syntax (append x rest)))
	       (with-syntax ([x (loop (syntax x) (sub1 depth))]
			     [rest (loop (syntax rest) depth)]
			     [uq-splicing (stx-car (stx-car stx))])
		 (stepper-syntax-property (syntax/loc stx (the-cons (list (quote uq-splicing) x) rest))
                                  'stepper-hint
                                  'quasiquote-the-cons-application)))]
	  [intermediate-unquote-splicing
	   (teach-syntax-error
	    'quasiquote
	    stx
	    #f
	    "misuse of ,@ or `unquote-splicing' within a quasiquoting backquote")]
	  [(intermediate-quasiquote x)
	   (with-syntax ([x (loop (syntax x) (add1 depth))]
			 [qq (stx-car stx)])
	     (syntax/loc stx (list (quote qq) x)))]
	  [(a . b)
	   (with-syntax ([a (loop (syntax a) depth)]
			 [b (loop (syntax b) depth)])
	     (stepper-syntax-property (syntax/loc stx (the-cons a b))
                              'stepper-hint
                              'quasiquote-the-cons-application))]
	  [any
	   (syntax/loc stx (quote any))])))

    (define (intermediate-unquote/proc stx)
      (teach-syntax-error
       'unquote
       stx
       #f
       "misuse of a comma or `unquote', not under a quasiquoting backquote"))
    
    (define (intermediate-unquote-splicing/proc stx)
      (teach-syntax-error
       'unquote-splicing
       stx
       #f
       "misuse of ,@ or `unquote-splicing', not under a quasiquoting backquote"))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; time
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (intermediate-time/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ . exprs)
	    (check-single-expression 'time 
				     "after `time'"
				     stx
				     (syntax->list (syntax exprs))
				     null)
	    (stepper-syntax-property 
	     (syntax/loc stx (time . exprs))
	     'stepper-skipto
             (append 
              ;; let-values-bindings
              skipto/second
              ;; rhs of first binding
              skipto/first
              skipto/second
              ;; 2nd term of application:
              skipto/cdr
              skipto/second
              ;; lambda-body:
              skipto/cddr
              skipto/first))]
	   [_else
	    (bad-use-error 'time stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-define/proc stx)
      ;; Handle the case that doesn't fit into intermediate, then dispatch to 
      ;; the common code that it also used by beginner/intermediate.
      (syntax-case stx ()
	[(_ (name) expr)
	 (and (identifier/non-kw? (syntax name))
	      (ok-definition-context))
	 (check-definition-new
	  'define
	  stx
	  (syntax name)
	  (syntax/loc stx (define (name) expr))
          (list #'name))]
	[(_ (name) expr ...)
	 (check-single-result-expr (syntax->list (syntax (expr ...)))
				   #f
				   stx
				   (list #'name))]
	[(_ . rest)
	 ;; Call transformer define/proc.
	 ;; Note that we call the transformer instead of producing
	 ;; new syntax object that is an `intermediate-define' form;
	 ;; that's important for syntax errors, so that they
	 ;; report `advanced-define' as the source.
	 (define/proc #f #t stx)]
	[_else
	 (bad-use-error 'define stx)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; lambda (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-lambda/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_  (name ...) . exprs)
	    (let ([names (syntax->list (syntax (name ...)))])
	      (for-each (lambda (name)
			  (unless (identifier/non-kw? name)
			    (teach-syntax-error
			     'lambda
			     stx
			     name
			     "expected a name for an argument, but found ~a"
			     (something-else/kw name))))
			names)
	      (let ([dup (check-duplicate-identifier names)])
		(when dup
		  (teach-syntax-error
		   'lambda
		   stx
		   dup
		   "found an argument name that is used more than once: ~a"
		   (syntax-e dup))))
	      (check-single-expression 'lambda 
				       "after the argument-name sequence"
				       stx
				       (syntax->list (syntax exprs))
				       names)
	      (syntax/loc stx (lambda (name ...) . exprs)))]
	   [(_ arg-non-seq . exprs)
	    (teach-syntax-error
	     'lambda
	     stx
	     (syntax arg-non-seq)
	     "expected a parenthesized sequence of argument names after `lambda', but found ~a"
	     (something-else (syntax arg-non-seq)))]
	   [(_)
	    (teach-syntax-error
	     'lambda
	     stx
	     #f
	     "expected a sequence of argument names after `lambda', but nothing's there")]
	   [_else
	    (bad-use-error 'lambda stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; application (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-app/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ rator rand ...)
	    (syntax/loc stx (#%app rator rand ...))]
	   [(_)
	    (teach-syntax-error
	     '|function call|
	       stx
	       #f
	       "expected a defined name or a primitive operation name after an ~
                open parenthesis, but nothing's there")]
	   [_else (bad-use-error '#%app stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; set! (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; We disallow set!s on lambda-bound variables, which we recognize
    ;; as lexically-bound variables that are not bound to
    ;; set!-transformer syntax values.

    (define-values (advanced-set!/proc advanced-set!-continue/proc)
      (let ([proc
	     (lambda (continuing?)
	       (lambda (stx)
		 (ensure-expression
		  stx
		  (lambda ()
		    (syntax-case stx ()
		      [(_ id expr ...)
		       (identifier? (syntax id))
		       (let ([exprs (syntax->list (syntax (expr ...)))])
			 ;; Check that id isn't syntax, and not lexical.
			 (when ((with-handlers ([exn:fail? (lambda (exn) (lambda () #t))])
				  ;; First try syntax:
				  (let ([binding (syntax-local-value (syntax id))])
				    ;; If it's a transformer binding, then it can take care of itself...
				    (if (set!-transformer? binding)
					(lambda () #f) ;; no lex check wanted
					(lambda ()
					  (teach-syntax-error
					   'set!
					   stx
					   (syntax id)
					   "expected a defined name after `set!', but found a keyword"))))))
			   ;; Now try lexical:
			   (when (eq? 'lexical (identifier-binding (syntax id)))
			     (teach-syntax-error
			      'set!
			      stx
			      (syntax id)
			      "expected a defined name after `set!', but found a function argument name")))
			 ;; If we're in a module, we'd like to check here whether
			 ;;  the identier is bound, but we need to delay that check
			 ;;  in case the id is defined later in the module. So only
			 ;;  do this in continuing mode:
			 (when continuing?
			   (let ([binding (identifier-binding #'id)])
			     (cond
			      [(and (not binding)
				    (syntax-source-module #'id))
			       (teach-syntax-error
				'unknown
				#'id
				#f
				"name is not defined")]
			      [(and (list? binding)
				    (or (not (module-path-index? (car binding)))
					(let-values ([(path rel) (module-path-index-split (car binding))])
					  path)))
			       (teach-syntax-error
				'unknown
				#'id
				#f
				"cannot set a primitive name")])))
			 ;; Check the RHS
			 (check-single-expression 'set!
						  "for the new value"
						  stx
						  exprs
						  null)
			 (if continuing?
			     (stepper-syntax-property
			      (syntax/loc stx (begin (set! id expr ...) set!-result))
			      'stepper-skipto
                              (append skipto/cdr
                                      skipto/first))
			     (stepper-ignore-checker (syntax/loc stx (#%app values (advanced-set!-continue id expr ...))))))]
		      [(_ id . __)
		       (teach-syntax-error
			'set!
			stx
			(syntax id)
			"expected a defined name after `set!', but found ~a"
			(something-else (syntax id)))]
		      [(_)
		       (teach-syntax-error
			'set!
			stx
			(syntax id)
			"expected a defined name after `set!', but nothing's there")]
		      [_else (bad-use-error 'set! stx)])))))])
	(values (proc #f)
		(proc #t))))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; when and unless (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define-values (advanced-when/proc advanced-unless/proc)
      (let ([mk
	     (lambda (who target-stx)
	       (lambda (stx)
		 (ensure-expression
		  stx
		  (lambda ()
		    (syntax-case stx ()
		      [(_ q expr ...)
		       (let ([exprs (syntax->list (syntax (expr ...)))])
			 (check-single-expression who
						  (format "for the answer in `~a'"
							  who)
						  stx
						  exprs
						  null)
			 (with-syntax ([who who]
				       [target target-stx])
			   (syntax/loc stx (target (verify-boolean q 'who) expr ...))))]
		      [(_)
		       (teach-syntax-error
			who
			stx
			#f
			"expected a question expression after `~a', but nothing's there"
			who)]
		      [_else
		       (bad-use-error who stx)])))))])
	(values (mk 'when (quote-syntax when))
		(mk 'unless (quote-syntax unless)))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; define-struct (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-define-struct/proc stx)
      (do-define-struct stx #f #t #t))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; let (advanced)       >> mz errors in named case <<
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-let/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_ name ids body)
	    (identifier/non-kw? (syntax name))
	    (syntax/loc stx (let name ids body))]
	   [(_ name . rest)
	    (identifier/non-kw? (syntax name))
	    (teach-syntax-error
	     'let
	     stx
	     #f
	     "bad syntax for named `let'")]
	   [(_ . rest)
	    (syntax/loc stx (intermediate-let . rest))]
	   [_else
	    (bad-use-error 'let stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; begin (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Mask out the top-level begin
    (define (advanced-begin/proc stx)
      (syntax-case stx ()
	[(_)
	 (teach-syntax-error
	  'begin
	  stx
	  #f
	  "expected a sequence of expressions after `begin', but nothing's there")]
	[(_ e ...)
	 (stepper-syntax-property (syntax/loc stx (let () e ...))
                          'stepper-hint
                          'comes-from-begin)]
	[_else
	 (bad-use-error 'begin stx)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; begin0 (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (advanced-begin0/proc stx)
      (syntax-case stx ()
	[(_)
	 (teach-syntax-error
	  'begin
	  stx
	  #f
	  "expected a sequence of expressions after `begin0', but nothing's there")]
	[(_ e ...)
	 (syntax/loc stx (begin0 e ...))]
	[_else
	 (bad-use-error 'begin0 stx)]))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; case
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (advanced-case/proc stx)
      (ensure-expression
       stx
       (lambda ()
	 (syntax-case stx ()
	   [(_)
	    (teach-syntax-error
	     'case
	     stx
	     #f
	     "expected an expression after `case', but nothing's there")]
	   [(_ expr)
	    (teach-syntax-error
	     'case
	     stx
	     #f
	     "expected a choices--answer clause after the expression following `case', but nothing's there")]
	   [(_ v-expr clause ...)
	    (let ([clauses (syntax->list (syntax (clause ...)))])
	      (for-each
	       (lambda (clause)
		 (syntax-case clause (beginner-else)
		   [(beginner-else answer ...)
		    (let ([lpos (memq clause clauses)])
		      (when (not (null? (cdr lpos)))
			(teach-syntax-error
			 'case
			 stx
			 clause
			 "found an `else' clause that isn't the last clause ~
                                    in its `case' expression"))
		      (let ([answers (syntax->list (syntax (answer ...)))])
			(check-single-expression 'case
						 "for the answer in a case clause"
						 clause
						 answers
						 null)))]
		   [(choices answer ...)
		    (let ([choices (syntax choices)]
			  [answers (syntax->list (syntax (answer ...)))])
		      (syntax-case choices ()
			[(elem ...)
			 (let ([elems (syntax->list (syntax (elem ...)))])
			   (for-each (lambda (e)
				       (let ([v (syntax-e e)])
					 (unless (or (number? v)
						     (symbol? v))
					   (teach-syntax-error
					    'case
					    stx
					    e
					    "expected a name (for a symbol) or a number as a choice value, but found ~a"
					    (something-else e)))))
				     elems))]
			[_else (teach-syntax-error
				'case
				stx
				choices
				"expected a parenthesized sequence of choice values, but found ~a"
				(something-else choices))])
		      (when (stx-null? choices)
			(teach-syntax-error
			 'case
			 stx
			 choices
			 "expected at least once choice in a parenthesized sequence of choice values, but nothing's there"))
		      (check-single-expression 'case
					       "for the answer in a `case' clause"
					       clause
					       answers
					       null))]
		   [()
		    (teach-syntax-error
		     'case
		     stx
		     clause
		     "expected a choices--answer clause, but found an empty clause")]
		   [_else
		    (teach-syntax-error
		     'case
		     stx
		     clause
		     "expected a choices--answer clause, but found ~a"
		     (something-else clause))]))
	       clauses)
	      ;; Add `else' clause for error, if necessary:
	      (let ([clauses (let loop ([clauses clauses])
			       (cond
				[(null? clauses)
				 (list
				  (syntax/loc stx
				    [else (error 'cases "the expression matched none of the choices")]))]
				[(syntax-case (car clauses) (beginner-else)
				   [(beginner-else . _) (syntax/loc (car clauses) (else . _))]
				   [_else #f])
				 => 
				 (lambda (x) (cons x (cdr clauses)))]
				[else (cons (car clauses) (loop (cdr clauses)))]))])
		(with-syntax ([clauses clauses])
		  (syntax/loc stx (case v-expr . clauses)))))]
	   [_else (bad-use-error 'case stx)]))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; delay (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define advanced-delay/proc
      (lambda (stx)
	(ensure-expression
	 stx
	 (lambda ()
	   (syntax-case stx ()
	     [(_ expr ...)
	      (begin
		(check-single-expression 'delay
					 "after the `delay' keyword"
					 stx
					 (syntax->list (syntax (expr ...)))
					 null)
		(syntax (delay expr ...)))]
	     [_else (bad-use-error 'delay stx)])))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; shared (advanced)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; We do all the initial syntax checks, and otherwise
    ;; let "shared-body.ss" implement the form.

    (define advanced-shared/proc
      (lambda (stx)
	(ensure-expression
	 stx
	 (lambda ()
	   ;; Helper for the main implementation
	   (define (make-check-cdr name)
	     (with-syntax ([name name])
	       (syntax (unless (cyclic-list? (cdr name))
			 (raise-type-error
			  'cons
			  "list or cyclic list"
			  1
			  (car name)
			  (cdr name))))))

	   ;; Check the syntax before letting the main implementation go:
	   (syntax-case stx ()
	     [(_ (binding ...) . exprs)
	      (let ([bindings (syntax->list (syntax (binding ...)))])
		(for-each
		 (lambda (binding)
		   (syntax-case binding ()
		     [(id . exprs)
		      (identifier/non-kw? (syntax id))
		      (check-single-expression 'shared
					       "after the binding name"
					       binding
					       (syntax->list (syntax exprs))
					       #f)]
		     [(a . rest)
		      (not (identifier/non-kw? (syntax a)))
		      (teach-syntax-error
		       'shared
		       stx
		       (syntax a)
		       "expected a name for the binding, but found ~a"
		       (something-else/kw (syntax a)))]
		     [()
		      (teach-syntax-error
		       'shared
		       stx
		       (syntax a)
		       "expected a name for a binding, but nothing's there")]
		     [_else
		      (teach-syntax-error
		       'shared
		       stx
		       binding
		       "expected a name--expression pair for a binding, but found ~a"
		       (something-else binding))]))
		 bindings)
		(check-single-expression 'shared
					 "after the bindings"
					 stx
					 (syntax->list (syntax exprs))
					 #f))]
	     [(_ bad-bind . exprs)
	      (teach-syntax-error
	       'shared
	       stx
	       (syntax bad-bind)
	       "expected a sequence of bindings after `shared', but found ~a"
	       (something-else (syntax bad-bind)))]
	     [(_)
	      (teach-syntax-error
	       'shared
	       stx
	       (syntax bad-bind)
	       "expected a sequence of bindings after `shared', but nothing's there")]
	     [_else (bad-use-error 'shared stx)])

	   ;; The main implementation
	   (include (build-path up up "mzlib" "private" "shared-body.ss"))))))))
