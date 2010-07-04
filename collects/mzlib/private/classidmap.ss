
(module classidmap mzscheme
  (require (lib "stx.ss" "syntax"))

  (define-values (struct:s!t make-s!t s!t? s!t-ref s!t-set!)
    (make-struct-type 'set!-transformer #f 2 0 #f null (current-inspector) 0))
  
  (define (mk-set!-trans old-id proc)
    (make-set!-transformer (make-s!t proc old-id)))

  (define (make-method-apply id this orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args)
	(list* id this orig-args)]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(list* 'apply id this (reverse (cons args accum)))])))

  (define (find the-finder name src)
    (let ([this-id (syntax-local-value (syntax-local-get-shadower the-finder))])
      (datum->syntax-object this-id name src)))

  ;; Check Syntax binding info:
  (define (binding from to stx)
    stx
    ;; This 'bound-in-source is no longer needed
    #;
    (syntax-property
     stx
     'bound-in-source
     (cons from (syntax-local-introduce to))))


  (define (make-this-map orig-id the-finder the-obj)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (mk-set!-trans
       orig-id
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate object identifier" stx)]
	   [(id . args)
	    (datum->syntax-object 
	     stx
	     (cons (find the-finder the-obj stx) (syntax args))
	     stx)]
	   [id (find the-finder the-obj stx)])))))

  (define (make-field-map the-finder the-obj the-binder the-binder-localized field-accessor field-mutator field-pos/null)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (mk-set!-trans
       the-binder-localized
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (list* field-mutator (find the-finder the-obj stx) (append field-pos/null (list (syntax expr))))
	      stx))]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (cons (list* field-accessor (find the-finder the-obj stx) field-pos/null) (syntax args))
	      stx))]
	   [_else
	    (binding
	     the-binder stx
	     (datum->syntax-object 
	      the-finder
	      (list* field-accessor (find the-finder the-obj stx) field-pos/null)
	      stx))])))))

  (define (make-method-map the-finder the-obj the-binder the-binder-localized method-accessor)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (mk-set!-trans
       the-binder-localized
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply
	       (list method-accessor (find the-finder the-obj stx))
	       (find the-finder the-obj stx)
	       (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  ;; For methods that are dirrectly available via their names
  ;;  (e.g., private methods)
  (define (make-direct-method-map the-finder the-obj the-binder the-binder-localized new-name)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (mk-set!-trans
       the-binder-localized
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply (find the-finder new-name stx) (find the-finder the-obj stx) (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of method (not in application)" 
	     stx)])))))

  (define (make-rename-super-map the-finder the-obj the-binder the-binder-localized rename-temp)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)])
      (mk-set!-trans
       the-binder-localized
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate super method" stx)]
	   [(id . args)
	    (binding
	     the-binder (syntax id)
	     (datum->syntax-object 
	      the-finder
	      (make-method-apply (find the-finder rename-temp stx) (find the-finder the-obj stx) (syntax args))
	      stx))]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of super method (not in application)" 
	     stx)])))))

  (define (make-rename-inner-map the-finder the-obj the-binder the-binder-localized rename-temp)
    (let ([set!-stx (datum->syntax-object the-finder 'set!)]
	  [lambda-stx (datum->syntax-object the-finder 'lambda)])
      (mk-set!-trans
       the-binder-localized
       (lambda (stx)
	 (syntax-case stx ()
	   [(set! id expr)
	    (module-identifier=? (syntax set!) set!-stx)
	    (raise-syntax-error 'class "cannot mutate inner method" stx)]
	   [(id (lambda () default) . args)
	    (module-identifier=? (syntax lambda) lambda-stx)
	    (let ([target (find the-finder the-obj stx)])
	      (binding
	       the-binder (syntax id)
	       (datum->syntax-object 
		the-finder
		(make-method-apply (list (find the-finder rename-temp stx) target #'default)
				   target (syntax args))
		stx)))]
	   [(id (lambda largs default) . args)
	    (module-identifier=? (syntax lambda) lambda-stx)
	    (raise-syntax-error 
	     'class 
	     "misuse of inner method (lambda for default does not take zero arguments)" 
	     stx)]
	   [(id (lambda . rest) . args)
	    (module-identifier=? (syntax lambda) lambda-stx)
	    (raise-syntax-error 
	     'class 
	     "misuse of inner method (ill-formed lambda for default)" 
	     stx)]
	   [(id . args)
	    (raise-syntax-error 
	     'class 
	     "misuse of inner method (no lambda-wrapped default after name)" 
	     stx)]
	   [_else
	    (raise-syntax-error 
	     'class 
	     "misuse of inner method (not in application)" 
	     stx)])))))

  (define (generate-super-call stx the-finder the-obj rename-temp args)
    (datum->syntax-object 
     the-finder
     (make-method-apply (find the-finder rename-temp stx) 
			(find the-finder the-obj stx) 
			args)
     stx))

  (define (generate-inner-call stx the-finder the-obj default-expr rename-temp args)
    (datum->syntax-object 
     the-finder
     (let ([target (find the-finder the-obj stx)])
       (datum->syntax-object 
	the-finder
	`(let ([i (,(find the-finder rename-temp stx) ,target)])
	   (if i
	       ,(make-method-apply 'i target args)
	       ,default-expr))
	stx))
     stx))

  (define init-error-map
    (make-set!-transformer
     (lambda (stx)
       (raise-syntax-error 
	'class
	"cannot use non-field init variable in a method"
	stx))))

  (define super-error-map
    (lambda (stx)
      (raise-syntax-error 
       'class
       "cannot use superclass initialization form in a method"
       stx)))

  (define (make-with-method-map set!-stx id-stx method-stx method-obj-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx ()
	 [(set! id expr)
	  (module-identifier=? (syntax set!) set!-stx)
	  (raise-syntax-error 'with-method "cannot mutate method" stx)]
	 [(id . args)
	  (datum->syntax-object 
	   set!-stx
	   (make-method-apply
	    method-stx
	    method-obj-stx
	    (syntax args))
	   stx)]
	 [_else
	  (raise-syntax-error 
	   'with-method 
	   "misuse of method (not in application)" 
	   stx)]))))

  (define (flatten-args orig-args)
    (let loop ([args orig-args][accum null])
      (cond
       [(stx-null? args) orig-args]
       [(stx-pair? args)
	(loop (stx-cdr args) (cons (stx-car args) accum))]
       [else
	(reverse (cons args accum))])))

  (define-struct private-name (orig-id gen-id))

  (define (localize orig-id)
    (let loop ([id orig-id])
      (let ([v (syntax-local-value id (lambda () #f))])
	(cond
	 [(and v (private-name? v))
	  (list 'unquote 
		(binding (private-name-orig-id v)
			 id
			 (private-name-gen-id v)))]
	 [(and (set!-transformer? v)
	       (s!t? (set!-transformer-procedure v)))
	  (s!t-ref (set!-transformer-procedure v) 1)]
	 [else orig-id]))))

  (define-struct class-context ())

  (define (generate-class-expand-context)
    (let ([c (syntax-local-context)]
	  [v (make-class-context)])
      (if (pair? c)
	  (cons-immutable v c)
	  (list-immutable v))))

  (define (class-top-level-context? ctx)
    (and (pair? ctx)
	 (class-context? (car ctx))))

  (provide (protect make-this-map make-field-map make-method-map 
		    make-direct-method-map 
		    make-rename-super-map make-rename-inner-map
		    init-error-map super-error-map 
		    make-with-method-map
		    flatten-args
		    make-private-name localize
		    generate-super-call generate-inner-call
		    generate-class-expand-context class-top-level-context?)))
