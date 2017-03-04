(define any (signature any %any))

(define-syntax define-record-procedures*
  
  (let ()
    (define (filter-map proc l)
      (if (null? l)
	  '()
	  (let ((result (proc (car l))))
	    (if result
		(cons result (filter-map proc (cdr l)))
		(filter-map proc (cdr l))))))
    
    
    (define (syntax-member? thing stuff)
      (cond
       ((null? stuff) #f)
       ((free-identifier=? thing (car stuff)) #t)
       (else (syntax-member? thing (cdr stuff)))))

    (define (map-with-index proc list)
      (let loop ((i 0) (list list) (rev-result '()))
	(if (null? list)
	    (reverse rev-result)
	    (loop (+ 1 i)
		  (cdr list)
		  (cons (proc i (car list)) rev-result)))))

    (lambda (x)
      (syntax-case x ()
	((_ ?stx
	    ?type-name
	    ?mutable?
	    ?signature-constructor-name
	    ?constructor
	    ?predicate
	    (?field-spec ...))

	 (with-syntax
	     ((number-of-fields (length (syntax->list (syntax (?field-spec ...)))))
	      ((accessor ...)
	       (map (lambda (field-spec)
		      (syntax-case field-spec ()
			((accessor mutator signature) #'accessor)))
		    (syntax->list (syntax (?field-spec ...)))))
	      ((mutator ...)
	       (map (lambda (field-spec)
		      (syntax-case field-spec ()
			((accessor mutator signature) #'mutator)))
		    (syntax->list (syntax (?field-spec ...))))))
	   (let ((maybe-field-signatures
		  (map (lambda (field-spec)
			 (syntax-case field-spec ()
			   ((accessor mutator #f) #f)
			   ((accessor mutator sig) #'sig)))
		       (syntax->list (syntax (?field-spec ...))))))
	     (with-syntax
		 (((field-signature ...)
		   (map (lambda (sig) (or sig #'any)) maybe-field-signatures))
		  ((accessor-proc ...)
		   (map-with-index
		    (lambda (i accessor)
		      (with-syntax ((i i)
				    (tag accessor))
			(syntax-property (syntax/loc
					  accessor
					  (lambda (s)
					    (when (not (raw-predicate s))
					      (raise
					       (make-exn:fail:contract
						(string->immutable-string
						 (format "~a: Argument kein ~a: ~e" 
							 'tag '?type-name s))
						(current-continuation-marks))))
					    (raw-generic-access s i)))
					 'inferred-name
					 (syntax-e accessor))))
		    (syntax->list #'(accessor ...))))
		  ((our-accessor ...) (generate-temporaries #'(accessor ...)))
		  (real-constructor
		   ;; use a different name for the value binding, but
		   ;; make sure the stepper prints the one from the d-r-p form
		   (let ((name #`?constructor))
		     (stepper-syntax-property
		      (datum->syntax
		       #f
		       (string->uninterned-symbol
			(symbol->string (syntax-e name))))
		      'stepper-orig-name
		      name)))
		  ((mutator-proc ...)
		   (map-with-index
		    (lambda (i mutator)
		      (with-syntax ((i i)
				    (tag mutator))
			(syntax-property (syntax/loc
					  mutator
					  (lambda (s v)
					    (when (not (raw-predicate s))
					      (raise
					       (make-exn:fail:contract
						(string->immutable-string
						 (format "~a: Argument kein ~a: ~e" 
							 'tag '?type-name s))
						(current-continuation-marks))))
					    (raw-generic-mutate s i v)))
					 'inferred-name
					 (syntax-e mutator))))
		    (syntax->list #'(mutator ...))))
		  (constructor-proc
		   (syntax-property #'(lambda (accessor ...)
					(raw-constructor accessor ... #f))
				    'inferred-name
				    (syntax-e #'?constructor)))
		  (predicate-proc
		   (syntax-property #'(lambda (thing)
					(raw-predicate thing))
				    'inferred-name
				    (syntax-e #'?predicate)))
		  ((raw-accessor-proc ...)
		   (map-with-index (lambda (i _)
				     #`(lambda (r)
					 (raw-generic-access r #,i)))
				   (syntax->list #'(?field-spec ...))))
		  ((raw-mutator-proc ...)
		   (map-with-index (lambda (i _)
				     #`(lambda (r val)
					 (raw-generic-mutate r #,i val)))
				   (syntax->list #'(?field-spec ...))))

		  (record-equal? #`(lambda (r1 r2 equal?)
				     (and #,@(map-with-index (lambda (i field-spec)
							       #`(equal? (raw-generic-access r1 #,i)
									 (raw-generic-access r2 #,i)))
							     (syntax->list #'(?field-spec ...)))))))

				 
	       (with-syntax
		   ((struct-type-defs
		     #'(define-values (type-descriptor
				       raw-constructor
				       raw-predicate
				       raw-generic-access
				       raw-generic-mutate)
			 (make-struct-type
			  '?type-name #f (+ 1 number-of-fields) 0
			  #f
			  (list
			   (cons prop:print-convert-constructor-name
				 '?constructor)
			   (cons prop:custom-write
				 (make-constructor-style-printer
				  (lambda (obj)
				    (string->symbol (string-append "record:" (symbol->string '?type-name))))
				  (lambda (obj)
				    (access-record-fields obj raw-generic-access number-of-fields))))
			   (cons prop:print-converter
				 (lambda (r recur)
				   (list '?constructor
					 (recur (raw-accessor-proc r)) ...)))
			   (cons prop:equal+hash
				 (list record-equal?
				       (make-equal-hash (lambda (r i) (raw-generic-access r i)) number-of-fields) 
				       (make-equal2-hash (lambda (r i) (raw-generic-access r i)) number-of-fields)))
			   (cons prop:lazy-wrap
				 (make-lazy-wrap-info constructor-proc
						      (list raw-accessor-proc ...)
						      (list raw-mutator-proc ...)
						      (lambda (r)
							(raw-generic-access r number-of-fields))
						      (lambda (r val)
							(raw-generic-mutate r number-of-fields val)))))
			  (make-inspector))))
		    (real-constructor-def #'(define/signature real-constructor
					      (signature (field-signature ... -> ?type-name))
					      constructor-proc))
		    (constructor-def #'(define-syntax ?constructor
					 (let ()
					   (define-struct info ()
					     #:super struct:struct-info
					     ;; support `signature'
					     #:property 
					     prop:procedure
					     (lambda (_ stx)
					       (syntax-case stx ()
						 [(self . args) (syntax/loc stx (real-constructor . args))]
						 [else (syntax/loc stx real-constructor)])))
					   (make-info (lambda ()
							(list #f
							      #'real-constructor
							      #'real-predicate
							      (reverse (syntax->list #'(our-accessor ...)))
							      (map (lambda (_) #f) (syntax->list #'(our-accessor ...)))
							      #f))))))
		    (predicate-def #'(define-values (?predicate real-predicate)
				       (values predicate-proc predicate-proc)))
		    (accessor-defs #'(define-values (accessor ... our-accessor ...)
				       (values accessor-proc ... accessor-proc ...)))
		    (mutator-defs #'(define-values (mutator ...) (values mutator-proc ...)))
		    (signature-def
		     (with-syntax (((?param ...) (generate-temporaries #'(?field-spec ...))))
		       (with-syntax (((component-signature ...)
				      (map (lambda (accessor param)
					     (with-syntax ((?accessor accessor)
							   (?param param))
					       #'(at ?param (property ?accessor ?param))))
					   (syntax->list #'(our-accessor ...))
					   (syntax->list #'(?param ...)))))
			 (with-syntax ((base-signature
					(stepper-syntax-property
					 #`(define ?type-name
					     #,(cond
						((null? maybe-field-signatures)
						 #'(let ((sig (signature ?type-name (predicate real-predicate))))
						     (set-signature-arbitrary-promise!
						      sig
						      (delay (arbitrary-one-of equal? (real-constructor))))
						     sig))
						((andmap values maybe-field-signatures) ; monomorphic
						 #'(let* ((sigs (list field-signature ...))
							  (sig
							   (make-lazy-wrap-signature '?type-name #t
										     type-descriptor raw-predicate
										     sigs
										     #'?type-name)))
						     (set-signature-arbitrary-promise! 
						      sig
						      (delay
							(let ((arbs (map signature-arbitrary sigs)))
							  (when (andmap values arbs)
							    (apply arbitrary-record
								   real-constructor
								   (list raw-accessor-proc ...)
								   arbs)))))
						     sig))
						(else
						 #'(signature ?type-name (predicate real-predicate)))))
					 'stepper-skip-completely 
					 #t))
				       (constructor-signature
					(stepper-syntax-property
					 (if (syntax->datum #'?mutable?)
					     ;; no lazy signatures
					     #'(define (?signature-constructor-name ?param ...)
						 (signature
						  (combined (at ?type-name (predicate real-predicate))
							    component-signature ...)))
					     ;; lazy signatures
					     #'(define (?signature-constructor-name ?param ...)
						 (let* ((sigs (list ?param ...))
							(sig
							 (make-lazy-wrap-signature '?type-name #t
										   type-descriptor raw-predicate
										   sigs
										   #'?type-name)))
						   (set-signature-arbitrary-promise! 
						    sig
						    (delay
						      (let ((arbs (map signature-arbitrary sigs)))
							(when (andmap values arbs)
							  (apply arbitrary-record
								 real-constructor
								 (list raw-accessor-proc ...)
								 arbs)))))
						   sig)))
					 'stepper-skip-completely
					 #t)))
			   #'(begin
			       ;; we use real-predicate to avoid infinite recursion if a signature
			       ;; for ?type-name using ?predicate is inadvertently defined
			       base-signature
			       constructor-signature))))))
		 ;; again, with properties
		 (with-syntax ((struct-type-defs
				(stepper-syntax-property
				 (syntax/loc x struct-type-defs) 'stepper-black-box-expr #'?stx))
			       (real-constructor-def
				(stepper-syntax-property #'real-constructor-def 'stepper-skip-completely #t))
			       (predicate-def
				(stepper-syntax-property #'predicate-def 'stepper-skip-completely #t))
			       (accessor-defs
				(stepper-syntax-property #'accessor-defs 'stepper-skip-completely #t))
			       (mutator-defs
				(stepper-syntax-property #'mutator-defs 'stepper-skip-completely #t)))
		   #'(begin
		       struct-type-defs
		       signature-def
		       ;; the signature might be used in the definitions, hence this ordering
		       predicate-def real-constructor-def constructor-def accessor-defs mutator-defs)))))))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ... (?field-spec ...))
       (raise-syntax-error 
	#f 
	"Vor den Selektoren/Mutatoren steht eine Form zuviel" #'rest1))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures*" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures*" x))))))

(define (access-record-fields rec acc count)
  (let recur ((i 0))
    (if (= i count)
	'()
	(cons (acc rec i)
	      (recur (+ i 1))))))

(define (make-equal-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
	       (factor 1)
	       (hash 0))
      (if (= i field-count)
	  hash
	  (loop (+ 1 i)
		(* factor 33)
		(+ hash (* factor (recur (generic-access r i)))))))))

(define (make-equal2-hash generic-access field-count)
  (lambda (r recur)
    (let loop ((i 0)
	       (factor 1)
	       (hash 0))
      (if (= i field-count)
	  hash
	  (loop (+ 1 i)
		(* factor 33)
		(+ hash (* factor 
			   (recur (generic-access r (- field-count i 1))))))))))

;; (define-record-procedures :pare kons pare? (kar kdr))

(define-syntax define-record-procedures
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
          ?constructor
          ?predicate
          (?field-spec ...))

       (begin
         (check-for-id!
          (syntax ?type-name)
          "Typ-Name ist kein Bezeichner")
         
         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Bezeichner")
         
         (check-for-id!
          (syntax ?predicate)
          "Prädikat ist kein Bezeichner")
         
         (with-syntax ((?stx x)
		       (field-specs
			(map
			 (lambda (field-spec dummy-mutator)
			   (syntax-case field-spec ()
			     ((accessor signature)
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#`(accessor #,dummy-mutator signature)))
			     (accessor
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#`(accessor #,dummy-mutator #f)))))
			 (syntax->list #'(?field-spec ...))
			 (generate-temporaries #'(?field-spec ...)))))
           (syntax
            (define-record-procedures* ?stx ?type-name #f
	      dummy-signature-constructor-name
              ?constructor
              ?predicate
              field-specs)))))

       ((_ ?type-name
           ?constructor
           ?predicate
           rest)
        (raise-syntax-error 
         #f 
         "Der vierte Operand ist keine Liste von Selektoren" (syntax rest)))
       ((_ ?type-name
           ?constructor
           ?predicate
           rest1 rest2 ... (accessor ...))
        (raise-syntax-error 
         #f 
         "Vor den Selektoren steht eine Form zuviel" #'rest1))
       ((_ ?type-name
           ?constructor
           ?predicate
           rest1 rest2 ...)
        (raise-syntax-error 
         #f 
         "Zu viele Operanden für define-record-procedures" x))
       ((_ arg1 ...)
        (raise-syntax-error 
         #f 
         "Zu wenige Operanden für define-record-procedures" x))
      )))

(define-syntax define-record-procedures-parametric
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?signature-constructor-name
          ?constructor
          ?predicate
          (accessor  ...))


       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?signature-constructor-name)
	  "Signaturkonstruktor-Name ist kein Bezeichner")

         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
         
	 (check-for-id-list! 
	  (syntax->list (syntax (accessor ...)))
	  "Selektor ist kein Bezeichner")

	 (with-syntax ((?stx x)
		       ((dummy-mutator ...)
			(generate-temporaries (syntax (accessor ...)))))
	   (syntax
	    (define-record-procedures* ?stx ?type-name #f ?signature-constructor-name
	      ?constructor
	      ?predicate
	      ((accessor dummy-mutator #f) ...))))))

      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist keine Liste von Selektoren" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-parametric" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-parametric" x))
      )))

;; (define-record-procedures-2 :pare kons pare? ((kar set-kar!) kdr))

(define-syntax define-record-procedures-2
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  (?field-spec ...))

       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?constructor)
	  "Konstruktor ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
	 
	 (with-syntax ((?stx x)
		       (field-specs
			(map
			 (lambda (field-spec dummy-mutator)
			   (syntax-case field-spec ()
			     ((accessor mutator signature)
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#'(accessor mutator signature)))
			     ((accessor mutator)
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#'(accessor mutator #f)))
			     (accessor
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#`(accessor #,dummy-mutator #f)))))
			 (syntax->list #'(?field-spec ...))
			 (generate-temporaries #'(?field-spec ...)))))
	   #'(define-record-procedures* ?stx ?type-name #t
	       dummy-signature-constructor-name
	       ?constructor
	       ?predicate
	       field-specs))))
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-2" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-2" x)))))

(define-syntax define-record-procedures-parametric-2
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  (?field-spec ...))

       (begin
	 (check-for-id!
	  (syntax ?type-name)
	  "Record-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?signature-constructor-name)
	  "Signaturkonstruktor-Name ist kein Bezeichner")

	 (check-for-id!
	  (syntax ?constructor)
	  "Konstruktor ist kein Bezeichner")
         
	 (check-for-id!
	  (syntax ?predicate)
	  "Prädikat ist kein Bezeichner")
	 
	 (with-syntax ((?stx x)
		       (field-specs
			(map
			 (lambda (field-spec dummy-mutator)
			   (syntax-case field-spec ()
			     ((accessor mutator)
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#'(accessor mutator #f)))
			     (accessor
			      (begin
				(check-for-id! (syntax accessor)
					       "Selektor ist kein Bezeichner")
				#`(accessor #,dummy-mutator #f)))))
			 (syntax->list #'(?field-spec ...))
			 (generate-temporaries #'(?field-spec ...)))))
	   #'(define-record-procedures* ?stx ?type-name #t ?signature-constructor-name
	       ?constructor
	       ?predicate
	       field-specs))))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest)
       (raise-syntax-error 
	#f 
	"Der vierte Operand ist illegal" (syntax rest)))
      ((_ ?type-name
	  ?signature-constructor-name
	  ?constructor
	  ?predicate
	  rest1 rest2 ...)
       (raise-syntax-error 
	#f 
	"Zu viele Operanden für define-record-procedures-parametric-2" x))
      ((_ arg1 ...)
       (raise-syntax-error 
	#f 
	"Zu wenige Operanden für define-record-procedures-parametric-2" x)))))
