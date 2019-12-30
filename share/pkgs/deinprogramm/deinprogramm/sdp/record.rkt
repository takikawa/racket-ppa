#lang scheme/base

(provide define-record-functions)

(require scheme/promise
	 mzlib/struct
         mzlib/pconvert-prop
         mzlib/pretty
	 deinprogramm/signature/signature
	 deinprogramm/signature/signature-german
	 deinprogramm/signature/signature-syntax
	 (only-in deinprogramm/quickcheck/quickcheck arbitrary-record arbitrary-one-of))

(require deinprogramm/private/explicit-write)

(require (for-syntax scheme/base)
         (for-syntax deinprogramm/private/syntax-checkers)
         (for-syntax stepper/private/syntax-property)
	 (for-syntax racket/struct-info)
         (for-syntax syntax/struct))

(define any (signature any %any))

(begin-for-syntax
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
	       (cons (proc i (car list)) rev-result))))))

(define-syntax define-record-functions*
  (lambda (x)
    (syntax-case x ()
      ((_ ?stx
	  ?type-spec
	  ?constructor
	  ?predicate
	  (?accessor ?field-signature) ...)

       (with-syntax
	   (((?type-name ?type-params ...)
	     (if (identifier? #'?type-spec)
		 #'(?type-spec)
		 #'?type-spec))
	    (number-of-fields (length (syntax->list (syntax (?accessor ...))))))
	 (with-syntax
	     (((accessor-proc ...)
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
		(syntax->list #'(?accessor ...))))
	      ((our-accessor ...) (generate-temporaries #'(?accessor ...)))
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
	      (constructor-proc
	       (syntax-property #'(lambda (?accessor ...)
				    (raw-constructor ?accessor ... #f))
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
			       (syntax->list #'(?accessor ...))))
	      ((raw-mutator-proc ...)
	       (map-with-index (lambda (i _)
				 #`(lambda (r val)
				     (raw-generic-mutate r #,i val)))
			       (syntax->list #'(?accessor ...))))
	      (record-equal? #`(lambda (r1 r2 equal?)
				 (and #,@(map-with-index (lambda (i _)
							   #`(equal? (raw-generic-access r1 #,i)
								     (raw-generic-access r2 #,i)))
							 (syntax->list #'(?accessor ...))))))
	      ((?type-param-bindings ...)
	       (map (lambda (type-param)
		      (with-syntax ((?type-param type-param)
				    (?type-var (string->symbol
						(string-append "%" (symbol->string (syntax->datum type-param))))))
			#'(?type-param (signature ?type-var))))
		    (syntax->list #'(?type-params ...)))))

				 
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
		(real-constructor-def
		 #'(define/signature real-constructor
		     (let (?type-param-bindings ...)
		       (signature (?field-signature ... -> ?type-spec)))
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
		(accessor-defs #'(define-values (?accessor ... our-accessor ...)
				   (values accessor-proc ... accessor-proc ...)))
		(signature-def
		 (with-syntax (((?param ...) (generate-temporaries #'(?accessor ...))))
		   (with-syntax (((component-signature ...)
				  (map (lambda (accessor param)
					 (with-syntax ((?accessor accessor)
						       (?param param))
					   #'(at ?param (property ?accessor ?param))))
				       (syntax->list #'(our-accessor ...))
				       (syntax->list #'(?param ...)))))
		     (stepper-syntax-property
		      #'(define ?type-spec
			  (let* ((sigs (list (signature ?field-signature) ...))
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
		      'stepper-skip-completely 
		      #t)))))
	     ;; again, with properties
	     (with-syntax ((struct-type-defs
			    (stepper-syntax-property
			     (syntax/loc x struct-type-defs) 'stepper-black-box-expr #'?stx))
			   (real-constructor-def
			    (stepper-syntax-property #'real-constructor-def 'stepper-skip-completely #t))
			   (predicate-def
			    (stepper-syntax-property #'predicate-def 'stepper-skip-completely #t))
			   (accessor-defs
			    (stepper-syntax-property #'accessor-defs 'stepper-skip-completely #t)))
	       #'(begin
		   struct-type-defs
		   signature-def
		   ;; the signature might be used in the definitions, hence this ordering
		   predicate-def real-constructor-def constructor-def accessor-defs)))))))))

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

;; FIXME: duplicate from primitives.rkt
(define-for-syntax (binding-in-this-module? b)
  (and (list? b)
       (module-path-index? (car b))
       (let-values (((path base) (module-path-index-split (car b))))
	 (and (not path) (not base)))))

(define-for-syntax (check-id-unbound! id)
  (cond
   ((identifier-binding id)
    => (lambda (binding)
	 (if (binding-in-this-module? binding)
	     (raise-syntax-error
	      #f
	      "Es gibt schon eine Definition für den Namen"
	      id)
	     (raise-syntax-error
	      #f
	      "Dieser Name gehört einer eingebauten Funktion"
	      id))))))

;; (define-record-functions :pare kons pare? (kar integer) (kdr list-of-integers))

(define-syntax define-record-functions
  (lambda (x)
    (syntax-case x ()
      ((_ ?type-spec)
       (raise-syntax-error 
        #f 
        "Zu wenige Operanden für define-record-functions" x))
      ((_ ?type-spec ?constructor)	; nullary case
       #'(define-record-functions ?type-spec ?constructor dummy-predicate))
      ((_ ?type-spec
          ?constructor
          (?accessor ?signature) ?field-spec ...)
       #'(define-record-functions ?type-spec ?constructor dummy-predicate (?accessor ?signature) ?field-spec ...))
      
      ((_ ?type-spec
          ?constructor
          ?predicate
          ?field-spec ...)

       (with-syntax (((?type-name ?type-params ...)
		      (if (identifier? #'?type-spec)
			  #'(?type-spec)
			  #'?type-spec)))

         (check-for-id!
          (syntax ?type-name)
          "Typ ist kein Name")

	 (for-each (lambda (type-param)
		     (check-for-id!
		      type-param
		      "Parameter zu Typ-Konstruktor ist kein Name"))
		   (syntax->list #'(?type-params ...)))
         
         (check-for-id!
          (syntax ?constructor)
          "Konstruktor ist kein Name")
         
         (check-for-id!
          (syntax ?predicate)
          "Prädikat ist kein Name")

	 (check-id-unbound! #'?type-name)
	 (check-id-unbound! #'?constructor)
	 (check-id-unbound! #'?predicate)
	 
	 (for-each
	  (lambda (field-spec)
	    (syntax-case field-spec ()
	      ((?accessor ?selector)
	       (begin
		 (check-for-id! #'?accessor "Selektor ist kein Name")
		 (check-id-unbound! #'?accessor)))
	      (?stuff
	       (raise-syntax-error #f "Feld hat nicht die Form (Selektor Signatur)" #'?stuff))))
	  (syntax->list #'(?field-spec ...)))

	 
         (with-syntax ((?stx x))
           #'(define-record-functions* ?stx ?type-spec
               ?constructor
               ?predicate
               ?field-spec ...)))))))


