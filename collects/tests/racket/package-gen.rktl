
(require scheme/package
	 mzlib/pretty
	 syntax/toplevel)

(define (check x)
  (unless (equal? x 'this-is-right)
    (error "check" "nopde: ~e" x)))

(define open-context-forms
  (list (lambda (l) `(begin ,@l))
	(lambda (l) `(let () ,@l))
	(lambda (l) `(define-package other () ,@l))))

(define open-forms
  (apply
   append
   (map
    (lambda (open-form)
      (map (lambda (ctx)
	     (ctx `((,open-form pk-to-open) (check var-to-use))))
	   open-context-forms))
    (list 'open-package 'open*-package))))

(define (mk-package-shell-forms name)
  (list (lambda (body) `(define-package ,name #:all-defined ,@body))
	(lambda (body) `(define-package ,name (var-to-use) ,@body))))

(define package-shell-forms
  (mk-package-shell-forms 'pk-to-open))

(define defn-forms
  (list '(define var-to-use 'this-is-right)
	'(define* var-to-use 'this-is-right)
	'(begin
	    (define* var-to-use 'this-is-wrong)
	    (define* var-to-use 'this-is-right))))

(define body-forms
  (apply
   append
   (map (lambda (body-ctx)
	  (map body-ctx defn-forms))
	(list
	 (lambda (d) `(,d))
	 (lambda (d) `((begin ,d)))
	 (lambda (d) `((define y 'no-this-one) ,d))
	 (lambda (d) `(,d (define y 'no-this-one)))))))


(define package-forms
  (apply 
   append
   (map
    (lambda (ps)
      (map ps body-forms))
    package-shell-forms)))

(define combo-context-forms
  (list (lambda (p o) `(begin ,p ,o))
	(lambda (p o) `(let () ,p ,o 10))
	(lambda (p o) `(define-package out1 #:all-defined ,p ,o))
	(lambda (p o) `(define-package out2 #:all-defined (define-package out1 #:all-defined ,p ,o)))))

(define all-forms
  (apply
   append
   (map (lambda (cc)
	  (apply
	   append
	   (map (lambda (p)
		  (map (lambda (o)
			 (cc p o))
		       open-forms))
		package-forms)))
	combo-context-forms)))

(define do-threshold 3)

(let ([ns (current-namespace)]
      [total (length all-forms)]
      [cnt 0])
  (for-each (lambda (form)
	      (set! cnt (add1 cnt))
	      (when (zero? (modulo cnt 100))
		(printf "~a/~a\n" cnt total))
	      (when ((add1 (random 10)) . >= . do-threshold)
                ;; (pretty-print form)
		(parameterize ([current-namespace (make-base-namespace)])
		  (namespace-attach-module ns 'scheme/package)
		  (let ([done? #f]
			[mode "top-level"])
		    (with-handlers ([exn:fail?
				     (lambda (x)
				       (printf "At ~a:\n" mode)
				       (pretty-print form)
				       (raise x))])
		      (eval `(require scheme/package))
		      (eval `(define check ,(lambda (x)
					      (check x)
					      (set! done? #t))))
		      (eval form)
		      (unless done?
			(error "check" "didn't execute"))
		      (set! done? #f)
		      (set! mode "top-level expand")
		      (eval-syntax (expand-top-level-with-compile-time-evals 
				    (datum->syntax #f form)))
		      (unless done?
			(error "check" "didn't execute after expand"))
		      (let ([mod (lambda (name)
				   `(module ,name scheme/base
				      (require scheme/package)
				      (define check ,(lambda (x)
						       (check x)
						       (set! done? #t)))
				      ,form))])
			(set! done? #f)
			(set! mode "module")
			(eval (mod 'm))
			(eval `(require 'm))
			(unless done?
			  (error "check" "module didn't execute"))
			(set! done? #f)
			(set! mode "module expand")
			(eval-syntax (expand (mod 'n)))
			(eval `(require 'n))
			(unless done?
			  (error "check" "module didn't execute after expand"))))))))
	    all-forms))
