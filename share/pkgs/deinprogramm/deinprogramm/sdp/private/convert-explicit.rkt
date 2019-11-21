#lang racket/base
(provide convert-explicit)

(require mzlib/pretty
	 mzlib/struct)

(require deinprogramm/private/explicit-write)

(require deinprogramm/signature/signature-german
	 (only-in deinprogramm/signature/signature
		  signature? signature-name))

; I HATE DEFINE-STRUCT!
(define-struct/properties :empty-list ()
  ((prop:custom-write
    (lambda (r port write?)
      (write-string "#<empty-list>" port))))
  (make-inspector))

(define-struct/properties :signature (sig)
  ((prop:custom-write
    (lambda (r port write?)
      (cond
       ((signature-name (:signature-sig r))
	=> (lambda (n)
	     (write-string "#<signature:" port)
	     (write-string (symbol->string n) port)
	     (write-string ">" port)))
    (else
     (write-string "#<signature>" port))))))
  (make-inspector))

;; might be improper
(define-struct/properties :list (elements)
  ((prop:custom-write (make-constructor-style-printer
		       (lambda (obj) 'list)
		       (lambda (obj) (:list-elements obj)))))
  (make-inspector))

; we wrap procedures in this so we can print them as #<function:...>
(struct :function (func)
 #:property prop:custom-write
 (lambda (r port write?)
   (cond
    ((object-name (:function-func r))
     => (lambda (n)
	  (write-string "#<function:" port)
	  (write-string (symbol->string n) port)
	  (write-string ">" port)))
    (else
     (write-string "#<function>" port))))
 #:property prop:procedure (struct-field-index func)
 #:inspector (make-inspector))

(define (convert-explicit v)
  (let ((hash (make-hasheq)))
    (let recur ((v v))
      (cond
       ((null? v) (make-:empty-list)) ; prevent silly printing of sharing
       ((signature? v) (make-:signature v))
       ((procedure? v) (:function v))
       ((pair? v)
	(make-:list
	 (let list-recur ((v v))
	   (cond
	    ((null? v)
	     v)
	    ((not (pair? v))
	     '()) ; the stepper feeds all kinds of garbage in here
	    (else
	     (cons (recur (car v))
		   (list-recur (cdr v))))))))
       ((struct? v)
	(or (hash-ref hash v #f)
	    (let-values (((ty skipped?) (struct-info v)))
	      (cond
	       ((and ty (lazy-wrap? ty))
		(let ((lazy-wrap-info (lazy-wrap-ref ty)))
		  (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
			(raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info)))
		    (let ((val (apply constructor (map (lambda (raw-accessor)
							 (recur (raw-accessor v)))
						       raw-accessors))))
		      (hash-set! hash v val)
		      val))))
	       (else v)))))
       (else
	v)))))


