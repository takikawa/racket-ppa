; I HATE DEFINE-STRUCT!
(define-struct/properties :empty-list ()
  ((prop:custom-write
    (lambda (r port write?)
      (write-string "#<empty-list>" port))))
  (make-inspector))

;; might be improper
(define-struct/properties :list (elements)
  ((prop:custom-write (make-constructor-style-printer
		       (lambda (obj) 'list)
		       (lambda (obj) (:list-elements obj)))))
  (make-inspector))

(define (convert-explicit v)
  (let ((hash (make-hasheq)))
    (let recur ((v v))
      (cond
       ((null? v) (make-:empty-list)) ; prevent silly printing of sharing
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

