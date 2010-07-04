
;; Basic checks for the intermediate language. See also
;;  beginner.ss

(load-relative "loadtest.ss")

;; Don't need these:
(define no-extra-if-tests? #t)

;; Check export names:
(require syntax/docprovide)
(let ([docs (lookup-documentation '(lib "htdp-intermediate.ss" "lang") 'procedures)])
  (for-each
   (lambda (row)
     (for-each
      (lambda (doc)
	(let ([v (dynamic-require '(lib "htdp-intermediate.ss" "lang") (car doc))])
	  (when (procedure? v)
	    (test (car doc) object-name v))))
      (cdr row)))
   docs))

(define current-htdp-lang 'lang/htdp-intermediate)
(load-relative "htdp-test.ss")

(require (lib "htdp-intermediate.ss" "lang"))

(load-relative "beg-adv.ss")
(load-relative "beg-intml.ss")
(load-relative "beg-intm.ss")
(load-relative "bega-adv.ss")
(load-relative "intm-intml.ss")
(load-relative "intm-adv.ss")

(report-errs)
