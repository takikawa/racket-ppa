;; Mostly copied from racket/private/custom-write.rkt, except for the
;; trailing : after the constructor name in write mode, which we don't
;; have.
#lang racket/base
(require racket/pretty)
(provide make-constructor-style-printer)

;; make-constructor-style-printer : (Any -> (U String Symbol))
;;                                  (Any -> (Sequenceof Any))
;;                               -> (Any OutputPort (U #t #f 0 1)) -> Void
(define (make-constructor-style-printer get-constructor get-contents)
  (lambda (obj port mode)
    (define (recur x p)
      (case mode
        ((#t) (write x p))
        ((#f) (display x p))
        ((0 1) (print x p mode))))

    ;; Only two cases: 0 vs everything else
    (define (print-prefix p)
      (let ([prefix
             (case mode
               ((0) "(")
               (else "#<"))]
            [constructor
             (get-constructor obj)])
        (write-string prefix p)
        (display constructor p)))

    (define (print-suffix p)
      (let ([suffix
             (case mode
               ((0) ")")
               (else ">"))])
        (write-string suffix p)))

    (define (print-contents p leading-space)
      (let ([lead (if leading-space (make-string (add1 leading-space) #\space) " ")]
	    [cnt (get-contents obj)])
	(if (not (sequence? cnt))
	    (write-string "<contents not a sequence>" p)
	    (for ([elt cnt]) ;; note: generic sequence
		 (when leading-space
		   (pretty-print-newline p (pretty-print-columns)))
		 (write-string lead p)
		 (recur elt p)))))

    (define (print/one-line p)
      (print-prefix p)
      (print-contents p #f)
      (print-suffix p))

    (define (print/multi-line p)
      (let-values ([(line col pos) (port-next-location p)])
        (print-prefix p)
        (print-contents p col)
        (print-suffix p)))

    (cond [(and (pretty-printing)
                (integer? (pretty-print-columns)))
           ((let/ec esc
              (letrec ([tport
                        (make-tentative-pretty-print-output-port
                         port
                         (- (pretty-print-columns) 1)
                         (lambda () 
                           (esc
                            (lambda ()
                              (tentative-pretty-print-port-cancel tport)
                              (print/multi-line port)))))])
                (print/one-line tport)
                (tentative-pretty-print-port-transfer tport port))
              void))]
          [else
           (print/one-line port)])
    (void)))
