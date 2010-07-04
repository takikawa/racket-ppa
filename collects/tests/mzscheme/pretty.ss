
; Test pretty-print.

;; Regression test results in pp-regression.ss. When things
;;  have to change, re-run with `record-for-regression?' to #t.
(define record-for-regression? #f)
;; Disable `use-regression?' when inspecting results after a
;;  changed; when it's ok, then record the new regression results.
(define use-regression? #t)

(load-relative "loadtest.ss")

(Section 'pretty)

(require mzlib/pretty)

(define (pprec-print pprec port write?)
  (define (print-one n port)
    ((if write? write display) (pprec-ref pprec n) port))
  (let ([pp? (and (pretty-printing)
		  (number? (pretty-print-columns)))])
    (write-string (if write? "W{" "D{") port)
    (let-values ([(l c p) (if pp?
			      (port-next-location port)
			      (values 0 0 0))])
      (print-one 0 port)
      (if pp?
	  (let* ([mx (- (pretty-print-columns) 1)]
		 [tp (make-tentative-pretty-print-output-port
		      port
		      mx
		      void)])
	    (write-string " " tp)
	    (print-one 1 tp)
	    (let-values ([(xl xc xp) (port-next-location tp)])
	      (if (xc . < . mx)
		  (tentative-pretty-print-port-transfer tp port)
		  (begin
		    (tentative-pretty-print-port-cancel tp)
		    (let ([cnt (pretty-print-newline port mx)])
		      (write-string (make-string (max 0 (- c cnt)) #\space) port))
		    (print-one 1 port)))))
	  (begin
	    (write-string " " port)
	    (print-one 1 port)))
      (write-string "}" port))))

(define-values (s:pprec make-pprec pprec? pprec-ref pprec-set!)
  (make-struct-type 'pprec #f 2 0 #f
		    (list (cons prop:custom-write pprec-print))))

(test "10" pretty-format 10)
(test "1/2" pretty-format 1/2)
(test "-1/2" pretty-format -1/2)
(test "1/2+3/4i" pretty-format 1/2+3/4i)
(test "0.333" pretty-format #i0.333)
(test "2.0+1.0i" pretty-format #i2+1i)
(test "'a" pretty-format ''a)
(test "`a" pretty-format '`a)
(test ",a" pretty-format ',a)
(test ",@a" pretty-format ',@a)
(test "#'a" pretty-format '#'a)
(test "W{1 2}" pretty-format (make-pprec 1 2))
(test "#&10" pretty-format (box 10))
(parameterize ([print-box #f])
  (test "#<box>" pretty-format (box 10)))
(test "#(10)" pretty-format (vector 10))
(test "#(10 10)" pretty-format (vector 10 10))
(parameterize ([print-vector-length #t])
  (test "#1(10)" pretty-format (vector 10))
  (test "#2(10)" pretty-format (vector 10 10))
  (test "#2(10 20)" pretty-format (vector 10 20)))
(test "#hasheq((1 . 2))" pretty-format (let ([ht (make-hasheq)])
                                         (hash-set! ht 1 2)
                                         ht))
(test "#hash((1 . 2))" pretty-format (let ([ht (make-hash)])
                                       (hash-set! ht 1 2)
                                       ht))
(test "#hash((1 . 2))" pretty-format #hash((1 . 2)))
(test "#hasheq((1 . 2))" pretty-format #hasheq((1 . 2)))
(parameterize ([print-hash-table #f])
  (test "#<hash>" pretty-format (let ([ht (make-hasheq)])
                                  (hash-set! ht 1 2)
                                  ht)))

(parameterize ([pretty-print-abbreviate-read-macros #f])
  (test "(quote a)" pretty-format ''a)
  (test "(quasiquote a)" pretty-format '`a)
  (test "(unquote a)" pretty-format ',a)
  (test "(unquote-splicing a)" pretty-format ',@a)
  (test "(syntax a)" pretty-format '#'a))


(test "(1\n 2)" pretty-format '(1 2) 2)
(test "(1 2)" pretty-format '(1 2) 'infinity)

(parameterize ([print-hash-table #t])
  (test "#hasheq((1 . 2))" pretty-format (let ([ht (make-hasheq)])
                                       (hash-set! ht 1 2)
                                       ht)))

(test #t pretty-print-style-table? (pretty-print-current-style-table))
(test #t pretty-print-style-table? (pretty-print-extend-style-table #f null null))
(test #t pretty-print-style-table? (pretty-print-extend-style-table (pretty-print-current-style-table) null null))

(parameterize ([pretty-print-columns 20])
  (test "(1234567890 1 2 3 4)" pretty-format '(1234567890 1 2 3 4))
  (test "(1234567890xx\n  1\n  2\n  3\n  4)" pretty-format '(1234567890xx 1 2 3 4))
  (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4))
  (let ([table (pretty-print-extend-style-table #f null null)])
    (parameterize ([pretty-print-current-style-table 
                    (pretty-print-extend-style-table table '(lambda) '(list))])
      (test "(lambda\n  1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4)))
    (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4))
    (parameterize ([pretty-print-current-style-table table])
      (test "(lambda 1234567890\n  1\n  2\n  3\n  4)" pretty-format '(lambda 1234567890 1 2 3 4)))))

(parameterize ([pretty-print-exact-as-decimal #t])
  (test "10" pretty-format 10)
  (test "0.5" pretty-format 1/2)
  (test "-0.5" pretty-format -1/2)
  (test "3500.5" pretty-format 7001/2)
  (test "0.0001220703125" pretty-format 1/8192)
  (test "0.0000000000000006869768746897623487"
	pretty-format 6869768746897623487/10000000000000000000000000000000000)
  (test "0.00000000000001048576" pretty-format (/ (expt 5 20)))
  
  (test "1/3" pretty-format 1/3)
  (test "1/300000000000000000000000" pretty-format 1/300000000000000000000000)
  
  (test "0.5+0.75i" pretty-format 1/2+3/4i)
  (test "0.5-0.75i" pretty-format 1/2-3/4i)
  (test "1/9+3/17i" pretty-format 1/9+3/17i)
  (test "0.333" pretty-format #i0.333)
  (test "2.0+1.0i" pretty-format #i2+1i))

(let ()
  (define-struct wrap (content))
  (define (add-wrappers x)
    (let loop ([x x])
      (cond
        [(symbol? x) (make-wrap x)]
        [(pair? x) (cons (loop (car x))
                         (loop (cdr x)))]
        [else x])))
  (parameterize ([pretty-print-remap-stylable
                  (λ (x)
                    (and (wrap? x)
                         (wrap-content x)))]
                 [pretty-print-columns 6]
                 [pretty-print-size-hook
                  (λ (val dsp? port)
                    (if (wrap? val)
                        (string-length (format "~s" (wrap-content val)))
                        #f))]
                 [pretty-print-print-hook
                  (λ (val dsp? port)
                    (write (wrap-content val) port))])
    (test "(lambda (x)\n  abcdef)" pretty-format (add-wrappers '(lambda (x) abcdef)))
    (test "(call/cc\n  call/cc)" pretty-format (add-wrappers '(call/cc call/cc)))))

(parameterize ([print-struct #t])
  (let ()
    (define-struct s (x) #:inspector (make-inspector))
    (test "#(struct:s 1)" pretty-format (make-s 1))))

(err/rt-test (pretty-print-extend-style-table 'ack '(a) '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) 'a '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(1) '(b)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) 'b))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) '(1)))
(err/rt-test (pretty-print-extend-style-table (pretty-print-current-style-table) '(a) '(b c)) exn:application:mismatch?)

(define-struct s (a b c))

(define (make k?)
  (let ([make (if k? make (lambda (x) '(end)))])
    (list
     1
     'a
     "a"
     (list 'long-name-numero-uno-one-the-first-supreme-item
	   'long-name-number-two-di-ar-ge-second-line)
     (map (lambda (v v2)
	    (make-s v 2 v2))
	  (make #f)
	  (reverse (make #f)))
     '(1)
     '(1 2 3)
     '(1 . 2)
     (mcons 1 2)
     (mcons 1 (mcons 2 null))
     #(1 2 3 4 5)
     (read (open-input-string "(#0=() . #0#)"))
     (read (open-input-string "#1=(1 . #1#)"))
     (map box (make #f))
     (make #f)
     (make-pprec 1 2)
     (make-pprec 'long-name-numero-uno-one-the-first-supreme-item
		 'long-name-number-two-di-ar-ge-second-line)
     (let ([p (make-pprec "a" "c")])
       (pprec-set! p 0 p)
       p))))

(define vs (make #t))

(define print-line-no
  (lambda (line port offset width) 
    (if line         
	(begin
	  (when (positive? line) (write-char #\newline port))
	  (fprintf port "~s~a~a~a " line
		   (if (< line 10) " " "")
		   (if (< line 100) " " "")
		   (if (< line 1000) " " ""))
	  5)
	(fprintf port "!~n"))))

(define modes
  (list
   (list "DEPTH=2" pretty-print-depth 2)
   (list "GRAPH-ON" print-graph #t)
   (list "HASH-TABLE-ON" print-hash-table #t)
   (list "STRUCT-ON" print-struct #t)
   (list "LINE-NO-ON" pretty-print-print-line print-line-no)
   (list "SUPER-WIDE" pretty-print-columns 300)))

(define num-combinations (arithmetic-shift 1 (length modes)))

(define regression-path
  (build-path (current-load-relative-directory)
	      "pp-regression.ss"))

(define recorded (if record-for-regression?
		     null
		     (with-input-from-file regression-path
		       read)))

(define (record-or-check id thunk)
  (if use-regression?
      (let ([str (let ([p (open-output-bytes)])
		   (parameterize ([current-output-port p])
		     (thunk))
		   (get-output-bytes p))])
	(if record-for-regression?
	    (set! recorded (cons (cons id str) recorded))
	    (test (cdr (assoc id recorded)) values str)))
      (thunk)))

(let loop ([n 0])
  (when (< n num-combinations)
    (record-or-check
     `(mode ,n)
     (lambda ()
       (let loop ([modes modes][n n])
	 (cond
	  [(null? modes) (printf ":~n") (map pretty-print vs)]
	  [(positive? (bitwise-and n 1))
	   (let ([mode (car modes)])
	     (printf "~s " (car mode))
	     (parameterize ([(cadr mode) (caddr mode)])
	       (loop (cdr modes) (arithmetic-shift n -1))))]
	  [else
	   (loop (cdr modes) (arithmetic-shift n -1))]))))
    (loop (add1 n))))

(when record-for-regression?
  (with-output-to-file regression-path
    #:exists 'truncate/replace
    (lambda () (write recorded))))

(test #t 'use-regression? use-regression?)

(report-errs)
