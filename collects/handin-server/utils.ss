(module utils mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred")
	   (lib "posn.ss" "lang")
	   "run-status.ss"
	   (prefix pc: (lib "pconvert.ss"))
	   (lib "pretty.ss")
	   (lib "list.ss")
	   (lib "string.ss")
	   (only "handin-server.ss" LOG timeout-control))

  (provide unpack-submission

	   unpack-test-suite-submission
	   is-test-suite-submission?

	   make-evaluator
	   make-evaluator/submission
	   evaluate-all
	   evaluate-submission

	   call-with-evaluator
	   call-with-evaluator/submission
	   reraise-exn-as-submission-problem
	   current-run-status
	   message
           current-value-printer

	   coverage-enabled

	   check-proc
	   check-defined
	   look-for-tests
	   user-construct
	   test-history-enabled

	   LOG
	   timeout-control)

  (define (unpack-submission str)
    (let* ([base (make-object editor-stream-in-bytes-base% str)]
	   [stream (make-object editor-stream-in% base)]
	   [definitions-text (make-object text%)]
	   [interactions-text (make-object text%)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send definitions-text read-from-file stream)
      (send interactions-text read-from-file stream)
      (read-editor-global-footer stream)
      (values definitions-text interactions-text)))

  (define (unpack-test-suite-submission str)
    (let* ([base (make-object editor-stream-in-bytes-base% str)]
	   [stream (make-object editor-stream-in% base)]
	   [ts (make-object ts-load%)])
      (read-editor-version stream base #t)
      (read-editor-global-header stream)
      (send ts read-from-file stream)
      (read-editor-global-footer stream)
      ts))

  (define (is-test-suite-submission? str)
    (send (unpack-test-suite-submission str)
	  got-program?))

  ;; Test Suite Unpacking ----------------------------------------
  ;; This code duplicates just enough of the test-suite snips
  ;;  to make test-suite files readable.
  
  (define program-header-field-name "drscheme:test-suite:program")

  (define csc (new
	       (class snip-class%
		 (define/override (read f)
		   (let ([case (new case%)])
		     (send case read-from-file f)
		     case))
		 (super-new))))
  (send csc set-classname "case%")
  (send csc set-version 1)
  (send (get-the-snip-class-list) add csc)

  (define case%
    (class editor-snip%
      (inherit set-snipclass get-editor)

      (define call (new text%))
      (define expected (new text%))
      (define test (new text%))

      (define/public (read-from-file f)
	(send call read-from-file f)
	(send expected read-from-file f)
	(send test read-from-file f)
	(send f get-string))

      (super-new)

      (set-snipclass csc)
      (send (get-editor) insert (make-object editor-snip% call))
      (send (get-editor) insert (make-object editor-snip% expected))
      (send (get-editor) insert (make-object editor-snip% test))))

  (define dsc (new
	       (class snip-class%
		 (define/override (read f)
		   (let ([helper (new helper%)])
		     (send helper read-from-file f)
		     helper))
		 (super-new))))
  (send dsc set-classname "drscheme:test-suite:helper%")
  (send dsc set-version 1)
  (send (get-the-snip-class-list) add dsc)

  (define helper%
    (class editor-snip%
      (inherit set-snipclass get-editor)

      (define/public (read-from-file f)
	(send (get-editor) read-from-file f))

      (super-new)

      (set-snipclass dsc)))

  (define ts-load%
    (class pasteboard%
      (define program (new text%))

      (define got-p? #f)
      (define/public (got-program?) got-p?)

      (define/override (read-header-from-file stream name)
	(if (string=? name program-header-field-name)
	    (begin
	      (set! got-p? #t)
	      (send program read-from-file stream))
	    (super read-header-from-file stream name)))

      (super-new)))

  ;; Protection ---------------------------------------

  (define ok-path-re
    (regexp
     (string-append
      "^(?:"
      (apply string-append
             (cdr (apply append
                         (map (lambda (p)
                                (list "|" (regexp-quote (path->string p))))
                              (current-library-collection-paths)))))
      ")(?:/|$)")))

  (define tight-security
    (make-security-guard
     (current-security-guard)
     (lambda (what path modes)
       (when (or (memq 'write modes)
                 (memq 'execute modes)
                 (memq 'delete modes)
                 (and path (not (regexp-match ok-path-re (path->string path)))))
         (error what "file access denied (~a)" path)))
     (lambda (what host port mode) (error what "network access denied"))))

  (define null-input (open-input-string ""))
  (define (safe-eval expr . more)
    (parameterize ([current-security-guard tight-security]
                   [current-input-port null-input]
                   ;; breaks: [current-code-inspector (make-inspector)]
                   )
      (apply eval expr more)))

  ;; Execution ----------------------------------------

  (define coverage-enabled (make-parameter #f))

  (define (make-evaluator language teachpacks program-port)
    (let ([coverage-enabled (coverage-enabled)]
          [execute-counts #f]
          [ns (make-namespace-with-mred)]
	  [orig-ns (current-namespace)]
	  [posn-module ((current-module-name-resolver) '(lib "posn.ss" "lang") #f #f)])
      (parameterize ([current-namespace ns]
		     [read-case-sensitive #t]
		     [read-decimal-as-inexact #f]
		     [current-inspector (make-inspector)])
	(namespace-attach-module orig-ns posn-module)
	(parameterize ([current-eventspace (make-eventspace)])
	  (let ([ch (make-channel)]
		[result-ch (make-channel)])
	    (queue-callback
	     (lambda ()
	       ;; First read program and evaluate it as a module:
	       (with-handlers ([void (lambda (exn) (channel-put result-ch (cons 'exn exn)))])
		 (let* ([body
                         (parameterize ([read-case-sensitive #t]
                                        [read-decimal-as-inexact #f])
                           (let loop ([l null])
                             (let ([expr (read-syntax 'program program-port)])
                               (if (eof-object? expr)
                                 (reverse l)
                                 (loop (cons expr l))))))]
                        [body (append (if (and (pair? teachpacks)
                                               (eq? 'begin (car teachpacks)))
                                        (cdr teachpacks)
                                        (map (lambda (tp)
                                               `(,#'require
                                                 ,(if (pair? tp)
                                                    tp `(file ,tp))))
                                             teachpacks))
                                      body)]
                        [body
                         (cond
                          [(and (symbol? language)
                                (memq language '(beginner
                                                 beginner-abbr
                                                 intermediate
                                                 intermediate-lambda
                                                 advanced)))
                           `(module m
                                (lib ,(case language
                                        [(beginner) "htdp-beginner.ss"]
                                        [(beginner-abbr) "htdp-beginner-abbr.ss"]
                                        [(intermediate) "htdp-intermediate.ss"]
                                        [(intermediate-lambda) "htdp-intermediate-lambda.ss"]
                                        [(advanced) "htdp-advanced.ss"])
                                     "lang")
                              ,@body)]
                          [(or (and (pair? language) (eq? 'lib (car language)))
                               (symbol? language))
                           `(module m ,language ,@body)]
                          [(and (pair? language)
                                (eq? 'begin (car language)))
                           `(begin ,language ,@body)]
                          [else (error 'make-evaluator
                                       "Bad language specification: ~e"
                                       language)])])
                   (when coverage-enabled
                     (for-each safe-eval
                               '((require (lib "errortrace.ss" "errortrace"))
                                 (execute-counts-enabled #t))))
                   (safe-eval body)
                   (when (and (pair? body) (eq? 'module (car body))
                              (pair? (cdr body)) (symbol? (cadr body)))
                     (let ([mod (cadr body)])
                       (safe-eval `(require ,mod))
                       (current-namespace (module->namespace mod))))
                   (when coverage-enabled
                     (set! execute-counts
                           (map (lambda (x) (cons (car x) (cdr x)))
                                (filter (lambda (x)
                                          (eq? 'program (syntax-source (car x))))
                                        (safe-eval '(get-execute-counts) ns))))))
		 (channel-put result-ch 'ok))
	       ;; Now wait for interaction expressions:
	       (let loop ()
		 (let ([expr (channel-get ch)])
		   (unless (eof-object? expr)
		     (with-handlers ([void (lambda (exn)
					     (channel-put result-ch (cons 'exn exn)))])
		       (channel-put result-ch (cons 'val (safe-eval expr))))
		     (loop))))
	       (let loop ()
		 (channel-put result-ch '(exn . no-more-to-evaluate))
		 (loop))))
	    (let ([r (channel-get result-ch)])
	      (if (eq? r 'ok)
		  ;; Initial program executed ok, so return an evaluator:
		  (lambda (expr . more)
                    (if (pair? more)
                      (case (car more)
                        [(execute-counts) execute-counts]
                        [else (error 'make-evaluator
                                     "Bad arguments: ~e"
                                     (cons expr more))])
                      (begin (channel-put ch expr)
                             (let ([r (channel-get result-ch)])
                               (if (eq? (car r) 'exn)
                                 (raise (cdr r))
                                 (cdr r))))))
		  ;; Program didn't execute:
		  (raise (cdr r)))))))))

  (define (open-input-text-editor/lines str)
    (let ([inp (open-input-text-editor str)])
      (port-count-lines! inp) inp))

  (define (make-evaluator/submission language teachpacks str)
    (let-values ([(defs interacts) (unpack-submission str)])
      (make-evaluator language teachpacks (open-input-text-editor/lines defs))))

  (define (evaluate-all source port eval)
    (let loop ()
      (let ([expr (parameterize ([read-case-sensitive #t]
				 [read-decimal-as-inexact #f])
		    (read-syntax source port))])
	(unless (eof-object? expr)
	  (eval expr)
	  (loop)))))

  (define (evaluate-submission str eval)
    (let-values ([(defs interacts) (unpack-submission str)])
      (evaluate-all 'handin (open-input-text-editor/lines defs) eval)))

  (define (reraise-exn-as-submission-problem thunk)
    (with-handlers ([void (lambda (exn)
			    (error
			     (if (exn? exn)
				 (exn-message exn)
				 (format "~s" exn))))])
      (thunk)))

  ;; ----------------------------------------
  ;;  Auto-test utils

  (define (check-defined e id)
    (with-handlers ([exn:fail:syntax? void]
		    [exn:fail:contract:variable?
		     (lambda (x)
		       (error
			(format
			 "\"~a\" is not defined, but it must be defined for handin"
			 (exn:fail:contract:variable-id x))))])
      (e #`(#,namespace-variable-value '#,id #t))))

  (define (mk-args args)
    (let loop ([l args])
      (if (null? l)
	  ""
	  (string-append " " (format "~e" (car l)) (loop (cdr l))))))

  (define test-history-enabled (make-parameter #f))
  (define test-history (make-parameter null))
  
  (define (format-history one-test)
    (if (test-history-enabled)
	(format "(begin~a)"
		(apply string-append
		       (map (lambda (s)
			      (format " ~a" s))
			    (reverse (test-history)))))
	one-test))

  (define (check-proc e result equal? f . args)
    (let ([test (format "(~a~a)" f (mk-args args))])
      (when (test-history-enabled)
	(test-history (cons test (test-history))))
      (current-run-status (format "running instructor-supplied test ~a" 
				  (format-history test)))
      (let-values ([(ok? val)
		    (with-handlers ([void
				     (lambda (x)
				       (error
					(format "instructor-supplied test ~a failed with an error: ~e"
						(format-history test)
						(exn-message x))))])
		      (let ([val (e `(,f ,@(map value-converter args)))])
			(values (or (eq? 'anything result)
				    (equal? val result))
				val)))])
	(unless ok?
	  (error
	   (format "instructor-supplied test ~a should have produced ~e, instead produced ~e"
		   (format-history test)
		   result
		   val)))
	val)))

  (define (user-construct e func . args)
    (apply check-proc e func 'anything eq? args))

  (define (look-for-tests t name count)
    (let ([p (open-input-text-editor/lines t)])
      (let loop ([found 0])
	(let ([e (read p)])
	  (if (eof-object? e)
	      (when (found . < . count)
		(error (format "found ~a test~a for ~a, need at least ~a test~a"
			       found
			       (if (= found 1) "" "s")
			       name
			       count
			       (if (= count 1) "" "s"))))
	      (loop (+ found
		       (if (and (pair? e)
				(eq? (car e) name))
			   1
			   0))))))))

  (define list-abbreviation-enabled (make-parameter #f))

  (define (value-converter v)
    (parameterize ([pc:booleans-as-true/false #t]
		   [pc:abbreviate-cons-as-list (list-abbreviation-enabled)]
		   [pc:constructor-style-printing #t])
      (pc:print-convert v)))

  (define (default-value-printer v)
    (parameterize ([pretty-print-show-inexactness #t]
		   [pretty-print-.-symbol-without-bars #t]
		   [pretty-print-exact-as-decimal #t]
		   [pretty-print-columns +inf.0]
		   [read-case-sensitive #t])
      (let ([p (open-output-string)])
	(pretty-print (value-converter v) p)
	(regexp-replace #rx"\n$" (get-output-string p) ""))))
  (define current-value-printer (make-parameter default-value-printer))

  (define (call-with-evaluator lang teachpacks program-port go)
    (parameterize ([error-value->string-handler (lambda (v s)
						  ((current-value-printer) v))]
		   [list-abbreviation-enabled (not (or (eq? lang 'beginner)
						       (eq? lang 'beginner-abbr)))])
      (reraise-exn-as-submission-problem
       (lambda ()
	 (let ([e (make-evaluator lang teachpacks program-port)])
	   (current-run-status "executing your code")
	   (go e))))))

  (define (call-with-evaluator/submission lang teachpacks str go)
    (let-values ([(defs interacts) (unpack-submission str)])
      (call-with-evaluator lang teachpacks (open-input-text-editor/lines defs) go)))

  )
