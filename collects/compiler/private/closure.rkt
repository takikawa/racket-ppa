;; collect closure-making expressions
;; (c) 1996-1997 Sebastian Good
;; (c) 1997-2011 PLT Scheme, Inc

;; Closure-making expressions, such as lambda, are
;;  replaced with explicit make-closure AST nodes.
;; Closures that with empty free-variable sets are replaced
;;  with varrefs to a one-time created global closure. These
;;  create-once closures are collected into a list for
;;  special handling.
;; All closure-making AST nodes, which were replaced with
;;  make-closure nodes, are collected into the list
;;  compiler:closure-list.

;;; Annotatitons: ----------------------------------------------
;;    <no annotation changes>
;;; ------------------------------------------------------------

(module closure mzscheme
  (require mzlib/unit
           mzlib/list
           mzlib/etc)

  (require syntax/zodiac-sig)

  (require "sig.rkt"
           "../sig.rkt")

  (provide closure@)
  (define-unit closure@
      (import (prefix compiler:option: compiler:option^)
	      compiler:library^
	      compiler:cstructs^
	      (prefix zodiac: zodiac^)
	      compiler:zlayer^
	      compiler:const^
	      compiler:driver^)
      (export compiler:closure^)

      (define compiler:closure-list null)
      (define compiler:add-closure!
	(lambda (l)
	  (set! compiler:closure-list
		(cons l compiler:closure-list))))
      (define (compiler:get-closure-list) compiler:closure-list)

      (define-struct (top-level-varref/bind-from-lift zodiac:top-level-varref) (lambda pls?))

      ;; fully lifted lambdas (i.e., really static, not per-load)
      (define compiler:lifted-lambda-vars null) 
      (define compiler:lifted-lambdas null) 
      ;; one-time closure-creation to be performed per-load
      (define compiler:once-closures-list null)
      (define compiler:once-closures-globals-list null)

      (define (compiler:get-lifted-lambda-vars) compiler:lifted-lambda-vars) 
      (define (compiler:get-lifted-lambdas) compiler:lifted-lambdas) 
      (define (compiler:get-once-closures-list) compiler:once-closures-list)
      (define (compiler:get-once-closures-globals-list) compiler:once-closures-globals-list)

      (define compiler:add-lifted-lambda!
	(lambda (lam pls?)
	  ;; Set the closure's liftable field to a new top-level-varref
	  (let* ([code (get-annotation lam)]
		 [var (gensym (if pls? 
				  'pllifted
				  'lifted))]
		 [sv (make-top-level-varref/bind-from-lift
		      (zodiac:zodiac-stx lam)
		      (make-empty-box)
		      var
		      #f
		      (box '())
		      #f
		      #f
		      #f
		      lam
		      pls?)]
		 [def (zodiac:make-define-values-form 
		       (zodiac:zodiac-stx lam)
		       (make-empty-box)
		       (list sv) lam)])
	    (set-annotation! sv (varref:empty-attributes))
	    (varref:add-attribute! sv varref:static)

	    ;; Set the procedure annoation's `liftable' field to a list
	    ;;  cotaining the sv, which indicates that it was just convrted;
	    ;;  (list sv) is changed to sv by a pass in "lift.rkt".
	    (set-procedure-code-liftable! code (list sv))

	    (if pls?
		(begin
		  (varref:add-attribute! sv varref:per-load-static)
		  (compiler:add-per-load-static-list! var)
		  (set! compiler:once-closures-list (cons def compiler:once-closures-list))
		  (set! compiler:once-closures-globals-list (cons (code-global-vars code) compiler:once-closures-globals-list)))
		(begin
		  (set! compiler:lifted-lambda-vars (cons sv compiler:lifted-lambda-vars))
		  (set! compiler:lifted-lambdas (cons def compiler:lifted-lambdas)))))))
      
      (define (compiler:init-once-closure-lists!)
	(set! compiler:once-closures-list null)
	(set! compiler:once-closures-globals-list null))
      
      (define (compiler:init-closure-lists!)
	(set! compiler:closure-list null)
	(set! compiler:lifted-lambda-vars null)
	(compiler:init-once-closure-lists!)
	(compiler:init-lifted-lambda-list!))
      
      (define (compiler:init-lifted-lambda-list!)
	(set! compiler:lifted-lambdas null))

      (define closure-expression!
	(letrec
	    ([transform-closure!
	      (lambda (ast args)
		(compiler:add-closure! ast)
		(let* ([code (get-annotation ast)]
		       [name (closure-code-name code)]
		       [free (code-free-vars code)]
		       [mk-closure (make-compiler:make-closure 
				    (zodiac:zodiac-stx ast)
				    ast free args
				    name)])
		  mk-closure))]
	     [transform!
	      (lambda (ast)
		(cond
		 
		 ;;------------------------------------------------------------------
		 ;; CONSTANTS
		 ;;
		 [(zodiac:quote-form? ast) ast]
		 
		 ;;------------------------------------------------------------------
		 ;; VARIABLE REFERENCES
		 ;;
		 [(zodiac:varref? ast) ast]

		 ;;------------------------------------------------------------------
		 ;; LAMBDA EXPRESSIONS
		 ;;
		 ;; We turn this into a make-closure form and catalogue the code body
		 ;; we also decide which vehicle in which to put the body
		 ;;
		 [(zodiac:case-lambda-form? ast)
		  (zodiac:set-case-lambda-form-bodies! ast 
						       (map (lambda (body)
							      (transform! body))
							    (zodiac:case-lambda-form-bodies ast)))
		  (transform-closure! ast null)]

		 ;;------------------------------------------------------------------
		 ;; LET EXPRESSIONS
		 ;;
		 [(zodiac:let-values-form? ast)
		  (zodiac:set-let-values-form-vals! 
		   ast (map transform! (zodiac:let-values-form-vals ast)))
		  (zodiac:set-let-values-form-body! 
		   ast (transform! (zodiac:let-values-form-body ast)))
		  ast]

		 [(zodiac:letrec-values-form? ast)
		  (zodiac:set-letrec-values-form-vals! 
		   ast 
		   (map transform! (zodiac:letrec-values-form-vals ast)))
		  (zodiac:set-letrec-values-form-body! 
		   ast (transform! (zodiac:letrec-values-form-body ast)))
		  ast]
		 
		 ;;-----------------------------------------------------------------
		 ;; IF EXPRESSIONS
		 ;;
		 [(zodiac:if-form? ast)
		  (zodiac:set-if-form-test! ast (transform! (zodiac:if-form-test ast)))
		  (zodiac:set-if-form-then! ast (transform! (zodiac:if-form-then ast)))
		  (zodiac:set-if-form-else! ast (transform! (zodiac:if-form-else ast)))
		  ast]

		 ;;------------------------------------------------------------------
		 ;; BEGIN EXPRESSIONS
		 ;;
		 [(zodiac:begin-form? ast)
                  (zodiac:set-begin-form-bodies!
                   ast
                   (map transform! (zodiac:begin-form-bodies ast)))
		  ast]
		 
		 ;;------------------------------------------------------------------
		 ;; BEGIN0 EXPRESSIONS
		 ;;
		 [(zodiac:begin0-form? ast)
		  (zodiac:set-begin0-form-first!
		   ast (transform! (zodiac:begin0-form-first ast)))
		  (zodiac:set-begin0-form-rest!
		   ast (transform! (zodiac:begin0-form-rest ast)))
		  ast]
		 
		 ;;------------------------------------------------------------------
		 ;; DEFINE/SET! EXPRESSIONS
		 ;;
		 [(zodiac:set!-form? ast)
		  (zodiac:set-set!-form-val! ast (transform! (zodiac:set!-form-val ast)))
		  ast]
		 
		 [(zodiac:define-values-form? ast)
		  (zodiac:set-define-values-form-val! 
		   ast 
		   (transform! (zodiac:define-values-form-val ast)))
		  ast]
		 
		 ;;------------------------------------------------------------------
		 ;; DEFINE-SYNTAX
		 ;;
		 [(zodiac:define-syntaxes-form? ast)
		  (zodiac:set-define-syntaxes-form-expr! 
		   ast 
		   (transform! (zodiac:define-syntaxes-form-expr ast)))
		  ast]
		 
		 ;;-----------------------------------------------------------------
		 ;; APPLICATIONS
		 ;;
		 ;; Now we should be applying closures to arguments.  The actual
		 ;; extraction of code and environment parts will happen in the 
		 ;; vm translation
		 ;;
		 [(zodiac:app? ast)
		  (zodiac:set-app-fun! ast (transform! (zodiac:app-fun ast)))
		  (zodiac:set-app-args! ast (map transform! (zodiac:app-args ast)))
		  ast]
		 
		 ;;-------------------------------------------------------------------
		 ;; WITH-CONTINUATION-MARK
		 ;;
		 ;;
		 [(zodiac:with-continuation-mark-form? ast)
		  
		  (zodiac:set-with-continuation-mark-form-key!
		   ast
		   (transform! (zodiac:with-continuation-mark-form-key ast)))
		  
		  (zodiac:set-with-continuation-mark-form-val!
		   ast
		   (transform! (zodiac:with-continuation-mark-form-val ast)))
		  
		  (zodiac:set-with-continuation-mark-form-body!
		   ast
		   (transform! (zodiac:with-continuation-mark-form-body ast)))
		  
		  ast]

		 ;;-----------------------------------------------------------
		 ;; GLOBALS
		 ;;
		 [(zodiac:global-prepare? ast)
		  (zodiac:set-global-prepare-vec!
		   ast
		   (transform! (zodiac:global-prepare-vec ast)))
		  ast]
		 [(zodiac:global-lookup? ast)
		  (zodiac:set-global-lookup-vec!
		   ast
		   (transform! (zodiac:global-lookup-vec ast)))
		  ast]
		 [(zodiac:global-assign? ast)
		  (zodiac:set-global-assign-vec!
		   ast
		   (transform! (zodiac:global-assign-vec ast)))
		  (zodiac:set-global-assign-expr!
		   ast
		   (transform! (zodiac:global-assign-expr ast)))
		  ast]
		 [(zodiac:safe-vector-ref? ast)
		  (zodiac:set-safe-vector-ref-vec!
		   ast
		   (transform! (zodiac:safe-vector-ref-vec ast)))
		  ast]
		 
		 [else (compiler:internal-error 
			ast 
			(format
			 "closure-expression: form not supported: ~a" ast))]))])
	  (lambda (ast) (transform! ast))))))
