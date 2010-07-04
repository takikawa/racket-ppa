
(module htdp-intermediate-lambda mzscheme
  (require "private/teach.ss"
	   "private/contract-forms.ss"
	   (lib "etc.ss")
	   (lib "list.ss")
	   (lib "docprovide.ss" "syntax"))
  
  ;; syntax:
  (provide (rename intermediate-define define)
	   (rename intermediate-define-struct define-struct)
	   (rename intermediate-lambda lambda)
	   (rename advanced-app #%app)
	   (rename beginner-top #%top)
	   (rename intermediate-local local)
	   (rename intermediate-let let)
	   (rename intermediate-let* let*)
	   (rename intermediate-letrec letrec)
	   (rename intermediate-recur recur)
	   (rename beginner-cond cond)
	   (rename beginner-else else)
	   (rename beginner-if if)
	   (rename beginner-and and)
	   (rename beginner-or or)
           (rename beginner-require require)
           (rename beginner-dots ..)
           (rename beginner-dots ...)
           (rename beginner-dots ....)
           (rename beginner-dots .....)
           (rename beginner-dots ......)
	   (rename intermediate-quote quote)
	   (rename intermediate-quasiquote quasiquote)
	   (rename intermediate-unquote unquote)
	   (rename intermediate-unquote-splicing unquote-splicing)
	   (rename intermediate-time time)
	   (rename intermediate-module-begin #%module-begin)
	   ;; (rename intermediate-contract contract)
	   ;; (rename intermediate-define-data define-data)
	   #%datum
           #%top-interaction
	   empty true false)

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: (lib "htdp-intermediate.ss" "lang") procedures)))
