
(module htdp-advanced scheme/base
  (require "private/teach.ss"
	   "private/teachprims.ss"
	   "private/teach-module-begin.ss"
	   mzlib/etc
	   mzlib/list
	   mzlib/pretty
	   syntax/docprovide
           scheme/promise
           test-engine/scheme-tests
	   "posn.ss")

  ;; syntax:
  (provide (rename-out
            [advanced-define define]
            [advanced-define-struct define-struct]
            [advanced-define-datatype define-datatype]
            [advanced-lambda lambda]
            [advanced-lambda λ]
            [advanced-app #%app]
            [beginner-top #%top]
            [intermediate-local local]
            [advanced-let let]
            [intermediate-let* let*]
            [intermediate-letrec letrec]
            [advanced-recur recur]
            [beginner-cond cond]
            [beginner-else else]
            [beginner-if if]
            [beginner-and and]
            [beginner-or or]
            [beginner-require require]
            [beginner-dots ..]
            [beginner-dots ...]
            [beginner-dots ....]
            [beginner-dots .....]
            [beginner-dots ......]
            [intermediate-quote quote]
            [intermediate-quasiquote quasiquote]
            [intermediate-unquote unquote]
            [intermediate-unquote-splicing unquote-splicing]
            [intermediate-time time]
            [advanced-begin begin]
            [advanced-begin0 begin0]
            [advanced-shared shared]
            [advanced-set! set!]
            [advanced-when when]
            [advanced-unless unless]
            [advanced-case case]
            [advanced-match match]
            [advanced-delay delay]
            [advanced-module-begin #%module-begin]
            )
           check-expect
           check-within
           check-error
           check-member-of
           check-range
	   #%datum
           #%top-interaction
	   empty true false

	   signature : -> mixed one-of predicate combined
	   Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list Any
	   cons-of
	   Property
	   check-property for-all ==> expect expect-within expect-member-of expect-range)

  ;; procedures:
  (provide-and-document
   procedures

   (all-from-except intermediate: lang/htdp-intermediate-lambda procedures
		    cons list* append random)
   (all-from advanced: lang/private/advanced-funs procedures))
  )
