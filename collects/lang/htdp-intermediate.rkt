
(module htdp-intermediate scheme/base
  (require "private/teach.ss"
           "private/teachprims.ss"
	   "private/teach-module-begin.ss"
	   mzlib/etc
	   mzlib/list
	   syntax/docprovide
           test-engine/scheme-tests)

  ;; syntax:
  (provide (rename-out 
            [intermediate-define define]
            [intermediate-define-struct define-struct]
            [intermediate-pre-lambda lambda]
            [intermediate-app #%app]
            [beginner-top #%top]
            [intermediate-local local]
            [intermediate-let let]
            [intermediate-let* let*]
            [intermediate-letrec letrec]
            ;; [intermediate-recur recur]
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
            [intermediate-module-begin #%module-begin]
            )
           check-expect
           check-within
           check-error
           check-member-of
           check-range
	   #%datum
           #%top-interaction
	   empty true false

; 	   signature : -> mixed one-of predicate combined
; 	   Number Real Rational Integer Natural Boolean True False String Symbol Char Empty-list Any
; 	   cons-of
; 	   Property 
; 	   check-property for-all ==> expect expect-within expect-member-of expect-range
	   )

  ;; procedures:
  (provide-and-document
   procedures
   (all-from beginner: lang/private/intermediate-funs procedures))
  (provide
   (rename-out
     [beginner-list? list?]))
  )
