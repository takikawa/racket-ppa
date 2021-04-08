#lang racket/base

(require syntax/docprovide
         deinprogramm/sdp/private/module-begin
         deinprogramm/sdp/private/primitives)
(provide #%app #%top (rename-out (vanilla-module-begin #%module-begin)) #%datum #%top-interaction
	 require lib planet provide
	 define let let* letrec lambda λ cond if else and or
	 define-record define-record-functions
	 match
	 .. ... .... ..... ......
	 check-expect check-within check-error check-member-of check-range check-satisfied
	 check-property for-all ==> expect expect-within expect-member-of expect-range
	 signature : -> mixed one-of enum predicate combined list-of nonempty-list-of
	 number real rational integer integer-from-to
         natural boolean true false string empty-list any property)
(provide-and-document
 procedures
 (all-from-except vanilla: deinprogramm/sdp/private/primitives procedures
		  quote eq? equal?
		  set!
		  symbol? symbol=? string->symbol symbol->string
		  apply))

