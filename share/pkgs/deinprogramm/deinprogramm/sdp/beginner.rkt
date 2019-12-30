#lang deinprogramm/sdp

(require syntax/docprovide)
(provide #%app #%top (rename-out (sdp-module-begin #%module-begin)) #%datum #%top-interaction
	 require lib planet provide
         define lambda λ cond if else and or
         define-record-functions
	 match
         .. ... .... ..... ......
	 check-expect check-within check-error check-member-of check-range check-satisfied
	 check-property for-all ==> expect expect-within expect-member-of expect-range
	 signature : -> mixed one-of enum predicate combined
	 number real rational integer natural boolean true false string any property)
(provide-and-document
 procedures
 (all-from-except beginner: deinprogramm/sdp/private/primitives procedures
		  set! eq? equal?
		  quote
		  empty empty? cons cons? first rest
		  length map for-each reverse append list list-ref fold filter
		  symbol? symbol=? string->symbol symbol->string
		  apply))
