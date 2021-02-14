#lang racket/base

(require syntax/docprovide "deflam.rkt"
         deinprogramm/sdp/private/module-begin
         (except-in deinprogramm/sdp/private/primitives lambda λ define))
(provide #%app #%top (rename-out (advanced-module-begin #%module-begin)) #%datum #%top-interaction 
	 require lib planet provide
         let let* letrec lambda λ define
	 cond if else and or quote
         define-record define-record-functions
	 match
         .. ... .... ..... ......
	 check-expect check-within check-error check-member-of check-range check-satisfied
	 check-property for-all ==> expect expect-within expect-member-of expect-range
	 signature : -> mixed one-of enum predicate combined list-of nonempty-list-of
	 number real rational integer integer-from-to natural
         boolean true false string symbol empty-list any property)
(provide-and-document
 procedures
 (all-from advanced: deinprogramm/sdp/private/primitives procedures))

