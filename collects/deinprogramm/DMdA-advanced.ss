#lang deinprogramm/DMdA

(require syntax/docprovide)
(provide #%app #%top (rename-out (DMdA-module-begin #%module-begin)) #%datum #%top-interaction require lib planet
         let let* letrec
	 (rename-out (DMdA-advanced-lambda lambda))
	 (rename-out (DMdA-advanced-define define))
	 cond if else begin and or set! quote
         define-record-procedures define-record-procedures-2 
	 define-record-procedures-parametric define-record-procedures-parametric-2
         .. ... .... ..... ......
	 check-expect check-within check-error
	 check-property for-all ==> expect expect-within
	 contract : define-contract -> mixed one-of predicate combined property
	 number real rational integer natural boolean true false string symbol empty-list unspecific
	 chocolate-cookie)
(provide cons)
(provide-and-document
 procedures
 (all-from advanced: deinprogramm/DMdA procedures))

