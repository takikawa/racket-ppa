#lang deinprogramm/sdp

;; this file exists so there is a single file that exports
;; identifiers named 'define' and 'lambda' that are the 
;; advanced versions of 'define' and 'lambda',
;; so that we can tell scribble about this file and then it
;; can connect up the re-exports to the documentation properly.

(provide (rename-out (sdp-advanced-lambda lambda))
	 (rename-out (sdp-advanced-lambda λ))
	 (rename-out (sdp-advanced-define define)))
