#lang scheme/base
(require "test-utils.ss"
         (for-syntax scheme/base)
         (private type-annotation parse-type base-types)
         (types convenience utils)
	 (env type-environments type-name-env init-envs)
	 (utils tc-utils)
	 (rep type-rep)
         schemeunit)

(provide type-annotation-tests)

(define-syntax-rule (tat ann-stx ty)
  (check-tc-result-equal? (format "~a" (quote ann-stx))  
                          (type-ascription (let ([ons (current-namespace)]
                                                 [ns (make-empty-namespace)])
                                             (parameterize ([current-namespace ns])
                                               (namespace-attach-module ons 'scheme/base ns)
                                               (namespace-require 'scheme/base)
                                               (namespace-require 'typed-scheme/private/prims)
                                               (namespace-require 'typed-scheme/private/base-types)
                                               (namespace-require 'typed-scheme/private/base-types-extra)
                                               (expand 'ann-stx))))
                          ty))

#reader typed-scheme/typed-reader
(define (type-annotation-tests)
  (test-suite 
   "Type Annotation tests"
   ;; FIXME - ask Ryan
   ;(tat (ann foo : Number) (ret -Number))
   (tat foo #f)
   (tat (ann foo : 3) (ret (-val 3) (make-NoFilter) (make-NoObject)))))

(define-go
  type-annotation-tests)

