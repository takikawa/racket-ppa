#lang scheme/base

(require "../utils/utils.ss" 
	 syntax/boundmap
         (utils tc-utils)
         (types utils))

(provide register-type register-type-if-undefined
         finish-register-type
         maybe-finish-register-type
         register-type/undefined
         lookup-type
         register-types
         check-all-registered-types
         type-env-map)

;; module-identifier-mapping from id -> type or Box[type]
;; where id is a variable, and type is the type of the variable
;; if the result is a box, then the type has not actually been defined, just registered
(define the-mapping (make-module-identifier-mapping))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type id type)
  (module-identifier-mapping-put! the-mapping id type))

(define (register-type-if-undefined id type)
  (if (module-identifier-mapping-get the-mapping id (lambda _ #f))
      (tc-error/stx id "Duplicate type annotation for ~a" (syntax-e id))
      (register-type id type)))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type/undefined id type)
  ;(printf "register-type/undef ~a~n" (syntax-e id))
  (if (module-identifier-mapping-get the-mapping id (lambda _ #f))
      (tc-error/stx id "Duplicate type annotation for ~a" (syntax-e id))
      (module-identifier-mapping-put! the-mapping id (box type))))

;; add a bunch of types to the mapping
;; listof[id] listof[type] -> void
(define (register-types ids types)
  (for-each register-type ids types))

;; given an identifier, return the type associated with it
;; if none found, calls lookup-fail
;; identifier -> type 
(define (lookup-type id [fail-handler (lambda () (lookup-type-fail id))])
  (let ([v (module-identifier-mapping-get the-mapping id fail-handler)])
    (if (box? v) (unbox v) v)))

(define (maybe-finish-register-type id)
  (let ([v (module-identifier-mapping-get the-mapping id)])
    (if (box? v)
        (register-type id (unbox v))
        #f)))

(define (finish-register-type id)
  (unless (maybe-finish-register-type id)
    (tc-error/stx id "Duplicate defintion for ~a" (syntax-e id))))

(define (check-all-registered-types)
  (module-identifier-mapping-for-each 
   the-mapping 
   (lambda (id e) 
     (when (box? e) (tc-error/stx id "Declaration for ~a provided, but with no definition" (syntax-e id))))))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]  
(define (type-env-map f)
  (module-identifier-mapping-map the-mapping f))
