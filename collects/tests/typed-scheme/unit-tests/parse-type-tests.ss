#lang scheme/base
(require "test-utils.ss" "planet-requires.ss" (for-syntax scheme/base))
(require (utils tc-utils)
	 (env type-alias-env type-environments type-name-env init-envs)
	 (rep type-rep)
	 (private type-comparison parse-type subtype
		  union type-utils)
         (schemeunit))

(require (rename-in (private type-effect-convenience) [-> t:->])
         (except-in (private base-types) Un)
         (for-template (private base-types)))

(provide parse-type-tests)

;; HORRIBLE HACK!
;; We are solving the following problem:
;; when we require "base-env.ss" for template, it constructs the type-alias-env
;; in phase 0 (relative to this module), but populates it with phase -1 identifiers
;; The identifiers are also bound in this module at phase -1, but the comparison for
;; the table is phase 0, so they don't compare correctly

;; The solution is to add the identifiers to the table at phase 0.
;; We do this by going through the table, constructing new identifiers based on the symbol of the old identifier.
;; This relies on the identifiers being bound at phase 0 in this module (which they are, because we have a
;; phase 0 require of "base-env.ss").
(for ([pr (type-alias-env-map cons)])
  (let ([nm (car pr)]
        [ty (cdr pr)])
    (register-resolved-type-alias (datum->syntax #'here (syntax->datum nm)) ty)))

(define-syntax (run-one stx)
  (syntax-case stx ()
    [(_ ty) (syntax/loc stx
              (parameterize ([current-tvars initial-tvar-env]
                             [current-orig-stx #'ty]
                             [orig-module-stx #'ty]
                             [expanded-module-stx #'ty]
                             [delay-errors? #f])
                (parse-type (syntax ty))))]))

(define-syntax (pt-test stx)
  (syntax-case stx ()
    [(_ ts tv) (syntax/loc stx (pt-test ts tv initial-tvar-env))]
    [(_ ty-stx ty-val tvar-env)
     (quasisyntax/loc
         stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
                  (parameterize ([current-tvars tvar-env]
                                 [delay-errors? #f])
                    (check type-equal? (parse-type (quote-syntax ty-stx)) ty-val))))]))

(define-syntax pt-tests
  (syntax-rules ()
    [(_ nm [elems ...] ...)
     (test-suite nm
                 (pt-test elems ...) ...)]))

(define (parse-type-tests)  
  (pt-tests
   "parse-type tests" 
   [Number N]
   [Any Univ]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(Listof Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(pred Number) (make-pred-ty N)]
   [(values Number Boolean Number) (-values (list N B N))]
   [(Number -> Number) (t:-> N N)]
   [(Number -> Number) (t:-> N N)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . ->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(U Number Boolean) (Un N B)]
   [(U Number Boolean Number) (Un N B)]
   [(U Number Boolean 1) (Un N B)]
   [(All (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(All (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(∀ (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   
   [(Listof Number) (make-Listof  N)]
   
   [a (-v a) (extend-env (list 'a) (list (-v a))
                            initial-tvar-env)]
   
   ))


(define-go
  parse-type-tests)



