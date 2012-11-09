#lang racket/base
(require "../utils/utils.rkt")

(require (rep type-rep rep-utils free-variance)
         (env type-name-env)
         (utils tc-utils)
         (types utils)
         racket/match
         racket/contract)

(provide resolve-name resolve-app needs-resolving?
         resolve resolve-app-check-error)
(provide/cond-contract [resolve-once (Type/c . -> . (or/c Type/c #f))])

(define-struct poly (name vars) #:prefab)

(define (resolve-name t)
  (match t
    [(Name: n) (let ([t (lookup-type-name n)])
                 (if (Type/c t) t #f))]
    [_ (int-err "resolve-name: not a name ~a" t)]))

(define already-resolving? (make-parameter #f))


(define (resolve-app-check-error rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Poly-unsafe: n _)
       (unless (= n (length rands))
         (tc-error "wrong number of arguments to polymorphic type: expected ~a and got ~a"
                   n (length rands)))]
      [(Name: n)
       (when (and (current-poly-struct)
                  (free-identifier=? n (poly-name (current-poly-struct)))
                  (not (or (ormap Error? rands)
                           (andmap type-equal? rands (poly-vars (current-poly-struct))))))
         (tc-error "Structure type constructor ~a applied to non-regular arguments ~a" rator rands))]
      [(Mu: _ _) (void)]
      [(App: _ _ _) (void)]
      [(Error:) (void)]
      [_ (tc-error/delayed "Type ~a cannot be applied, arguments were: ~a" rator rands)])))


(define (resolve-app rator rands stx)
  (parameterize ([current-orig-stx stx]
                 [already-resolving? #t])
    (resolve-app-check-error rator rands stx)
    (match rator
      [(Name: _)
       (let ([r (resolve-name rator)])
         (and r (resolve-app r rands stx)))]
      [(Poly: _ _) (instantiate-poly rator rands)]
      [(Mu: _ _) (resolve-app (unfold rator) rands stx)]
      [(App: r r* s) (resolve-app (resolve-app r r* s) rands stx)]
      [_ (tc-error "cannot apply a non-polymorphic type: ~a" rator)])))


(define (needs-resolving? t)
  (or (Mu? t) (App? t) (Name? t)))

(define resolver-cache (make-hasheq))

(define (resolve-once t)
  (define seq (Rep-seq t))
  (define r (hash-ref resolver-cache seq #f))
  (or r
      (let ([r* (match t
                  [(Mu: _ _) (unfold t)]
                  [(App: r r* s)
                   (resolve-app r r* s)]
                  [(Name: _) (resolve-name t)])])
        (when r*
          (hash-set! resolver-cache seq r*))
        r*)))

(define (resolve t)
  (if (needs-resolving? t) (resolve-once t) t))

;(trace resolve-app)
