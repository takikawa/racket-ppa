#lang racket/base
(require "../utils/utils.rkt")

(require (rep type-rep rep-utils)
         (env type-name-env)
         (utils tc-utils)
         (types utils current-seen)
         racket/match
         (contract-req)
         racket/format)

(provide resolve-name resolve-app needs-resolving?
         resolve resolve-app-check-error
         resolver-cache-remove!
         current-check-polymorphic-recursion)
(provide/cond-contract [resolve-once (Type/c . -> . (or/c Type/c #f))])

(define-struct poly (name vars) #:prefab)

;; (Parameter (Option Poly-Rec-Info))
;; This parameter controls whether or not the resolving process
;; should check for polymorphic recursion in implicit recursive
;; type names. This should only need to be enabled at type alias
;; definition time.
;;
;; If not #f, it should be a record of a procedure that checks if an
;; alias is in the same connected component as the original alias
;; and a list of symbols that correspond to the type parameters of
;; the type being parsed.
(define current-check-polymorphic-recursion (make-parameter #f))

(define (resolve-name t)
  (match t
    [(Name/simple: n) (let ([t (lookup-type-name n)])
                        (if (Type/c? t) t #f))]
    [_ (int-err "resolve-name: not a name ~a" t)]))

(define already-resolving? (make-parameter #f))


(define (resolve-app-check-error rator rands stx)
  (parameterize ([current-orig-stx stx])
    (match rator
      [(Poly-unsafe: n _)
       (unless (= n (length rands))
         (tc-error (~a "wrong number of arguments to polymorphic type"
                       "\n  type: " rator
                       "\n  expected: " n
                       "\n  given: " (length rands)
                       "\n  arguments...: " rands)))]
      [(Name/struct: n)
       (when (and (current-poly-struct)
                  (free-identifier=? n (poly-name (current-poly-struct))))
        (define num-rands (length rands))
        (define num-poly (length (poly-vars (current-poly-struct))))
        ;; check arity of constructor first
        (if (= num-rands num-poly)
            (when (not (or (ormap Error? rands)
                           (andmap type-equal? rands
                                   (poly-vars (current-poly-struct)))))
                  (tc-error (~a "structure type constructor applied to non-regular arguments"
                                "\n  type: " rator
                                "\n  arguments...: " rands)))
            (tc-error (~a "wrong number of arguments to structure type constructor"
                          "\n  type: " rator
                          "\n  expected: " num-poly
                          "\n  given: " num-rands
                          "\n  arguments...: " rands))))]
      [(Name: name-id num-args #f)
       (cond [(> num-args 0)
              (define num-rands (length rands))
              (unless (= num-rands num-args)
                (tc-error (~a "wrong number of arguments to polymorphic type"
                              "\n  type: " rator
                              "\n  expected: " num-args
                              "\n  given: " num-rands
                              "\n  arguments...: " rands)))
              ;; Does not allow polymorphic recursion since both type
              ;; inference and equirecursive subtyping for polymorphic
              ;; recursion are difficult.
              ;;
              ;; Type inference is known to be undecidable in general, but
              ;; practical algorithms do exist[1] that do not diverge in
              ;; practice.
              ;;
              ;; It is possible that equirecursive subtyping with polymorphic
              ;; recursion is as difficult as equivalence of DPDAs[2], which is
              ;; known to be decidable[3], but good algorithms may not exist.
              ;;
              ;; [1] Fritz Henglein. "Type inference with polymorphic recursion"
              ;;     TOPLAS 1993
              ;; [2] Marvin Solomon. "Type definitions with parameters"
              ;;     POPL 1978
              ;; [3] Geraud Senizergues.
              ;;     "L(A)=L(B)? decidability results from complete formal systems"
              ;;     TCS 2001.
              ;;
              ;; check-argument : Type Id -> Void
              ;; Check argument to make sure there's no polymorphic recursion
              (define (check-argument given-type arg-name)
                (define ok?
                  (or (F? given-type)
                      (not (member (syntax-e arg-name) (fv given-type)))))
                (unless ok?
                  (tc-error (~a "recursive type cannot be applied at a"
                                " different type in its recursive invocation"
                                "\n  type: " rator
                                "\n  new argument name: " arg-name
                                "\n  new argument: " given-type
                                "\n  new arguments...: " rands))))
              (match (current-check-polymorphic-recursion)
                [`#s(poly-rec-info ,same-component? ,current-vars)
                 #:when (same-component? name-id)
                 (for* ([rand (in-list rands)]
                        [var (in-list current-vars)])
                   (check-argument rand var))]
                [_ (void)])]
             [else
              (tc-error (~a "type cannot be applied"
                            "\n  type: " rator
                            "\n  arguments...: " rands))])]
      [(Mu: _ _) (void)]
      [(App: _ _ _) (void)]
      [(Error:) (void)]
      [_ (tc-error/delayed (~a "type cannot be applied"
                               "\n  type: " rator
                               "\n  arguments...: " rands))])))


(define (resolve-app rator rands stx)
  (parameterize ([current-orig-stx stx]
                 [already-resolving? #t])
    (resolve-app-check-error rator rands stx)
    (match rator
      [(? Name?)
       (let ([r (resolve-name rator)])
         (and r (resolve-app r rands stx)))]
      [(Poly: _ _) (instantiate-poly rator rands)]
      [(Mu: _ _) (resolve-app (unfold rator) rands stx)]
      [(App: r r* s) (resolve-app (resolve-app r r* s) rands stx)]
      [_ (tc-error (~a "cannot apply a non-polymorphic type"
                       "\n  type: " rator
                       "\n  arguments: " rands))])))


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
                  [(? Name?) (resolve-name t)])])
        (when (and r*
                   (not (currently-subtyping?)))
          (hash-set! resolver-cache seq r*))
        r*)))

;; resolver-cache-remove! : (Listof Type) -> Void
;; Removes the given types from the resolver cache. This is
;; only used by recursive type alias set-up, which sometimes needs to
;; undo certain resolutions.
(define (resolver-cache-remove! keys)
  (for ([key (in-list keys)])
    (hash-remove! resolver-cache (Rep-seq key))))

;; Repeatedly unfolds Mu, App, and Name constructors until the top type
;; constructor is not one of them.
;; Type/c? -> Type/c?
(define (resolve t)
  (let loop ((t t))
    (if (needs-resolving? t)
        (loop (resolve-once t))
        t)))

;(trace resolve-app)
