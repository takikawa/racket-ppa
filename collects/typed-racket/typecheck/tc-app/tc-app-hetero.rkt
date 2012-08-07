#lang racket/unit

(require (rename-in "../../utils/utils.rkt" [infer r:infer])
         "../signatures.rkt" "../tc-metafunctions.rkt" "../check-below.rkt"
         "../tc-app-helper.rkt" "../find-annotation.rkt" "../tc-funapp.rkt"
         "../tc-subst.rkt" (prefix-in c: racket/contract)
         syntax/parse racket/match racket/trace scheme/list
         unstable/sequence  unstable/list
         ;; fixme - don't need to be bound in this phase - only to make tests work
         scheme/bool
         racket/unsafe/ops
         (only-in racket/private/class-internal do-make-object)
         (only-in syntax/location module-name-fixup)
         (only-in '#%kernel [apply k:apply] [reverse k:reverse])
         ;; end fixme
         (for-syntax syntax/parse scheme/base (utils tc-utils))
         (private type-annotation)
         (types utils abbrev union subtype resolve convenience type-table substitute generalize)
         (utils tc-utils)
         (only-in srfi/1 alist-delete)
         (except-in (env type-env-structs tvar-env index-env) extend)
         (rep type-rep filter-rep object-rep rep-utils)
         (r:infer infer)
         '#%paramz
         (for-template
          racket/unsafe/ops racket/fixnum racket/flonum
          (only-in '#%kernel [apply k:apply] [reverse k:reverse])
          "../internal-forms.rkt" scheme/base scheme/bool '#%paramz
          (only-in racket/private/class-internal do-make-object)
          (only-in syntax/location module-name-fixup)))

(import tc-expr^ tc-lambda^ tc-let^ tc-apply^ tc-app^)
(export tc-app-hetero^)




(define (tc/index expr)
  (syntax-parse expr
   [((~literal quote) i:number)
    (let ((type (tc-literal #'i)))
      (add-typeof-expr expr type)
      (values type (syntax-e #'i)))]
   [_
    (match (tc-expr expr)
     [(and type (tc-result1: (Value: (? number? i))))
      (values type i)]
     [type (values type #f)])]))

(define (index-error i-val i-bound expr type expected name)
  (define return (or expected (ret (Un))))
  (cond 
    [(not (and (integer? i-val) (exact? i-val)))
     (tc-error/expr #:stx expr #:return return "expected exact integer for ~a index, but got ~a" name i-val)]
    [(< i-val 0)
     (tc-error/expr #:stx expr #:return return "index ~a too small for ~a ~a" i-val name type)]
    [(not (< i-val i-bound))
     (tc-error/expr #:stx expr #:return return "index ~a too large for ~a ~a" i-val name type)]))

(define (valid-index? i bound)
 (and (integer? i) (exact? i) (<= 0 i (sub1 bound))))


;; FIXME - Do something with paths in the case that a structure/vector is not mutable
(define (tc/hetero-ref i-e es-t vec-t expected name)
  (define-values (i-t i-val) (tc/index i-e))
  (define i-bound (length es-t))
  (cond
    [(valid-index? i-val i-bound)
     (cond-check-below (ret (list-ref es-t i-val)) expected)]
    [(not i-val)
     (check-below i-t -Integer)
     (cond-check-below (ret (apply Un es-t)) expected)]
    [else
     (index-error i-val i-bound i-e vec-t expected name)]))

(define (tc/hetero-set! i-e es-t val-e vec-t expected name)
  (define-values (i-t i-val) (tc/index i-e))
  (define i-bound (length es-t))
  (cond 
    [(valid-index? i-val i-bound)
     (tc-expr/check val-e (ret (list-ref es-t i-val)))
     (cond-check-below (ret -Void) expected)]
    [(not i-val)
     (single-value val-e)
     (tc-error/expr
       #:stx i-e #:return (or expected (ret -Void))
       "expected statically known index for ~a mutation, but got ~a"
       name (match i-t [(tc-result1: t) t]))]
    [else
     (single-value val-e)
     (index-error i-val i-bound i-e vec-t expected) name]))


(define-syntax-class special-op
  (pattern i:identifier
           #:when (or (syntax-property #'i 'type-inst)
                      (syntax-property #'i 'type-ascription))))

(define (tc/app-hetero form expected)
  (syntax-parse form
    #:literals (#%plain-app 
                vector-ref unsafe-vector-ref unsafe-vector*-ref
                vector-set! unsafe-vector-set! unsafe-vector*-set!
                unsafe-struct-ref unsafe-struct*-ref
                unsafe-struct-set! unsafe-struct*-set!
                vector-immutable vector)
    [(#%plain-app op:special-op args ...) #f]
    ;; unsafe struct-ref 
    [(#%plain-app (~or unsafe-struct-ref unsafe-struct*-ref) struct:expr index:expr)
     (match (single-value #'struct)
       [(tc-result1: (and struct-t (app resolve (Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _ _))))
        (tc/hetero-ref #'index flds struct-t expected "struct")]
       [s-ty #f])]
    ;; vector-ref on het vectors
    [(#%plain-app (~or vector-ref unsafe-vector-ref unsafe-vector*-ref) vec:expr index:expr)
     (match (single-value #'vec)
       [(tc-result1: (and vec-t (app resolve (HeterogenousVector: es))))
        (tc/hetero-ref #'index es vec-t expected "vector")]
       [v-ty #f])]
    ;; unsafe struct-set! 
    [(#%plain-app (~or unsafe-struct-set! unsafe-struct*-set!) s:expr index:expr val:expr)
     (match (single-value #'s)
       [(tc-result1: (and struct-t (app resolve (Struct: _ _ (list (fld: flds _ _) ...) _ _ _ _ _))))
        (tc/hetero-set! #'index flds #'val struct-t expected "struct")]
       [s-ty #f])]
    ;; vector-set! on het vectors
    [(#%plain-app (~or vector-set! unsafe-vector-set! unsafe-vector*-set!) v:expr index:expr val:expr)
     (match (single-value #'v)
       [(tc-result1: (and vec-t (app resolve (HeterogenousVector: es))))
        (tc/hetero-set! #'index es #'val vec-t expected "vector")]
       [v-ty #f])]
    [(#%plain-app (~or vector-immutable vector) args:expr ...)
     (match expected
       [(tc-result1: (app resolve (Vector: t))) #f]
       [(tc-result1: (app resolve (HeterogenousVector: ts)))
        (unless (= (length ts) (length (syntax->list #'(args ...))))
          (tc-error/expr "expected vector with ~a elements, but got ~a"
                         (length ts)
                         (make-HeterogenousVector (map tc-expr/t (syntax->list #'(args ...))))))
        (for ([e (in-list (syntax->list #'(args ...)))]
              [t (in-list ts)])
          (tc-expr/check e (ret t)))
        expected]
       ;; If the expected type is a union, then we examine just the parts
       ;; of the union that are vectors.  If there's only one of those,
       ;; we re-run this whole algorithm with that.  Otherwise, we treat
       ;; it like any other expected type.
       [(tc-result1: (app resolve (Union: ts))) (=> continue)
        (define u-ts (for/list ([t (in-list ts)]
                                #:when (eq? 'vector (Type-key t)))
                       t))
        (match u-ts
          [(list t0) (tc/app/check form (ret t0))]
          [_ (continue)])]
       ;; since vectors are mutable, if there is no expected type, we want to generalize the element type
       [(or #f (tc-result1: _))
        (ret (make-HeterogenousVector (map (lambda (x) (generalize (tc-expr/t x)))
                                           (syntax->list #'(args ...)))))]
       [_ (int-err "bad expected: ~a" expected)])]
    [_ #f]))
