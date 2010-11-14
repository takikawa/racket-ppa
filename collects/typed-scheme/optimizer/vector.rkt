#lang scheme/base

(require syntax/parse
         racket/match
         (for-template scheme/base racket/flonum scheme/unsafe/ops)
         "../utils/utils.rkt"
         (rep type-rep)
         (types type-table utils)
         (optimizer utils))

(provide vector-opt-expr)


(define-syntax-class vector-op
  #:commit
  ;; we need the * versions of these unsafe operations to be chaperone-safe
  (pattern (~literal vector-ref)  #:with unsafe #'unsafe-vector-ref)
  (pattern (~literal vector-set!) #:with unsafe #'unsafe-vector-set!))

(define-syntax-class vector-expr
  #:commit
  (pattern e:expr
           #:when (match (type-of #'e)
                    [(tc-result1: (HeterogenousVector: _)) #t]
                    [_ #f])
           #:with opt ((optimize) #'e)))

(define-syntax-class vector-opt-expr
  #:commit
  ;; vector-length of a known-length vector
  (pattern (#%plain-app (~and op (~or (~literal vector-length)
                                      (~literal unsafe-vector-length)
                                      (~literal unsafe-vector*-length)))
                        v:vector-expr)
           #:with opt
           (begin (log-optimization "known-length vector-length" #'op)
                  (match (type-of #'v)
                    [(tc-result1: (HeterogenousVector: es))
                     #`(begin v.opt #,(length es))]))) ; v may have side effects
  ;; we can optimize vector-length on all vectors.
  ;; since the program typechecked, we know the arg is a vector.
  ;; we can optimize no matter what.
  (pattern (#%plain-app (~and op (~literal vector-length)) v:expr)
           #:with opt
           (begin (log-optimization "vector-length" #'op)
                  #`(unsafe-vector-length #,((optimize) #'v))))
  ;; same for flvector-length
  (pattern (#%plain-app (~and op (~literal flvector-length)) v:expr)
           #:with opt
           (begin (log-optimization "flvector-length" #'op)
                  #`(unsafe-flvector-length #,((optimize) #'v))))
  ;; we can optimize vector ref and set! on vectors of known length if we know
  ;; the index is within bounds (for now, literal or singleton type)
  (pattern (#%plain-app op:vector-op v:vector-expr i:expr new:expr ...)
           #:when (let ((len (match (type-of #'v)
                               [(tc-result1: (HeterogenousVector: es)) (length es)]
                               [_ 0]))
                        (ival (or (syntax-parse #'i [((~literal quote) i:number) (syntax-e #'i)] [_ #f])
                                  (match (type-of #'i)
                                    [(tc-result1: (Value: (? number? i))) i]
                                    [_ #f]))))
                    (and (integer? ival) (exact? ival) (<= 0 ival (sub1 len))))
           #:with opt
           (begin (log-optimization "vector" #'op)
                  #`(op.unsafe v.opt #,((optimize) #'i)
                               #,@(map (optimize) (syntax->list #'(new ...)))))))
