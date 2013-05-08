#lang racket/base
;; This file is for the abbreviations need to implement union.rkt

(require "../utils/utils.rkt")

(require (rep type-rep filter-rep object-rep rep-utils)
         (env mvar-env)
         racket/match racket/list (prefix-in c: (contract-req))
         (for-syntax racket/base syntax/parse racket/list)
         (for-template racket/base))

(provide (all-defined-out)
         (rename-out [make-Listof -lst]
                     [make-MListof -mlst]))

;Top and error types
(define Univ (make-Univ))
(define -Bottom (make-Union null))
(define Err (make-Error))

;A Type that corresponds to the any contract for the
;return type of functions
(define ManyUniv (make-AnyValues))

;;Convinient constructors
(define -val make-Value)

;; Char type and List type (needed because of how sequences are checked in subtype)
(define -Char (make-Base 'Char #'char? char? #'-Char #f))
(define (make-Listof elem) (-mu list-rec (simple-Un (-val null) (make-Pair elem list-rec))))
(define (make-MListof elem) (-mu list-rec (simple-Un (-val null) (make-MPair elem list-rec))))
;; Void is needed for Params
(define -Void (make-Base 'Void #'void? void? #'-Void #f))

;; -lst* Type is needed by substitute for ListDots
(define -pair make-Pair)
(define (-lst* #:tail [tail (-val null)] . args)
  (for/fold ([tl tail]) ([a (reverse args)]) (-pair a tl)))


;; Simple union type, does not check for overlaps


;; Union constructor
;; Normalizes representation by sorting types.
;; Type * -> Type
;; The input types can be union types, but should not have a complicated
;; overlap relationship.
(define simple-Un
  (let ()
    ;; List[Type] -> Type
    ;; Argument types should not overlap or be union types
    (define (make-union* types)
      (match types
        [(list t) t]
        [_ (make-Union types)]))

    ;; Type -> List[Type]
    (define (flat t)
      (match t
        [(Union: es) es]
        [_ (list t)]))

    (case-lambda
      [() -Bottom]
      [(t) t]
      [args
       (make-union* (remove-dups (sort (append-map flat args) type<?)))])))

;; Recursive types
(define-syntax -v
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))

;;Results
(define/cond-contract (-result t [f -no-filter] [o -no-obj])
  (c:->* (Type/c) (FilterSet? Object?) Result?)
  (make-Result t f o))

;;Filters
(define -top (make-Top))
(define -bot (make-Bot))
(define -no-filter (make-FilterSet -top -top))
(define -no-obj (make-Empty))


(define/cond-contract (-FS + -)
  (c:-> Filter/c Filter/c FilterSet?)
  (make-FilterSet + -))

(define/cond-contract (-filter t i [p null])
     (c:->* (Type/c name-ref/c) ((c:listof PathElem?)) Filter/c)
     (if (or (type-equal? Univ t) (and (identifier? i) (is-var-mutated? i)))
         -top
         (make-TypeFilter t p i)))

(define/cond-contract (-not-filter t i [p null])
     (c:->* (Type/c name-ref/c) ((c:listof PathElem?)) Filter/c)
     (if (or (type-equal? -Bottom t) (and (identifier? i) (is-var-mutated? i)))
         -top
         (make-NotTypeFilter t p i)))

(define (-filter-at t o)
  (match o
    [(Path: p i) (-filter t i p)]
    [_ -top]))
(define (-not-filter-at t o)
  (match o
    [(Path: p i) (-not-filter t i p)]
    [_ -top]))


;; Function types
(define/cond-contract (make-arr* dom rng
                                 #:rest [rest #f] #:drest [drest #f] #:kws [kws null]
                                 #:filters [filters -no-filter] #:object [obj -no-obj])
  (c:->* ((c:listof Type/c) (c:or/c SomeValues/c Type/c))
         (#:rest (c:or/c #f Type/c)
          #:drest (c:or/c #f (c:cons/c Type/c symbol?))
          #:kws (c:listof Keyword?)
          #:filters FilterSet?
          #:object Object?)
         arr?)
  (make-arr dom (if (Type/c? rng)
                    (make-Values (list (-result rng filters obj)))
                    rng)
            rest drest (sort #:key Keyword-kw kws keyword<?)))

(define-syntax (->* stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom rng)
     #'(make-Function (list (make-arr* dom rng)))]
    [(_ dom rst rng)
     #'(make-Function (list (make-arr* dom rng #:rest rst)))]
    [(_ dom rng :c filters)
     #'(make-Function (list (make-arr* dom rng #:filters filters)))]
    [(_ dom rng _:c filters _:c object)
     #'(make-Function (list (make-arr* dom rng #:filters filters #:object object)))]
    [(_ dom rst rng _:c filters)
     #'(make-Function (list (make-arr* dom rng #:rest rst #:filters filters)))]
    [(_ dom rst rng _:c filters : object)
     #'(make-Function (list (make-arr* dom rng #:rest rst #:filters filters #:object object)))]))

(define-syntax (-> stx)
  (define-syntax-class c
    (pattern x:id #:fail-unless (eq? ': (syntax-e #'x)) #f))
  (syntax-parse stx
    [(_ dom ... rng _:c filters _:c objects)
     #'(->* (list dom ...) rng : filters : objects)]
    [(_ dom ... rng :c filters)
     #'(->* (list dom ...) rng : filters)]
    [(_ dom ... rng)
     #'(->* (list dom ...) rng)]))

(define-syntax ->...
  (syntax-rules (:)
    [(_ dom rng)
     (->* dom rng)]
    [(_ dom (dty dbound) rng)
     (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound))))]
    [(_ dom rng : filters)
     (->* dom rng : filters)]
    [(_ dom (dty dbound) rng : filters)
     (make-Function (list (make-arr* dom rng #:drest (cons dty 'dbound) #:filters filters)))]))

(define (->acc dom rng path)
  (make-Function (list (make-arr* dom rng
                                  #:filters (-FS (-not-filter (-val #f) 0 path)
                                                 (-filter (-val #f) 0 path))
                                  #:object (make-Path path 0)))))

(define (cl->* . args)
  (define (funty-arities f)
    (match f
      [(Function: as) as]))
  (make-Function (apply append (map funty-arities args))))

(define-syntax cl->
  (syntax-parser
   [(_ [(dom ...) rng] ...)
    #'(cl->* (dom ... . -> . rng) ...)]))

(define-syntax (->key stx)
  (syntax-parse stx
                [(_ ty:expr ... (~seq k:keyword kty:expr opt:boolean) ... rng)
                 #'(make-Function
                    (list
                     (make-arr* (list ty ...)
                                rng
                                #:kws (sort #:key (match-lambda [(Keyword: kw _ _) kw])
                                            (list (make-Keyword 'k kty opt) ...)
                                            keyword<?))))]))

(define-syntax (->optkey stx)
  (syntax-parse stx
                [(_ ty:expr ... [oty:expr ...] (~seq k:keyword kty:expr opt:boolean) ... rng)
                 (let ([l (syntax->list #'(oty ...))])
                   (with-syntax ([((extra ...) ...)
                                  (for/list ([i (in-range (add1 (length l)))])
                                    (take l i))])
                     #'(make-Function
                        (list
                         (make-arr* (list ty ... extra ...)
                                    rng
                                    #:kws (sort #:key (match-lambda [(Keyword: kw _ _) kw])
                                                (list (make-Keyword 'k kty opt) ...)
                                                keyword<?))
                         ...))))]))

(define (make-arr-dots dom rng dty dbound)
  (make-arr* dom rng #:drest (cons dty dbound)))
