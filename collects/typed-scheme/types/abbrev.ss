#lang scheme

(require "../utils/utils.ss")

(require (rep type-rep object-rep filter-rep)
	 "printer.ss" "utils.ss"
         (utils tc-utils)
         scheme/list
         scheme/match         
         scheme/promise
         (prefix-in c: scheme/contract)
         (for-syntax scheme/base stxclass)
	 (for-template scheme/base scheme/contract scheme/tcp))

(provide (all-defined-out)
         (rename-out [make-Listof -lst]))

;; convenient constructors


(define -App make-App)
(define -pair make-Pair)
(define -val make-Value)
(define -Param make-Param)
(define -box make-Box)
(define -vec make-Vector)
(define -LFS make-LFilterSet)
(define -FS make-FilterSet)

(define-syntax *Un
  (syntax-rules ()
    [(_ . args) (make-Union (list . args))]))


(define (make-Listof elem) (-mu list-rec (*Un (-val null) (-pair elem list-rec))))

(define (-lst* #:tail [tail (-val null)] . args)
  (if (null? args)
      tail
      (-pair (car args) (apply -lst* #:tail tail (cdr args)))))

(define (-Tuple l)
  (foldr -pair (-val '()) l))

(define (untuple t)
  (match t
    [(Value: '()) null]
    [(Pair: a b) (cond [(untuple b) => (lambda (l) (cons a l))]
                       [else #f])]
    [_ #f]))


(d/c (-result t [f -no-lfilter] [o -no-lobj])
  (c:->* (Type/c) (LFilterSet? LatentObject?) Result?)
  (make-Result t f o))

(d/c (-values args)
     (c:-> (listof Type/c) Values?)
     (make-Values (for/list ([i args]) (-result i))))

;; basic types

(define make-promise-ty
  (let ([s (string->uninterned-symbol "Promise")])
    (lambda (t)
      (make-Struct s #f (list t) #f #f #'promise? values))))

(define -Listof (-poly (list-elem) (make-Listof list-elem)))


(define -Number (make-Base 'Number #'number?))
(define -Integer (make-Base 'Integer #'exact-integer?))
(define -Boolean (make-Base 'Boolean #'boolean?))
(define -Symbol (make-Base 'Symbol #'symbol?))
(define -Void (make-Base 'Void #'void?))
(define -Bytes (make-Base 'Bytes #'bytes?))
(define -Regexp (make-Base 'Regexp #'(and/c regexp? (not/c pregexp?) (not/c byte-regexp?))))
(define -PRegexp (make-Base 'PRegexp #'(and/c pregexp? (not/c byte-pregexp?))))
(define -Byte-Regexp (make-Base 'Byte-Regexp #'(and/c byte-regexp? (not/c byte-pregexp?))))
(define -Byte-PRegexp (make-Base 'Byte-PRegexp #'byte-pregexp?))
(define -String (make-Base 'String #'string?))
(define -Keyword (make-Base 'Keyword #'keyword?))
(define -Char (make-Base 'Char #'char?))
(define -Prompt-Tag (make-Base 'Prompt-Tag #'continuation-prompt-tag?))
(define -Cont-Mark-Set (make-Base 'Continuation-Mark-Set #'continuation-mark-set?))
(define -Path (make-Base 'Path #'path?))
(define -Namespace (make-Base 'Namespace #'namespace?))
(define -Output-Port (make-Base 'Output-Port #'output-port?))
(define -Input-Port (make-Base 'Input-Port #'input-port?))
(define -TCP-Listener (make-Base 'TCP-Listener #'tcp-listener?))

(define -Syntax make-Syntax)
(define -HT make-Hashtable)
(define -Promise make-promise-ty)

(define Univ (make-Univ))
(define Err (make-Error))

(define -Nat -Integer)
(define -Real -Number)

(define -Port (*Un -Output-Port -Input-Port))

(define -Pathlike (*Un -String -Path))
(define -Pathlike* (*Un -String -Path (-val 'up) (-val 'same)))
(define -Pattern (*Un -Bytes -Regexp -PRegexp -Byte-Regexp -Byte-PRegexp -String))
(define -Byte -Number)

(define -no-lfilter (make-LFilterSet null null))
(define -no-filter (make-FilterSet null null))
(define -no-lobj (make-LEmpty))
(define -no-obj (make-Empty))

(define -car (make-CarPE))
(define -cdr (make-CdrPE))

;; convenient syntax

(define-syntax -v 
  (syntax-rules ()
    [(_ x) (make-F 'x)]))

(define-syntax -poly
  (syntax-rules ()
    [(_ (vars ...) ty)
     (let ([vars (-v vars)] ...)
       (make-Poly (list 'vars ...) ty))]))

(define-syntax -polydots
  (syntax-rules ()
    [(_ (vars ... dotted) ty)
     (let ([dotted (-v dotted)]
           [vars (-v vars)] ...)
       (make-PolyDots (list 'vars ... 'dotted) ty))]))

(define-syntax -mu
  (syntax-rules ()
    [(_ var ty)
     (let ([var (-v var)])
       (make-Mu 'var ty))]))

;; function type constructors

(define top-func (make-Function (list (make-top-arr))))

(d/c (make-arr* dom rng
                #:rest [rest #f] #:drest [drest #f] #:kws [kws null]
                #:filters [filters -no-lfilter] #:object [obj -no-lobj])
  (c:->* ((listof Type/c) (or/c Values? ValuesDots? Type/c))
         (#:rest (or/c #f Type/c) 
          #:drest (or/c #f (cons/c Type/c symbol?))
          #:kws (listof Keyword?)
          #:filters LFilterSet?
          #:object LatentObject?)
         arr?)
  (make-arr dom (if (or (Values? rng) (ValuesDots? rng))
                    rng
                    (make-Values (list (-result rng filters obj))))
            rest drest (sort #:key Keyword-kw kws keyword<?)))

(define-syntax (->* stx)
  (define-syntax-class c
    (pattern x:id #:when (eq? ': (syntax-e #'x))))
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
    (pattern x:id #:when (eq? ': (syntax-e #'x))))
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
                                  #:filters (-LFS (list (-not-filter (-val #f) path)) 
                                                  (list (-filter (-val #f) path)))
                                  #:object (make-LPath path 0)))))

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
                [(_ ty:expr ... (~or (k:keyword kty:expr opt:boolean)) ... rng)
                 #'(make-Function
                    (list
                     (make-arr* (list ty ...)
                                rng
                                #:kws (list (make-Keyword 'k kty opt) ...))))]))

(define (make-arr-dots dom rng dty dbound)
  (make-arr* dom rng #:drest (cons dty dbound)))

(define (-struct name parent flds [proc #f] [poly #f] [pred #'dummy] [cert values])
  (make-Struct name parent flds proc poly pred cert))

(define (-filter t [p null] [i 0])
  (make-LTypeFilter t p i))

(define (-not-filter t [p null] [i 0])
  (make-LNotTypeFilter t p i))


(d/c make-pred-ty
  (case-> (c:-> Type/c Type/c)
          (c:-> (listof Type/c) Type/c Type/c Type/c))
  (case-lambda 
    [(in out t)
     (->* in out : (-LFS (list (-filter t)) (list (-not-filter t))))]
    [(t) (make-pred-ty (list Univ) -Boolean t)]))

(define true-filter (-FS (list) (list (make-Bot))))
(define false-filter (-FS (list (make-Bot)) (list)))


(define (opt-fn args opt-args result)
  (apply cl->* (for/list ([i (in-range (add1 (length opt-args)))])                         
                 (make-Function (list (make-arr* (append args (take opt-args i)) result))))))

(define-syntax-rule (->opt args ... [opt ...] res)
  (opt-fn (list args ...) (list opt ...) res))
