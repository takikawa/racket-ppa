#lang scheme/base
(require "../utils/utils.ss")

(require (utils tc-utils) 
	 "rep-utils.ss" "object-rep.ss" "filter-rep.ss" "free-variance.ss"
         mzlib/trace scheme/match
         scheme/contract
         (for-syntax scheme/base))

(define name-table (make-weak-hasheq))

(define Type/c?
   (λ (e)
     (and (Type? e)
          (not (Scope? e))
          (not (arr? e))
          (not (Values? e))
          (not (ValuesDots? e))
          (not (Result? e)))))

(define Type/c
  (flat-named-contract 'Type Type/c?))

;; Name = Symbol

;; Type is defined in rep-utils.ss

;; t must be a Type
(dt Scope ([t (or/c Type/c Scope?)]) [#:key (Type-key t)])

(define (scope-depth k)
  (flat-named-contract 
   (format "Scope of depth ~a" k)
   (lambda (sc)
     (define (f k sc)
       (cond [(= 0 k) (Type/c? sc)]
             [(not (Scope? sc)) #f]
             [else (f (sub1 k) (Scope-t sc))]))
     (f k sc))))

;; this is ONLY used when a type error ocurrs
(dt Error () [#:frees #f] [#:fold-rhs #:base])

;; i is an nat
(dt B ([i natural-number/c])
    [#:frees empty-hash-table (make-immutable-hasheq (list (cons i Covariant)))]
    [#:fold-rhs #:base])

;; n is a Name
(dt F ([n symbol?]) [#:frees (make-immutable-hasheq (list (cons n Covariant))) empty-hash-table] [#:fold-rhs #:base])

;; id is an Identifier
(dt Name ([id identifier?]) [#:intern (hash-id id)] [#:frees #f] [#:fold-rhs #:base])

;; rator is a type
;; rands is a list of types
;; stx is the syntax of the pair of parens
(dt App ([rator Type/c] [rands (listof Type/c)] [stx (or/c #f syntax?)])
    [#:intern (list rator rands)]
    [#:frees (combine-frees (map free-vars* (cons rator rands)))
             (combine-frees (map free-idxs* (cons rator rands)))]
    [#:fold-rhs (*App (type-rec-id rator)
                      (map type-rec-id rands)
                      stx)])

;; left and right are Types
(dt Pair ([left Type/c] [right Type/c]) [#:key 'pair])

;; *mutable* pairs - distinct from regular pairs
;; left and right are Types
(dt MPair ([left Type/c] [right Type/c]) [#:key 'mpair])


;; elem is a Type
(dt Vector ([elem Type/c]) 
    [#:frees (make-invariant (free-vars* elem)) (make-invariant (free-idxs* elem))]
    [#:key 'vector])

;; elem is a Type
(dt Box ([elem Type/c]) [#:frees (make-invariant (free-vars* elem)) (make-invariant (free-idxs* elem))]
    [#:key 'box])  

;; name is a Symbol (not a Name)
(dt Base ([name symbol?] [contract syntax?]) [#:frees #f] [#:fold-rhs #:base] [#:intern name]
    [#:key (case name
	     [(Number Integer) 'number]
	     [(Boolean) 'boolean]
	     [(String) 'string]
	     [(Symbol) 'symbol]
             [(Keyword) 'keyword]
	     [else #f])])

;; body is a Scope
(dt Mu ([body (scope-depth 1)]) #:no-provide [#:frees (free-vars* body) (without-below 1 (free-idxs* body))]
    [#:fold-rhs (*Mu (*Scope (type-rec-id (Scope-t body))))]
    [#:key (Type-key body)])    

;; n is how many variables are bound here
;; body is a Scope
(dt Poly (n body) #:no-provide 
    [#:contract (->d ([n natural-number/c]
                      [body (scope-depth n)])
                     (#:syntax [stx (or/c #f syntax?)])
                     [result Poly?])]
    [#:frees (free-vars* body) (without-below n (free-idxs* body))]
    [#:fold-rhs (let ([body* (remove-scopes n body)])
                  (*Poly n (add-scopes n (type-rec-id body*))))]
    [#:key (Type-key body)])

;; n is how many variables are bound here
;; there are n-1 'normal' vars and 1 ... var
;; body is a Scope
(dt PolyDots (n body) #:no-provide
    [#:contract (->d ([n natural-number/c]
                      [body (scope-depth n)])
                     (#:syntax [stx (or/c #f syntax?)])
                     [result PolyDots?])]
    [#:key (Type-key body)]
    [#:frees (free-vars* body) (without-below n (free-idxs* body))]
    [#:fold-rhs (let ([body* (remove-scopes n body)])
                  (*PolyDots n (add-scopes n (type-rec-id body*))))])

;; pred : identifier
;; cert : syntax certifier
(dt Opaque ([pred identifier?] [cert procedure?]) 
    [#:intern (hash-id pred)] [#:frees #f] [#:fold-rhs #:base] [#:key pred])

;; kw : keyword?
;; ty : Type
;; required? : Boolean
(dt Keyword ([kw keyword?] [ty Type/c] [required? boolean?])
    [#:frees (λ (f) (f ty))]
    [#:fold-rhs (*Keyword kw (type-rec-id ty) required?)])

(dt Result ([t Type/c] [f LFilterSet?] [o LatentObject?])
    [#:frees (λ (frees) (combine-frees (map frees (list t f o))))]
    [#:fold-rhs (*Result (type-rec-id t) (latentfilter-rec-id f) (latentobject-rec-id o))])

;; types : Listof[Type]
(dt Values ([rs (listof Result?)]) 
    [#:frees (λ (f) (combine-frees (map f rs)))]
    [#:fold-rhs (*Values (map type-rec-id rs))])

(dt ValuesDots ([rs (listof Result?)] [dty Type/c] [dbound (or/c symbol? natural-number/c)])
    [#:frees (λ (f) (combine-frees (map f (cons dty rs))))]
    [#:fold-rhs (*ValuesDots (map type-rec-id rs) (type-rec-id dty) dbound)])

;; arr is NOT a Type
(dt arr ([dom (listof Type/c)] 
         [rng (or/c Values? ValuesDots?)]
         [rest (or/c #f Type/c)] 
         [drest (or/c #f (cons/c Type/c (or/c natural-number/c symbol?)))]
         [kws (listof Keyword?)])
    [#:frees (combine-frees 
              (append (map (compose flip-variances free-vars*) 
                           (append (if rest (list rest) null)
                                   (map Keyword-ty kws)
                                   dom))
                      (match drest
                        [(cons t (? symbol? bnd))
                         (list (fix-bound (flip-variances (free-vars* t)) bnd))]                          
                        [(cons t (? number? bnd)) 
                         (list (flip-variances (free-vars* t)))]
                        [#f null])
                      (list (free-vars* rng))))
             (combine-frees 
              (append (map (compose flip-variances free-idxs*) 
                           (append (if rest (list rest) null)
                                   (map Keyword-ty kws)
                                   dom))
                      (match drest
                        [(cons t (? symbol? bnd))
                         (list (flip-variances (free-idxs* t)))]
                        [(cons t (? number? bnd)) 
                         (list (fix-bound (flip-variances (free-idxs* t)) bnd))]
                        [#f null])
                      (list (free-idxs* rng))))]
    [#:fold-rhs (*arr (map type-rec-id dom)
                      (type-rec-id rng)
                      (and rest (type-rec-id rest))
                      (and drest (cons (type-rec-id (car drest)) (cdr drest)))
                      (map type-rec-id kws))])

;; top-arr is the supertype of all function types
(dt top-arr () [#:fold-rhs #:base])

(define arr/c (or/c top-arr? arr?))

;; arities : Listof[arr]
(dt Function ([arities (listof arr/c)])
    [#:key 'procedure]
    [#:frees (combine-frees (map free-vars* arities))
             (combine-frees (map free-idxs* arities))]
    [#:fold-rhs (*Function (map type-rec-id arities))])


;; name : symbol
;; parent : Struct
;; flds : Listof[Type]
;; proc : Function Type
;; poly? : is this a polymorphic type?
;; pred-id : identifier for the predicate of the struct
;; cert : syntax certifier for pred-id
(dt Struct ([name symbol?] 
            [parent (or/c #f Struct? Name?)] 
            [flds (listof Type/c)]
            [proc (or/c #f Function?)]
            [poly? boolean?] 
            [pred-id identifier?]
            [cert procedure?]
            [acc-ids (listof identifier?)])
    [#:intern (list name parent flds proc)]
    [#:frees (combine-frees (map free-vars* (append (if proc (list proc) null) (if parent (list parent) null) flds)))
             (combine-frees (map free-idxs* (append (if proc (list proc) null) (if parent (list parent) null) flds)))]
    [#:fold-rhs (*Struct name 
                         (and parent (type-rec-id parent))
                         (map type-rec-id flds)
                         (and proc (type-rec-id proc))
                         poly?
                         pred-id
                         cert
                         acc-ids)]
    [#:key #f])

;; A structure type descriptor
;; s : struct
(dt StructType ([s Struct?]) [#:key 'struct-type])

;; the supertype of all of these values
(dt BoxTop () [#:fold-rhs #:base] [#:key 'box])
(dt VectorTop () [#:fold-rhs #:base] [#:key 'vector])
(dt HashtableTop () [#:fold-rhs #:base] [#:key 'hash])
(dt MPairTop () [#:fold-rhs #:base] [#:key 'mpair])
(dt StructTop ([name Struct?]) [#:key #f])

;; v : Scheme Value
(dt Value (v) [#:frees #f] [#:fold-rhs #:base] [#:key (cond [(number? v) 'number]
							    [(boolean? v) 'boolean]
							    [(null? v) 'null]
							    [else #f])])

;; elems : Listof[Type]
(dt Union ([elems (and/c (listof Type/c)                         
                         (lambda (es)
                           (or (null? es)
                               (let-values ([(sorted? k)
                                             (for/fold ([sorted? #t]
                                                        [last (car es)])
                                               ([e (cdr es)])
                                               (values
                                                (and sorted? (type<? last e))
                                                e))])
                                 sorted?))))]) 
    [#:frees (combine-frees (map free-vars* elems))
             (combine-frees (map free-idxs* elems))]
    [#:fold-rhs ((get-union-maker) (map type-rec-id elems))]
    [#:key (let loop ([res null] [ts elems])
             (if (null? ts) res
                 (let ([k (Type-key (car ts))])
                   (cond [(pair? k) (loop (append k res) (cdr ts))]
                         [k (loop (cons k res) (cdr ts))]
                         [else #f]))))])
    
(dt Univ () [#:frees #f] [#:fold-rhs #:base])

;; in : Type
;; out : Type
(dt Param ([in Type/c] [out Type/c]) [#:key 'parameter])

;; key : Type
;; value : Type
(dt Hashtable ([key Type/c] [value Type/c]) [#:key 'hash])

;; parent : Type
;; pred : Identifier
;; cert : Certifier
(dt Refinement (parent pred cert)
    [#:key (Type-key parent)]
    [#:intern (list parent (hash-id pred))]
    [#:fold-rhs (*Refinement (type-rec-id parent) pred cert)]
    [#:frees (free-vars* parent)
             (free-idxs* parent)])
    

;; t : Type
(dt Syntax ([t Type/c]) [#:key 'syntax])

;; pos-flds  : (Listof Type)
;; name-flds : (Listof (Tuple Symbol Type Boolean))
;; methods   : (Listof (Tuple Symbol Function))
(dt Class ([pos-flds (listof Type/c)]
           [name-flds (listof (list/c symbol? Type/c boolean?))]
           [methods (listof (list/c symbol? Function?))])
    [#:frees (combine-frees
              (map free-vars* (append pos-flds 
                                      (map cadr name-flds)
                                      (map cadr methods))))
             (combine-frees
              (map free-idxs* (append pos-flds 
                                      (map cadr name-flds)
                                      (map cadr methods))))]
    [#:key 'class]
    [#:fold-rhs (match (list pos-flds name-flds methods)
                  [(list
                    pos-tys 
                    (list (list init-names init-tys) ___)
                    (list (list mname mty) ___))
                   (*Class
                    (map type-rec-id pos-tys)
                    (map list
                         init-names
                         (map type-rec-id init-tys))
                    (map list mname (map type-rec-id mty)))])])

;; cls : Class
(dt Instance ([cls Type/c]) [#:key 'instance])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ugly hack - should use units

(define union-maker (box (lambda args (int-err "Union not yet available"))))

(define (set-union-maker! v) (set-box! union-maker v))
(define (get-union-maker) (unbox union-maker))

(provide set-union-maker! get-union-maker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-dups: List[Type] -> List[Type]
;; removes duplicate types from a SORTED list
(d/c (remove-dups types)
  ((listof Rep?) . -> . (listof Rep?))
  (cond [(null? types) types]
        [(null? (cdr types)) types]
        [(type-equal? (car types) (cadr types)) (remove-dups (cdr types))]
        [else (cons (car types) (remove-dups (cdr types)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add-scopes n t)
  (if (zero? n) t
      (add-scopes (sub1 n) (*Scope t))))

(define (remove-scopes n sc)
  (if (zero? n) 
      sc
      (match sc
        [(Scope: sc*) (remove-scopes (sub1 n) sc*)]
        [_ (int-err "Tried to remove too many scopes: ~a" sc)])))

;; type equality
(d/c (type-equal? s t) (Rep? Rep? . -> . boolean?) (eq? (Rep-seq s) (Rep-seq t)))

;; inequality - good
(d/c (type<? s t)
  (Rep? Rep? . -> . boolean?)
  (< (Rep-seq s) (Rep-seq t)))

(d/c (type-compare s t)
  (Rep? Rep? . -> . (or/c -1 0 1))
  (cond [(type-equal? s t) 0]
        [(type<? s t) 1]
        [else -1]))

(define ((sub-lf st) e)
  (latentfilter-case (#:Type st
                      #:LatentFilter (sub-lf st))
                     e))

(define ((sub-lo st) e)
  (latentobject-case (#:Type st
                      #:LatentObject (sub-lo st)
                      #:PathElem (sub-pe st))
                     e))

(define ((sub-pe st) e)
  (pathelem-case (#:Type st
                  #:PathElem (sub-pe st))
                 e))

;; abstract-many : Names Type -> Scope^n 
;; where n is the length of names  
(define (abstract-many names ty)
  (define (nameTo name count type)
    (let loop ([outer 0] [ty type])
      (define (sb t) (loop outer t))
      (define slf (sub-lf sb))
      (type-case 
       (#:Type sb #:LatentFilter (sub-lf sb) #:LatentObject (sub-lo sb))
       ty
       [#:F name* (if (eq? name name*) (*B (+ count outer)) ty)]
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions 
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (if (eq? (cdr drest) name) (+ count outer) (cdr drest)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
              (*ValuesDots (map sb rs)
                           (sb dty)
                           (if (eq? dbound name) (+ count outer) dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyDots n body* 
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (*Scope (loop (+ n outer) body))))]
       [#:Poly n body* 
               (let ([body (remove-scopes n body*)])
                 (*Poly n (*Scope (loop (+ n outer) body))))])))
  (let ([n (length names)])
    (let loop ([ty ty] [names names] [count (sub1 n)])
      (if (zero? count)
          (add-scopes n (nameTo (car names) 0 ty))
          (loop (nameTo (car names) count ty)
                (cdr names)
                (sub1 count))))))

;; instantiate-many : List[Type] Scope^n -> Type 
;; where n is the length of types  
;; all of the types MUST be Fs
(define (instantiate-many images sc)
  (define (replace image count type)
    (let loop ([outer 0] [ty type])
      (define (sb t) (loop outer t))    
      (define slf (sub-lf sb))  
      (type-case 
       (#:Type sb #:LatentFilter slf #:LatentObject (sub-lo sb))
       ty
       [#:B idx (if (= (+ count outer) idx)
                    image
                    ty)]      
       ;; necessary to avoid infinite loops
       [#:Union elems (*Union (remove-dups (sort (map sb elems) type<?)))]
       ;; functions
       [#:arr dom rng rest drest kws
              (*arr (map sb dom)
                    (sb rng)
                    (if rest (sb rest) #f)
                    (if drest
                        (cons (sb (car drest))
                              (if (eqv? (cdr drest) (+ count outer)) (F-n image) (cdr drest)))
                        #f)
                    (map sb kws))]
       [#:ValuesDots rs dty dbound
                     (*ValuesDots (map sb rs)
                                  (sb dty)
                                  (if (eqv? dbound (+ count outer)) (F-n image) dbound))]
       [#:Mu (Scope: body) (*Mu (*Scope (loop (add1 outer) body)))]
       [#:PolyDots n body* 
                   (let ([body (remove-scopes n body*)])
                     (*PolyDots n (*Scope (loop (+ n outer) body))))]
       [#:Poly n body*
               (let ([body (remove-scopes n body*)])
                 (*Poly n (*Scope (loop (+ n outer) body))))])))
  (let ([n (length images)])
    (let loop ([ty (remove-scopes n sc)] [images images] [count (sub1 n)])
      (if (zero? count)
          (replace (car images) 0 ty)
          (loop (replace (car images) count ty)
                (cdr images)
                (sub1 count))))))

(define (abstract name ty)
  (abstract-many (list name) ty))

(define (instantiate type sc)
  (instantiate-many (list type) sc))

#;(trace instantiate-many abstract-many)

;; the 'smart' constructor
(define (Mu* name body)    
  (let ([v (*Mu (abstract name body))])
    (hash-set! name-table v name)
    v))

;; the 'smart' destructor
(define (Mu-body* name t)
  (match t
    [(Mu: scope)
     (instantiate (*F name) scope)]))

;; the 'smart' constructor
(define (Poly* names body)
  (if (null? names) body
      (let ([v (*Poly (length names) (abstract-many names body))])
        (hash-set! name-table v names)
        v)))

;; the 'smart' destructor
(define (Poly-body* names t)
  (match t
    [(Poly: n scope)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map *F names) scope)]))

;; the 'smart' constructor
(define (PolyDots* names body)
  (if (null? names) body
      (let ([v (*PolyDots (length names) (abstract-many names body))])
        (hash-set! name-table v names)
        v)))

;; the 'smart' destructor
(define (PolyDots-body* names t)
  (match t
    [(PolyDots: n scope)
     (unless (= (length names) n)
       (int-err "Wrong number of names: expected ~a got ~a" n (length names)))
     (instantiate-many (map *F names) scope)]))

(print-struct #t)

(define-match-expander Mu-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ bp) #'(? Mu? (app (lambda (t) (Scope-t (Mu-body t))) bp))])))

(define-match-expander Poly-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? Poly? (app (lambda (t) (list (Poly-n t) (Poly-body t))) (list n bp)))])))

(define-match-expander PolyDots-unsafe:
  (lambda (stx)
    (syntax-case stx ()
      [(_ n bp) #'(? PolyDots? (app (lambda (t) (list (PolyDots-n t) (PolyDots-body t))) (list n bp)))])))

(define-match-expander Mu:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (gensym)])
                               (list sym (Mu-body* sym t))))
                 (list np bp)))])))

(define-match-expander Mu-name:
  (lambda (stx)
    (syntax-case stx ()
      [(_ np bp)
       #'(? Mu?
            (app (lambda (t) (let ([sym (hash-ref name-table t (lambda _ (gensym)))])
                               (list sym (Mu-body* sym t))))
                 (list np bp)))])))

;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander Poly:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t) 
                   (let* ([n (Poly-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable  
(define-match-expander Poly-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? Poly?
            (app (lambda (t) 
                   (let* ([n (Poly-n t)]
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (Poly-body* syms t))))
                 (list nps bp)))])))

;; This match expander wraps the smart constructor
;; names are generated with gensym
(define-match-expander PolyDots:*
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t) 
                   (let* ([n (PolyDots-n t)]
                          [syms (build-list n (lambda _ (gensym)))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

;; This match expander uses the names from the hashtable  
(define-match-expander PolyDots-names:
  (lambda (stx)
    (syntax-case stx ()
      [(_ nps bp)
       #'(? PolyDots?
            (app (lambda (t) 
                   (let* ([n (PolyDots-n t)]
                          [syms (hash-ref name-table t (lambda _ (build-list n (lambda _ (gensym)))))])
                     (list syms (PolyDots-body* syms t))))
                 (list nps bp)))])))

;(trace subst subst-all)

(provide
 Mu-name: Poly-names:
 PolyDots-names:
 Type-seq  
 Mu-unsafe: Poly-unsafe:
 PolyDots-unsafe:
 Mu? Poly? PolyDots?
 arr
 Type? Filter? LatentFilter? Object? LatentObject?
 Type/c
 Poly-n
 PolyDots-n
 free-vars*
 type-compare type<?
 remove-dups
 sub-lf sub-lo sub-pe
 Values: Values? Values-rs
 (rename-out [Mu:* Mu:]               
             [Poly:* Poly:]
             [PolyDots:* PolyDots:]
             [Mu* make-Mu]
             [Poly* make-Poly]
             [PolyDots* make-PolyDots]
             [Mu-body* Mu-body]
             [Poly-body* Poly-body]
             [PolyDots-body* PolyDots-body]))

(p/c [type-equal? (Rep? Rep? . -> . boolean?)])

;(trace unfold)
