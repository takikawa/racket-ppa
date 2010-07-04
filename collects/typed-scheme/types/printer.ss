#lang scheme/base

(require "../utils/utils.ss")
(require (rep type-rep filter-rep object-rep rep-utils)
	 (utils tc-utils)
	 scheme/match)

;; do we attempt to find instantiations of polymorphic types to print? 
;; FIXME - currently broken
(define print-poly-types? #f)
;; do we use simple type aliases in printing
(define print-aliases #t)

;; does t have a type name associated with it currently?
;; has-name : Type -> Maybe[Symbol]
(define (has-name? t) 
  (define ns ((current-type-names)))
  (let/ec return
    (unless print-aliases
      (return #f))
    (for-each
     (lambda (pair)
       (cond [(eq? t (cdr pair))
              (return (car pair))]))
     ns)
    #f))

;; print out an effect
;; print-effect : Effect Port Boolean -> Void
(define (print-latentfilter c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(LFilterSet: thn els) (fp "(")
                           (for ([i thn]) (fp "~a " i)) (fp "|")
                           (for ([i els]) (fp " ~a" i))
                           (fp")")]
    [(LNotTypeFilter: type path idx) (fp "(! ~a @ ~a ~a)" type path idx)]
    [(LTypeFilter: type path idx) (fp "(~a @ ~a ~a)" type path idx)]
    [(LBot:) (fp "LBot")]))

(define (print-filter c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(FilterSet: thn els) (fp "(")
                          (for ([i thn]) (fp "~a " i)) (fp "|")
                          (for ([i els]) (fp " ~a" i))
                          (fp")")]
    [(NoFilter:) (fp "-")]
    [(NotTypeFilter: type path id) (fp "(! ~a @ ~a ~a)" type path (syntax-e id))]
    [(TypeFilter: type path id) (fp "(~a @ ~a ~a)" type path (syntax-e id))]
    [(Bot:) (fp "Bot")]))

(define (print-pathelem c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(CarPE:) (fp "car")]
    [(CdrPE:) (fp "cdr")]
    [(StructPE: t i) (fp "(~a ~a)" t i)]))

(define (print-latentobject c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(LEmpty:) (fp "")]
    [(LPath: pes i) (fp "~a" (append pes (list i)))]))

(define (print-object c port write?)
  (define (fp . args) (apply fprintf port args))
  (match c
    [(NoObject:) (fp "-")]
    [(Empty:) (fp "")]
    [(Path: pes i) (fp "~a" (append pes (list (syntax-e i))))]))

;; print out a type
;; print-type : Type Port Boolean -> Void
(define (print-type c port write?)
  (define (fp . args) (apply fprintf port args)) 
  (define (print-arr a)
    (match a
      [(top-arr:)
       (fp "Procedure")]
      [(arr: dom rng rest drest kws)
       (fp "(")
       (for-each (lambda (t) (fp "~a " t)) dom)
       (for ([kw kws])
         (match kw
           [(Keyword: k t req?)
            (if req?
                (fp "~a ~a " k t)
                (fp "[~a ~a] " k t))]))
       (when rest
         (fp "~a* " rest))
       (when drest
         (fp "~a ... ~a " (car drest) (cdr drest)))
       (match rng
         [(Values: (list (Result: t (LFilterSet: (list) (list)) (LEmpty:))))
          (fp "-> ~a" t)]
         [(Values: (list (Result: t
				  (LFilterSet: (list (LTypeFilter: ft '() 0))
					       (list (LNotTypeFilter: ft '() 0)))
				  (LEmpty:)))) 
          (fp "-> ~a : ~a" t ft)]
         [(Values: (list (Result: t fs (LEmpty:)))) 
          (fp "-> ~a : ~a" t fs)]
         [(Values: (list (Result: t lf lo)))
          (fp "-> ~a : ~a ~a" t lf lo)]
         [_
          (fp "-> ~a" rng)])
       (fp ")")]))
  (define (tuple? t)
    (match t
      [(Pair: a (? tuple?)) #t]
      [(Value: '()) #t]
      [_ #f]))
  (define (tuple-elems t)
    (match t
      [(Pair: a e) (cons a (tuple-elems e))]
      [(Value: '()) null]))
  (match c 
    [(Univ:) (fp "Any")]
    [(? has-name?) (fp "~a" (has-name? c))]
    ;; names are just the printed as the original syntax
    [(Name: stx) (fp "~a" (syntax-e stx))]
    [(App: rator rands stx) 
     (fp "~a" (list* rator rands))]
    ;; special cases for lists
    [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
     (fp "(Listof ~a)" elem-ty)]
    [(Mu: var (Union: (list (Pair: elem-ty (F: var)) (Value: '()))))
     (fp "(Listof ~a)" elem-ty)]
    [(Value: v) (cond [(or (symbol? v) (null? v))
                       (fp "'~a" v)]
                      [else (fp "~a" v)])]
    [(? tuple? t)
     (fp "~a" (cons 'List (tuple-elems t)))]
    [(Base: n cnt) (fp "~a" n)]      
    [(Opaque: pred _) (fp "(Opaque ~a)" (syntax->datum pred))]
    [(Struct: 'Promise par (list fld) proc _ _ _) (fp "(Promise ~a)" fld)]      
    [(Struct: nm par flds proc _ _ _) 
     (fp "#(struct:~a ~a" nm flds)
     (when proc
       (fp " ~a" proc))
     (fp ")")]
    [(Function: arities)
     (let ()         
       (match arities
         [(list) (fp "(case-lambda)")]
         [(list a) (print-arr a)]
         [(list a b ...) (fp "(case-lambda ")
                         (print-arr a) 
                         (for-each 
                          (lambda (e) (fp " ") (print-arr e))
                          b)
                         (fp ")")]))]
    [(arr: _ _ _ _ _) (print-arr c)]
    [(Vector: e) (fp "(Vectorof ~a)" e)]
    [(Box: e) (fp "(Box ~a)" e)]
    [(Union: elems) (fp "~a" (cons 'U elems))]
    [(Pair: l r) (fp "(Pair ~a ~a)" l r)]
    [(F: nm) (fp "~a" nm)]   
    ;; FIXME
    [(Values: (list v)) (fp "~a" v)]
    [(Values: (list v ...)) (fp "~a" (cons 'values v))]
    [(ValuesDots: v dty dbound) (fp "~a" (cons 'values (append v (list dty '... dbound))))]
    [(Param: in out) 
     (if (equal? in out)
         (fp "(Parameter ~a)" in)           
         (fp "(Parameter ~a ~a)" in out))]
    [(Hashtable: k v) (fp "(HashTable ~a ~a)" k v)]
    
    #;[(Poly-unsafe: n b) (fp "(unsafe-poly ~a ~a ~a)" (Type-seq c) n b)]
    [(Poly-names: names body) 
     #;(fprintf (current-error-port) "POLY SEQ: ~a~n" (Type-seq body))
     (fp "(All ~a ~a)" names body)]
    #;[(PolyDots-unsafe: n b) (fp "(unsafe-polydots ~a ~a ~a)" (Type-seq c) n b)]
    [(PolyDots-names: (list names ... dotted) body)
     (fp "(All ~a ~a)" (append names (list dotted '...)) body)]
    #;
    [(Mu-unsafe: b) (fp "(unsafe-mu ~a ~a)" (Type-seq c) b)]
    [(Mu: x (Syntax: (Union: (list
                              (Base: 'Number _) 
                              (Base: 'Boolean _)
                              (Base: 'Symbol _)
                              (Base: 'String _)
                              (Mu: var (Union: (list (Value: '()) (Pair: (F: x) (F: var)))))
                              (Mu: y (Union: (list (F: x) (Pair: (F: x) (F: y)))))
                              (Vector: (F: x))
                              (Box: (F: x))))))
     (fp "Syntax")]
    [(Mu-name: name body) (fp "(Rec ~a ~a)" name body)]
    ;; FIXME - this should not be used
    #;
    [(Scope: sc) (fp "(Scope ~a)" sc)]
    
    [(B: idx) (fp "(B ~a)" idx)]      
    [(Syntax: t) (fp "(Syntaxof ~a)" t)]
    [(Instance: t) (fp "(Instance ~a)" t)]
    [(Class: pf nf ms) (fp "(Class)")]
    [(Result: t (LFilterSet: (list) (list)) (LEmpty:)) (fp "~a" t)]
    [(Result: t fs (LEmpty:)) (fp "(~a : ~a)" t fs)]
    [(Result: t fs lo) (fp "(~a : ~a : ~a)" t fs lo)]
    [(Refinement: parent p? _)
     (fp "(Refinement ~a ~a)" parent (syntax-e p?))]
    [(Error:) (fp "Error")]
    [else (fp "(Unknown Type: ~a)" (struct->vector c))]
    ))

(set-box! print-type* print-type)
(set-box! print-filter* print-filter)
(set-box! print-latentfilter* print-latentfilter)
(set-box! print-object* print-object)
(set-box! print-latentobject* print-latentobject)
(set-box! print-pathelem* print-pathelem)
