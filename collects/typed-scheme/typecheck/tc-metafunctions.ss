#lang scheme/base

(require "../utils/utils.ss")
(require (rename-in (types subtype convenience remove-intersect union utils)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (rep type-rep)
         scheme/contract scheme/match unstable/match
         (for-syntax scheme/base))

(provide combine-filter apply-filter abstract-filter abstract-filters
         split-lfilters merge-filter-sets values->tc-results tc-results->values)

;; this implements the sequence invariant described on the first page relating to Bot
(define (lcombine l1 l2)
  (cond [(memq (make-LBot) l1)
         (make-LFilterSet (list (make-LBot)) null)]
        [(memq (make-LBot) l2)
         (make-LFilterSet null (list (make-LBot)))]
        [else (make-LFilterSet l1 l2)]))

(define (combine l1 l2)
  (cond [(memq (make-Bot) l1)
         (make-FilterSet (list (make-Bot)) null)]
        [(memq (make-Bot) l2)
         (make-FilterSet null (list (make-Bot)))]
        [else (make-FilterSet l1 l2)]))

(d/c (abstract-filters keys ids results)
     ((listof index/c) (listof identifier?) tc-results? . -> . (or/c Values? ValuesDots?))
     (define (mk l [drest #f])
       (if drest (make-ValuesDots l (car drest) (cdr drest)) (make-Values l)))
     (match results
       [(tc-results: ts fs os dty dbound)
        (make-ValuesDots 
         (for/list ([t ts]
                    [f fs]
                    [o os])
           (make-Result t (abstract-filter ids keys f) (abstract-object ids keys o)))
         dty dbound)]
       [(tc-results: ts fs os)
        (make-Values
         (for/list ([t ts]
                    [f fs]
                    [o os])
           (make-Result t (abstract-filter ids keys f) (abstract-object ids keys o))))]))

(define/contract (abstract-object ids keys o)
  (-> (listof identifier?) (listof index/c) Object? LatentObject?)
  (define (lookup y)
    (for/first ([x ids] [i keys] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match o    
    [(Path: p (lookup: idx)) (make-LPath p idx)]
    [_ (make-LEmpty)]))

(d/c (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof index/c) FilterSet/c LatentFilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (lcombine
      (apply append (for/list ([f f+]) (abo ids keys f)))
      (apply append (for/list ([f f-]) (abo ids keys f))))]))

(d/c (abo xs idxs f)
  (-> (listof identifier?) (listof index/c) Filter/c (or/c '() (list/c LatentFilter/c)))
  (define (lookup y)
    (for/first ([x xs] [i idxs] #:when (free-identifier=? x y)) i)) 
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match f
    [(Bot:) (list (make-LBot))]
    [(TypeFilter: t p (lookup: idx)) (list (make-LTypeFilter t p idx))]
    [(NotTypeFilter: t p (lookup: idx)) (list (make-LNotTypeFilter t p idx))]
    [(ImpFilter: a c)
     (match* [(abo a) (abo c)]
       [((list a*) (list c*)) (list (make-LImpFilter a* c*))]
       [(_ _) null])]
    [_ null]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (make-FilterSet (apply append f+) (apply append f-))]))

(d/c (apply-filter lfs t o)
  (-> LatentFilterSet/c Type/c Object? FilterSet/c)
  (match lfs
    [(LFilterSet: lf+ lf-)
     (combine
      (apply append (for/list ([lf lf+]) (apo lf t o)))
      (apply append (for/list ([lf lf-]) (apo lf t o))))]))

(d/c (apo lf s o)
  (-> LatentFilter/c Type/c Object? (or/c '() (list/c Filter/c)))
  (match* (lf s o)
    [((ImpFilter: as cs) _ _)
     (match* [(for/list ([a as]) (apo a s o))
	      (for/list ([c cs]) (apo c s o))]
       [((list (list a*) ...)
	 (list (list c*) ...)) (list (make-ImpFilter a* c*))]
       [(_ _) null])]
    [((LBot:) _ _) (list (make-Bot))]
    [((LNotTypeFilter: (? (lambda (t) (subtype s t)) t) (list) _) _ _) (list (make-Bot))]
    [((LTypeFilter: (? (lambda (t) (not (overlap s t))) t) (list) _) _ _) (list (make-Bot))]
    [(_ _ (Empty:)) null]
    [((LTypeFilter: t pi* _) _ (Path: pi x)) (list (make-TypeFilter t (append pi* pi) x))]
    [((LNotTypeFilter: t pi* _) _ (Path: pi x)) (list (make-NotTypeFilter t (append pi* pi) x))]))

(define/contract (split-lfilters lf idx)  
  (LatentFilterSet/c index/c . -> . LatentFilterSet/c)
  (define (idx= lf)
    (match lf
      [(LBot:) #t]
      [(LNotTypeFilter: _ _ idx*) (type-equal? idx* idx)]
      [(LTypeFilter: _ _ idx*) (type-equal? idx* idx)]))
  (match lf
    [(LFilterSet: lf+ lf-)
     (make-LFilterSet (filter idx= lf+) (filter idx= lf-))]))

(define-match-expander T-FS:
  (lambda (stx) #'(FilterSet: _ (list (Bot:)))))
(define-match-expander F-FS:
  (lambda (stx) #'(FilterSet: (list (Bot:)) _)))

(d/c (combine-filter f1 f2 f3 t2 t3 o2 o3)
  (FilterSet/c FilterSet/c FilterSet/c Type? Type? Object? Object? . -> . tc-results?)
  (define (mk f) (ret (Un t2 t3) f (make-Empty)))
  (match* (f1 f2 f3)
    [((T-FS:) f _) (ret t2 f o2)]
    [((F-FS:) _ f) (ret t3 f o3)]
    ;; the student expansion
    [(f (T-FS:) (F-FS:)) (mk f)]
    ;; skipping the general or/predicate rule because it's really complicated
    ;; or/predicate special case for one elem lists
    ;; note that we are relying on equal? on identifiers here
    [((FilterSet: (list (TypeFilter: t pi x)) (list (NotTypeFilter: t pi x)))
      (T-FS:)
      (FilterSet: (list (TypeFilter: s pi x)) (list (NotTypeFilter: s pi x))))
     (mk (make-FilterSet (list (make-TypeFilter (Un t s) pi x)) (list (make-NotTypeFilter (Un t s) pi x))))]
    ;; or
    [((FilterSet: f1+ f1-) (T-FS:) (FilterSet: f3+ f3-)) (mk (combine null (append f1- f3-)))]
    ;; and
    [((FilterSet: f1+ f1-) (FilterSet: f2+ f2-) (F-FS:)) 
     (mk (combine (append f1+ f2+)
		  null
		  #;
		  (append (for/list ([f f1-])
			    (make-ImpFilter f2+ f))
			  (for/list ([f f2-])
			    (make-ImpFilter f1+ f)))))]
    [(f f* f*) (mk f*)]
    [(_ _ _)
     ;; could intersect f2 and f3 here
     (mk (make-FilterSet null null))]))

;; (or/c Values? ValuesDots?) listof[identifier] -> tc-results?
(define (values->tc-results tc formals)
  (match tc
    [(ValuesDots: (list (Result: ts lfs los) ...) dty dbound)
     (ret ts
          (for/list ([lf lfs]) 
            (or
             (and (null? formals)
                  (match lf
                    [(LFilterSet: lf+ lf-)
                     (combine (if (memq (make-LBot) lf+) (list (make-Bot)) (list))
                              (if (memq (make-LBot) lf-) (list (make-Bot)) (list)))]))
             (merge-filter-sets
              (for/list ([x formals] [i (in-naturals)])
                (apply-filter (split-lfilters lf i) Univ (make-Path null x))))))
          (for/list ([lo los])
            (or 
             (for/or ([x formals] [i (in-naturals)])
               (match lo
                 [(LEmpty:) #f]
                 [(LPath: p (== i)) (make-Path p x)]))
             (make-Empty)))
          dty dbound)]
    [(Values: (list (Result: ts lfs los) ...))
     (ret ts
          (for/list ([lf lfs]) 
            (or
             (and (null? formals)
                  (match lf
                    [(LFilterSet: lf+ lf-)
                     (combine (if (memq (make-LBot) lf+) (list (make-Bot)) (list))
                              (if (memq (make-LBot) lf-) (list (make-Bot)) (list)))]))
             (merge-filter-sets
              (for/list ([x formals] [i (in-naturals)])
                (apply-filter (split-lfilters lf i) Univ (make-Path null x))))))
          (for/list ([lo los])
            (or 
             (for/or ([x formals] [i (in-naturals)])
               (match lo
                 [(LEmpty:) #f]
                 [(LPath: p (== i)) (make-Path p x)]))
             (make-Empty))))]))

(define (tc-results->values tc)
  (match tc
    [(tc-results: ts) (-values ts)]))
