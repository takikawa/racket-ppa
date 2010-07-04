#lang scheme/base

(require scheme/match scheme/contract)
(require "rep-utils.ss" "free-variance.ss")

(define Filter/c
  (flat-named-contract
   'Filter
   (λ (e)
     (and (Filter? e) (not (NoFilter? e)) (not (FilterSet? e))))))

(define LatentFilter/c
  (flat-named-contract
   'LatentFilter
   (λ (e)
     (and (LatentFilter? e) (not (LFilterSet? e))))))

(provide Filter/c LatentFilter/c FilterSet/c LatentFilterSet/c index/c)

(df Bot () [#:fold-rhs #:base])

(df TypeFilter ([t Type?] [p (listof PathElem?)] [v identifier?])
  [#:intern (list t p (hash-id v))]
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*TypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

(df NotTypeFilter ([t Type?] [p (listof PathElem?)] [v identifier?])
  [#:intern (list t p (hash-id v))]
  [#:frees (combine-frees (map free-vars* (cons t p)))
	   (combine-frees (map free-idxs* (cons t p)))]
  [#:fold-rhs (*NotTypeFilter (type-rec-id t) (map pathelem-rec-id p) v)])

;; implication
(df ImpFilter ([a (listof Filter/c)] [c (listof Filter/c)]))

(df FilterSet (thn els)
     [#:frees (combine-frees (map free-vars* (append thn els)))
	      (combine-frees (map free-idxs* (append thn els)))]
     [#:fold-rhs (*FilterSet (map filter-rec-id thn) (map filter-rec-id els))]
     [#:contract (->d ([t (cond [(ormap Bot? t)
                                 (list/c Bot?)]
                                [(ormap Bot? e)
                                 (list/c)]
                                [else (listof Filter/c)])]
                       [e (cond [(ormap Bot? e)
                                 (list/c Bot?)]
                                [(ormap Bot? t)
                                 (list/c)]
                                [else (listof Filter/c)])])
                      ()
                      [result FilterSet?])])

;; represents no info about the filters of this expression
;; should only be used for parsing type annotations and expected types
(df NoFilter () [#:fold-rhs #:base])

(define index/c (or/c natural-number/c keyword?))

(dlf LBot () [#:fold-rhs #:base])

(dlf LTypeFilter ([t Type?] [p (listof PathElem?)] [idx index/c])
  [#:frees (lambda (frees*) (combine-frees (map (compose make-invariant frees*) (cons t p))))]
  [#:fold-rhs (*LTypeFilter (type-rec-id t) (map pathelem-rec-id p) idx)])

(dlf LNotTypeFilter ([t Type?] [p (listof PathElem?)] [idx index/c])
  [#:frees (lambda (frees*) (combine-frees (map (compose make-invariant frees*) (cons t p))))]
  [#:fold-rhs (*LNotTypeFilter (type-rec-id t) (map pathelem-rec-id p) idx)])

;; implication
(df LImpFilter ([a (listof LatentFilter/c)] [c (listof LatentFilter/c)]))

(dlf LFilterSet (thn els)
     [#:frees (combine-frees (map free-vars* (append thn els)))
	      (combine-frees (map free-idxs* (append thn els)))]
     [#:fold-rhs (*LFilterSet (map latentfilter-rec-id thn) (map latentfilter-rec-id els))]
     [#:contract (->d ([t (cond [(ormap LBot? t)
                                 (list/c LBot?)]
                                [(ormap LBot? e)
                                 (list/c)]
                                [else (listof LatentFilter/c)])]
                       [e (cond [(ormap LBot? e)
                                 (list/c LBot?)]
                                [(ormap LBot? t)
                                 (list/c)]
                                [else (listof LatentFilter/c)])])
                      ()
                      [result LFilterSet?])])

(define FilterSet/c
  (flat-named-contract
   'FilterSet
   (λ (e) (or (FilterSet? e) (NoFilter? e)))))

(define LatentFilterSet/c
  (flat-named-contract
   'LatentFilterSet
   (λ (e) (or (LFilterSet? e)))))