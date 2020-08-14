#lang racket/base
(require racket/match
         racket/struct
         racket/list
         syntax/stx
         "context.rkt"
         "stx-util.rkt")
(provide (all-defined-out))

;; This module has two implementations of the VT data structure: a Lazy version
;; that keeps a history of syntax adjustments and traces the complete history on
;; each lookup, and an Eager version that maintains a single lookup table and
;; eagerly updates it on each adjustment.

;; The Lazy version is quite slow except for tiny examples, but the code is
;; left (for now) for reference and (if CHECK-WITH-LAZY? is set to true) to
;; check the correctness of the Eager version.

(define CHECK-WITH-LAZY? #f)

(module util racket/base
  (provide (all-defined-out))
  (define (stx? x) (or (syntax? x) (pair? x) (null? x))))
(require 'util)

;; In general, this file assumes that we only care about finding non-tainted
;; syntax. So it stops searching after seeing armed/tainted term.

;; ============================================================

;; A VT is (vt:zoom (Listof Path) EagerVT LazyVT/#f)
(struct vt:zoom (paths evt lvt) #:prefab)

;; vt-zoom : VT Path -> VT
(define (vt-zoom vt path)
  (match vt [(vt:zoom ps evt lvt) (vt:zoom (cons path ps) evt lvt)]))

;; vt-unzoom : VT Path
(define (vt-unzoom vt p)
  (match vt
    [(vt:zoom (cons (== p) ps) evt lvt) (vt:zoom ps evt lvt)]
    [_ (error 'vt-unzoom "failed: ~e, ~e" p vt)]))

;; vt-track : Stx Stx VT [Any] -> VT
(define (vt-track from to in [type #f])
  (cond [(eq? from to) in]
        [else (match in
                [(vt:zoom ps evt lvt)
                 (vt:zoom ps
                          (evt-track from to evt)
                          (and CHECK-WITH-LAZY? (lvt-track from to lvt)))])]))

;; vt-base : Stx -> VT
(define (vt-base stx)
  (vt:zoom null (evt-base stx) (and CHECK-WITH-LAZY? (lvt-base stx))))

;; vt-merge-at-path : Stx/VT Path VT -> VT
(define (vt-merge-at-path vt path sub-vt)
  (let ([sub-vt (if (stx? sub-vt) (vt-base sub-vt) sub-vt)])
    (if (equal? path null)
        sub-vt
        (match vt
          [(vt:zoom zoom-ps evt lvt)
           (match-define (vt:zoom '() sub-evt sub-lvt) sub-vt)
           (let ([path (foldl append path zoom-ps)])
             (vt:zoom zoom-ps
                      (evt-merge-at-path evt path sub-evt)
                      (and CHECK-WITH-LAZY? (lvt-merge-at-path lvt path sub-lvt))))]
          [(? stx? stx) (vt-merge-at-path (vt-base stx) path sub-vt)]))))

;; vt-seek : Stx VT -> (Listof Path)
;; Handles zoomed VTs. The zoom-paths are removed from the prefix of each result
;; (top = most-recent zoom is removed last). Results that do not extend the
;; prefixes are discarded.
(define (vt-seek want vt)
  (match vt
    [(vt:zoom zoom-ps evt lvt)
     (define e-results (evt-seek want evt))
     (when CHECK-WITH-LAZY?
       (define l-results (lvt-seek want lvt))
       (unless (equal? e-results l-results)
         #;(set! the-vt-error (list 'seek want vt))
         (error 'vt-seek "mismatch in results: e=> ~e, l=> ~e" e-results l-results)))
     (define (cut-prefix p) (foldr path-cut-prefix p zoom-ps))
     (filter list? (map cut-prefix e-results))]))

;; vt->stx : VT Path -> Stx
;; Note: ignores tracking, only uses base, patches, and zoom.
(define (vt->stx vt)
  (match vt
    [(vt:zoom zoom-ps evt lvt)
     (define e-stx (evt->stx evt))
     (when CHECK-WITH-LAZY?
       (define l-stx (lvt->stx lvt))
       (unless (equal? (stx->datum e-stx) (stx->datum l-stx))
         #;(set! the-vt-error (list 'to-stx vt))
         (error 'vt->stx "mismatch in results: e=> ~e, l=> ~e" e-stx l-stx)))
     (foldr (lambda (p stx) (path-get stx p)) e-stx zoom-ps)]))

;; ----------------------------------------

#|
(define the-vt-error #f) ;; mutated
(define (check-vt who vt)
  (match vt
    [(vt:zoom zoom-ps (vt:eager estx eh) lvt)
     (unless (equal? (stx->datum estx) (stx->datum (lvt->stx lvt)))
       (set! the-vt-error (list who vt))
       (error who "stx mismatch: e=> ~e, l=> ~e" (stx->datum estx) (stx->datum (lvt->stx lvt))))
     (for ([(stx rpath) (in-hash eh)] #:when (list? rpath))
       (define lpaths (lvt-seek stx lvt))
       (unless (member (reverse rpath) lpaths)
         (set! the-vt-error (list who vt))
         (error who "lookup mismatch on ~e, e=> ~e, l=> ~e" stx (reverse rpath) lpaths)))])
  vt)
|#

;; ----------------------------------------

;; An EagerVT is (vt:eager Stx Hash[Syntax => EagerResult])
;; where EagerResult = ReversedPath | (delayed Syntax) -- see extend-eager-hash.
;; The abbreviation "evt" has no relation to synchronizable events.
(struct vt:eager (stx h) #:prefab)
(struct delayed (stx) #:prefab)

;; evt-base : Stx -> EagerVT
(define (evt-base stx)
  (define h
    (let loop ([stx stx] [rpath null] [h '#hash()])
      (cond [(syntax? stx)
             (let ([h (hash-set h stx rpath)])
               (cond [(syntax-armed/tainted? stx) h]
                     [else (loop (syntax-e stx) rpath h)]))]
            [(pair? stx)
             (let ([h h])
               (loop (car stx) (path-add-car rpath)
                     (loop (cdr stx) (path-add-cdr rpath) h)))]
            ;; FIXME: vector, box, prefab
            [else h])))
  (vt:eager stx h))

;; evt-track : Stx Stx EagerVT -> EagerVT
(define (evt-track from to in)
  (match in
    [(vt:eager stx h)
     (vt:eager stx (extend-eager-hash from to h))]))

;; Case: FROM --> TO (neither arm nor disarm)
;; - if FROM is visible at P, then map TO to P and recur
;; - if FROM is not visible, then do not map TO but just recur
;; Case: FROM ---arm--> TO
;; - if FROM was previously visible at P, then map TO to P
;; - if FROM was not previously visible, then map TO to (delayed FROM) because
;;   FROM might contain relevant subterms, and we need to connect them when TO
;;   eventually gets disarmed
;; Case: FROM -disarm-> TO
;; - if FROM was previously visible at P,
;;   then map TO to P and map subterms of TO to extensions of P
;; - if FROM was mapped to (delayed FROM'), then recur on (FROM' --> TO)
;; - if FROM was not previously visible, stop

;; Invariant: STX is mapped to (delayed STX*) only if STX is armed.
;; But armed STX may also be mapped to path, if present in base term.

(define (extend-eager-hash from to old-h)
  (define (stx-e x) (if (syntax? x) (syntax-e x) x))

  (define (hash-forward h from to)
    (cond [(syntax? to)
           (match (hash-ref old-h from #f)
             [(? list? rpath) (hash-set h to rpath)]
             [(delayed from*) (hash-set h to (delayed from*))]
             ['#f h])]
          [else h]))

  (define (loop from to h)
    (cond [(and (syntax? from) (syntax? to))
           (cond [(or (syntax-tainted? from) (syntax-tainted? to))
                  h]
                 [(syntax-armed? to)
                  (cond [(syntax-armed? from) ;; no arm/disarm
                         (hash-forward h from to)]
                        [else ;; arm
                         (match (hash-ref old-h from #f)
                           [(? list? rpath) (hash-set h to rpath)]
                           ['#f (hash-set h to (delayed from))])])]
                 [(syntax-armed? from) ;; disarm
                  (match (hash-ref old-h from #f)
                    [(? list? rpath) (handle-disarm from to rpath h)]
                    [(delayed from*) (loop from* to h)]
                    ['#f h])]
                 [else ;; no arm/disarm
                  (let ([h (hash-forward h from to)])
                    (datumloop (syntax-e from) (syntax-e to) h))])]
          [(or (syntax? from) (syntax? to))
           (loop (stx-e from) (stx-e to) h)]
          [else (datumloop from to h)]))

  (define (datumloop from to h)
    (cond [(and (pair? from) (pair? to))
           (loop (car from) (car to)
                 (loop (cdr from) (cdr to) h))]
          [else h]))

  (define (handle-disarm from to rpath h)
    (let dloop ([to to] [rpath rpath] [h h])
      (cond [(syntax? to)
             (let ([h (hash-set h to rpath)])
               (cond [(syntax-tainted? to) h]
                     [(syntax-armed? to) h]
                     [else (dloop (syntax-e to) rpath h)]))]
            [(pair? to)
             (let ([h h])
               (dloop (car to) (path-add-car rpath)
                      (dloop (cdr to) (path-add-cdr rpath) h)))]
            [else h])))

  (loop from to old-h))

;; evt-merge-at-path : EagerVT Path EagerVT -> EagerVT
(define (evt-merge-at-path evt path sub-evt)
  (match-define (vt:eager stx h) evt)
  (match-define (vt:eager sub-stx sub-h) sub-evt)
  (vt:eager (path-replace stx path sub-stx #:resyntax? #t)
            (hash-add-at-path (hash-remove-with-prefix/suffix h path) path sub-h)))

(define (hash-remove-with-prefix/suffix h prefix)
  (for/fold ([h h]) ([(k rpath) (in-hash h)] #:when (list? rpath))
    (if (rpath-prefix/suffix? prefix rpath) (hash-remove h k) h)))

(define (rpath-prefix/suffix? prefix rpath)
  (define path (reverse rpath))
  (or (path-prefix? prefix path) (path-prefix? path prefix)))

(define (hash-add-at-path h prefix sub-h)
  (define rprefix (reverse prefix))
  (for/fold ([h h]) ([(k sub-rpath) (in-hash sub-h)])
    (hash-set h k (if (list? sub-rpath) (append sub-rpath rprefix) sub-rpath))))

;; evt->stx : EagerVT Path -> Stx
(define (evt->stx evt)
  (match evt [(vt:eager stx _) stx]))

;; evt-seek : EagerVT : Stx EagerVT -> (Listof Path)
(define (evt-seek want evt)
  (match evt
    [(vt:eager _ h)
     (match (hash-ref h want #f)
       [(? list? rpath) (list (reverse rpath))]
       [_ null])]))

;; ------------------------------------------------------------

;; A LazyVT is one of
;; - (vt:base Stx Hash)             -- the term itself
;; - (vt:track Stx Stx LazyVT Hash) -- scope/arm/etc FROM, producing TO, within IN
;; - (vt:patch Path LazyVT LazyVT)  -- replace subterm at AT with TO, within IN
(struct vt:base (stx h) #;#:prefab
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (self) 'vt:base)
                                  (match-lambda [(vt:base stx _) (list stx)])))
(struct vt:track (from to in h) #;#:prefab
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (self) 'vt:track)
                                  (match-lambda [(vt:track from to in _) (list from to in)])))
(struct vt:patch (at to in) #:prefab)

;; If vt ends in [e1->e2], and we want to search for e, then the naive approach
;; is to search for e in e2, fetch the corresponding (ie, same path) subterm of
;; e1, and continue searching for *that* term in the rest of vt. That is in fact
;; how we handle *rename* (ie, scope) changes. But if [e1->e2] is a disarm step,
;; then we cannot fetch a subterm from within the (armed) term e1. So we add the
;; path to a delayed *narrowing* and search for e1; once we find the pre-armed
;; version of e1, we apply the narrowing. Note that the search for e1 might
;; itself evolve through adjustments---that's fine.

(module list-monad racket/base
  (require racket/match)
  (provide (all-defined-out))
  ;; List monad
  ;; type (M X) = (Listof X)
  (define (disj a b) (if (null? b) a (append a b)))
  (define return list)
  (define (bind1 c f) (for*/list ([x (in-list c)]) (f x)))
  (define (bind c f) (for*/list ([x (in-list c)] [y (in-list (f x))]) y))
  (define (fail) null)
  (define (to-list c) c)
  ;; Monad syntax
  (define-syntax do
    (syntax-rules (<- =)
      [(_ ([p <- rhs] . rest) . body)
       (bind rhs (match-lambda [p (do rest . body)]))]
      [(_ ([p = rhs] . rest) . body)
       (match rhs [p (do rest . body)])]
      [(_ () . body) (let () . body)])))
(require 'list-monad)

;; lvt-base : Stx -> LazyVT
(define (lvt-base stx)
  (define h
    (let loop ([stx stx] [rpath null] [h '#hasheq()])
      (cond [(syntax? stx)
             (let ([h (hash-set h stx (reverse rpath))])
               (cond [(syntax-armed/tainted? stx) h]
                     [else (loop (syntax-e stx) rpath h)]))]
            [(pair? stx)
             (let ([h h])
               (loop (car stx) (path-add-car rpath)
                     (loop (cdr stx) (path-add-cdr rpath) h)))]
            [else h])))
  (vt:base stx h))

;; lvt-track : Stx Stx LazyVT -> LazyVT
(define (lvt-track from to in)
  (vt:track from to in (make-track-hash from to)))

;; hash maps Syntax => (cons Stx Path)
(define (make-track-hash from to)
  (define h (make-hasheq))
  (define (loop to from rpath)
    (cond [(syntax? to)
           (when (syntax? from)
             (hash-set! h to (cons from (reverse rpath))))
           (unless (syntax-armed/tainted? to)
             (loop (syntax-e to) from rpath))]
          [(pair? to)
           (cond [(pair? from) ;; rpath = null
                  (loop (car to) (car from) rpath)
                  (loop (cdr to) (cdr from) rpath)]
                 [(and (syntax? from) (syntax-armed/tainted? from))
                  (loop (car to) from (path-add-car rpath))
                  (loop (cdr to) from (path-add-cdr rpath))]
                 [(syntax? from)
                  (loop to (syntax-e from) rpath)]
                 [else (error 'make-track-hash "mismatch: ~e, ~e" from to)])]
          ;; FIXME: vector, box, prefab
          [else (void)]))
  (begin (loop to from null) h))

;; lvt-merge-at-path : LazyVT Path LazyVT -> LazyVT
(define (lvt-merge-at-path vt path sub-vt)
  (vt:patch path sub-vt vt))

;; lvt->stx : LazyVT Path -> Stx
(define (lvt->stx vt)
  (let loop ([vt vt])
    (match vt
      [(vt:base stx _)
       stx]
      [(vt:track _ _ in _)
       (loop in)]
      [(vt:patch at to in)
       (path-replace (loop in) at (loop to) #:resyntax? #f)])))

;; lvt-seek : Stx LazyVT -> (Listof Path)
(define (lvt-seek want lvt)
  (to-list (seek1 want lvt null)))

;; A Seeking is (seeking Stx Path) -- represents an intermediate search point.
(struct seeking (want narrow) #:prefab)

;; make-seeking : Stx Path -> Seeking
;; Discharges as much of the delayed narrowing as possible, then wraps in seeking.
(define (make-seeking want narrow)
  (define-values (want* narrow*)
    (path-get-until want narrow (lambda (x) (and (syntax? x) (syntax-armed/tainted? x)))))
  (seeking want* narrow*))

;; seek1 : Stx LazyVT Path -> (M Path)
;; Find the path(s) of the NARROW subterm of WANT in VT.
(define (seek1 want vt narrow)
  (seek* (list (make-seeking want narrow)) vt))

;; seek* : Stx LazyVT Path -> (M Path)
;; PRE: narrow is null or want is armed (or tainted)
(define (seek* ss vt)
  (define unique-ss (remove-duplicates ss))
  (match vt
    [(vt:base _ h)
     ;; I think narrow could be non-empty here, if stx already had armed terms
     ;; and vt only tracked their disarming. (FIXME: test)
     (do ([(seeking want narrow) <- unique-ss])
         (cond [(hash-ref h want #f)
                => (lambda (path) (return (path-append path narrow)))]
               [else (fail)]))]
    [(vt:track from to in (? hash? h))
     ;; Possibilities:
     ;; - WANT is in TO (and corresponding subterm of FROM is in IN)
     ;; - WANT is in IN -- FIXME: add strict replacement flag?
     (define next-ss
       (disj (do ([(seeking want narrow) <- unique-ss])
                 (cond [(hash-ref h want #f)
                        => (match-lambda
                             [(cons want* narrow*)
                              (return (make-seeking want* (append narrow* narrow)))])]
                       [else (fail)]))
             unique-ss))
     (seek* next-ss in)]
    [(vt:track from to in _)
     ;; NOTE: unreachable case, left for comparison, history, etc
     ;; FIXME: untested...
     (define next-ss
       (disj (do ([(seeking want narrow) <- unique-ss]
                  [path1 <- (seek1 want to narrow)])
                 (return (make-seeking from (path-append path1 narrow))))
             unique-ss))
     (seek* next-ss in)]
    [(vt:patch at to in)
     ;; Possibilities:
     ;; - WANT is in TO
     ;; - WANT is in IN and no part of it is replaced
     ;; - WANT is in IN[AT:=TO] (but not IN) -- I think this is impossible in practice (???)
     ;;   or at least, we won't act on it unless that subterm has T honesty
     (disj (do ([p <- (seek* unique-ss in)])
               (if (path-prefix? at p) (fail) (return p)))
           (do ([p <- (seek* unique-ss to)])
               (return (path-append at p))))]))

;; path-append : Path Path -> Path
(define (path-append a b) (append a b))
