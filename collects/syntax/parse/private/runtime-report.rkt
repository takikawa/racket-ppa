#lang racket/base
(require racket/list
         racket/format
         syntax/stx
         racket/struct
         syntax/srcloc
         "minimatch.rkt"
         syntax/parse/private/residual
         "kws.rkt")
(provide call-current-failure-handler
         current-failure-handler
         invert-failure
         maximal-failures
         invert-ps
         ps->stx+index)

#|
TODO: given (expect:thing _ D _ R) and (expect:thing _ D _ #f),
  simplify to (expect:thing _ D _ #f)
  thus, "expected D" rather than "expected D or D for R" (?)
|#

#|
Note: there is a cyclic dependence between residual.rkt and this module,
broken by a lazy-require of this module into residual.rkt
|#

(define (call-current-failure-handler ctx fs)
  (call-with-values (lambda () ((current-failure-handler) ctx fs))
    (lambda vals
      (error 'current-failure-handler
             "current-failure-handler: did not escape, produced ~e"
             (case (length vals)
               ((1) (car vals))
               (else (cons 'values vals)))))))

(define (default-failure-handler ctx fs)
  (handle-failureset ctx fs))

(define current-failure-handler
  (make-parameter default-failure-handler))


;; ============================================================
;; Processing failure sets

#|
We use progress to select the maximal failures and determine the syntax
they're complaining about. After that, we no longer care about progress.

Old versions of syntax-parse (through 6.4) grouped failures into
progress-equivalence-classes and generated reports by class, but only showed
one report. New syntax-parse just mixes all maximal failures together and
deals with the fact that they might not be talking about the same terms.
|#

;; handle-failureset : (list Symbol/#f Syntax) FailureSet -> escapes
(define (handle-failureset ctx fs)
  (define inverted-fs (map invert-failure (reverse (flatten fs))))
  (define maximal-classes (maximal-failures inverted-fs))
  (define ess (map failure-expectstack (append* maximal-classes)))
  (define report (report/sync-shared ess))
  ;; Hack: alternative to new (primitive) phase-crossing exn type is to store
  ;; extra information in exn continuation marks. Currently for debugging only.
  (with-continuation-mark 'syntax-parse-error
    (hasheq 'raw-failures fs
            'maximal maximal-classes)
    (error/report ctx report)))

;; An RFailure is (failure IPS RExpectList)

;; invert-failure : Failure -> RFailure
(define (invert-failure f)
  (match f
    [(failure ps es)
     (failure (invert-ps ps) (invert-expectstack es (ps->stx+index ps)))]))

;; A Report is (report String (Listof String) Syntax/#f Syntax/#f)
(define-struct report (message context stx within-stx) #:prefab)


;; ============================================================
;; Progress

;; maximal-failures : (listof InvFailure) -> (listof (listof InvFailure))
(define (maximal-failures fs)
  (maximal/progress
   (for/list ([f (in-list fs)])
     (cons (failure-progress f) f))))

#|
Progress ordering
-----------------

Nearly a lexicographic generalization of partial order on frames.
  (( CAR < CDR ) || stx ) < POST )
  - stx incomparable except with self

But ORD prefixes are sorted out (and discarded) before comparison with 
rest of progress. Like post, ord comparable only w/in same group:
  - (ord g n1) < (ord g n2) if n1 < n2
  - (ord g1 n1) || (ord g2 n2) when g1 != g2


Progress equality
-----------------

If ps1 = ps2 then both must "blame" the same term,
ie (ps->stx+index ps1) = (ps->stx+index ps2).
|#

;; An Inverted PS (IPS) is a PS inverted for easy comparison.
;; An IPS may not contain any 'opaque frames.

;; invert-ps : PS -> IPS
;; Reverse and truncate at earliest 'opaque frame.
(define (invert-ps ps)
  (reverse (ps-truncate-opaque ps)))

;; ps-truncate-opaque : PS -> PS
;; Returns maximal tail with no 'opaque frame.
(define (ps-truncate-opaque ps)
  (let loop ([ps ps] [acc ps])
    ;; acc is the biggest tail that has not been seen to contain 'opaque
    (cond [(null? ps) acc]
          [(eq? (car ps) 'opaque)
           (loop (cdr ps) (cdr ps))]
          [else (loop (cdr ps) acc)])))

;; maximal/progress : (listof (cons IPS A)) -> (listof (listof A))
;; Eliminates As with non-maximal progress, then groups As into
;; equivalence classes according to progress.
(define (maximal/progress items)
  (cond [(null? items)
         null]
        [(null? (cdr items))
         (list (list (cdr (car items))))]
        [else
         (let loop ([items items] [non-ORD-items null])
           (define-values (ORD non-ORD)
             (partition (lambda (item) (ord? (item-first-prf item))) items))
           (cond [(pair? ORD)
                  (loop (maximal-prf1/ord ORD) (append non-ORD non-ORD-items))]
                 [else
                  (maximal/prf1 (append non-ORD non-ORD-items))]))]))

;; maximal/prf1 : (Listof (Cons IPS A) -> (Listof (Listof A))
(define (maximal/prf1 items)
  (define-values (POST rest1)
    (partition (lambda (item) (eq? 'post (item-first-prf item))) items))
  (cond [(pair? POST)
         (maximal/progress (map item-pop-prf POST))]
        [else
         (define-values (STX rest2)
           (partition (lambda (item) (syntax? (item-first-prf item))) rest1))
         (define-values (CDR rest3)
           (partition (lambda (item) (exact-integer? (item-first-prf item))) rest2))
         (define-values (CAR rest4)
           (partition (lambda (item) (eq? 'car (item-first-prf item))) rest3))
         (define-values (NULL rest5)
           (partition (lambda (item) (eq? '#f (item-first-prf item))) rest4))
         (unless (null? rest5)
           (error 'syntax-parse "INTERNAL ERROR: bad progress: ~e\n" rest5))
         (cond [(pair? CDR)
                (define leastCDR (apply min (map item-first-prf CDR)))
                (append
                 (maximal/stx STX)
                 (maximal/progress (map (lambda (item) (item-pop-prf-ncdrs item leastCDR)) CDR)))]
               [(pair? CAR)
                (append
                 (maximal/stx STX)
                 (maximal/progress (map item-pop-prf CAR)))]
               [(pair? STX)
                (maximal/stx STX)]
               [(pair? NULL)
                (list (map cdr NULL))]
               [else null])]))

;; maximal-prf1/ord : (NEListof (Cons IPS A)) -> (NEListof (Cons IPS A))
;; PRE: each item has ORD first frame
;; Keep only maximal by first frame and pop first frame from each item.
(define (maximal-prf1/ord items)
  ;; groups : (NEListof (NEListof (cons A IPS)))
  (define groups (group-by (lambda (item) (ord-group (item-first-prf item))) items))
  (append*
   (for/list ([group (in-list groups)])
     (define group* (filter-max group (lambda (item) (ord-index (item-first-prf item)))))
     (map item-pop-prf group*))))

;; maximal/stx : (NEListof (cons IPS A)) -> (NEListof (NEListof A))
;; PRE: Each IPS starts with a stx frame.
(define (maximal/stx items)
  ;; groups : (Listof (Listof (cons IPS A)))
  (define groups (group-by item-first-prf items))
  (append*
   (for/list ([group (in-list groups)])
     (maximal/progress (map item-pop-prf group)))))

;; filter-max : (Listof X) (X -> Nat) -> (Listof X)
(define (filter-max xs x->nat)
  (let loop ([xs xs] [nmax -inf.0] [r-keep null])
    (cond [(null? xs)
           (reverse r-keep)]
          [else
           (define n0 (x->nat (car xs)))
           (cond [(> n0 nmax)
                  (loop (cdr xs) n0 (list (car xs)))]
                 [(= n0 nmax)
                  (loop (cdr xs) nmax (cons (car xs) r-keep))]
                 [else
                  (loop (cdr xs) nmax r-keep)])])))

;; item-first-prf : (cons IPS A) -> prframe/#f
(define (item-first-prf item)
  (define ips (car item))
  (and (pair? ips) (car ips)))

;; item-split-ord : (cons IPS A) -> (cons IPS (cons IPS A))
(define (item-split-ord item)
  (define ips (car item))
  (define a (cdr item))
  (define-values (rest-ips r-ord)
    (let loop ([ips ips] [r-ord null])
      (cond [(and (pair? ips) (ord? (car ips)))
             (loop (cdr ips) (cons (car ips) r-ord))]
            [else (values ips r-ord)])))
  (list* (reverse r-ord) rest-ips a))

;; item-pop-prf : (cons IPS A) -> (cons IPS A)
(define (item-pop-prf item)
  (let ([ips (car item)]
        [a (cdr item)])
    (cons (cdr ips) a)))

;; item-pop-prf-ncdrs : (cons IPS A) -> (cons IPS A)
;; Assumes first frame is nat > ncdrs.
(define (item-pop-prf-ncdrs item ncdrs)
  (let ([ips (car item)]
        [a (cdr item)])
    (cond [(= (car ips) ncdrs) (cons (cdr ips) a)]
          [else (cons (cons (- (car ips) ncdrs) (cdr ips)) a)])))

;; ps->stx+index : Progress -> (cons Syntax Nat)
;; Gets the innermost stx that should have a real srcloc, and the offset
;; (number of cdrs) within that where the progress ends.
(define (ps->stx+index ps)
  (define (interp ps)
    (match ps
      [(cons (? syntax? stx) _) stx]
      [(cons 'car parent)
       (let* ([d (interp parent)]
              [d (if (syntax? d) (syntax-e d) d)])
         (cond [(pair? d) (car d)]
               [(vector? d) (vector->list d)]
               [(box? d) (unbox d)]
               [(prefab-struct-key d) (struct->list d)]
               [else (error 'ps->stx+index "INTERNAL ERROR: unexpected: ~e" d)]))]
      [(cons (? exact-positive-integer? n) parent)
       (for/fold ([stx (interp parent)]) ([i (in-range n)])
         (stx-cdr stx))]
      [(cons (? ord?) parent)
       (interp parent)]
      [(cons 'post parent)
       (interp parent)]))
  (let ([ps (ps-truncate-opaque ps)])
    (match ps
      [(cons (? syntax? stx) _)
       (cons stx 0)]
      [(cons 'car parent)
       (cons (interp ps) 0)]
      [(cons (? exact-positive-integer? n) parent)
       (cons (interp parent) n)]
      [(cons (? ord?) parent)
       (ps->stx+index parent)]
      [(cons 'post parent)
       (ps->stx+index parent)])))


;; ============================================================
;; Expectation simplification

;; normalize-expectstack : ExpectStack StxIdx -> ExpectList
;; Converts to list, converts expect:thing term rep, and truncates
;; expectstack after opaque (ie, transparent=#f) frames.
(define (normalize-expectstack es stx+index [truncate-opaque? #t])
  (reverse (invert-expectstack es stx+index truncate-opaque?)))

;; invert-expectstack : ExpectStack StxIdx -> RExpectList
;; Converts to reversed list, converts expect:thing term rep,
;; and truncates expectstack after opaque (ie, transparent=#f) frames.
(define (invert-expectstack es stx+index [truncate-opaque? #t])
  (let loop ([es es] [acc null])
    (match es
      ['#f acc]
      [(expect:thing ps desc tr? role rest-es)
       (cond [(and truncate-opaque? (not tr?))
              (loop rest-es (cons (expect:thing #f desc #t role (ps->stx+index ps)) null))]
             [else
              (loop rest-es (cons (expect:thing #f desc tr? role (ps->stx+index ps)) acc))])]
      [(expect:message message rest-es)
       (loop rest-es (cons (expect:message message stx+index) acc))]
      [(expect:atom atom rest-es)
       (loop rest-es (cons (expect:atom atom stx+index) acc))]
      [(expect:literal literal rest-es)
       (loop rest-es (cons (expect:literal literal stx+index) acc))]
      [(expect:proper-pair first-desc rest-es)
       (loop rest-es (cons (expect:proper-pair first-desc stx+index) acc))])))

;; expect->stxidx : Expect -> StxIdx
(define (expect->stxidx e)
  (cond [(expect:thing? e) (expect:thing-next e)]
        [(expect:message? e) (expect:message-next e)]
        [(expect:atom? e) (expect:atom-next e)]
        [(expect:literal? e) (expect:literal-next e)]
        [(expect:proper-pair? e) (expect:proper-pair-next e)]
        [(expect:disj? e) (expect:disj-next e)]))

#| Simplification

A list of ExpectLists represents a tree, with shared tails meaning shared
branches of the tree. We need a "reasonable" way to simplify it to a list to
show to the user. Here we develop "reasonable" by example. (It would be nice,
of course, to also have some way of exploring the full failure trees.)

Notation: [A B X] means an ExpectList with class/description A at root and X
at leaf. If the term sequences differ, write [t1:A ...] etc.

Options:
  (o) = "old behavior (through 6.4)"
  (f) = "first divergence"
  (s) = "sync on shared"

Case 1: [A B X], [A B Y]

  This is nearly the ideal situation: report as

    expected X or Y, while parsing B, while parsing A

Case 2: [A X], [A]

  For example, matching #'1 as (~describe A (x:id ...)) yields [A], [A '()],
  but we don't want to see "expected ()".

  So simplify to [A]---that is, drop X.

But there are other cases that are more problematic.

Case 3:  [t1:A t2:B t3:X], [t1:A t2:C t3:Y]

  Could report as:
  (o) expected X for t3, while parsing t2 as B, while parsing t1 as A (also other errors)
  (f) expected B or C for t2, while parsing t1 as A
  (x) expected X or Y for t3, while parsing t2 as B or C, while parsing t1 as A

  (o) is not good
  (b) loses the most specific error information
  (x) implies spurious contexts (eg, X while parsing C)

  I like (b) best for this situation, but ...

Case 4: [t1:A t2:B t4:X], [t1:A t3:C t4:Y]

  Could report as:
  (f') expected B or C, while parsing t1 as A
  (s) expected X or Y for t4, while ..., while parsing t1 as A
  (f) expected A for t1

  (f') is problematic, since terms are different!
  (s) okay, but nothing good to put in that ... space
  (f) loses a lot of information

Case 5: [t1:A t2:B t3:X], [t1:A t4:C t5:Y]

  Only feasible choice (no other sync points):
  (f,s) expected A for t1

Case 6: [t1:A _ t2:B t3:X], [t1:A _ t2:C t3:Y]

  Could report as:
  (s') expected X or Y for t3, while parsing t2 as B or C, while ..., while parsing t1 as A
  (s) expected X or Y for t3, while ..., while parsing t1 as A

  (s') again implies spurious contexts, bad
  (s) okay

Case 7: [_ t2:B t3:C _], [_ t3:C t2:B _]

  Same frames show up in different orders. (Can this really happen? Probably,
  with very weird uses of ~parse.)

--

This suggests the following new algorithm based on (s):
- Step 1: emit an intermediate "unified" expectstack (extended with "..." markers)
  - make a list (in order) of frames shared by all expectstacks
  - emit those frames with "..." markers if (sometimes) unshared stuff between
  - continue processing with the tails after the last shared frame:
  - find the last term shared by all expectstacks (if any)
  - find the last frame for that term for each expectstack
  - combine in expect:disj and emit
- Step 2:
  - remove trailing and collapse adjacent "..." markers

|#

;; report* : (NEListof RExpectList) ((NEListof (NEListof RExpectList)) -> ExpectList)
;;        -> Report
(define (report* ess handle-divergence)
  (define es ;; ExpectList
    (let loop ([ess ess] [acc null])
      (cond [(ormap null? ess) acc]
            [else
             (define groups (group-by car ess))
             (cond [(singleton? groups)
                    (define group (car groups))
                    (define frame (car (car group)))
                    (loop (map cdr group) (cons frame acc))]
                   [else ;; found point of divergence
                    (append (handle-divergence groups) acc)])])))
  (define stx+index (if (pair? es) (expect->stxidx (car es)) (cons #f 0)))
  (report/expectstack (clean-up es) (car stx+index) (cdr stx+index)))

;; clean-up : ExpectList -> ExpectList
;; Remove leading and collapse adjacent '... markers
(define (clean-up es)
  (if (and (pair? es) (eq? (car es) '...))
      (clean-up (cdr es))
      (let loop ([es es])
        (cond [(null? es) null]
              [(eq? (car es) '...)
               (cons '... (clean-up es))]
              [else (cons (car es) (loop (cdr es)))]))))

;; --

;; report/first-divergence : (NEListof RExpectList) -> Report
;; Generate a single report, using frames from root to first divergence.
(define (report/first-divergence ess)
  (report* ess handle-divergence/first))

;; handle-divergence/first : (NEListof (NEListof RExpectList)) -> ExpectList
(define (handle-divergence/first ess-groups)
  (define representative-ess (map car ess-groups))
  (define first-frames (map car representative-ess))
  ;; Do all of the first frames talk about the same term?
  (cond [(all-equal? (map expect->stxidx first-frames))
         (list (expect:disj first-frames #f))]
        [else null]))

;; --

;; report/sync-shared : (NEListof RExpectList) -> Report
;; Generate a single report, syncing on shared frames (and later, terms).
(define (report/sync-shared ess)
  (report* ess handle-divergence/sync-shared))

;; handle-divergence/sync-shared : (NEListof (NEListof RExpectList)) -> ExpectList
(define (handle-divergence/sync-shared ess-groups)
  (define ess (append* ess-groups)) ;; (NEListof RExpectList)
  (define shared-frames (get-shared ess values))
  ;; rsegs : (NEListof (Rev2n+1-Listof RExpectList))
  (define rsegs (for/list ([es (in-list ess)]) (rsplit es values shared-frames)))
  (define final-seg (map car rsegs)) ;; (NEListof RExpectList), no common frames
  (define ctx-rsegs (transpose (map cdr rsegs))) ;; (Rev2n-Listof (NEListof RExpectList))
  (append (hd/sync-shared/final final-seg)
          (hd/sync-shared/ctx ctx-rsegs)))

;; hd/sync-shared/final : (NEListof RExpectList) -> ExpectList
;; PRE: ess has no shared frames, but may have shared terms.
(define (hd/sync-shared/final ess0)
  (define ess (remove-extensions ess0))
  (define shared-terms (get-shared ess expect->stxidx))
  (cond [(null? shared-terms) null]
        [else
         ;; split at the last shared term
         (define rsegs ;; (NEListof (3-Listof RExpectList))
           (for/list ([es (in-list ess)])
             (rsplit es expect->stxidx (list (last shared-terms)))))
         ;; only care about the got segment and pre, not post
         (define last-term-ess ;; (NEListof RExpectList)
           (map cadr rsegs))
         (define pre-term-ess ;; (NEListof RExpectList)
           (map caddr rsegs))
         ;; last is most specific
         (append
          (list (expect:disj (remove-duplicates (reverse (map last last-term-ess)))
                             (last shared-terms)))
          (if (ormap pair? pre-term-ess) '(...) '()))]))

;; hd/sync-shared/ctx : (Rev2n-Listof (NEListof RExpectList)) -> ExpectList
;; In [gotN preN ... got1 pre1] order, where 1 is root-most, N is leaf-most.
;; We want leaf-most-first, so just process naturally.
(define (hd/sync-shared/ctx rsegs)
  (let loop ([rsegs rsegs])
    (cond [(null? rsegs) null]
          [(null? (cdr rsegs)) (error 'syntax-parse "INTERNAL ERROR: bad segments")]
          [else (append
                 ;; shared frame: possible for duplicate ctx frames, but unlikely
                 (let ([ess (car rsegs)]) (list (car (car ess))))
                 ;; inter frames:
                 (let ([ess (cadr rsegs)]) (if (ormap  pair? ess) '(...) '()))
                 ;; recur
                 (loop (cddr rsegs)))])))

;; transpose : (Listof (Listof X)) -> (Listof (Listof X))
(define (transpose xss)
  (cond [(ormap null? xss) null]
        [else (cons (map car xss) (transpose (map cdr xss)))]))

;; get-shared : (Listof (Listof X)) (X -> Y) -> (Listof Y)
;; Return a list of Ys s.t. occur in order in (map of) each xs in xss.
(define (get-shared xss get-y)
  (cond [(null? xss) null]
        [else
         (define yhs ;; (Listof (Hash Y => Nat))
           (for/list ([xs (in-list xss)])
             (for/hash ([x (in-list xs)] [i (in-naturals 1)])
               (values (get-y x) i))))
         (remove-duplicates
          (let loop ([xs (car xss)] [last (for/list ([xs (in-list xss)]) 0)])
            ;; last is list of indexes of last accepted y; only accept next if occurs
            ;; after last in every sequence (see Case 7 above)
            (cond [(null? xs) null]
                  [else
                   (define y (get-y (car xs)))
                   (define curr (for/list ([yh (in-list yhs)]) (hash-ref yh y -1)))
                   (cond [(andmap > curr last)
                          (cons y (loop (cdr xs) curr))]
                         [else (loop (cdr xs) last)])])))]))

;; rsplit : (Listof X) (X -> Y) (Listof Y) -> (Listof (Listof X))
;; Given [y1 ... yN], splits xs into [rest gotN preN ... got1 pre1].
;; Thus the result has 2N+1 elements. The sublists are in original order.
(define (rsplit xs get-y ys)
  (define (loop xs ys segsacc)
    (cond [(null? ys) (cons xs segsacc)]
          [else (pre-loop xs ys segsacc null)]))
  (define (pre-loop xs ys segsacc preacc)
    (cond [(and (pair? xs) (equal? (get-y (car xs)) (car ys)))
           (got-loop (cdr xs) ys segsacc preacc (list (car xs)))]
          [else
           (pre-loop (cdr xs) ys segsacc (cons (car xs) preacc))]))
  (define (got-loop xs ys segsacc preacc gotacc)
    (cond [(and (pair? xs) (equal? (get-y (car xs)) (car ys)))
           (got-loop (cdr xs) ys segsacc preacc (cons (car xs) gotacc))]
          [else
           (loop xs (cdr ys) (list* (reverse gotacc) (reverse preacc) segsacc))]))
  (loop xs ys null))

;; singleton? : list -> boolean
(define (singleton? x) (and (pair? x) (null? (cdr x))))

;; remove-extensions : (Listof (Listof X)) -> (Listof (Listof X))
;; Remove any element that is an extension of another.
(define (remove-extensions xss)
  (cond [(null? xss) null]
        [else
         (let loop ([xss xss])
           (cond [(singleton? xss) xss]
                 [(ormap null? xss) (list null)]
                 [else
                  (define groups (group-by car xss))
                  (append*
                   (for/list ([group (in-list groups)])
                     (define group* (loop (map cdr group)))
                     (map (lambda (x) (cons (caar group) x)) group*)))]))]))

;; all-equal? : (Listof Any) -> Boolean
(define (all-equal? xs) (for/and ([x (in-list xs)]) (equal? x (car xs))))


;; ============================================================
;; Reporting

;; report/expectstack : ExpectList Syntax Nat -> Report
(define (report/expectstack es stx index)
  (define frame-expect (and (pair? es) (car es)))
  (define context-frames (if (pair? es) (cdr es) null))
  (define context (append* (map context-prose-for-expect context-frames)))
  (cond [(not frame-expect)
         (report "bad syntax" context #f #f)]
        [else
         (define-values (x cx) (stx-list-drop/cx stx stx index))
         (define frame-stx (datum->syntax cx x cx))
         (define within-stx (if (syntax? x) #f cx))
         (cond [(and (match frame-expect [(expect:atom '() _) #t] [_ #f])
                     (stx-pair? frame-stx))
                (report "unexpected term" context (stx-car frame-stx) #f)]
               [(expect:disj? frame-expect)
                (report (prose-for-expects (expect:disj-expects frame-expect))
                        context frame-stx within-stx)]
               [else
                (report (prose-for-expects (list frame-expect))
                        context frame-stx within-stx)])]))

;; prose-for-expects : (listof Expect) -> string
(define (prose-for-expects expects)
  (define msgs (filter expect:message? expects))
  (define things (filter expect:thing? expects))
  (define literal (filter expect:literal? expects))
  (define atom/symbol
    (filter (lambda (e) (and (expect:atom? e) (symbol? (expect:atom-atom e)))) expects))
  (define atom/nonsym
    (filter (lambda (e) (and (expect:atom? e) (not (symbol? (expect:atom-atom e))))) expects))
  (define proper-pairs (filter expect:proper-pair? expects))
  (join-sep
   (append (map prose-for-expect (append msgs things))
           (prose-for-expects/literals literal "identifiers")
           (prose-for-expects/literals atom/symbol "literal symbols")
           (prose-for-expects/literals atom/nonsym "literals")
           (prose-for-expects/pairs proper-pairs))
   ";" "or"))

(define (prose-for-expects/literals expects whats)
  (cond [(null? expects) null]
        [(singleton? expects) (map prose-for-expect expects)]
        [else
         (define (prose e)
           (match e
             [(expect:atom (? symbol? atom) _)
              (format "`~s'" atom)]
             [(expect:atom atom _)
              (format "~s" atom)]
             [(expect:literal literal _)
              (format "`~s'" (syntax-e literal))]))
         (list (string-append "expected one of these " whats ": "
                              (join-sep (map prose expects) "," "or")))]))

(define (prose-for-expects/pairs expects)
  (if (pair? expects) (list (prose-for-proper-pair-expects expects)) null))

;; prose-for-expect : Expect -> string
(define (prose-for-expect e)
  (match e
    [(expect:thing _ description transparent? role _)
     (if role
         (format "expected ~a for ~a" description role)
         (format "expected ~a" description))]
    [(expect:atom (? symbol? atom) _)
     (format "expected the literal symbol `~s'" atom)]
    [(expect:atom atom _)
     (format "expected the literal ~s" atom)]
    [(expect:literal literal _)
     (format "expected the identifier `~s'" (syntax-e literal))]
    [(expect:message message _)
     message]
    [(expect:proper-pair '#f _)
     "expected more terms"]))

;; prose-for-proper-pair-expects : (listof expect:proper-pair) -> string
(define (prose-for-proper-pair-expects es)
  (define descs (remove-duplicates (map expect:proper-pair-first-desc es)))
  (cond [(for/or ([desc descs]) (equal? desc #f))
         ;; FIXME: better way to indicate unknown ???
         "expected more terms"]
        [else
         (format "expected more terms starting with ~a"
                 (join-sep (map prose-for-first-desc descs)
                           "," "or"))]))

;; prose-for-first-desc : FirstDesc -> string
(define (prose-for-first-desc desc)
  (match desc
    [(? string?) desc]
    [(list 'any) "any term"] ;; FIXME: maybe should cancel out other descs ???
    [(list 'literal id) (format "the identifier `~s'" id)]
    [(list 'datum (? symbol? s)) (format "the literal symbol `~s'" s)]
    [(list 'datum d) (format "the literal ~s" d)]))

;; context-prose-for-expect : (U '... expect:thing) -> (listof string)
(define (context-prose-for-expect e)
  (match e
    ['...
     (list "while parsing different things...")]
    [(expect:thing '#f description transparent? role stx+index)
     (let ([stx (stx+index->stx stx+index)])
       (cons (~a "while parsing " description
                 (if role (~a " for " role) ""))
             (if (error-print-source-location)
                 (list (~a " term: "
                           (~s (syntax->datum stx)
                               #:limit-marker "..."
                               #:max-width 50))
                       (~a " location: "
                           (or (source-location->string stx) "not available")))
                 null)))]))

(define (stx+index->stx stx+index)
  (let*-values ([(stx) (car stx+index)]
                [(index) (cdr stx+index)]
                [(x cx) (stx-list-drop/cx stx stx index)])
    (datum->syntax cx x cx)))


;; ============================================================
;; Raise exception

(define (error/report ctx report)
  (let* ([message (report-message report)]
         [context (report-context report)]
         [stx (cadr ctx)]
         [who (or (car ctx) (infer-who stx))]
         [sub-stx (report-stx report)]
         [within-stx (report-within-stx report)]
         [message
          (format "~a: ~a~a~a~a~a"
                  who message
                  (format-if "at" (stx-if-loc sub-stx))
                  (format-if "within" (stx-if-loc within-stx))
                  (format-if "in" (stx-if-loc stx))
                  (if (null? context)
                      ""
                      (apply string-append
                             "\n  parsing context: "
                             (for/list ([c (in-list context)])
                               (format "\n   ~a" c)))))]
         [message
          (if (error-print-source-location)
              (let ([source-stx (or stx sub-stx within-stx)])
                (string-append (source-location->prefix source-stx) message))
              message)])
    (raise
     (exn:fail:syntax message (current-continuation-marks)
                      (map syntax-taint
                           (cond [within-stx (list within-stx)]
                                 [sub-stx (list sub-stx)]
                                 [stx (list stx)]
                                 [else null]))))))

(define (format-if prefix val)
  (if val
      (format "\n  ~a: ~a" prefix val)
      ""))

(define (stx-if-loc stx)
  (and (syntax? stx)
       (error-print-source-location)
       (format "~.s" (syntax->datum stx))))

(define (infer-who stx)
  (let* ([maybe-id (if (stx-pair? stx) (stx-car stx) stx)])
    (if (identifier? maybe-id) (syntax-e maybe-id) '?)))

(define (comma-list items)
  (join-sep items "," "or"))

(define (improper-stx->list stx)
  (syntax-case stx ()
    [(a . b) (cons #'a (improper-stx->list #'b))]
    [() null]
    [rest (list #'rest)]))


;; ============================================================
;; Debugging

(provide failureset->sexpr
         failure->sexpr
         expectstack->sexpr
         expect->sexpr)

(define (failureset->sexpr fs)
  (let ([fs (flatten fs)])
    (case (length fs)
      ((1) (failure->sexpr (car fs)))
      (else `(union ,@(map failure->sexpr fs))))))

(define (failure->sexpr f)
  (match f
    [(failure progress expectstack)
     `(failure ,(progress->sexpr progress)
               #:expected ,(expectstack->sexpr expectstack))]))

(define (expectstack->sexpr es)
  (map expect->sexpr es))

(define (expect->sexpr e) e)

(define (progress->sexpr ps)
  (for/list ([pf (in-list ps)])
    (match pf
      [(? syntax? stx) 'stx]
      [_ pf])))
