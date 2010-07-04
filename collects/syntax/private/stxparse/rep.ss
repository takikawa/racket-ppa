#lang scheme/base
(require (for-template scheme/base)
         (for-template "runtime.ss")
         scheme/contract/base
         "minimatch.ss"
         scheme/dict
         syntax/id-table
         syntax/stx
         syntax/keyword
         unstable/syntax
         unstable/struct
         "rep-data.ss"
         "codegen-data.ss")

;; Error reporting
;; All entry points should have explicit, mandatory #:context arg
;; (mandatory from outside, at least)

(provide/contract
 [parse-rhs
  (-> syntax? boolean? boolean? #:context (or/c false/c syntax?)
      rhs?)]
 [parse-whole-pattern 
  (-> syntax? DeclEnv/c #:context (or/c false/c syntax?)
      pattern?)]
 [parse-pattern-directives
  (-> stx-list?
      #:allow-declare? boolean?
      #:decls (or/c false/c DeclEnv/c)
      #:context (or/c false/c syntax?)
      (values stx-list? DeclEnv/c (listof syntax?) (listof SideClause/c)))]
 [parse-directive-table any/c]
 [get-decls+defs
  (-> list? boolean? #:context (or/c false/c syntax?)
      (values DeclEnv/c (listof syntax?)))]
 #|
 [decls-create-defs
  (-> DeclEnv/c
      (values DeclEnv/c (listof syntax?)))]
 |#
 [create-aux-def
  (-> list? ;; DeclEntry
      (values identifier? identifier? (listof sattr?) (listof syntax?) boolean?))]
 [check-literals-list
  (-> syntax? syntax?
      (listof (list/c identifier? identifier?)))]
 #|
 [check-literal-sets-list
  (-> syntax? syntax?
      (listof (listof (list/c identifier? identifier?))))]
 |#
 [check-conventions-rules
  (-> syntax? syntax?
      (listof (list/c regexp? any/c)))])

(define (atomic-datum? stx)
  (let ([datum (syntax-e stx)])
    (or (null? datum)
        (boolean? datum)
        (string? datum)
        (number? datum)
        (keyword? datum))))

(define (id-predicate kw)
  (lambda (stx)
    (and (identifier? stx)
         (free-identifier=? stx kw)
         (begin (disappeared! stx) #t))))

(define wildcard? (id-predicate (quote-syntax _)))
(define epsilon?  (id-predicate (quote-syntax ||)))
(define dots?     (id-predicate (quote-syntax ...)))

(define keywords
  (list (quote-syntax _)
        (quote-syntax ||)
        (quote-syntax ...)
        (quote-syntax ~var)
        (quote-syntax ~datum)
        (quote-syntax ~literal)
        (quote-syntax ~and)
        (quote-syntax ~or)
        (quote-syntax ~not)
        (quote-syntax ~seq)
        (quote-syntax ~rep)
        (quote-syntax ~once)
        (quote-syntax ~optional)
        (quote-syntax ~bounds)
        (quote-syntax ~rest)
        (quote-syntax ~describe)
        (quote-syntax ~!)
        (quote-syntax ~bind)
        (quote-syntax ~fail)
        (quote-syntax ~parse)))

(define (reserved? stx)
  (and (identifier? stx)
       (for/or ([kw keywords])
         (free-identifier=? stx kw))))

(define (safe-name? stx)
  (and (identifier? stx)
       (not (regexp-match? #rx"^~" (symbol->string (syntax-e stx))))))

;; ---

(define (disappeared! x)
  (cond [(identifier? x)
         (record-disappeared-uses (list x))]
        [(and (stx-pair? x) (identifier? (stx-car x)))
         (record-disappeared-uses (list (stx-car x)))]
        [else
         (raise-type-error 'disappeared!
                           "identifier or syntax with leading identifier"
                           x)]))

;; ---

;; parse-rhs : stx boolean boolean stx -> RHS
;; If strict? is true, then referenced stxclasses must be defined and
;; literals must be bound. Set to #f for pass1 (attr collection);
;; parser requires stxclasses to be bound.
(define (parse-rhs stx strict? splicing? #:context ctx)
  (parameterize ((current-syntax-context ctx))
    (define-values (rest description transp? attributes auto-nested? decls defs)
      (parse-rhs/part1 stx strict?))
    (define patterns
      (parameterize ((stxclass-lookup-config
                      (cond [strict? 'yes]
                            [auto-nested? 'try]
                            [else 'no])))
        (parse-variants rest decls splicing?)))
    (when (null? patterns)
      (wrong-syntax #f "expected at least one variant"))
    (let ([sattrs
           (or attributes
               (intersect-sattrss (map variant-attrs patterns)))])
      (make rhs stx sattrs transp? description patterns defs))))

(define (parse-rhs/part1 stx strict?)
  (define-values (chunks rest)
    (parse-keyword-options stx rhs-directive-table
                           #:context (current-syntax-context)
                           #:incompatible '((#:attributes #:auto-nested-attributes))
                           #:no-duplicates? #t))
  (define description (options-select-value chunks '#:description #:default #f))
  (define opaque? (and (assq '#:opaque chunks) #t))
  (define transparent? (not opaque?))
  (define auto-nested? (and (assq '#:auto-nested-attributes chunks) #t))
  (define attributes (options-select-value chunks '#:attributes #:default #f))
  (define-values (decls defs) (get-decls+defs chunks strict?))
  (values rest description transparent? attributes auto-nested? decls defs))

(define (parse-variants rest decls splicing?)
  (define (gather-patterns stx)
    (syntax-case stx (pattern)
      [((pattern . _) . rest)
       (begin (disappeared! (stx-car stx))
              (cons (parse-variant (stx-car stx) splicing? decls)
                    (gather-patterns #'rest)))]
      [(bad-variant . rest)
       (wrong-syntax #'bad-variant "expected syntax-class variant")]
      [()
       null]))
  (gather-patterns rest))

;; get-decls+defs : chunks boolean -> (values DeclEnv (listof syntax))
(define (get-decls+defs chunks strict?
                        #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (decls-create-defs (get-decls chunks strict?))))

;; get-decls : chunks -> DeclEnv
(define (get-decls chunks strict?)
  (define lits (options-select-value chunks '#:literals #:default null))
  (define litsets (options-select-value chunks '#:literal-sets #:default null))
  (define convs (options-select-value chunks '#:conventions #:default null))
  (define literals
    (append-lits+litsets (check-literals-bound lits strict?)
                         litsets))
  (define convention-rules (apply append convs))
  (new-declenv literals #:conventions convention-rules))

(define (check-literals-bound lits strict?)
  (define phase (syntax-local-phase-level))
  (when strict?
    (for ([p lits])
      ;; FIXME: hack...
      (unless (or (identifier-binding (cadr p) phase)
                  (identifier-binding (cadr p) (add1 phase))
                  (identifier-binding (cadr p) (sub1 phase))
                  (identifier-binding (cadr p) #f))
        (wrong-syntax (cadr p) "unbound identifier not allowed as literal"))))
  lits)

;; decls-create-defs : DeclEnv -> (values DeclEnv (listof stx))
(define (decls-create-defs decls0)
  (for/fold ([decls decls0] [defs null])
      ([(k v) (in-dict (declenv-table decls0))]
       #:when (memq (car v) '(stxclass splicing-stxclass)))
    (let-values ([(parser description attrs new-defs splicing?) (create-aux-def v)])
      (values (declenv-put-parser decls k parser description attrs splicing?)
              (append new-defs defs)))))

;; create-aux-def : DeclEntry -> (values id id (listof SAttr) (listof stx) boolean)
(define (create-aux-def entry)
  (let ([sc-name (caddr entry)]
        [args (cadddr entry)])
    (let ([sc (get-stxclass/check-arg-count sc-name (length args))])
      (with-syntax ([sc-parser (stxclass-parser-name sc)]
                    [sc-description (stxclass-description sc)])
        (if (pair? args)
            (with-syntax ([x (generate-temporary 'x)]
                          [parser (generate-temporary sc-name)]
                          [description (generate-temporary sc-name)]
                          [(arg ...) args])
              (values #'parser #'description (stxclass-attrs sc)
                      (list #'(define (parser x) (sc-parser x arg ...))
                            #'(define (description) (description arg ...)))
                      (stxclass/h? sc)))
            (values #'sc-parser #'sc-description (stxclass-attrs sc)
                    null (stxclass/h? sc)))))))

(define (append-lits+litsets lits litsets)
  (define seen (make-bound-id-table lits))
  (for ([litset litsets])
    (for ([lit litset])
      (when (bound-id-table-ref seen (car lit) #f)
        (wrong-syntax (car lit) "duplicate literal declaration"))
      (bound-id-table-set! seen (car lit) #t)))
  (apply append lits litsets))

;; parse-variant : stx boolean DeclEnv -> RHS
(define (parse-variant stx splicing? decls0)
  (syntax-case stx (pattern)
    [(pattern p . rest)
     (let-values ([(rest decls defs clauses)
                   (parse-pattern-directives #'rest
                                             #:allow-declare? #t
                                             #:decls decls0)])
       (disappeared! stx)
       (unless (stx-null? rest)
         (wrong-syntax (if (pair? rest) (car rest) rest)
                       "unexpected terms after pattern directives"))
       (let* ([pattern
               (parse-whole-pattern #'p decls splicing?)]
              [attrs
               (append-iattrs
                (cons (pattern-attrs pattern)
                      (side-clauses-attrss clauses)))]
              [sattrs (iattrs->sattrs attrs)])
         (make variant stx sattrs pattern clauses defs)))]))

(define (side-clauses-attrss clauses)
  (for/list ([c clauses]
             #:when (or (clause:with? c) (clause:attr? c)))
    (if (clause:with? c)
        (pattern-attrs (clause:with-pattern c))
        (list (clause:attr-attr c)))))

;; parse-whole-pattern : stx DeclEnv boolean -> Pattern
(define (parse-whole-pattern stx decls [splicing? #f]
                             #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define pattern
      (if splicing?
          (parse-head-pattern stx decls)
          (parse-single-pattern stx decls)))
    (define pvars (map attr-name (pattern-attrs pattern)))
    (define excess-domain (declenv-domain-difference decls pvars))
    (when (pair? excess-domain)
      (wrong-syntax #f "declared pattern variables do not appear in pattern"
                    #:extra excess-domain))
    pattern))

;; ----

;; parse-single-pattern : stx DeclEnv -> SinglePattern
(define (parse-single-pattern stx decls)
  ;; FIXME: allow ghosts, convert to single-term pattern???
  (let ([p (parse-*-pattern stx decls #f #f)])
    p))

;; parse-head-pattern : stx DeclEnv -> HeadPattern
(define (parse-head-pattern stx decls)
  (parse-*-pattern stx decls #t #f))

;; parse-*-pattern : stx DeclEnv boolean boolean -> Pattern
(define (parse-*-pattern stx decls allow-head? allow-ghost?)
  (define (check-head! x)
    (unless allow-head?
      (wrong-syntax stx "head pattern not allowed here"))
    x)
  (define (check-ghost! x)
    ;; Coerce to S-pattern IF only S-patterns allowed
    (cond [allow-ghost? x]
          [(not allow-head?) (ghost-pattern->single-pattern x)]
          [else
           (wrong-syntax stx "action pattern not allowed here")]))
  (syntax-case stx (~var ~literal ~datum ~and ~or ~not ~rest ~describe
                    ~seq ~optional ~! ~bind ~fail ~parse)
    [wildcard
     (wildcard? #'wildcard)
     (begin (disappeared! stx)
            (create-pat:any))]
    [~!
     (disappeared! stx)
     (check-ghost!
      (create-ghost:cut))]
    [reserved
     (reserved? #'reserved)
     (wrong-syntax stx "pattern keyword not allowed here")]
    [id
     (identifier? #'id)
     (parse-pat:id stx decls allow-head?)]
    [datum
     (atomic-datum? #'datum)
     (create-pat:datum (syntax->datum #'datum))]
    [(~var . rest)
     (disappeared! stx)
     (parse-pat:var stx decls allow-head?)]
    [(~datum . rest)
     (disappeared! stx)
     (syntax-case stx (~datum)
       [(~datum d)
        (create-pat:datum (syntax->datum #'d))]
       [_ (wrong-syntax stx "bad ~~datum form")])]
    [(~literal . rest)
     (disappeared! stx)
     (parse-pat:literal stx decls)]
    [(~and . rest)
     (disappeared! stx)
     (parse-pat:and stx decls allow-head? allow-ghost?)]
    [(~or . rest)
     (disappeared! stx)
     (parse-pat:or stx decls allow-head?)]
    [(~not . rest)
     (disappeared! stx)
     (parse-pat:not stx decls)]
    [(~rest . rest)
     (disappeared! stx)
     (parse-pat:rest stx decls)]
    [(~describe . rest)
     (disappeared! stx)
     (parse-pat:describe stx decls allow-head?)]
    [(~seq . rest)
     (disappeared! stx)
     (check-head!
      (parse-hpat:seq stx #'rest decls))]
    [(~optional . rest)
     (disappeared! stx)
     (check-head!
      (parse-hpat:optional stx decls))]
    [(~bind . rest)
     (disappeared! stx)
     (check-ghost!
      (parse-pat:bind stx decls))]
    [(~fail . rest)
     (disappeared! stx)
     (check-ghost!
      (parse-pat:fail stx decls))]
    [(~parse . rest)
     (disappeared! stx)
     (check-ghost!
      (parse-pat:parse stx decls))]
    [(head dots . tail)
     (dots? #'dots)
     (begin (disappeared! #'dots)
            (parse-pat:dots stx #'head #'tail decls))]
    [(head . tail)
     (let ([headp (parse-*-pattern #'head decls #t #t)]
           [tailp (parse-single-pattern #'tail decls)])
       ;; Only make pat:head if head is complicated;
       ;; otherwise simple compound/pair
       ;; FIXME: Could also inline ~seq patterns from head...?
       (cond [(ghost-pattern? headp)
              (create-pat:ghost headp tailp)]
             [(head-pattern? headp)
              (create-pat:head headp tailp)]
             [else
              (create-pat:compound '#:pair (list headp tailp))]))]
    [#(a ...)
     (let ([lp (parse-single-pattern (syntax/loc stx (a ...)) decls)])
       (create-pat:compound '#:vector (list lp)))]
    [b
     (box? (syntax-e #'b))
     (let ([bp (parse-single-pattern (unbox (syntax-e #'b)) decls)])
       (create-pat:compound '#:box (list bp)))]
    [s
     (and (struct? (syntax-e #'s)) (prefab-struct-key (syntax-e #'s)))
     (let* ([s (syntax-e #'s)]
            [key (prefab-struct-key s)]
            [contents (struct->list s)])
       (let ([lp (parse-single-pattern (datum->syntax #f contents #'s) decls)])
         (create-pat:compound `(#:pstruct ,key) (list lp))))]))

;; parse-ellipsis-head-pattern : stx DeclEnv number -> EllipsisHeadPattern
(define (parse-ellipsis-head-pattern stx decls)
  (syntax-case stx (~bounds ~optional ~once)
    [(~optional . _)
     (disappeared! stx)
     (parse-ehpat/optional stx decls)]
    [(~once . _)
     (disappeared! stx)
     (parse-ehpat/once stx decls)]
    [(~bounds . _)
     (disappeared! stx)
     (parse-ehpat/bounds stx decls)]
    [_
     (let ([head (parse-head-pattern stx decls)])
       (make ehpat (map increase-depth (pattern-attrs head))
             head
             #f))]))

;; ----

(define (parse-pat:id id decls allow-head?)
  (define entry (declenv-lookup decls id))
  (match entry
    [(list 'literal internal-id literal-id)
     (create-pat:literal literal-id)]
    [(list 'stxclass _ _ _)
     (error 'parse-pat:id
            "(internal error) decls had leftover 'stxclass entry: ~s"
            entry)]
    [(list 'splicing-stxclass _ _ _)
     (error 'parse-pat:id
            "(internal error) decls had leftover 'splicing-stxclass entry: ~s"
            entry)]
    [(list 'parser parser description attrs)
     (parse-pat:id/s id parser null attrs)]
    [(list 'splicing-parser parser description attrs)
     (parse-pat:id/h id parser null attrs)]
    ['#f
     (when #f ;; FIXME: enable?
       (unless (safe-name? id)
         (wrong-syntax id "expected identifier not starting with ~ character")))
     (let-values ([(name sc) (split-id/get-stxclass id decls)])
       (if sc
           (parse-pat:var* id allow-head? name sc null)
           (create-pat:var name #f null null)))]))

(define (parse-pat:var stx decls allow-head?)
  (define name0
    (syntax-case stx (~var)
      [(~var name . _)
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected identifier"))
       #'name]
      [_
       (wrong-syntax stx "bad ~~var form")]))
  (define-values (scname args)
    (syntax-case stx (~var)
      [(~var _name)
       (values #f null)]
      [(~var _name sc)
       (identifier? #'sc)
       (values #'sc null)]
      [(~var _name (sc arg ...))
       (identifier? #'sc)
       (values #'sc (syntax->list #'(arg ...)))]
      [_
       (wrong-syntax stx "bad ~~var form")]))
  (cond [(and (epsilon? name0) (not scname))
         (wrong-syntax name0 "illegal pattern variable name")]
        [(and (wildcard? name0) (not scname))
         (create-pat:any)]
        [scname
         (let ([sc (get-stxclass/check-arg-count scname (length args))])
           (parse-pat:var* stx allow-head? name0 sc args))]
        [else ;; Just proper name
         (create-pat:var name0 #f null null)]))

(define (parse-pat:var* stx allow-head? name sc args)
  (cond [(stxclass/s? sc)
         (parse-pat:id/s name
                         (stxclass-parser-name sc)
                         args
                         (stxclass-attrs sc))]
        [(stxclass/h? sc)
         (unless allow-head?
           (wrong-syntax stx "splicing syntax class not allowed here"))
         (parse-pat:id/h name
                         (stxclass-parser-name sc)
                         args
                         (stxclass-attrs sc))]))

(define (parse-pat:id/s name parser args attrs)
  (define prefix (name->prefix name))
  (define bind (name->bind name))
  (create-pat:var bind parser args (id-pattern-attrs attrs prefix)))

(define (parse-pat:id/h name parser args attrs)
  (define prefix (name->prefix name))
  (define bind (name->bind name))
  (create-hpat:var bind parser args (id-pattern-attrs attrs prefix)))

(define (name->prefix id)
  (cond [(wildcard? id) #f]
        [(epsilon? id) id]
        [else (format-id id "~a." (syntax-e id))]))

(define (name->bind id)
  (cond [(wildcard? id) #f]
        [(epsilon? id) #f]
        [else id]))

;; id-pattern-attrs : (listof SAttr)IdPrefix -> (listof IAttr)
(define (id-pattern-attrs sattrs prefix)
  (if prefix
      (for/list ([a sattrs])
        (prefix-attr a prefix))
      null))

;; prefix-attr : SAttr identifier -> IAttr
(define (prefix-attr a prefix)
  (make attr (prefix-attr-name prefix (attr-name a))
        (attr-depth a)
        (attr-syntax? a)))

;; prefix-attr-name : id symbol -> id
(define (prefix-attr-name prefix name)
  (format-id prefix "~a~a" (syntax-e prefix) name))

;; ----

(define (parse-pat:literal stx decls)
  (syntax-case stx (~literal)
    [(~literal lit)
     (unless (identifier? #'lit)
       (wrong-syntax #'lit "expected identifier"))
     (create-pat:literal #'lit)]
    [_
     (wrong-syntax stx "bad ~~literal pattern")]))

(define (parse-pat:describe stx decls allow-head?)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options #'rest describe-option-table
                                          #:no-duplicates? #t
                                          #:context stx)])
       (define transparent? (and (assq '#:transparent chunks) #t))
       (syntax-case rest ()
         [(description pattern)
          (let ([p (parse-*-pattern #'pattern decls allow-head? #f)])
            (if (head-pattern? p)
                (create-hpat:describe #'description transparent? p)
                (create-pat:describe #'description transparent? p)))]))]))

(define (split-prefix xs pred)
  (let loop ([xs xs] [rprefix null])
    (cond [(and (pair? xs) (pred (car xs)))
           (loop (cdr xs) (cons (car xs) rprefix))]
          [else
           (values (reverse rprefix) xs)])))

(define (parse-pat:and stx decls allow-head? allow-ghost?)
  ;; allow-ghost? = allowed to *return* pure ghost pattern;
  ;; all ~and patterns are allowed to *contain* ghost patterns
  (define patterns0 (parse-cdr-patterns stx decls allow-head? #t))
  (define-values (ghosts patterns) (split-prefix patterns0 ghost-pattern?))
  (cond [(null? patterns)
         (cond [allow-ghost?
                (create-ghost:and ghosts)]
               [allow-head?
                (wrong-syntax stx "expected at least one head pattern")]
               [else
                (wrong-syntax stx "expected at least one single-term pattern")])]
        [else
         (let ([p (parse-pat:and* stx patterns)])
           (if (head-pattern? p)
               (for/fold ([p p]) ([ghost (reverse ghosts)])
                 (create-hpat:ghost ghost p))
               (for/fold ([p p]) ([ghost (reverse ghosts)])
                 (create-pat:ghost ghost p))))]))

(define (parse-pat:and* stx patterns)
  ;; patterns is non-empty (empty case handled above)
  (cond [(null? (cdr patterns))
         (car patterns)]
        [(ormap head-pattern? patterns)
         ;; Check to make sure *all* are head patterns
         (for ([pattern patterns]
               [pattern-stx (stx->list (stx-cdr stx))])
           (unless (or (ghost-pattern? pattern) (head-pattern? pattern))
             (wrong-syntax
              pattern-stx
              "single-term pattern not allowed after head pattern")))
         (let ([p0 (car patterns)]
               [lps (map ghost/head-pattern->list-pattern (cdr patterns))])
           (create-hpat:and p0 (create-pat:and lps)))]
        [else
         (create-pat:and
          (for/list ([p patterns])
            (if (ghost-pattern? p)
                (ghost-pattern->single-pattern p)
                p)))]))

(define (parse-pat:or stx decls allow-head?)
  (define patterns (parse-cdr-patterns stx decls allow-head? #f))
  (cond [(null? (cdr patterns))
         (car patterns)]
        [else
         (cond [(ormap head-pattern? patterns)
                (create-hpat:or patterns)]
               [else
                (create-pat:or patterns)])]))

(define (parse-pat:not stx decls)
  (syntax-case stx (~not)
    [(~not pattern)
     (let ([p (parse-single-pattern #'pattern decls)])
       (create-pat:not p))]
    [_
     (wrong-syntax stx "expected a single subpattern")]))

(define (parse-hpat:seq stx list-stx decls)
  (define pattern (parse-single-pattern list-stx decls))
  (check-list-pattern pattern stx)
  (create-hpat:seq pattern))

(define (parse-cdr-patterns stx decls allow-head? allow-ghost?)
  (unless (stx-list? stx)
    (wrong-syntax stx "expected sequence of patterns"))
  (let ([result
         (for/list ([sub (cdr (stx->list stx))])
           (parse-*-pattern sub decls allow-head? allow-ghost?))])
    (when (null? result)
      (wrong-syntax stx "expected at least one pattern"))
    result))

(define (parse-pat:dots stx head tail decls)
  (define headps
    (syntax-case head (~or)
      [(~or . _)
       (begin
         (unless (stx-list? head)
           (wrong-syntax head "expected sequence of patterns"))
         (unless (stx-pair? (stx-cdr head))
           (wrong-syntax head "expected at least one pattern"))
         (for/list ([sub (cdr (stx->list head))])
           (parse-ellipsis-head-pattern sub decls)))]
      [_
       (list (parse-ellipsis-head-pattern head decls))]))
  (define tailp (parse-single-pattern tail decls))
  (create-pat:dots headps tailp))

(define (parse-pat:bind stx decls)
  (syntax-case stx ()
    [(_ clause ...)
     (let ([clauses (check-bind-clause-list #'(clause ...) stx)])
       (make ghost:bind
         (append-iattrs (side-clauses-attrss clauses))
         clauses))]))

(define (parse-pat:fail stx decls)
  (syntax-case stx ()
    [(_ . rest)
     (let-values ([(chunks rest)
                   (parse-keyword-options #'rest fail-directive-table
                                          #:context stx
                                          #:incompatible '((#:when #:unless))
                                          #:no-duplicates? #t)])
       (let ([condition
              (if (null? chunks)
                  #'#t
                  (let ([chunk (car chunks)])
                    (if (eq? (car chunk) '#:when)
                        (caddr chunk)
                        #`(not #,(caddr chunk)))))])
         (syntax-case rest ()
           [(message)
            (create-ghost:fail condition #'message)]
           [()
            (wrong-syntax stx "missing message expression")]
           [_
            (wrong-syntax stx "bad ~~fail pattern")])))]))

(define (parse-pat:parse stx decls)
  (syntax-case stx (~parse)
    [(~parse pattern expr)
     (let ([p (parse-single-pattern #'pattern decls)])
       (create-ghost:parse p #'expr))]
    [_
     (wrong-syntax stx "bad ~~parse pattern")]))


(define (parse-pat:rest stx decls)
  (syntax-case stx ()
    [(_ pattern)
     (parse-single-pattern #'pattern decls)]))

(define (check-list-pattern pattern stx)
  (match pattern
    [(make pat:datum _base '())
     #t]
    [(make pat:head _base _head tail)
     (check-list-pattern tail stx)]
    [(make pat:dots _base _head tail)
     (check-list-pattern tail stx)]
    [(make pat:compound _base '#:pair (list _head tail))
     (check-list-pattern tail stx)]
    [_
     (wrong-syntax stx "expected proper list pattern")]))

(define (parse-hpat:optional stx decls)
  (define-values (head all-iattrs _name _tmm defaults)
    (parse-optional-pattern stx decls h-optional-directive-table))
  (make hpat:optional all-iattrs head defaults))

(define (parse-ehpat/optional stx decls)
  (define-values (head all-iattrs name too-many-msg defaults)
    (parse-optional-pattern stx decls eh-optional-directive-table))
  (make ehpat all-iattrs head
        (make rep:optional name too-many-msg defaults)))

(define (parse-optional-pattern stx decls optional-directive-table)
  (syntax-case stx (~optional)
    [(~optional p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define chunks
         (parse-keyword-options/eol #'options optional-directive-table
                                    #:no-duplicates? #t
                                    #:context stx))
       (let ([too-many-msg
              (options-select-value chunks '#:too-many #:default #'#f)]
             [name
              (options-select-value chunks '#:name #:default #'#f)]
             [defaults
              (options-select-value chunks '#:defaults #:default '())])
         (define pattern-iattrs (pattern-attrs head))
         (define defaults-iattrs
           (append-iattrs (side-clauses-attrss defaults)))
         (define all-iattrs
           (union-iattrs (list pattern-iattrs defaults-iattrs)))
         (check-iattrs-subset defaults-iattrs pattern-iattrs stx)
         (values head all-iattrs name too-many-msg defaults)))]))

(define (parse-ehpat/once stx decls)
  (syntax-case stx (~once)
    [(~once p . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define chunks
         (parse-keyword-options/eol #'options
                                    (list (list '#:too-few check-expression)
                                          (list '#:too-many check-expression)
                                          (list '#:name check-expression))
                                    #:context stx))
       (let ([too-few-msg
              (options-select-value chunks '#:too-few #:default #'#f)]
             [too-many-msg
              (options-select-value chunks '#:too-many #:default #'#f)]
             [name
              (options-select-value chunks '#:name #:default #'#f)])
         (make ehpat (pattern-attrs head)
               head
               (make rep:once name too-few-msg too-many-msg))))]))

(define (parse-ehpat/bounds stx decls)
  (syntax-case stx (~bounds)
    [(~bounds p min max . options)
     (let ([head (parse-head-pattern #'p decls)])
       (define minN (syntax-e #'min))
       (define maxN (syntax-e #'max))
       (unless (exact-nonnegative-integer? minN)
         (wrong-syntax #'min
                       "expected exact nonnegative integer"))
       (unless (or (exact-nonnegative-integer? maxN) (equal? maxN +inf.0))
         (wrong-syntax #'max
                       "expected exact nonnegative integer or +inf.0"))
       (when (> minN maxN)
         (wrong-syntax stx "minimum larger than maximum repetition constraint"))
       (let ([chunks (parse-keyword-options #'options
                                            (list (list '#:too-few check-expression)
                                                  (list '#:too-many check-expression)
                                                  (list '#:name check-expression))
                                            #:context stx)])
         (let ([too-few-msg
                (options-select-value chunks '#:too-few #:default #'#f)]
               [too-many-msg
                (options-select-value chunks '#:too-many #:default #'#f)]
               [name
                (options-select-value chunks '#:name #:default #'#f)])
           (make ehpat (map increase-depth (pattern-attrs head))
                 head
                 (make rep:bounds #'min #'max
                       name too-few-msg too-many-msg)))))]))

;; -----

;; parse-pattern-directives : stxs(PatternDirective) <kw-args>
;;                         -> stx DeclEnv (listof stx) (listof SideClause)
(define (parse-pattern-directives stx
                                  #:allow-declare? allow-declare?
                                  #:decls decls
                                  #:context [ctx (current-syntax-context)])
  (parameterize ((current-syntax-context ctx))
    (define-values (chunks rest)
      (parse-keyword-options stx pattern-directive-table #:context ctx))
    (define-values (decls2 chunks2)
      (if allow-declare?
          (grab-decls chunks decls)
          (values decls chunks)))
    (define sides
      ;; NOTE: use *original* decls
      ;; because decls2 has #:declares for *above* pattern
      (parse-pattern-sides chunks2 decls))
    (define-values (decls3 defs)
      (decls-create-defs decls2))
    (values rest decls3 defs (parse-pattern-sides chunks2 decls))))

;; parse-pattern-sides : (listof chunk) DeclEnv
;;                    -> (listof SideClause/c)
;; Invariant: decls contains only literals bindings
(define (parse-pattern-sides chunks decls)
  (match chunks
    [(cons (list '#:declare declare-stx _ _) rest)
     (wrong-syntax declare-stx
                   "#:declare can only follow pattern or #:with clause")]
    [(cons (list '#:fail-when fw-stx when-condition expr) rest)
     (cons (make clause:fail when-condition expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:fail-unless fu-stx unless-condition expr) rest)
     (cons (make clause:fail #`(not #,unless-condition) expr)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:when w-stx unless-condition) rest)
     ;; Bleh: when is basically fail-unless without the msg argument
     (cons (make clause:fail #`(not #,unless-condition) #'#f)
           (parse-pattern-sides rest decls))]
    [(cons (list '#:with with-stx pattern expr) rest)
     (let-values ([(decls2 rest) (grab-decls rest decls)])
       (let-values ([(decls2a defs) (decls-create-defs decls2)])
         (cons (make clause:with (parse-whole-pattern pattern decls2a) expr defs)
               (parse-pattern-sides rest decls))))]
    [(cons (list '#:attr attr-stx a expr) rest)
     (cons (make clause:attr a expr)
           (parse-pattern-sides rest decls))]
    ['()
     '()]))

;; grab-decls : (listof chunk) DeclEnv
;;           -> (values DeclEnv (listof chunk))
(define (grab-decls chunks decls)
  (define (add-decl stx decls)
    (syntax-case stx ()
      [(#:declare name sc)
       (identifier? #'sc)
       (add-decl* #'name #'sc null)]
      [(#:declare name (sc expr ...))
       (identifier? #'sc)
       (add-decl* #'name #'sc (syntax->list #'(expr ...)))]
      [(#:declare name bad-sc)
       (wrong-syntax #'bad-sc
                     "expected syntax class name (possibly with parameters)")]))
  (define (add-decl* id sc-name args)
    (declenv-put-stxclass decls id sc-name args))
  (define (loop chunks decls)
    (match chunks
      [(cons (cons '#:declare decl-stx) rest)
       (loop rest (add-decl decl-stx decls))]
      [_ (values decls chunks)]))
  (loop chunks decls))


;; ----

;; Keyword Options & Checkers

;; check-attr-arity-list : stx stx -> (listof SAttr)
(define (check-attr-arity-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected list of attribute declarations" ctx stx))
  (let ([iattrs (for/list ([x (stx->list stx)]) (check-attr-arity x ctx))])
    (iattrs->sattrs (append-iattrs (map list iattrs)))))

;; check-attr-arity : stx stx -> IAttr
(define (check-attr-arity stx ctx)
  (syntax-case stx ()
    [attr
     (identifier? #'attr)
     (make-attr #'attr 0 #f)]
    [(attr depth)
     (begin (unless (identifier? #'attr)
              (raise-syntax-error #f "expected attribute name" ctx #'attr))
            (unless (exact-nonnegative-integer? (syntax-e #'depth))
              (raise-syntax-error #f "expected depth (nonnegative integer)" ctx #'depth))
            (make-attr #'attr (syntax-e #'depth) #f))]
    [_
     (raise-syntax-error #f "expected attribute name with optional depth declaration" ctx stx)]))

;; check-literals-list : stx stx -> (listof (list id id))
(define (check-literals-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literals list" ctx stx))
  (let ([lits (for/list ([x (stx->list stx)]) (check-literal-entry x ctx))])
    (let ([dup (check-duplicate-identifier (map car lits))])
      (when dup (raise-syntax-error #f "duplicate literal identifier" ctx dup)))
    lits))

;; check-literal-entry : stx stx -> (list id id)
(define (check-literal-entry stx ctx)
  (syntax-case stx ()
    [(internal external)
     (and (identifier? #'internal) (identifier? #'external))
     (list #'internal #'external)]
    [id
     (identifier? #'id)
     (list #'id #'id)]
    [_
     (raise-syntax-error #f "expected literal (identifier or pair of identifiers)"
                         ctx stx)]))

(define (check-literal-sets-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected literal-set list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-literal-set-entry x ctx)))

(define (check-literal-set-entry stx ctx)
  (define (elaborate litset-id lctx)
    (let ([litset (syntax-local-value/catch litset-id literalset?)])
      (unless litset
        (raise-syntax-error #f "expected identifier defined as a literal-set"
                            ctx litset-id))
      (elaborate-litset litset lctx stx)))
  (syntax-case stx ()
    [(litset #:at lctx)
     (and (identifier? #'litset) (identifier? #'lctx))
     (elaborate #'litset #'lctx)]
    [litset
     (identifier? #'litset)
     (elaborate #'litset #'litset)]
    [_
     (raise-syntax-error #f "expected literal-set entry" ctx stx)]))

(define (elaborate-litset litset lctx srcctx)
  (for/list ([entry (literalset-literals litset)])
    (list (datum->syntax lctx (car entry) srcctx)
          (cadr entry))))

(define (check-conventions-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected conventions list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-conventions x ctx)))

(define (check-conventions stx ctx)
  (define (elaborate conventions-id)
    (let ([cs (syntax-local-value/catch conventions-id conventions?)])
      (unless cs
        (raise-syntax-error #f "expected identifier defined as a conventions"
                            ctx conventions-id))
      (conventions-rules cs)))
  (syntax-case stx ()
    [conventions
     (identifier? #'conventions)
     (elaborate #'conventions)]
    [_
     (raise-syntax-error "expected conventions entry" ctx stx)]))

(define (check-conventions-rules stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected convention rule list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check-conventions-rule x ctx)))

(define (check-conventions-rule stx ctx)
  (define (check-conventions-pattern x blame)
    (cond [(symbol? x) (regexp (string-append "^" (regexp-quote (symbol->string x)) "$"))]
          [(regexp? x) x]
          [else (raise-syntax-error #f  "expected identifier convention pattern" ctx blame)]))
  (define (check-sc-expr x)
    (syntax-case x ()
      [sc (identifier? #'sc) (list #'sc null)]
      [(sc arg ...) (identifier? #'sc) (list #'sc (syntax->list #'(arg ...)))]
      [_ (raise-syntax-error #f "expected syntax class use" ctx x)]))
  (syntax-case stx ()
    [(rx sc)
     (list (check-conventions-pattern (syntax-e #'rx) #'rx)
           (check-sc-expr #'sc))]))

;; bind clauses
(define (check-bind-clause-list stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected sequence of bind clauses" ctx stx))
  (for/list ([clause (stx->list stx)])
    (check-bind-clause clause ctx)))

(define (check-bind-clause clause ctx)
  (syntax-case clause ()
    [(attr-decl expr)
     (make clause:attr (check-attr-arity #'attr-decl ctx) #'expr)]
    [_ (raise-syntax-error #f "expected bind clause" ctx clause)]))

;; common-parse-directive-table
(define common-parse-directive-table
  (list (list '#:literals check-literals-list)
        (list '#:literal-sets check-literal-sets-list)
        (list '#:conventions check-conventions-list)))

;; parse-directive-table
(define parse-directive-table
  (list* (list '#:context check-expression)
         common-parse-directive-table))

;; rhs-directive-table
(define rhs-directive-table
  (list* (list '#:description check-expression)
         (list '#:transparent)
         (list '#:opaque)
         (list '#:attributes check-attr-arity-list)
         (list '#:auto-nested-attributes)
         common-parse-directive-table))

;; pattern-directive-table
(define pattern-directive-table
  (list (list '#:declare check-identifier check-expression)
        (list '#:fail-when check-expression check-expression)
        (list '#:fail-unless check-expression check-expression)
        (list '#:when check-expression)
        (list '#:with check-expression check-expression)
        (list '#:attr check-attr-arity check-expression)))

;; fail-directive-table
(define fail-directive-table
  (list (list '#:when check-expression)
        (list '#:unless check-expression)))

;; describe-option-table
(define describe-option-table
  (list (list '#:transparent)))

;; eh-optional-directive-table
(define eh-optional-directive-table
  (list (list '#:too-many check-expression)
        (list '#:name check-expression)
        (list '#:defaults check-bind-clause-list)))

;; h-optional-directive-table
(define h-optional-directive-table
  (list (list '#:defaults check-bind-clause-list)))
