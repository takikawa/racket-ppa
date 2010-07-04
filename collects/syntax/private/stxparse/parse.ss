#lang scheme/base
(require (for-syntax scheme/base
                     scheme/match
                     scheme/private/sc
                     syntax/stx
                     syntax/id-table
                     "rep-data.ss"
                     "rep.ss"
                     "codegen-data.ss"
                     "../util.ss")
         scheme/stxparam
         scheme/list
         scheme/match
         syntax/stx
         "runtime.ss"
         "runtime-prose.ss")
(provide (all-defined-out))

(define-for-syntax (wash stx)
  (syntax-e stx))
(define-for-syntax (wash-list washer stx)
  (let ([l (stx->list stx)])
    (unless l (raise-type-error 'wash-list "stx-list" stx))
    (map washer l)))
(define-for-syntax (wash-iattr stx)
  (with-syntax ([#s(attr name depth syntax?) stx])
    (make-attr #'name (wash #'depth) (wash #'syntax?))))
(define-for-syntax (wash-sattr stx)
  (with-syntax ([#s(attr name depth syntax?) stx])
    (make-attr (wash #'name) (wash #'depth) (wash #'syntax?))))

(define-for-syntax (wash-iattrs stx)
  (wash-list wash-iattr stx))
(define-for-syntax (wash-sattrs stx)
  (wash-list wash-sattr stx))

;; ----

;; (fail expr #:expect expr #:fce FCE) : expr
(define-syntax (fail stx)
  (syntax-case stx ()
    [(fail x #:expect p #:fce fce)
     (let ([fc-expr (frontier->dfc-expr (wash #'fce))]
           [fstx-expr (frontier->fstx-expr (wash #'fce))])
       #`(enclosing-fail
          (make-failure x #,fc-expr #,fstx-expr p)))]))

;; (parse:rhs RHS (SAttr ...) (id ...) id boolean)
;;   : expr[(values ParseFunction DescriptionFunction)]
;; Takes a list of the relevant attrs; order is significant!
;; Returns either fail or a list having length same as 'relsattrs'
(define-syntax (parse:rhs stx)
  (syntax-case stx ()
    [(parse:rhs #s(rhs _ _ transparent? _ variants (def ...))
                relsattrs (arg ...) get-description splicing?)
     #`(lambda (x arg ...)
         (define (fail-rhs failure)
           (expectation-of-thing (get-description arg ...)
                                 transparent?
                                 (if transparent? failure #f)))
         def ...
         (syntax-parameterize ((this-syntax (make-rename-transformer #'x)))
           (with-enclosing-fail* fail-rhs
             (parse:variants x relsattrs variants splicing?))))]))

;; (parse:variants id (SAttr ...) (Variant ...) boolean)
;;   : expr[SyntaxClassResult]
(define-syntax (parse:variants stx)
  (syntax-case stx ()
    [(parse:variants x relsattrs (variant ...) splicing?)
     #'(try (parse:variant x relsattrs variant splicing?) ...)]))

(define-syntax (parse:variant stx)
  (syntax-case stx ()
    [(parse:variant x relsattrs variant #f)
     (with-syntax ([#s(variant _ _ pattern _ (def ...)) #'variant]
                   [fc (empty-frontier #'x)])
       #`(let ()
           def ...
           (parse:S x fc pattern (variant-success x relsattrs variant ()))))]
    [(parse:variant x relsattrs variant #t)
     (with-syntax ([#s(variant _ _ pattern _ (def ...)) #'variant]
                   [fc (empty-frontier #'x)])
       #`(let ()
           def ...
           (parse:H x fc pattern rest index
                    (variant-success x relsattrs variant (rest index)))))]))

;; (variant-success id (SAttr ...) Variant (expr ...)) : expr[SyntaxClassResult]
(define-syntax (variant-success stx)
  (syntax-case stx ()
    [(variant-success x relsattrs #s(variant _ _ pattern sides _) (also ...))
     #`(convert-sides x sides
                      (base-success-expr #,(pattern-attrs (wash #'pattern))
                                         relsattrs
                                         (also ...)))]))

;; (convert-sides id (Side ...) (m (IAttr ...) . MArgs)) : expr[X]
;;   where (m (IAttr ...) MArgs) : expr[X]
(define-syntax (convert-sides stx)
  (syntax-case stx ()
    [(convert-sides x () kexpr)
     #'kexpr]
    [(convert-sides x (side0 . sides) (k iattrs . kargs))
     (syntax-case #'side0 ()
       [#s(clause:fail condition message)
        #`(if (without-fails condition)
              (fail x
                    #:expect (expectation-of-message message)
                    #:fce #,(done-frontier #'x))
              (convert-sides x sides (k iattrs . kargs)))]
       [#s(clause:with pattern expr (def ...))
        (with-syntax ([(p-iattr ...) (pattern-attrs (wash #'pattern))])
          #`(let ([y (without-fails expr)])
              def ...
              (parse:S y #,(done-frontier #'x) pattern
                       (convert-sides x sides
                                      (k (p-iattr ... . iattrs) . kargs)))))]
       [#s(clause:attr a expr)
        #`(let-attributes ([a (without-fails (check-list^depth a expr))])
            (convert-sides x sides (k (a . iattrs) . kargs)))])]))

;; (base-success-expr (IAttr ...) (SAttr ...) (expr ...) : expr[SCResult]
(define-syntax (base-success-expr stx)
  (syntax-case stx ()
    [(base-success-expr iattrs relsattrs (also ...))
     (let ([reliattrs
            (reorder-iattrs (wash-sattrs #'relsattrs)
                            (wash-iattrs #'iattrs))])
       (with-syntax ([(#s(attr name _ _) ...) reliattrs])
         #'(list also ... (attribute name) ...)))]))

;; ----

;; (parse:clauses id (Clause ...))
(define-syntax (parse:clauses stx)
  (syntax-case stx ()
    [(parse:clauses x clauses)
     (let ()
       (define-values (chunks clauses-stx)
         (chunk-kw-seq/no-dups #'clauses parse-directive-table))
       (define-values (decls0 defs) (get-decls+defs chunks))
       (define (for-clause clause)
         (syntax-case clause ()
           [[p . rest]
            (let-values ([(rest decls sides)
                          (parse-pattern-directives #'rest #:decls decls0)])
              (define-values (decls2 defs2) (decls-create-defs decls))
              (with-syntax ([rest rest]
                            [fc (empty-frontier #'x)]
                            [pattern (parse-whole-pattern #'p decls2)]
                            [(local-def ...) defs2])
                #`(let ()
                    local-def ...
                    (parse:S x fc pattern
                             (convert-sides x #,sides
                                            (clause-success () (let () . rest)))))))]))
       (unless (and (stx-list? clauses-stx) (stx-pair? clauses-stx))
         (wrong-syntax clauses-stx "expected non-empty sequence of clauses"))
       (with-syntax ([(def ...) defs]
                     [(alternative ...)
                      (map for-clause (stx->list clauses-stx))])
         #`(let ()
             def ...
             (try alternative ...))))]))

(define-for-syntax (wash-literal stx)
  (syntax-case stx ()
    [(a b) (list #'a #'b)]))
(define-for-syntax (wash-literals stx)
  (wash-list wash-literal stx))

#|
;; (parse:clause id ([id id] ...) Clause) : expr
(define-syntax (parse:clause stx)
  (syntax-case stx ()
    [(parse:clause x literals [p . rest])
     (let-values ([(rest decls sides)
                   (parse-pattern-directives
                    #'rest #:decls (new-declenv (wash-literals #'literals)))])
       (with-syntax ([rest rest]
                     [fc (empty-frontier #'x)]
                     [pattern (parse-whole-pattern #'p decls)])
         #`(parse:S x fc pattern 
                    (convert-sides x #,sides
                                   (clause-success () (let () . rest))))))]))
|#

;; (clause-success (IAttr ...) expr) : expr
(define-syntax (clause-success stx)
  (syntax-case stx ()
    [(clause-success _ expr)
     #'expr]))

;; ----

;; (parse:S id FCE SinglePattern expr) : expr
(define-syntax (parse:S stx)
  (syntax-case stx ()
    [(parse:S x fc pattern0 k)
     (syntax-case #'pattern0 ()
       [#s(internal-rest-pattern rest index index0)
        #`(let ([rest x]
                [index (- #,(frontier->index-expr (wash #'fc)) index0)])
            k)]
       [#s(pat:name attrs pattern (name ...))
        #`(let-attributes ([#s(attr name 0 #t) x] ...)
            (parse:S x fc pattern k))]
       [#s(pat:any attrs)
        #'k]
       [#s(pat:sc (a ...) parser description bind-term? bind-attrs?)
        #`(let ([result (parser x)])
            (if (ok? result)
                (let/unpack ((a ...)
                             #,(let ([bind-term? (syntax-e #'bind-term?)]
                                     [bind-attrs? (syntax-e #'bind-attrs?)])
                                 (cond [(and bind-term? bind-attrs?)
                                        #'(cons x result)]
                                       [bind-term? ;; not possible, I think
                                        #'(list x)]
                                       [bind-attrs?
                                        #'result]
                                       [else #'null])))
                  k)
                (fail x #:expect result #:fce fc)))]
       [#s(pat:datum attrs datum)
        #`(let ([d (syntax-e x)])
            (if (equal? d (quote datum))
                k
                (fail x
                      #:expect (expectation-of-constant datum)
                      #:fce fc)))]
       [#s(pat:literal attrs literal)
        #`(if (and (identifier? x) (free-identifier=? x (quote-syntax literal)))
              k
              (fail x
                    #:expect (expectation-of-literal literal)
                    #:fce fc))]
       [#s(pat:head attrs head tail)
        #`(parse:H x fc head rest index
                   (parse:S rest #,(frontier:add-index (wash #'fc) #'index) tail k))]
       [#s(pat:dots attrs head tail)
        #`(parse:dots x fc head tail k)]
       [#s(pat:and attrs subpatterns)
        (for/fold ([k #'k]) ([subpattern (reverse (syntax->list #'subpatterns))])
          #`(parse:S x fc #,subpattern #,k))]
       [#s(pat:or (a ...) (subpattern ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (fail id ...)
                     (with-enclosing-fail fail
                       (let-attributes ([a id] ...) k)))])
              (try (parse:S x fc subpattern
                            (disjunct subpattern success (enclosing-fail) (id ...)))
                   ...)))]
       [#s(pat:compound attrs kind0 (part-pattern ...))
        (let ([kind (get-kind (wash #'kind0))])
          (with-syntax ([(part ...) (generate-temporaries (kind-selectors kind))])
            (with-syntax ([predicate (kind-predicate kind)]
                          [(part-fc ...)
                           (for/list ([fproc (kind-frontier-procs kind)]
                                      [part-var (syntax->list #'(part ...))])
                             (fproc (wash #'fc) part-var))]
                          [(part-expr ...)
                           (for/list ([selector (kind-selectors kind)])
                             (selector #'x #'datum))])
              #`(let ([datum (syntax-e x)])
                  (if (predicate datum)
                      (let ([part part-expr] ...)
                        (parse:S* (part ...) (part-fc ...) (part-pattern ...) k))
                      (fail x
                            #:expect (expectation-of-compound kind0 (part-pattern ...))
                            #:fce fc))))))]
       [#s(pat:cut attrs pattern)
        #`(with-enclosing-fail enclosing-cut-fail
            (parse:S x fc pattern k))]
       [#s(pat:describe attrs description pattern)
        #`(let ([previous-fail enclosing-fail]
                [previous-cut-fail enclosing-cut-fail])
            (define (new-fail failure)
              (fail x
                    #:expect (expectation-of-thing description #f failure)
                    #:fce fc))
            (with-enclosing-fail* new-fail
              (parse:S x #,(empty-frontier #'x) pattern
                       (with-enclosing-cut-fail previous-cut-fail
                         (with-enclosing-fail previous-fail
                           k)))))]
       [#s(pat:bind _ clauses)
        #'(convert-sides x clauses (clause-success () k))]
       [#s(pat:fail _ condition message)
        #`(if condition
              (fail x
                    #:expect (expectation-of-message message)
                    #:fce fc)
              k)])]))

;; (parse:S* (id ...) (FCE ...) (SinglePattern ...) expr) : expr
(define-syntax parse:S*
  (syntax-rules ()
    [(parse:S* () () () k)
     k]
    [(parse:S* (part0 . parts) (fc0 . fcs) (pattern0 . patterns) k)
     (parse:S part0 fc0 pattern0 (parse:S* parts fcs patterns k))]))

;; (disjunct Pattern id (expr ...) (id ...)) : expr
(define-syntax (disjunct stx)
  (syntax-case stx ()
    [(disjunct pattern success (pre ...) (id ...))
     (with-syntax ([(#s(attr sub-id _ _) ...) (pattern-attrs (wash #'pattern))])
       (with-syntax ([(alt-sub-id ...) (generate-temporaries #'(sub-id ...))])
         #`(let ([alt-sub-id (attribute sub-id)] ...)
             (let ([id #f] ...)
               (let ([sub-id alt-sub-id] ...)
                 (success pre ... id ...))))))]))

(begin-for-syntax
 ;; convert-list-pattern : ListPattern id -> SinglePattern
 ;; Converts '() datum pattern at end of list to bind (cons stx index)
 ;; to rest-var.
 (define (convert-list-pattern pattern end-pattern)
   (syntax-case pattern ()
     [#s(pat:datum () ())
      end-pattern]
     [#s(pat:name attrs pattern names)
      (with-syntax ([pattern (convert-list-pattern #'pattern end-pattern)])
        #'#s(pat:name attrs pattern names))]
     [#s(pat:head attrs head tail)
      (with-syntax ([tail (convert-list-pattern #'tail end-pattern)])
        #'#s(pat:head attrs head tail))]
     [#s(pat:dots attrs head tail)
      (with-syntax ([tail (convert-list-pattern #'tail end-pattern)])
        #'#s(pat:dots attrs head tail))]
     [#s(pat:compound attrs #:pair (head-part tail-part))
      (with-syntax ([tail-part (convert-list-pattern #'tail-part end-pattern)])
        #'#s(pat:compound attrs #:pair (head-part tail-part)))])))

;; (parse:H id FCE HeadPattern id id expr) : expr
(define-syntax (parse:H stx)
  (syntax-case stx ()
    [(parse:H x fc head rest index k)
     (syntax-case #'head ()
       [#s(hpat:describe _ description pattern)
        #`(let ([previous-fail enclosing-fail]
                [previous-cut-fail enclosing-cut-fail])
            (define (new-fail failure)
              (fail x
                    #:expect (expectation-of-thing description #f failure)
                    #:fce fc))
            (with-enclosing-fail* new-fail
              (parse:H x #,(empty-frontier #'x) pattern
                       rest index
                       (with-enclosing-cut-fail previous-cut-fail
                         (with-enclosing-fail previous-fail
                           k)))))]
       [#s(hpat:ssc (a ...) parser description bind-term? bind-attrs?)
        #`(let ([result (parser x)])
            (if (ok? result)
                (let ([rest (car result)]
                      [index (cadr result)])
                  (let/unpack ((a ...)
                               #,(let ([bind-term? (syntax-e #'bind-term?)]
                                       [bind-attrs? (syntax-e #'bind-attrs?)])
                                   (cond [(and bind-term? bind-attrs?)
                                          #`(cons (stx-list-take x index) (cddr result))]
                                         [bind-term?
                                          #'(list (stx-list-take x index))]
                                         [bind-attrs?
                                          #'(cddr result)]
                                         [else
                                          #'null])))
                    k))
                (fail x #:expect result #:fce fc)))]
       [#s(hpat:or (a ...) (subpattern ...))
        (with-syntax ([(#s(attr id _ _) ...) #'(a ...)])
          #`(let ([success
                   (lambda (rest index fail id ...)
                     (with-enclosing-fail fail
                       (let-attributes ([a id] ...) k)))])
              (try (parse:H x fc subpattern rest index
                            (disjunct subpattern success
                                      (rest index enclosing-fail) (id ...)))
                   ...)))]
       [#s(hpat:seq attrs pattern)
        (with-syntax ([index0 (frontier->index-expr (wash #'fc))])
          (with-syntax ([pattern
                         (convert-list-pattern
                          #'pattern
                          #'#s(internal-rest-pattern rest index index0))])
            #'(parse:S x fc pattern k)))]
       [_
        (with-syntax ([attrs (pattern-attrs (wash #'head))]
                      [index0 (frontier->index-expr (wash #'fc))])
          #'(parse:S x fc
                     #s(pat:compound attrs
                                     #:pair
                                     (head #s(internal-rest-pattern
                                              rest index
                                              index0)))
                     k))])]))

;; (parse:dots id FCE EHPattern SinglePattern expr) : expr
(define-syntax (parse:dots stx)
  (syntax-case stx ()
    [(parse:dots x fc (#s(ehpat head-attrs head head-repc) ...) tail k)
     (let ()
       (define repcs (wash-list wash #'(head-repc ...)))
       (define rep-ids (for/list ([repc repcs])
                         (and repc (generate-temporary 'rep))))
       (define rel-repcs (filter values repcs))
       (define rel-rep-ids (filter values rep-ids))
       (define aattrs
         (for/list ([head-attrs (syntax->list #'(head-attrs ...))]
                    [repc repcs]
                    #:when #t
                    [a (wash-iattrs head-attrs)])
           (cons a repc)))
       (define attrs (map car aattrs))
       (define attr-repcs (map cdr aattrs))
       (define ids (map attr-name attrs))
       (with-syntax ([(id ...) ids]
                     [(alt-id ...) (generate-temporaries ids)]
                     [reps rel-rep-ids]
                     [(head-rep ...) rep-ids]
                     [(rel-rep ...) rel-rep-ids]
                     [(rel-repc ...) rel-repcs]
                     [(a ...) attrs]
                     [(attr-repc ...) attr-repcs]
                     [loop-fc (frontier:add-index (wash #'fc) #'index)])
         (define-pattern-variable alt-map #'((id . alt-id) ...))
         (define-pattern-variable loop-k
           #'(dots-loop dx (+ index index2) enclosing-fail rel-rep ... alt-id ...))
         #`(let ()
             (define (dots-loop dx index loop-fail rel-rep ... alt-id ...)
               (with-enclosing-fail loop-fail
                 (try (parse:EH dx loop-fc head head-repc index2 alt-map head-rep
                                loop-k)
                      ...
                      (cond [(< rel-rep (rep:min-number rel-repc))
                             (fail dx
                                   #:expect (expectation-of-reps/too-few rel-rep rel-repc)
                                   #:fce loop-fc)]
                            ...
                            [else
                             (let-attributes ([a (rep:finalize attr-repc alt-id)] ...)
                               (parse:S dx loop-fc tail k))]))))
             (let ([rel-rep 0] ...
                   [alt-id (rep:initial-value attr-repc)] ...)
               (dots-loop x 0 enclosing-fail rel-rep ... alt-id ...)))))]))

;; (parse:EH id FCE EHPattern id id ((id . id) ...)
;;           RepConstraint/#f expr) : expr
(define-syntax (parse:EH stx)
  (syntax-case stx ()
    [(parse:EH x fc head repc index alts rep k0)
     (let ()
       (define-pattern-variable k
         (let* ([main-attrs (wash-iattrs (pattern-attrs (wash #'head)))]
                [ids (map attr-name main-attrs)]
                [alt-ids
                 (let ([table (make-bound-id-table)])
                   (for ([entry (syntax->list #'alts)])
                     (let ([entry (syntax-e entry)])
                       (bound-id-table-set! table (car entry) (cdr entry))))
                   (for/list ([id ids]) (bound-id-table-ref table id)))])
           (with-syntax ([(id ...) ids]
                         [(alt-id ...) alt-ids]
                         [(alt-a ...) (map rename-attr main-attrs alt-ids)])
             #`(let ([alt-id (rep:combine repc (attribute id) alt-id)] ...)
                 k0))))
       (syntax-case #'repc ()
         [#f #`(parse:H x fc head x index k)]
         [_  #`(parse:H x fc head x index
                        (if (< rep (rep:max-number repc))
                            (let ([rep (add1 rep)]) k)
                            (fail x
                                  #:expect (expectation-of-reps/too-many rep repc)
                                  #:fce #,(frontier:add-index (wash #'fc)
                                                              #'index))))]))]))

;; (rep:finalize RepConstraint expr) : expr
(define-syntax (rep:finalize stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _) v) #'v]
    [(_ #s(rep:optional _ _) v) #'v]
    [(_ _ v) #'(reverse v)]))

;; (rep:initial-value RepConstraint) : expr
(define-syntax (rep:initial-value stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'#f]
    [(_ #s(rep:optional _ _)) #'#f]
    [(_ _) #'null]))

;; (rep:min-number RepConstraint) : expr
(define-syntax (rep:min-number stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'1]
    [(_ #s(rep:optional _ _)) #'0]
    [(_ #s(rep:bounds min max _ _ _)) #'min]))

;; (rep:max-number RepConstraint) : expr
(define-syntax (rep:max-number stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _)) #'1]
    [(_ #s(rep:optional _ _)) #'1]
    [(_ #s(rep:bounds min max _ _ _)) #'max]))

;; (rep:combine RepConstraint expr expr) : expr
(define-syntax (rep:combine stx)
  (syntax-case stx ()
    [(_ #s(rep:once _ _ _) a b) #'a]
    [(_ #s(rep:optional _ _) a b) #'a]
    [(_ _ a b) #'(cons a b)]))

;; ----

(define-syntax-rule (expectation-of-thing description transparent? chained)
  (make-expect:thing description transparent? chained))

(define-syntax-rule (expectation-of-message message)
  (let ([msg message])
    (if msg (make-expect:message msg) 'ineffable)))

(define-syntax-rule (expectation-of-constant constant)
  (make-expect:atom 'constant))

(define-syntax-rule (expectation-of-literal literal)
  (make-expect:literal (quote-syntax literal)))

(define-syntax expectation-of-compound
  (syntax-rules ()
    [(_ #:pair (head-pattern tail-pattern))
     (make-expect:pair)]
    [(_ _ _) 'ineffable]))

(define-syntax expectation-of-reps/too-few
  (syntax-rules ()
    [(_ rep #s(rep:once name too-few-msg too-many-msg))
     (expectation-of-message/too-few too-few-msg name)]
    [(_ rep #s(rep:optional name too-many-msg))
     (error 'impossible)]
    [(_ rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (expectation-of-message/too-few too-few-msg name)]))

(define-syntax expectation-of-reps/too-many
  (syntax-rules ()
    [(_ rep #s(rep:once name too-few-msg too-many-msg))
     (expectation-of-message/too-many too-many-msg name)]
    [(_ rep #s(rep:optional name too-many-msg))
     (expectation-of-message/too-many too-many-msg name)]
    [(_ rep #s(rep:bounds min max name too-few-msg too-many-msg))
     (expectation-of-message/too-many too-many-msg name)]))

(define-syntax-rule (expectation-of-message/too-few msg name)
  (expectation-of-message
   (or msg
       (let ([n name])
         (if n
             (format "missing required occurrence of ~a" n)
             "repetition constraint violated")))))

(define-syntax-rule (expectation-of-message/too-many msg name)
  (expectation-of-message
   (or msg
       (let ([n name])
         (if n
             (format "too many occurrences of ~a" n)
             "repetition constraint violated")))))
