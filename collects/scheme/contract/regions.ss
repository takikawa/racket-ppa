#lang scheme/base

(provide define-struct/contract
         define/contract
         with-contract)

(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info
                     syntax/define
                     syntax/kerncase
                     syntax/parse
                     unstable/syntax
                     (prefix-in a: "private/helpers.ss"))
         scheme/splicing
         scheme/stxparam
         unstable/location
         "private/arrow.ss"
         "private/base.ss"
         "private/guts.ss")

;; These are useful for all below.

(define-syntax (verify-contract stx)
  (syntax-case stx ()
    [(_ name x) (a:known-good-contract? #'x) #'x]
    [(_ name x) #'(coerce-contract name x)]))



;                                                                                                    
;                                                                                                    
;                                                                                                    
;       ;           ;;; ;                     ;                                                      
;       ;          ;                          ;                                                      
;       ;          ;                         ;                           ;                       ;   
;    ;; ;    ;;;  ;;;;  ;   ; ;;     ;;;     ;     ;;;    ;;;    ; ;;   ;;;;  ; ;  ;;;     ;;;  ;;;; 
;   ;  ;;   ;   ;  ;    ;   ;;  ;   ;   ;    ;    ;   ;  ;   ;   ;;  ;   ;    ;;  ;   ;   ;   ;  ;   
;  ;    ;  ;    ;  ;    ;   ;   ;  ;    ;    ;   ;      ;     ;  ;   ;   ;    ;       ;  ;       ;   
;  ;    ;  ;;;;;;  ;    ;   ;   ;  ;;;;;;   ;    ;      ;     ;  ;   ;   ;    ;    ;;;;  ;       ;   
;  ;    ;  ;       ;    ;   ;   ;  ;        ;    ;      ;     ;  ;   ;   ;    ;   ;   ;  ;       ;   
;   ;  ;;   ;      ;    ;   ;   ;   ;       ;     ;   ;  ;   ;   ;   ;   ;    ;   ;   ;   ;   ;  ;   
;    ;; ;    ;;;;  ;    ;   ;   ;    ;;;;  ;       ;;;    ;;;    ;   ;    ;;  ;    ;;;;;   ;;;    ;; 
;                                          ;                                                         
;                                          ;                                                         
;                                                                                                    

;; (define/contract id contract expr)
;; defines `id' with `contract'; initially binding
;; it to the result of `expr'.  These variables may not be set!'d.
(define-syntax (define/contract define-stx)
  (define-splicing-syntax-class fv-clause
    #:description "a free variable clause"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq #:freevars ([var:id ctc:expr] ...))]
    [pattern (~seq #:freevar v:id c:expr)
             #:with (var ...) (list #'v)
             #:with (ctc ...) (list #'c)])
  (define-splicing-syntax-class fvs
    #:description "a sequence of free variable clauses"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq f:fv-clause ...)
             #:with (var ...) #'(f.var ... ...)
             #:with (ctc ...) #'(f.ctc ... ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate imported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (when (memq (syntax-local-context) '(expression module-begin))
    (raise-syntax-error 'define/contract
                        "not used in definition context"
                        define-stx))
  (syntax-parse define-stx
    [(_ name:id contract fv:fvs body)
     (syntax/loc define-stx
       (with-contract #:region definition name
         ([name contract])
         #:freevars ([fv.var fv.ctc] ...)
         (define name body)))]
    [(_ name:id contract fv:fvs body0 body ...)
     (raise-syntax-error 'define/contract
                         "multiple expressions after identifier and contract"
                         #'(body ...))]
    [(_ name+arg-list contract fv:fvs body0 body ...)
     (let-values ([(name body-expr)
                   (normalize-definition
                    (datum->syntax #'define-stx (list* 'define/contract #'name+arg-list
                                                       #'body0 #'(body ...)))
                    #'lambda #t #t)])
       (with-syntax ([name name]
                     [body-expr body-expr])
         (syntax/loc define-stx
           (with-contract #:region function name
             ([name contract])
             #:freevars ([fv.var fv.ctc] ...)
             (define name body-expr)))))]))

(define-syntax (define-struct/contract stx)
  (define-struct field-info (stx ctc [mutable? #:mutable] auto?))
  (define-struct s-info (auto-value-stx transparent? def-stxs? def-vals?))

  (define syntax-error
    (lambda v
      (apply raise-syntax-error 'define-struct/contract v)))
  
  (define (build-struct-names name field-infos)
    (let ([name-str (symbol->string (syntax-case name ()
                                      [id (identifier? #'id)
                                          (syntax-e #'id)]
                                      [(sub super) 
                                       (syntax-e #'sub)]))])
      (list* 
       (syntax-case name ()
         [id (identifier? #'id) #'id]
         [(sub super) #'sub])
       (datum->syntax 
        name
        (string->symbol
         (string-append "struct:" name-str)))
       (datum->syntax 
        name
        (string->symbol
         (string-append "make-" name-str)))
       (datum->syntax 
        name
        (string->symbol
         (string-append name-str "?")))           
       (apply append
              (for/list ([finfo field-infos])
                (let ([field-str (symbol->string (syntax-e (field-info-stx finfo)))])
                  (cons (datum->syntax 
                         name
                         (string->symbol
                          (string-append name-str "-" field-str)))
                        (if (field-info-mutable? finfo)
                            (list (datum->syntax 
                                   name
                                   (string->symbol
                                    (string-append "set-" name-str "-" field-str "!"))))
                            null))))))))
  
  (define (build-contracts stx pred field-infos)
    (list* (syntax/loc stx any/c)
           (syntax/loc stx any/c)
           (apply append
                  (for/list ([finfo field-infos])
                    (let ([field-ctc (field-info-ctc finfo)])
                      (cons (quasisyntax/loc stx
                              (-> #,pred #,field-ctc))
                            (if (field-info-mutable? finfo)
                                (list 
                                 (quasisyntax/loc stx
                                   (-> #,pred #,field-ctc void?)))
                                null)))))))
  
  (define (check-field f ctc)
    (let ([p-list (syntax->list f)])
      (if p-list
          (begin 
            (when (null? p-list)
              (syntax-error "expected struct field" f))
            (unless (identifier? (car p-list))
              (syntax-error "expected identifier" f))
            (let loop ([rest (cdr p-list)]
                       [mutable? #f]
                       [auto? #f])
              (if (null? rest) 
                  (make-field-info (car p-list) ctc mutable? auto?)
                  (let ([elem (syntax-e (car rest))])
                    (if (keyword? elem)
                        (cond
                          [(eq? elem '#:mutable)
                           (begin (when mutable?
                                    (syntax-error "redundant #:mutable"
                                                  (car rest)))
                                  (loop (cdr rest) #t auto?))]
                          [(eq? elem '#:auto)
                           (begin (when auto?
                                    (syntax-error "redundant #:mutable"
                                                  (car rest)))
                                  (loop (cdr rest) mutable? #t))]
                          [else (syntax-error "expected #:mutable or #:auto"
                                              (car rest))])
                        (syntax-error "expected #:mutable or #:auto"
                                      (car rest)))))))
          (if (identifier? f)
              (make-field-info f ctc #f #f)
              (syntax-error "expected struct field" f)))))
  (define (check-kwds kwd-list field-infos)
    (let loop ([kwds kwd-list]
               [auto-value-stx #f]
               [mutable? #f]
               [transparent? #f]
               [def-stxs? #t]
               [def-vals? #t])
      (if (null? kwds)
          (make-s-info auto-value-stx transparent? def-stxs? def-vals?)
          (let ([kwd (syntax-e (car kwds))])
            (when (not (keyword? kwd))
              (syntax-error "expected a keyword"
                            (car kwds)))
            (cond
              [(eq? kwd '#:auto-value)
               (when (null? (cdr kwds))
                 (syntax-error "expected a following expression"
                               (car kwds)))
               (loop (cddr kwds) (cadr kwds)
                     transparent? mutable? def-stxs? def-vals?)]
              ;; let arbitrary properties through
              [(eq? kwd '#:property)
               (when (null? (cdr kwds))
                 (syntax-error "expected a property"
                               (car kwds)))
               (when (null? (cddr kwds))
                 (syntax-error "expected a value for the property"
                               (car kwds)))
               (loop (cdddr kwds) auto-value-stx
                     mutable? transparent? def-stxs? def-vals?)]
              [(eq? kwd '#:mutable)
               (when mutable?
                 (syntax-error "redundant #:mutable"
                               (car kwds)))
               (for ([finfo field-infos])
                 (set-field-info-mutable?! finfo #t))
               (loop (cdr kwds) auto-value-stx
                     #t transparent? def-stxs? def-vals?)]
              [(eq? kwd '#:transparent)
               (when transparent?
                 (syntax-error "redundant #:transparent"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     mutable? #t def-stxs? def-vals?)]
              [(eq? kwd '#:omit-define-syntaxes)
               (when (not def-stxs?)
                 (syntax-error "redundant #:omit-define-syntaxes"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     transparent? mutable? #f def-vals?)]
              [(eq? kwd '#:omit-define-values)
               (when (not def-vals?)
                 (syntax-error "redundant #:omit-define-values"
                               (car kwds)))
               (loop (cdr kwds) auto-value-stx
                     transparent? mutable? def-stxs? #f)]
              [else (syntax-error "unexpected keyword"
                                  (car kwds))])))))
  (syntax-case stx ()
    [(_ name ([field ctc] ...) kwds ...)
     (let ([fields (syntax->list #'(field ...))])
       (unless (or (identifier? #'name)
                   (syntax-case #'name ()
                     [(x y) (and (identifier? #'x)
                                 (identifier? #'y))]
                     [_ #f]))
         (syntax-error "expected identifier for struct name or a sub-type relationship (subtype supertype)"
                       #'name))
       (let* ([field-infos (map check-field fields (syntax->list #'(ctc ...)))]
              [sinfo (check-kwds (syntax->list #'(kwds ...)) field-infos)]
              [names (build-struct-names #'name field-infos)]
              [pred (cadddr names)]
              [ctcs (build-contracts stx pred field-infos)]
              [super-fields (syntax-case #'name ()
                              [(child parent)
                               (let ([v (syntax-local-value #'parent (lambda () #f))])
                                 (unless (struct-info? v)
                                   (raise-syntax-error #f "identifier is not bound to a structure type" stx #'parent))
                                 (let ([v (extract-struct-info v)])
                                   (cadddr v)))]
                              [else '()])])
         (let-values ([(non-auto-fields auto-fields)
                       (let loop ([fields field-infos]
                                  [nautos null]
                                  [autos null])
                         (if (null? fields)
                             (values (reverse nautos)
                                     (reverse autos))
                             (if (field-info-auto? (car fields))
                                 (loop (cdr fields)
                                       nautos
                                       (cons (car fields) autos))
                                 (if (null? autos)
                                     (loop (cdr fields)
                                           (cons (car fields) nautos)
                                           autos)
                                     (syntax-error "non-auto field after auto fields"
                                                   (field-info-stx (car fields)))))))])
           (with-syntax ([ctc-bindings
                          (if (s-info-def-vals? sinfo)
                              (map list (cddr names)
                                   ctcs)
                              null)]
                         [orig stx]
                         [struct-name (syntax-case #'name ()
                                        [id (identifier? #'id) #'id]
                                        [(id1 super) #'id1])]
                         [(auto-check ...)
                          (let* ([av-stx (if (s-info-auto-value-stx sinfo)
                                             (s-info-auto-value-stx sinfo)
                                             #'#f)]
                                 [av-id (datum->syntax av-stx
                                                       (string->symbol
                                                        (string-append (syntax-case #'name ()
                                                                         [id (identifier? #'id)
                                                                             (symbol->string (syntax-e #'id))]
                                                                         [(id1 super)
                                                                          (symbol->string (syntax-e #'id1))])
                                                                       ":auto-value"))
                                                       av-stx)])
                            (for/list ([finfo auto-fields])
                              #`(let ([#,av-id #,av-stx])
                                  (contract #,(field-info-ctc finfo)
                                            #,av-id
                                            '(struct name)
                                            'cant-happen
                                            (quote #,av-id)
                                            (quote-srcloc #,av-id)))))]
                         ;; a list of variables, one for each super field
                         [(super-fields ...) (generate-temporaries super-fields)]
                         ;; the contract for a super field is any/c becuase the
                         ;; super constructor will have its own contract
                         [(super-contracts ...) (for/list ([i (in-list super-fields)])
                                                  (datum->syntax stx 'any/c))]
                         [(non-auto-contracts ...)
                          (map field-info-ctc
                               (filter (lambda (f)
                                         (not (field-info-auto? f)))
                                       field-infos))]
                         ;; the make-foo function. this is used to make the contract
                         ;; print the right name in the blame
                         [maker (caddr names)]
                         [(non-auto-name ...)
                          (map field-info-stx non-auto-fields)])
             (syntax/loc stx
               (begin
                 (define-values () (begin auto-check ... (values)))
                 (define (guard super-fields ... non-auto-name ... struct-name)
                   (values super-fields ... non-auto-name ...))
                 (define blame-id
                   (current-contract-region))
                 (with-contract #:region struct struct-name
                                ctc-bindings
                                (define-struct/derived orig name (field ...)
                                  kwds ...
                                  #:guard (contract (-> super-contracts ... non-auto-contracts ... symbol? any)
                                                    guard
                                                    (current-contract-region) blame-id
                                                    (quote maker)
                                                    (quote-srcloc maker))))))))))]
    [(_ name . bad-fields)
     (identifier? #'name)
     (syntax-error "expected a list of field name/contract pairs"
                   #'bad-fields)]
    [(_ . body)
     (syntax-error "expected a structure name"
                   #'body)]))

;                                                                                         
;                                                                                         
;              ;       ;                                                                  
;                  ;   ;                                     ;                         ;  
;                  ;   ;                                     ;                         ;  
;  ;    ;    ; ;  ;;;; ; ;;;          ;;;     ;;;    ; ;;;  ;;;; ; ;;   ;;;;     ;;;  ;;;;
;  ;    ;    ; ;   ;   ;;   ;        ;   ;   ;   ;   ;;   ;  ;   ;;    ;    ;   ;   ;  ;  
;   ;  ; ;  ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;   ;  ; ;  ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;        ;;;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;     ;;;  ;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ; ;;;;  ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;   ; ;   ; ;  ;   ;   ;    ;       ;       ;     ;  ;    ;  ;   ;    ;     ;  ;       ;  
;    ;     ;   ;   ;   ;    ;        ;   ;   ;   ;   ;    ;  ;   ;    ;    ;;   ;   ;  ;  
;    ;     ;   ;   ;;; ;    ;         ;;;     ;;;    ;    ;  ;;; ;     ;;;; ;    ;;;   ;;;
;                                                                                         
;                                                                                         
;                                                                                         

(define-for-syntax (make-contracted-id-transformer id contract-stx pos-blame-stx neg-blame-stx)
  (with-syntax ([ctc contract-stx]
                [id id]
                [pos pos-blame-stx]
                [neg neg-blame-stx])
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! i arg)
          (quasisyntax/loc stx
            (set! id (contract ctc arg neg pos (quote id) (quote-srcloc id))))]
         [(f arg ...)
          (quasisyntax/loc stx
            ((contract ctc id pos neg (quote id) (quote-srcloc id))
             arg ...))]
         [ident
          (identifier? (syntax ident))
          (quasisyntax/loc stx
            (contract ctc id pos neg (quote id) (quote-srcloc id)))])))))

(define-syntax (with-contract-helper stx)
  (syntax-case stx ()
    [(_ ())
     #'(begin)]
    [(_ (p0 p ...))
     (raise-syntax-error 'with-contract
                         "no definition found for identifier"
                         #'p0)]
    [(_ (p ...) body0 body ...)
     (let ([expanded-body0 (local-expand #'body0
                                         (syntax-local-context)
                                         (kernel-form-identifier-list))])
       (define (filter-ids to-filter to-remove)
         (filter (λ (i1)
                   (not (memf (λ (i2)
                                (bound-identifier=? i1 i2))
                              to-remove)))
                 to-filter))
       (syntax-case expanded-body0 (begin define-values define-syntaxes)
         [(begin sub ...)
          (syntax/loc stx
            (with-contract-helper (p ...) sub ... body ...))]
         [(define-syntaxes (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (with-syntax ([def expanded-body0]
                          [unused-ps (filter-ids (syntax->list #'(p ...)) ids)])
              (with-syntax ()
                (syntax/loc stx
                  (begin def (with-contract-helper unused-ps body ...))))))]
         [(define-values (id ...) expr)
          (let ([ids (syntax->list #'(id ...))])
            (with-syntax ([def expanded-body0]
                          [unused-ps (filter-ids (syntax->list #'(p ...)) ids)])
              (syntax/loc stx
                (begin def (with-contract-helper unused-ps body ...)))))]
         [else
          (quasisyntax/loc stx
            (begin #,expanded-body0
                   (with-contract-helper (p ...) body ...)))]))]))

(define-syntax (with-contract stx)
  (define-splicing-syntax-class region-clause
    #:description "contract region type"
    [pattern (~seq #:region region:id)])
  (define-splicing-syntax-class fv-clause
    #:description "a free variable clause"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq #:freevars ([var:id ctc:expr] ...))]
    [pattern (~seq #:freevar v:id c:expr)
             #:with (var ...) (list #'v)
             #:with (ctc ...) (list #'c)])
  (define-splicing-syntax-class fvs
    #:description "a sequence of free variable clauses"
    #:attributes ([var 1] [ctc 1])
    [pattern (~seq f:fv-clause ...)
             #:with (var ...) #'(f.var ... ...)
             #:with (ctc ...) #'(f.ctc ... ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate imported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (define-syntax-class export-clause
    #:description "a name/contract pair"
    [pattern (var:id ctc:expr)])
  (define-syntax-class exports-clause
    #:attributes ([var 1] [ctc 1])
    #:description "a sequence of name/contract pairs"
    [pattern (ec:export-clause ...)
             #:with (var ...) #'(ec.var ...)
             #:with (ctc ...) #'(ec.ctc ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
             (format "duplicate exported name ~a" 
                     (syntax-e (check-duplicate-identifier (syntax->list #'(var ...)))))])
  (define-splicing-syntax-class results-clause
    #:attributes ([ctc 1])
    #:description "a results clause"
    [pattern (~seq #:result c:expr)
	     #:with (ctc ...) #'(c)]
    [pattern (~seq #:results (ctc:expr ...))])
  (syntax-parse stx
    [(_ (~optional :region-clause #:defaults ([region #'region])) blame:id rc:results-clause fv:fvs . body)
     (if (not (eq? (syntax-local-context) 'expression))
         (quasisyntax/loc stx (#%expression #,stx))
         (let*-values ([(intdef) (syntax-local-make-definition-context)]
                       [(ctx) (list (gensym 'intdef))]
                       [(cid-marker) (make-syntax-introducer)]
                       [(free-vars free-ctcs)
                        (values (syntax->list #'(fv.var ...))
                                (syntax->list #'(fv.ctc ...)))])
           (define (add-context stx)
             (let ([ctx-added-stx (local-expand #`(quote #,stx) ctx (list #'quote) intdef)])
               (syntax-case ctx-added-stx ()
                 [(_ expr) #'expr])))
           (syntax-local-bind-syntaxes free-vars #f intdef)
           (internal-definition-context-seal intdef)
           (with-syntax ([blame-stx #''(region blame)]
                         [blame-id (generate-temporary)]
                         [(res ...) (generate-temporaries #'(rc.ctc ...))]
                         [(free-var ...) free-vars]
                         [(free-var-id ...) (add-context #`#,free-vars)]
                         [(free-ctc-id ...) (map cid-marker free-vars)]
                         [(free-ctc ...) (map (λ (c v)
                                                (syntax-property c 'inferred-name v))
                                              free-ctcs
                                              free-vars)])
             (with-syntax ([new-stx (add-context #'(syntax-parameterize 
                                                    ([current-contract-region (λ (stx) #'blame-stx)])
                                                    (let-values ([(res ...) (let () . body)])
                                                      (values (contract (verify-contract 'with-contract rc.ctc)
                                                                        res
                                                                        blame-stx
                                                                        blame-id) ...))))])
               (quasisyntax/loc stx
                 (let ()
                   (define-values (free-ctc-id ...)
                     (values (verify-contract 'with-contract free-ctc) ...))
                   (define blame-id
                     (current-contract-region))
                   (define-values ()
                     (begin (contract free-ctc-id
                                      free-var
                                      blame-id
                                      'cant-happen
                                      (quote free-var)
                                      (quote-srcloc free-var))
                            ...
                            (values)))
                   (define-syntaxes (free-var-id ...)
                     (values (make-contracted-id-transformer
                              (quote-syntax free-var)
                              (quote-syntax free-ctc-id)
                              (quote-syntax blame-id)
                              (quote-syntax blame-stx)) ...))
                   new-stx))))))]
    [(_ (~optional :region-clause #:defaults ([region #'region])) blame:id ec:exports-clause fv:fvs . body)
     (when (memq (syntax-local-context) '(expression module-begin))
       (raise-syntax-error 'with-contract
                           "not used in definition context"
                           stx))
     (let*-values ([(intdef) (syntax-local-make-definition-context)]
                   [(ctx) (list (gensym 'intdef))]
                   [(cid-marker) (make-syntax-introducer)]
                   [(free-vars free-ctcs)
                    (values (syntax->list #'(fv.var ...))
                            (syntax->list #'(fv.ctc ...)))]
                   [(protected protections)
                    (values (syntax->list #'(ec.var ...))
                            (syntax->list #'(ec.ctc ...)))])
       (define (add-context stx)
         (let ([ctx-added-stx (local-expand #`(quote #,stx)
                                            ctx
                                            (list #'quote)
                                            intdef)])
           (syntax-case ctx-added-stx ()
             [(_ expr) #'expr])))
       (syntax-local-bind-syntaxes protected #f intdef)
       (syntax-local-bind-syntaxes free-vars #f intdef)
       (internal-definition-context-seal intdef)
       (with-syntax ([blame-stx #''(region blame)]
                     [blame-id (generate-temporary)]
                     [(free-var ...) free-vars]
                     [(free-var-id ...) (add-context #`#,free-vars)]
                     [(free-ctc-id ...) (map cid-marker free-vars)]
                     [(free-ctc ...) (map (λ (c v)
                                            (syntax-property c 'inferred-name v))
                                          free-ctcs
                                          free-vars)]
                     [(ctc-id ...) (map cid-marker protected)]
                     [(ctc ...) (map (λ (c v)
                                       (syntax-property (add-context c) 'inferred-name v))
                                     protections
                                     protected)]
                     [(p ...) protected]
                     [(marked-p ...) (add-context #`#,protected)])
         (with-syntax ([new-stx (add-context #'(splicing-syntax-parameterize 
                                                ([current-contract-region (λ (stx) #'blame-stx)])
                                                . body))])
           (quasisyntax/loc stx
             (begin
               (define-values (free-ctc-id ...)
                 (values (verify-contract 'with-contract free-ctc) ...))
               (define blame-id
                 (current-contract-region))
               (define-values ()
                 (begin (contract free-ctc-id
                                  free-var
                                  blame-id
                                  'cant-happen
                                  (quote free-var)
                                  (quote-srcloc free-var))
                        ...
                        (values)))
               (define-syntaxes (free-var-id ...)
                 (values (make-contracted-id-transformer
                          (quote-syntax free-var)
                          (quote-syntax free-ctc-id)
                          (quote-syntax blame-id)
                          (quote-syntax blame-stx)) ...))
               (with-contract-helper (marked-p ...) new-stx)
               (define-values (ctc-id ...)
                 (values (verify-contract 'with-contract ctc) ...))
               (define-values ()
                 (begin (contract ctc-id
                                  marked-p
                                  blame-stx
                                  'cant-happen
                                  (quote marked-p)
                                  (quote-srcloc marked-p))
                        ...
                        (values)))
               (define-syntaxes (p ...)
                 (values (make-contracted-id-transformer
                          (quote-syntax marked-p)
                          (quote-syntax ctc-id)
                          (quote-syntax blame-stx)
                          (quote-syntax blame-id)) ...)))))))]))
