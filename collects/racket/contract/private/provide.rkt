#lang racket/base

(provide provide/contract
         (for-syntax make-provide/contract-transformer))

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info
                     setup/path-to-relative
                     (prefix-in a: "helpers.rkt"))
         "arrow.rkt"
         "base.rkt"
         "guts.rkt"
         "misc.rkt"
         "exists.rkt"
         syntax/location
         syntax/srcloc)

(define-syntax (verify-contract stx)
  (syntax-case stx ()
    [(_ name x) (a:known-good-contract? #'x) #'x]
    [(_ name x) #'(coerce-contract name x)]))

(define-for-syntax (self-ctor-transformer orig stx)
  (with-syntax ([orig orig])
    (syntax-case stx ()
      [(_ arg ...) (datum->syntax stx
                                  (syntax-e (syntax (orig arg ...)))
                                  stx
                                  stx)]
      [_ (syntax orig)])))

(define-for-syntax make-applicable-struct-info
  (letrec-values ([(struct: make- ? ref set!)
                   (make-struct-type 'self-ctor-struct-info struct:struct-info
                                     1 0 #f
                                     (list (cons prop:procedure
                                                 (lambda (v stx)
                                                   (self-ctor-transformer ((ref v 0)) stx))))
                                     (current-inspector) #f '(0))])
    make-))

(define-for-syntax (make-provide/contract-transformer
                    contract-id id external-id pos-module-source)
  (make-set!-transformer
   (let ([saved-id-table (make-hasheq)])
     (λ (stx)
       (if (eq? 'expression (syntax-local-context))
         ;; In an expression context:
         (let* ([key (syntax-local-lift-context)]
                ;; Already lifted in this lifting context?
                [lifted-id
                 (or (hash-ref saved-id-table key #f)
                     ;; No: lift the contract creation:
                     (with-syntax ([contract-id contract-id]
                                   [id id]
                                   [external-id external-id]
                                   [pos-module-source pos-module-source]
                                   [loc-id (identifier-prune-to-source-module id)])
                       (let ([srcloc-code
                              (with-syntax
                                  ([src
                                    (or (and (path-string? (syntax-source #'id))
                                             (path->relative-string/library
                                              (syntax-source #'id) #f))
                                        (syntax-source #'id))]
                                   [line (syntax-line     #'id)]
                                   [col  (syntax-column   #'id)]
                                   [pos  (syntax-position #'id)]
                                   [span (syntax-span     #'id)])
                                #'(make-srcloc 'src 'line 'col 'pos 'span))])
                         (syntax-local-introduce
                          (syntax-local-lift-expression
                           #`(contract contract-id
                                       id
                                       pos-module-source
                                       (quote-module-name)
                                       'external-id
                                       #,srcloc-code))))))])
           (when key (hash-set! saved-id-table key lifted-id))
           ;; Expand to a use of the lifted expression:
           (with-syntax ([saved-id (syntax-local-introduce lifted-id)])
             (syntax-case stx (set!)
               [name (identifier? #'name) #'saved-id]
               [(set! id arg)
                (raise-syntax-error
                 'provide/contract
                 "cannot set! a provide/contract variable"
                 stx #'id)]
               [(name . more)
                (with-syntax ([app (datum->syntax stx '#%app)])
                  (syntax/loc stx (app saved-id . more)))])))
         ;; In case of partial expansion for module-level and internal-defn
         ;; contexts, delay expansion until it's a good time to lift
         ;; expressions:
         (quasisyntax/loc stx (#%expression #,stx)))))))

(define-for-syntax (true-provide/contract provide-stx)
  (syntax-case provide-stx (struct)
    [(_ p/c-ele ...)
     (let ()
       
       ;; ids : table[id -o> (listof id)]
       ;; code-for-each-clause adds identifiers to this map.
       ;; when it binds things; they are then used to signal 
       ;; a syntax error for duplicates
       (define dups-table (make-hash))
       (define (add-to-dups-table id)
         (hash-update!
          dups-table
          (syntax-e id)
          (λ (ids) (cons id ids))
          '()))
       (define (signal-dup-syntax-error)
         (hash-for-each
          dups-table
          (λ (k ids)
            (let loop ([ids ids])
              (cond
                [(null? ids) (void)]
                [else
                 (cond
                   [(ormap (λ (x) (bound-identifier=? (car ids) x)) (cdr ids))
                    (let ([dups (filter (λ (x) (bound-identifier=? (car ids) x))
                                        ids)])
                      (raise-syntax-error 'provide/contract
                                          "duplicate identifiers"
                                          provide-stx
                                          (car dups)
                                          (cdr dups)))]
                   [else
                    (loop (cdr ids))])])))))
       
       ;; code-for-each-clause : (listof syntax) -> (listof syntax)
       ;; constructs code for each clause of a provide/contract
       (define (code-for-each-clause clauses)
         (let loop ([clauses clauses]
                    [exists-binders '()])
           (cond
           [(null? clauses) null]
           [else
            (let ([clause (car clauses)])
              ;; compare raw identifiers for `struct' and `rename' just like provide does
              (syntax-case* clause (struct rename) (λ (x y) (eq? (syntax-e x) (syntax-e y))) 
                [exists
		 (or (eq? '#:exists (syntax-e #'exists)) (eq? '#:∃ (syntax-e #'exists)))
                 (cond
                   [(null? (cdr clauses))
                    (raise-syntax-error 'provide/conract
                                        (format "expected either a single variable or a sequence of variables to follow ~a, but found nothing"
						(syntax-e #'exists))
                                        provide-stx
                                        clause)]
                   [else
                    (syntax-case (cadr clauses) ()
		      [x
		       (identifier? #'x)
                       (with-syntax ([(x-gen) (generate-temporaries #'(x))])
                         (cons (code-for-one-exists-id #'x #'x-gen)
                               (loop (cddr clauses) 
                                     (add-a-binder #'x #'x-gen exists-binders))))]
                      [(x ...)
                       (andmap identifier? (syntax->list #'(x ...)))
                       (with-syntax ([(x-gen ...) (generate-temporaries #'(x ...))])
                         (append (map code-for-one-exists-id 
                                      (syntax->list #'(x ...))
                                      (syntax->list #'(x-gen ...)))
                                 (loop (cddr clauses)
                                       (let loop ([binders exists-binders]
                                                  [xs (syntax->list #'(x ...))]
                                                  [x-gens (syntax->list #'(x-gen ...))])
                                         (cond
                                           [(null? xs) binders]
                                           [else (loop (add-a-binder (car xs) (car x-gens) binders)
                                                       (cdr xs)
                                                       (cdr x-gens))])))))]
                      [else
                       (raise-syntax-error 'provide/contract
					   (format "expected either a single variable or a sequence of variables to follow ~a"
						   (syntax-e #'exists))
                                           provide-stx
                                           (cadr clauses))])])]
                [(rename this-name new-name contract)
                 (and (identifier? (syntax this-name))
                      (identifier? (syntax new-name)))
                 (begin
                   (add-to-dups-table #'new-name)
                   (cons (code-for-one-id provide-stx 
                                          (syntax this-name) #f
                                          (add-exists-binders (syntax contract) exists-binders)
                                          (syntax new-name))
                         (loop (cdr clauses) exists-binders)))]
                [(rename this-name new-name contract)
                 (identifier? (syntax this-name))
                 (raise-syntax-error 'provide/contract 
                                     "malformed rename clause, expected an identifier" 
                                     provide-stx
                                     (syntax new-name))]
                [(rename this-name new-name contract)
                 (identifier? (syntax new-name))
                 (raise-syntax-error 'provide/contract 
                                     "malformed rename clause, expected an identifier" 
                                     provide-stx
                                     (syntax this-name))]
                [(rename . _)
                 (raise-syntax-error 'provide/contract "malformed rename clause" provide-stx clause)]
                [(struct struct-name ((field-name contract) ...))
                 (and (well-formed-struct-name? (syntax struct-name))
                      (andmap identifier? (syntax->list (syntax (field-name ...)))))
                 (let ([sc (build-struct-code provide-stx
                                              (syntax struct-name)
                                              (syntax->list (syntax (field-name ...)))
                                              (map (λ (x) (add-exists-binders x exists-binders))
                                                   (syntax->list (syntax (contract ...)))))])
                   (add-to-dups-table #'struct-name)
                   (cons sc (loop (cdr clauses) exists-binders)))]
                [(struct name)
                 (identifier? (syntax name))
                 (raise-syntax-error 'provide/contract
                                     "missing fields"
                                     provide-stx
                                     clause)]
                [(struct name . rest)
                 (not (well-formed-struct-name? (syntax name)))
                 (raise-syntax-error 'provide/contract "name must be an identifier or two identifiers with parens around them"
                                     provide-stx
                                     (syntax name))]
                [(struct name (fields ...))
                 (for-each (λ (field)
                             (syntax-case field ()
                               [(x y) 
                                (identifier? (syntax x)) 
                                (void)]
                               [(x y) 
                                (raise-syntax-error 'provide/contract
                                                    "malformed struct field, expected identifier"
                                                    provide-stx
                                                    (syntax x))]
                               [else
                                (raise-syntax-error 'provide/contract
                                                    "malformed struct field"
                                                    provide-stx
                                                    field)]))
                           (syntax->list (syntax (fields ...))))
                 
                 ;; if we didn't find a bad field something is wrong!
                 (raise-syntax-error 'provide/contract "internal error" provide-stx clause)]
                [(struct name . fields)
                 (raise-syntax-error 'provide/contract
                                     "malformed struct fields"
                                     provide-stx
                                     clause)]
                [(name contract)
                 (identifier? (syntax name))
                 (begin
                   (add-to-dups-table #'name)
                   (cons (code-for-one-id provide-stx 
                                          (syntax name) #f
                                          (add-exists-binders (syntax contract)
                                                              exists-binders)
                                          #f)
                         (loop (cdr clauses) exists-binders)))]
                [(name contract)
                 (raise-syntax-error 'provide/contract
                                     "expected identifier"
                                     provide-stx
                                     (syntax name))]
                [unk
                 (raise-syntax-error 'provide/contract
                                     "malformed clause"
                                     provide-stx
                                     (syntax unk))]))])))
       
       ;; well-formed-struct-name? : syntax -> bool
       (define (well-formed-struct-name? stx)
         (or (identifier? stx)
             (syntax-case stx ()
               [(name super)
                (and (identifier? (syntax name))
                     (identifier? (syntax super)))
                #t]
               [else #f])))
       
       ;; build-struct-code : syntax syntax (listof syntax) (listof syntax) -> syntax
       ;; constructs the code for a struct clause
       ;; first arg is the original syntax object, for source locations
       (define (build-struct-code stx struct-name-position field-names field-contracts)
         (let* ([struct-name (syntax-case struct-name-position ()
                               [(a b) (syntax a)]
                               [else struct-name-position])]
                [super-id (syntax-case struct-name-position ()
                            [(a b) (syntax b)]
                            [else #t])]
                
                
                [all-parent-struct-count/names (get-field-counts/struct-names struct-name provide-stx)]
                [parent-struct-count (if (null? all-parent-struct-count/names)
                                         #f
                                         (let ([pp (cdr all-parent-struct-count/names)])
                                           (if (null? pp)
                                               #f
                                               (car (car pp)))))]
                
                [struct-info (a:lookup-struct-info struct-name-position provide-stx)]
                [constructor-id (list-ref struct-info 1)]
                [predicate-id (list-ref struct-info 2)]
                [selector-ids (reverse (list-ref struct-info 3))]
                [type-is-only-constructor? (free-identifier=? constructor-id struct-name)]
                [type-is-constructor? #t] ; I think there's no way to detect when the struct-name binding isn't a constructor
                [is-id-ok?
                 (λ (id i)
                   (if (or (not parent-struct-count)
                           (parent-struct-count . <= . i))
                       id
                       #t))]
                [mutator-ids (reverse (list-ref struct-info 4))] ;; (listof (union #f identifier))
                [field-contract-ids (map (λ (field-name field-contract) 
                                           (if (a:known-good-contract? field-contract)
                                               field-contract
                                               (a:mangle-id provide-stx
                                                            "provide/contract-field-contract"
                                                            field-name
                                                            struct-name)))
                                         field-names
                                         field-contracts)]
                [struct:struct-name
                 (datum->syntax
                  struct-name
                  (string->symbol
                   (string-append
                    "struct:"
                    (symbol->string (syntax-e struct-name)))))]
                
                [-struct:struct-name
                 (datum->syntax
                  struct-name
                  (string->symbol
                   (string-append
                    "-struct:"
                    (symbol->string (syntax-e struct-name)))))]
                
                [is-new-id?
                 (λ (index)
                   (or (not parent-struct-count)
                       (parent-struct-count . <= . index)))])
           
           (let ([unknown-info
                  (λ (what names)
                    (raise-syntax-error
                     'provide/contract
                     (format "cannot determine ~a, found ~s" what names)
                     provide-stx
                     struct-name))])
             
             (unless (or (null? selector-ids)
                         (identifier? (last selector-ids)))
               (unknown-info "the selectors" (map syntax->datum selector-ids)))
             
             (unless constructor-id (unknown-info "constructor" constructor-id))
             (unless predicate-id (unknown-info "predicate" predicate-id))
             (unless (andmap/count is-id-ok? selector-ids)
               (unknown-info "selectors"
                             (map (λ (x) (if (syntax? x)
                                             (syntax->datum x)
                                             x))
                                  selector-ids))))
           
           (unless (equal? (length selector-ids)
                           (length field-contract-ids))
             (raise-syntax-error 'provide/contract
                                 (format "found ~a field~a in struct, but ~a contract~a"
                                         (length selector-ids)
                                         (if (= 1 (length selector-ids)) "" "s")
                                         (length field-contract-ids)
                                         (if (= 1 (length field-contract-ids)) "" "s"))
                                 provide-stx
                                 struct-name))
           
           ;; make sure the field names are right.
           (let* ([relative-counts (let loop ([c (map car all-parent-struct-count/names)])
                                     (cond
                                       [(null? c) null]
                                       [(null? (cdr c)) c]
                                       [else (cons (- (car c) (cadr c))
                                                   (loop (cdr c)))]))]
                  [names (map cdr all-parent-struct-count/names)]
                  [predicate-name (format "~a" (syntax-e predicate-id))]
                  [struct-name (substring predicate-name 0 (sub1 (string-length predicate-name)))])
             (let loop ([count (car relative-counts)]
                        [name (car names)]
                        [counts (cdr relative-counts)]
                        [names (cdr names)]
                        [selector-strs (reverse (map (λ (x) (format "~a" (syntax-e x))) selector-ids))]
                        [field-names (reverse field-names)])
               (cond
                 [(or (null? selector-strs) (null? field-names)) 
                  (void)]
                 [(zero? count) 
                  (loop (car counts) (car names) (cdr counts) (cdr names) 
                        selector-strs
                        field-names)]
                 [else
                  (let* ([selector-str (car selector-strs)]
                         [field-name (car field-names)]
                         [field-name-should-be
                          (substring selector-str 
                                     (+ (string-length name) 1)
                                     (string-length selector-str))]
                         [field-name-is (format "~a" (syntax-e field-name))])
                    (unless (equal? field-name-should-be field-name-is)
                      (raise-syntax-error 'provide/contract
                                          (format "expected field name to be ~a, but found ~a"
                                                  field-name-should-be
                                                  field-name-is)
                                          provide-stx
                                          field-name))
                    (loop (- count 1)
                          name
                          counts
                          names
                          (cdr selector-strs)
                          (cdr field-names)))])))
           
           (with-syntax ([((selector-codes selector-new-names) ...)
                          (filter
                           (λ (x) x)
                           (map/count (λ (selector-id field-contract-id index)
                                        (if (is-new-id? index)
                                            (code-for-one-id/new-name
                                             stx
                                             selector-id #f
                                             (build-selector-contract struct-name 
                                                                      predicate-id
                                                                      field-contract-id)
                                             #f)
                                            #f))
                                      selector-ids
                                      field-contract-ids))]
                         [(rev-selector-old-names ...)
                          (reverse
                           (filter
                            (λ (x) x)
                            (map/count (λ (selector-id index)
                                         (if (not (is-new-id? index))
                                             selector-id
                                             #f))
                                       selector-ids)))]
                         [(mutator-codes/mutator-new-names ...)
                          (map/count (λ (mutator-id field-contract-id index)
                                       (if (and mutator-id (is-new-id? index))
                                           (code-for-one-id/new-name stx
                                                                     mutator-id #f
                                                                     (build-mutator-contract struct-name 
                                                                                             predicate-id
                                                                                             field-contract-id)
                                                                     #f)
                                           #f))
                                     mutator-ids
                                     field-contract-ids)]
                         [(predicate-code predicate-new-name)
                          (code-for-one-id/new-name stx predicate-id #f (syntax (-> any/c boolean?)) #f)]
                         [(constructor-code constructor-new-name)
                          (code-for-one-id/new-name
                           stx
                           constructor-id struct-name
                           (build-constructor-contract stx
                                                       field-contract-ids 
                                                       predicate-id)
                           #f
                           #t
                           (not type-is-only-constructor?))]
                         
                         [(field-contract-id-definitions ...)
                          (filter values (map (λ (field-contract-id field-contract)
                                                (if (a:known-good-contract? field-contract)
                                                    #f
                                                    (with-syntax ([field-contract-id field-contract-id]
                                                                  [field-contract field-contract])
                                                      #'(define field-contract-id (verify-contract 'provide/contract field-contract)))))
                                              field-contract-ids
                                              field-contracts))]
                         [(field-contracts ...) field-contracts]
                         [(field-contract-ids ...) field-contract-ids])
             
             (with-syntax ([((mutator-codes mutator-new-names) ...)
                            (filter syntax-e (syntax->list #'(mutator-codes/mutator-new-names ...)))])
               (with-syntax ([(rev-selector-new-names ...) (reverse (syntax->list (syntax (selector-new-names ...))))]
                             [(rev-mutator-new-names ...) (reverse (syntax->list (syntax (mutator-new-names ...))))])
                 (with-syntax ([struct-code 
                                (with-syntax ([id-rename (a:mangle-id provide-stx 
                                                                      "provide/contract-struct-expandsion-info-id"
                                                                      struct-name)]
                                              [struct-name struct-name]
                                              [-struct:struct-name -struct:struct-name]
                                              [super-id (if (boolean? super-id)
                                                            super-id
                                                            (with-syntax ([super-id super-id])
                                                              (syntax (quote-syntax super-id))))]
                                              [(mutator-id-info ...)
                                               (map (λ (x)
                                                      (syntax-case x ()
                                                        [(a b) #'(quote-syntax b)]
                                                        [else #f]))
                                                    (syntax->list #'(mutator-codes/mutator-new-names ...)))]
                                              [(exported-selector-ids ...) (reverse selector-ids)]
                                              )
                                  #`(begin
                                      (provide (rename-out [id-rename struct-name]))
                                      (define-syntax id-rename
                                        #,(let ([proc 
                                                 #`(lambda () 
                                                     (list (quote-syntax -struct:struct-name)
                                                           #,(if type-is-only-constructor?
                                                                 #'(quote-syntax id-rename)
                                                                 #'(quote-syntax constructor-new-name))
                                                           (quote-syntax predicate-new-name)
                                                           (list (quote-syntax rev-selector-new-names) ...
                                                                 (quote-syntax rev-selector-old-names) ...)
                                                           (list mutator-id-info ...)
                                                           super-id))])
                                            (if type-is-constructor?
                                                #`(make-applicable-struct-info #,proc
                                                                               (lambda ()
                                                                                 (quote-syntax constructor-new-name)))
                                                #`(make-struct-info #,proc))))))]
                               [struct:struct-name struct:struct-name]
                               [-struct:struct-name -struct:struct-name]
                               [struct-name struct-name]
                               [(selector-ids ...) selector-ids])
                   (syntax/loc stx
                     (begin
                       struct-code
                       field-contract-id-definitions ...
                       selector-codes ...
                       mutator-codes ...
                       predicate-code
                       constructor-code
                       
                       ;; expanding out the body of the `make-pc-struct-type' function
                       ;; directly here in the expansion makes this very expensive at compile time
                       ;; when there are a lot of provide/contract clause using structs
                       (define -struct:struct-name 
                         (make-pc-struct-type 'struct-name struct:struct-name field-contract-ids ...))
                       (provide (rename-out [-struct:struct-name struct:struct-name]))))))))))
       
       (define (map/count f . ls)
         (let loop ([ls ls]
                    [i 0])
           (cond
             [(andmap null? ls) '()]
             [(ormap null? ls) (error 'map/count "mismatched lists")]
             [else (cons (apply f (append (map car ls) (list i)))
                         (loop (map cdr ls)
                               (+ i 1)))])))
       
       ;; andmap/count : (X Y int -> Z) (listof X) (listof Y) -> (listof Z)
       (define (andmap/count f l1)
         (let loop ([l1 l1]
                    [i 0])
           (cond
             [(null? l1) #t]
             [else (and (f (car l1) i)
                        (loop (cdr l1)
                              (+ i 1)))])))
       
       ;; get-field-counts/struct-names : syntax syntax -> (listof (cons number symbol))
       ;; returns a list of numbers corresponding to the numbers of fields for each of the parent structs
       (define (get-field-counts/struct-names struct-name provide-stx)
         (let loop ([parent-info-id struct-name])
           (let ([parent-info 
                  (and (identifier? parent-info-id)
                       (a:lookup-struct-info parent-info-id provide-stx))])
             (cond
               [(boolean? parent-info) null]
               [else
                (let ([fields (list-ref parent-info 3)]
                      [predicate (list-ref parent-info 2)])
                  (cond
                    [(and (not (null? fields))
                          (not (last fields)))
                     (raise-syntax-error 
                      'provide/contract
                      "cannot determine the number of fields in super struct"
                      provide-stx
                      struct-name)]
                    [else
                     (cons (cons (length fields) (predicate->struct-name provide-stx predicate))
                           (loop (list-ref parent-info 5)))]))]))))
       
       (define (predicate->struct-name orig-stx stx)
         (and stx
              (let ([m (regexp-match #rx"^(.*)[?]$" (format "~a" (syntax-e stx)))])
                (cond
                  [m (cadr m)]
                  [else (raise-syntax-error 'contract-base.rkt
                                            "unable to cope with a struct supertype whose predicate doesn't end with `?'"
                                            orig-stx)]))))
       
       ;; build-constructor-contract : syntax (listof syntax) syntax -> syntax
       (define (build-constructor-contract stx field-contract-ids predicate-id)
         (with-syntax ([(field-contract-ids ...) field-contract-ids]
                       [predicate-id predicate-id])
           (syntax/loc stx
             (-> field-contract-ids ...
                 predicate-id))))
       
       ;; build-selector-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-selector-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id field-contract-id))))
       
       ;; build-mutator-contract : syntax syntax -> syntax
       ;; constructs the contract for a selector
       (define (build-mutator-contract struct-name predicate-id field-contract-id)
         (with-syntax ([field-contract-id field-contract-id]
                       [predicate-id predicate-id])
           (syntax (-> predicate-id
                       field-contract-id
                       void?))))
       
       ;; code-for-one-exists-id : syntax -> syntax
       (define (code-for-one-exists-id x x-gen)
         #`(define #,x-gen (new-∃/c '#,x)))
       
       (define (add-exists-binders stx exists-binders)
         (if (null? exists-binders)
             stx
             #`(let #,exists-binders #,stx)))
       
       (define (add-a-binder id id-gen binders)
         (cons #`[#,id #,id-gen] binders))
       
       ;; code-for-one-id : syntax syntax syntax (union syntax #f) -> syntax
       ;; given the syntax for an identifier and a contract,
       ;; builds a begin expression for the entire contract and provide
       ;; the first syntax object is used for source locations
       (define (code-for-one-id stx id reflect-id ctrct user-rename-id)
         (with-syntax ([(code id) (code-for-one-id/new-name stx id reflect-id ctrct user-rename-id)])
           (syntax code)))
       
       ;; code-for-one-id/new-name : syntax syntax syntax (union syntax #f) -> (values syntax syntax)
       ;; given the syntax for an identifier and a contract,
       ;; builds a begin expression for the entire contract and provide
       ;; the first syntax object is used for source locations
       (define code-for-one-id/new-name
         (case-lambda
           [(stx id reflect-id ctrct user-rename-id) 
            (code-for-one-id/new-name stx id reflect-id ctrct user-rename-id #f #t)]
           [(stx id reflect-id ctrct user-rename-id mangle-for-maker?)
            (code-for-one-id/new-name id reflect-id ctrct user-rename-id mangle-for-maker? #t)]
           [(stx id reflect-id ctrct/no-prop user-rename-id mangle-for-maker? provide?)
            (let ([no-need-to-check-ctrct? (a:known-good-contract? ctrct/no-prop)]
                  [ex-id (or reflect-id id)]
                  [ctrct (syntax-property ctrct/no-prop
                                          'racket/contract:contract-on-boundary
                                          (gensym 'provide/contract-boundary))])
              (with-syntax ([id-rename ((if mangle-for-maker? 
                                            a:mangle-id-for-maker
                                            a:mangle-id)
                                        provide-stx
                                        "provide/contract-id" 
                                        (or user-rename-id ex-id))]
                            [contract-id (if no-need-to-check-ctrct?
                                             ctrct
                                             (a:mangle-id provide-stx
                                                          "provide/contract-contract-id" 
                                                          (or user-rename-id ex-id)))]
                            [pos-module-source (a:mangle-id provide-stx 
                                                            "provide/contract-pos-module-source"
                                                            (or user-rename-id ex-id))]
                            [pos-stx (datum->syntax id 'here)]
                            [id id]
                            [ex-id ex-id]
                            [ctrct (syntax-property ctrct 'inferred-name ex-id)]
                            [external-name (or user-rename-id id)]
                            [reflect-external-name (or user-rename-id ex-id)]
                            [where-stx stx])
                (with-syntax ([extra-test 
                               (syntax-case #'ctrct (->)
                                 [(-> dom ... arg)
                                  #`(and (procedure? id)
                                         (procedure-arity-includes? id #,(length (syntax->list #'(dom ...)))))]
                                 [_ #f])])
                  (with-syntax ([code
                                 (syntax-property
                                  (quasisyntax/loc stx
                                    (begin
                                      (define pos-module-source (quote-module-name))
                                      
                                      #,@(if no-need-to-check-ctrct?
                                             (list)
                                             (list #'(define contract-id 
                                                       (let ([ex-id ctrct]) ;; let is here to give the right name.
                                                         (verify-contract 'provide/contract ex-id)))))
                                      (define-syntax id-rename
                                        (make-provide/contract-transformer (quote-syntax contract-id)
                                                                           (a:update-loc
                                                                            (quote-syntax id)
                                                                            (vector
                                                                             '#,(syntax-source #'id)
                                                                             #,(syntax-line #'id)
                                                                             #,(syntax-column #'id)
                                                                             #,(syntax-position #'id)
                                                                             #,(syntax-span #'id)))
                                                                           (quote-syntax reflect-external-name)
                                                                           (quote-syntax pos-module-source)))
                                      
                                      #,@(if provide?
                                             (list #`(provide (rename-out [id-rename external-name])))
                                             null)))
                                  'provide/contract-original-contract
                                  (vector #'external-name #'ctrct))])
                    
                    (syntax-local-lift-module-end-declaration
                     #`(begin 
                         (unless extra-test
                           (contract contract-id id pos-module-source 'ignored 'id 
                                     (quote-srcloc id)))
                         (void)))
                    
                    (syntax (code id-rename))))))]))
       
       (with-syntax ([(bodies ...) (code-for-each-clause (syntax->list (syntax (p/c-ele ...))))])
         (signal-dup-syntax-error)
         (syntax 
          (begin
            bodies ...))))]))

(define-syntax (provide/contract stx)
  (define s-l-c (syntax-local-context))
  (case s-l-c
   [(module-begin) ;; the case under discussion
    #`(begin (define-values () (values))  ;; force us into the 'module' local context
             #,stx)]
   [(module) ;; the good case
    (true-provide/contract stx)]
   [else ;; expression or internal definition
    (raise-syntax-error 'provide/contract 
                        (format "not allowed in a ~a context"
                                (if (pair? s-l-c)
                                    "internal definition"
                                    s-l-c))
                        stx)]))

(define (make-pc-struct-type struct-name struct:struct-name . ctcs)
  (let-values ([(struct:struct-name _make _pred _get _set)
                (make-struct-type struct-name
                                  struct:struct-name
                                  0 ;; init
                                  0 ;; auto
                                  #f  ;; auto-v
                                  '() ;; props
                                  #f  ;; inspector
                                  #f ;; proc-spec
                                  '()  ;; immutable-k-list
                                  (λ args
                                    (let ([vals (let loop ([args args])
                                                  (cond
                                                    [(null? args) null]
                                                    [(null? (cdr args)) null]
                                                    [else (cons (car args) (loop (cdr args)))]))])
                                      (apply values
                                             (map (λ (ctc val)
                                                    (contract ctc
                                                              val
                                                              'not-enough-info-for-blame
                                                              'not-enough-info-for-blame
                                                              '#f
                                                              (build-source-location #f)))
                                                  ctcs
                                                  vals)))))])
    struct:struct-name))
