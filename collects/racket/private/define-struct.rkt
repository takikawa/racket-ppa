
;; Based on
;;  (planet "struct.ss" ("ryanc" "macros.plt" 1 0)))

(module define-struct '#%kernel
  (#%require "more-scheme.rkt" "small-scheme.rkt" "define.rkt" "../stxparam.rkt"
             (for-syntax '#%kernel "define.rkt"
                         "stx.rkt" "stxcase-scheme.rkt" "small-scheme.rkt" 
                         "stxloc.rkt" "qqstx.rkt"
                         "struct-info.rkt"))

  (#%provide define-struct*
             define-struct/derived
             struct-field-index
             struct-copy
             (for-syntax 
	      (rename checked-struct-info-rec? checked-struct-info?)))
  
  (define-values-for-syntax
    (struct:checked-struct-info 
     make-checked-struct-info 
     checked-struct-info-rec?
     checked-struct-info-ref
     checked-struct-info-set!)
    (make-struct-type 'checked-struct-info struct:struct-info
                      0 0 #f
                      null (current-inspector)
                      (lambda (v stx)
                        (raise-syntax-error
                         #f
                         "identifier for static struct-type information cannot be used as an expression"
                         stx))
                      null
                      (lambda (proc info)
                        (if (and (procedure? proc)
                                 (procedure-arity-includes? proc 0))
                            proc
                            (raise-type-error 'make-struct-info
                                              "procedure (arity 0)"
                                              proc)))))

  (define-for-syntax (self-ctor-transformer orig stx)
    (define (transfer-srcloc orig stx)
      (datum->syntax orig (syntax-e orig) stx orig))
    (syntax-case stx ()
      [(self arg ...) (datum->syntax stx
                                     (cons (transfer-srcloc orig #'self)
                                           (syntax-e (syntax (arg ...))))
                                     stx
                                     stx)]
      [_ (transfer-srcloc orig stx)]))
  
  (define-values-for-syntax (make-self-ctor-struct-info)
    (letrec-values ([(struct: make- ? ref set!)
                     (make-struct-type 'self-ctor-struct-info struct:struct-info
                                       1 0 #f
                                       (list (cons prop:procedure
                                                   (lambda (v stx)
                                                     (self-ctor-transformer ((ref v 0)) stx))))
                                       (current-inspector) #f '(0))])
      make-))
  (define-values-for-syntax (make-self-ctor-checked-struct-info)
    (letrec-values ([(struct: make- ? ref set!)
                     (make-struct-type 'self-ctor-checked-struct-info struct:checked-struct-info
                                       1 0 #f
                                       (list (cons prop:procedure
                                                   (lambda (v stx)
                                                     (self-ctor-transformer ((ref v 0)) stx))))
                                       (current-inspector) #f '(0))])
      make-))

  (define-syntax-parameter struct-field-index
    (lambda (stx)
      (raise-syntax-error #f "allowed only within a structure type definition" stx)))

  (define (check-struct-type name what)
    (when what
      (unless (struct-type? what)
        (raise-type-error name "struct-type or #f" what)))
    what)

  (define (check-inspector name what)
    (when what
      (unless (inspector? what)
        (raise-type-error name "inspector or #f" what)))
    what)

  (define (check-reflection-name name what)
    (unless (symbol? what)
      (raise-type-error name "symbol" what))
    what)

  (define-syntax (define-struct* stx)
    (syntax-case stx ()
      [(_ . rest)
       (with-syntax ([stx stx])
         #'(define-struct/derived stx . rest))]))

  (define-syntax (define-struct/derived full-stx)
    (define make-field list)
    (define field-id car)
    (define field-default-value cadr)
    (define field-auto? caddr)
    (define field-mutable? cadddr)

    (define (build-name id . parts)
      (datum->syntax
       id
       (string->symbol
        (apply string-append
               (map (lambda (p)
                      (if (syntax? p)
                          (symbol->string (syntax-e p))
                          p))
                    parts)))
       id))

    (define (bad why kw where . alt)
      (raise-syntax-error
       #f
       (format "~a ~a specification~a"
               why
               (if (string? kw)
                   kw
                   (syntax-e kw))
               where)
       stx
       (if (null? alt) kw (car alt))))

    (define (check-exprs orig-n ps what)
      (let loop ([nps (cdr ps)][n orig-n])
        (unless (zero? n)
          (unless (and (pair? nps)
                       (not (keyword? (syntax-e (car nps)))))
            (raise-syntax-error
             #f
             (format "expected ~a ~a~a after keyword~a"
                     orig-n
                     (or what "expression")
                     (if (= orig-n 1) "" "s")
                     (if (pair? nps)
                         ", found a keyword"
                         ""))
             stx
             (car ps)))
          (loop (cdr nps) (sub1 n)))))
    
    ;; Parse one field with a sequence of keyword-based specs:
    (define (parse-field f)
      (syntax-case f ()
        [id
         (identifier? #'id)
         (make-field #'id #f #f #f)]
        [(id p ...)
         (identifier? #'id)
         (let loop ([ps (syntax->list #'(p ...))]
                    [def-val #f]
                    [auto? #f]
                    [mutable? #f])
           (cond
            [(null? ps) (make-field #'id def-val auto? mutable?)]
            [(eq? '#:mutable (syntax-e (car ps)))
             (when mutable?
               (bad "redundant" (car ps) " for field"))
             (loop (cdr ps) def-val auto? #t)]
            #;
            [(eq? #:default (syntax-e (car ps)))
             (check-exprs 1 ps #f)
             (when def-val
               (bad "multiple" (car ps) " for field"))
             (loop (cddr ps) (cadr ps) auto? mutable?)]
            [(eq? '#:auto (syntax-e (car ps)))
             (when auto?
               (bad "redundant" (car ps) " for field"))
             (loop (cdr ps) def-val #t mutable?)]
            [else
             (raise-syntax-error
              #f
              (if (keyword? (syntax-e (car ps)))
                  "unrecognized field-specification keyword"
                  "expected a field-specification keyword")
              stx
              (car ps))]))]
        [_else
         (raise-syntax-error
          #f
          "expected a field identifier or a parenthesized identifier and field-specification sequence"
          stx
          f)]))

    (define (lookup config s)
      (cdr (assq s config)))

    (define (extend-config config s val)
      (cond
       [(null? config) (error 'struct "internal error: can't find config element: ~s" s)]
       [(eq? (caar config) s) (cons (cons s val) (cdr config))]
       [else (cons (car config) (extend-config (cdr config) s val))]))

    (define insp-keys
      "#:inspector, #:transparent, or #:prefab")

    ;; Parse sequence of keyword-based struct specs
    (define (parse-props fm p super-id)
      (let loop ([p p]
                 [config '((#:super . #f)
                           (#:inspector . #f)
                           (#:auto-value . #f)
                           (#:props . ())
                           (#:mutable . #f)
                           (#:guard . #f)
                           (#:constructor-name . #f)
                           (#:reflection-name . #f)
                           (#:only-constructor? . #f)
                           (#:omit-define-values . #f)
                           (#:omit-define-syntaxes . #f))]
                 [nongen? #f])
        (cond
         [(null? p) config]
         [(eq? '#:super (syntax-e (car p)))
          (check-exprs 1 p #f)
          (when (lookup config '#:super)
            (bad "multiple" (car p) "s"))
          (when super-id
            (raise-syntax-error
             #f
             (string-append
              "#:super specification disallowed because a struct supertype id"
              " was supplied with the struct type id")
             stx
             (car p)))
          (loop (cddr p)
                (extend-config config '#:super (cadr p))
                nongen?)]
         [(memq (syntax-e (car p))
                '(#:guard #:auto-value))
          (let ([key (syntax-e (car p))])
            (check-exprs 1 p #f)
            (when (lookup config key)
              (bad "multiple" (car p) "s"))
            (when (and nongen?
                       (eq? key '#:guard))
              (bad "cannot provide" (car p) " for prefab structure type"))
            (loop (cddr p)
                  (extend-config config key (cadr p))
                  nongen?))]
         [(eq? '#:property (syntax-e (car p)))
          (check-exprs 2 p #f)
          (when nongen?
            (bad "cannot use" (car p) " for prefab structure type"))
          (loop (cdddr p)
                (extend-config config
                               '#:props
                               (cons (cons (cadr p) (caddr p))
                                     (lookup config '#:props)))
                nongen?)]
         [(eq? '#:inspector (syntax-e (car p)))
          (check-exprs 1 p #f)
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (loop (cddr p)
                (extend-config config '#:inspector 
                               #`(check-inspector '#,fm #,(cadr p)))
                nongen?)]
         [(eq? '#:transparent (syntax-e (car p)))
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (loop (cdr p)
                (extend-config config '#:inspector #'#f)
                nongen?)]
         [(or (eq? '#:constructor-name (syntax-e (car p)))
              (eq? '#:extra-constructor-name (syntax-e (car p))))
          (check-exprs 1 p "identifier")
          (when (lookup config '#:constructor-name)
            (bad "multiple" "#:constructor-name or #:extra-constructor-name keys" (car p)))
          (unless (identifier? (cadr p))
            (bad "need an identifier after" (car p) (cadr p)))
          (loop (cddr p)
                (extend-config (extend-config config '#:constructor-name (cadr p))
                               '#:only-constructor?
                               (eq? '#:constructor-name (syntax-e (car p))))
                nongen?)]
         [(eq? '#:reflection-name (syntax-e (car p)))
          (check-exprs 1 p "expression")
          (when (lookup config '#:reflection-name)
            (bad "multiple" "#:reflection-name keys" (car p)))
          (loop (cddr p)
                (extend-config config '#:reflection-name (cadr p))
                nongen?)]
         [(eq? '#:prefab (syntax-e (car p)))
          (when (lookup config '#:inspector)
            (bad "multiple" insp-keys "s" (car p)))
          (when (pair? (lookup config '#:props))
            (bad "cannot use" (car p) " for a structure type with properties"))
          (when (lookup config '#:guard)
            (bad "cannot use" (car p) " for a structure type with a guard"))
          (loop (cdr p)
                (extend-config config '#:inspector #''prefab)
                #t)]
         [(memq (syntax-e (car p))
                '(#:mutable #:omit-define-values #:omit-define-syntaxes))
          (let ([key (syntax-e (car p))])
            (when (lookup config key)
              (bad "redundant" (car p) ""))
            (loop (cdr p)
                  (extend-config config key #t)
                  nongen?))]
         [else
          (raise-syntax-error
           #f
           (if (keyword? (syntax-e (car p)))
               "unrecognized struct-specification keyword"
               "expected a struct-specification keyword")
           stx
           (car p))])))

    (define stx (syntax-case full-stx ()
                  [(_ stx . _) #'stx]))
    
    (syntax-case full-stx ()
      [(_ (fm . _) id (field ...) prop ...)
       (let-values ([(id super-id)
                     (if (identifier? #'id)
                         (values #'id #f)
                         (syntax-case #'id ()
                           [(id super-id) 
                            (and (identifier? #'id)
                                 (identifier? #'super-id))
                            (values #'id #'super-id)]
                           [else
                            (raise-syntax-error 
                             #f
                             "bad syntax; expected <id> for structure-type name or (<id> <id>) for name and supertype name"
                             stx
                             #'id)]))])
         (let-values ([(super-info super-info-checked?)
		       (if super-id
			   (let ([v (syntax-local-value super-id (lambda () #f))])
			     (if (struct-info? v)
				 (values (extract-struct-info v) (checked-struct-info-rec? v))
				 (raise-syntax-error
				  #f
				  (format "parent struct type not defined~a"
					  (if v
					      (format " (~a does not name struct type information)"
						      (syntax-e super-id))
					      ""))
				  stx
				  super-id)))
			   ;; if there's no super type, it's like it was checked
			   (values #f #t))])
           (when (and super-info
                      (not (car super-info)))
             (raise-syntax-error
              #f
              "no structure type descriptor available for supertype"
              stx
              super-id))
           (let* ([field-stxes (syntax->list #'(field ...))]
                  [fields (map parse-field field-stxes)]
                  [dup (check-duplicate-identifier (map field-id fields))])
             (when dup
               (raise-syntax-error
                #f
                "duplicate field identifier"
                stx
                dup))
             (let ([auto-count
                    (let loop ([fields fields] [field-stxes field-stxes] [auto? #f])
                      (cond
                       [(null? fields) 0]
                       [(field-auto? (car fields))
                        (+ 1 (loop (cdr fields) (cdr field-stxes) #t))]
                       [auto?
                        (raise-syntax-error
                         #f
                         "non-auto field after an auto field disallowed"
                         stx
                         (car field-stxes))]
                       [else
                        (loop (cdr fields) (cdr field-stxes) #f)]))])
               (let*-values ([(inspector super-expr props auto-val guard ctor-name ctor-only? 
                                         reflect-name-expr mutable?
                                         omit-define-values? omit-define-syntaxes?)
                              (let ([config (parse-props #'fm (syntax->list #'(prop ...)) super-id)])
                                (values (lookup config '#:inspector)
                                        (lookup config '#:super)
                                        (lookup config '#:props)
                                        (lookup config '#:auto-value)
                                        (lookup config '#:guard)
                                        (lookup config '#:constructor-name)
                                        (lookup config '#:only-constructor?)
                                        (lookup config '#:reflection-name)
                                        (lookup config '#:mutable)
                                        (lookup config '#:omit-define-values)
                                        (lookup config '#:omit-define-syntaxes)))]
                             [(self-ctor?)
                              (and ctor-name (bound-identifier=? id ctor-name))]
                             [(name-as-ctor?) (or self-ctor? (not ctor-only?))])
                 (when mutable?
                   (for-each (lambda (f f-stx)
                               (when (field-mutable? f)
                                 (raise-syntax-error
                                  #f
                                  "redundant #:mutable specification in field"
                                  stx
                                  f-stx)))
                             fields field-stxes))
                 (let ([struct: (build-name id "struct:" id)]
                       [make- (if ctor-name
                                  (if self-ctor?
                                      (car (generate-temporaries (list id)))
                                      ctor-name)
                                  (build-name id "make-" id))]
                       [? (build-name id id "?")]
                       [sels (map (lambda (f)
                                    (build-name id ; (field-id f) 
                                                id "-" (field-id f)))
                                  fields)]
                       [sets (let loop ([fields fields])
                               (cond
                                [(null? fields) null]
                                [(not (or mutable? (field-mutable? (car fields))))
                                 (loop (cdr fields))]
                                [else
                                 (cons (build-name id ; (field-id (car fields))
                                                   "set-"
                                                   id
                                                   "-"
                                                   (field-id (car fields))
                                                   "!")
                                       (loop (cdr fields)))]))]
                       [super-struct: (if super-info
                                          (or (car super-info)
                                              (raise-syntax-error
                                               #f
                                               "no structure type descriptor available for supertype"
                                               stx
                                               super-id))
                                          (and super-expr
                                               #`(check-struct-type 'fm #,super-expr)))]
                       [prune (lambda (stx) (identifier-prune-lexical-context stx
                                                                              (list (syntax-e stx) '#%top)))]
                       [reflect-name-expr (if reflect-name-expr
                                              (quasisyntax (check-reflection-name 'fm #,reflect-name-expr))
                                              (quasisyntax '#,id))])
                   (let ([run-time-defns
                          (lambda ()
                            (quasisyntax/loc stx
                              (define-values (#,struct: #,make- #,? #,@sels #,@sets)
                                (let-values ([(struct: make- ? -ref -set!)
                                              (syntax-parameterize ([struct-field-index
                                                                     (lambda (stx)
                                                                       (syntax-case stx #,(map field-id fields)
                                                                         #,@(let loop ([fields fields][pos 0])
                                                                              (cond
                                                                               [(null? fields) null]
                                                                               [else (cons #`[(_ #,(field-id (car fields))) #'#,pos]
                                                                                           (loop (cdr fields) (add1 pos)))]))
                                                                         [(_ name) (raise-syntax-error #f "no such field" stx #'name)]))])
                                                (make-struct-type #,reflect-name-expr
                                                                  #,super-struct:
                                                                  #,(- (length fields) auto-count)
                                                                  #,auto-count
                                                                  #,auto-val
                                                                  #,(if (null? props)
                                                                        #'null
                                                                        #`(list #,@(map (lambda (p)
                                                                                          #`(cons #,(car p) #,(cdr p)))
                                                                                        props)))
                                                                  #,(or inspector
                                                                        #`(current-inspector))
                                                                  #f
                                                                  '#,(let loop ([i 0]
                                                                                [fields fields])
                                                                       (cond
                                                                        [(null? fields) null]
                                                                        [(field-auto? (car fields)) null]
                                                                        [(not (or mutable? (field-mutable? (car fields))))
                                                                         (cons i (loop (add1 i) (cdr fields)))]
                                                                        [else (loop (add1 i) (cdr fields))]))
                                                                  #,guard
                                                                  '#,(if ctor-only? ctor-name id)))])
                                  (values struct: make- ?
                                          #,@(let loop ([i 0][fields fields])
                                               (if (null? fields)
                                                   null
                                                   (cons #`(make-struct-field-accessor -ref #,i '#,(field-id (car fields)))
                                                         (loop (add1 i) (cdr fields)))))
                                          #,@(let loop ([i 0][fields fields])
                                               (if (null? fields)
                                                   null
                                                   (if (not (or mutable? (field-mutable? (car fields))))
                                                       (loop (add1 i) (cdr fields))
                                                       (cons #`(make-struct-field-mutator -set! #,i '#,(field-id (car fields)))
                                                             (loop (add1 i) (cdr fields)))))))))))]
                         [compile-time-defns
                          (lambda ()
                            (let ([protect (lambda (sel)
                                             (and sel
                                                  (if (syntax-e sel)
                                                      #`(quote-syntax #,(prune sel))
                                                      sel)))]
				  [mk-info (if super-info-checked?
                                               (if name-as-ctor?
                                                   #'make-self-ctor-checked-struct-info
                                                   #'make-checked-struct-info)
                                               (if name-as-ctor?
                                                   #'make-self-ctor-struct-info
                                                   #'make-struct-info))])
                              (quasisyntax/loc stx
                                (define-syntaxes (#,id)
                                  (#,mk-info
                                   (lambda ()
                                     (list
                                      (quote-syntax #,(prune struct:))
                                      (quote-syntax #,(prune (if (and ctor-name self-ctor?)
                                                                 id
                                                                 make-)))
                                      (quote-syntax #,(prune ?))
                                      (list
                                       #,@(map protect (reverse sels))
                                       #,@(if super-info
                                              (map protect (list-ref super-info 3))
                                              (if super-expr
                                                  '(#f)
                                                  null)))
                                      (list
                                       #,@(reverse
                                           (let loop ([fields fields][sets sets])
                                             (cond
                                              [(null? fields) null]
                                              [(not (or mutable? (field-mutable? (car fields))))
                                               (cons #f (loop (cdr fields) sets))]
                                              [else
                                               (cons (protect (car sets))
                                                     (loop (cdr fields) (cdr sets)))])))
                                       #,@(if super-info
                                              (map protect (list-ref super-info 4))
                                              (if super-expr
                                                  '(#f)
                                                  null)))
                                      #,(if super-id
                                            (protect super-id)
                                            (if super-expr
                                                #f
                                                #t))))
                                   #,@(if name-as-ctor?
                                          (list #`(lambda () (quote-syntax #,make-)))
                                          null))))))])
                     (let ([result
                            (cond
                             [(and (not omit-define-values?) (not omit-define-syntaxes?))
                              #`(begin #,(run-time-defns) #,(compile-time-defns))]
                             [omit-define-syntaxes?
                              (run-time-defns)]
                             [omit-define-values?
                              (compile-time-defns)]
                             [else #'(begin)])])
                       (if super-id
                           (syntax-property result 
                                            'disappeared-use 
                                            (syntax-local-introduce super-id))
                           result)))))))))]
      [(_ _ id . _)
       (not (or (identifier? #'id)
                (and (syntax->list #'id)
                     (= 2 (length (syntax->list #'id)))
                     (andmap identifier? (syntax->list #'id)))))
       (raise-syntax-error
        #f
        "bad syntax; expected <id> for structure-type name or (<id> <id>) for name and supertype name"
        stx
        #'id)]
      [(_ _ id (field ...) . _)
       (begin
         (for-each parse-field (syntax->list #'(field ...)))
         (raise-syntax-error
          #f
          "bad syntax after field sequence"
          stx))]
      [(_ _ id fields . _)
       (raise-syntax-error
        #f
        "bad syntax; expected a parenthesized sequence of field descriptions"
        stx
        #'fields)]
      [(_ _ id)
       (raise-syntax-error
        #f
        "bad syntax; missing fields"
        stx)]
      [_
       (raise-syntax-error
        #f
        "bad syntax"
        stx)]))

  (define-syntax (struct-copy stx)
    (if (not (eq? (syntax-local-context) 'expression))
        (quasisyntax/loc stx (#%expression #,stx))
        (syntax-case stx ()
          [(form-name info struct-expr field+val ...)
           (let ([ans (syntax->list  #'(field+val ...))])
             ;; Check syntax:
             (unless (identifier? #'info)
               (raise-syntax-error #f "not an identifier for structure type" stx #'info))
             (for-each (lambda (an)
                         (syntax-case an ()
                           [(field val)
                            (unless (identifier? #'field)
                              (raise-syntax-error #f 
                                                  "not an identifier for field name" 
                                                  stx
                                                  #'field))]
                           [_
                            (raise-syntax-error #f
                                                "expected a field update of the form (<field-id> <expr>)"
                                                stx
                                                an)]))
                       ans)

             (let ([new-fields
                    (map (lambda (an)
                           (syntax-case an ()
                             [(field expr)
                              (list (datum->syntax #'field
                                                   (string->symbol
                                                    (format "~a-~a"
                                                            (syntax-e #'info)
                                                            (syntax-e #'field)))
                                                   #'field)
                                    #'expr
                                    (car (generate-temporaries (list #'field))))]))
                         ans)])

               ;; new-binding-for : syntax[field-name] -> (union syntax[expression] #f)
               (let ([new-binding-for 
                      (lambda (f)
                        (ormap (lambda (new-field) 
                                 (and (free-identifier=? (car new-field) f)
                                      (caddr new-field)))
                               new-fields))])
                 
                 (let-values ([(construct pred accessors)
                               (let ([v (syntax-local-value #'info (lambda () #f))])
                                 (unless (struct-info? v)
                                   (raise-syntax-error #f "identifier is not bound to a structure type" stx #'info))
                                 (let ([v (extract-struct-info v)])
                                   (values (cadr v)
                                           (caddr v)
                                           (cadddr v))))])
                   (unless construct
                     (raise-syntax-error #f
                                         "constructor not statically known for structure type"
                                         stx
                                         #'info))
                   (unless pred
                     (raise-syntax-error #f
                                         "predicate not statically known for structure type"
                                         stx
                                         #'info))
                   (unless (andmap values accessors)
                     (raise-syntax-error #f
                                         "not all accessors are statically known for structure type"
                                         stx
                                         #'info))
                   (let ([dests
                          (map (lambda (new-field)
                                 (or (ormap (lambda (f2) 
                                              (and f2 
                                                   (free-identifier=? (car new-field) f2) 
                                                   f2))
                                            accessors)
                                     (raise-syntax-error #f 
                                                         "accessor name not associated with the given structure type" 
                                                         stx
                                                         (car new-field))))
                               new-fields)])
                     ;; Check for duplicates using dests, not as, because mod=? as might not be id=?
                     (let ((dupe (check-duplicate-identifier dests)))
                       (when dupe 
                         (raise-syntax-error #f 
                                             "duplicate field assignment" 
                                             stx 
                                             ;; Map back to an original field:
                                             (ormap (lambda (nf)
                                                      (and nf
                                                           (free-identifier=? dupe (car nf))
                                                           (car nf)))
                                                    (reverse new-fields)))))
                 
                     ;; the actual result
                     #`(let ((the-struct struct-expr))
                         (if (#,pred the-struct)
                             (let #,(map (lambda (new-field)
                                           #`[#,(caddr new-field) #,(cadr new-field)])
                                         new-fields)
                               (#,construct
                                #,@(map 
                                    (lambda (field) (or (new-binding-for field) 
                                                        #`(#,field the-struct)))
                                    (reverse accessors))))
                             (raise-type-error 'form-name 
                                               #,(format "~a" (syntax-e #'info))
                                               the-struct))))))))]))))
