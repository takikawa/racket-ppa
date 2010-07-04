#lang scheme/base
(require "../utils/utils.ss")

(require mzlib/struct 
         mzlib/plt-match
         syntax/boundmap
         "free-variance.ss"
         "interning.ss"
         mzlib/etc
         scheme/contract         
         (for-syntax 
          scheme/list
          stxclass/util
          scheme/match
          stxclass
          scheme/base
          syntax/struct
          syntax/stx
          scheme/contract
          (utils utils)))

(provide == defintern hash-id (for-syntax fold-target))

(define-for-syntax fold-target #'fold-target)

(define-for-syntax (mk par ht-stx key?)
  (define-syntax-class opt-cnt-id
    #:attributes (i cnt)
    (pattern i:id
             #:with cnt #'any/c)
    (pattern [i:id cnt]))
  (define-syntax-class no-provide-kw
    (pattern #:no-provide))
  (define-syntax-class idlist
    #:attributes ((i 1) (cnt 1) fs)
    (pattern (oci:opt-cnt-id ...)               
             #:with (i ...) #'(oci.i ...)
             #:with (cnt ...) #'(oci.cnt ...)
             #:with fs #'(i ...)))
  (define (combiner f flds)
    (syntax-parse flds
      [() #'empty-hash-table]
      [(e) #`(#,f e)]
      [(e ...) #`(combine-frees (list (#,f e) ...))]))
  (define-syntax-class frees-pat
    #:transparent
    #:attributes (f1 f2 def)
    (pattern (f1:expr f2:expr)
             #:with def #'(begin))
    (pattern (#f)
             #:with f1 #'empty-hash-table
             #:with f2 #'empty-hash-table
             #:with def #'(begin))
    (pattern (e:expr)
             #:with id (generate-temporary)
             #:with def #'(define id e)
             #:with f1 #'(id free-vars*)
             #:with f2 #'(id free-idxs*)))
  (define-syntax-class fold-pat
    #:transparent
    #:attributes (e)
    (pattern #:base
             #:with e fold-target)
    (pattern ex:expr
             #:with e #'#'ex))
  (lambda (stx)          
    (syntax-parse stx 
      [(dform nm:id flds:idlist (~or [[#:key key-expr:expr]] #:opt 
                                     [[#:intern intern?:expr]] #:opt
                                     [[#:frees . frees:frees-pat]] #:opt
                                     [[#:fold-rhs fold-rhs:fold-pat]] #:opt
                                     [[#:contract cnt:expr]] #:opt
                                     [no-provide?:no-provide-kw] #:opt) ...)
       (with-syntax* 
        ([ex (mk-id #'nm #'nm ":")]
         [fold-name (mk-id #f #'nm "-fold")]
         [kw-stx (string->keyword (symbol->string #'nm.datum))]
         [parent par]
         [(s:ty maker pred acc ...) (build-struct-names #'nm (syntax->list #'flds.fs) #f #t #'nm)]
         [*maker (mk-id #'nm "*" #'nm)]
         [**maker (mk-id #'nm "**" #'nm)]
         [*maker-cnt (if enable-contracts?
                         (or #'cnt #'(flds.cnt ... . -> . pred))
                         #'any/c)]
         [ht-stx ht-stx]
         [bfs-fold-rhs (cond [#'fold-rhs #`(procedure-rename
                                            (lambda () #,#'fold-rhs.e)
                                            'fold-name)]
                             ;; otherwise we assume that everything is a type, 
                             ;; and recur on all the arguments
                             [else #'(procedure-rename
                                      (lambda () 
                                        #`(*maker (#,type-rec-id flds.i) ...))
                                      'fold-name)])]
         [provides (if #'no-provide?
                       #'(begin)                                 
                       #`(begin 
                           (provide #;nm ex pred acc ...)
                           (p/c (rename *maker maker *maker-cnt))))]
         [intern 
          (let ([mk (lambda (int) 
		      (if key? 
			  #`(defintern (**maker . flds.fs) maker #,int #:extra-arg key-expr)
			  #`(defintern (**maker . flds.fs) maker #,int)))])
         (syntax-parse #'flds.fs
              [_  #:when #'intern?
                 (mk #'intern?)]
              [() (mk #'#f)]
              [(f) (mk #'f)]
              [_ (mk #'(list . flds.fs))]))]
         [(ign-pats ...) (if key? #'(_ _) #'(_))]
         [frees-def (if #'frees #'frees.def #'(begin))]
         [frees 
          (with-syntax ([(f1 f2) (if #'frees
                                     #'(frees.f1 frees.f2)
                                     (list (combiner #'free-vars* #'flds.fs)
                                           (combiner #'free-idxs* #'flds.fs)))])
            (quasisyntax/loc stx
		(w/c nm ([*maker *maker-cnt])
                   (define (*maker . flds.fs)
                     (define v (**maker . flds.fs))
                     frees-def
                     (unless-in-table 
                      var-table v
                      (define fvs f1)
                      (define fis f2)
                      (hash-set! var-table v fvs)
                      (hash-set! index-table v fis))
                     v))))])
        #`(begin
            (define-struct (nm parent) flds.fs #:inspector #f)
            (define-match-expander ex
              (lambda (s)
                (syntax-parse s 
                  [(_ . fs) 
                   #:with pat (syntax/loc s (ign-pats ... . fs))
                   (syntax/loc s (struct nm pat))])))
            (begin-for-syntax
              (hash-set! ht-stx 'kw-stx (list #'ex #'flds.fs bfs-fold-rhs #'#,stx)))
            intern
            provides
            frees))])))

(define-for-syntax (mk-fold ht type-rec-id rec-ids kws)
  (lambda (stx)
    (define new-ht (hash-copy ht))
    (define (mk-matcher kw) 
      (datum->syntax stx (string->symbol (string-append (keyword->string kw) ":"))))
    (define/contract (put k lst)
      (keyword? (list/c syntax?
                        syntax?
                        (-> syntax?)
                        syntax?) 
                . -> . void?)
      (hash-set! new-ht k lst))
    (define (add-clause cl)
      (syntax-parse cl
        [(kw:keyword #:matcher mtch pats ... expr)
         (put (syntax-e #'kw) (list #'mtch 
                                    (syntax/loc cl (pats ...))
                                    (lambda () #'expr)
                                    cl))]
        [(kw:keyword pats ... expr) 
         (put (syntax-e #'kw) (list (mk-matcher (syntax-e #'kw)) 
                                    (syntax/loc cl (pats ...))
                                    (lambda () #'expr)
                                    cl))]))
    (define-syntax-class clause
      (pattern  
       (k:keyword #:matcher mtch pats ... e:expr)
       #:with kw #'k.datum
       #:with val (list #'mtch 
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax))
      (pattern
       (k:keyword pats ... e:expr) 
       #:with kw (syntax-e #'k)
       #:with val (list (mk-matcher #'kw) 
                        (syntax/loc this-syntax (pats ...))
                        (lambda () #'e)
                        this-syntax)))
    (define (gen-clause k v)
      (match v
        [(list match-ex pats body-f src)
         (let ([pat (quasisyntax/loc src (#,match-ex  . #,pats))])
           (quasisyntax/loc src (#,pat #,(body-f))))]))
    (define-syntax-class (keyword-in kws)
      #:attributes (datum)
      (pattern k:keyword
               #:when (memq #'k.datum kws)
               #:with datum #'k.datum))
    (define-syntax-class (sized-list kws)
      #:description (format "keyword expr pairs matching with keywords in the list ~a" kws)
      (pattern ((~or [k e:expr]) ...)
               #:declare k (keyword-in kws)
               #:when (equal? (length (attribute k.datum)) (length (remove-duplicates (attribute k.datum))))
               #:with mapping (for/hash ([k* (attribute k.datum)]
                                         [e* (attribute e)])
                                (values k* e*))
               ))
    (syntax-parse stx
      [(tc recs ty clauses:clause ...)
       #:declare recs (sized-list kws)
       (begin 
         (for ([k (attribute clauses.kw)]
               [v (attribute clauses.val)])
           (put k v))
         (with-syntax ([(let-clauses ...)
                        (for/list ([rec-id rec-ids]
                                   [k kws])
                          #`[#,rec-id #,(hash-ref (attribute recs.mapping) k
                                                  #'values)])])
           #`(let (let-clauses ...
                   [#,fold-target ty])
               ;; then generate the fold
               #,(quasisyntax/loc stx
                   (match #,fold-target
                     #,@(hash-map new-ht gen-clause))))))])))


(define-syntax (make-prim-type stx)
  (define default-flds #'(seq))
  (define-syntax-class type-name-base
    #:attributes (i lower-s first-letter key? (fld-names 1))
    #:transparent
    (pattern i:id
             #:with lower-s (string-downcase (symbol->string #'i.datum))
             #:with (fld-names ...) default-flds
	     #:with key? #'#f
             #:with first-letter (string-ref #'lower-s 0))
    (pattern [i:id #:d d-name:id]
             #:with (fld-names ...) default-flds
             #:with lower-s (string-downcase (symbol->string #'i.datum))
	     #:with key? #'#f
             #:with first-letter (symbol->string #'d-name.datum))
    (pattern [i:id #:key]
             #:with (fld-names ...) (datum->syntax #f (append (syntax->list default-flds) 
                                                              (syntax->list #'(key))))
             #:with lower-s (string-downcase (symbol->string #'i.datum))
	     #:with key? #'#t
             #:with first-letter (string-ref #'lower-s 0)))
  (define-syntax-class type-name
    #:transparent
    (pattern :type-name-base
             #:with name #'i
             #:with keyword (string->keyword (symbol->string (syntax-e #'i)))
             #:with tmp-rec-id (generate-temporary)
             #:with case (mk-id #'i #'lower-s "-case")
             #:with printer (mk-id #'i "print-" #'lower-s "*")
             #:with ht (mk-id #'i #'lower-s "-name-ht")
             #:with rec-id (mk-id #'i #'lower-s "-rec-id")
             #:with d-id (mk-id #'i "d" #'first-letter)
             #:with (_ _ pred? accs ...) 
                    (datum->syntax #f (build-struct-names #'name (syntax->list #'(fld-names ...)) #f #t #'name))))
  (syntax-parse stx
    [(_ i:type-name ...)
     (with-syntax* ([(fresh-ids ...) (generate-temporaries #'(i.name ...))]
                    [(default-ids ...) (generate-temporaries #'(i.name ...))]
                    [fresh-ids-list #'(fresh-ids ...)]
                    [(anys ...) (for/list ([i (syntax->list #'fresh-ids-list)]) #'any/c)])
       #'(begin
           (provide i.d-id ... i.printer ... i.name ... i.pred? ... i.accs ... ...
                    (for-syntax i.ht ... i.rec-id ...))
           (define-syntax i.d-id (mk #'i.name #'i.ht i.key?)) ...
           (define-for-syntax i.ht (make-hasheq)) ...
           (define-struct/printer i.name (i.fld-names ...) (lambda (a b c) ((unbox i.printer) a b c))) ...
           (define-for-syntax i.rec-id #'i.rec-id) ...   
           (provide i.case ...)           
           (define-syntaxes (i.case ...)
             (let ()               
               (apply values
                      (map (lambda (ht) 
                             (mk-fold ht 
                                      (car (list #'i.rec-id ...))
                                      (list #'i.rec-id ...)
                                      '(i.keyword ...)))
                           (list i.ht ...)))))))]))

(make-prim-type [Type #:key]
                Filter
                [LatentFilter #:d lf]
                Object 
                [LatentObject #:d lo]
                [PathElem #:d pe])
