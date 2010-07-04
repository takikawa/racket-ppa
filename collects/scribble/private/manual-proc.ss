#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../config.ss"
         "../basic.ss"
         "../manual-struct.ss"
         "qsloc.ss"
         "manual-utils.ss"
         "manual-vars.ss"
         "manual-style.ss"
         "manual-scheme.ss"
         "manual-bind.ss"
         "manual-method.ss"
         "manual-ex.ss"
         scheme/string
         scheme/list
         (for-syntax scheme/base)
         (for-label scheme/base
                    scheme/class))

(provide defproc defproc* defstruct
         defparam defparam* defboolparam
         defthing defthing* defthing/proc
         ;; private:
         *defthing)

(define dots0
  (make-element "schememeta" (list "...")))
(define dots1
  (make-element "schememeta" (list "...+")))

(define (make-openers n)
  (schemeparenfont
   (case n [(1) "("] [(0) ""] [(2) "(("] [else (make-string n #\()])))
(define (make-closers n)
  (schemeparenfont
   (case n [(1) ")"] [(0) ""] [(2) "))"] [else (make-string n #\()])))

(define-syntax (arg-contract stx)
  (syntax-case stx (... ...+ _...superclass-args...)
    [(_ [id contract])
     (identifier? #'id)
     #'(schemeblock0 contract)]
    [(_ [id contract val])
     (identifier? #'id)
     #'(schemeblock0 contract)]
    [(_ [kw id contract])
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(schemeblock0 contract)]
    [(_ [kw id contract val])
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(schemeblock0 contract)]
    [(_ (... ...)) #'#f]
    [(_ (... ...+)) #'#f]
    [(_ _...superclass-args...) #'#f]
    [(_ arg) (raise-syntax-error 'defproc "bad argument form" #'arg)]))

(define-syntax (arg-default stx)
  (syntax-case stx (... ...+ _...superclass-args...)
    [(_ [id contract])
     (identifier? #'id)
     #'#f]
    [(_ [id contract val])
     (identifier? #'id)
     #'(schemeblock0 val)]
    [(_ [kw id contract])
     (keyword? (syntax-e #'kw))
     #'#f]
    [(_ [kw id contract val])
     (keyword? (syntax-e #'kw))
     #'(schemeblock0 val)]
    [_ #'#f]))

(define-syntax (extract-proc-id stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     #`(quote-syntax/loc id)]
    [(_ (proto arg ...))
     #'(extract-proc-id proto)]
    [(_ thing) (raise-syntax-error 'defproc "bad prototype" #'thing)]))

(define-syntax (arg-contracts stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-contract arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-contracts proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (arg-defaults stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-default arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-defaults proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (result-contract stx)
  (syntax-case stx (values)
    [(_ (values c ...))
     #'(list (schemeblock0 c) ...)]
    [(_ c)
     (if (string? (syntax-e #'c))
       (raise-syntax-error 'defproc
                           "expected a result contract, found a string" #'c)
       #'(schemeblock0 c))]))

(define-syntax-rule (defproc (id arg ...) result desc ...)
  (defproc* [[(id arg ...) result]] desc ...))

(define-syntax defproc*
  (syntax-rules ()
    [(_ [[proto result] ...] desc ...)
     (defproc* #:mode procedure #:within #f [[proto result] ...] desc ...)]
    [(_ #:mode m #:within cl [[proto result] ...] desc ...)
     (with-togetherable-scheme-variables
      ()
      ([proc proto] ...)
      (*defproc 'm (quote-syntax/loc cl)
                (list (extract-proc-id proto) ...)
                '[proto ...]
                (list (arg-contracts proto) ...)
                (list (arg-defaults proto) ...)
                (list (lambda () (result-contract result)) ...)
                (lambda () (list desc ...))))]))

(define-struct arg
  (special? kw id optional? starts-optional? ends-optional? num-closers))

(define (*defproc mode within-id
                  stx-ids prototypes arg-contractss arg-valss result-contracts
                  content-thunk)
  (define ((arg->elem show-opt-start?) arg)
    (let* ([e (cond [(not (arg-special? arg))
                     (if (arg-kw arg)
                       (if (eq? mode 'new)
                         (make-element
                          #f (list (schemeparenfont "[")
                                   (schemeidfont (keyword->string (arg-kw arg)))
                                   spacer
                                   (to-element (make-var-id (arg-id arg)))
                                   (schemeparenfont "]")))
                         (make-element
                          #f (list (to-element (arg-kw arg))
                                   spacer
                                   (to-element (make-var-id (arg-id arg))))))
                       (to-element (make-var-id (arg-id arg))))]
                    [(eq? (arg-id arg) '...+) dots1]
                    [(eq? (arg-id arg) '...) dots0]
                    [(eq? (arg-id arg) '_...superclass-args...) (to-element (arg-id arg))]
                    [else (to-element (make-var-id (arg-id arg)))])]
           [e (if (arg-ends-optional? arg)
                (make-element #f (list e "]"))
                e)]
           [e (if (zero? (arg-num-closers arg))
                e
                (make-element
                 #f (list e (make-closers (arg-num-closers arg)))))])
      (if (and show-opt-start? (arg-starts-optional? arg))
        (make-element #f (list "[" e))
        e)))
  (define (prototype-depth p)
    (let loop ([p (car p)])
      (if (symbol? p) 0 (+ 1 (loop (car p))))))
  (define (prototype-args p)
    (define (parse-arg v in-optional? depth next-optional? next-special-dots?)
      (let* ([id (if (pair? v) ((if (keyword? (car v)) cadr car) v) v)]
             [kw (and (pair? v) (keyword? (car v)) (car v))]
             [default? (and (pair? v) (pair? ((if kw cdddr cddr) v)))])
        (make-arg (symbol? v) kw id default?
                  (and default? (not in-optional?))
                  (or (and (not default?)
                           in-optional?) ; => must be special
                      (and default?
                           (not next-optional?)
                           (not next-special-dots?)))
                  depth)))
    (let loop ([p p] [last-depth 0])
      (append
       (if (symbol? (car p))
         null
         (loop (car p) (add1 last-depth)))
       (let loop ([p (cdr p)][in-optional? #f])
         (cond
           [(null? p) null]
           [(null? (cdr p))
            (list (parse-arg (car p) in-optional? last-depth #f #f))]
           [else
            (let ([a (parse-arg
                      (car p)
                      in-optional?
                      0
                      (let ([v (cadr p)])
                        (and (pair? v)
                             (not
                              (null? ((if (keyword? (car v)) cdddr cddr) v)))))
                      (and (not (pair? (cadr p)))
                           (not (eq? '_...superclass-args... (cadr p)))))])
              (cons a (loop (cdr p)
                            (and (arg-optional? a)
                                 (not (arg-ends-optional? a))))))])))))
  (define (prototype-size args first-combine next-combine special-combine?)
    (let loop ([s args] [combine first-combine])
      (if (null? s)
        0
        (combine
         (loop (cdr s) next-combine)
         (let ([a (car s)])
           (+ (arg-num-closers a)
              (if (arg-special? a)
                (string-length (symbol->string (arg-id a)))
                (+ (if (arg-kw a)
                     (+ (if (eq? mode 'new) 2 0)
                        (string-length (keyword->string (arg-kw a)))
                        3
                        (string-length (symbol->string (arg-id a))))
                     (string-length (symbol->string (arg-id a))))
                   (if (and special-combine?
                            (pair? (cdr s))
                            (arg-special? (cadr s))
                            (not (eq? '_...superclass-args...
                                      (arg-id (cadr s)))))
                     (+ 1 (string-length (symbol->string (arg-id (cadr s)))))
                     0)))))))))
  (define (extract-id p)
    (let loop ([p p])
      (if (symbol? (car p)) (car p) (loop (car p)))))
  (define (do-one stx-id prototype args arg-contracts arg-vals result-contract
                  first?)
    (define tagged
      (cond
        [(eq? mode 'new)
         (make-element #f (list (scheme new) spacer (to-element within-id)))]
        [(eq? mode 'make)
         (make-element
          #f (list (scheme make-object) spacer (to-element within-id)))]
        [(eq? mode 'send)
         (make-element
          #f
          (list (scheme send) spacer
                (name-this-object (syntax-e within-id)) spacer
                (if first?
                  (let* ([mname (extract-id prototype)]
                         [target-maker (id-to-target-maker within-id #f)]
                         [content (list (*method mname within-id))])
                    (if target-maker
                      (target-maker
                       content
                       (lambda (ctag)
                         (let ([tag (method-tag ctag mname)])
                           (make-toc-target-element
                            #f
                            (list (make-index-element
                                   #f
                                   content
                                   tag
                                   (list (symbol->string mname))
                                   content
                                   (with-exporting-libraries
                                    (lambda (libs)
                                      (make-method-index-desc
                                       (syntax-e within-id)
                                       libs mname ctag)))))
                            tag))))
                      (car content)))
                  (*method (extract-id prototype) within-id))))]
        [first?
         (let ([target-maker (id-to-target-maker stx-id #t)]
               [content (list (definition-site (extract-id prototype)
                                               stx-id #f))])
           (if target-maker
             (target-maker
              content
              (lambda (tag)
                (make-toc-target-element
                 #f
                 (list (make-index-element
                        #f content tag
                        (list (symbol->string (extract-id prototype)))
                        content
                        (with-exporting-libraries
                         (lambda (libs)
                           (make-procedure-index-desc (extract-id prototype)
                                                      libs)))))
                 tag)))
             (car content)))]
        [else
         (annote-exporting-library
          (let ([sig (current-signature)])
            (if sig
              (*sig-elem (sig-id sig) (extract-id prototype))
              (to-element (make-just-context (extract-id prototype)
                                             stx-id)))))]))
    (define p-depth (prototype-depth prototype))
    (define flat-size (+ (prototype-size args + + #f)
                         p-depth
                         (element-width tagged)))
    (define short? (or (flat-size . < . 40) ((length args) . < . 2)))
    (define res
      (let ([res (result-contract)])
        (if (list? res)
          ;; multiple results
          (if (null? res)
            'nbsp
            (let ([w (apply + (map block-width res))])
              (if (or (ormap table? res) (w . > . 40))
                (make-table
                 #f (map (lambda (fe) (list (make-flow (list fe)))) res))
                (make-table
                 #f
                 (list (let loop ([res res])
                         (if (null? (cdr res))
                           (list (make-flow (list (car res))))
                           (list* (make-flow (list (car res)))
                                  flow-spacer
                                  (loop (cdr res))))))))))
          res)))
    (define tagged+arg-width (+ (prototype-size args max max #t)
                                p-depth
                                (element-width tagged)))
    (define result-next-line?
      ((+ (if short? flat-size tagged+arg-width) (block-width res))
       . >= . (- max-proto-width 7)))
    (define end (list flow-spacer (to-flow 'rarr)
                      flow-spacer (make-flow (list res))))
    (append
     (list
      (list
       (make-flow
        (if short?
          ;; The single-line case:
          (make-table-if-necessary
           "prototype"
           (list
            (cons
             (to-flow
              (make-element
               #f
               `(,(make-openers (add1 p-depth))
                 ,tagged
                 ,@(if (null? args)
                     (list (make-closers p-depth))
                     (append-map (lambda (arg)
                                   (list spacer ((arg->elem #t) arg)))
                                 args))
                 ,(schemeparenfont ")"))))
             (if result-next-line? null end))))
          ;; The multi-line case:
          (let ([not-end (if result-next-line?
                           (list flow-spacer)
                           (list flow-spacer flow-spacer
                                 flow-spacer flow-spacer))]
                [one-ok? (tagged+arg-width . < . 60)])
            (list
             (make-table
              "prototype"
              (cons
               (cons
                (to-flow
                 (make-element
                  #f
                  (list
                   (make-openers (add1 p-depth))
                   tagged)))
                (if one-ok?
                  (list*
                   (if (arg-starts-optional? (car args))
                     (to-flow (make-element #f (list spacer "[")))
                     flow-spacer)
                   (to-flow ((arg->elem #f) (car args)))
                   not-end)
                  (list* 'cont 'cont not-end)))
               (let loop ([args (if one-ok? (cdr args) args)])
                 (if (null? args)
                   null
                   (let ([dots-next?
                          (or (and (pair? (cdr args))
                                   (arg-special? (cadr args))
                                   (not (eq? '_...superclass-args...
                                             (arg-id (cadr args))))))])
                     (cons
                      (list*
                       flow-spacer
                       (if (arg-starts-optional? (car args))
                         (to-flow (make-element #f (list spacer "[")))
                         flow-spacer)
                       (let ([a ((arg->elem #f) (car args))]
                             [next (if dots-next?
                                     (make-element
                                      #f (list spacer
                                               ((arg->elem #f)
                                                (cadr args))))
                                     "")])
                         (to-flow
                          (cond
                            [(null? ((if dots-next? cddr cdr) args))
                             (make-element
                              #f
                              (list a next (schemeparenfont ")")))]
                            [(equal? next "") a]
                            [else
                             (make-element #f (list a next))])))
                       (if (and (null? ((if dots-next? cddr cdr) args))
                                (not result-next-line?))
                         end
                         not-end))
                      (loop ((if dots-next? cddr cdr)
                             args))))))))))))))
     (if result-next-line?
       (list (list (make-flow (make-table-if-necessary "prototype"
                                                       (list end)))))
       null)
     (append-map
      (lambda (arg arg-contract arg-val)
        (cond
          [(not (arg-special? arg))
           (let* ([arg-cont (arg-contract)]
                  [base-len (+ 5 (string-length (symbol->string (arg-id arg)))
                               (block-width arg-cont))]
                  [arg-val (and arg-val (arg-val))]
                  [def-len (if (arg-optional? arg) (block-width arg-val) 0)]
                  [base-list
                   (list (to-flow (hspace 2))
                         (to-flow (to-element (make-var-id (arg-id arg))))
                         flow-spacer
                         (to-flow ":")
                         flow-spacer
                         (make-flow (list arg-cont)))])
             (list
              (list
               (make-flow
                (if (and (arg-optional? arg)
                         ((+ base-len 3 def-len) . >= . max-proto-width))
                  (list
                   (make-table
                    "argcontract"
                    (list base-list (list flow-spacer flow-spacer flow-spacer
                                          (to-flow "=") flow-spacer
                                          (make-flow (list arg-val))))))
                  (make-table-if-necessary
                   "argcontract"
                   (list
                    (append
                     base-list
                     (if (and (arg-optional? arg)
                              ((+ base-len 3 def-len) . < . max-proto-width))
                       (list flow-spacer (to-flow "=") flow-spacer
                             (make-flow (list arg-val)))
                       null)))))))))]
          [else null]))
      args
      arg-contracts
      arg-vals)))
  (define all-args (map prototype-args prototypes))
  (define var-list
    (filter-map (lambda (a) (and (not (arg-special? a)) (arg-id a)))
                (append* all-args)))
  (make-box-splice
   (cons
    (make-table
     'boxed
     (append-map
      do-one
      stx-ids prototypes all-args arg-contractss arg-valss result-contracts
      (let loop ([ps prototypes] [accum null])
        (cond [(null? ps) null]
              [(ormap (lambda (a) (eq? (extract-id (car ps)) a)) accum)
               (cons #f (loop (cdr ps) accum))]
              [else (cons #t (loop (cdr ps)
                                   (cons (extract-id (car ps)) accum)))]))))
    (content-thunk))))

(define-syntax-rule (defparam id arg contract desc ...)
  (defproc* ([(id) contract] [(id [arg contract]) void?]) desc ...))
(define-syntax-rule (defparam* id arg in-contract out-contract desc ...)
  (defproc* ([(id) out-contract] [(id [arg in-contract]) void?]) desc ...))
(define-syntax-rule (defboolparam id arg desc ...)
  (defproc* ([(id) boolean?] [(id [arg any/c]) void?]) desc ...))

;; ----------------------------------------

(define-syntax defstruct
  (syntax-rules ()
    [(_ name fields #:mutable #:inspector #f desc ...)
     (**defstruct name fields #f #t #f desc ...)]
    [(_ name fields #:mutable #:transparent desc ...)
     (**defstruct name fields #f #t #f desc ...)]
    [(_ name fields #:mutable #:prefab desc ...)
     (**defstruct name fields #f #t #t desc ...)]
    [(_ name fields #:mutable desc ...)
     (**defstruct name fields #f #f #f desc ...)]
    [(_ name fields #:inspector #f desc ...)
     (**defstruct name fields #t #t #f desc ...)]
    [(_ name fields #:transparent desc ...)
     (**defstruct name fields #t #t #f desc ...)]
    [(_ name fields #:prefab desc ...)
     (**defstruct name fields #t #t #t desc ...)]
    [(_ name fields desc ...)
     (**defstruct name fields #t #f #f desc ...)]))

(define-syntax-rule (**defstruct name ([field field-contract] ...) immutable?
                                 transparent? prefab? desc ...)
  (with-togetherable-scheme-variables
   ()
   ()
   (*defstruct (quote-syntax/loc name) 'name
               '([field field-contract] ...)
               (list (lambda () (schemeblock0 field-contract)) ...)
               immutable? transparent? prefab? (lambda () (list desc ...)))))

(define (*defstruct stx-id name fields field-contracts immutable? transparent? prefab?
                    content-thunk)
  (define (field-name f) ((if (pair? (car f)) caar car) f))
  (define (field-view f)
    (if (pair? (car f)) (make-shaped-parens (car f) #\[) (car f)))
  (make-box-splice
   (cons
    (make-table
     'boxed
     (cons
      (list (make-flow
             (list
              (let* ([the-name
                      (let ([just-name
                             (make-target-element*
                              make-toc-target-element
                              (if (pair? name)
                                (car (syntax-e stx-id))
                                stx-id)
                              (annote-exporting-library
                               (to-element
                                (if (pair? name)
                                  (make-just-context (car name)
                                                     (car (syntax-e stx-id)))
                                  stx-id)))
                              (let ([name (if (pair? name) (car name) name)])
                                (list* (list 'info name)
                                       (list 'type 'struct: name)
                                       (list 'predicate name '?)
                                       (list 'constructor 'make- name)
                                       (append
                                        (map (lambda (f)
                                               (list 'accessor name '-
                                                     (field-name f)))
                                             fields)
                                        (filter-map
                                         (lambda (f)
                                           (if (or (not immutable?)
                                                   (and (pair? (car f))
                                                        (memq '#:mutable
                                                              (car f))))
                                             (list 'mutator 'set- name '-
                                                   (field-name f) '!)
                                             #f))
                                         fields)))))])
                        (if (pair? name)
                          (to-element (list just-name
                                            (make-just-context
                                             (cadr name)
                                             (cadr (syntax-e stx-id)))))
                          just-name))]
                     [short-width
                      (apply + (length fields) 8
                             (append
                              (map (lambda (s)
                                     (string-length (symbol->string s)))
                                   (append (if (pair? name) name (list name))
                                           (map field-name fields)))
                              (map (lambda (f)
                                     (if (pair? (car f))
                                       (+ 3 2 (string-length (keyword->string
                                                              (cadar f))))
                                       0))
                                   fields)))])
                (if (and (short-width . < . max-proto-width)
                         immutable?
                         (not transparent?))
                  (make-omitable-paragraph
                   (list
                    (to-element
                     `(,(schemeparenfont "struct")
                       ,the-name
                       ,(map field-view fields)))))
                  (make-table
                   #f
                   (append
                    (list
                     (list (to-flow (schemeparenfont "(struct"))
                           flow-spacer
                           (to-flow the-name)
                           (if (or (null? fields)
                                   (short-width . < . max-proto-width))
                               flow-spacer
                               (to-flow (make-element
                                         #f (list spacer (schemeparenfont "(")))))
                           (to-flow (if (or (null? fields)
                                            (short-width . < . max-proto-width))
                                        (make-element
                                         #f (cons (to-element (map field-view
                                                                   fields))
                                                  (if (and immutable?
                                                           (not transparent?))
                                                      (list (schemeparenfont ")"))
                                                      null)))
                                        (to-element (field-view (car fields)))))))
                    (if (short-width . < . max-proto-width)
                      null
                      (let loop ([fields (if (null? fields)
                                           fields (cdr fields))])
                        (if (null? fields)
                          null
                          (cons
                           (let ([fld (car fields)])
                             (list flow-spacer flow-spacer
                                   flow-spacer flow-spacer
                                   (to-flow
                                    (let ([e (to-element (field-view fld))])
                                      (if (null? (cdr fields))
                                        (make-element
                                         #f
                                         (list e (schemeparenfont
                                                  (if (and immutable?
                                                           (not transparent?))
                                                    "))" ")"))))
                                        e)))))
                           (loop (cdr fields))))))
                    (cond
                      [(and (not immutable?) transparent?)
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (to-element '#:mutable))
                              'cont
                              'cont)
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (if prefab?
                                                  (to-element '#:prefab)
                                                  (to-element '#:transparent))
                                              (schemeparenfont ")"))))
                              'cont
                              'cont))]
                      [(not immutable?)
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (to-element '#:mutable)
                                              (schemeparenfont ")"))))
                              'cont
                              'cont))]
                      [transparent?
                       (list
                        (list flow-spacer flow-spacer
                              (to-flow (make-element
                                        #f
                                        (list (if prefab?
                                                  (to-element '#:prefab)
                                                  (to-element '#:transparent))
                                              (schemeparenfont ")"))))
                              'cont
                              'cont))]
                      [else null]))))))))
      (map (lambda (v field-contract)
             (cond
               [(pair? v)
                (list
                 (make-flow
                  (make-table-if-necessary
                   "argcontract"
                   (list (list (to-flow (hspace 2))
                               (to-flow (to-element (field-name v)))
                               flow-spacer
                               (to-flow ":")
                               flow-spacer
                               (make-flow (list (field-contract))))))))]
               [else null]))
           fields field-contracts)))
    (content-thunk))))

;; ----------------------------------------

(define-syntax-rule (defthing id result desc ...)
  (with-togetherable-scheme-variables
   ()
   ()
   (*defthing (list (quote-syntax/loc id)) (list 'id) #f
              (list (schemeblock0 result))
              (lambda () (list desc ...)))))

(define-syntax-rule (defthing* ([id result] ...) desc ...)
  (with-togetherable-scheme-variables
   ()
   ()
   (*defthing (list (quote-syntax/loc id) ...) (list 'id ...) #f
              (list (schemeblock0 result) ...)
              (lambda () (list desc ...)))))

(define (*defthing stx-ids names form? result-contracts content-thunk)
  (make-box-splice
   (cons
    (make-table
     'boxed
     (map
      (lambda (stx-id name result-contract)
        (list
         (make-flow
          (make-table-if-necessary
           "argcontract"
           (list
            (list
             (make-flow
              (list
               (make-omitable-paragraph
                (list
                 (let ([target-maker
                        ((if form? id-to-form-target-maker id-to-target-maker)
                         stx-id #t)]
                       [content (list (definition-site name stx-id form?))])
                   (if target-maker
                     (target-maker
                      content
                      (lambda (tag)
                        (make-toc-target-element
                         #f
                         (list
                          (make-index-element
                           #f
                           content
                           tag
                           (list (symbol->string name))
                           content
                           (with-exporting-libraries
                            (lambda (libs) (make-thing-index-desc name libs)))))
                         tag)))
                     (car content)))
                 spacer ":" spacer))))
             (make-flow (list (if (block? result-contract)
                                result-contract
                                (make-omitable-paragraph (list result-contract)))))))))))
      stx-ids names result-contracts))
    (content-thunk))))

(define (defthing/proc id contract descs)
  (*defthing (list id) (list (syntax-e id)) #f (list contract)
             (lambda () descs)))

(define (make-target-element* inner-make-target-element stx-id content wrappers)
  (if (null? wrappers)
    content
    (make-target-element*
     make-target-element
     stx-id
     (let* ([name (string-append* (map symbol->string (cdar wrappers)))]
            [target-maker
             (id-to-target-maker (datum->syntax stx-id (string->symbol name))
                                 #t)])
       (if target-maker
         (target-maker
          (list content)
          (lambda (tag)
            (inner-make-target-element
             #f
             (list
              (make-index-element
               #f
               (list content)
               tag
               (list name)
               (list (schemeidfont (make-element "schemevaluelink"
                                                 (list name))))
               (with-exporting-libraries
                (lambda (libs)
                  (let ([name (string->symbol name)])
                    (if (eq? 'info (caar wrappers))
                      (make-struct-index-desc name libs)
                      (make-procedure-index-desc name libs)))))))
             tag)))
         content))
     (cdr wrappers))))

