#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/serialize
                     syntax/parse
                     syntax/stx)
         racket/promise
         racket/serialize
         racket/runtime-path
         ffi/unsafe
         (only-in '#%foreign ctype-c->scheme))

(provide (rename-out [prop:serialize-cstruct? serializable-cstruct?])
         define-serializable-cstruct)


(define-values (prop:serialize-cstruct prop:serialize-cstruct? prop:serialize-cstruct-ref)
  (make-struct-type-property 'serialize-cstruct))


(define cpointer-mapping (make-weak-hash))


(define-syntax (define-serializable-cstruct stx)
  (syntax-parse stx
    [(_ _ID:id ([field-id:id type-expr:expr] ...)
        (~or (~optional (~seq #:malloc-mode malloc-mode:expr)
                        #:name "#:malloc-mode"
                        #:defaults ([malloc-mode #'(quote atomic)]))
             (~optional (~seq (~and #:serialize-inplace serialize-inplace-kw))
                        #:name "#:serialize-inplace")
             (~optional (~seq (~and #:deserialize-inplace deserialize-inplace-kw))
                        #:name "#:deserialize-inplace")
             (~optional (~seq #:alignment align-expr:expr)
                        #:name "#:alignment")
             (~optional (~seq #:version vers:exact-nonnegative-integer)
                        #:name "#:version"
                        #:defaults ([vers #'0]))
             (~optional (~seq #:other-versions ([other-vers:exact-nonnegative-integer
                                                 other-vers-deser-chain:expr
                                                 other-vers-convert:expr
                                                 other-vers-unconvert:expr
                                                 other-vers-cycle-convert:expr]
                                                ...))
                        #:name "#:other-versions"
                        #:defaults ([(other-vers 1) null]
                                    [(other-vers-deser-chain 1) null]
                                    [(other-vers-convert 1) null]
                                    [(other-vers-unconvert 1) null]
                                    [(other-vers-cycle-convert 1) null]))
             (~seq #:property prop-expr:expr propval-expr:expr))
        ...)

     (unless (memq (syntax-local-context) '(module top-level))
       (raise-syntax-error #f "only allowed in module or top-level context" stx))
     (unless (stx-pair? #'(type-expr ...))
       (raise-syntax-error #f "expected [field-id ctype] ..." stx))

     (define id (string->symbol (cadr (or (regexp-match #rx"^_(.*)$" (symbol->string (syntax-e #'_ID)))
                                          (raise-syntax-error #f "id must start with '_'" stx #'_ID)))))
     (define (make-deserialize-id vers)
       (format-id #'_ID "deserialize:cstruct:~a~a" id
                  (if (zero? (syntax-e vers))
                      ""
                      (format "-v~a" (syntax-e vers)))))
     (define (make-deserialize-chain-id vers)
       (format-id #'_ID "deserialize-chain:cstruct:~a~a" id
                  (if (zero? (syntax-e vers))
                      ""
                      (format "-v~a" (syntax-e vers)))))

     (with-syntax ([_ID-pointer (format-id #'_ID "~a-pointer" #'_ID)]

                   [(align ...)
                    (if (attribute align-expr)
                        (list '#:alignment (attribute align-expr))
                        null)]
                   [((props ...) ...)
                    (map (lambda (p v)
                           (when (free-identifier=? #'prop:serializable p)
                             (raise-syntax-error #f "#:property prop:serializable not allowed" stx p))
                           (list '#:property p v))
                         (attribute prop-expr) (attribute propval-expr))]

                   [(acc-list ...) (stx-map (lambda (e) (format-id #'_ID "~a-~a" id e))
                                            #'(field-id ...))]
                   [(mod-list ...) (stx-map (lambda (e) (format-id #'_ID "set-~a-~a!" id e))
                                            #'(field-id ...))]

                   [deser-ID (make-deserialize-id #'vers)]
                   [deser-chain-ID (make-deserialize-chain-id #'vers)]
                   [(other-vers-deser-ID ...) (for/list ([other-vers (in-list (syntax->list #'(other-vers ...)))])
                                                (make-deserialize-id other-vers))]
                                                          
                   [make-ID/mode (format-id #'_ID "make-~a/mode" id)]

                   [serialize-inplace (and (attribute serialize-inplace-kw) (not (eq? 'cs (system-type 'gc))))]
                   [deserialize-inplace (and (attribute deserialize-inplace-kw) (not (eq? 'cs (system-type 'gc))))])

       (quasisyntax/loc stx
         (begin
           #,@(if (eq? 'top-level (syntax-local-context))
                  ;; Forward reference:
                  `((define-syntaxes (copy-any-non-pointers-for-ID? all-serializable) (values)))
                  null)
           
           ;; the wrapped cstruct
           (define-cstruct _ID ([field-id type-expr] ...)
             align ...
             props ... ...
             #:property prop:serializable
             (make-serialize-info
              (lambda (s)
                (force all-serializable)
                (hash-set! cpointer-mapping s s)
                (define bs
                  (if serialize-inplace
                      (make-sized-byte-string s (ctype-sizeof _ID))
                      (let ([mem (make-bytes (ctype-sizeof _ID))])
                        (when (force copy-any-non-pointers-for-ID?)
                          (memcpy mem s 1 _ID))
                        mem)))
                (vector bs (serialize-cstruct-pointers s)))
              (quote-syntax deser-ID)
              #t
              (or (current-load-relative-directory) (current-directory)))
             #:property prop:serialize-cstruct
             (lambda () (values _ID (list acc-list ...) (list mod-list ...))))

           ;; malloc according to malloc-mode
           (define (malloc-ID)
             (if (procedure? malloc-mode)
                 (malloc-mode (ctype-sizeof _ID))
                 (malloc _ID malloc-mode)))

           ;; must be delayed to handle cyclic structs
           (define copy-any-non-pointers-for-ID?
             (delay (copy-any-non-pointers? _ID)))

           ;; deserialization proc
           #,@(if (eq? (syntax-local-context) 'module)
                  #`((runtime-require (submod "." deserialize-info))
                     (module+ deserialize-info (provide deser-ID
                                                        other-vers-deser-ID ...)))
                  null)
           (define deser-chain-ID (id->deserialize-chain-info _ID _ID-pointer deserialize-inplace malloc-ID copy-any-non-pointers-for-ID?))
           (define deser-ID (deserialize-chain-info->deserialize-info deser-chain-ID))
           (define other-vers-deser-ID (chain+converters->deserialize-info other-vers-deser-chain
                                                                           other-vers-convert
                                                                           other-vers-unconvert
                                                                           other-vers-cycle-convert))
           ...

           ;; mode-aware make-ID
           (define (make-ID/mode field-id ...)
             (define s (ptr-ref (malloc-ID) _ID))
             (mod-list s field-id) ...
             s)

           ;; ctype serializable check (must be delayed to handle cyclic structs)
           (define all-serializable
             (delay
               (check-all-serializable (list type-expr ...)
                                       _ID
                                       '(field-id ...)))))))]))

(define (check-all-serializable ctypes _ID field-ids)
  (for ([ct (in-list ctypes)]
        [t (in-list (ctype->layout _ID))]
        [n field-ids])
    (define base (ctype-layout-base-type t))

    ;; fpointer never possible
    (when (eq? base 'fpointer)
      (error 'serialize-cstruct "~a::~a of type ~a is not serializable" '_ID n t))

    ;; struct (ptr, embedded), maybe in array
    (when (or (list? base)
              (eq? base 'pointer))

      (define c->s (ctype-c->scheme (array-base-type ct)))
      (unless (and c->s (prop:serialize-cstruct? (c->s (malloc _pointer))))
        (error 'serialize-cstruct "~a::~a of type ~a is not serializable" '_ID n t)))))

(define (array-base-type ct)
  (if (vector? (ctype->layout ct))
      (array-base-type (array-type ((ctype-c->scheme ct) #f)))
      ct))

(define (ctype-layout-base-type v)
  (if (vector? v)
      (ctype-layout-base-type (vector-ref v 0))
      v))

(struct chain-deserialize-info (make cycle-make))

(define (id->deserialize-chain-info _ID _ID-pointer deserialize-inplace malloc-ID copy-any-non-pointers-for-ID?)
  (chain-deserialize-info
   (lambda (bs ptrs)
     (define s
       (if deserialize-inplace
           (cast bs _bytes _ID-pointer)
           (let ([mem (malloc-ID)])
             (when (force copy-any-non-pointers-for-ID?)
               (memcpy mem bs 1 _ID))
             (cast mem _pointer _ID-pointer))))
     (deserialize-cstruct-pointers s ptrs)
     s)

   (lambda ()
     (define s (malloc-ID))
     (values (cast s _pointer _ID-pointer)
             (lambda (s0)
               (memcpy s s0 1 _ID))))))

(define (deserialize-chain-info->deserialize-info c)
  (make-deserialize-info
   (chain-deserialize-info-make c)
   (chain-deserialize-info-cycle-make c)))

(define (chain+converters->deserialize-info c
                                            other-vers-convert 
                                            other-vers-unconvert 
                                            other-vers-cycle-convert)
  (make-deserialize-info
   (lambda (bs ptrs)
     (other-vers-convert ((chain-deserialize-info-make c) bs ptrs)))
   (lambda ()
     (define-values (old-v old-fill!) ((chain-deserialize-info-cycle-make c)))
     (define-values (v fill!) (other-vers-cycle-convert))
     (values v
             (lambda (s0)
               (old-fill! (other-vers-unconvert s0))
               (fill! old-v))))))

(define ptr-types '(bytes string/ucs-4 string/utf-16 pointer gcpointer))

(define (copy-any-non-pointers? _ID)
  (for/or ([t (in-list (ctype->layout _ID))])
    (let ([base (ctype-layout-base-type t)])
      (let loop ([base base])
        (cond
          [(list? base) (ormap loop base)]
          [(vector? base) (loop (vector-ref base 0))]
          [else (not (memq base ptr-types))])))))

(define (serialize-cstruct-pointers o)
  (define who 'serialize-cstruct-pointers)
  (unless (prop:serialize-cstruct? o)
    (raise-argument-error who "serializable-cstruct?" o))

  (define-values (_ID accs mods) ((prop:serialize-cstruct-ref o)))

  (define (serialize-basic t o)
    (cond
     [(list? t)
      (and o (serialize-cstruct-pointers o))]

     [(memq t ptr-types)
      (unless (serializable? o)
        (raise-argument-error who "serializable?" o))
      (hash-ref! cpointer-mapping o o)]))

  (for/vector ([t (in-list (ctype->layout _ID))]
               [acc (in-list accs)]
               #:when (let ([base (ctype-layout-base-type t)])
                        (when (eq? base 'fpointer)
                          (raise-argument-error who "serializable?" 'fpointer))
                        (or (list? base)
                            (memq base ptr-types))))

    (define base (ctype-layout-base-type t))

    (cond
     [(vector? t)
      (let loop ([ar (acc o)])
        (define len (array-length ar))
        (for/vector #:length len ([i (in-range len)])
          (define v (array-ref ar i))
          (if (array? v)
              (loop v)
              (serialize-basic base v))))]

     [else
      (serialize-basic t (acc o))])))


(define (deserialize-cstruct-pointers o ptrs)
  (unless (prop:serialize-cstruct? o)
    (raise-argument-error 'deserialize-cstruct-pointers "serializable-cstruct?" o))

  (define-values (_ID accs mods) ((prop:serialize-cstruct-ref o)))

  (for ([acc (in-list accs)]
        [mod (in-list mods)]
        [t (in-list (ctype->layout _ID))]
        [p (in-vector ptrs)])

    (define base (ctype-layout-base-type t))
    (cond
     [(and (vector? t)
           (or (memq base ptr-types)
               (list? base)))

      (let loop ([ar (acc o)]
                 [pvec p]
                 [sub-t t])
        (define len (array-length ar))
        (if (vector? (vector-ref sub-t 0))
            (for ([i (in-range len)]
                  [pv (in-vector pvec)])
              (loop (array-ref ar i) pv (vector-ref sub-t 0)))
            (for ([i (in-range len)]
                  [pv (in-vector pvec)])
              (if (list? base)
                  (deserialize-cstruct-pointers (array-ref ar i) pv)
                  (array-set! ar i pv)))))]


     [(list? t)
      (deserialize-cstruct-pointers (acc o) p)]

     [(and p (memq t ptr-types))
      (mod o p)])))
