#lang scheme/base
(require scheme/contract
         scheme/match
         syntax/stx
         syntax/id-table
         "../util.ss"
         "rep-patterns.ss")
(provide (struct-out attr))

#|
An IAttr is (make-attr identifier number boolean)
An SAttr is (make-attr symbol number boolean)

The number is the ellipsis nesting depth. The boolean is true iff the
attr is guaranteed to be bound to a value which is a syntax object (or
a list^depth of syntax objects).
|#

(define-struct attr (name depth syntax?) #:prefab)

(define (iattr? a)
  (and (attr? a) (identifier? (attr-name a))))

(define (sattr? a)
  (and (attr? a) (symbol? (attr-name a))))

;; increase-depth : Attr -> Attr
(define (increase-depth x)
  (make attr (attr-name x) (add1 (attr-depth x)) (attr-syntax? x)))

(provide/contract
 [iattr? (any/c . -> . boolean?)]
 [sattr? (any/c . -> . boolean?)]

 [increase-depth
  (-> attr? attr?)]
 [attr-make-uncertain
  (-> attr? attr?)]

 ;; IAttr operations
 [append-iattrs
  (-> (listof (listof iattr?))
      (listof iattr?))]
 [union-iattrs
  (-> (listof (listof iattr?))
      (listof iattr?))]
 [reorder-iattrs
  (-> (listof sattr?) (listof iattr?)
      (listof iattr?))]
 [rename-attr
  (-> iattr? identifier?
      iattr?)]

 ;; SAttr operations
 [iattr->sattr
  (-> iattr?
      sattr?)]
 [iattrs->sattrs
  (-> (listof iattr?)
      (listof sattr?))]

 [intersect-sattrss
  (-> (listof (listof sattr?))
      (listof sattr?))])

;; IAttr operations

;; append-iattrs : (listof (listof IAttr)) -> (listof IAttr)
(define (append-iattrs attrss)
  (let* ([all (apply append attrss)]
         [names (map attr-name all)]
         [dup (check-duplicate-identifier names)])
    (when dup
      (wrong-syntax dup "duplicate attribute"))
    all))

;; union-iattrs : (listof (listof IAttr)) -> (listof IAttr)
(define (union-iattrs attrss)
  (define count-t (make-bound-id-table))
  (define attr-t (make-bound-id-table))
  (define list-count (length attrss))
  (for* ([attrs attrss] [attr attrs])
    (define name (attr-name attr))
    (define prev (bound-id-table-ref attr-t name #f))
    (bound-id-table-set! attr-t name (join-attrs attr prev))
    (let ([pc (bound-id-table-ref count-t name 0)])
      (bound-id-table-set! count-t name (add1 pc))))
  (for/list ([a (bound-id-table-map attr-t (lambda (_ v) v))])
    (if (= (bound-id-table-ref count-t (attr-name a)) list-count)
        a
        (attr-make-uncertain a))))

;; join-attrs : Attr Attr/#f -> Attr
;; Works with both IAttrs and SAttrs.
;; Assumes attrs have same name.
(define (join-attrs a b)
  (if (and a b)
      (proper-join-attrs a b)
      (or a b)))

(define (proper-join-attrs a b)
  (let ([aname (attr-name a)])
    (unless (equal? (attr-depth a) (attr-depth b))
      (wrong-syntax (and (syntax? aname) aname)
                    "attribute '~a' occurs with different nesting depth"
                    (if (syntax? aname) (syntax-e aname) aname)))
    (make attr aname (attr-depth a) (and (attr-syntax? a) (attr-syntax? b)))))

(define (attr-make-uncertain a)
  (make attr (attr-name a) (attr-depth a) #f))

(define (iattr->sattr a)
  (match a
    [(struct attr (name depth syntax?))
     (make attr (syntax-e name) depth syntax?)]))

(define (iattrs->sattrs as)
  (map iattr->sattr as))

(define (rename-attr a name)
  (make attr name (attr-depth a) (attr-syntax? a)))

;; intersect-sattrss : (listof (listof SAttr)) -> (listof SAttr)
(define (intersect-sattrss attrss)
  (cond [(null? attrss) null]
        [else
         (let* ([namess (map (lambda (attrs) (map attr-name attrs)) attrss)]
                [names (filter (lambda (s)
                                 (andmap (lambda (names) (memq s names))
                                         (cdr namess)))
                               (car namess))]
                [ht (make-hasheq)]
                [put (lambda (attr) (hash-set! ht (attr-name attr) attr))]
                [fetch-like (lambda (attr) (hash-ref ht (attr-name attr) #f))])
           (for* ([attrs attrss]
                  [attr attrs]
                  #:when (memq (attr-name attr) names))
             (put (join-attrs attr (fetch-like attr))))
           (sort (hash-map ht (lambda (k v) v))
                 (lambda (a b)
                   (string<? (symbol->string (attr-name a))
                             (symbol->string (attr-name b))))))]))

;; reorder-iattrs : (listof SAttr) (listof IAttr) -> (listof IAttr)
;; Reorders iattrs (and restricts) based on relsattrs
;; If a relsattr is not found, or if depth or contents mismatches, raises error.
(define (reorder-iattrs relsattrs iattrs)
  (let ([ht (make-hasheq)])
    (for ([iattr iattrs])
      (let ([remap-name (syntax-e (attr-name iattr))])
        (hash-set! ht remap-name iattr)))
    (let loop ([relsattrs relsattrs])
      (match relsattrs
        ['() null]
        [(cons sattr rest)
         (let ([iattr (hash-ref ht (attr-name sattr) #f)])
           (check-iattr-satisfies-sattr iattr sattr)
           (cons iattr (loop rest)))]))))

(define (check-iattr-satisfies-sattr iattr sattr)
  (unless iattr
    (wrong-syntax #f "required attribute is not defined: ~s" (attr-name sattr)))
  (unless (= (attr-depth iattr) (attr-depth sattr))
    (wrong-syntax (attr-name iattr)
                  "attribute has wrong depth (expected ~s, found ~s)"
                  (attr-depth sattr) (attr-depth iattr)))
  (when (and (attr-syntax? sattr) (not (attr-syntax? iattr)))
    (wrong-syntax (attr-name iattr)
                  "attribute may not be bound to syntax: ~s"
                  (attr-name sattr))))
