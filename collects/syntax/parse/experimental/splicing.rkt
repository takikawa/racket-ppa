#lang racket/base
(require (for-syntax racket/base
                     "../../parse.rkt"
                     "../private/rep-data.rkt"
                     "../private/kws.rkt")
         "../private/runtime.rkt")
(provide define-primitive-splicing-syntax-class)

(define-syntax (define-primitive-splicing-syntax-class stx)

  (define-syntax-class attr
    (pattern name:id
             #:with depth #'0)
    (pattern [name:id depth:nat]))

  (syntax-parse stx
    [(dssp (name:id param:id ...)
       (~or (~once (~seq #:attributes (a:attr ...))
                   #:name "attributes declaration")
            (~once (~seq #:description description)
                   #:name "description declaration")) ...
       proc:expr)
     #'(begin
         (define (get-description param ...)
           description)
         (define parser
           (let ([permute (mk-permute '(a.name ...))])
             (lambda (x cx pr es fh cp success param ...)
               (let ([stx (datum->syntax cx x cx)])
                 (let ([result
                        (let/ec escape
                          (cons 'ok
                                (proc stx
                                      (lambda ([msg #f] [stx #f])
                                        (escape (list 'error msg stx))))))])
                   (case (car result)
                     ((ok)
                      (apply success
                             ((mk-check-result pr 'name (length '(a.name ...)) permute x cx fh cp)
                              (cdr result))))
                     ((error)
                      (let ([es
                             (list* (expect:message (cadr result))
                                    (expect:thing (get-description param ...) #f)
                                    es)])
                        (fh (failure pr es))))))))))
         (define-syntax name
           (stxclass 'name (arity (length '(param ...)) (length '(param ...)) '() '())
                     (sort-sattrs '(#s(attr a.name a.depth #f) ...))
                     (quote-syntax parser)
                     #t
                     #s(options #t #t)
                     #f)))]))

(define (mk-permute unsorted-attrs)
  (let ([sorted-attrs
         (sort unsorted-attrs string<? #:key symbol->string #:cache-keys? #t)])
    (if (equal? unsorted-attrs sorted-attrs)
        values
        (let* ([pos-table
                (for/hasheq ([a (in-list unsorted-attrs)] [i (in-naturals)])
                  (values a i))]
               [indexes
                (for/vector ([a (in-list sorted-attrs)])
                  (hash-ref pos-table a))])
          (lambda (result)
            (for/list ([index (in-vector indexes)])
              (list-ref result index)))))))

(define (mk-check-result pr name attr-count permute x cx fh cp)
  (lambda (result)
    (unless (list? result)
      (error name "parser returned non-list"))
    (let ([rlength (length result)])
      (unless (= rlength (+ 1 attr-count))
        (error name "parser returned list of wrong length; expected length ~s, got ~e"
               (+ 1 attr-count)
               result))
      (let ([skip (car result)])
        ;; Compute rest-x & rest-cx from skip
        (unless (exact-nonnegative-integer? skip)
          (error name "expected exact nonnegative integer for first element of result list, got ~e"
                 skip))
        (let-values ([(rest-x rest-cx) (stx-list-drop/cx x cx skip)])
          (list* fh cp rest-x rest-cx (ps-add-cdr pr skip)
                 (permute (cdr result))))))))
