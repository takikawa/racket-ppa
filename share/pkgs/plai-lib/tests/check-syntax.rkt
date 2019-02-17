#lang racket
(require drracket/check-syntax
         rackunit)

(define str-port
  (open-input-string  #<<---
(module m racket/base
  (require plai/datatype)
  
  (define-type name1
    [complex (n name1?)]
    [simple])
  
  (name1? 3)
  
  (type-case name1 (complex (simple))
    [simple () 7]
    [complex (n) 11]))
---
                      ))
(port-count-lines! str-port)
(define stx
  (read-syntax
   (build-path (current-directory) "whatever.rkt")
   str-port))

(define interesting-names '(name1 complex simple))

(define id-positions (make-hash))
(for ([name (in-list interesting-names)])
  (define positions '())
  (let loop ([stx stx])
    (cond
      [(pair? stx)
       (loop (car stx))
       (loop (cdr stx))]
      [(identifier? stx)
       (when (equal? (syntax-e stx) name)
         (hash-set! id-positions
                    (list (- (syntax-position stx) 1)
                          (- (+ (syntax-position stx) (syntax-span stx)) 1))
                    name))]
      [(syntax? stx) (loop (syntax-e stx))])))

(define arrow-counts (make-hash))
(for ([x (in-list (show-content stx))])
  (match x
    [`#(syncheck:add-arrow/name-dup/pxpy
        ,head-start ,head-end ,_ ,_ ;; start coord
        ,tail-start ,tail-end ,_ ,_ ;; end coord
        ,_ ...) ;; more stuff
     (define head (hash-ref id-positions (list head-start head-end) #f))
     (define tail (hash-ref id-positions (list tail-start tail-end) #f))
     (when (and head (equal? head tail))
       (hash-set! arrow-counts head (+ (hash-ref arrow-counts head 0) 1)))]
    [_ (void)]))

;; check only that there is one arrows that connect
;; name1 bindings and there are two arrows that
;; connect 'complex' and 'simple' bindings. 
(check-equal? arrow-counts
              (make-hash (list (cons 'name1 1)
                               (cons 'complex 2)
                               (cons 'simple 2))))
