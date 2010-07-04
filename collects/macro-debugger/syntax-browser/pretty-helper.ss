
#lang scheme/base
(require scheme/class
         macro-debugger/util/class-iop
         syntax/stx
         "interfaces.ss")
(provide (all-defined-out))

;; Problem: If stx1 and stx2 are two distinguishable syntax objects, it
;; still may be the case that (syntax-e stx1) and (syntax-e stx2) are 
;; indistinguishable.

;; Solution: Rather than map stx to (syntax-e stx), in the cases where
;; (syntax-e stx) is confusable, map it to a different, unique, value.
;;   - stx is identifier : map it to an uninterned symbol w/ same rep
;;     (Symbols are useful: see pretty-print's style table)
;;   - else : map it to a syntax-dummy object

;; NOTE: Nulls are only wrapped when *not* list-terminators.  
;; If they were always wrapped, the pretty-printer would screw up
;; list printing (I think).

(define-struct syntax-dummy (val))

;; A SuffixOption is one of
;; - 'never             -- never
;; - 'always            -- suffix > 0
;; - 'over-limit        -- suffix > limit
;; - 'all-if-over-limit -- suffix > 0 if any over limit

;; syntax->datum/tables : stx partition% num SuffixOption
;;                        -> (values s-expr hashtable hashtable)
;; When partition is not false, tracks the partititions that subterms belong to
;; When limit is a number, restarts processing with numbering? set to true
;; 
;; Returns three values:
;;   - an S-expression
;;   - a hashtable mapping S-expressions to syntax objects
;;   - a hashtable mapping syntax objects to S-expressions
;; Syntax objects which are eq? will map to same flat values
(define (syntax->datum/tables stx partition limit suffixopt)
  (table stx partition limit suffixopt))

;; table : syntax maybe-partition% maybe-num SuffixOption -> (values s-expr hashtable hashtable)
(define (table stx partition limit suffixopt)
  (define (make-identifier-proxy id)
    (case suffixopt
      ((never) (unintern (syntax-e id)))
      ((always)
       (let ([n (send: partition partition<%> get-partition id)])
         (if (zero? n) (unintern (syntax-e id)) (suffix (syntax-e id) n))))
      ((over-limit)
       (let ([n (send: partition partition<%> get-partition id)])
         (if (<= n limit)
             (unintern (syntax-e id))
             (suffix (syntax-e id) n))))))

  (let/ec escape
    (let ([flat=>stx (make-hasheq)]
          [stx=>flat (make-hasheq)])
      (define (loop obj)
        (cond [(hash-ref stx=>flat obj (lambda _ #f))
               => (lambda (datum) datum)]
              [(and partition (identifier? obj))
               (when (and (eq? suffixopt 'all-if-over-limit)
                          (> (send: partition partition<%> count) limit))
                 (call-with-values (lambda () (table stx partition #f 'always))
                                   escape))
               (let ([lp-datum (make-identifier-proxy obj)])
                 (hash-set! flat=>stx lp-datum obj)
                 (hash-set! stx=>flat obj lp-datum)
                 lp-datum)]
              [(and (syntax? obj) (check+convert-special-expression obj))
               => (lambda (newobj)
                    (when partition (send: partition partition<%> get-partition obj))
                    (let* ([inner (cadr newobj)]
                           [lp-inner-datum (loop inner)]
                           [lp-datum (list (car newobj) lp-inner-datum)])
                      (hash-set! flat=>stx lp-inner-datum inner)
                      (hash-set! stx=>flat inner lp-inner-datum)
                      (hash-set! flat=>stx lp-datum obj)
                      (hash-set! stx=>flat obj lp-datum)
                      lp-datum))]
              [(syntax? obj)
               (when partition (send: partition partition<%> get-partition obj))
               (let ([lp-datum (loop (syntax-e obj))])
                 (hash-set! flat=>stx lp-datum obj)
                 (hash-set! stx=>flat obj lp-datum)
                 lp-datum)]
              [(pair? obj)
               (pairloop obj)]
              [(symbol? obj)
               (unintern obj)]
              [(null? obj)
               (make-syntax-dummy obj)]
              [(boolean? obj)
               (make-syntax-dummy obj)]
              [(number? obj)
               (make-syntax-dummy obj)]
              [(keyword? obj)
               (make-syntax-dummy obj)]
              [(vector? obj) 
               (list->vector (map loop (vector->list obj)))]
              [(box? obj)
               (box (loop (unbox obj)))]
              [else obj]))
      (define (pairloop obj)
        (cond [(pair? obj)
               (cons (loop (car obj))
                     (pairloop (cdr obj)))]
              [(null? obj)
               null]
              [(and (syntax? obj) (null? (syntax-e obj)))
               null]
              [else (loop obj)]))
      (values (loop stx)
              flat=>stx
              stx=>flat))))

;; check+convert-special-expression : syntax -> #f/syntaxish
(define (check+convert-special-expression stx)
  (define stx-list (stx->list stx))
  (and stx-list (= 2 (length stx-list))
       (let ([kw (car stx-list)]
             [expr (cadr stx-list)])
         (and (identifier? kw)
              (memq (syntax-e kw) special-expression-keywords)
              (bound-identifier=? kw (datum->syntax stx (syntax-e kw)))
              (andmap (lambda (f) (equal? (f stx) (f kw)))
                      (list syntax-source
                            syntax-line
                            syntax-column
                            syntax-position
                            syntax-original?
                            syntax-source-module))
              (cons (syntax-e kw)
                    (list expr))))))

(define special-expression-keywords
  '(quote quasiquote unquote unquote-splicing syntax))
;; FIXME: quasisyntax unsyntax unsyntax-splicing

(define (unintern sym)
  (string->uninterned-symbol (symbol->string sym)))

(define (suffix sym n)
  (string->uninterned-symbol (format "~a:~a" sym n)))
