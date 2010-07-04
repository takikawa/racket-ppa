#lang scheme/base

(require scheme/bool scheme/contract)

(provide poly/c parametric/c opaque/c memory/c)

(define-syntax-rule (poly/c [x ...] c)
  (make-polymorphic-contract 'poly/c
                             memory/c
                             '(x ...)
                             (lambda (x ...) c)))

(define-syntax-rule (parametric/c [x ...] c)
  (make-polymorphic-contract 'parametric/c
                             opaque/c
                             '(x ...)
                             (lambda (x ...) c)))

(define-struct polymorphic-contract [title barrier vars body]
  #:property prop:contract
  (build-contract-property
   #:name
   (lambda (c)
     (list (polymorphic-contract-title c)
           (polymorphic-contract-vars c)
           '...))
   #:projection
   (lambda (c)
     (lambda (b)

       (define (wrap p)
         ;; values in polymorphic types come in from negative position,
         ;; relative to the poly/c contract
         (define negative? (blame-swapped? b))
         (define barrier/c (polymorphic-contract-barrier c))
         (define instances
           (for/list ([var (in-list (polymorphic-contract-vars c))])
             (barrier/c negative? var)))
         (define protector
           (apply (polymorphic-contract-body c) instances))
         (((contract-projection protector) b) p))

       (lambda (p)
         (unless (procedure? p)
           (raise-blame-error b p "expected a procedure; got: ~e" p))
         (make-keyword-procedure
          (lambda (keys vals . args) (keyword-apply (wrap p) keys vals args))
          (case-lambda
            [() ((wrap p))]
            [(a) ((wrap p) a)]
            [(a b) ((wrap p) a b)]
            [(a b c) ((wrap p) a b c)]
            [(a b c d) ((wrap p) a b c d)]
            [(a b c d e) ((wrap p) a b c d e)]
            [(a b c d e f) ((wrap p) a b c d e f)]
            [(a b c d e f g) ((wrap p) a b c d e f g)]
            [(a b c d e f g h) ((wrap p) a b c d e f g h)]
            [args (apply (wrap p) args)])))))))

(define (memory/c positive? name)
  (define memory (make-weak-hasheq))
  (define (make x) (hash-set! memory x #t) x)
  (define (pred x) (hash-has-key? memory x))
  (define (get x) x)
  (make-barrier-contract name positive? make pred get))

(define (opaque/c positive? name)
  (define-values [ type make pred getter setter ]
    (make-struct-type name #f 1 0))
  (define (get x) (getter x 0))
  (make-barrier-contract name positive? make pred get))

(define-struct barrier-contract [name positive? make pred get]
  #:property prop:contract
  (build-contract-property
   #:name (lambda (c) (barrier-contract-name c))
   #:projection
   (lambda (c)
     (lambda (b)
       (if (boolean=? (blame-original? b) (barrier-contract-positive? c))
         (lambda (x)
           ((barrier-contract-make c) x))
         (lambda (x)
           (if ((barrier-contract-pred c) x)
             ((barrier-contract-get c) x)
             (raise-blame-error b x "expected a(n) ~a; got: ~e"
                                (barrier-contract-name c) x))))))))
