#lang racket/base

(require "blame.rkt")

(provide prop:contract
         contract-struct?
         contract-struct-name
         contract-struct-first-order
         contract-struct-projection
         contract-struct-stronger?

         prop:flat-contract
         flat-contract-struct?
         
         prop:chaperone-contract
         chaperone-contract-struct?

         contract-property?
         build-contract-property
         
         chaperone-contract-property?
         build-chaperone-contract-property

         flat-contract-property?
         build-flat-contract-property

         make-contract
         make-chaperone-contract
         make-flat-contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct contract-property [ name first-order projection stronger generator ]
  #:omit-define-syntaxes)

(define (contract-property-guard prop info)
  (unless (contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a contract property; got: ~e"
              'prop:contract
              prop)
      (current-continuation-marks))))
  prop)

(define-values [ prop:contract contract-struct? contract-struct-property ]
  (make-struct-type-property 'prop:contract contract-property-guard))

(define (contract-struct-name c)
  (let* ([prop (contract-struct-property c)]
         [get-name (contract-property-name prop)]
         [name (get-name c)])
    name))

(define (contract-struct-first-order c)
  (let* ([prop (contract-struct-property c)]
         [get-first-order (contract-property-first-order prop)]
         [first-order (get-first-order c)])
    first-order))

(define (contract-struct-projection c)
  (let* ([prop (contract-struct-property c)]
         [get-projection (contract-property-projection prop)]
         [projection (get-projection c)])
    projection))

(define (contract-struct-stronger? a b)
  (let* ([prop (contract-struct-property a)]
         [stronger (contract-property-stronger prop)])
    (stronger a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Chaperone Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct chaperone-contract-property [implementation]
  #:omit-define-syntaxes)

(define (chaperone-contract-property-guard prop info)
  (unless (chaperone-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a chaperone contract property; got: ~e"
              'prop:chaperone-contract
              prop)
      (current-continuation-marks))))
  prop)

;; We check to make sure the contract projection actually resulted in
;; a chaperone (or chaperone-friendly) version of the value.
(define (chaperone-contract-property->contract-property fc)
  (let ([impl (chaperone-contract-property-implementation fc)])
    impl))

(define-values [ prop:chaperone-contract
                 chaperone-contract-struct?
                 chaperone-contract-struct-property ]
  (make-struct-type-property
   'prop:chaperone-contract
   chaperone-contract-property-guard
   (list (cons prop:contract chaperone-contract-property->contract-property))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Flat Contract Property
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct flat-contract-property [implementation]
  #:omit-define-syntaxes)

(define (flat-contract-property-guard prop info)
  (unless (flat-contract-property? prop)
    (raise
     (make-exn:fail:contract
      (format "~a: expected a flat contract property; got: ~e"
              'prop:flat-contract
              prop)
      (current-continuation-marks))))
  prop)

(define (flat-contract-property->chaperone-contract-property fc)
  (let ([impl (flat-contract-property-implementation fc)])
    (make-chaperone-contract-property impl)))

(define (flat-contract-property->procedure-property prop)
  (let* ([impl (flat-contract-property-implementation prop)]
         [get-predicate (contract-property-first-order impl)])
    (lambda (c x) ((get-predicate c) x))))

(define-values [ prop:flat-contract
                 flat-contract-struct?
                 flat-contract-struct-property ]
  (make-struct-type-property
   'prop:flat-contract
   flat-contract-property-guard
   (list (cons prop:chaperone-contract flat-contract-property->chaperone-contract-property)
         (cons prop:procedure flat-contract-property->procedure-property))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Friendly Property Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((build-property mk default-name projection-wrapper)
         #:name [get-name #f]
         #:first-order [get-first-order #f]
         #:projection [get-projection #f]
         #:stronger [stronger #f]
         #:generator [generator #f])

  (let* ([get-name (or get-name (lambda (c) default-name))]
         [get-first-order (or get-first-order get-any?)]
         [get-projection
          (cond
            [get-projection (projection-wrapper get-projection)]
            [else (get-first-order-projection
                   get-name get-first-order)])]
         [stronger (or stronger weakest)])

    (mk get-name get-first-order get-projection stronger generator)))

(define build-contract-property
  (build-property make-contract-property 'anonymous-contract values))

;; Here we'll force the projection to always return the original value,
;; instead of assuming that the provided projection does so appropriately.
(define (flat-projection-wrapper f)
  (λ (c)
    (let ([proj (f c)])
      (λ (b)
        (let ([p (proj b)])
          (λ (v) (p v) v))))))

(define build-flat-contract-property
  (build-property (compose make-flat-contract-property make-contract-property)
                  'anonymous-flat-contract
                  flat-projection-wrapper))

(define (chaperone-projection-wrapper f)
  (λ (c)
    (let ([proj (f c)])
      (λ (b)
        (let ([p (proj b)])
          (λ (v)
            (let ([v* (p v)])
              (unless (chaperone-of? v* v)
                (error 'prop:chaperone-contract (format "expected a chaperone of ~v, got ~v" v v*)))
              v*)))))))

(define build-chaperone-contract-property
  (build-property (compose make-chaperone-contract-property make-contract-property)
                  'anonymous-chaperone-contract
                  chaperone-projection-wrapper))

(define (get-any? c) any?)
(define (any? x) #t)

(define (weakest a b) #f)

(define ((get-first-order-projection get-name get-first-order) c)
  (first-order-projection (get-name c) (get-first-order c)))

(define (((first-order-projection name first-order) b) x)
  (if (first-order x)
    x
    (raise-blame-error b x "expected <~a>, given: ~e" name x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Simple Contract Construction
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct make-contract [ name first-order projection stronger ]
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:name (lambda (c) (make-contract-name c))
   #:first-order (lambda (c) (make-contract-first-order c))
   #:projection (lambda (c) (make-contract-projection c))
   #:stronger (lambda (a b) ((make-contract-stronger a) a b))
   #:generator #f))

(define-struct make-chaperone-contract [ name first-order projection stronger ]
  #:omit-define-syntaxes
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:name (lambda (c) (make-chaperone-contract-name c))
   #:first-order (lambda (c) (make-chaperone-contract-first-order c))
   #:projection (lambda (c) (make-chaperone-contract-projection c))
   #:stronger (lambda (a b) ((make-chaperone-contract-stronger a) a b))
   #:generator #f))

(define-struct make-flat-contract [ name first-order projection stronger ]
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (lambda (c) (make-flat-contract-name c))
   #:first-order (lambda (c) (make-flat-contract-first-order c))
   #:projection (lambda (c) (make-flat-contract-projection c))
   #:stronger (lambda (a b) ((make-flat-contract-stronger a) a b))
   #:generator #f))

(define ((build-contract mk default-name)
         #:name [name #f]
         #:first-order [first-order #f]
         #:projection [projection #f]
         #:stronger [stronger #f])

  (let* ([name (or name default-name)]
         [first-order (or first-order any?)]
         [projection (or projection (first-order-projection name first-order))]
         [stronger (or stronger as-strong?)])

    (mk name first-order projection stronger)))

(define (as-strong? a b)
  (procedure-closure-contents-eq?
   (contract-struct-projection a)
   (contract-struct-projection b)))

(define make-contract
  (build-contract make-make-contract 'anonymous-contract))

(define make-chaperone-contract
  (build-contract make-make-chaperone-contract 'anonymous-chaperone-contract))

(define make-flat-contract
  (build-contract make-make-flat-contract 'anonymous-flat-contract))
