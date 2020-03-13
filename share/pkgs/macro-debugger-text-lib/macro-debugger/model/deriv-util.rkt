#lang racket/base
(require (for-syntax racket/base racket/struct-info)
         racket/match
         "deriv.rkt")

(provide make
         ok-node?
         interrupted-node?
         wderiv-e1
         wderiv-e2
         wlderiv-es1
         wlderiv-es2
         wbderiv-es1
         wbderiv-es2
         wderivlist-es2
         for-subnodes)

;; ----

(define (check sym pred type x)
  (unless (pred x)
    (raise-type-error sym type x)))

(define (ok-node? x)
  (check 'ok-node? node? "node" x)
  (and (node-z2 x) #t))
(define (interrupted-node? x)
  (check 'interrupted-node? node? "node" x)
  (not (node-z2 x)))


(define (wderiv-e1 x)
  (check 'wderiv-e1 deriv? "deriv" x)
  (node-z1 x))
(define (wderiv-e2 x)
  (check 'wderiv-e2 deriv? "deriv" x)
  (node-z2 x))

(define (wlderiv-es1 x)
  (check 'wlderiv-es1 lderiv? "lderiv" x)
  (node-z1 x))
(define (wlderiv-es2 x)
  (check 'wlderiv-es2 lderiv? "lderiv" x)
  (node-z2 x))

(define (wbderiv-es1 x)
  (check 'wbderiv-es1 bderiv? "bderiv" x)
  (node-z1 x))
(define (wbderiv-es2 x)
  (check 'wbderiv-es2 bderiv? "bderiv" x))

;; wderivlist-es2 : (list-of WDeriv) -> (list-of Stx)/#f
(define (wderivlist-es2 xs)
  (let ([es2 (map wderiv-e2 xs)])
    (and (andmap syntax? es2) es2)))

;; get-struct-info : identifier stx -> struct-info-list
(define-for-syntax (get-struct-info id ctx)
  (define (bad-struct-name x)
    (raise-syntax-error #f "expected struct name" ctx x))
  (unless (identifier? id)
    (bad-struct-name id))
  (let ([value (syntax-local-value id (lambda () #f))])
    (unless (struct-info? value)
      (bad-struct-name id))
    (extract-struct-info value)))

;; (make struct-name field-expr ...)
;; Checks that correct number of fields given.
(define-syntax (make stx)
  (syntax-case stx ()
    [(make S expr ...)
     (let ()
       (define info (get-struct-info #'S stx))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(expr ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s, got ~s)"
                    (syntax-e #'S)
                    num-slots
                    num-provided)
            stx)))
       (with-syntax ([constructor constructor])
         (syntax-property #'(constructor expr ...)
                          'disappeared-use
                          #'S)))]))

;; for-subnodes : X (X -> Void) -> Void
;; where X is one of the structs listed in deriv-c.rkt; includes some non-Node structs
(define (for-subnodes x #:recur recur1 #:recur/phase-up [recur1/phase-up recur1])
  (define (recur* node)
    (if (list? node) (for-each recur* node) (recur1 node)))
  (define (recur*/phase-up nodes)
    (if (list? nodes) (for-each recur*/phase-up nodes) (recur1/phase-up nodes)))
  (define (recur . nodes) (recur* nodes))
  (define (recur/phase-up . nodes) (recur*/phase-up nodes))
  ;; Handle variants
  (match x
    [(ecte z1 z2 locals first second locals2)
     (recur locals first second locals2)]
    [(lift-deriv z1 z2 first lift-stx second)
     (recur first second)]
    [(lift/let-deriv z1 z2 first lift-stx second)
     (recur first second)]
    [(tagrule z1 z2 untagged-stx tagged-stx next)
     (recur next)]
    [(mrule z1 z2 rs da ?1 me1 locals me2 ?2 etx retx next)
     (recur locals next)]
    [(local-exn exn) (void)]
    [(local-expansion z1 z2 for-stx? me1 inner lifted me2 opaque)
     (if for-stx? (recur/phase-up inner) (recur inner))]
    [(local-lift-expr ids orig renamed) (void)]
    [(local-lift-end orig renamed wrapped) (void)]
    [(local-lift-require req expr mexpr) (void)]
    [(local-lift-provide prov) (void)]
    [(local-lift-module orig renamed) (void)]
    [(local-bind names ?1 renames bindrhs)
     (recur bindrhs)]
    [(local-value name ?1 resolves bound? binding) (void)]
    [(track-syntax op new-stx old-stx) (void)]
    [(local-remark contents) (void)]
    [(p:variable z1 z2 rs da ?1) (void)]
    [(p:module z1 z2 rs de1 ?1 prep rename ensure-mb body shift)
     (recur prep ensure-mb body)]
    [(mod:ensure-mb track1 check add-mb track2)
     (recur check add-mb)]
    [(mod:add-mb ?1 tag track check ?2)
     (recur check)]
    [(p:#%module-begin z1 z2 rs da ?1 me pass12 ?2 pass3 ?3 pass4)
     (recur pass12 pass3 pass4)]
    [(p:define-syntaxes z1 z2 rs da ?1 prep rhs locals)
     (recur prep locals)
     (recur/phase-up rhs)]
    [(p:define-values z1 z2 rs da ?1 rhs)
     (recur rhs)]
    [(p:begin-for-syntax z1 z2 rs da ?1 prep body locals)
     (recur prep locals)
     (recur/phase-up body)]
    [(p:#%expression z1 z2 rs da ?1 inner untag)
     (recur inner)]
    [(p:if z1 z2 rs da ?1 test then else)
     (recur test then else)]
    [(p:wcm z1 z2 rs da ?1 key mark body)
     (recur key mark body)]
    [(p:set! _ _ _ _ _ id-resolves ?2 rhs)
     (recur rhs)]
    [(p:set!-macro _ _ _ _ _ deriv)
     (recur deriv)]
    [(p:#%app _ _ _ _ _ derivs)
     (recur derivs)]
    [(p:begin _ _ _ _ _ derivs)
     (recur derivs)]
    [(p:begin0 _ _ _ _ _ derivs)
     (recur derivs)]
    [(p:lambda _ _ _ _ _ renames body)
     (recur body)]
    [(p:case-lambda _ _ _ _ _ renames+bodies)
     (recur renames+bodies)]
    [(p:let-values _ _ _ _ _ renames rhss body)
     (recur rhss body)]
    [(p:letrec-values _ _ _ _ _ renames rhss body)
     (recur rhss body)]
    [(p:letrec-syntaxes+values _ _ _ _ _ srenames prep sbindrhss vrhss body)
     (recur prep sbindrhss vrhss body)]
    [(p:provide _ _ _ _ _ inners ?2)
     (recur inners)]
    [(p:require _ _ _ _ _ locals)
     (recur locals)]
    [(p:submodule _ _ _ _ _ exp locals)
     (recur exp)]
    [(p:submodule* _ _ _ _ _ exp locals)
     (recur exp locals)]
    [(p:#%stratified-body _ _ _ _ _ bderiv)
     (recur bderiv)]
    [(? p::STOP?) (void)]
    [(p:declare _ _ _ _ _) (void)]
    [(lderiv _ _ ?1 derivs)
     (recur derivs)]
    [(bderiv _ _ _ pass1 pass2)
     (recur pass1 pass2)]
    [(block:letrec _ rhss body)
     (recur rhss body)]
    [(b:error ?1) (void)]
    [(b:expr head)
     (recur head)]
    [(b:splice head _ ?1 tail ?2)
     (recur head)]
    [(b:defvals head _ ?1 rename ?2)
     (recur head)]
    [(b:defstx head _ ?1 rename ?2 prep bindrhs)
     (recur head prep bindrhs)]
    [(bind-syntaxes rhs locals)
     (recur/phase-up rhs)
     (recur locals)]
    [(clc ?1 renames body)
     (recur body)]
    [(mod:pass-1-and-2 pass1 pass2)
     (recur pass1 pass2)]
    [(modp1:prim head prim)
     (recur head prim)]
    [(modp1:lift head _ _ _ mods)
     (recur head mods)]
    [(modp1:splice _ _ _) (void)]
    [(modp2:skip) (void)]
    [(modp2:cons deriv locals)
     (recur deriv locals)]
    [(modp2:lift deriv locals _ _ _ mods defs)
     (recur deriv locals mods defs)]
    [(mod:lift-end tail) (void)]
    [(modp34:bfs derivs)
     (recur derivs)]
    [(bfs:lift lderiv lifts)
     (recur lderiv)]
    [#f (void)]))
