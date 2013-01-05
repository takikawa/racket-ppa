#lang racket

;; Random testing of type preservation for reals.

(require redex
         racket/flonum racket/unsafe/ops
         racket/sandbox racket/cmdline
         "random-real.rkt")

(require typed-racket/utils/utils
         (typecheck typechecker)
         (utils tc-utils)
         (types subtype utils))

(require (prefix-in b: (base-env base-env))
         (prefix-in n: (base-env base-env-numeric)))

(b:init) (n:init)
(define-namespace-anchor anch)

(define-language tr-arith
  [n real]
  ;; randomly generate F, not E, because literal numbers self-evaluate
  ;; (i.e. generates a useless test)
  [E n F]
  #;; racket/math
  [F (degrees->radians E)
     (radians->degrees E)
     (exact-round E)
     (exact-floor E)
     (exact-ceiling E)
     (exact-truncate E)
     (sinh E)
     (cosh E)
     (tanh E)
     (nan? E)
     (infinite? E)
     ]
  ;; racket/base, racket/flonum, racket/unsafe/ops
  [F (* E)
     (* E E)
     (* E E E)
     (+ E)
     (+ E E)
     (+ E E E)
     (- E)
     (- E E)
     (- E E E)
     (/ E)
     (/ E E)
     (/ E E E)
     (max E)
     (max E E)
     (max E E E)
     (min E)
     (min E E)
     (min E E E)
     (add1 E)
     (sub1 E)
     (abs E)
     (floor E)
     (ceiling E)
     (truncate E)
     (round E)
     (sqrt E)
     (log E)
     (exp E)
     (cos E)
     (sin E)
     (tan E)
     (sqr E)
     (flabs E)
     (flround E)
     (flfloor E)
     (flceiling E)
     (fltruncate E)
     (flsin E)
     (flcos E)
     (fltan E)
     (flatan E)
     (flasin E)
     (flacos E)
     (fllog E)
     (flexp E)
     (flsqrt E)
     (unsafe-flabs E)
     (unsafe-flmin E E)
     (unsafe-flmax E E)
     (unsafe-flsqrt E)
     (fl+ E E)
     (fl- E E)
     (fl* E E)
     (fl/ E E)
     (flmin E E)
     (flmax E E)
     (flexpt E E)
     (unsafe-fl+ E E)
     (unsafe-fl- E E)
     (unsafe-fl* E E)
     (unsafe-fl/ E E)
     ])
;; generated from: (map car (file->list "base-env-parts"))

(define (get-type e [typecheck (compose tc-expr expand)])
  (parameterize ([delay-errors? #f]
                 [current-namespace (namespace-anchor->namespace anch)]
                 [orig-module-stx (quote-syntax e)])
    (typecheck (datum->syntax #'here e))))

(define (right-type? before)
  (define type-before (match (get-type before) [(tc-result1: b) b]))
  (define after (with-handlers ([values values])
                  (eval before (namespace-anchor->namespace anch))))
  (cond [(exn? after)  #t]
        [else
         (define type-after (get-type after tc-literal))
         (define subtype? (subtype type-after type-before))
         (unless subtype?
           (printf "type-before = ~v~ntype-after = ~v~n" type-before type-after))
         subtype?]))

;; Redex can't generate reals, so we convert ints to reals.
(define (exp->real-exp E) ; numbers or symbols or lists
  (cond [(number? E)
         (random-integer->random-real (exact-round E))]
        [(list? E)
         (map exp->real-exp E)]
        [else
         E]))

(define num-exceptions 0)

(define (mk-eval lang)
  (call-with-trusted-sandbox-configuration
   (λ () (make-evaluator lang #:requires '(racket/flonum racket/unsafe/ops)))))
(define racket-eval (mk-eval 'racket))
(define tr-eval     (mk-eval 'typed/racket))

(define (check-all-reals sexp)
  (or (with-handlers
          ;; something went wrong, almost certainly typechecking failed
          ;; in which case we ignore the expression
          ([exn?  (λ (e)
                    (set! num-exceptions (+ num-exceptions 1))
                    #t)])
        (get-type sexp)
        #f) ; go on and check preservation
      (and (right-type? sexp)
           ;; do we get the same result with and without optimization?
           (let ()
             (define racket-failed?  #f)
             (define both-failed?    #f)
             (define racket-result
               (with-handlers
                   ;; something went wrong, e.g. division by zero
                   ;; TR must fail too
                   ([exn? (λ (e) (set! racket-failed? #t))])
                 (racket-eval sexp)))
             (define tr-result
               (with-handlers
                   ;; did Racket error too?
                   ([exn? (λ (e) (when racket-failed?
                                   (set! both-failed? #t)))])
                 (tr-eval sexp)))
             (or both-failed?
                 (and (not racket-failed?)
                      (or (=      racket-result tr-result)
                          ;; for NaN, which is not = to itself
                          (equal? racket-result tr-result))))))))

(define n-attempts 1000)
(command-line
 #:once-each
 [("-n") n "Number of attempts" (set! n-attempts (string->number n))])

(call-with-limits
 #f 1000
 (lambda ()
   (redex-check tr-arith F (check-all-reals (term F))
                #:attempts n-attempts
                #:prepare exp->real-exp)))

;(printf "bad tests (usually typechecking failed): ~v~n" num-exceptions)
