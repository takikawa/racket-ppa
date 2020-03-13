#lang racket/base
(require racket/match
         macro-debugger/model/deriv
         macro-debugger/model/deriv-util
         racket/struct
         "util.rkt")
(provide deriv->refs)

;; ========

;; phase : (parameterof nat)
(define phase (make-parameter 0))
(define (add-disappeared-uses?) #t)

;; ========

;; deriv->refs : *Deriv* -> Refs
;; *Deriv* = Deriv | LDeriv | BRule | ModRule | ... (anything from deriv.rkt)
(define (deriv->refs deriv0)

  ;; refs : (listof Refs), mutable
  (define refs null)

  (define (recur . args)
    (let check ([arg args])
      (cond [(syntax? arg) (error 'deriv->refs "internal error on ~s" arg)]
            [(list? arg) (for-each check arg)]
            [else (void)]))
    (for ([arg (in-list args)])
      (if (list? arg)
          (apply recur arg)
          (analyze-deriv arg))))
  (define (recur/phase-up . args)
    (parameterize ((phase (add1 (phase))))
      (apply recur args)))
  (define (add-refs! rs)
    (set! refs (append rs refs)))
  (define (add! ids [mode 'reference])
    (let ([p (phase)])
      (add-refs! (for/list ([id (in-list ids)])
                   (ref p id mode (identifier-binding id p))))))
  (define (add/binding! id binding mode)
    (add-refs! (list (ref (phase) id mode binding))))

  ;; analyze/quote-syntax : stx -> void
  ;; Current approach: estimate that an identifier in a syntax template
  ;; may be used at (sub1 (phase)) or (phase).
  ;; FIXME: Allow for more conservative choices, too.
  ;; FIXME: #%top, #%app, #%datum, etc?
  ;; FIXME: Track tentative (in quote-syntax) references separately?
  (define (analyze/quote-syntax qs-stx)
    (let ([phases (for/list ([offset '(0 1 -1 2 -2)]) (+ (phase) offset))]
          [stx (syntax-case qs-stx ()
                 [(_quote-syntax x) #'x]
                 [(_quote-syntax x #:local) #'x])])
      (define (add*! id)
        (add-refs! (for/list ([p (in-list phases)])
                     (ref p id 'quote-syntax (identifier-binding id p)))))
      (let loop ([stx stx])
        (let ([d (if (syntax? stx) (syntax-e stx) stx)])
          (cond [(identifier? stx) (add*! stx)]
                [(pair? d)
                 (loop (car d))
                 (loop (cdr d))]
                [(vector? d)
                 (map loop (vector->list d))]
                [(prefab-struct-key d)
                 (map loop (struct->list d))]
                [(box? d)
                 (loop (unbox d))]
                [else
                 (void)])))))

  (define (analyze-deriv deriv)
    ;; Handle common base (ie, resolves) part of derivs, if applicable
    (match deriv
      [(base z1 z2 resolves da ?1)
       (add! resolves)
       (when (and (syntax? z2) (add-disappeared-uses?))
         (let ([uses (syntax-property z2 'disappeared-use)])
           (add! (let loop ([x uses] [onto null])
                   (cond [(identifier? x) (cons x onto)]
                         [(pair? x) (loop (car x) (loop (cdr x) onto))]
                         [else onto]))
                 'disappeared-use)))]
      [_
       (void)])
    ;; Handle individual variants
    (match deriv
      ;; Special cases
      [(local-value name ?1 resolves bound? binding)
       #|
       Beware: in one common case, local-member-name, the binding of name is
       mutated (because used as binder in class body), so original binding is lost!
       Use binding instead.
       |#
       (when (and bound? (pair? binding))
         (add/binding! name binding 'syntax-local-value))]
      [(p:set! _ _ _ _ _ id-resolves ?2 rhs)
       (add! id-resolves)
       (recur rhs)]
      [(p:quote-syntax z1 z2 _ _ _)
       (when z2 (analyze/quote-syntax z2))]
      ;; Otherwise, recur through children
      [deriv (for-subnodes deriv #:recur recur #:recur/phase-up recur/phase-up)]))

  (analyze-deriv deriv0)
  refs)
