#lang scheme/base
(require (for-syntax scheme/base
                     syntax/kerncase)
         "stxparam.rkt"
         "private/stxparam.rkt"
         "private/local.rkt")

(provide splicing-let-syntax
         splicing-let-syntaxes
         splicing-letrec-syntax
         splicing-letrec-syntaxes
         splicing-let
         splicing-let-values
         splicing-letrec
         splicing-letrec-values
         splicing-letrec-syntaxes+values
         splicing-local
         splicing-syntax-parameterize)

(define-for-syntax ((check-id stx) id-stx)
  (unless (identifier? id-stx)
    (raise-syntax-error #f "expected an identifier" stx id-stx))
  (list id-stx))

(define-for-syntax ((check-ids stx) ids-stx)
  (let ([ids (syntax->list ids-stx)])
    (unless ids
      (raise-syntax-error 
       #f
       "expected a parenthesized sequence of identifiers"
       stx
       ids-stx))
    (for-each (check-id stx) ids)
    ids))

(define-for-syntax (check-dup-binding stx idss)
  (let ([dup-id (check-duplicate-identifier (apply append idss))])
    (when dup-id
      (raise-syntax-error #f "duplicate binding" stx dup-id))))

(define-for-syntax (do-let-syntax stx rec? multi? let-id def-id need-top-decl?)
  (syntax-case stx ()
    [(_ ([ids expr] ...) body ...)
     (let ([all-ids (map ((if multi? check-ids check-id) stx)
                         (syntax->list #'(ids ...)))])
       (check-dup-binding stx all-ids)
       (if (eq? 'expression (syntax-local-context))
           (with-syntax ([LET let-id])
             (syntax/loc stx
               (LET ([ids expr] ...)
                 (#%expression body)
                 ...)))
           (let ([def-ctx (syntax-local-make-definition-context)]
                 [ctx (list (gensym 'intdef))])
             (syntax-local-bind-syntaxes (apply append all-ids) #f def-ctx)
             (internal-definition-context-seal def-ctx)
             (let* ([add-context
                     (lambda (expr)
                       (let ([q (local-expand #`(quote #,expr)
                                              ctx
                                              (list #'quote)
                                              def-ctx)])
                         (syntax-case q ()
                           [(_ expr) #'expr])))])
               (with-syntax ([((id ...) ...)
                              (map (lambda (ids)
                                     (map add-context ids))
                                   all-ids)]
                             [(expr ...)
                              (let ([exprs (syntax->list #'(expr ...))])
                                (if rec?
                                    (map add-context exprs)
                                    exprs))]
                             [(body ...)
                              (map add-context (syntax->list #'(body ...)))]
                             [DEF def-id])
                 (with-syntax ([(top-decl ...)
                                (if (and need-top-decl? (equal? 'top-level (syntax-local-context)))
                                    #'((define-syntaxes (id ... ...) (values)))
                                    null)])
                   #'(begin
                       top-decl ...
                       (DEF (id ...) expr)
                       ...
                       body ...)))))))]))

(define-syntax (splicing-let-syntax stx)
  (do-let-syntax stx #f #f #'let-syntax #'define-syntaxes #f))

(define-syntax (splicing-let-syntaxes stx)
  (do-let-syntax stx #f #t #'let-syntaxes #'define-syntaxes #f))

(define-syntax (splicing-letrec-syntax stx)
  (do-let-syntax stx #t #f #'letrec-syntax #'define-syntaxes #f))

(define-syntax (splicing-letrec-syntaxes stx)
  (do-let-syntax stx #t #t #'letrec-syntaxes #'define-syntaxes #f))

(define-syntax (splicing-let stx)
  (do-let-syntax stx #f #f #'let #'define-values #f))

(define-syntax (splicing-let-values stx)
  (do-let-syntax stx #f #t #'let-values #'define-values #f))

(define-syntax (splicing-letrec stx)
  (do-let-syntax stx #t #f #'letrec #'define-values #t))

(define-syntax (splicing-letrec-values stx)
  (do-let-syntax stx #t #t #'letrec-values #'define-values #t))

;; ----------------------------------------

(define-syntax (splicing-letrec-syntaxes+values stx)
  (syntax-case stx ()
    [(_ ([sids sexpr] ...) ([vids vexpr] ...) body ...)
     (let* ([all-sids (map (check-ids stx)
                           (syntax->list #'(sids ...)))]
            [all-vids (map (check-ids stx)
                           (syntax->list #'(vids ...)))]
            [all-ids (append all-sids all-vids)])
       (check-dup-binding stx all-ids)
       (if (eq? 'expression (syntax-local-context))
           (syntax/loc stx
             (letrec-syntaxes+values ([sids sexpr] ...) ([vids vexpr] ...)
               (#%expression body) ...))
           (let ([def-ctx (syntax-local-make-definition-context)]
                 [ctx (list (gensym 'intdef))])
             (syntax-local-bind-syntaxes (apply append all-ids) #f def-ctx)
             (internal-definition-context-seal def-ctx)
             (let* ([add-context
                     (lambda (expr)
                       (let ([q (local-expand #`(quote #,expr)
                                              ctx
                                              (list #'quote)
                                              def-ctx)])
                         (syntax-case q ()
                           [(_ expr) #'expr])))]
                    [add-context-to-idss
                     (lambda (idss)
                       (map add-context idss))])
               (with-syntax ([((sid ...) ...)
                              (map add-context-to-idss all-sids)]
                             [((vid ...) ...)
                              (map add-context-to-idss all-vids)]
                             [(sexpr ...)
                              (map add-context (syntax->list #'(sexpr ...)))]
                             [(vexpr ...)
                              (map add-context (syntax->list #'(vexpr ...)))]
                             [(body ...)
                              (map add-context (syntax->list #'(body ...)))])
                 (with-syntax ([top-decl
                                (if (equal? 'top-level (syntax-local-context))
                                    #'(define-syntaxes (vid ... ...) (values))
                                    #'(begin))])
                   (syntax/loc stx
                     (begin
                       top-decl
                       (define-syntaxes (sid ...) sexpr) ...
                       (define-values (vid ...) vexpr) ...
                       body ...))))))))]))



(define-syntax (splicing-local stx)
  (do-local stx #'splicing-letrec-syntaxes+values))

;; ----------------------------------------

(define-syntax (splicing-syntax-parameterize stx)
  (if (eq? 'expression (syntax-local-context))
      ;; Splicing is no help in an expression context:
      (do-syntax-parameterize stx #'let-syntaxes #f)
      ;; Let `syntax-parameterize' check syntax, then continue
      (do-syntax-parameterize stx #'ssp-let-syntaxes #t)))

(define-syntax (ssp-let-syntaxes stx)
  (syntax-case stx ()
    [(_ ([(id) rhs] ...) body ...)
     (with-syntax ([(splicing-temp ...) (generate-temporaries #'(id ...))])
       #'(begin
           ;; Evaluate each RHS only once:
           (define-syntax splicing-temp rhs) ...
           ;; Partially expand `body' to push down `let-syntax':
           (expand-ssp-body (id ...) (splicing-temp ...) body)
           ...))]))

(define-syntax (expand-ssp-body stx)
  (syntax-case stx ()
    [(_ (sp-id ...) (temp-id ...) body)
     (let ([body (local-expand #'(letrec-syntaxes/trans ([(sp-id) (syntax-local-value (quote-syntax temp-id))]
                                                         ...)
                                    (force-expand body))
                               (syntax-local-context)
                               null ;; `force-expand' actually determines stopping places
                               #f)])
       ;; Extract expanded body out of `body':
       (syntax-case body (quote)
         [(ls _ _ (quote body))
          (let ([body #'body])
            (syntax-case body ( begin
                                define-values
                                define-syntaxes
                                define-for-syntaxes
                                #%require
                                #%provide )
              [(begin expr ...)
               (syntax/loc body
                 (begin (expand-ssp-body (sp-id ...) (temp-id ...) expr) ...))]
              [(define-values (id ...) rhs)
               (syntax/loc body
                 (define-values (id ...)
                   (letrec-syntaxes ([(sp-id) (syntax-local-value (quote-syntax temp-id))] ...)
                     rhs)))]
              [(define-syntaxes . _) body]
              [(define-for-syntaxes . _) body]
              [(#%require . _) body]
              [(#%provide . _) body]
              [expr (syntax/loc body
                      (letrec-syntaxes ([(sp-id) (syntax-local-value (quote-syntax temp-id))] ...)
                        expr))]))]))]))

(define-syntax (letrec-syntaxes/trans stx)
  (syntax-case stx ()
    [(_ bindings body)
     (syntax-property
      #'(letrec-syntaxes bindings body)
      'certify-mode
      'transparent)]))

(define-syntax (force-expand stx)
  (syntax-case stx ()
    [(_ stx)
     ;; Expand `stx' to reveal type of form, and then preserve it via
     ;; `quote':
     (syntax-property
      #`(quote #,(local-expand #'stx
                               'module
                               (kernel-form-identifier-list)
                               #f))
      'certify-mode
      'transparent)]))
