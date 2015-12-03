#lang racket/base
(require (for-syntax racket/base
                     syntax/define
                     syntax/boundmap
                     racket/pretty))

(provide define-package
         package-begin

         open-package
         open*-package

         define*
         define*-values
         define*-syntax
         define*-syntaxes

         (for-syntax package?
                     package-exported-identifiers
                     package-original-identifiers))


;; For permission to move scopes from a definition in a package
;; to a binding of the identifier when the package is opened:
(define-for-syntax code-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

;; ----------------------------------------

(begin-for-syntax
  (struct package (root-id sig-ids impl-ids))

  (define (get-package who id)
    (let ([p (syntax-local-value id (lambda () #f))])
      (unless (package? p)
        (error who
               "not defined as a package\n  identifier: ~a"
               id))))

  (define (package-exported-identifiers id)
    (define p (get-package 'package-exported-identifiers id))
    (map
     (lambda (sig-id)
       (make-syntax-delta-introducer sig-id (package-root-id p))
       (datum->syntax id (syntax-e sig-id) sig-id sig-id))
     (syntax->list (package-sig-ids p))))
  
  (define (package-original-identifiers id)
    (define p (get-package 'package-original-identifiers id))
    (syntax->list (package-impl-ids p))))

(define-syntax (define-package stx)
  (check-definition-context stx)
  (syntax-case stx ()
    [(_ id . _)
     (let ([id #'id])
       (unless (identifier? id)
         (raise-syntax-error #f
                             "expected an identifier for the package name"
                             stx
                             id))
       (define (accumulate exports forms)
         (define intro (make-syntax-introducer))
         #`(drive-top-level
            (accumulate-package #,id #,(intro id) #,(intro id) #f #,stx
                                #,exports
                                ()
                                #,(intro forms))))
       (syntax-case stx ()
         [(_ _ #:only (export-id ...) form ...)
          (accumulate #'(#:only (export-id ...)) #'(form ...))]
         [(_ _ #:all-defined-except (export-id ...) form ...)
          (accumulate #'(#:all-defined-except (export-id ...)) #'(form ...))]
         [(_ _ #:all-defined form ...)
          (accumulate #'(#:all-defined-except ()) #'(form ...))]
         [(_ _ (export-id ...) form ...)
          (accumulate #'(#:only (export-id ...)) #'(form ...))]))]))

(define-syntax (accumulate-package stx)
  (syntax-case stx ()
    [(_ id intro-id star-id any-stars? orig-form exports defined-ids (form . forms))
     (let ([exp-form (local-expand #'form
                                   (list (gensym))
                                   (list #'define-values
                                         #'-define*-values
                                         #'define-syntaxes
                                         #'-define*-syntaxes
                                         #'begin)
                                   #f)])
       (syntax-case exp-form (begin)
         [(begin seq-form ...)
          #`(accumulate-package id intro-id star-id any-stars? orig-form
                                exports
                                defined-ids
                                (seq-form ... . forms))]
         [(def (new-def-id ...) rhs)
          (or (free-identifier=? #'def #'define-values)
              (free-identifier=? #'def #'define-syntaxes)
              (free-identifier=? #'def #'-define*-values)
              (free-identifier=? #'def #'-define*-syntaxes))
          (let* ([star? (or (free-identifier=? #'def #'-define*-values)
                            (free-identifier=? #'def #'-define*-syntaxes))]
                 [next-intro (if star?
                                 (make-syntax-introducer)
                                 (lambda (s) s))]
                 [exp-form
                  (with-syntax ([(new-def-id ...) (if star?
                                                      ;; Add another scope layer:
                                                      (next-intro #'(new-def-id ...))
                                                      ;; Remove star layers:
                                                      ((make-syntax-delta-introducer #'star-id #'intro-id)
                                                       #'(new-def-id ...)
                                                       'remove))])
                    (syntax/loc exp-form
                      (def (new-def-id ...) rhs)))])
            (with-syntax ([(_ (new-def-id ...) _) exp-form]) ; sync with above adjustments to `new-def-id`
              (when (and (not star?)
                         (syntax-e #'any-stars?))
                ;; Make sure that a name is not defined with `define` if
                ;; there's a preceeding `define*`
                (let ([intro (make-syntax-delta-introducer #'star-id #'intro-id)])
                  (for ([id (in-list (syntax->list #'(new-def-id ...)))])
                    (unless (free-identifier=? id (intro id))
                      (raise-syntax-error #f
                                          "duplicate definition for identifier"
                                          #'orig-form
                                          id)))))
              ;; Let definition out of `accumulate-package` form, accumulate new
              ;; defintions, and continue with the rest of the package body:
              (with-syntax ([forms (next-intro #'forms)]
                            [star-id (next-intro #'star-id)]
                            [any-stars? (or star? (syntax-e #'any-stars?))])
                #`(begin
                    #,exp-form
                    (accumulate-package id intro-id star-id any-stars? orig-form
                                        exports
                                        (new-def-id ... . defined-ids)
                                        forms)))))]
         [_
          (and (not (syntax-e #'id))
               (null? (syntax-e #'forms)))
          ;; Allow last expression to produce a result for `package-begin`
          exp-form]
         [_
          #`(begin
              (begin0 (void) #,exp-form)
              (accumulate-package id intro-id star-id any-stars? orig-form
                                  exports
                                  defined-ids
                                  forms))]))]
    [(_ #f #f #f _ orig-form exports defined-ids ())
     ;; Last thing in `begin-package` was a definition; add a `(void)`
     #'(void)]
    [(_ id intro-id star-id any-stars? orig-form exports defined-ids ())
     (let ()
       (define (find-ids ids keep?)
         (define intro (make-syntax-delta-introducer #'star-id #'id))
         (let ([ids (syntax->list ids)]
               [defined-ids (syntax->list #'defined-ids)])
           (define defined-bindings (make-bound-identifier-mapping))
           ;; `defined-ids` were accumulated in reverse order; add them
           ;; in the original order, so that we end up with the last
           ;; definition of each equilavent id (in the case of `define*`s
           (for-each
            (lambda (defined-id)
              (bound-identifier-mapping-put! defined-bindings
                                             (syntax-local-identifier-as-binding
                                              (intro defined-id 'remove))
                                             defined-id))
            (reverse defined-ids))
           ;; Check that each explicitly named `id` is defined:
           (define mentioned-ids (make-bound-identifier-mapping))
           (for-each (lambda (id)
                       (define bind-id (syntax-local-identifier-as-binding
                                        id))
                       (unless (bound-identifier-mapping-get defined-bindings
                                                             bind-id
                                                             (lambda () #f))
                         (raise-syntax-error #f
                                             "identifier not defined within the package"
                                             #'orig-form
                                             id))
                       (bound-identifier-mapping-put! mentioned-ids
                                                      bind-id
                                                      #t))
                     ids)
           ;; Get identifiers that should be exported:
           (filter
            values
            (bound-identifier-mapping-map
             defined-bindings
             (lambda (bind-id defined-id)
               (and (keep? (bound-identifier-mapping-get mentioned-ids bind-id
                                                         (lambda () #f)))
                    (cons bind-id
                          defined-id)))))))
       (define mapping
         (syntax-case #'exports ()
           [(#:only (id ...))
            (find-ids #'(id ...) values)]
           [(#:all-defined-except (id ...))
            (find-ids #'(id ...) not)]))
       (cond
        [(not (syntax-e #'id))
         #'(begin)]
        [else
         #`(define-syntax id (package (quote-syntax star-id)
                                      (quote-syntax #,(map car mapping))
                                      (quote-syntax #,(map cdr mapping))))]))]))

(define-for-syntax (do-open-package stx def-stxes)
  (check-definition-context stx)
  (syntax-case stx ()
    [(_ id)
     (let ([id #'id])
       (unless (identifier? id)
         (raise-syntax-error #f
                             "not an identifier for a package to open"
                             stx
                             id))
       (let ([p (syntax-local-value id (lambda () #f))])
         (unless (package? p)
           (raise-syntax-error #f
                               "not defined as a package"
                               stx
                               id))
         (define (locally sig-id)
           (define local-id
             ((make-syntax-delta-introducer (syntax-disarm sig-id code-insp) (package-root-id p))
              (datum->syntax (syntax-disarm id code-insp) (syntax-e sig-id) sig-id sig-id)))
           (syntax-rearm (syntax-rearm local-id sig-id) id))
         #`(begin
             #,@(map (lambda (sig-id impl-id)
                       #`(#,def-stxes (#,(locally sig-id))
                           (make-rename-transformer (quote-syntax #,impl-id))))
                     (syntax->list (package-sig-ids p))
                     (syntax->list (syntax-local-introduce (package-impl-ids p)))))))]))

(define-syntax (open-package stx)
  (do-open-package stx #'define-syntaxes))
(define-syntax (open*-package stx)
  (do-open-package stx #'define*-syntaxes))

(define-syntax (package-begin stx)
  (if (eq? 'expression (syntax-local-context))
      #`(let () #,stx)
      (syntax-case stx ()
        [(_ form ...)
         #`(drive-top-level
            (accumulate-package #f id id #f #,stx
                                (#:only ())
                                ()
                                #,((make-syntax-introducer)
                                   #'(form ...))))])))

(define-for-syntax (check-definition-context stx)
  (when (eq? 'expression (syntax-local-context))
    (raise-syntax-error #f
                        "not in a definition context"
                        stx)))

;; ----------------------------------------

(define-syntax (drive-top-level stx)
  (syntax-case stx ()
    [(_ form)
     (cond
      [(eq? 'top-level (syntax-local-context))
       ;; In a opt-level context, we need to use the `(define-syntaxes
       ;; (...) (values))` trick to introduce all defined names before
       ;; expanding expressions.
       #'(accumulate-top-level () (form))]
      [else
       ;; No special treatment needed:
       #'form])]))

(define-syntax (accumulate-top-level stx)
  (syntax-case stx ()
    [(_ exp-forms ())
     #`(begin
         #,@(reverse (syntax->list #'exp-forms)))]
    [(_ exp-forms (form . forms))
     (let ([exp-form (local-expand #'form
                                   (list (gensym))
                                   (list #'define-values
                                         #'define-syntaxes
                                         #'begin)
                                   #f)])
       (syntax-case exp-form (begin define-values define-syntaxes)
         [(begin form ...)
          #'(accumulate-top-level exp-forms (form ... . forms))]
         [(define-values (new-def-id ...) rhs)
          #`(begin
              (define-syntaxes (new-def-id ...) (values))
              (accumulate-top-level (#,exp-form . exp-forms)
                                    forms))]
         [(define-syntaxes . _)
          #`(begin
              #,exp-form
              (accumulate-top-level exp-forms forms))]
         [_
          #`(accumulate-top-level (#,exp-form . exp-forms) forms)]))]))
  
;; ----------------------------------------

(define-for-syntax (do-define-* stx define-values-id)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (let ([ids (syntax->list #'(id ...))])
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error
                      #f
                      "expected an identifier for definition"
                      stx
                      id)))
                 ids)
       (with-syntax ([define-values define-values-id])
         (syntax/loc stx
           (define-values (id ...) rhs))))]))
(define-syntax (-define*-values stx)
  (do-define-* stx #'define-values))
(define-syntax (-define*-syntaxes stx)
  (do-define-* stx #'define-syntaxes))
(define-syntax (define*-values stx)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (syntax-property
      (syntax/loc stx (-define*-values (id ...) rhs))
      'certify-mode
      'transparent-binding)]))
(define-syntax (define*-syntaxes stx)
  (syntax-case stx ()
    [(_ (id ...) rhs)
     (syntax-property
      (syntax/loc stx (-define*-syntaxes (id ...) rhs))
      'certify-mode
      'transparent-binding)]))

(define-syntax (define* stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda)])
    (quasisyntax/loc stx
      (define*-values (#,id) #,rhs))))
(define-syntax (define*-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda)])
    (quasisyntax/loc stx
      (define*-syntaxes (#,id) #,rhs))))
