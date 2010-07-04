#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../config.ss"
         "../basic.ss"
         "../manual-struct.ss"
         "manual-ex.ss"
         scheme/string
         scheme/list
         scheme/class
         scheme/stxparam
         scheme/serialize
         setup/main-collects
         (for-syntax scheme/base
                     syntax/boundmap
                     syntax/kerncase)
         (for-label scheme/base
                    scheme/class))


(provide definition-site
         libs->taglet
         annote-exporting-library
         with-exporting-libraries
         id-to-target-maker
         id-to-form-target-maker
         *sig-elem
         (struct-out sig)
         ;; public:
         make-binding-redirect-elements
         sigelem)

(define (gen-absolute-tag)
  `(abs ,(make-generated-tag)))

(define-struct sig (id))

(define-syntax-rule (sigelem sig elem)
  (*sig-elem (quote-syntax sig) 'elem))

(define (*sig-elem sig elem)
  (let ([s (to-element/no-color elem)])
    (make-delayed-element
     (lambda (renderer sec ri)
       (let* ([tag (find-scheme-tag sec ri sig #f)]
              [taglet (and tag (append (cadr tag) (list elem)))]
              [vtag (and tag `(sig-val ,taglet))]
              [stag (and tag `(sig-form ,taglet))]
              [sd (and stag (resolve-get/tentative sec ri stag))])
         (list
          (make-element
           "schemesymbol"
           (list
            (cond [sd (make-link-element "schemesyntaxlink" (list s) stag)]
                  [vtag (make-link-element "schemevaluelink" (list s) vtag)]
                  [else s]))))))
     (lambda () s)
     (lambda () s))))

(define (annote-exporting-library e)
  (make-delayed-element
   (lambda (render p ri)
     (let ([from (resolve-get/tentative p ri '(exporting-libraries #f))])
       (if (and from (pair? from))
         (list (make-hover-element
                #f
                (list e)
                (intern-taglet
                 (string-append
                  "Provided from: "
                  (let loop ([from from])
                    (if (null? (cdr from))
                      (format "~s" (car from))
                      (format "~s, ~a" (car from) (loop (cdr from)))))))))
         (list e))))
   (lambda () e)
   (lambda () e)))

(define (get-exporting-libraries render p ri)
  (resolve-get/tentative p ri '(exporting-libraries #f)))

(define (with-exporting-libraries proc)
  (make-delayed-index-desc
   (lambda (render part ri)
     (proc (or (get-exporting-libraries render part ri) null)))))

(define (definition-site name stx-id form?)
  (let ([sig (current-signature)])
    (if sig
      (*sig-elem (sig-id sig) name)
      (annote-exporting-library
       (to-element (make-just-context name stx-id))))))

(define checkers (make-hash))

(define (libs->taglet id libs source-libs)
  (let ([lib
         (or (ormap (lambda (lib)
                      (let ([checker
                             (hash-ref
                              checkers lib
                              (lambda ()
                                (let ([ns-id 
                                       (let ([ns (make-base-empty-namespace)])
                                         (parameterize ([current-namespace ns])
                                           (namespace-require `(for-label ,lib))
                                           (namespace-syntax-introduce (datum->syntax #f 'x))))])
                                  (let ([checker
                                         (lambda (id)
                                           (free-label-identifier=?
                                            (datum->syntax ns-id (syntax-e id))
                                            id))])
                                    (hash-set! checkers lib checker)
                                    checker))))])
                        (and (checker id) lib)))
                    (or source-libs null))
             (and (pair? libs) (car libs)))])
    (and lib (module-path-index->taglet
              (module-path-index-join lib #f)))))

(define (id-to-target-maker id dep?)
  (*id-to-target-maker 'def id dep?))

(define (id-to-form-target-maker id dep?)
  (*id-to-target-maker 'form id dep?))

(define (*id-to-target-maker sym id dep?)
  (let ([sig (current-signature)])
    (lambda (content mk)
      (make-part-relative-element
       (lambda (ci)
         (let ([e (ormap (lambda (p)
                           (ormap (lambda (e)
                                    (and (exporting-libraries? e) e))
                                  (part-to-collect p)))
                         (collect-info-parents ci))])
           (unless e
             ;; Call raise-syntax-error to capture error message:
             (with-handlers ([exn:fail:syntax?
                              (lambda (exn)
                                (fprintf (current-error-port)
                                         "~a\n" (exn-message exn)))])
               (raise-syntax-error
                'WARNING
                "no declared exporting libraries for definition" id)))
           (if e
             (let* ([lib-taglet (libs->taglet
                                 (if sig (sig-id sig) id)
                                 (exporting-libraries-libs e)
                                 (exporting-libraries-source-libs e))]
                    [tag (intern-taglet
                          (list (if sig
                                  (case sym
                                    [(def) 'sig-val]
                                    [(form) 'sig-def])
                                  sym)
                                `(,lib-taglet
                                  ,@(if sig (list (syntax-e (sig-id sig))) null)
                                  ,(syntax-e id))))])
               (if (or sig (not dep?))
                 (list (mk tag))
                 (list (make-target-element
                        #f
                        (list (mk tag))
                        (intern-taglet
                         `(dep ,(list lib-taglet (syntax-e id))))))))
             content)))
       (lambda () (car content))
       (lambda () (car content))))))

(define (make-binding-redirect-elements mod-path redirects)
  (let ([taglet (module-path-index->taglet 
                 (module-path-index-join mod-path #f))])
    (make-element
     #f
     (map
      (lambda (redirect)
        (let ([id (car redirect)]
              [form? (cadr redirect)]
              [path (caddr redirect)]
              [anchor (cadddr redirect)])
          (let ([make-one
                 (lambda (kind)
                   (make-redirect-target-element
                    #f
                    null
                    (intern-taglet (list kind (list taglet id)))
                    path
                    anchor))])
            (make-element
             #f
             (list (make-one (if form? 'form 'def))
                   (make-one 'dep)
                   (make-index-element #f
                                       null
                                       (list (if form? 'form 'def)
                                             (list taglet id))
                                       (list (symbol->string id))
                                       (list
                                        (make-element
                                         "schemesymbol"
                                         (list
                                          (make-element
                                           (if form?
                                             "schemesyntaxlink"
                                             "schemevaluelink")
                                           (list (symbol->string id))))))
                                       ((if form?
                                          make-form-index-desc
                                          make-procedure-index-desc)
                                        id
                                        (list mod-path))))))))
      redirects))))
