#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../basic.ss"
         "../manual-struct.ss"
         "qsloc.ss"
         "manual-utils.ss"
         "manual-vars.ss"
         "manual-style.ss"
         "manual-scheme.ss"
         "manual-bind.ss"
         "manual-method.ss"
         "manual-ex.ss"
         scheme/string
         scheme/list
         (for-syntax scheme/base)
         (for-label scheme/base))


(provide defform defform* defform/subs defform*/subs defform/none
         defidform defidform/inline
         specform specform/subs
         specsubform specsubform/subs specspecsubform specspecsubform/subs
         specsubform/inline
         defsubform defsubform*
         racketgrammar racketgrammar*
         (rename-out [racketgrammar schemegrammar]
                     [racketgrammar* schemegrammar*])
         var svar)

(define-syntax (defform*/subs stx)
  (syntax-case stx ()
    [(_ #:id defined-id #:literals (lit ...) [spec spec1 ...]
        ([non-term-id non-term-form ...] ...)
        #:contracts ([contract-nonterm contract-expr] ...)
        desc ...)
     (with-syntax ([new-spec
                    (let loop ([spec #'spec])
                      (if (and (identifier? spec)
                               (free-identifier=? spec #'defined-id))
                        (datum->syntax #'here '(unsyntax x) spec spec)
                        (syntax-case spec ()
                          [(a . b)
                           (datum->syntax spec
                                          (cons (loop #'a) (loop #'b))
                                          spec
                                          spec)]
                          [_ spec])))])
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error #f
                                         "expected an identifier for a literal"
                                         stx
                                         id)))
                 (syntax->list #'(lit ...)))
       #'(with-togetherable-scheme-variables
          (lit ...)
          ([form spec] [form spec1] ... 
           [non-term (non-term-id non-term-form ...)] ...)
          (*defforms (quote-syntax/loc defined-id)
                     '(spec spec1 ...)
                     (list (lambda (x) (schemeblock0/form new-spec))
                           (lambda (ignored) (schemeblock0/form spec1)) ...)
                     '((non-term-id non-term-form ...) ...)
                     (list (list (lambda () (scheme non-term-id))
                                 (lambda () (schemeblock0/form non-term-form))
                                 ...)
                           ...)
                     (list (list (lambda () (scheme contract-nonterm))
                                 (lambda () (schemeblock0 contract-expr)))
                           ...)
                     (lambda () (list desc ...)))))]
    [(fm #:id defined-id #:literals (lit ...) [spec spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (syntax/loc stx
       (fm #:id defined-id #:literals (lit ...) [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ()
           desc ...))]
    [(fm #:id id [spec spec1 ...] ([non-term-id non-term-form ...] ...)
         desc ...)
     (syntax/loc stx
       (fm #:id id #:literals () [spec spec1 ...]
           ([non-term-id non-term-form ...] ...)
           #:contracts ()
           desc ...))]
    [(fm #:literals lits [(spec-id . spec-rest) spec1 ...]
         ([non-term-id non-term-form ...] ...)
         desc ...)
     (with-syntax ([(_ _ _ [spec . _] . _) stx])
       (syntax/loc stx
         (fm #:id spec-id #:literals lits [spec spec1 ...]
             ([non-term-id non-term-form ...] ...)
             desc ...)))]
    [(fm [spec spec1 ...] ([non-term-id non-term-form ...] ...) desc ...)
     (syntax/loc stx
       (fm #:literals () [spec spec1 ...] ([non-term-id non-term-form ...] ...)
           desc ...))]))

(define-syntax (defform* stx)
  (syntax-case stx ()
    [(_ #:id id #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals lits [spec ...] () desc ...))]
    [(_ #:literals lits [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:literals lits [spec ...] () desc ...))]
    [(_ #:id id [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs #:id id [spec ...] () desc ...))]
    [(_ [spec ...] desc ...)
     (syntax/loc stx
       (defform*/subs [spec ...] () desc ...))]))

(define-syntax (defform stx)
  (syntax-case stx ()
    [(_ #:id id #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals (lit ...) [spec] () desc ...))]
    [(_ #:id id spec desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals () [spec] () desc ...))]
    [(_ #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (defform*/subs #:literals (lit ...) [spec] () desc ...))]
    [(_ spec desc ...)
     (syntax/loc stx
       (defform*/subs [spec] () desc ...))]))

(define-syntax (defform/subs stx)
  (syntax-case stx ()
    [(_ #:id id #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals lits [spec] subs desc ...))]
    [(_ #:id id spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:id id #:literals () [spec] subs desc ...))]
    [(_ #:literals lits spec subs desc ...)
     (syntax/loc stx
       (defform*/subs #:literals lits [spec] subs desc ...))]
    [(_ spec subs desc ...)
     (syntax/loc stx
       (defform*/subs [spec] subs desc ...))]))

(define-syntax (defform/none stx)
  (syntax-case stx ()
    [(_ #:literals (lit ...) spec #:contracts ([contract-id contract-expr] ...) desc ...)
     (begin
       (for-each (lambda (id)
                   (unless (identifier? id)
                     (raise-syntax-error #f
                                         "expected an identifier for a literal"
                                         stx
                                         id)))
                 (syntax->list #'(lit ...)))
       #'(with-togetherable-scheme-variables
          (lit ...)
          ([form/none spec])
          (*defforms #f
                     '(spec) (list (lambda (ignored) (schemeblock0/form spec)))
                     null null
                     (list (list (lambda () (scheme contract-id))
                                 (lambda () (schemeblock0 contract-expr)))
                           ...)
                     (lambda () (list desc ...)))))]
    [(fm #:literals (lit ...) spec desc ...)
     (syntax/loc stx
       (fm #:literals (lit ...) spec #:contracts () desc ...))]
    [(fm spec desc ...)
     (syntax/loc stx
       (fm #:literals () spec desc ...))]))

(define-syntax (defidform/inline stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     #'(defform-site (quote-syntax id))]))

(define-syntax (defidform stx)
  (syntax-case stx ()
    [(_ spec-id desc ...)
     #'(with-togetherable-scheme-variables
        ()
        ()
        (*defforms (quote-syntax/loc spec-id)
                   '(spec-id)
                   (list (lambda (x) (make-omitable-paragraph (list x))))
                   null
                   null
                   null
                   (lambda () (list desc ...))))]))

(define (into-blockquote s)
  (make-blockquote "leftindent"
                   (if (splice? s)
                     (flow-paragraphs (decode-flow (splice-run s)))
                     (list s))))

(define-syntax (defsubform stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform . rest))]))

(define-syntax (defsubform* stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform* . rest))]))

(define-syntax spec?form/subs
  (syntax-rules ()
    [(_ has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        #:contracts ([contract-nonterm contract-expr] ...)
        desc ...)
     (with-scheme-variables
      (lit ...)
      ([form/maybe (has-kw? spec)]
       [non-term (non-term-id non-term-form ...)] ...)
      (*specsubform 'spec '(lit ...) (lambda () (schemeblock0/form spec))
                    '((non-term-id non-term-form ...) ...)
                    (list (list (lambda () (scheme non-term-id))
                                (lambda () (schemeblock0/form non-term-form))
                                ...)
                          ...)
                    (list (list (lambda () (scheme contract-nonterm))
                                (lambda () (schemeblock0 contract-expr)))
                          ...)
                    (lambda () (list desc ...))))]
    [(_ has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs has-kw? #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     #:contracts ()
                     desc ...)]))

(define-syntax specsubform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (spec?form/subs #f #:literals (lit ...) spec () desc ...)]
    [(_ spec desc ...)
     (specsubform #:literals () spec desc ...)]))

(define-syntax specsubform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs #f #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     desc ...)]
    [(_ spec subs desc ...)
     (specsubform/subs #:literals () spec subs desc ...)]))

(define-syntax-rule (specspecsubform spec desc ...)
  (make-blockquote "leftindent" (list (specsubform spec desc ...))))

(define-syntax-rule (specspecsubform/subs spec subs desc ...)
  (make-blockquote "leftindent" (list (specsubform/subs spec subs desc ...))))

(define-syntax specform
  (syntax-rules ()
    [(_ #:literals (lit ...) spec desc ...)
     (spec?form/subs #t #:literals (lit ...) spec () desc ...)]
    [(_ spec desc ...)
     (specform #:literals () spec desc ...)]))

(define-syntax specform/subs
  (syntax-rules ()
    [(_ #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
        desc ...)
     (spec?form/subs #t #:literals (lit ...) spec ([non-term-id non-term-form ...] ...)
                     desc ...)]
    [(_ spec ([non-term-id non-term-form ...] ...) desc ...)
     (specform/subs #:literals () spec ([non-term-id non-term-form ...] ...)
                    desc ...)]))

(define-syntax-rule (specsubform/inline spec desc ...)
  (with-scheme-variables
   ()
   ([form/maybe (#f spec)])
   (*specsubform 'spec null #f null null null (lambda () (list desc ...)))))

(define-syntax racketgrammar
  (syntax-rules ()
    [(_ #:literals (lit ...) id clause ...)
     (with-scheme-variables
      (lit ...)
      ([non-term (id clause ...)])
      (*racketgrammar '(lit ...)
                      '(id clause ...)
                      (lambda ()
                        (list (list (scheme id)
                                    (schemeblock0/form clause) ...)))))]
    [(_ id clause ...) (racketgrammar #:literals () id clause ...)]))

(define-syntax racketgrammar*
  (syntax-rules ()
    [(_ #:literals (lit ...) [id clause ...] ...)
     (with-scheme-variables
      (lit ...)
      ([non-term (id clause ...)] ...)
      (*racketgrammar '(lit ...)
                      '(id ... clause ... ...)
                      (lambda ()
                        (list (list (scheme id) (schemeblock0/form clause) ...)
                              ...))))]
    [(_ [id clause ...] ...)
     (racketgrammar* #:literals () [id clause ...] ...)]))

(define-syntax-rule (var id)
  (*var 'id))

(define-syntax-rule (svar id)
  (*var 'id))


(define (meta-symbol? s) (memq s '(... ...+ ?)))

(define (defform-site kw-id)
  (let ([target-maker (id-to-form-target-maker kw-id #t)]
        [content (list (definition-site (syntax-e kw-id)
                         kw-id #t))])
    (if target-maker
        (target-maker
         content
         (lambda (tag)
           (make-toc-target-element
            #f
            (if kw-id
                (list (make-index-element
                       #f content tag
                       (list (symbol->string (syntax-e kw-id)))
                       content
                       (with-exporting-libraries
                        (lambda (libs)
                          (make-form-index-desc (syntax-e kw-id)
                                                libs)))))
                content)
            tag)))
        (car content))))

(define (*defforms kw-id forms form-procs subs sub-procs contract-procs content-thunk)
  (parameterize ([current-meta-list '(... ...+)])
    (make-box-splice
     (cons
      (make-table
       'boxed
       (append
        (map
         (lambda (form form-proc)
           (list
            (make-flow
             (list
              ((or form-proc
                   (lambda (x)
                     (make-omitable-paragraph
                      (list (to-element `(,x . ,(cdr form)))))))
               (and kw-id
                    (eq? form (car forms))
                    (defform-site kw-id)))))))
         forms form-procs)
        (if (null? sub-procs)
          null
          (list (list flow-empty-line)
                (list (make-flow
                       (list (let ([l (map (lambda (sub)
                                             (map (lambda (f) (f)) sub))
                                           sub-procs)])
                               (*schemerawgrammars "specgrammar"
                                                   (map car l)
                                                   (map cdr l))))))))
        (make-contracts-table contract-procs)))
      (content-thunk)))))

(define (*specsubform form lits form-thunk subs sub-procs contract-procs content-thunk)
  (parameterize ([current-meta-list '(... ...+)])
    (make-blockquote
     "leftindent"
     (cons
      (make-table
       'boxed
       (cons
        (list
         (make-flow
          (list
           (if form-thunk
             (form-thunk)
             (make-omitable-paragraph (list (to-element form)))))))
        (append
         (if (null? sub-procs)
             null
             (list (list flow-empty-line)
                   (list (make-flow
                          (list (let ([l (map (lambda (sub)
                                                (map (lambda (f) (f)) sub))
                                              sub-procs)])
                                  (*schemerawgrammars "specgrammar"
                                                      (map car l)
                                                      (map cdr l))))))))
         (make-contracts-table contract-procs))))
      (flow-paragraphs (decode-flow (content-thunk)))))))

(define (*schemerawgrammars style nonterms clauseses)
  (make-table
   `((valignment baseline baseline baseline baseline baseline)
     (alignment right left center left left)
     (style ,style))
   (cdr
    (append-map
     (lambda (nonterm clauses)
       (list*
        (list flow-empty-line flow-empty-line flow-empty-line
              flow-empty-line flow-empty-line)
        (list (to-flow nonterm) flow-empty-line (to-flow "=") flow-empty-line
              (make-flow (list (car clauses))))
        (map (lambda (clause)
               (list flow-empty-line flow-empty-line
                     (to-flow "|") flow-empty-line
                     (make-flow (list clause))))
             (cdr clauses))))
     nonterms clauseses))))

(define (*schemerawgrammar style nonterm clause1 . clauses)
  (*schemerawgrammars style (list nonterm) (list (cons clause1 clauses))))

(define (*racketgrammar lits s-expr clauseses-thunk)
  (let ([l (clauseses-thunk)])
    (*schemerawgrammars #f
                        (map (lambda (x)
                               (make-element #f
                                             (list (hspace 2)
                                                   (car x))))
                             l)
                        (map cdr l))))

(define (*var id)
  (to-element (*var-sym id)))

(define (*var-sym id)
  (string->symbol (format "_~a" id)))

(define (make-contracts-table contract-procs)
  (if (null? contract-procs)
      null
      (append
       (list (list flow-empty-line))
       (list (list (make-flow
                    (map (lambda (c)
                           (make-table
                            "argcontract"
                            (list
                             (list (to-flow (hspace 2))
                                   (to-flow ((car c)))
                                   flow-spacer
                                   (to-flow ":")
                                   flow-spacer
                                   (make-flow (list ((cadr c))))))))
                         contract-procs)))))))
