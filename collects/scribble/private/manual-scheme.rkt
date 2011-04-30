#lang racket/base
(require "../decode.ss"
         "../struct.ss"
         "../scheme.ss"
         "../search.ss"
         "../basic.ss"
         racket/list
         "manual-utils.ss"
         "manual-style.ss"
         (for-syntax racket/base)
         (for-label racket/base))

(provide racketblock RACKETBLOCK racketblock/form
         racketblock0 RACKETBLOCK0 racketblock0/form
         racketresultblock racketresultblock0
         RACKETRESULTBLOCK RACKETRESULTBLOCK0
         racketblockelem
         racketinput RACKETINPUT 
         racketinput0 RACKETINPUT0
         racketmod 
         racketmod0
         racket RACKET racket/form racketresult racketid 
         racketmodname
         racketmodlink indexed-racket
         racketlink
         
         (rename-out [racketblock schemeblock]
                     [RACKETBLOCK SCHEMEBLOCK]
                     [racketblock/form schemeblock/form]
                     [racketblock0 schemeblock0]
                     [RACKETBLOCK0 SCHEMEBLOCK0]
                     [racketblock0/form schemeblock0/form]
                     [racketblockelem schemeblockelem]
                     [racketinput schemeinput]
                     [racketmod schememod]
                     [racket scheme]
                     [RACKET SCHEME]
                     [racket/form scheme/form]
                     [racketresult schemeresult]
                     [racketid schemeid]
                     [racketmodname schememodname]
                     [racketmodlink schememodlink]
                     [indexed-racket indexed-scheme]
                     [racketlink schemelink]))

(define-code racketblock0 to-paragraph)
(define-code racketblock to-block-paragraph)
(define-code RACKETBLOCK to-block-paragraph UNSYNTAX)
(define-code RACKETBLOCK0 to-paragraph UNSYNTAX)

(define (code-inset b)
  (make-blockquote 'code-inset (list b)))

(define (to-block-paragraph v)
  (code-inset (to-paragraph v)))

(define (to-result-paragraph v)
  (to-paragraph v 
                #:color? #f 
                #:wrap-elem
                (lambda (e) (make-element result-color e))))
(define (to-result-paragraph/prefix a b c)
  (let ([to-paragraph (to-paragraph/prefix a b c)])
    (lambda (v)
      (to-paragraph v 
                    #:color? #f 
                    #:wrap-elem
                    (lambda (e) (make-element result-color e))))))

(define-code racketresultblock0 to-result-paragraph)
(define-code racketresultblock (to-result-paragraph/prefix (hspace 2) (hspace 2) ""))
(define-code RACKETRESULTBLOCK (to-result-paragraph/prefix (hspace 2) (hspace 2) "")
  UNSYNTAX)
(define-code RACKETRESULTBLOCK0 to-result-paragraph UNSYNTAX)

(define interaction-prompt (make-element 'tt (list "> " )))
(define-code racketinput to-input-paragraph/inset)
(define-code RACKETINPUT to-input-paragraph/inset)
(define-code racketinput0 to-input-paragraph)
(define-code RACKETINPUT0 to-input-paragraph)

(define to-input-paragraph
  (to-paragraph/prefix
   (make-element #f interaction-prompt)
   (hspace 2)
   ""))
  
(define to-input-paragraph/inset
  (lambda (v)
    (code-inset (to-input-paragraph v))))

(define-syntax (racketmod0 stx)
  (syntax-case stx ()
    [(_ #:file filename lang rest ...)
     (with-syntax ([modtag (datum->syntax
                            #'here
                            `(unsyntax (make-element
                                        #f
                                        (list (hash-lang)
                                              spacer
                                              ,(if (identifier? #'lang)
                                                   `(as-modname-link
                                                     ',#'lang
                                                     (to-element ',#'lang))
                                                   #'(racket lang)))))
                            #'lang)])
       (if (syntax-e #'filename)
           (quasisyntax/loc stx
             (filebox
              filename
              #,(syntax/loc stx (racketblock0 modtag rest ...))))
           (syntax/loc stx (racketblock0 modtag rest ...))))]
    [(_ lang rest ...)
     (syntax/loc stx (racketmod0 #:file #f lang rest ...))]))

(define-syntax-rule (racketmod rest ...)
  (code-inset (racketmod0 rest ...)))

(define (to-element/result s)
  (make-element result-color (list (to-element/no-color s))))
(define (to-element/id s)
  (make-element symbol-color (list (to-element/no-color s))))

(define-syntax (keep-s-expr stx)
  (syntax-case stx ()
    [(_ ctx s srcloc)
     (let ([sv (syntax-e
                (syntax-case #'s (quote)
                  [(quote s) #'s]
                  [_ #'s]))])
       (if (or (number? sv)
               (boolean? sv)
               (and (pair? sv)
                    (identifier? (car sv))
                    (or (free-identifier=? #'cons (car sv))
                        (free-identifier=? #'list (car sv)))))
         ;; We know that the context is irrelvant
         #'s
         ;; Context may be relevant:
         #'(*keep-s-expr s ctx)))]))
(define (*keep-s-expr s ctx)
  (if (symbol? s)
    (make-just-context s ctx)
    s))

(define (add-sq-prop s name val)
  (if (eq? name 'paren-shape)
    (make-shaped-parens s val)
    s))

(define-code racketblockelem to-element)

(define-code racket to-element unsyntax keep-s-expr add-sq-prop)
(define-code RACKET to-element UNSYNTAX keep-s-expr add-sq-prop)
(define-code racketresult to-element/result unsyntax keep-s-expr add-sq-prop)
(define-code racketid to-element/id unsyntax keep-s-expr add-sq-prop)
(define-code *racketmodname to-element unsyntax keep-s-expr add-sq-prop)

(define-syntax racketmodname
  (syntax-rules (unsyntax)
    [(racketmodname #,n)
     (let ([sym n])
       (as-modname-link sym (to-element sym)))]
    [(racketmodname n)
     (as-modname-link 'n (*racketmodname n))]))

(define-syntax racketmodlink
  (syntax-rules (unsyntax)
    [(racketmodlink n content ...)
     (*as-modname-link 'n (elem #:style #f content ...))]))

(define (as-modname-link s e)
  (if (symbol? s)
      (*as-modname-link s e)
      e))

(define (*as-modname-link s e)
  (make-link-element module-link-color
                     (list e)
                     `(mod-path ,(format "~s" s))))

(define-syntax-rule (indexed-racket x)
  (add-racket-index 'x (racket x)))

(define (add-racket-index s e)
  (let ([k (cond [(and (pair? s) (eq? (car s) 'quote)) (format "~s" (cadr s))]
                 [(string? s) s]
                 [else (format "~s" s)])])
    (index* (list k) (list e) e)))

(define-syntax-rule (define-/form id base)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ a)
       (with-syntax ([ellipses (datum->syntax #'a '(... ...))])
         #'(let ([ellipses #f])
             (base a)))])))

(define-/form racketblock0/form racketblock0)
(define-/form racketblock/form racketblock)
(define-/form racket/form racket)

(define (*racketlink stx-id id . s)
  (let ([content (decode-content s)])
    (make-delayed-element
     (lambda (r p ri)
       (list
        (make-link-element
         #f
         content
         (or (find-racket-tag p ri stx-id #f)
             `(undef ,(format "--UNDEFINED:~a--" (syntax-e stx-id)))))))
     (lambda () content)
     (lambda () content))))

(define-syntax-rule (racketlink id . content)
  (*racketlink (quote-syntax id) 'id . content))

