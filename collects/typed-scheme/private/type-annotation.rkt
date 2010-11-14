#lang scheme/base

(require "../utils/utils.rkt" 
	 (rep type-rep)
	 (utils tc-utils)
	 (env global-env)
         (except-in (types subtype union convenience resolve utils) -> ->*)
         (private parse-type)
         (only-in scheme/contract listof ->)
         racket/match mzlib/trace)
(provide type-annotation
         get-type
         get-types
         get-type/infer
         type-label-symbol
         type-ascrip-symbol
         type-dotted-symbol
         type-ascription
         remove-ascription
         check-type
         dotted?)

(define type-label-symbol 'type-label)
(define type-ascrip-symbol 'type-ascription)
(define type-dotted-symbol 'type-dotted)

(define (print-size stx)
  (syntax-case stx ()
    [(a . b) (begin
               (printf/log "Annotation Sexp Pair \n")
               (print-size #'a)
               (print-size #'b))]      
    [_ (printf/log "Annotation Sexp \n")]))

;; get the type annotation of this syntax
;; syntax -> Maybe[Type]
;; is let-binding really necessary? - remember to record the bugs!
(define (type-annotation stx #:infer [let-binding #f])
  (define (pt prop)
    (when (and (identifier? stx) 
               let-binding
               (lookup-type stx (lambda () #f)))
      (maybe-finish-register-type stx)
      (tc-error/expr #:stx stx "Duplicate type annotation for ~a" (syntax-e stx)))
    (if (syntax? prop)
        (parse-type prop)
        (parse-type/id stx prop)))
  ;(unless let-binding (error 'ohno))
  ;(printf "in type-annotation:~a\n" (syntax->datum stx))
  (cond       
    [(syntax-property stx type-label-symbol) => pt]
    [(syntax-property stx type-ascrip-symbol) => pt]
    ;; this is so that : annotation works in internal def ctxts
    [(and (identifier? stx) (lookup-type stx (lambda () #f)))
     =>
     (lambda (t)
       (maybe-finish-register-type stx)
       t)]
    [else #f]))

;(trace type-annotation)

(define (type-ascription stx)
  (define (pt prop)
    (if (syntax? prop)
        (parse-tc-results prop)
        (parse-tc-results/id stx prop)))
  (cond
    [(syntax-property stx type-ascrip-symbol) 
     =>
     (lambda (prop)
       (if (pair? prop)
           (pt (car prop))
           (pt prop)))]    
    [else #f]))

(define (remove-ascription stx)
  (syntax-property stx type-ascrip-symbol 
                   (cond
                     [(syntax-property stx type-ascrip-symbol) 
                      =>
                      (lambda (prop)
                        (if (pair? prop)
                            (cdr prop)
                            #f))]
                     [else #f])))

(define (log/ann stx ty)
  (printf/log "Required Annotated Variable: ~a ~a\n" (syntax-e stx) ty))
(define (log/extra stx ty ty2)
  (printf/log "Extra Annotated Variable: ~a ~a ~a\n" (syntax-e stx) ty ty2))
(define (log/noann stx ty)
  (printf/log "Unannotated Variable: ~a ~a\n" (syntax-e stx) ty))

;; get the type annotation of this identifier, otherwise error
;; if #:default is provided, return that instead of error
;; identifier #:default Type -> Type
(define (get-type stx #:default [default #f] #:infer [infer #t])
  (parameterize
      ([current-orig-stx stx])
    (cond
      [(type-annotation stx #:infer infer)]
      [default default]
      [(not (syntax-original? stx))
       (tc-error "untyped variable: ~a" (syntax-e stx))]
      [else
       (tc-error "no type information on variable ~a" (syntax-e stx))])))

;; Listof[identifer] #:default Type -> Listof[Type]
(define (get-types stxs #:default [default #f])
  (map (lambda (e) (get-type e #:default default)) stxs))

;; list[identifier] stx (stx -> tc-results?) (stx tc-results? -> tc-results?) -> tc-results?
;; stxs : the identifiers, possibly with type annotations on them
;; expr : the RHS expression
;; tc-expr : a function like `tc-expr' from tc-expr-unit
;; tc-expr/check : a function like `tc-expr/check' from tc-expr-unit
(d/c (get-type/infer stxs expr tc-expr tc-expr/check)
  ((listof identifier?) syntax? (syntax? . -> . tc-results?) (syntax? tc-results? . -> . tc-results?) . -> . tc-results?)
  (match stxs
    [(list stx ...)
     (let ([anns (for/list ([s stxs]) (type-annotation s #:infer #t))])
       (if (for/and ([a anns]) a)
           (tc-expr/check expr (ret anns))
           (let ([ty (tc-expr expr)])
             (match ty
               [(tc-results: tys fs os) 
                (if (not (= (length stxs) (length tys)))
                    (begin
                      (tc-error/delayed 
                                      "Expression should produce ~a values, but produces ~a values of types ~a"
                                      (length stxs) (length tys) (stringify tys))
                      (ret (map (lambda _ (Un)) stxs)))
                    (combine-results 
                     (for/list ([stx stxs] [ty tys] [a anns] [f fs] [o os])
                       (cond [a (check-type stx ty a) (ret a f o)]
			     ;; mutated variables get generalized, so that we don't infer too small a type
			     [(is-var-mutated? stx) (ret (generalize ty) f o)]
                             [else (ret ty f o)]))))]))))]))

;; check that e-type is compatible with ty in context of stx
;; otherwise, error
;; syntax type type -> void
(define (check-type stx e-type ty)
  (parameterize ([current-orig-stx stx])
    (unless (subtype e-type ty)
      ;(printf "orig-stx: ~a" (syntax->datum stx*))
      (tc-error "Body had type:\n~a\nVariable had type:\n~a\n" e-type ty))))

(define (dotted? stx)
  (cond [(syntax-property stx type-dotted-symbol) => syntax-e]
        [else #f]))
