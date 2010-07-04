#lang scheme/base

(require (rename-in "utils/utils.ss" [infer r:infer]))

(require (private base-types)
         (for-syntax 
          (except-in stxclass id)
          scheme/base
          (private type-contract)
          (types utils convenience)
	  (typecheck typechecker provide-handling)
	  (env type-environments type-name-env type-alias-env)
	  (r:infer infer)
	  (utils tc-utils)
	  (rep type-rep)
	  (except-in (utils utils) infer extend)
          (only-in (r:infer infer-dummy) infer-param)
          scheme/nest
          syntax/kerncase
          scheme/match))




(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require]))

(define-for-syntax catch-errors? #f)

;(begin (init-tnames))


(define-syntax (module-begin stx)
  (define module-name (syntax-property stx 'enclosing-module-name))
  ;(printf "BEGIN: ~a~n" (syntax->datum stx))
  (with-logging-to-file 
   (build-path (find-system-path 'temp-dir) "ts-poly.log")
   #;
   (log-file-name (syntax-source stx) module-name)
   (syntax-case stx ()
     [(mb forms ...)
      (nest
          ([begin (set-box! typed-context? #t)
                  (start-timing module-name)]
           [with-handlers
               ([(lambda (e) (and catch-errors? (exn:fail? e) (not (exn:fail:syntax? e))))
                 (lambda (e) (tc-error "Internal error: ~a" e))])]
           [parameterize (;; disable fancy printing
                          [custom-printer #t]
                          ;; a cheat to avoid units
                          [infer-param infer]
                          ;; do we report multiple errors
                          [delay-errors? #t]
                          ;; this parameter is for parsing types
                          [current-tvars initial-tvar-env]
                          ;; this parameter is just for printing types
                          ;; this is a parameter to avoid dependency issues
                          [current-type-names
                           (lambda ()
                             (append 
                              (type-name-env-map (lambda (id ty)
                                                   (cons (syntax-e id) ty)))
                              (type-alias-env-map (lambda (id ty)
                                                    (cons (syntax-e id) ty)))))]
                          ;; reinitialize seen type variables
                          [type-name-references null])]
           [begin (do-time "Initialized Envs")]
           ;; local-expand the module
           ;; pmb = #%plain-module-begin                            
           [with-syntax ([new-mod 
                          (local-expand (syntax/loc stx
                                          (#%plain-module-begin 
                                           forms ...))
                                        'module-begin 
                                        null)])]
           [with-syntax ([(pmb body2 ...) #'new-mod])]
           [begin (do-time "Local Expand Done")]
           [with-syntax ([after-code (parameterize ([orig-module-stx stx]
                                                    [expanded-module-stx #'new-mod])
                                       (type-check #'(body2 ...)))]
                         [check-syntax-help (syntax-property #'(void) 'disappeared-use (type-name-references))]
                         [(transformed-body ...) (remove-provides #'(body2 ...))])]
           [with-syntax ([(transformed-body ...) (change-contract-fixups #'(transformed-body ...))])])
        (do-time "Typechecked")
        #;(printf "checked ~a~n" module-name)
        #;(printf "created ~a types~n" (count!))
        #;(printf "tried to create ~a types~n" (all-count!))
        #;(printf "created ~a union types~n" (union-count!))
        ;; reconstruct the module with the extra code
        #'(#%module-begin transformed-body ... after-code check-syntax-help))])))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . (module . rest))
     (eq? 'module (syntax-e #'module))
     #'(module . rest)]
    [(_ . form)     
     (nest
         ([begin (set-box! typed-context? #t)]
          [parameterize (;; a cheat to avoid units
                         [infer-param infer]
                         ;; this paramter is for parsing types
                         [current-tvars initial-tvar-env]
                         ;; this parameter is just for printing types
                         ;; this is a parameter to avoid dependency issues
                         [current-type-names
                          (lambda () 
                            (append 
                             (type-name-env-map (lambda (id ty)
                                                  (cons (syntax-e id) ty)))
                             (type-alias-env-map (lambda (id ty)
                                                   (cons (syntax-e id) ty)))))])]
         ;(do-time "Initialized Envs")
          ;; local-expand the module
          [let ([body2 (local-expand #'(#%top-interaction . form) 'top-level null)])]
          [parameterize ([orig-module-stx #'form]
                         [expanded-module-stx body2])]
          ;; typecheck the body, and produce syntax-time code that registers types
          [let ([type (tc-toplevel-form body2)])])
         (define-syntax-class invis-kw
           #:literals (define-values define-syntaxes #%require #%provide begin)
           (pattern define-values)
           (pattern define-syntaxes)
           (pattern #%require)
           (pattern #%provide)
           (pattern begin))
       (syntax-parse body2
         [(head:invis-kw . _) 
          body2]
         [_ (let ([ty-str (match type
                            [(tc-result1: (? (lambda (t) (type-equal? t -Void)))) #f]
                            [(tc-result1: t)
                             (format "- : ~a\n" t)]
                            [(tc-results: t)
                             (format "- : ~a\n" (cons 'Values t))]
                            [x (int-err "bad type result: ~a" x)])])
              (if ty-str                  
                  #`(let ([type '#,ty-str])
                      (begin0 
                        #,body2
                        (display type)))
                  body2))]))]))



