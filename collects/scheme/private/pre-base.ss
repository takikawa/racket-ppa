
;; A sane "core" for finishing up the "scheme/base" library

(module pre-base '#%kernel
  (#%require (for-syntax '#%kernel))
  (#%require "more-scheme.ss"
             "misc.ss"
             (all-except "define.ss" define)
             "letstx-scheme.ss"
             "kw.ss"
             "define-struct.ss"
             "reqprov.ss"
             "modbeg.ss"
             "for.ss"
             "map.ss" ; shadows #%kernel bindings
             "kernstruct.ss"
             '#%builtin) ; so it's attached

  (define-syntaxes (#%top-interaction)
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (datum->syntax stx (cdr (syntax-e stx)) stx stx)))

  (define-values (new-apply)
    (make-keyword-procedure
     (lambda (kws kw-args proc args . rest)
       (keyword-apply proc kws kw-args (apply list* args rest)))
     apply))

  (define-values (new-keyword-apply)
    (make-keyword-procedure
     (lambda (kws kw-args proc orig-kws orig-kw-args args . rest)
       (let-values ([(kws kw-args)
                     (let loop ([kws kws] [kw-args kw-args]
                                [kws2 orig-kws] [kw-args2 orig-kw-args]
                                [swapped? #f])
                       (cond
                        [(null? kws) (values kws2 kw-args2)]
                        [(null? kws2) (values kws kw-args)]
                        [(keyword<? (car kws) (car kws2))
                         (let-values ([(res-kws res-kw-args)
                                       (loop (cdr kws) (cdr kw-args) kws2 kw-args2 #f)])
                           (values (cons (car kws) res-kws)
                                   (cons (car kw-args) res-kw-args)))]
                        [swapped?
                         (raise-mismatch-error
                          'keyword-apply
                          "keyword duplicated in list and direct keyword arguments: "
                          (car kws))]
                        [else (loop kws2 kw-args2 kws kw-args #t)]))])
         (keyword-apply proc kws kw-args (apply list* args rest))))
     keyword-apply))

  (#%provide (all-from-except "more-scheme.ss" old-case fluid-let)
             (all-from "misc.ss")
             (all-from "define.ss")
             (all-from-except "letstx-scheme.ss" -define -define-syntax -define-struct old-cond)
             (rename new-lambda lambda)
             (rename new-λ λ)
             (rename new-define define)
             (rename new-app #%app)
             (rename new-apply apply)
             (rename new-prop:procedure prop:procedure)
             (rename #%app #%plain-app)
             (rename lambda #%plain-lambda)
             (rename #%module-begin #%plain-module-begin)
             (rename module-begin #%module-begin)
             (all-from-except '#%kernel lambda λ #%app #%module-begin apply prop:procedure)
             (all-from "reqprov.ss")
             (all-from "for.ss")
             (all-from "kernstruct.ss")
             #%top-interaction

             map for-each andmap ormap
             make-keyword-procedure
             (rename new-keyword-apply keyword-apply)
             procedure-keywords
             procedure-reduce-keyword-arity
             (rename define-struct* define-struct)
             define-struct/derived
             struct-field-index
             struct-copy))
