#lang racket/base
(require scribble/manual
         scribble/eval
         racket/string
         racket/sandbox)
(provide ellipses
         make-sp-eval)

(define ellipses (racket ...))

(define (fixup/short exn)
  (let ([src (ormap values (exn:fail:syntax-exprs exn))])
    (if src
        (make-exn:fail:syntax
         (let* ([msg (exn-message exn)]
                [oneline? (not (regexp-match? #rx"\n" msg))])
           (format "~a~a at: ~s"
                   msg (if oneline? "" "\n ") (syntax->datum src)))
         (exn-continuation-marks exn)
         (exn:fail:syntax-exprs exn))
        exn)))

(define (fixup/long exn)
  (let ([src (ormap values (exn:fail:syntax-exprs exn))])
    (if src
        (make-exn:fail:syntax
         (string-trim (exn-message exn)
                      #rx"[^ ]*[ ]"
                      #:left? #t #:right? #f)
         (exn-continuation-marks exn)
         (exn:fail:syntax-exprs exn))
        exn)))

(define (make-sp-eval [short? #f])
  (define fixup (if short? fixup/short fixup/long))
  (define the-eval
    (call-with-trusted-sandbox-configuration
     (lambda ()
       (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string]
                      [sandbox-propagate-breaks #f]
                      [sandbox-eval-handlers
                       (list #f
                             (lambda (thunk)
                               (with-handlers ([exn:fail:syntax?
                                                (lambda (e) (raise (fixup e)))])
                                 (thunk))))])
         (make-evaluator 'racket/base
                         #:requires (let ([mods '(racket/promise
                                                  racket/pretty
                                                  syntax/parse
                                                  syntax/parse/debug
                                                  syntax/parse/experimental/splicing
                                                  syntax/parse/experimental/contract
                                                  syntax/parse/experimental/reflect
                                                  syntax/parse/experimental/specialize
                                                  syntax/parse/experimental/template
                                                  syntax/parse/experimental/eh
                                                  syntax/transformer)])
                                      `((for-syntax racket/base ,@mods)
                                        ,@mods)))))))
  (call-in-sandbox-context the-eval
    (lambda ()
      (current-print (dynamic-require 'racket/pretty 'pretty-print-handler))
      (when short? (error-print-source-location #f))))
  the-eval)

;; ----

(define Spattern "single-term pattern")
(define Lpattern "list pattern")
(define Hpattern "head pattern")
(define EHpattern "ellipsis-head pattern")
(define Apattern "action pattern")

(define Spatterns "single-term patterns")
(define Lpatterns "list patterns")
(define Hpatterns "head patterns")
(define EHpatterns "ellipsis-head patterns")
(define Apatterns "action patterns")

(provide Spattern
         Lpattern
         Hpattern
         EHpattern
         Apattern
         Spatterns
         Lpatterns
         Hpatterns
         EHpatterns
         Apatterns)

;; ----

(define-syntax-rule (defhere id) (defidentifier #'id #:form? #t))

(define-syntax ref
  (syntax-rules ()
    [(ref id suffix ...)
     (elemref (list 'pattern-link (list 'id 'suffix ...))
              (racketkeywordfont (symbol->string 'id))
              (superscript (symbol->string 'suffix)) ...
              #:underline? #f)]))
(define-syntax def
  (syntax-rules ()
    [(def id suffix ...)
     (elemtag (list 'pattern-link (list 'id 'suffix ...))
              (racket id)
              #|(superscript (symbol->string 'suffix)) ...|# )]))

(define-syntax-rule (defsubthing . xs)
  (nested #:style "leftindent" (defthing . xs)))

(provide defhere
         ref
         def
         defsubthing)

;; ----

(require (for-label racket/base
                    racket/contract
                    (except-in syntax/parse ...+)
                    syntax/parse/debug
                    syntax/parse/experimental/contract
                    syntax/parse/experimental/splicing
                    syntax/parse/experimental/reflect
                    syntax/parse/experimental/provide
                    syntax/parse/experimental/specialize
                    syntax/parse/experimental/template
                    syntax/parse/experimental/eh
                    syntax/transformer
                    "parse-dummy-bindings.rkt"))
(provide (for-label (all-from-out racket/base)
                    (all-from-out racket/contract)
                    (all-from-out syntax/parse)
                    (all-from-out syntax/parse/debug)
                    (all-from-out syntax/parse/experimental/contract)
                    (all-from-out syntax/parse/experimental/splicing)
                    (all-from-out syntax/parse/experimental/reflect)
                    (all-from-out syntax/parse/experimental/provide)
                    (all-from-out syntax/parse/experimental/specialize)
                    (all-from-out syntax/parse/experimental/template)
                    (all-from-out syntax/parse/experimental/eh)
                    (all-from-out syntax/transformer)
                    (all-from-out "parse-dummy-bindings.rkt")))
