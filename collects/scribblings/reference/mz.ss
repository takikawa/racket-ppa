(module mz scheme/base
  (require scribble/struct
           scribble/manual
           scribble/eval
           scribble/decode
           scheme/contract
           "../icons.ss")

  (provide (all-from-out scribble/manual)
           (all-from-out scribble/eval)
           (all-from-out scheme/contract))

  (require (for-label scheme))
  (provide (for-label (all-from-out scheme)))

  (provide mz-examples)
  (define mz-eval (make-base-eval))
  (define-syntax mz-examples
    (syntax-rules ()
      [(_ #:eval . rest)
       (examples #:eval . rest)]
      [(_ . rest)
       (examples #:eval mz-eval . rest)]))

  (define AllUnix "Unix and Mac OS X")
  (provide AllUnix)

  (provide note-lib)
  (define-syntax note-lib
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (begin
         (declare-exporting lib scheme #:use-sources (src ...))
         (defmodule*/no-declare (lib)
           (t (make-collect-element
               #f null
               (lambda (ci)
                 (collect-put! ci `(scheme-extra-lib ,'lib) (schememodname lib))))
              "The bindings documented in this section are provided by the "
              (schememodname lib)
              " and "
              (schememodname scheme)
              " libraries, but not " (schememodname scheme/base)
              "."
              . more)))]
      [(_ lib . more)
       (note-lib lib #:use-sources () . more)]))

  (provide note-init-lib)
  (define-syntax note-init-lib
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (begin
         (declare-exporting lib scheme/init #:use-sources (src ...))
         (defmodule*/no-declare (lib)
           (t "The bindings documented in this section are provided by the "
              (schememodname lib)
              " and "
              (schememodname scheme/init)
              " libraries, which means that they are available when "
              (exec "mzscheme") " is started with no command-line arguments."
              " They are not provided by " (schememodname scheme/base)
              " or " (schememodname scheme) "."
              . more)))]
      [(_ lib . more)
       (note-init-lib lib #:use-sources () . more)]))

  (provide note-lib-only)
  (define-syntax note-lib-only
    (syntax-rules ()
      [(_ lib #:use-sources (src ...) . more)
       (defmodule lib #:use-sources (src ...)
         (t "The bindings documented in this section are provided by the "
            (schememodname lib)
            " library, not " (schememodname scheme/base)
            " or " (schememodname scheme)
            "."
            . more))]
      [(_ lib . more)
       (note-lib-only lib #:use-sources () . more)]))
    
  (define (*exnraise s)
    (make-element #f (list s " exception is raised")))
  (define-syntax exnraise
    (syntax-rules ()
      [(_ s) (*exnraise (scheme s))]))
  (define-syntax Exn
    (syntax-rules ()
      [(_ s) (scheme s)]))
  (provide exnraise Exn)

  (provide margin-note/ref
           refalso moreref Guide guideintro guidesecref
           HonuManual)

  (define (margin-note/ref . s)
    (apply margin-note
           (decode-content (cons magnify s))))

  (define (refalso tag . s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " also provides information on ")
                                   s
                                   (list ".")))))

  (define (moreref tag . s)
    (apply margin-note
           (decode-content (append (list magnify (secref tag) " provides more information on ")
                                   s
                                   (list ".")))))
  
  (define (guidesecref s)
    (secref #:doc '(lib "scribblings/guide/guide.scrbl") s))

  (define (guideintro tag . s)
    (apply margin-note
           (decode-content (append (list finger (guidesecref tag) " in " Guide " introduces ")
                                   s
                                   (list ".")))))

  (define Guide
    (other-manual '(lib "scribblings/guide/guide.scrbl")))

  (define HonuManual
    (other-manual '(lib "scribblings/honu/honu.scrbl"))))

