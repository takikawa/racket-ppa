#lang at-exp racket/base

(require scribble/struct
         scribble/manual
         scribble/examples
         scribble/decode
         racket/contract
         racket/contract/collapsible
         "../icons.rkt")

(provide (all-from-out scribble/manual)
         (all-from-out scribble/examples)
         (all-from-out racket/contract)
         (all-from-out racket/contract/collapsible))

(require (for-label racket))
(provide (for-label (all-from-out racket)))
(require (for-label racket/contract/collapsible))
(provide (for-label (all-from-out racket/contract/collapsible)))

(provide mz-examples)
(define mz-eval (make-base-eval))
(define-syntax mz-examples
  (syntax-rules ()
    [(_ #:eval . rest)
     (examples #:eval . rest)]
    [(_ . rest)
     (examples #:eval mz-eval . rest)]))

(define AllUnix "Unix and Mac OS")
(provide AllUnix)

(provide note-lib)
(define-syntax note-lib
  (syntax-rules ()
    [(_ lib #:more-libs (lib+ ...) #:use-sources (src ...) . more)
     (begin
       (declare-exporting lib lib+ ... racket #:use-sources (src ...))
       (defmodule*/no-declare (lib lib+ ...)
         (t (make-collect-element
             #f null
             (lambda (ci)
               (collect-put! ci `(racket-extra-lib ,'lib) (racketmodname lib))))
            "The bindings documented in this section are provided by the "
            (combine-library-names (racketmodname lib) (racketmodname lib+) ...) ; includes trailing space
            "and "
            (racketmodname racket)
            " libraries, but not " (racketmodname racket/base)
            "."
            . more)))]
    [(_ lib #:more-libs (lib+ ...) . more)
     (note-lib lib #:more-libs (lib+ ...) #:use-sources () . more)]
    [(_ lib #:use-sources (src ...) . more)
     (note-lib lib #:more-libs () #:use-sources (src ...) . more)]
    [(_ lib . more)
     (note-lib lib #:more-libs () #:use-sources () . more)]))

(define (combine-library-names lib . lib+s)
  (if (null? lib+s)
      (list lib " ")
      (for/list ([lib (in-list (cons lib lib+s))])
        (list lib ", "))))

(provide note-init-lib)
(define-syntax note-init-lib
  (syntax-rules ()
    [(_ lib #:use-sources (src ...) . more)
     (begin
       (declare-exporting lib racket/init #:use-sources (src ...))
       (defmodule*/no-declare (lib)
         (t "The bindings documented in this section are provided by the "
            (racketmodname lib)
            " and "
            (racketmodname racket/init)
            " libraries, which means that they are available when "
            " the Racket executable is started with no command-line arguments."
            " They are not provided by " (racketmodname racket/base)
            " or " (racketmodname racket) "."
            . more)))]
    [(_ lib . more)
     (note-init-lib lib #:use-sources () . more)]))

(provide note-lib-only)
(define-syntax note-lib-only
  (syntax-rules ()
    [(_ lib #:use-sources (src ...) . more)
     (defmodule lib #:use-sources (src ...)
       (t "The bindings documented in this section are provided by the "
          (racketmodname lib)
          " library, not " (racketmodname racket/base)
          " or " (racketmodname racket)
          "."
          . more))]
    [(_ lib . more)
     (note-lib-only lib #:use-sources () . more)]))

(define (*exnraise s)
  (make-element #f (list s " exception is raised")))
(define-syntax exnraise
  (syntax-rules ()
    [(_ s) (*exnraise (racket s))]))
(define-syntax Exn
  (syntax-rules ()
    [(_ s) (racket s)]))
(provide exnraise Exn)

(provide margin-note/ref
         refalso moreref Guide guideintro guidealso guidesecref
         raco-doc)

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

(define (guidealso tag)
  (apply margin-note
         (decode-content (append (list finger "See also " (guidesecref tag) " in " Guide ".")))))

(define Guide
  (other-manual '(lib "scribblings/guide/guide.scrbl")))

(define raco-doc
  '(lib "scribblings/raco/raco.scrbl"))

(provide see-read-print)
(define (see-read-print tag-part #:print [print-tag-part tag-part] vals)
  @elem{See @secref[(string-append "parse-" tag-part)]
            for information on @racket[read]ing
            @|vals| and @secref[(string-append "print-" print-tag-part)]
            for information on @racket[print]ing @|vals|.})

(provide speed)
(define-syntax speed
  (syntax-rules ()
    [(_ id what)
     (t "An " (racket id) " application can provide better performance for "
        (elem what)
        " iteration when it appears directly in a " (racket for) " clause.")]))

(provide resultItself ResultItself)
(define (esultItself T x)
  (make-element #f (list T "he "
                         (tech "synchronization result")
                         " of a " x " is the " x " itself")))
(define (ResultItself x) (esultItself "T" x))
(define (resultItself x) (esultItself "t" x))

;; The arity of many functions changed in 7.0.0.13:
(provide history/arity)
(define-syntax-rule (history/arity arg ...)
  (history #:changed "7.0.0.13" @elem{Allow one argument, in addition to allowing two or more.}
           arg ...))

(provide envvar-indexed)
(define (envvar-indexed s)
  (as-index (envvar s)))
