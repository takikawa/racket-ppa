#lang scribble/doc

@(require scribble/manual
          scribble/eval
          scriblib/render-cond
          scribble/core
          scribble/html-properties
          (for-syntax scheme/base)
          (for-label scheme/base
                     racket/stream))

@(provide (all-defined-out))

@(define-syntax (srfi stx)
  (syntax-case stx ()
   [(_ num #:subdir subdir? . title)
    (with-syntax ([srfi/n (string->symbol (format "srfi/~a" (syntax-e #'num)))])
      #'(begin
          (section #:tag (format "srfi-~a" num)
                   #:style 'unnumbered
                   (format "SRFI ~a: " num)
                   . title)
          (defmodule srfi/n)
          "Original specification: "
          (let* ([label (format "SRFI ~a" num)]
                 [sub (if subdir? (format "srfi-~a/" num) "")]
                 [url (λ (b) (format "~a/srfi-std/~asrfi-~a.html" b sub num))])
            (cond-element
              [(or latex text) @link[(url "https://docs.racket-lang.org") label]]
              [else @link[(url ".") label]]))))]
   [(_ num . title) #'(srfi num #:subdir #f . title)]))

@;{ The `lst' argument is a list of
       (list sym syntactic-form? html-anchor) }
@(define (redirect n lst #:subdir [subdir? #f])
   (let ([file (if subdir?
                 (format "srfi-~a/srfi-~a.html" n n)
                 (format "srfi-~a.html" n))]
         [mod-path (string->symbol (format "srfi/~a" n))])
     (make-binding-redirect-elements mod-path
       (map (lambda (b)
              (list (car b) (cadr b)
                    (build-path "srfi-std" file)
                    (caddr b)))
            lst))))

@(define in-core
   (case-lambda
     [() (in-core ".")]
     [(k) @elem{This SRFI's bindings are also available in
                @racketmodname[racket/base]@|k|}]))

@(begin
  (define-syntax-rule (def-mz mz-if)
    (begin
      (require (for-label mzscheme))
      (define mz-if (racket if))))
  (def-mz mz-if))

@(define srfi-std (style #f (list (install-resource "srfi-std"))))

@(define srfi-license-history-url
   ;; explains the "historical reasons" for the restrictive license on SRFI 5
   "https://srfi-email.schemers.org/srfi-announce/msg/2652023/")
@(define srfi-5-std-taglet "srfi-5-std")
@(define srfi-5-license-taglet "srfi-5-std-license")

@(define (racket-license-link . args)
   ;; FIXME why does this not work?
   #;
   (apply seclink "License" #:doc '(lib "scribblings/main/license.scrbl") args)
   (define (link base)
     (apply hyperlink (string-append base "license/index.html") args))
   (cond-element
    [(or latex text)
     (link "https://docs.racket-lang.org/")]
    [else
     (link "../")]))
