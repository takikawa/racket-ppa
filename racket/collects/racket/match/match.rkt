#lang racket/base

(require (only-in "runtime.rkt"
                  match-equality-test
                  exn:misc:match?)
         (only-in "match-expander.rkt"
                  define-match-expander)
         "define-forms.rkt"
         "struct.rkt"
         (for-syntax racket/lazy-require
                     "syntax-local-match-introduce.rkt"
                     (only-in "stxtime.rkt"
                              match-...-nesting
                              match-expander?
                              legacy-match-expander?
                              prop:match-expander
                              prop:legacy-match-expander)))

(begin-for-syntax
  (lazy-require [racket/match/parse (parse)]))

(provide (for-syntax match-...-nesting match-expander? legacy-match-expander?
                     prop:match-expander prop:legacy-match-expander
                     syntax-local-match-introduce)
         match-equality-test
         define-match-expander
         struct* ==          
         exn:misc:match?)

(define-forms parse
  match match* match-lambda match-lambda* match-lambda** match-let match-let*
  match-let-values match-let*-values
  match-define match-define-values match-letrec match/values
  match/derived match*/derived
  define/match)
