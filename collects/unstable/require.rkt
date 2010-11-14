#lang racket

(require (for-syntax racket/match
                     racket/require-transform
                     racket/provide-transform
                     syntax/parse
                     unstable/planet-syntax)
         unstable/define)

(define-syntax (define-planet-package stx)
  (syntax-parse stx
    [(_ name:id pkg:id)
     (syntax/loc stx
       (define-syntax name
         (make-require-transformer
          (lambda (stx*)
            (syntax-parse stx*
              [(_) (expand-import (datum->syntax stx* (list #'planet #'pkg)))]
              [(_ file:id)
               (let* ([prefix (symbol->string (syntax-e #'pkg))]
                      [suffix (symbol->string (syntax-e #'file))]
                      [sym (string->symbol (string-append prefix "/" suffix))]
                      [spec (datum->syntax stx* (list #'planet sym))])
                 (expand-import spec))])))))]))

(define-syntax (define-collection stx)
  (syntax-parse stx
    [(_ name:id collect:id)
     #'(define-syntax name
         (make-require-transformer
          (lambda (stx*)
            (syntax-parse stx*
              [(_) (expand-import (datum->syntax stx* (syntax-e #'collect)))]
              [(_ file:id)
               (let* ([prefix (symbol->string (syntax-e #'collect))]
                      [suffix (symbol->string (syntax-e #'file))]
                      [sym (string->symbol (string-append prefix "/" suffix))]
                      [spec (datum->syntax stx* sym)])
                 (expand-import spec))]))))]))

(define-syntax this-package-in
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ file:id)
        (expand-import (make-planet-path stx #'file))]))))

(define-syntax this-package-out
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ file:id)
        (expand-export
         (datum->syntax
          stx
          (list #'all-from-out (make-planet-path stx #'file)))
         modes)]))))

(define-for-syntax (import->export i)
  (make-export (import-local-id i)
               (syntax-e (import-local-id i))
               (import-mode i)
               #f
               (import-orig-stx i)))

(define-syntax box-require
  (make-require-transformer
   (lambda (stx)
     (syntax-parse stx
       [(_ ibox spec:expr)
        #:declare ibox (static box? "mutable box for expanded import specs")
        (let-values ([(imports sources) (expand-import #'spec)])
          (set-box! (syntax-local-value #'ibox) imports)
          (values imports sources))]))))

(define-syntax box-provide
  (make-provide-transformer
   (lambda (stx modes)
     (syntax-parse stx
       [(_ ibox)
        #:declare ibox (static box? "mutable box for expanded import specs")
        (map import->export (unbox (syntax-local-value #'ibox)))]))))

(define-syntax-rule (require/provide spec ...)
  (begin
    (define-syntax imports (box #f))
    (require (box-require imports (combine-in spec ...)))
    (provide (box-provide imports))))

(define-syntax (quote-require stx)
  (syntax-parse stx
    [(_ spec:expr ...)
     (let*-values ([(imports sources)
                    (expand-import (syntax/loc stx (combine-in spec ...)))])
       (with-syntax ([(name ...) (map import-local-id imports)])
         (syntax/loc stx '(name ...))))]))

;; rename-import : Import Identifier -> Import
;; Creates a new import that binds the given identifier, but otherwise acts as
;; the original import.
(define-for-syntax (rename-import i id)
  (struct-copy import i [local-id id]))

;; import->raw-require-spec : Import -> Syntax
;; Constructs a raw-require-spec (suitable for #%require) that should have the
;; same behavior as a require-spec that produces the given import.
(define-for-syntax (import->raw-require-spec i)
  (match i
    [(struct import [local-id
                     src-sym
                     src-mod-path
                     mode
                     req-mode
                     orig-mode
                     orig-stx])
     (datum->syntax
      orig-stx
      (list #'just-meta
            req-mode
            (list #'for-meta
                  mode
                  (list #'rename
                        src-mod-path
                        (syntax-local-introduce local-id)
                        src-sym)))
      orig-stx)]))

(provide require/provide
         quote-require
         define-planet-package
         define-collection
         this-package-in)
