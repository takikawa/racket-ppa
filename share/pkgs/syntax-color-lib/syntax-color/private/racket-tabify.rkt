#lang racket/base

(provide racket-tabify-table->head-sexp-type
         racket-tabify-default-table)

;; racket-tabify-table->head-sexp-type : (list ht regexp regexp regexp regexp)
;;                                       -> string 
;;                                       -> (or/c #f 'lambda 'define 'begin 'for/fold)
(define (racket-tabify-table->head-sexp-type pref)
  (define ht (car pref))
  (define beg-reg (list-ref pref 1))
  (define def-reg (list-ref pref 2))
  (define lam-reg (list-ref pref 3))
  (define for/fold-reg (list-ref pref 4))
  (lambda (text)
    (hash-ref
     ht
     (with-handlers ((exn:fail:read? (λ (x) #f)))
       (read (open-input-string text)))
     (λ ()
       (cond
         [(and beg-reg (regexp-match? beg-reg text)) 'begin]
         [(and def-reg (regexp-match? def-reg text)) 'define]
         [(and lam-reg (regexp-match? lam-reg text)) 'lambda]
         [(and for/fold-reg (regexp-match? for/fold-reg text)) 'for/fold]
         [else #f])))))

;; default input to `racket-tabify-table->head-sexp-type`
(define racket-tabify-default-table
  (let ([defaults-ht (make-hasheq)])
    (for-each (λ (x) (hash-set! defaults-ht x 'for/fold))
              '(for/fold for/fold: for*/fold for*/fold:
                 for/lists for/lists: for*/lists for*/lists:))
    (for-each (λ (x) (hash-set! defaults-ht x 'define))
              '(struct local struct: pdefine: match-define match-define-values
                 pattern))
    (for-each (λ (x) (hash-set! defaults-ht x 'begin))
              '(case-lambda case-lambda: pcase-lambda:
                            match-lambda match-lambda*
                            syntax-parser
                            cond
                            delay
                            unit compound-unit
                            public private override require
                            inherit
                            ;; Explicitly indent these with- constructs using begin-like style
                            ;; for otherwise they will be captured by the regexp of lambda-like style
                            with-output-to-string with-output-to-bytes
                            with-module-reading-parameterization))
    (for-each (λ (x) (hash-set! defaults-ht x 'lambda))
              `(
                cases
                instantiate super-instantiate
                syntax/loc quasisyntax/loc
                datum-case
                match match* match-let match-let* match-letrec
                
                λ lambda let let* letrec recur
                lambda/kw
                letrec-values
                with-syntax with-syntax*
                with-continuation-mark
                module module* module+
                match match-let match-let* match-letrec
                let/cc let/ec
                let-syntax letrec-syntax letrec-syntaxes+values
                
                let: letrec: let*:
                let-values: letrec-values: let*-values:
                let/cc: let/ec:
                lambda: λ:
                plambda: opt-lambda: popt-lambda:

                splicing-let splicing-letrec splicing-let-values
                splicing-letrec-values splicing-let-syntax
                splicing-letrec-syntax splicing-let-syntaxes
                splicing-letrec-syntaxes splicing-letrec-syntaxes+values
                splicing-local splicing-parameterize splicing-syntax-parameterize

                do:
                
                kernel-syntax-case
                syntax-case syntax-case* syntax-rules syntax-id-rules
                syntax-parse
                fluid-let
                let-struct let-values let*-values
                case when unless 
                class class*
                rec
                make-object mixin
                do opt-lambda
                send* with-method
                define-record
                shared
                with-handlers
                interface
                parameterize parameterize* syntax-parameterize
                call-with-input-file call-with-input-file* with-input-from-file
                with-input-from-string
                call-with-output-file
                with-output-to-file

                for-all

                big-bang
                
                type-case))
    (list defaults-ht #rx"^begin" #rx"^def" #rx"^(for\\*?(/|$)|with-)" #f)))
