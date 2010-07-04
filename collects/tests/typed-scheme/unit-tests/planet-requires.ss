#lang scheme/base

(require (for-syntax scheme/base scheme/require-transform)
         scheme/require-syntax)

(define-for-syntax (splice-requires specs)  
  (define subs (map (compose cons expand-import) specs))
  (values (apply append (map car subs)) (apply append (map cdr subs))))

(define-syntax define-module
  (syntax-rules ()
    [(_ nm spec ...)
     
     (define-syntax nm
       (make-require-transformer
        (lambda (stx)
          (splice-requires (list (syntax-local-introduce (quote-syntax spec)) ...)))))
     #;
     (define-require-syntax nm
       (lambda (stx) 
         (syntax-case stx ()
           [(_) (datum->syntax stx (syntax->datum #'(combine-in spec ...)))])))]))

#;
(define-syntax define-module
  (lambda (stx)
    (syntax-case stx ()
      [(_ nm spec ...)
       (syntax/loc stx
         (define-syntax nm
           (make-require-transformer
            (lambda (stx)
              (splice-requires (list (syntax-local-introduce (quote-syntax spec)) ...))))))])))

(define-syntax planet/multiple
  (make-require-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(_ plt files ...)
        (let ([mk (lambda (spc)
                    (syntax-case spc (prefix-in)
                      [e
                       (string? (syntax-e #'e))
                       (datum->syntax spc `(planet ,#'e ,#'plt) spc)]
                      [(prefix-in p e)
                       (datum->syntax spc `(prefix-in ,#'p (planet ,#'e ,#'plt)) spc)]))])
          (splice-requires (map mk (syntax->list #'(files ...)))))]))))


(provide schemeunit)
;; why is this neccessary?
(provide planet/multiple)

(define-module schemeunit
  (planet/multiple ("schematics" "schemeunit.plt" 2 3)
                   "test.ss"
                   ;"graphical-ui.ss"
                   "text-ui.ss"
                   "util.ss")
  ;; disabled until Carl updates to v4
  #;
  (planet/multiple ("cce" "fasttest.plt" 1 2)
                   "random.ss"
                   "schemeunit.ss"))
