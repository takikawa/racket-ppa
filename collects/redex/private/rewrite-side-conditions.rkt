(module rewrite-side-conditions scheme
  (require mzlib/list
           "underscore-allowed.rkt")
  (require (for-template
            mzscheme
            "term.rkt"
            "matcher.rkt"))
  
  (provide rewrite-side-conditions/check-errs
           extract-names
           (rename-out [binds? id-binds?])
           raise-ellipsis-depth-error
           make-language-id
           language-id-nts)
  
  (provide (struct-out id/depth))
  
  (define-values (language-id make-language-id language-id? language-id-get language-id-set) (make-struct-type 'language-id #f 2 0 #f '() #f 0))
  
  (define (language-id-nts stx id) (language-id-getter stx id 1))
  (define (language-id-getter stx id n)
    (unless (identifier? stx)
      (raise-syntax-error id "expected an identifier defined by define-language" stx))
    (let ([val (syntax-local-value stx (λ () #f))])
      (unless (and (set!-transformer? val)
                   (language-id? (set!-transformer-procedure val)))
        (raise-syntax-error id "expected an identifier defined by define-language" stx))
      (language-id-get (set!-transformer-procedure val) n)))
  
  (define (rewrite-side-conditions/check-errs all-nts what bind-names? orig-stx)
    (define (expected-exact name n stx)
      (raise-syntax-error what (format "~a expected to have ~a arguments" 
                                       name
                                       n)
                          orig-stx 
                          stx))
    (define (expected-arguments name stx)
      (raise-syntax-error what (format "~a expected to have arguments" name) orig-stx stx))
    (define ((expect-identifier src) stx)
      (unless (identifier? stx)
        (raise-syntax-error what "expected an identifier" src stx)))
    
    ;; call this and discard the result to ensure that all names are at the right ellipsis depths. 
    (extract-names all-nts what bind-names? orig-stx) 
    
    (let loop ([term orig-stx])
      (syntax-case term (side-condition variable-except variable-prefix hole name in-hole hide-hole side-condition cross unquote)
        [(side-condition pre-pat (and))
         ;; rewriting metafunctions (and possibly other things) that have no where, etc clauses
         ;; end up with side-conditions that are empty 'and' expressions, so we just toss them here.
         (loop #'pre-pat)]
        [(side-condition pre-pat exp)
         (with-syntax ([pat (loop (syntax pre-pat))])
           (let-values ([(names names/ellipses) (extract-names all-nts what bind-names? (syntax pat))])
             (with-syntax ([(name ...) names]
                           [(name/ellipses ...) names/ellipses]
                           [src-loc (parameterize ([print-syntax-width 0])
                                      (format "~s" #'exp))])
               (syntax/loc term
                 (side-condition
                  pat
                  ,(lambda (bindings)
                     (term-let
                      ([name/ellipses (lookup-binding bindings 'name)] ...)
                      exp))
                  ; For use in error messages.
                  src-loc)))))]
        [(side-condition a ...) (expected-exact 'side-condition 2 term)]
        [side-condition (expected-arguments 'side-condition term)]
        [(variable-except a ...)
         (for-each (expect-identifier term) (syntax->list #'(a ...)))
         term]
        [variable-except (expected-arguments 'variable-except term)]
        [(variable-prefix a)
         ((expect-identifier term) #'a)
         term]
        [(variable-prefix a ...) (expected-exact 'variable-prefix 1 term)]
        [variable-prefix (expected-arguments 'variable-prefix term)]
        [hole term]
        [(name x y) #`(name x #,(loop #'y))]
        [(name x ...) (expected-exact 'name 2 term)]
        [name (expected-arguments 'name term)]
        [(in-hole a b) #`(in-hole #,(loop #'a) #,(loop #'b))]
        [(in-hole a ...) (expected-exact 'in-hole 2 term)]
        [in-hole (expected-arguments 'in-hole term)]
        [(hide-hole a) #`(hide-hole #,(loop #'a))]
        [(hide-hole a ...) (expected-exact 'hide-hole 1 term)]
        [hide-hole (expected-arguments 'hide-hole term)]
        [(cross a)
         ((expect-identifier term) #'a)
         term]
        [(cross a ...) (expected-exact 'cross 1 term)]
        [cross (expected-arguments 'cross term)]
        [(unquote . _)
         (raise-syntax-error what "unquote disallowed in patterns" orig-stx term)]
        [_
         (identifier? term)
         (match (regexp-match #rx"^([^_]*)_.*" (symbol->string (syntax-e term)))
           [(list _ (app string->symbol s))
            (if (or (memq s (cons '... underscore-allowed))
                    (memq s all-nts))
                term
                (raise-syntax-error
                 what
                 (format "before underscore must be either a non-terminal or a built-in pattern, found ~a in ~s"
                         s (syntax-e term))
                 orig-stx 
                 term))]
           [_ term])]
        [(terms ...)
         (map loop (syntax->list (syntax (terms ...))))]
        [else
         (when (pair? (syntax-e term))
           (let loop ([term term])
             (cond
               [(syntax? term) (loop (syntax-e term))]
               [(pair? term) (loop (cdr term))]
               [(null? term) (void)]
               [#t
                (raise-syntax-error what "dotted pairs not supported in patterns" orig-stx term)])))
         term])))
  
  (define-struct id/depth (id depth))
  
  ;; extract-names : syntax syntax -> (values (listof syntax) (listof syntax[x | (x ...) | ((x ...) ...) | ...]))
  (define (extract-names all-nts what bind-names? orig-stx [mode 'rhs-only])
    (let* ([dups
            (let loop ([stx orig-stx]
                       [names null]
                       [depth 0])
              (syntax-case stx (name in-hole side-condition cross)
                [(name sym pat)
                 (identifier? (syntax sym))
                 (loop (syntax pat) 
                       (cons (make-id/depth (syntax sym) depth) names)
                       depth)]
                [(in-hole pat1 pat2)
                 (loop (syntax pat1)
                       (loop (syntax pat2) names depth)
                       depth)]
                [(side-condition pat . rest)
                 (loop (syntax pat) names depth)]
                [(cross _) names]
                [(pat ...)
                 (let i-loop ([pats (syntax->list (syntax (pat ...)))]
                              [names names])
                   (cond
                     [(null? pats) names]
                     [else 
                      (if (or (null? (cdr pats))
                              (not (identifier? (cadr pats)))
                              (not (or (free-identifier=? (quote-syntax ...)
                                                          (cadr pats))
                                       (let ([inside (syntax-e (cadr pats))])
                                         (regexp-match #rx"^\\.\\.\\._" (symbol->string inside))))))
                          (i-loop (cdr pats)
                                  (loop (car pats) names depth))
                          (i-loop (cdr pats)
                                  (loop (car pats) names (+ depth 1))))]))]
                [x
                 (and (identifier? (syntax x))
                      ((case mode
                         [(rhs-only) binds-in-right-hand-side?]
                         [(binds-anywhere) binds?])
                       all-nts bind-names? (syntax x)))
                 (cons (make-id/depth (syntax x) depth) names)]
                [else names]))]
           [no-dups (filter-duplicates what orig-stx dups)])
      (values (map id/depth-id no-dups)
              (map build-dots no-dups))))
  
  ;; build-dots : id/depth -> syntax[x | (x ...) | ((x ...) ...) | ...]
  (define (build-dots id/depth)
    (let loop ([depth (id/depth-depth id/depth)])
      (cond
        [(zero? depth) (id/depth-id id/depth)]
        [else (with-syntax ([rest (loop (- depth 1))]
                            [dots (quote-syntax ...)])
                (syntax (rest dots)))])))
  
  (define (binds? nts bind-names? x)
    (or (and bind-names? (memq (syntax-e x) nts))
        (and bind-names? (memq (syntax-e x) underscore-allowed))
        (regexp-match #rx"_" (symbol->string (syntax-e x)))))
  
  (define (binds-in-right-hand-side? nts bind-names? x)
    (and (binds? nts bind-names? x)
         (let ([str (symbol->string (syntax-e x))])
           (and (not (regexp-match #rx"^\\.\\.\\._" str))
                (not (regexp-match #rx"_!_" str))))))
  
  (define (raise-ellipsis-depth-error what one-binder one-depth another-binder another-depth)
    (raise
     (make-exn:fail:syntax
      (format "~a: found the same binder, ~s, at different depths, ~a and ~a"
              what
              (syntax->datum one-binder)
              one-depth
              another-depth)
      (current-continuation-marks)
      (list one-binder another-binder))))
  
  (define (filter-duplicates what orig-stx dups)
    (let loop ([dups dups])
      (cond
        [(null? dups) null]
        [else 
         (cons
          (car dups)
          (filter (lambda (x) 
                    (let ([same-id? (free-identifier=? (id/depth-id x)
                                                       (id/depth-id (car dups)))])
                      (when same-id?
                        (unless (equal? (id/depth-depth x)
                                        (id/depth-depth (car dups)))
                          (raise-ellipsis-depth-error
                           what 
                           (id/depth-id x) (id/depth-depth x)
                           (id/depth-id (car dups)) (id/depth-depth (car dups)))))
                      (not same-id?)))
                  (loop (cdr dups))))]))))
