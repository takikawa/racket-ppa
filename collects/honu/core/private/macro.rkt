#lang racket/base

(require "honu-typed-scheme.ss"
         "literals.rkt"
         "parse.ss"
         "syntax.ss"
         syntax/parse
         (for-syntax macro-debugger/emit)
         (for-meta 2 macro-debugger/emit
                   racket/base)
         (for-meta -3
           (only-in "literals.rkt" (#%parens literal-parens)))
         #;
         (for-template (only-in "literals.rkt" (#%parens literal-parens)))
         (for-syntax "debug.ss"
                     "contexts.ss"
                     "parse.ss"
                     "syntax.ss"
                     "literals.rkt"
                     "honu-typed-scheme.ss"
                     racket/base
                     syntax/parse
                     syntax/stx
                     scheme/pretty
                     scheme/trace))

(provide (all-defined-out))

(define-syntax (ensure-defined stx)
  (syntax-case stx ()
    [(_ id ...)
     (begin
       (for-each (lambda (id)
                   (syntax-local-value id (lambda () (raise-syntax-error 'syntax-id "not defined" id))))
                 (syntax->list #'(id ...)))
       #'(void))]))

(ensure-defined #%parens #%braces)

(define-for-syntax (extract-conventions pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       ;; todo: export honu attributes for syntax/parse
       (loop (cons #'(any expr) out)
             #'(rest ...))
       #;
       (loop (cons #'(any attribute) out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (loop out #'(rest1 rest ...))]
      [(foo) out])))

(define-for-syntax (extract-patterns pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       (loop (cons #'any out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (let ([f (if (eq? (syntax->datum #'foo) 'crackers)
                  #'(... ...)
                  #'foo)])
         (loop (cons f out)
               #'(rest1 rest ...)))]
      [(foo) (reverse (cons #'foo out))])))

#|
(define-for-syntax (convert stx)
  (syntax-case stx (...)
    [(_ x ...)
     |#

#;
(define-for-syntax (get-attributes stx)
  (define (attach name attributes)
    (list))
  (syntax-parse stx #:literals (honu-:)
    [(variable:identifier honu-: class:identifier rest ...)
     (let ([vs (attach #'variable (syntax-class-attributes (attribute class)))])
       (append vs (get-attributes #'(rest ...))))]
    [(one rest ...) (append (get-attributes #'one)
                            (get-attributes #'(rest ...)))]
    [else (list)]))

(define-for-syntax (fix-template stx)
  (define (fix-classes stx)
    (syntax-parse stx #:literals (honu-:)
      [(variable:identifier honu-: class:identifier rest ...)
       (with-syntax ([(rest* ...) (fix-template #'(rest ...))])
         (datum->syntax stx (cons #'(~var variable class #:attr-name-separator "_")
                                  #'(rest* ...))
                        stx)
         #;
         #'((~var variable class) rest* ...))]
      [(one rest ...)
       (with-syntax ([one* (fix-template #'one)]
                     [(rest* ...) (fix-template #'(rest ...))])
         (datum->syntax stx (cons #'one*
                                  #'(rest* ...))
                        stx)
         #;
         #'(one* rest* ...))]
      [else stx]))
  ;; removes commas from a pattern
  (define (fix-commas stx)
    (syntax-parse stx
      #:literals (honu-comma
                   [ellipses ...])
      [(a honu-comma ellipses rest ...)
       (with-syntax ([a* (fix-commas #'a)]
                     [(rest* ...) (fix-commas #'(rest ...))])
         (datum->syntax stx
                        `((~seq ,#'a* (~optional |,|)) ... ,@#'(rest* ...))
                        stx stx)
         #;
         (datum->syntax stx
                        (cons
                          #'a*
                          (cons
                            #'(... ...)
                            #'(rest* ...)))
                        stx stx))]
      [(z rest ...)
       (with-syntax ([z* (fix-commas #'z)]
                     [(rest* ...) (fix-commas #'(rest ...))])
         (datum->syntax stx
                        (cons #'z* #'(rest* ...))
                        stx stx))]
      [else stx]))
  (define all-fixes (compose fix-commas fix-classes))
  (all-fixes stx))

#|
(define-for-syntax (fix-template stx)
  [(any \;
            (... ...) rest1 rest ...)
       (loop (cons #'(semicolon any (... ..)))
             #'(rest1 rest ...))]
      [((any1 any ...) rest1 rest ...)
       (loop (loop out #'(any1 any ...))
             #'(rest1 rest ...))]
      |#


;; x = 1 + y; ...

#;
(define-honu-syntax honu-macro
  (lambda (stx ctx)
    (debug "Original macro: ~a\n" (syntax->datum stx))
    (syntax-case stx (#%parens #%braces)
      [(_ (#%parens honu-literal ...)
          (#%braces (#%braces name pattern ...))
          (#%braces (#%braces template ...))
          . rest)
       (with-syntax ([(conventions ...)
                      (extract-conventions #'(pattern ...))]
                     [(raw-patterns ...)
                      (extract-patterns #'(pattern ...))]
                     [(fixed-template ...)
                      (fix-template #'(template ...))])
         (debug "new template ~a\n" (syntax->datum #'(fixed-template ...)))
         (values
           (syntax/loc
             stx
             (begin
               #|
               (define honu-literal (lambda () (error 'honu-literal "cant use this")))
               ...
               |#
               (define-honu-syntax name
                 (lambda (stx ctx)
                   (debug "Try to match against pattern ~a. Literals ~a\n" '(name raw-patterns ... . rrest) '(honu-literal ...))
                   (debug "stx is ~a\n" (syntax->datum stx))
                   ;; (debug "head is ~a\n" (stx-car stx))
                   ;; (debug "= is ~a\n" =)
                   (debug "my matcher ~a\n"
                           (syntax-case stx (to set! do honu-end honu-literal ...)
                             [(name q set! v to m do bb (... ...) honu-end) (syntax->datum #'(bb (... ...)))]
                             [(name raw-patterns ...)
                              'ok2]
                             [(name pattern ...) 'ok5]
                             [(name v (... ...) honu-literal ...) 'ok4]
                             [(name v (... ...)) 'ok3]
                             #;
                             [(name v (... ...)) (syntax->datum #'(v (... ...)))]
                             [else 'bad]))
                   #;
                   (debug "case pattern ~a\n"
                           #'(syntax-case stx
                               (honu-literal ...)
                               [(name pattern ...)
                                #'(honu-unparsed-block
                                    #f obj 'obj #f ctx
                                    fixed-template ...)]))

                   (let ([result (syntax-case stx
                                   #;
                                   (to set! do honu-end)
                                   (honu-literal ...)
                                   #;
                                   [(name q set! v to m do bb (... ...) honu-end) (syntax->datum #'(bb (... ...)))]
                                   [(name pattern ...) 'ok]
                                   [(name raw-patterns ...)
                                    #'(honu-unparsed-block
                                        #f obj 'obj #f ctx
                                        fixed-template ...)]
                                   [else 'fail-boat])])
                     (debug "result was ~a\n" result))
                   (syntax-case stx (honu-literal ...)
                     [(name raw-patterns ... . rrest)
                      (values
                        #'(honu-unparsed-block
                            #f obj 'obj #f ctx
                            fixed-template ...)
                        #'rrest)])))
               #;
               (define-honu-syntax name
                 (lambda (stx ctx)
                   (define-conventions honu-conventions conventions ...)
                   #;
                   (debug "Hello from ~a transformer. Syntax is ~a\n" 'name (syntax->datum stx))
                   (syntax-parse stx
                                 #:literals (honu-literal ...)
                                 #:conventions (honu-conventions)
                                 [(name raw-patterns ... . rrest)
                                  (values
                                    #'(honu-unparsed-block
                                        #f obj 'obj #f ctx
                                        fixed-template ...)
                                    #'rrest)])))))
           #'rest))])
    ))

(define-for-syntax (delimiter? x)
  (or (free-identifier=? x #'\;)))

(define-syntax (my-ellipses stx) (raise-syntax-error 'my-ellipses "dont use this"))
;; (define-syntax (wrapped stx) (raise-syntax-error 'wrapped "dont use wrap"))
;; just a phase 0 identifier
(define wrapped #f)
(define unwrap #f)

(define-for-syntax (pull stx)
  (define (reverse-syntax stx)
    (with-syntax ([(x ...) (reverse (syntax->list stx))])
      #'(x ...)))
  (define-syntax-class stop-class
    (pattern x:id #:when (or (free-identifier=? #'x #'(... ...))
                             (free-identifier=? #'x #'\;))))
  (define (do-ellipses stx)
    (let loop ([ellipses '()]
               [body '()]
               [stx stx])
      (cond
        [(null? stx) (values (with-syntax ([(ellipses ...) ellipses]
                                           [(body ...) body])
                               #'(ellipses ... body ...))
                             stx)]
        [(and (identifier? (car stx))
              (free-identifier=? (car stx) #'(... ...)))
         (loop (cons #'(... ...) ellipses) body (cdr stx))]
        [(and (identifier? (car stx))
              (free-identifier=? (car stx) #'\;))
         ;; (debug "Found a ; in ~a\n" (syntax->datum stx))
         (with-syntax ([all (cdr stx)])
           ;; (debug "Found a ; -- ~a\n" (syntax->datum #'all))
           (syntax-parse #'all
                         [((~and x (~not _:stop-class)) ... stop:stop-class y ...)
                          (with-syntax ([(ellipses ...) ellipses]
                                        [(x* ...) (reverse-syntax #'(x ...))])
                            (values #'(ellipses ... (wrapped x* ... \;) unwrap)
                                    #'(stop y ...)))]
                         [else (with-syntax ([(f ...) (reverse-syntax #'all)]
                                             [(ellipses ...) ellipses])
                                 (values #'(ellipses ... (wrapped f ... \;) unwrap)
                                         #'()))]))])))
  (let loop ([all '()]
             [stx (reverse (syntax->list stx))])
    (if (null? stx)
        (with-syntax ([x all])
          #'x)
        (let ([head (car stx)]
              [tail (cdr stx)])
          (cond
            [(and (identifier? head)
                  (free-identifier=? head #'(... ...)))
             (let-values ([(wrapped rest) (do-ellipses (cons head tail))])
               (loop (cons (reverse-syntax wrapped) all) (syntax->list rest)))]
            [else (loop (cons head all) tail)])))))

;; rename this to wrap
#;
(define-for-syntax (pull stx)
  (define (reverse-syntax stx)
    (with-syntax ([(x ...) (reverse (syntax->list stx))])
      #'(x ...)))
  (define-syntax-class delimiter-class
    (pattern x:id #:when (delimiter? #'x)))
  (define-syntax-class ellipses-class
                       (pattern x:id #:when (free-identifier=? #'x #'(... ...))))
  (define-syntax-class not-ellipses-class
                       (pattern x:id #:when (not (free-identifier=? #'x #'(... ...)))))
  ;; use this if you are defining your own ellipses identifier
  #;
  (define-syntax-class ellipses-class
                       #:literals (...)
                       (pattern my-ellipses))
  (if (not (stx-pair? stx))
    stx
    (let ([stx (reverse (syntax->list stx))])
      ;; (debug-parse stx (ellipses1:ellipses-class ellipses:ellipses-class ... x ...))
      ;; (debug "stx is ~a\n" stx)
      ;; (debug "... = ~a\n" (free-identifier=? #'(... ...) (stx-car stx)))
      (syntax-parse stx
        [(before:not-ellipses-class ... ellipses1:ellipses-class ellipses:ellipses-class ... delimiter:delimiter-class x ...)
         (with-syntax ([(x* ...) (reverse-syntax (pull #'(delimiter x ...)))])
           (reverse-syntax
             (with-syntax ([wrapped #'wrapped]
                           [original
                            (with-syntax ([(ellipses* ...) (map (lambda (_)
                                                            #'((... ...) (... ...)))
                                                          (syntax->list #'(ellipses1 ellipses ...)))]
                                          [(x-new ...) (generate-temporaries #'(delimiter x ...))])
                              (reverse-syntax #'(before ... ellipses* ... x-new ...)))]
                           #;
                           [original (syntax->datum (reverse-syntax #'(ellipses1 ellipses ... x ...)))])
               #'(ellipses1 ellipses ... (wrapped x* ...) unwrap))))]
        [(ellipses1:ellipses-class ellipses:ellipses-class ... x ...)
         (with-syntax ([(x* ...) (reverse-syntax (pull #'(x ...)))])
           (reverse-syntax
             (with-syntax ([wrapped #'wrapped]
                           [original
                            (with-syntax ([(ellipses* ...) (map (lambda (_)
                                                            #'((... ...) (... ...)))
                                                          (syntax->list #'(ellipses1 ellipses ...)))]
                                          [(x-new ...) (generate-temporaries #'(x ...))])
                              (reverse-syntax #'(ellipses* ... x-new ...)))]
                           #;
                           [original (syntax->datum (reverse-syntax #'(ellipses1 ellipses ... x ...)))])
               #'(ellipses1 ellipses ... (wrapped x* ...) unwrap))))]
        [(x ...) (with-syntax ([(x* ...) (map pull (syntax->list #'(x ...)))])
                   (reverse-syntax #'(x* ...)))]))))

;; (begin-for-syntax (trace pull))

(define-for-syntax (unpull stx)
  (define-syntax-class ellipses-class
    (pattern x:id #:when (free-identifier=? #'x #'(... ...))))
  (define-syntax-class delimiter-class
    (pattern x:id #:when (delimiter? #'x)))
  ;; (debug "unpull ~a\n" (syntax->datum stx))
  (syntax-parse stx
                #:literals (wrapped unwrap)
                [((~and z (~not (unwrap _ ...))) ... (unwrap (wrapped x ... delimiter:delimiter-class) ...) rest ...)
                 (with-syntax ([(x1 ...) (apply append (map syntax->list (syntax->list #'((x ... delimiter) ...))))]
                               [(rest* ...) (unpull #'(rest ...))])
                 #'(z ... x1 ... rest* ...))]
                [(unwrap (wrapped x ... delimiter:delimiter-class) ...)
                 (with-syntax ([(x1 ...) (apply append (map syntax->list (syntax->list #'((x ... delimiter) ...))))])
                 #'(x1 ...))]
                [(unwrap (wrapped x ... y) ...)
                 (with-syntax ([(x1 ...) (car (syntax->list #'((x ...) ...)))])
                   (with-syntax ([(x1* ...) (map unpull (syntax->list #'(x1 ...)))]
                                 [(y* ...) (map unpull (syntax->list #'(y ...)))])
                     #'(x1* ... y* ...)))]
                [(unwrap . x) (raise-syntax-error 'unpull "unhandled unwrap ~a" stx)]
                [(x ...) (with-syntax ([(x* ...) (map unpull (syntax->list #'(x ...)))])
                           #'(x* ...))]
                [else stx]))

;; rename this to unwrap
#;
(define-syntax (unpull stx)
  (define-syntax-class ellipses-class
                       (pattern x:id #:when (free-identifier=? #'x #'(... ...))))
  (define (do-it stx)
    (syntax-parse stx
      #:literals (wrapped)
      [((wrapped x ... y) ...)
       (with-syntax ([(x1 ...) (car (syntax->list #'((x ...) ...)))])
         #'(x1 ... y ...))]
      [((wrapped x ...) ellipses1:ellipses-class ellipses:ellipses-class ...)
       (with-syntax ([(x* ...) (map do-it (syntax->list #'(x ...)))])
         #'(x* ...  ellipses1 ellipses ...))]
      [(x ...) (with-syntax ([(x* ...) (map do-it (syntax->list #'(x ...)))])
                 #'(x* ...))]
      [else stx]))
  (syntax-case stx ()
    [(_ x ...) (do-it #'(x ...))]))

(provide (for-syntax unpull))

#;
(define-honu-syntax unpull
  (lambda (stx ctx)
    (define-syntax-class ellipses-class
      (pattern x:id #:when (free-identifier=? #'x #'(... ...))))
    (define (do-it stx)
      (syntax-parse stx
                    #:literals (wrapped)
                    [((wrapped x ... y) ...)
                     (with-syntax ([(x1 ...) (car (syntax->list #'((x ...) ...)))])
                       #'(x1 ... y ...))]
                    [((wrapped x ...) ellipses1:ellipses-class ellipses:ellipses-class ...)
                     (with-syntax ([(x* ...) (map do-it (syntax->list #'(x ...)))])
                       #'(x* ...  ellipses1 ellipses ...))]
                    [(x ...) (with-syntax ([(x* ...) (map do-it (syntax->list #'(x ...)))])
                               (debug "x* is ~a\n" #'(x* ...))
                               #'(x* ...))]
                    [else stx]))
    (syntax-case stx ()
      [(_ x ...) (values (do-it #'(x ...))
                         #'())])))
 
#;
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ x ...)
     (begin
       (pretty-print (syntax->datum (pull #'(x ...))))
       (pretty-print (syntax->datum (unpull (pull #'(x ...)))))
       #'1)]))

(define-syntax (my-syntax stx)
  (syntax-case stx ()
    [(_ name pattern template)
     (with-syntax ([wrap-it (pull #'template)])
       #'(define-syntax (name stx)
           (syntax-case stx ()
             [pattern #'wrap-it]
             [else (raise-syntax-error 'name (format "~a does not match pattern ~a"
                                                     (syntax->datum stx)
                                                     'pattern))]
             )))]))

#;
(define-syntax (honu-unparsed-expr stx)
  (define (fix stx)
    (debug "Fix ~a\n" (syntax->datum stx))
    (syntax-parse stx #:literals (honu-syntax #%parens)
      [(honu-syntax (#%parens x ...) y ...)
       (with-syntax ([(y* ...) (fix #'(y ...))])
         #'(x ... y* ...))]
      [(z x ...)
       (with-syntax ([z* (fix #'z)]
                     [(x* ...) (fix #'(x ...))])
         #'(z* x* ...))]
      [else stx]
      ))
  (debug "unparsed expr ~a\n" stx)
  (fix (stx-cdr stx)))

(define-syntax (test2 stx)
  (syntax-case stx ()
    [(_ x ...)
     (begin
       (with-syntax ([pulled (pull #'(x ...))])
         #'(unpull pulled)))]))

(define-honu-syntax honu-pattern
  (lambda (stx ctx)
    (syntax-parse stx #:literal-sets ([cruft #:at stx])
      #:literals (honu-literal)
      ;; #%parens #%brackets semicolon)
      [(_ name
          (~optional (~seq honu-literal (#%parens literals ...)))
          (#%parens all-attributes:identifier ...)
          (#%brackets xpattern ...)
          semicolon . rest)
       (define my-parens (datum->syntax #'name '#%parens #'name #'name))
       (define (create-pattern stuff)
         (with-syntax ([(fixed ...) (fix-template stuff)])
           (syntax/loc stuff (pattern (~seq fixed ...)))))
       (values
         (lambda ()
           (if (attribute literals)
             (with-syntax ([final-pattern (create-pattern #'(xpattern ...))])
                           (syntax/loc stx
                                       (define-splicing-syntax-class name
                                                                     #:literal-sets ([cruft #:at name])
                                                                     #:literals (literals ...)
                                                                     #:attributes (all-attributes ...)
                                                                     final-pattern)))
             (with-syntax ([final-pattern (create-pattern #'(xpattern ...))])
                           (syntax/loc stx
                                       (define-splicing-syntax-class name
                                                                     #:literal-sets ([cruft #:at name])
                                                                     #:attributes (all-attributes ...)
                                                                     final-pattern)))))
         #'rest)])))

(define foobar 0)

(define-honu-syntax honu-infix-macro
  (lambda (stx ctx)
    (debug "Infix macro!\n")
    (define-splicing-syntax-class patterns
                         #:literal-sets ([cruft #:phase (syntax-local-phase-level)])
      #;
      [pattern (~seq x ...)
               #:with (template ...) '()
               #:with (code ...) '()
               #:with (fixed ...) '()
               #:when (begin
                        (debug "Trying to parse ~a\n" (syntax->datum #'(x ...)))
                        #f)]
      #;
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) '()
               #:when (begin
                        (debug "Got template as ~a. Code is ~a\n" (syntax->datum #'(template ...)) (syntax->datum #'(code ...)))
                        #f)]
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) (fix-template #'(template ...))])
    (define-syntax-class honu-macro3
                         ;; #:literals (#%parens #%braces)
                         #:literal-sets ([cruft ;;#:at stx
                                           #:phase (syntax-local-phase-level)
                                           ])
      [pattern (_ name (#%parens literals ...)
                  pattern:patterns ...
                  . rest)
               #:with result
               (list
                 (with-syntax (
                               #;
                               [(fixed ...) (fix-template #'(template ...))]
                               #;
                               [first-pattern (stx-car #'(template ...))]
                               #;
                               [your-bracket (datum->syntax #'name '#%brackets #'name)]
                               #;
                               [your-braces (datum->syntax #'name '#%braces #'name)]
                               #;
                               [your-parens (datum->syntax #'name '#%parens #'name)])
                   (apply-scheme-syntax
                   (syntax/loc stx
                               (define-honu-infix-syntax name
                                 (lambda (stx ctx)
                                   #;
                                   (debug "Executing macro `~a' on input `~a'\n" 'name (syntax->datum stx))
                                   (debug "~a pattern is ~a\n" 'name '(pattern.fixed ... ...))
                                   (syntax-parse stx
                                     #:literal-sets ([cruft #:at name])
                                     #:literals (foobar literals ...)
                                     [(pattern.fixed ... rrest (... ...))
                                      (values
                                        #;
                                        (with-syntax ([(real-out (... ...)) #'(code ...)])
                                          (let ([result (let ()
                                                          (honu-unparsed-begin #'(real-out (... ...))))])
                                            (lambda () result)))
                                        (begin
                                          (emit-remark "Do macro transformer" (quote-syntax (pattern.code ...)))
                                          #;
                                          (debug "Macro transformer `~a'\n" (syntax->datum (quote-syntax (code ...))))
                                          (let ([result (let ()
                                                          (honu-unparsed-begin pattern.code ...))])
                                            (lambda ()
                                              (emit-remark "Excuting macro " (symbol->string 'name))
                                              result)))
                                        #'(rrest (... ...)))]
                                     ...
                                     [else (raise-syntax-error 'name "bad syntax")]
                                     )))))) 
                 #'rest)])
    (debug "Executing honu infix macro\n")
    (syntax-parse stx
      [out:honu-macro3 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]

      #;
      [(_ (#%parens honu-literal ...)
          (#%braces (#%braces name pattern ...))
          (#%braces (#%braces template ...))
          . rest)
       (with-syntax ([pulled (pull #'(template ...))]
                     [(pattern* ...) (map (lambda (stx)
                                            (if (and (identifier? stx)
                                                     (not (ormap (lambda (f)
                                                                   (free-identifier=? stx f))
                                                                 (syntax->list #'(honu-literal ...))))
                                                     (not (free-identifier=? stx #'(... ...))))
                                                (with-syntax ([x stx])
                                                  #'(~and x (~not (~or honu-literal ...))))
                                                stx))
                                          (syntax->list #'(pattern ...)))]
                     )
         (values
           (syntax/loc stx
          (define-honu-syntax name
              (lambda (stx ctx)
                ;; (define-literal-set literals (honu-literal ...))
                (syntax-parse stx
                              ;; #:literal-sets (literals)
                              #:literals (honu-literal ...)
                  [(name pattern* ... . rrest)
                   (with-syntax ([(out (... ...)) (unpull #'pulled)])
                     (define (X) (raise-syntax-error (syntax->datum #'name) "implement for this context"))
                     (values
                       (syntax/loc stx (honu-unparsed-expr (honu-syntax (#%parens out (... ...)))))
                      ;; this is sort of ugly, is there a better way?
                      #;
                      (cond
                        [(type-context? ctx) (X)]
                        [(type-or-expression-context? ctx) (X)]
                        [(expression-context? ctx) (syntax/loc stx (honu-unparsed-expr (out (... ...))))]
                        [(expression-block-context? ctx)
                         (syntax/loc stx 
                         (honu-unparsed-begin (honu-syntax #%parens (out (... ...)))))]
                        [(block-context? ctx)
                         (syntax/loc stx
                         (honu-unparsed-begin out (... ...)))]
                        [(variable-definition-context? ctx) (X)]
                        [(constant-definition-context? ctx) (X)]
                        [(function-definition-context? ctx) (X)]
                        [(prototype-context? ctx) (X)]
                        [else (syntax/loc stx (honu-syntax (#%parens (out (... ...)))))])
                      #;
                      #'(honu-unparsed-begin out (... ...))
                      #'rrest)
                     #;
                     #'(honu-unparsed-block
                        #f obj 'obj #f ctx
                        out (... ...))
                     #;
                     (values
                      #;
                      #'(honu-unparsed-expr out (... ...))
                      #'(honu-unparsed-block
                         #f obj 'obj #f ctx
                         out (... ...) rrest)
                      #;
                      #'rrest))]))))
          #'rest))]
      [(_ (m x ...)
          (z y ...)
          #;
          (#%braces (#%braces name pattern ...))
          . rest)
       (begin
         (debug "Got literals ~a\n" #'(x ...))
         (debug "M is ~a, = to #%parens is ~a\n" #'m (free-identifier=? #'#%parens #'m))
         (debug "Z is ~a, = to #%braces is ~a\n" #'z (free-identifier=? #'#%braces #'z))
         (debug "Rest is ~a\n" (syntax->datum #'rest))
         #;
         (debug "Got name ~a pattern ~a\n" #'name #'(pattern ...))
         (raise-syntax-error 'honu-macro "f1" stx))]
      [else (raise-syntax-error 'honu-macro "fail" stx)]
      )))

(define-honu-syntax honu-macro
  (lambda (stx ctx)
    (define-splicing-syntax-class patterns
                         #:literal-sets ([cruft #:phase (syntax-local-phase-level)])
      #;
      [pattern (~seq x ...)
               #:with (template ...) '()
               #:with (code ...) '()
               #:with (fixed ...) '()
               #:when (begin
                        (debug "Trying to parse ~a\n" (syntax->datum #'(x ...)))
                        #f)]
      #;
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) '()
               #:when (begin
                        (debug "Got template as ~a. Code is ~a\n" (syntax->datum #'(template ...)) (syntax->datum #'(code ...)))
                        #f)]
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) (fix-template #'(template ...))])
    (define-syntax-class honu-macro3
                         ;; #:literals (#%parens #%braces)
                         #:literal-sets ([cruft ;;#:at stx
                                           #:phase (syntax-local-phase-level)
                                           ])
      [pattern (_ name (#%parens literals ...)
                  pattern:patterns ...
                  . rest)
               #:with result
               (list
                 (with-syntax (
                               #;
                               [(fixed ...) (fix-template #'(template ...))]
                               #;
                               [first-pattern (stx-car #'(template ...))]
                               #;
                               [your-bracket (datum->syntax #'name '#%brackets #'name)]
                               #;
                               [your-braces (datum->syntax #'name '#%braces #'name)]
                               #;
                               [your-parens (datum->syntax #'name '#%parens #'name)])
                   ;;(debug "Ok macro3 go!\n")
                   #;
                   #'(define-honu-syntax name
                       (lambda (stx ctx)
                         (syntax-parse stx #:literals (your-parens your-bracket literals ...)
                           [(fixed ... rrest (... ...))
                            (values
                              #;
                              (with-syntax ([(real-out (... ...)) #'(code ...)])
                                (let ([result (honu-unparsed-begin #'(real-out (... ...)))])
                                  (lambda () result)))
                              (let ([result (honu-unparsed-begin code ...)])
                                (lambda () result))
                              #'(rrest (... ...)))])))
                   #;
                   (debug "Original pattern ~a\n" (syntax->datum #'(pattern.fixed ... rrest (... ...))))
                   (apply-scheme-syntax
                   (syntax/loc stx
                               (define-honu-syntax name
                                 (lambda (stx ctx)
                                   #;
                                   (debug "Executing macro `~a' on input `~a'\n" 'name (syntax->datum stx))
                                   (debug "~a pattern is ~a\n" 'name '(pattern.fixed ... ...))
                                   (syntax-parse stx
                                     #:literal-sets ([cruft #:at name])
                                     #:literals (foobar literals ...)
                                     [(pattern.fixed ... rrest (... ...))
                                      (values
                                        #;
                                        (with-syntax ([(real-out (... ...)) #'(code ...)])
                                          (let ([result (let ()
                                                          (honu-unparsed-begin #'(real-out (... ...))))])
                                            (lambda () result)))
                                        (begin
                                          (emit-remark "Do macro transformer" (quote-syntax (pattern.code ...)))
                                          #;
                                          (debug "Macro transformer `~a'\n" (syntax->datum (quote-syntax (code ...))))
                                          (let ([result (let ()
                                                          (honu-unparsed-begin pattern.code ...))])
                                            (lambda ()
                                              (emit-remark "Excuting macro " (symbol->string 'name))
                                              result)))
                                        #'(rrest (... ...)))]
                                     ...
                                     [else (raise-syntax-error 'name "bad syntax")]
                                     ))))))
                 #;
                 (with-syntax ([parsed (let-values ([(out rest*)
                                                     (parse-block-one/2 #'(code ...)
                                                                        the-expression-context)])
                                         out)])
                   (syntax/loc stx
                               (define-honu-syntax name
                                 (lambda (stx ctx)
                                   parsed))))
                 #'rest)])
    (define-syntax-class honu-macro2
                         #:literals (#%parens #%braces)
      [pattern (_ name (#%braces code ...)
                  . rest)
               #:with result
               (list
                 (syntax/loc stx
                             (define-honu-syntax name
                               (lambda (stx ctx)
                                 (values
                                   (honu-unparsed-begin code ...)
                                   (begin
                                     (debug "inside ~a stx is ~a\n" 'name stx)
                                     (syntax-parse stx #:literals (semicolon)
                                       [(_ semicolon rrest (... ...))
                                        #'(rrest (... ...))]))))))
                 #;
                 (with-syntax ([parsed (let-values ([(out rest*)
                                                     (parse-block-one/2 #'(code ...)
                                                                        the-expression-context)])
                                         out)])
                   (syntax/loc stx
                               (define-honu-syntax name
                                 (lambda (stx ctx)
                                   parsed))))
                 #'rest)])

    (define-syntax-class honu-macro1
                     #:literals (#%parens #%braces)
                     [pattern (_ (#%parens honu-literal ...)
                                 (#%braces (#%braces name pattern ...))
                                 (#%braces (#%braces template ...))
                                 . rest)
                              #:with result
                              (with-syntax ([pulled (pull #'(template ...))]
                                            [(pattern* ...) (map (lambda (stx)
                                                                   (if (and (identifier? stx)
                                                                            (not (ormap (lambda (f)
                                                                                          (free-identifier=? stx f))
                                                                                        (syntax->list #'(honu-literal ...))))
                                                                            (not (free-identifier=? stx #'(... ...))))
                                                                     (with-syntax ([x stx])
                                                                       #'(~and x (~not (~or honu-literal ...))))
                                                                     stx))
                                                                 (syntax->list #'(pattern ...)))]
                                            )
                                (list
                                  (syntax/loc stx
                                              (define-honu-syntax name
                                                (lambda (stx ctx)
                                                  ;; (define-literal-set literals (honu-literal ...))
                                                  (syntax-parse stx
                                                    ;; #:literal-sets (literals)
                                                    #:literals (honu-literal ...)
                                                    [(name pattern* ... . rrest)
                                                     (with-syntax ([(out (... ...)) (unpull #'pulled)])
                                                       (define (X) (raise-syntax-error (syntax->datum #'name) "implement for this context"))
                                                       (values
                                                         (syntax/loc stx (honu-unparsed-expr (honu-syntax (#%parens out (... ...)))))
                                                         ;; this is sort of ugly, is there a better way?
                                                         #;
                                                         (cond
                                                           [(type-context? ctx) (X)]
                                                           [(type-or-expression-context? ctx) (X)]
                                                           [(expression-context? ctx) (syntax/loc stx (honu-unparsed-expr (out (... ...))))]
                                                           [(expression-block-context? ctx)
                                                            (syntax/loc stx 
                                                                        (honu-unparsed-begin (honu-syntax #%parens (out (... ...)))))]
                                                           [(block-context? ctx)
                                                            (syntax/loc stx
                                                                        (honu-unparsed-begin out (... ...)))]
                                                           [(variable-definition-context? ctx) (X)]
                                                           [(constant-definition-context? ctx) (X)]
                                                           [(function-definition-context? ctx) (X)]
                                                           [(prototype-context? ctx) (X)]
                                                           [else (syntax/loc stx (honu-syntax (#%parens (out (... ...)))))])
                                                         #;
                                                         #'(honu-unparsed-begin out (... ...))
                                                         #'rrest)
                                                       #;
                                                       #'(honu-unparsed-block
                                                           #f obj 'obj #f ctx
                                                           out (... ...))
                                                       #;
                                                       (values
                                                         #;
                                                         #'(honu-unparsed-expr out (... ...))
                                                         #'(honu-unparsed-block
                                                             #f obj 'obj #f ctx
                                                             out (... ...) rrest)
                                                         #;
                                                         #'rrest))]))))
                                  #'rest))])
    (debug "Executing honu macro\n")
    (syntax-parse stx
      [out:honu-macro1 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]
      [out:honu-macro3 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]
      [out:honu-macro2 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]

      #;
      [(_ (#%parens honu-literal ...)
          (#%braces (#%braces name pattern ...))
          (#%braces (#%braces template ...))
          . rest)
       (with-syntax ([pulled (pull #'(template ...))]
                     [(pattern* ...) (map (lambda (stx)
                                            (if (and (identifier? stx)
                                                     (not (ormap (lambda (f)
                                                                   (free-identifier=? stx f))
                                                                 (syntax->list #'(honu-literal ...))))
                                                     (not (free-identifier=? stx #'(... ...))))
                                                (with-syntax ([x stx])
                                                  #'(~and x (~not (~or honu-literal ...))))
                                                stx))
                                          (syntax->list #'(pattern ...)))]
                     )
         (values
           (syntax/loc stx
          (define-honu-syntax name
              (lambda (stx ctx)
                ;; (define-literal-set literals (honu-literal ...))
                (syntax-parse stx
                              ;; #:literal-sets (literals)
                              #:literals (honu-literal ...)
                  [(name pattern* ... . rrest)
                   (with-syntax ([(out (... ...)) (unpull #'pulled)])
                     (define (X) (raise-syntax-error (syntax->datum #'name) "implement for this context"))
                     (values
                       (syntax/loc stx (honu-unparsed-expr (honu-syntax (#%parens out (... ...)))))
                      ;; this is sort of ugly, is there a better way?
                      #;
                      (cond
                        [(type-context? ctx) (X)]
                        [(type-or-expression-context? ctx) (X)]
                        [(expression-context? ctx) (syntax/loc stx (honu-unparsed-expr (out (... ...))))]
                        [(expression-block-context? ctx)
                         (syntax/loc stx 
                         (honu-unparsed-begin (honu-syntax #%parens (out (... ...)))))]
                        [(block-context? ctx)
                         (syntax/loc stx
                         (honu-unparsed-begin out (... ...)))]
                        [(variable-definition-context? ctx) (X)]
                        [(constant-definition-context? ctx) (X)]
                        [(function-definition-context? ctx) (X)]
                        [(prototype-context? ctx) (X)]
                        [else (syntax/loc stx (honu-syntax (#%parens (out (... ...)))))])
                      #;
                      #'(honu-unparsed-begin out (... ...))
                      #'rrest)
                     #;
                     #'(honu-unparsed-block
                        #f obj 'obj #f ctx
                        out (... ...))
                     #;
                     (values
                      #;
                      #'(honu-unparsed-expr out (... ...))
                      #'(honu-unparsed-block
                         #f obj 'obj #f ctx
                         out (... ...) rrest)
                      #;
                      #'rrest))]))))
          #'rest))]
      [(_ (m x ...)
          (z y ...)
          #;
          (#%braces (#%braces name pattern ...))
          . rest)
       (begin
         (debug "Got literals ~a\n" #'(x ...))
         (debug "M is ~a, = to #%parens is ~a\n" #'m (free-identifier=? #'#%parens #'m))
         (debug "Z is ~a, = to #%braces is ~a\n" #'z (free-identifier=? #'#%braces #'z))
         (debug "Rest is ~a\n" (syntax->datum #'rest))
         #;
         (debug "Got name ~a pattern ~a\n" #'name #'(pattern ...))
         (raise-syntax-error 'honu-macro "f1" stx))]
      [else (raise-syntax-error 'honu-macro "fail" stx)]
      )))

;; (my-syntax guz (_ display (#%parens x ...)) (+ x ...))
;; (guz display (#%parens 1 2 3 4))

;; (local-expand stx 'expression (list #'wrapped))

#|
(begin-for-syntax
  (trace pull))
(test display (#%parens x))
(test display (#%parens x ... ...) ...)
|#
