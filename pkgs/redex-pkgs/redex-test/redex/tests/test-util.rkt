#lang scheme

(require redex/private/matcher
         redex/private/lang-struct
         (for-syntax syntax/parse setup/path-to-relative)
         setup/path-to-relative
         racket/runtime-path)
(provide test test-syn-err tests reset-count
         syn-err-test-namespace
         print-tests-passed
         equal/bindings?
         test-contract-violation
         test-runtime-err
         exec-syntax-error-tests
         exec-runtime-error-tests)

(define-runtime-path this-dir ".")

(define syn-err-test-namespace (make-base-namespace))
(parameterize ([current-namespace syn-err-test-namespace])
  (eval '(require redex/reduction-semantics)))

(define (read-syntax-test path)
  (call-with-input-file path
    (λ (port)
      (port-count-lines! port)
      (read-syntax path port))))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_ expected got)
     (with-syntax ([line (syntax-line stx)]
                   [fn (if (path? (syntax-source (syntax got)))
                           (path->relative-string/library (syntax-source (syntax got)))
                           "<unknown file>")])
       (syntax/loc stx (test/proc (λ () expected) got line fn)))]))

(define (syntax-error-test-setup thunk)
  (parameterize ([current-namespace syn-err-test-namespace])
    (with-handlers ([exn:fail:syntax? 
                     (λ (exn) 
                       (values (exn-message exn)
                               (map source-location (exn:fail:syntax-exprs exn))))])
      (thunk))))
(define (runtime-error-test-setup thunk)
  (define errortrace-key (dynamic-require 'errortrace/errortrace-key 'errortrace-key))
  (parameterize ([current-compile ((dynamic-require 'errortrace/errortrace-lib 'make-errortrace-compile-handler))])
    (with-handlers ([exn:fail? 
                     (λ (exn) 
                       (values (exn-message exn)
                               (let ([ans (let ([marks (continuation-mark-set->list
                                                        (exn-continuation-marks exn)
                                                        errortrace-key)])
                                            (if (null? marks) '() (list (cdar marks))))])
                                 (let loop ([ans ans])
                                   (cond
                                     [(pair? ans) (cons (loop (car ans)) (loop (cdr ans)))]
                                     [(path? ans) (path->relative-string/library ans)]
                                     [else ans])))))])
      (thunk))))

(define ((exec-error-tests setup exec) path)
  (for ([test (read-tests (build-path this-dir path))])
    (exec-error-test test exec setup)))
(define exec-syntax-error-tests
  (exec-error-tests syntax-error-test-setup expand))
(define exec-runtime-error-tests
  (exec-error-tests runtime-error-test-setup eval))

(define (exec-error-test spec exec setup)
  (define-values (file line expected-message expected-sources test)
    (make-error-test spec))
  (let-values ([(actual-message actual-sources)
                (setup (λ () (begin (exec test) (values "" '()))))])
    (test/proc (λ () actual-message) expected-message line file)
    (test/proc (λ () actual-sources) expected-sources line file)))

(define (make-error-test spec)
  (syntax-case spec ()
    [(message named-pieces body)
     (make-error-test (syntax/loc spec (message named-pieces () body)))]
    [(message ([loc-name loc-piece] ...) ([non-loc-name non-loc-piece] ...) body)
     (values (and (path? (syntax-source spec))
                  (path->relative-string/library (syntax-source spec)))
             (syntax-line spec)
             (syntax-e #'message)
             (map source-location (syntax->list #'(loc-piece ...)))
             #'(let-syntax ([subst 
                             (λ (stx)
                               (syntax-case stx ()
                                 [(_ loc-name ... non-loc-name ...)
                                  #'body]))])
                 (subst loc-piece ... non-loc-piece ...)
                 (void)))]))

(define (source-location stx)
  (list (and (path? (syntax-source stx))
             (path->relative-string/library (syntax-source stx))) 
        (syntax-line stx) 
        (syntax-column stx) 
        (syntax-position stx)
        (syntax-span stx)))

(define (read-tests path)
  (call-with-input-file path
    (λ (port)
      (port-count-lines! port)
      (let loop ()
        (define test (read-syntax path port))
        (if (eof-object? test)
            '()
            (cons test (loop)))))))

(define-syntax (test-syn-err stx)
  #'(void))

(define-syntax (test-runtime-err stx)
  #'(void)
  #;
  #`(parameterize ([current-compile (make-errortrace-compile-handler)])
      #,(test-error-location 
         stx
         eval
         #'[exn:fail? 
            (λ (exn) 
              (values (exn-message exn)
                      (let ([marks (continuation-mark-set->list
                                    (exn-continuation-marks exn)
                                    errortrace-key)])
                        (if (null? marks) #f (list (cdar marks))))))])))

(define tests 0)
(define failures 0)
(define (reset-count) 
  (set! tests 0)
  (set! failures 0))

(define (print-tests-passed filename)
  (cond
    [(= 0 failures)
     (printf "~a: all ~a tests passed.\n" filename tests)]
    [else
     (printf "~a: ~a test~a failed.\n" filename failures (if (= 1 failures) "" "s"))]))

(define (test/proc run expected line filename)
  ;(printf "testing line ~s:~s\n" filename line)
  (let ([got (with-handlers ((exn:fail? values)) (run))])
    (set! tests (+ tests 1))
    (unless (and (not (exn? got))
                 (matches? got expected))
      (set! failures (+ 1 failures))
      (eprintf "test: file ~a line ~a:\n     got ~s\nexpected ~s\n"
               filename
               line
               got
               expected)
      (when (exn:fail? got)
        ((error-display-handler) (exn-message got) got))
      (eprintf "\n"))))

(define (matches? got expected)
  (cond
    [(regexp? expected)
     (and (string? got) (regexp-match expected got) #t)]
    [else
     (equal/bindings? got expected)]))

;; equal/bindings? : any any -> boolean
;; compares two sexps (with embedded bindings) for equality.
;; uses an order-insensitive comparison for the bindings
(define (equal/bindings? fst snd)
  (let loop ([fst fst]
             [snd snd])
    (cond
      [(pair? fst)
       (and (pair? snd) 
            (loop (car fst) (car snd))
            (loop (cdr fst) (cdr snd)))]
      [(mtch? fst)
       (and (mtch? snd)
            (loop (mtch-bindings fst)
                  (mtch-bindings snd))
            (let ([g1 (gensym 'run-match-test-sym)])
              (equal/bindings? (mtch-context fst)
                               (mtch-context snd)))
            (equal/bindings? (mtch-hole fst)
                             (mtch-hole snd)))]
      [(bindings? fst)
       (and (bindings? snd)
            (let ([fst-table (bindings-table fst)]
                  [snd-table (bindings-table snd)])
              (and (= (length fst-table)
                      (length snd-table))
                   (andmap
                    loop
                    (sort fst-table rib-lt)
                    (sort snd-table rib-lt)))))]
      [(and (bind? fst)
            (bind? snd)
            (context? (bind-exp fst))
            (context? (bind-exp snd)))
       (and (equal? (bind-name fst) (bind-name snd))
            (let ([g (gensym 'run-match-test-sym2)])
              (equal/bindings? (bind-exp fst)
                               (bind-exp snd))))]
      [(and (hole? fst)
            (hole? snd))
       #t]
      [else (equal? fst snd)])))

;; rib-lt : rib rib -> boolean
(define (rib-lt r1 r2) (string<=? (format "~s" (bind-name r1))
                                  (format "~s" (bind-name r2))))

(define-syntax (test-contract-violation stx)
  (syntax-parse stx
    [(_ expr
        (~or (~once (~seq #:blaming blaming:expr))
             (~optional (~seq #:message message:expr)
                        #:defaults ([message #'""]))
             (~optional (~seq #:extract extract:expr)
                        #:defaults ([extract #'values])))
        ...)
     #`(test (with-handlers ([(λ (exn)
                                (let ([exn (extract exn)])
                                  (and (exn:fail:contract:blame? exn)
                                       (regexp-match? 
                                        blaming
                                        (format "~a" (blame-positive (exn:fail:contract:blame-object exn)))))))
                              exn-message])
               expr
               (gensym 'no-violation))
             #,(syntax/loc stx (regexp message)))]))
