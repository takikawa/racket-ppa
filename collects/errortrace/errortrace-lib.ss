#lang scheme/base

;; Poor man's stack-trace-on-exceptions/profiler.
;; See manual for information.

(require "stacktrace.ss"
         "errortrace-key.ss"
         scheme/contract
         scheme/unit
         scheme/runtime-path
         (for-syntax scheme/base))

(define oprintf
  (let ([op (current-output-port)])
    (λ args
      (apply fprintf op args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test coverage run-time support
(define test-coverage-enabled (make-parameter #f))

(define test-coverage-state '())
(define (initialize-test-coverage) (set! test-coverage-state '()))

(define (initialize-test-coverage-point expr)
  (when (and (syntax-position expr)
             (syntax-span expr))
    (set! test-coverage-state (cons (list (syntax-source expr)
                                          (syntax-position expr)
                                          (syntax-span expr))
                                    test-coverage-state))))

;; get-coverage : -> (values (listof (list src number number)) (listof (list src number number)))
;; the first result is a (minimized) set of ranges for all of the code that could be executed
;; the second result is the set of ranges that were actually executed.
(define (get-coverage) 
  (let* ([hash (test-coverage-info)]
         [all (hash-ref hash 'base '())]
         [covered '()])
    (hash-for-each hash (lambda (x y) (unless (eq? x 'base) (set! covered (cons x covered)))))
    (values all covered)))
    
(define (add-test-coverage-init-code stx)
  (syntax-case stx (#%plain-module-begin)
    [(mod name init-import (#%plain-module-begin b1 b2 body ...))
     #`(#,(namespace-module-identifier) name init-import
                                        #,(syntax-recertify
                                           #`(#%plain-module-begin
                                              b1 b2 ;; the two requires that were introduced earlier
                                              (#%plain-app init-test-coverage '#,(remove-duplicates test-coverage-state))
                                              body ...)
                                           (list-ref (syntax->list stx) 3)
                                           orig-inspector
                                           #f))]))

(define (annotate-covered-file filename-path [display-string #f])
  (annotate-file filename-path 
                 (map (λ (c) (cons (car c) (if (cdr c) 1 0))) (get-coverage))
                 display-string))


;; The next procedure is called by `annotate' and `annotate-top' to wrap
;; expressions with test suite coverage information.  Returning the
;; first argument means no tests coverage information is collected.

;; test-coverage-point : syntax syntax -> (values syntax info)
;; sets a test coverage point for a single expression
(define (test-coverage-point body expr phase)
  (if (and (test-coverage-enabled) (zero? phase))
      (syntax-case expr ()
        [(mod name . reste)
         (and (identifier? #'mod)
              (free-identifier=? #'mod 
                                 (namespace-module-identifier)
                                 (namespace-base-phase)))
         ;; don't annotate module expressions
         body]
        [_
         (cond
           [(and (syntax-source expr)
                 (number? (syntax-position expr))
                 (number? (syntax-position expr)))
            (initialize-test-coverage-point expr)
            (with-syntax ([src (datum->syntax #f (syntax-source expr) (quote-syntax here))]
                          [start-pos (syntax-position expr)]
                          [end-pos (+ (syntax-position expr) (syntax-span expr))]
                          [body body])
              #'(begin (#%plain-app test-covered '(src start-pos end-pos)) body))]
           [else
            body])])
      body))

;; remove-duplicates : (listof X) -> (listof X)
(define (remove-duplicates l)
  (let ([ht (make-hash)])
    (for-each (lambda (x) (hash-set! ht x #t)) l)
    (sort (hash-map ht (lambda (x y) x))
          (lambda (x y)
            (cond
              [(= (list-ref x 1) (list-ref y 1))
               (< (list-ref x 2) (list-ref y 2))]
              [else
               (< (list-ref x 1) (list-ref y 1))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiling run-time support

(define profile-thread-cell (make-thread-cell #f))
(define profile-key (gensym))

(define thread->profile-table (make-weak-hasheq))

(define profiling-enabled (make-parameter #f))
(define profiling-record-enabled (make-parameter #t))
(define profile-paths-enabled (make-parameter #f))

(define (clear-profile-results)
  (when (thread-cell-ref profile-thread-cell)
    (hash-for-each
     (thread-cell-ref profile-thread-cell)
     (lambda (k v)
       (set-box! (vector-ref v 0) #f)
       (vector-set! v 1 0)
       (vector-set! v 2 0)
       (vector-set! v 4 null)))))

(define (initialize-profile-point key name expr)
  (unless (thread-cell-ref profile-thread-cell)
    (let ([new-table (make-hasheq)])
      (hash-set! thread->profile-table (current-thread) new-table)
      (thread-cell-set! profile-thread-cell new-table)))
  (hash-set! (thread-cell-ref profile-thread-cell)
             key
             (vector (box #f) 0 0 (and name (syntax-e name)) expr null)))

(define (register-profile-start key)
  (and (profiling-record-enabled)
       (thread-cell-ref profile-thread-cell)
       (let ([v (hash-ref (thread-cell-ref profile-thread-cell) key #f)])
         (when v
           (let ([b (vector-ref v 0)])
             (vector-set! v 1 (add1 (vector-ref v 1)))
             (when (profile-paths-enabled)
               (let ([cms
                      (continuation-mark-set->list
                       (current-continuation-marks)
                       profile-key)])
                 (unless (hash? (vector-ref v 5))
                   (vector-set! v 5 (make-hash)))
                 (hash-set! (vector-ref v 5) cms 
                            (add1 (hash-ref (vector-ref v 5) cms (lambda () 0))))))
             (if (unbox b)
                 #f
                 (begin
                   (set-box! b #t)
                   (current-process-milliseconds))))))))

(define (register-profile-done key start)
  (when start
    (when (thread-cell-ref profile-thread-cell)
      (let ([v (hash-ref (thread-cell-ref profile-thread-cell) key #f)])
        (when v
          (let ([b (vector-ref v 0)])
            (set-box! b #f)
            (vector-set! v 2
                         (+ (- (current-process-milliseconds) start)
                            (vector-ref v 2)))))))))

(define (get-profile-results [t (current-thread)])
  (cond
    [(hash-ref thread->profile-table t #f)
     =>
     (λ (profile-info)
       (hash-map profile-info
                 (lambda (key val)
                   (let ([count (vector-ref val 1)]
                         [time (vector-ref val 2)]
                         [name (vector-ref val 3)]
                         [expr (vector-ref val 4)]
                         [cmss (vector-ref val 5)])
                     (list count time name expr
                           (if (hash? cmss)
                               (hash-map cmss (lambda (ks v)
                                                (cons v 
                                                      (map (lambda (k)
                                                             (let ([v (cdr (hash-ref profile-info k))])
                                                               (list (vector-ref v 2)
                                                                     (vector-ref v 3))))
                                                           ks))))
                               null))))))]
    [else '()]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stacktrace instrumenter

(define-runtime-path key-syntax
  '(lib "errortrace-key-syntax.ss" "errortrace"))

(define dynamic-errortrace-key
  (dynamic-require key-syntax 'errortrace-key-syntax))

;; with-mark : stx stx -> stx
(define (with-mark mark expr)
  (let ([loc (make-st-mark mark)])
    (if loc
        (with-syntax ([expr expr]
                      [loc loc]
                      [et-key dynamic-errortrace-key])
          (execute-point
           mark
           (syntax
            (with-continuation-mark et-key
              loc
              expr))))
        expr)))

(define-values/invoke-unit/infer stacktrace@)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute counts

(define execute-info (make-hasheq))

(define execute-counts-enabled (make-parameter #f))

(define (register-executed-once key)
  (let ([i (hash-ref execute-info key)])
    (set-mcdr! i (add1 (mcdr i)))))

(define (execute-point mark expr)
  (if (execute-counts-enabled)
      (let ([key (gensym)])
        (hash-set! execute-info key (mcons mark 0))
        (with-syntax ([key (datum->syntax #f key (quote-syntax here))]
                      [expr expr]
                      [register-executed-once register-executed-once]);<- 3D!
          (syntax
           (begin
             (register-executed-once 'key)
             expr))))
      expr))

(define (get-execute-counts)
  (hash-map execute-info (lambda (k v) (cons (mcar v)
                                             (mcdr v)))))

(define (annotate-executed-file name [display-string "^.,"])
  (annotate-file name (get-execute-counts) display-string))

;; shared functionality for annotate-executed-file and annotate-covered-file
(define (annotate-file name counts display-string)
  (let ([name (path->complete-path name (current-directory))])
    (let* (;; Filter relevant syntaxes
           [here (filter (lambda (s)
                           (and (equal? name (syntax-source (car s)))
                                (syntax-position (car s))))
                         counts)]
           ;; Sort them: earlier first, wider if in same position
           [sorted (sort here
                         (lambda (a b)
                           (let ([ap (syntax-position (car a))]
                                 [bp (syntax-position (car b))])
                             (or (< ap bp)
                                 (and (= ap bp)
                                      (> (syntax-span (car a))
                                         (syntax-span (car b))))))))]
           ;; Merge entries with the same position+span
           [sorted (if (null? sorted)
                       sorted ; guarantee one element for the next case
                       (let loop ([xs (reverse sorted)] [r '()])
                         (cond [(null? (cdr xs)) (append xs r)]
                               [(and (= (syntax-position (caar xs))
                                        (syntax-position (caadr xs)))
                                     (= (syntax-span (caar xs))
                                        (syntax-span (caadr xs))))
                                ;; doesn't matter which syntax object is kept,
                                ;; we only care about its position+span
                                (loop (cons (cons (caar xs)
                                                  (max (cdar xs) (cdadr xs)))
                                            (cddr xs))
                                      r)]
                               [else (loop (cdr xs) (cons (car xs) r))])))]
           [pic (make-string (file-size name) #\space)]
           [display-string
            (case display-string
              [(#t) "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
              [(#f) "#."]
              [else display-string])]
           [many-char (string-ref display-string
                                  (sub1 (string-length display-string)))])
      ;; Fill out picture
      (for-each (lambda (s)
                  (let ([pos (sub1 (syntax-position (car s)))]
                        [span (syntax-span (car s))]
                        [key (let ([k (cdr s)])
                               (if (< k (string-length display-string))
                                   (string-ref display-string k)
                                   many-char))])
                    (let loop ([p pos])
                      (unless (= p (+ pos span))
                        (string-set! pic p key)
                        (loop (add1 p))))))
                sorted)
      ;; Write annotated file
      (with-input-from-file name
        (lambda ()
          (let loop ()
            (let ([pos (file-position (current-input-port))]
                  [line (read-line (current-input-port) 'any)])
              (unless (eof-object? line)
                (printf "~a\n" line)
                (let ([w (string-length line)])
                  ;; Blank leading spaces in pic (copy them: works for tabs)
                  (let loop ([i 0])
                    (when (and (< i w)
                               (char-whitespace? (string-ref line i)))
                      (string-set! pic (+ pos i) (string-ref line i))
                      (loop (add1 i))))
                  (printf "~a\n" (substring pic pos (+ pos w))))
                (loop)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval handler, exception handler

(define instrumenting-enabled
  (make-parameter #t))
(define error-context-display-depth
  (make-parameter 10000 (lambda (x) (and (integer? x) x))))

;; port exn -> void
;; effect: prints out the context surrounding the exception
(define (print-error-trace p x)
  (let loop ([n (error-context-display-depth)]
             [l (map st-mark-source
                     (continuation-mark-set->list (exn-continuation-marks x)
                                                  errortrace-key))])
    (cond
      [(or (zero? n) (null? l)) (void)]
      [(pair? l)
       (let* ([stx (car l)]
              [source (syntax-source stx)]
              [file (cond
                      [(string? source) source]
                      [(path? source)
                       (path->string source)]
                      [(not source)
                       #f]
                      [else
                       (format "~a" source)])]
              [line (syntax-line stx)]
              [col (syntax-column stx)]
              [pos (syntax-position stx)])
         (fprintf p "~a~a: ~e~n"
                  (or file "[unknown source]")
                  (cond
                    [line (format ":~a:~a" line col)]
                    [pos (format "::~a" pos)]
                    [else ""])
                  (syntax->datum stx))
         (loop (- n 1) (cdr l)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile printer

(define (output-profile-results paths? sort-time?)
  (profiling-enabled #f)
  (error-print-width 50)
  (printf "Sorting profile data...~n")
  (let* ([sel (if sort-time? cadr car)]
         [counts (sort (filter (lambda (c) (positive? (car c)))
                               (get-profile-results))
                       (lambda (a b) (< (sel a) (sel b))))]
         [total 0])
    (for-each
     (lambda (c)
       (set! total (+ total (sel c)))
       (printf "=========================================================~n")
       (printf "time = ~a : no. = ~a : ~e in ~s~n"
               (cadr c) (car c) (caddr c) (cadddr c))
       ;; print call paths
       (when paths?
         (for-each
          (lambda (cms)
            (unless (null? (cdr cms))
              (printf "  ~e VIA ~e" (car cms) (caadr cms))
              (for-each
               (lambda (cm)
                 (printf " <- ~e" (car cm)))
               (cddr cms))
              (printf "~n")))
          (sort (cadddr (cdr c)) (lambda (a b) (> (car a) (car b)))))))
     counts)
    (printf "Total samples: ~a~n" total)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define orig-inspector (current-code-inspector))

(define errortrace-annotate
  (lambda (top-e)
    (define (normal e)
      (annotate-top (expand-syntax e) 
                    (namespace-base-phase)))
    (syntax-case top-e ()
      [(mod name . reste)
       (and (identifier? #'mod)
            (free-identifier=? #'mod 
                               (namespace-module-identifier)
                               (namespace-base-phase)))
       (if (eq? (syntax-e #'name) 'errortrace-key)
           top-e
           (let ([top-e (expand-syntax top-e)])
             (initialize-test-coverage)
             (syntax-case top-e (#%plain-module-begin)
               [(mod name init-import (#%plain-module-begin body ...))
                (add-test-coverage-init-code
                 (normal
                  #`(#,(namespace-module-identifier) name init-import
                                                     #,(syntax-recertify
                                                        #`(#%plain-module-begin
                                                           #,((make-syntax-introducer)
                                                              (syntax/loc (datum->syntax #f 'x #f)
                                                                (#%require errortrace/errortrace-key)))
                                                           #,((make-syntax-introducer)
                                                              (syntax/loc (datum->syntax #f 'x #f)
                                                                (#%require (for-syntax errortrace/errortrace-key))))
                                                           body ...)
                                                        (list-ref (syntax->list top-e) 3)
                                                        orig-inspector
                                                        #f))))])))]
      [_else
       (normal top-e)])))

(define-namespace-anchor orig-namespace)

(define (make-errortrace-compile-handler)
  (let ([orig (current-compile)]
        [reg (namespace-module-registry (current-namespace))])
    (namespace-attach-module (namespace-anchor->namespace orig-namespace) 'scheme/base)
    (namespace-attach-module (namespace-anchor->namespace orig-namespace) 'errortrace/errortrace-key)
    (lambda (e immediate-eval?)
      (orig
       (if (and (instrumenting-enabled)
                (eq? reg
                     (namespace-module-registry (current-namespace)))
                (not (compiled-expression? (if (syntax? e)
                                               (syntax-e e)
                                               e))))
           (let ([e2 (errortrace-annotate
                      (if (syntax? e)
                          e
                          (namespace-syntax-introduce
                           (datum->syntax #f e))))])
             e2)
           e)
       immediate-eval?))))

(define errortrace-compile-handler (make-errortrace-compile-handler))

(define errortrace-error-display-handler
  (let ([orig (error-display-handler)])
    (lambda (msg exn)
      (if (exn? exn)
          (let ([p (open-output-string)])
            (display (exn-message exn) p)
            (newline p)
            (print-error-trace p exn)
            (orig (get-output-string p) exn))
          (orig msg exn)))))

(provide/contract
 [annotate-covered-file (->* (path-string?) ((or/c string? #t #f)) void?)]
 [annotate-executed-file (->* (path-string?) ((or/c string? #t #f)) void?)])
(provide make-errortrace-compile-handler
         errortrace-compile-handler
         errortrace-error-display-handler
         errortrace-annotate
         
         print-error-trace
         error-context-display-depth
         
         instrumenting-enabled
         
         profiling-enabled
         profiling-record-enabled
         profile-paths-enabled
         get-profile-results
         output-profile-results
         clear-profile-results
         
         execute-counts-enabled
         get-execute-counts
         
         ;; need to rename here to avoid having to rename when the unit is invoked.
         (rename-out [test-coverage-enabled coverage-counts-enabled])
         get-coverage
         test-coverage-info
         
         annotate-top)
