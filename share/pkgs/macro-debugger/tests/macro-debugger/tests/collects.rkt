#lang racket/base
(require racket/list
         racket/path
         macro-debugger/model/debug
         rackunit)
(provide collects-tests
         modules-for-test
         trace-modules)

(define verbose (make-parameter #f))
(define (vprintf fmt . args) (when (verbose) (apply printf fmt args)))

(define (test-libs name mods #:reductions? [reductions? #f])
  (test-suite name
    (for ([m mods])
      (test-lib m #:reductions? reductions?))))

(define (test-lib m #:reductions? [reductions? #f])
  (test-case (format "~s" m)
    (vprintf "tracing ~s ... " m)
    (let ([deriv (trace-module m)])
      (check-pred deriv? deriv "Not a deriv")
      (check-pred ok-node? deriv "Expansion error")
      (when reductions?
        (vprintf "stepping ... ")
        (check-steps deriv hide-none-policy)))
    (vprintf "ok\n")))

(define (check-steps deriv policy)
  (define-values (steps binders uses stx exn)
    (parameterize ((macro-policy policy)) (reductions+ deriv)))
  (check-pred syntax? stx)
  (check-eq? exn #f)
  (check-pred list? steps "Expected list for steps")
  (check-pred reduction-sequence? steps))

(define (reduction-sequence? steps)
  (andmap protostep? steps))

;; ----

(define (make-tracing-module-name-resolver omnr table)
  (case-lambda
    [(mod rel stx load?)
     (when load?
       (when (not rel)
         (hash-set! table mod #t))
       (when rel
         (let ([abs (rel+mod->mod rel mod)])
           (when abs (hash-set! table abs #t)))))
     (omnr mod rel stx load?)]
    [args
     (apply omnr args)]))

(define (rel+mod->mod rel mod)
  (let* ([rel (resolved-module-path-name rel)]
         [rel (if (pair? rel) (car rel) rel)])
    (if (pair? mod)
        #f  ;; give up on submodules for now; FIXME
        (let-values ([(base file dir?) (split-path rel)])
          (path->mod (simplify-path (build-path base mod)))))))

(define (path->mod path)
  (cond [(for/or ([c (current-library-collection-paths)]) (path->mod* path c))
         => (lambda (l)
              (string->symbol
               (path->string
                (path-replace-suffix (apply build-path l) #""))))]
        [else #f]))

(define (path->mod* path base)
  (let loop ([path (explode-path path)] [base (explode-path base)])
    (cond [(null? base) path]
          [(and (pair? path) (pair? base) (equal? (car path) (car base)))
           (loop (cdr path) (cdr base))]
          [else #f])))

(define (trace-modules mods)
  (define table (make-hash))
  (parameterize ((current-module-name-resolver
                  (make-tracing-module-name-resolver
                   (current-module-name-resolver)
                   table))
                 (current-namespace (make-base-namespace)))
    (for ([mod mods])
      (dynamic-require mod (void)))
    (let* ([loaded
            (hash-map table (lambda (k v) k))]
           [syms
            (for/list ([l loaded] #:when (symbol? l)) l)]
           [libs
            (for/list ([l loaded] #:when (and (pair? l) (eq? (car l) 'lib))) l)]
           [conv-libs
            (for/list ([l libs])
                      (string->symbol
                       (string-append
                        (apply string-append
                               (for/list ([d (cddr l)]) (string-append d "/")))
                        (path->string (path-replace-suffix (cadr l) #"")))))])
      (sort (remove-duplicates (append syms conv-libs))
            string<?
            #:key symbol->string))))

;; ----

(define modules-for-test
  (trace-modules '(racket/main typed/racket framework)))

(define collects-tests
  (test-libs "Trace collections" modules-for-test))

(module+ test
  (module config info (define timeout 1200))
  (require rackunit/text-ui)
  (parameterize ((verbose #t))
    (run-tests
     (test-libs "Trace and step collections" modules-for-test #:reductions? #t))))
