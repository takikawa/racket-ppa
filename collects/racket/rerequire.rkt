#lang racket/base

(require syntax/modcode
         racket/path)

(provide dynamic-rerequire)

(define (dynamic-rerequire mod #:verbosity [verbosity 'reload])
  (unless (module-path? mod)
    (raise-argument-error 'dynamic-rerequire "module-path?" mod))
  (unless (memq verbosity '(all reload none))
    (raise-argument-error 'dynamic-rerequire "(or/c 'all 'reload 'none)" verbosity))
  (rerequire mod verbosity))

(struct mod (name timestamp depends))

(define loaded (make-hash))

(define (rerequire mod verbosity)
  (define loaded-paths '())
  (define (collect-loaded-path! path) (set! loaded-paths (cons path loaded-paths)))
  ;; Collect dependencies while loading:
  (parameterize ([current-load/use-compiled
                  (rerequire-load/use-compiled (current-load/use-compiled)
                                               #f verbosity collect-loaded-path!)])
    (dynamic-require mod 0))
  ;; Reload anything that's not up to date:
  (check-latest mod verbosity collect-loaded-path!)
  ;; Return a list of the paths that were loaded this time, in order:
  (reverse loaded-paths))

(define (rerequire-load/use-compiled orig re? verbosity path-collector)
  (define notify
    (if (or (eq? 'all verbosity) (and re? (eq? 'reload verbosity)))
      (lambda (path)
        (eprintf "  [~aloading ~a]\n" (if re? "re-" "") path)
        (path-collector path))
      path-collector))
  (lambda (path name)
    (if (and name
             (not (and (pair? name)
                       (not (car name)))))
        ;; Module load:
        (with-handlers ([(lambda (exn)
                           (and (pair? name)
                                (exn:get-module-code? exn)))
                         (lambda (exn) 
                           ;; Load-handler protocol: quiet failure when a
                           ;; submodule is not found
                           (void))])
          (let* ([code (get-module-code
                        path "compiled"
                        (lambda (e)
                          (parameterize ([compile-enforce-module-constants #f])
                            (compile e)))
                        (lambda (ext loader?) (load-extension ext) #f)
                        #:notify notify)]
                 [dir  (or (current-load-relative-directory) (current-directory))]
                 [path (path->complete-path path dir)]
                 [path (normal-case-path (simplify-path path))])
            ;; Record module timestamp and dependencies:
            (define-values (ts actual-path) (get-timestamp path))
            (let ([a-mod (mod name
                              ts
                              (if code
                                  (apply append 
                                         (map cdr (module-compiled-imports code)))
                                  null))])
              (hash-set! loaded path a-mod))
            ;; Evaluate the module:
            (parameterize ([current-module-declare-source actual-path])
              (eval code))))
      ;; Not a module, or a submodule that we shouldn't load from source:
      (begin (notify path) (orig path name)))))

(define (get-timestamp path)
  (let ([ts (file-or-directory-modify-seconds path #f (lambda () #f))])
    (if ts
        (values ts path)
        (if (path-has-extension? path #".rkt")
            (let* ([alt-path (path-replace-extension path #".ss")]
                   [ts (file-or-directory-modify-seconds alt-path #f (lambda () #f))])
              (if ts
                  (values ts alt-path)
                  (values -inf.0 path)))
            (values -inf.0 path)))))

(define (check-latest mod verbosity path-collector)
  (define mpi (module-path-index-join mod #f))
  (define done (make-hash))
  (let loop ([mpi mpi])
    (define rpath (module-path-index-resolve mpi))
    (define path (let ([p (resolved-module-path-name rpath)])
                   (if (pair? p) (car p) p)))
    (when (path? path)
      (define npath (normal-case-path path))
      (unless (hash-ref done npath #f)
        (hash-set! done npath #t)
        (define mod (hash-ref loaded npath #f))
        (when mod
          (for-each loop (mod-depends mod))
          (define-values (ts actual-path) (get-timestamp npath))
          (when (ts . > . (mod-timestamp mod))
            (define orig (current-load/use-compiled))
            (parameterize ([current-load/use-compiled
                            (rerequire-load/use-compiled orig #f verbosity path-collector)]
                           [current-module-declare-name rpath]
                           [current-module-declare-source actual-path])
              ((rerequire-load/use-compiled orig #t verbosity path-collector)
               npath (mod-name mod)))))))))
