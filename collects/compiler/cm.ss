#lang scheme/base
(require syntax/modcode
         syntax/modresolve
         setup/main-collects
         scheme/file
         scheme/list)

(provide make-compilation-manager-load/use-compiled-handler
         managed-compile-zo
         make-caching-managed-compile-zo
         trust-existing-zos
         manager-compile-notify-handler
         ;manager-skip-file-handler ;; not yet tested, so not yet exported
         (rename-out [trace manager-trace-handler]))

(define manager-compile-notify-handler (make-parameter void))
(define trace (make-parameter void))
(define indent (make-parameter ""))
(define trust-existing-zos (make-parameter #f))
(define manager-skip-file-handler (make-parameter (λ (x) #f)))

(define (trace-printf fmt . args)
  (let ([t (trace)])
    (unless (eq? t void)
      (t (string-append (indent) (apply format fmt args))))))

(define (get-deps code path)
  (filter-map (lambda (x)
                (let ([r (resolve-module-path-index x path)])
                  (and (path? r) (path->bytes r))))
              (append-map cdr (module-compiled-imports code))))

(define (get-compilation-dir+name mode path)
  (let-values ([(base name must-be-dir?) (split-path path)])
    (values 
     (cond
       [(eq? 'relative base) mode]
       [else (build-path base mode)])
     name)))

(define (get-compilation-path mode path)
  (let-values ([(dir name) (get-compilation-dir+name mode path)])
    (build-path dir name)))

(define (get-compilation-dir mode path)
  (let-values ([(dir name) (get-compilation-dir+name mode path)])
    dir))

(define (touch path)
  (close-output-port (open-output-file path #:exists 'append)))

(define (try-file-time path)
  ;; might be better to use a `with-handlers'
  (and (file-exists? path) (file-or-directory-modify-seconds path)))

(define (try-delete-file path)
  ;; Attempt to delete, but give up if it doesn't work:
  (with-handlers ([exn:fail:filesystem? void])
    (trace-printf "deleting: ~a" path)
    (delete-file path)))

(define (compilation-failure mode path zo-name date-path reason)
  (try-delete-file zo-name)
  (trace-printf "failure"))

;; with-compile-output : path (output-port -> alpha) -> alpha
;;  Open path for writing, and arranges to delete path if there's
;;  an exception. Breaks are managed so that the port is reliably
;;  closed and the file is reliably deleted if there's a break
(define (with-compile-output path proc)
  (let ([bp (current-break-parameterization)])
    (with-handlers ([void (lambda (exn) (try-delete-file path) (raise exn))])
      (let ([out (open-output-file path #:exists 'truncate/replace)])
        (dynamic-wind
          void
          (lambda ()
            (call-with-break-parameterization bp (lambda () (proc out))))
          (lambda ()
            (close-output-port out)))))))

(define (write-deps code mode path external-deps)
  (let ([dep-path (path-add-suffix (get-compilation-path mode path) #".dep")]
        [deps (remove-duplicates (get-deps code path))]
        [external-deps (remove-duplicates external-deps)])
    (with-compile-output dep-path
      (lambda (op)
        (write `(,(version)
                 ,@(map path->main-collects-relative deps)
                 ,@(map (lambda (x)
                          (cons 'ext (path->main-collects-relative x)))
                        external-deps))
               op)
        (newline op)))))

(define (format-time sec)
  (let ([d (seconds->date sec)])
    (format "~a-~a-~a ~a:~a:~a"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))

(define (verify-times ss-name zo-name)
  (define ss-sec (try-file-time ss-name)) ; should exist
  (define zo-sec (try-file-time zo-name))
  (cond [(not ss-sec) (error 'compile-zo "internal error")]
        [(not zo-sec) (error 'compile-zo "failed to create .zo file (~a) for ~a"
                             zo-name ss-name)]
        [(< zo-sec ss-sec) (error 'compile-zo
                                  "date for newly created .zo file (~a @ ~a) ~
                                   is before source-file date (~a @ ~a)~a"
                                  zo-name
                                  (format-time (seconds->date zo-sec))
                                  ss-name
                                  (format-time (seconds->date ss-sec))
                                  (if (> ss-sec (current-seconds))
                                    ", which appears to be in the future"
                                    ""))]))

(define-struct ext-reader-guard (proc top)
  #:property prop:procedure (struct-field-index proc))
(define-struct file-dependency (path) #:prefab)

(define (compile-zo* mode path read-src-syntax zo-name)
  ;; External dependencies registered through reader guard and
  ;; accomplice-logged events:
  (define external-deps null)
  (define deps-sema (make-semaphore 1))
  (define done-key (gensym))
  (define (external-dep! p)
    (call-with-semaphore
     deps-sema
     (lambda ()
       (set! external-deps (cons (path->bytes p) external-deps)))))

  ;; Set up a logger to receive and filter accomplice events:
  (define accomplice-logger (make-logger))
  (define log-th
    (let ([orig-log (current-logger)]
          [receiver (make-log-receiver accomplice-logger 'info)])
      (thread (lambda ()
                (let loop ()
                  (let ([l (sync receiver)])
                    (unless (eq? (vector-ref l 2) done-key)
                      (if (and (eq? (vector-ref l 0) 'info)
                               (file-dependency? (vector-ref l 2))
                               (path? (file-dependency-path (vector-ref l 2))))
                        (external-dep! (file-dependency-path (vector-ref l 2)))
                        (log-message orig-log (vector-ref l 0) (vector-ref l 1)
                                     (vector-ref l 2)))
                      (loop))))))))

  ;; Compile the code:
  (define code
    (parameterize ([current-reader-guard
                    (let* ([rg (current-reader-guard)]
                           [rg (if (ext-reader-guard? rg)
                                 (ext-reader-guard-top rg)
                                 rg)])
                      (make-ext-reader-guard
                       (lambda (d)
                         ;; Start by calling the top installed guard to
                         ;; transform the module path, avoiding redundant
                         ;; dependencies by avoiding accumulation of these
                         ;; guards.
                         (let ([d (rg d)])
                           (when (module-path? d)
                             (let ([p (resolved-module-path-name
                                       (module-path-index-resolve
                                        (module-path-index-join d #f)))])
                               (when (path? p) (external-dep! p))))
                           d))
                       rg))]
                   [current-logger accomplice-logger])
      (get-module-code path mode compile
                       (lambda (a b) #f) ; extension handler
                       #:source-reader read-src-syntax)))
  (define code-dir (get-compilation-dir mode path))

  ;; Wait for accomplice logging to finish:
  (log-message accomplice-logger 'info "stop" done-key)
  (sync log-th)

  ;; Write the code and dependencies:
  (when code
    (make-directory* code-dir)
    (with-compile-output zo-name
      (lambda (out)
        (with-handlers ([exn:fail?
                         (lambda (ex)
                           (close-output-port out)
                           (compilation-failure mode path zo-name #f
                                                (exn-message ex))
                           (raise ex))])
          (parameterize ([current-write-relative-directory
                          (let-values ([(base name dir?) (split-path path)])
                            (if (eq? base 'relative)
                              (current-directory)
                              (path->complete-path base (current-directory))))])
            (write code out)))
        ;; redundant, but close as early as possible:
        (close-output-port out)
        ;; Note that we check time and write .deps before returning from
        ;; with-compile-output...
        (verify-times path zo-name)
        (write-deps code mode path external-deps)))))

(define (compile-zo mode path read-src-syntax)
  ((manager-compile-notify-handler) path)
  (trace-printf "compiling: ~a" path)
  (parameterize ([indent (string-append "  " (indent))])
    (let* ([zo-name (path-add-suffix (get-compilation-path mode path) #".zo")]
           [zo-exists? (file-exists? zo-name)])
      (if (and zo-exists? (trust-existing-zos))
          (touch zo-name)
          (begin (when zo-exists? (delete-file zo-name))
                 (with-handlers
                     ([exn:get-module-code?
                       (lambda (ex)
                         (compilation-failure mode path zo-name
                                              (exn:get-module-code-path ex)
                                              (exn-message ex))
                         (raise ex))])
                   (compile-zo* mode path read-src-syntax zo-name))))))
  (trace-printf "end compile: ~a" path))

(define (get-compiled-time mode path)
  (define-values (dir name) (get-compilation-dir+name mode path))
  (or (try-file-time (build-path dir "native" (system-library-subpath)
                                 (path-add-suffix name (system-type
                                                        'so-suffix))))
      (try-file-time (build-path dir (path-add-suffix name #".zo")))
      -inf.0))

(define (compile-root mode path0 up-to-date read-src-syntax)
  (define path (simplify-path (cleanse-path path0)))
  (define (read-deps)
    (with-handlers ([exn:fail:filesystem? (lambda (ex) (list (version)))])
      (call-with-input-file
          (path-add-suffix (get-compilation-path mode path) #".dep")
        read)))
  (define (do-check)
    (define path-zo-time (get-compiled-time mode path))
    (define path-time (try-file-time path))
    (cond
      [(not path-time)
       (trace-printf "~a does not exist" path)
       path-zo-time]
      [else
       (cond
         [(> path-time path-zo-time)
          (trace-printf "newer src...")
          (compile-zo mode path read-src-syntax)]
         [else
          (let ([deps (read-deps)])
            (cond
              [(not (and (pair? deps) (equal? (version) (car deps))))
               (trace-printf "newer version...")
               (compile-zo mode path read-src-syntax)]
              [(ormap
                (lambda (p)
                  ;; (cons 'ext rel-path) => a non-module file (check date)
                  ;; rel-path => a module file name (check transitive dates)
                  (define ext? (and (pair? p) (eq? 'ext (car p))))
                  (define d (main-collects-relative->path (if ext? (cdr p) p)))
                  (define t
                    (if ext?
                      (try-file-time d)
                      (compile-root mode d up-to-date read-src-syntax)))
                  (and t (> t path-zo-time)
                       (begin (trace-printf "newer: ~a (~a > ~a)..."
                                            d t path-zo-time)
                              #t)))
                (cdr deps))
               (compile-zo mode path read-src-syntax)]))])
       (let ([stamp (get-compiled-time mode path)])
         (hash-set! up-to-date path stamp)
         stamp)]))
  (or (and up-to-date (hash-ref up-to-date path #f))
      ((manager-skip-file-handler) path)
      (begin (trace-printf "checking: ~a" path)
             (do-check))))

(define (managed-compile-zo zo [read-src-syntax read-syntax])
  ((make-caching-managed-compile-zo read-src-syntax) zo))

(define (make-caching-managed-compile-zo [read-src-syntax read-syntax])
  (let ([cache (make-hash)])
    (lambda (zo)
      (parameterize ([current-load/use-compiled
                      (make-compilation-manager-load/use-compiled-handler/table
                       cache)])
        (compile-root (car (use-compiled-file-paths))
                      (path->complete-path zo)
                      cache read-src-syntax)
        (void)))))

(define (make-compilation-manager-load/use-compiled-handler)
  (make-compilation-manager-load/use-compiled-handler/table (make-hash)))

(define (make-compilation-manager-load/use-compiled-handler/table cache)
  (let ([orig-eval (current-eval)]
        [orig-load (current-load)]
        [orig-registry (namespace-module-registry (current-namespace))]
        [default-handler (current-load/use-compiled)]
        [modes (use-compiled-file-paths)])
    (define (compilation-manager-load-handler path mod-name)
      (cond [(not mod-name)
             (trace-printf "skipping:  ~a mod-name ~s" path mod-name)]
            [(or (null? (use-compiled-file-paths))
                 (not (equal? (car modes)
                              (car (use-compiled-file-paths)))))
             (trace-printf "skipping:  ~a compiled-paths's first element changed; current value ~s, first element was ~s"
                           path 
                           (use-compiled-file-paths)
                           (car modes))]
            [(not (eq? compilation-manager-load-handler
                       (current-load/use-compiled)))
             (trace-printf "skipping:  ~a current-load/use-compiled changed ~s"
                           path (current-load/use-compiled))]
            [(not (eq? orig-eval (current-eval)))
             (trace-printf "skipping:  ~a orig-eval ~s current-eval ~s"
                           path orig-eval (current-eval))]
            [(not (eq? orig-load (current-load)))
             (trace-printf "skipping:  ~a orig-load ~s current-load ~s"
                           path orig-load (current-load))]
            [(not (eq? orig-registry
                       (namespace-module-registry (current-namespace))))
             (trace-printf "skipping:  ~a orig-registry ~s current-registry ~s"
                           path orig-registry
                           (namespace-module-registry (current-namespace)))]
            [else
             (trace-printf "processing: ~a" path)
             (compile-root (car modes) path cache read-syntax)
             (trace-printf "done: ~a" path)])
      (default-handler path mod-name))
    (when (null? modes)
      (raise-mismatch-error 'make-compilation-manager-...
                            "empty use-compiled-file-paths list: "
                            modes))
    compilation-manager-load-handler))
