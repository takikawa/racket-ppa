#lang racket/base
(require syntax/modcode
         syntax/modresolve
         syntax/modread
         setup/dirs
         racket/file
         racket/list
         racket/path
         racket/promise
         openssl/sha1
         racket/place
         setup/collects
         compiler/compilation-path
         compiler/private/dep
         racket/contract/base)

(provide make-compilation-manager-load/use-compiled-handler
         managed-compile-zo
         make-caching-managed-compile-zo
         trust-existing-zos
         manager-compile-notify-handler
         manager-skip-file-handler
         file-stamp-in-collection
         file-stamp-in-paths
         manager-trace-handler
         get-file-sha1
         get-compiled-file-sha1
         with-compile-output
         
         managed-compiled-context-key
         make-compilation-context-error-display-handler
         
         parallel-lock-client
         make-compile-lock
         compile-lock->parallel-lock-client
         
         install-module-hashes!

         (contract-out
          [current-path->mode
           (parameter/c (or/c #f (-> path? (and/c path? relative-path?))))]))

(define current-path->mode (make-parameter #f))

(define cm-logger (make-logger 'compiler/cm (current-logger)))
(define (default-manager-trace-handler str)
  (when (log-level? cm-logger 'debug)
    (log-message cm-logger 'debug str (current-inexact-milliseconds))))

(struct compile-event (timestamp path action) #:prefab)
(define (log-compile-event path action)
  (when (log-level? cm-logger 'info 'compiler/cm)
    (log-message cm-logger 'info (format "~a~a: ~a" (get-indent-string) action path)
                 (compile-event (current-inexact-milliseconds) path action))))

(define manager-compile-notify-handler (make-parameter void))
(define manager-trace-handler (make-parameter default-manager-trace-handler))
(define indent (make-parameter 0))
(define trust-existing-zos (make-parameter #f))
(define manager-skip-file-handler (make-parameter (λ (x) #f)))
(define depth (make-parameter 0))
(define parallel-lock-client (make-parameter #f))

(define managed-compiled-context-key (gensym))
(define (make-compilation-context-error-display-handler orig)
  (lambda (str exn)
    (define l (continuation-mark-set->list
               (exn-continuation-marks exn)
               managed-compiled-context-key))
    (orig (if (null? l)
              str
              (apply
               string-append
               str
               "\n  compilation context...:"
               (for/list ([i (in-list l)])
                 (format "\n   ~a" i))))
          exn)))

(define (file-stamp-in-collection p)
  (file-stamp-in-paths p (current-library-collection-paths)))

(define (file-stamp-in-paths p paths)
  (let ([p-eles (explode-path (simple-form-path p))])
    (let c-loop ([paths paths])
      (cond
        [(null? paths) #f]
        [else
         (let i-loop ([collects-eles (explode-path (simple-form-path (car paths)))]
                      [p-eles p-eles])
           (cond
             [(null? collects-eles)
              ;; we're inside the collection hierarchy, so we just 
              ;; use the date of the original file (or the zo, whichever
              ;; is newer).
              (let-values ([(base name dir) (split-path p)])
                (let* ([p-date (file-or-directory-modify-seconds p #f (lambda () #f))]
                       [alt-date (and (not p-date)
                                      (file-or-directory-modify-seconds 
                                       (rkt->ss p) 
                                       #f 
                                       (lambda () #f)))]
                       [date (or p-date alt-date)]
                       [get-path (lambda ()
                                   (if p-date
                                       p
                                       (rkt->ss p)))]
                       [modes (use-compiled-file-paths)]
                       [roots (current-compiled-file-roots)]
                       [get-zo-date+mode (lambda (name)
                                           (ormap
                                            (lambda (root)
                                              (ormap
                                               (lambda (mode)
                                                 (let ([v (file-or-directory-modify-seconds
                                                           (build-path 
                                                            (reroot-path* base root)
                                                            mode
                                                            (path-add-extension name #".zo"))
                                                           #f
                                                           (lambda () #f))])
                                                   (and v (list* v mode root))))
                                               modes))
                                            roots))]
                       [main-zo-date+mode (and (or p-date (not alt-date))
                                               (get-zo-date+mode name))]
                       [alt-zo-date+mode (and (or alt-date
                                                  (and (not p-date) 
                                                       (not alt-date)
                                                       (not main-zo-date+mode)))
                                              (get-zo-date+mode (rkt->ss name)))]
                       [zo-date+mode (or main-zo-date+mode alt-zo-date+mode)]
                       [zo-date (and zo-date+mode (car zo-date+mode))]
                       [get-zo-path (lambda ()
                                      (let-values ([(name mode root)
                                                    (if main-zo-date+mode
                                                        (values (path-add-extension name #".zo")
                                                                (cadr main-zo-date+mode)
                                                                (cddr main-zo-date+mode))
                                                        (values (path-add-extension (rkt->ss name) #".zo")
                                                                (cadr alt-zo-date+mode)
                                                                (cddr alt-zo-date+mode)))])
                                        (build-path (reroot-path* base root) mode name)))])
                  (cond
                   [(and zo-date
                         (or (not date)
                             (zo-date . > . date)))
                    (cons zo-date
                          (delay (get-compiled-file-sha1 (get-zo-path))))]
                   [date
                    (cons date
                          (delay (get-source-sha1 (get-path))))]
                   [else #f])))]
             [(null? p-eles) 
              ;; this case shouldn't happen... I think.
              (c-loop (cdr paths))]
             [else
              (cond
                [(equal? (car p-eles) (car collects-eles))
                 (i-loop (cdr collects-eles) (cdr p-eles))]
                [else 
                 (c-loop (cdr paths))])]))]))))

(define (path*->collects-relative p)
  (if (bytes? p)
      (let ([q (path->collects-relative (bytes->path p))])
        (if (path? q)
            (path->bytes q)
            q))
      (path->collects-relative p)))

(define (collects-relative*->path p cache)
  (if (bytes? p)
      (bytes->path p)
      (hash-ref! cache p (lambda () (collects-relative->path p)))))

(define (reroot-path* base root)
  (cond
   [(eq? root 'same) base]
   [(relative-path? root)
    (build-path base root)]
   [else
    (reroot-path base root)]))

(define (trace-printf fmt . args)
  (let ([t (manager-trace-handler)])
    (unless (or (eq? t void)
                (and (equal? t default-manager-trace-handler)
                     (not (log-level? cm-logger 'debug))))
      (t (string-append (get-indent-string)
                        (apply format fmt args))))))

(define (get-indent-string)
  (build-string (indent)
                (λ (x)
                  (if (and (= 2 (modulo x 3))
                           (not (= x (- (indent) 1))))
                      #\|
                      #\space))))

(define (get-deps code path)
  (define ht
    (let loop ([code code] [ht (hash)])
      (define new-ht
        (for*/fold ([ht ht]) ([imports (in-list (module-compiled-imports code))]
                              [x (in-list (cdr imports))])
          (let* ([r (resolve-module-path-index x path)]
                 [r (if (pair? r) (cadr r) r)])
            (if (and (path? r) 
                     (not (equal? path r))
                     (not (equal? path r))
                     (not (equal? path (rkt->ss r))))
                (hash-set ht (path->bytes r) #t)
                ht))))
      (for*/fold ([ht new-ht]) ([non-star? (in-list '(#f #t))]
                                [subcode (in-list (module-compiled-submodules code non-star?))])
        (loop subcode ht))))
  (for/list ([k (in-hash-keys ht)]) k))

(define (get-compilation-path path->mode roots path)
  (let-values ([(dir name) (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots roots)])
    (build-path dir name)))

(define (touch path)
  (with-compiler-security-guard
   (file-or-directory-modify-seconds 
    path
    (current-seconds)
    (lambda ()
      (close-output-port (open-output-file path #:exists 'append))))))

(define (try-file-time path)
  (file-or-directory-modify-seconds path #f (lambda () #f)))

(define (try-delete-file path [noisy? #t])
  ;; Attempt to delete, but give up if it doesn't work:
  (with-handlers ([exn:fail:filesystem? void])
    (when noisy? (trace-printf "deleting ~a" path))
    (with-compiler-security-guard (delete-file path))))

(define (compilation-failure path->mode roots path zo-name date-path reason)
  (try-delete-file zo-name)
  (trace-printf "failure"))

;; with-compile-output : path (output-port path -> alpha) -> alpha
(define (with-compile-output path proc)
  (call-with-atomic-output-file 
   path
   #:security-guard (pick-security-guard)
   proc))

(define-syntax-rule
  (with-compiler-security-guard expr)
  (parameterize ([current-security-guard (pick-security-guard)]) 
    expr))

(define compiler-security-guard (make-parameter #f))

(define (pick-security-guard)
  (or (compiler-security-guard)
      (current-security-guard)))

(define (get-source-sha1 p)
  (with-handlers ([exn:fail:filesystem? (lambda (exn)
                                          (and (path-has-extension? p #".rkt")
                                               (get-source-sha1 (path-replace-extension p #".ss"))))])
    (call-with-input-file* p sha1)))

(define (get-dep-sha1s deps up-to-date collection-cache read-src-syntax path->mode roots must-exist? seen)
  (let ([l (for/fold ([l null]) ([dep (in-list deps)])
             (and l
                  (let* ([ext? (external-dep? dep)]
                         [p (collects-relative*->path (dep->encoded-path dep) collection-cache)])
                    (cond
                     [ext? (let ([v (get-source-sha1 p)])
                             (cond
                              [v (cons (cons (delay v) dep) l)]
                              [must-exist? (error 'cm "cannot find external-dependency file: ~v" p)]
                              [else #f]))]
                     [(or (hash-ref up-to-date (simple-form-path p) #f)
                          ;; Use `compile-root' with `sha1-only?' as #t:
                          (compile-root path->mode roots p up-to-date collection-cache read-src-syntax #t seen))
                      => (lambda (sh)
                           (cons (cons (cdr sh) dep) l))]
                     [must-exist?
                      ;; apparently, we're forced to use the source of the module,
                      ;; so compute a sha1 from it instead of the bytecode
                      (cons (cons (get-source-sha1 p) dep) l)]
                     [else #f]))))])
    (and l
         (let ([p (open-output-string)]
               [l (map (lambda (v) 
                         (let ([sha1 (force (car v))]
                               [dep (cdr v)])
                           (unless sha1
                             (error 'cm "no SHA-1 for dependency: ~s" dep))
                           (cons sha1 dep)))
                       l)])
           ;; sort by sha1s so that order doesn't matter
           (write (sort l string<? #:key car) p)
           ;; compute one hash from all hashes
           (sha1 (open-input-bytes (get-output-bytes p)))))))

(define (write-deps code path->mode roots path src-sha1
                    external-deps external-module-deps reader-deps 
                    up-to-date collection-cache read-src-syntax)
  (let ([dep-path (path-add-extension (get-compilation-path path->mode roots path) #".dep")]
        [deps (remove-duplicates (append (get-deps code path)
                                         external-module-deps ; can create cycles if misused!
                                         reader-deps))]
        [external-deps (remove-duplicates external-deps)])
    (define (path*->collects-relative/maybe-indirect dep)
      (if (and (pair? dep) (eq? 'indirect (car dep)))
          (cons 'indirect (path*->collects-relative (cdr dep)))
          (path*->collects-relative dep)))
    (with-compile-output dep-path
      (lambda (op tmp-path)
        (let ([deps (append
                     (map path*->collects-relative/maybe-indirect deps)
                     (map (lambda (x)
                            (define d (path*->collects-relative/maybe-indirect x))
                            (if (and (pair? d) (eq? 'indirect d))
                                (cons 'indirect (cons 'ext (cdr d)))
                                (cons 'ext d)))
                          external-deps))])
          (write (list* (version)
                        (cons (or src-sha1 (get-source-sha1 path))
                              (get-dep-sha1s deps up-to-date collection-cache read-src-syntax path->mode roots #t #hash()))
                        (sort deps s-exp<?))
                 op)
          (newline op))))))

(define (s-exp<? a b)
  (string<? (format "~s" a) (format "~s" b)))

(define (format-time sec)
  (let ([d (seconds->date sec)])
    (format "~a-~a-~a ~a:~a:~a"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))

(define (verify-times ss-name zo-name)
  (define ss-sec (file-or-directory-modify-seconds ss-name))
  (define zo-sec (try-file-time zo-name))
  (cond [(not ss-sec) (error 'compile-zo "internal error")]
        [(not zo-sec) (error 'compile-zo "failed to create .zo file (~a) for ~a"
                             zo-name ss-name)]
        [(< zo-sec ss-sec) (error 'compile-zo
                                  "date for newly created .zo file (~a @ ~a) ~
                                   is before source-file date (~a @ ~a)~a"
                                  zo-name (format-time zo-sec)
                                  ss-name (format-time ss-sec)
                                  (if (> ss-sec (current-seconds))
                                    ", which appears to be in the future"
                                    ""))]))

(define-struct ext-reader-guard (proc top)
  #:property prop:procedure (struct-field-index proc))
(define-struct file-dependency (path module?) #:prefab)
(define-struct (file-dependency/options file-dependency) (table) #:prefab)

(define (compile-zo* path->mode roots path src-sha1 read-src-syntax zo-name up-to-date collection-cache)
  ;; The `path' argument has been converted to .rkt or .ss form,
  ;;  as appropriate.
  ;; External dependencies registered through reader guard and
  ;; accomplice-logged events:
  (define external-deps null)
  (define external-module-deps null)
  (define reader-deps null)
  (define deps-sema (make-semaphore 1))
  (define done-key (gensym))
  (define (external-dep! p module? indirect?)
    (define bstr (path->bytes p))
    (define dep (if indirect?
                    (cons 'indirect bstr)
                    bstr))
    (if module?
        (set! external-module-deps (cons dep external-module-deps))
        (set! external-deps (cons dep external-deps))))
  (define (reader-dep! p)
    (call-with-semaphore
     deps-sema
     (lambda ()
       (set! reader-deps (cons (path->bytes p) reader-deps)))))

  ;; Set up a logger to receive and filter accomplice events:
  (define accomplice-logger (make-logger #f (current-logger)
                                         ;; Don't propoagate 'cm-accomplice events, so that
                                         ;; enclosing compilations don't see events intended
                                         ;; for this one:
                                         'none 'cm-accomplice
                                         ;; Propagate everything else:
                                         'debug))
  (define receiver (make-log-receiver accomplice-logger 'info 'cm-accomplice))
  
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
                             (let* ([p (resolved-module-path-name
                                        (module-path-index-resolve
                                         (module-path-index-join d #f)))]
                                    [p (if (pair? p)
                                           ;; Create a dependency only if 
                                           ;; the corresponding submodule is
                                           ;; declared:
                                           (if (module-declared? d #t)
                                               (car p)
                                               #f)
                                           p)])
                               (when (path? p) (reader-dep! p))))
                           d))
                       rg))]
                   [current-logger accomplice-logger])
      (with-continuation-mark
        managed-compiled-context-key
        path
        (get-module-code path (path->mode path) compile
                         (lambda (a b) #f) ; extension handler
                         #:source-reader read-src-syntax))))
  (define dest-roots (list (car roots)))
  (define code-dir (get-compilation-dir path #:modes (list (path->mode path)) #:roots dest-roots))

  ;; Get all accomplice data:
  (let loop ()
    (let ([l (sync/timeout 0 receiver)])
      (when l
        (when (and (eq? (vector-ref l 0) 'info)
                   (file-dependency? (vector-ref l 2))
                   (path? (file-dependency-path (vector-ref l 2))))
          (external-dep! (file-dependency-path (vector-ref l 2))
                         (file-dependency-module? (vector-ref l 2))
                         (and (file-dependency/options? (vector-ref l 2))
                              (hash-ref (file-dependency/options-table (vector-ref l 2))
                                        'indirect
                                        #f))))
        (loop))))

  ;; Write the code and dependencies:
  (when code
    (with-compiler-security-guard (make-directory* code-dir))
    (with-compile-output zo-name
      (lambda (out tmp-name)
        (with-handlers ([exn:fail?
                         (lambda (ex)
                           (close-output-port out)
                           (compilation-failure path->mode dest-roots path zo-name #f
                                                (exn-message ex))
                           (raise ex))])
          (parameterize ([current-write-relative-directory
                          (let* ([dir
                                  (let-values ([(base name dir?) (split-path path)])
                                    (if (eq? base 'relative)
                                        (current-directory)
                                        (path->complete-path base (current-directory))))]
                                 [collects-dir (find-collects-dir)]
                                 [e-dir (explode-path dir)]
                                 [e-collects-dir (explode-path collects-dir)])
                            (if (and ((length e-dir) . > . (length e-collects-dir))
                                     (for/and ([a (in-list e-dir)]
                                               [b (in-list e-collects-dir)])
                                       (equal? a b)))
                                ;; `dir' extends `collects-dir':
                                (cons dir collects-dir)
                                ;; `dir' doesn't extend `collects-dir':
                                dir))])
            (let ([b (open-output-bytes)])
              ;; Write bytecode into string
              (write code b)
              ;; Compute SHA1 over modules within bytecode
              (let* ([s (get-output-bytes b)])
                (install-module-hashes! s)
                ;; Write out the bytecode with module hash
                (write-bytes s out)))))
        ;; redundant, but close as early as possible:
        (close-output-port out)
        ;; Note that we check time and write .deps before returning from
        ;; with-compile-output...
        (verify-times path tmp-name)
        (write-deps code path->mode dest-roots path src-sha1
                    external-deps external-module-deps reader-deps 
                    up-to-date collection-cache read-src-syntax)))
    (trace-printf "wrote zo file: ~a" zo-name)))

(define (install-module-hashes! s [start 0] [len (bytes-length s)])
  (define vlen (bytes-ref s (+ start 2)))
  (define mode (integer->char (bytes-ref s (+ start 3 vlen))))
  (case mode
    [(#\T)
     ;; A single module:
     (define h (sha1-bytes (open-input-bytes (if (and (zero? start)
                                                      (= len (bytes-length s)))
                                                 s
                                                 (subbytes s start (+ start len))))))
     ;; Write sha1 for module hash:
     (bytes-copy! s (+ start 4 vlen) h)]
    [(#\D)
     ;; A directory form modules and submodules. The format starts with <count>,
     ;; and then it's <count> records of the format:
     ;;  <name-len> <name-bytes> <mod-pos> <mod-len> <left-pos> <right-pos>
     (define (read-num rel-pos)
       (define pos (+ start rel-pos))
       (integer-bytes->integer s #t #f pos (+ pos 4)))
     (define count (read-num (+ 4 vlen)))
     (for/fold ([pos (+ 8 vlen)]) ([i (in-range count)])
       (define pos-pos (+ pos 4 (read-num pos)))
       (define mod-start (read-num pos-pos))
       (define mod-len (read-num (+ pos-pos 4)))
       (install-module-hashes! s (+ start mod-start) mod-len)
       (+ pos-pos 16))
     (void)]
    [else 
     ;; ?? unknown mode
     (void)]))

(define (actual-source-path path)
  (if (file-exists? path) 
      path
      (let ([alt-path (rkt->ss path)])
        (if (file-exists? alt-path)
            alt-path
            path))))

(define (maybe-compile-zo sha1-only? deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache seen)
  (let ([actual-path (actual-source-path orig-path)])
    (unless sha1-only?
      ((manager-compile-notify-handler) actual-path)
      (trace-printf "maybe-compile-zo starting ~a" actual-path))
    (begin0
     (parameterize ([indent (+ 2 (indent))])
       (let* ([zo-name (path-add-extension (get-compilation-path path->mode roots path) #".zo")]
              [zo-exists? (file-exists? zo-name)])
         (if (and zo-exists? (trust-existing-zos))
             (begin
               (trace-printf "trusting: ~a" zo-name)
               (touch zo-name)
               #f)
             (let ([src-sha1 (and zo-exists?
                                  deps
                                  (cadr deps)
                                  (get-source-sha1 path))])
               (if (and zo-exists?
                        src-sha1
                        (equal? src-sha1 (caadr deps))
                        (equal? (get-dep-sha1s (cddr deps) up-to-date collection-cache read-src-syntax path->mode roots #f seen)
                                (cdadr deps)))
                   (begin
                     (trace-printf "hash-equivalent: ~a" zo-name)
                     (touch zo-name)
                     #f)
                   ((if sha1-only? values (lambda (build) (build) #f))
                    (lambda ()
                      (let* ([lc (parallel-lock-client)]
                             [_ (when lc (log-compile-event path 'locking))]
                             [locked? (and lc (lc 'lock zo-name))]
                             [ok-to-compile? (or (not lc) locked?)])
                        (dynamic-wind
                          (lambda () (void))
                          (lambda ()
                            (when ok-to-compile?
                              (log-compile-event path 'start-compile)
                              (when zo-exists? (try-delete-file zo-name #f))
                              (trace-printf (format "compiling ~a" actual-path))
                              (parameterize ([depth (+ (depth) 1)])
                                (with-handlers
                                    ([exn:get-module-code?
                                      (lambda (ex)
                                        (compilation-failure path->mode roots path zo-name
                                                             (exn:get-module-code-path ex)
                                                             (exn-message ex))
                                        (raise ex))])
                                  (compile-zo* path->mode roots path src-sha1 read-src-syntax zo-name up-to-date collection-cache)))
                              (trace-printf "compiled ~a" actual-path)))
                          (lambda ()
                            (when lc
                              (log-compile-event path (if locked? 'finish-compile 'already-done)))
                            (when locked?
                              (lc 'unlock zo-name))))))))))))
     (unless sha1-only?
       (trace-printf "maybe-compile-zo finished ~a" actual-path)))))

(define (get-compiled-time path->mode roots path)
  (define-values (dir name) (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots roots))
  (or (try-file-time (build-path dir "native" (system-library-subpath)
                                 (path-add-extension name (system-type
                                                           'so-suffix))))
      (try-file-time (build-path dir (path-add-extension name #".zo")))))

(define (try-file-sha1 path dep-path)
  (with-module-reading-parameterization
   (lambda ()
     (with-handlers ([exn:fail:filesystem? (lambda (exn) #f)])
       (string-append
        (call-with-input-file* path sha1)
        (with-handlers ([exn:fail:filesystem? (lambda (exn) "")])
          (call-with-input-file* dep-path (lambda (p) (cdadr (read p))))))))))

(define (get-compiled-sha1 path->mode roots path)
  (define-values (dir name) (get-compilation-dir+name path #:modes (list (path->mode path)) #:roots roots))
  (let ([dep-path (build-path dir (path-add-extension name #".dep"))])
    (or (try-file-sha1 (build-path dir "native" (system-library-subpath)
                                   (path-add-extension name (system-type
                                                             'so-suffix)))
                       dep-path)
        (try-file-sha1 (build-path dir (path-add-extension name #".zo"))
                       dep-path)
        "")))

(define (rkt->ss p)
  (if (path-has-extension? p #".rkt")
      (path-replace-extension p #".ss")
      p))

(define (compile-root path->mode roots path0 up-to-date collection-cache read-src-syntax sha1-only? seen)
  (define orig-path (simple-form-path path0))
  (define (read-deps path)
    (with-handlers ([exn:fail:filesystem? (lambda (ex) (list (version) '#f))])
      (with-module-reading-parameterization
       (lambda ()
         (call-with-input-file
             (path-add-extension (get-compilation-path path->mode roots path) #".dep")
           read)))))
  (define (do-check)
    (let* ([main-path orig-path]
           [alt-path (rkt->ss orig-path)]
           [main-path-time (try-file-time main-path)]
           [alt-path-time (and (not main-path-time)
                               (not (eq? alt-path main-path))
                               (try-file-time alt-path))]
           [path (if alt-path-time alt-path main-path)]
           [path-time (or main-path-time alt-path-time)]
           [path-zo-time (get-compiled-time path->mode roots path)])
      (cond
       [(hash-ref seen path #f)
        (error 'compile-zo 
               "dependency cycle\n  involves module: ~a"
               path)
        #f]
       [(not path-time)
        (trace-printf "~a does not exist" orig-path)
        (or (hash-ref up-to-date orig-path #f)
            (let ([stamp (cons (or path-zo-time +inf.0)
                               (delay (get-compiled-sha1 path->mode roots path)))])
              (hash-set! up-to-date main-path stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date alt-path stamp))
              stamp))]
       [else
        (let ([deps (read-deps path)]
              [new-seen (hash-set seen path #t)])
          (define build
            (cond
             [(not (and (pair? deps) (equal? (version) (car deps))))
              (lambda ()
                (trace-printf "newer version...")
                (maybe-compile-zo #f #f path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen))]
             [(> path-time (or path-zo-time -inf.0))
              (trace-printf "newer src... ~a > ~a" path-time path-zo-time)
              ;; If `sha1-only?', then `maybe-compile-zo' returns a #f or thunk:
              (maybe-compile-zo sha1-only? deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen)]
             [(ormap-strict
               (lambda (p)
                 (define ext? (external-dep? p))
                 (define d (collects-relative*->path (dep->encoded-path p) collection-cache))
                 (define t
                   (if ext?
                       (cons (or (try-file-time d) +inf.0) #f)
                       (compile-root path->mode roots d up-to-date collection-cache read-src-syntax #f new-seen)))
                 (and t
                      (car t)
                      (> (car t) (or path-zo-time -inf.0))
                      (begin (trace-printf "newer: ~a (~a > ~a)..."
                                           d (car t) path-zo-time)
                             #t)))
               (cddr deps))
              ;; If `sha1-only?', then `maybe-compile-zo' returns a #f or thunk:
              (maybe-compile-zo sha1-only? deps path->mode roots path orig-path read-src-syntax up-to-date collection-cache new-seen)]
             [else #f]))
          (cond
           [(and build sha1-only?) #f]
           [else
            (when build (build))
            (let ([stamp (cons (or (get-compiled-time path->mode roots path) +inf.0)
                               (delay (get-compiled-sha1 path->mode roots path)))])
              (hash-set! up-to-date main-path stamp)
              (unless (eq? main-path alt-path)
                (hash-set! up-to-date alt-path stamp))
              stamp)]))])))
  (or (hash-ref up-to-date orig-path #f)
      (let ([v ((manager-skip-file-handler) orig-path)])
        (and v
             (hash-set! up-to-date orig-path v)
             v))
      (begin (trace-printf "checking: ~a" orig-path)
             (do-check))))

(define (ormap-strict f l)
  (cond
    [(null? l) #f]
    [else
     (define a (f (car l)))
     (define b (ormap-strict f (cdr l)))
     (or a b)]))

(define (managed-compile-zo zo [read-src-syntax read-syntax] #:security-guard [security-guard #f])
  ((make-caching-managed-compile-zo read-src-syntax #:security-guard security-guard) zo))

(define (make-caching-managed-compile-zo [read-src-syntax read-syntax] #:security-guard [security-guard #f])
  (let ([cache (make-hash)]
        [collection-cache (make-hash)])
    (lambda (src)
      (parameterize ([current-load/use-compiled
                      (make-compilation-manager-load/use-compiled-handler/table
                       cache
                       collection-cache
                       #f
                       #:security-guard security-guard)]
                     [error-display-handler
                      (make-compilation-context-error-display-handler
                       (error-display-handler))])
        (compile-root (or (current-path->mode)
                          (let ([mode (car (use-compiled-file-paths))])
                            (λ (pth) mode)))
                      (current-compiled-file-roots)
                      (path->complete-path src)
                      cache
                      collection-cache
                      read-src-syntax
                      #f
                      #hash())
        (void)))))

(define (make-compilation-manager-load/use-compiled-handler [delete-zos-when-rkt-file-does-not-exist? #f]
                                                            #:security-guard 
                                                            [security-guard #f])
  (make-compilation-manager-load/use-compiled-handler/table (make-hash) (make-hash)
                                                            delete-zos-when-rkt-file-does-not-exist?
                                                            #:security-guard security-guard))

(define (make-compilation-manager-load/use-compiled-handler/table cache collection-cache
                                                                  delete-zos-when-rkt-file-does-not-exist?
                                                                  #:security-guard [security-guard #f])


  (define cp->m (current-path->mode))
  (define modes (use-compiled-file-paths))
  (when (and (not cp->m) (null? modes))
    (raise-mismatch-error 'make-compilation-manager-...
                          "use-compiled-file-paths is '() and current-path->mode is #f"))
  (define path->mode (or cp->m (λ (p) (car modes))))
  (let ([orig-eval (current-eval)]
        [orig-load (current-load)]
        [orig-registry (namespace-module-registry (current-namespace))]
        [default-handler (current-load/use-compiled)]
        [roots (current-compiled-file-roots)])
    (define (compilation-manager-load-handler path mod-name)
      (cond [(or (not mod-name)
                 ;; Don't trigger compilation if we're not supposed to work with source:
                 (and (pair? mod-name)
                      (not (car mod-name))))
             (trace-printf "skipping:  ~a mod-name ~s" path mod-name)]
            [(not (or (file-exists? path)
                      (let ([p2 (rkt->ss path)])
                        (and (not (eq? path p2))
                             (file-exists? p2)))))
             (trace-printf "skipping:  ~a file does not exist" path)
             (when delete-zos-when-rkt-file-does-not-exist?
               (define to-delete (path-add-extension (get-compilation-path path->mode roots path) #".zo"))
               (when (file-exists? to-delete)
                 (trace-printf "deleting:  ~s" to-delete)
                 (with-compiler-security-guard (delete-file to-delete))))]
            [(if cp->m
                 (not (equal? (current-path->mode) cp->m))
                 (let ([current-cfp (use-compiled-file-paths)])
                   (or (null? current-cfp)
                       (not (equal? (car current-cfp) (car modes))))))
             (if cp->m
                 (trace-printf "skipping:  ~a current-path->mode changed; current value ~s, original value was ~s"
                               path (current-path->mode) cp->m)
                 (trace-printf "skipping:  ~a use-compiled-file-paths's first element changed; current value ~s, first element was ~s"
                               path
                               (use-compiled-file-paths)
                               (car modes)))]
            [(not (equal? roots (current-compiled-file-roots)))
             (trace-printf "skipping:  ~a current-compiled-file-roots changed; current value ~s, original was ~s"
                           path 
                           (current-compiled-file-roots)
                           roots)]
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
             (parameterize ([compiler-security-guard security-guard])
               (compile-root path->mode roots path cache collection-cache read-syntax #f #hash()))
             (trace-printf "done: ~a" path)])
      (default-handler path mod-name))
    (when (null? roots)
      (raise-mismatch-error 'make-compilation-manager-...
                            "empty current-compiled-file-roots list: "
                            roots))
    compilation-manager-load-handler))


;; Exported:
(define (get-compiled-file-sha1 path)
  (try-file-sha1 path (path-replace-extension path #".dep")))

(define (get-file-sha1 path)
  (get-source-sha1 path))

(define (make-compile-lock)
  (define-values (manager-side-chan build-side-chan) (place-channel))
  (struct pending (response-chan zo-path died-chan-manager-side) #:transparent)
  (struct running (zo-path died-chan-manager-side) #:transparent)
  
  (define currently-locked-files (make-hash))
  (define pending-requests '())
  (define running-compiles '())
  
  (thread
   (λ ()
     (let loop ()
       (apply
        sync
        (handle-evt
         manager-side-chan
         (λ (req)
           (define command (list-ref req 0))
           (define zo-path (list-ref req 1))
           (define response-manager-side (list-ref req 2))
           (define died-chan-manager-side (list-ref req 3))
           (define compilation-thread-id (list-ref req 4))
           (case command
             [(lock)
              (cond
                [(hash-ref currently-locked-files zo-path #f)
                 (log-info (format "compile-lock: ~s ~a already locked" zo-path compilation-thread-id))
                 (set! pending-requests (cons (pending response-manager-side zo-path died-chan-manager-side)
                                              pending-requests))
                 (loop)]
                [else
                 (log-info (format "compile-lock: ~s ~a obtained lock" zo-path compilation-thread-id))
                 (hash-set! currently-locked-files zo-path #t)
                 (place-channel-put response-manager-side #t)
                 (set! running-compiles (cons (running zo-path died-chan-manager-side) running-compiles))
                 (loop)])]
             [(unlock)
              (log-info (format "compile-lock: ~s ~a unlocked" zo-path compilation-thread-id))
              (define (same-pending-zo-path? pending) (equal? (pending-zo-path pending) zo-path))
              (define to-unlock (filter same-pending-zo-path? pending-requests))
              (set! pending-requests (filter (compose not same-pending-zo-path?) pending-requests))
              (for ([pending (in-list to-unlock)])
                (place-channel-put (pending-response-chan pending) #f))
              (hash-remove! currently-locked-files zo-path)
              (set! running-compiles (filter (λ (a-running) (not (equal? (running-zo-path a-running) zo-path)))
                                             running-compiles))
              (loop)])))
        (for/list ([running-compile (in-list running-compiles)])
          (handle-evt
           (running-died-chan-manager-side running-compile)
           (λ (compilation-thread-id)
             (define zo-path (running-zo-path running-compile))
             (set! running-compiles (remove running-compile running-compiles))
             (define same-zo-pending 
               (filter (λ (pending) (equal? zo-path (pending-zo-path pending)))
                       pending-requests))
             (cond
               [(null? same-zo-pending)
                (log-info (format "compile-lock: ~s ~a died; no else waiting" zo-path compilation-thread-id))
                (hash-remove! currently-locked-files zo-path)
                (loop)]
               [else
                (log-info (format "compile-lock: ~s ~a died; someone else waiting" zo-path compilation-thread-id))
                (define to-be-running (car same-zo-pending))
                (set! pending-requests (remq to-be-running pending-requests))
                (place-channel-put (pending-response-chan to-be-running) #t)
                (set! running-compiles 
                      (cons (running zo-path (pending-died-chan-manager-side to-be-running))
                            running-compiles))
                (loop)]))))))))
  
  build-side-chan)

(define (compile-lock->parallel-lock-client build-side-chan [custodian #f])
  (define monitor-threads (make-hash))
  (define add-monitor-chan (make-channel))
  (define kill-monitor-chan (make-channel))
  
  (when custodian
    (parameterize ([current-custodian custodian])
      (thread
       (λ () 
         (let loop ()
           (sync
            (handle-evt add-monitor-chan
                        (λ (arg)
                          (define-values (zo-path monitor-thread) (apply values arg))
                          (hash-set! monitor-threads zo-path monitor-thread)
                          (loop)))
            (handle-evt kill-monitor-chan
                        (λ (zo-path)
                          (define thd/f (hash-ref monitor-threads zo-path #f))
                          (when thd/f (kill-thread thd/f))
                          (hash-remove! monitor-threads zo-path)
                          (loop)))))))))
  
  (λ (command zo-path)
    (define compiling-thread (current-thread))
    (define-values (response-builder-side response-manager-side) (place-channel))
    (define-values (died-chan-compiling-side died-chan-manager-side) (place-channel))
    (place-channel-put build-side-chan (list command 
                                             zo-path
                                             response-manager-side
                                             died-chan-manager-side 
                                             (eq-hash-code compiling-thread)))
    (cond
      [(eq? command 'lock)
       (define monitor-thread
        (and custodian
             (parameterize ([current-custodian custodian])
               (thread
                (λ ()
                  (thread-wait compiling-thread)
                  ;; compiling thread died; alert the server
                  ;; & remove this thread from the table
                  (place-channel-put died-chan-compiling-side (eq-hash-code compiling-thread))
                  (channel-put kill-monitor-chan zo-path))))))
       (when monitor-thread (channel-put add-monitor-chan (list zo-path monitor-thread)))
       (define res (place-channel-get response-builder-side))
       (when monitor-thread
         (unless res ;; someone else finished compilation for us; kill the monitor
           (channel-put kill-monitor-chan zo-path)))
       res]
      [(eq? command 'unlock)
       (when custodian 
         ;; we finished the compilation; kill the monitor
         (channel-put kill-monitor-chan zo-path))])))
