#lang scheme/base

(provide delete-directory/files
         copy-directory/files
         make-directory*
         make-temporary-file

         get-preference
         put-preferences

         fold-files
         find-files
         pathlist-closure)

;; utility: sorted dirlist so functions are deterministic
(define (sorted-dirlist [dir (current-directory)])
  (let* ([ps (directory-list dir)]
         [ps (map (lambda (p) (cons (path->string p) p)) ps)]
         [ps (sort ps (lambda (p1 p2) (string<? (car p1) (car p2))))]
         [ps (map cdr ps)])
    ps))

(define (delete-directory/files path)
  (unless (path-string? path)
    (raise-type-error 'delete-directory/files "path or string" path))
  (cond
   [(or (link-exists? path) (file-exists? path))
    (delete-file path)]
   [(directory-exists? path)
    (for-each (lambda (e) (delete-directory/files (build-path path e)))
              (sorted-dirlist path))
    (delete-directory path)]
   [else (error 'delete-directory/files
                "encountered ~a, neither a file nor a directory"
                path)]))

(define (copy-directory/files src dest)
  (cond [(file-exists? src)
         (copy-file src dest)]
        [(directory-exists? src)
         (make-directory dest)
         (for-each (lambda (e)
                     (copy-directory/files (build-path src e)
                                           (build-path dest e)))
                   (sorted-dirlist src))]
        [else (error 'copy-directory/files
                     "encountered ~a, neither a file nor a directory"
                     src)]))

(define (make-directory* dir)
  (let-values ([(base name dir?) (split-path dir)])
    (when (and (path? base)
               (not (directory-exists? base)))
      (make-directory* base))
    (unless (directory-exists? dir)
      (make-directory dir))))

(define (make-temporary-file [template "mztmp~a"] [copy-from #f] [base-dir #f])
  (with-handlers ([exn:fail:contract?
                   (lambda (x)
                     (raise-type-error 'make-temporary-file
                                       "format string for 1 argument"
                                       template))])
    (format template void))
  (unless (or (not copy-from)
              (path-string? copy-from)
              (eq? copy-from 'directory))
    (raise-type-error 'make-temporary-file
                      "path, valid-path string, 'directory, or #f"
                      copy-from))
  (unless (or (not base-dir) (path-string? base-dir))
    (raise-type-error 'make-temporary-file
                      "path, valid-path, string, or #f"
                      base-dir))
  (let ([tmpdir (find-system-path 'temp-dir)])
    (let loop ([s (current-seconds)][ms (current-milliseconds)])
      (let ([name (let ([n (format template (format "~a~a" s ms))])
                    (cond [base-dir (build-path base-dir n)]
                          [(relative-path? n) (build-path tmpdir n)]
                          [else n]))])
        (with-handlers ([exn:fail:filesystem:exists?
                         (lambda (x)
                           ;; try again with a new name
                           (loop (- s (random 10))
                                 (+ ms (random 10))))])
          (if copy-from
              (if (eq? copy-from 'directory)
                  (make-directory name)
                  (copy-file copy-from name))
              (close-output-port (open-output-file name)))
          name)))))

(define (with-pref-params thunk)
  (parameterize ([read-case-sensitive #f]
                 [read-square-bracket-as-paren #t]
                 [read-curly-brace-as-paren #t]
                 [read-accept-box #t]
                 [read-accept-compiled #f]
                 [read-accept-bar-quote #t]
                 [read-accept-graph #t]
                 [read-decimal-as-inexact #t]
                 [read-accept-dot #t]
                 [read-accept-infix-dot #t]
                 [read-accept-quasiquote #t]
                 [read-accept-reader #f]
                 [print-struct #f]
                 [print-graph #f] ; <--- FIXME: temporary solution to DrScheme-pref problem
                 [print-box #t]
                 [print-vector-length #t]
                 [current-readtable #f])
    (thunk)))

(define pref-cache (make-weak-box #f))

(define (path->key p)
  (string->symbol (bytes->string/latin-1 (path->bytes p))))

(define (pref-cache-install! fn-key fn-date f)
  (let ([table (or (weak-box-value pref-cache)
                   (make-hasheq))])
    (hash-set! table 
               (path->key fn-key)
               (cons
                (file-or-directory-modify-seconds fn-date #f (lambda () -inf.0))
                f))
    (unless (eq? table (weak-box-value pref-cache))
      (set! pref-cache (make-weak-box table)))))

(define (get-prefs flush-mode filename)
  (define (read-prefs default-pref-file)
    (with-handlers ([exn:fail:filesystem? (lambda (x) null)])
      (let* ([pref-file
              (or filename
                  (let ([f default-pref-file])
                    (if (file-exists? f)
                        ;; Using `file-exists?' means there's technically a
                        ;; race condition, but something has gone really wrong
                        ;; if the file disappears.
                        f
                        ;; Error here bails out through above `with-handlers'
                        (build-path (collection-path "defaults")
                                    "plt-prefs.ss"))))]
             [prefs (with-pref-params
                     (lambda ()
                       (with-input-from-file pref-file read)))])
        ;; Make sure file content had the right shape:
        (if (and (list? prefs)
                 (andmap (lambda (x)
                           (and (pair? x) (pair? (cdr x)) (null? (cddr x))))
                         prefs))
            prefs
            null))))
  (let* ([fn (path->complete-path
              (or filename
                  (find-system-path 'pref-file)))]
         [cache (let ([table (weak-box-value pref-cache)])
                  (and table (hash-ref table (path->key fn) #f)))])
    (if (and cache
             (or (not flush-mode)
                 (and (eq? flush-mode 'timestamp)
                      (= (car cache)
                         (file-or-directory-modify-seconds fn #f (lambda () -inf.0))))))
        (cdr cache)
        (let ([ts (file-or-directory-modify-seconds fn  #f (lambda () -inf.0))]
              [f (read-prefs fn)])
          (pref-cache-install! fn fn f)
          f))))

(define (get-preference name [fail-thunk (lambda () #f)]
                        [refresh-cache? 'timestamp]
                        [filename #f])
  (unless (symbol? name)
    (raise-type-error 'get-preference "symbol" name))
  (unless (and (procedure? fail-thunk)
               (procedure-arity-includes? fail-thunk 0))
    (raise-type-error 'get-preference "procedure (arity 0)" fail-thunk))
  (let ([f (get-prefs refresh-cache? filename)])
    (let ([m (assq name f)])
      (if m (cadr m) (fail-thunk)))))

(define (put-preferences names vals [lock-there #f] [filename #f])
  (unless (and (list? names) (andmap symbol? names))
    (raise-type-error 'put-preferences "list of symbols" names))
  (unless (list? vals)
    (raise-type-error 'put-preferences "list" vals))
  (unless (= (length names) (length vals))
    (raise-mismatch-error
     'put-preferences
     (format "the size of the name list (~a) does not match the size of the value list (~a): "
             (length names) (length vals))
     vals))
  (let-values ([(pref-file lock-file pref-dir)
                (let ([filename (or filename (find-system-path 'pref-file))])
                  (let-values ([(base name dir?) (split-path filename)])
                    (let ([dir (if (symbol? base)
                                   (current-directory)
                                   base)])
                      (unless (directory-exists? dir)
                        (make-directory* dir))
                      (values
                       filename
                       (build-path dir
                                   (bytes->path-element
                                    (bytes-append
                                     (if (eq? 'windows (system-type))
                                         #"_"
                                         #".")
                                     #"LOCK"
                                     (path-element->bytes name))))
                       dir))))])
    (with-handlers ([exn:fail:filesystem:exists?
                     (lambda (x)
                       (if lock-there
                           (lock-there lock-file)
                           (error 'put-preferences
                                  "some other process has the preference-file lock, as indicated by the existence of the lock file: ~e"
                                  lock-file)))])
      ;; Grab lock:
      (close-output-port (open-output-file lock-file #:exists 'error))
      (dynamic-wind
          void
          (lambda ()
            (let ([f (get-prefs #t filename)])
              (set! f (let loop ([f f][a null])
                        (cond
                         [(null? f) (reverse
                                     (append (map list names vals)
                                             a))]
                         [else (if (memq (caar f) names)
                                   (loop (cdr f) a)
                                   (loop (cdr f) (cons (car f) a)))])))
              ;; To write the file, copy the old one to a temporary name
              ;; (preserves permissions, etc), write to the temp file,
              ;; then move (atomicly) the temp file to the normal name.
              (let ([tmp-file (make-temporary-file
                               "TMPPREF~a"
                               (and (file-exists? pref-file) pref-file)
                               pref-dir)])
                ;; If something goes wrong, try to delete the temp file.
                (with-handlers ([exn:fail? (lambda (exn)
                                             (with-handlers ([exn:fail:filesystem? void])
                                               (delete-file tmp-file))
                                             (raise exn))])
                  ;; Write to temp file...
                  (with-output-to-file tmp-file
                    #:exists 'truncate/replace
                    (lambda ()
                      (with-pref-params
                       (lambda ()
                         ;; If a pref value turns out to be unreadable, raise
                         ;;  an exception instead of creating a bad pref file.
                         (parameterize ([print-unreadable #f])
                           ;; Poor man's pretty-print: one line per entry.
                           (printf "(\n")
                           (for-each (lambda (a)
                                       (if (and (list? (cadr a))
                                                (< 4 (length (cadr a))))
                                           (begin
                                             (printf " (~s\n  (\n" (car a))
                                             (for-each (lambda (i) (printf "   ~s\n" i)) (cadr a))
                                             (printf "  ))\n"))
                                           (printf " ~s\n" a)))
                                     f)
                           (printf ")\n"))))))
                  ;; Install the new table in the cache. It's possible that this
                  ;; cache entry will be replaced by a reading thread before we
                  ;; move the file, but that's ok. It just means that a future
                  ;; reading thread will have to read again.
                  (pref-cache-install! (path->complete-path
                                        (or filename
                                            (find-system-path 'pref-file)))
                                       tmp-file
                                       f)
                  (rename-file-or-directory tmp-file pref-file #t)))))
          (lambda ()
            ;; Release lock:
            (delete-file lock-file))))))

;; fold-files : (pathname sym alpha -> alpha) alpha pathname/#f -> alpha
(define (fold-files f init [path #f] [follow-links? #t])
  (define (do-path path acc)
    (cond [(and (not follow-links?) (link-exists? path)) (f path 'link acc)]
          [(directory-exists? path)
           (call-with-values (lambda () (f path 'dir acc))
             (letrec ([descend
                       (case-lambda
                        [(acc)
                         (do-paths (map (lambda (p) (build-path path p))
                                        (sorted-dirlist path))
                                   acc)]
                        [(acc descend?)
                         (if descend? (descend acc) acc)])])
               descend))]
          [(file-exists? path) (f path 'file acc)]
          [(link-exists? path) (f path 'link acc)] ; dangling links
          [else (error 'fold-files "path disappeared: ~e" path)]))
  (define (do-paths paths acc)
    (cond [(null? paths) acc]
          [else (do-paths (cdr paths) (do-path (car paths) acc))]))
  (if path (do-path path init) (do-paths (sorted-dirlist) init)))

(define (find-files f [path #f])
  (reverse
   (fold-files (lambda (path kind acc) (if (f path) (cons path acc) acc))
               null path)))

(define (pathlist-closure paths)
  (let loop ([paths (map (lambda (p) 
                           (let ([p2 (if (link-exists? p)
                                         (let ([p2 (resolve-path p)])
                                           (if (relative-path? p2)
                                               (let-values ([(base name dir?) (split-path p)])
                                                 (build-path base p2))
                                               p2))
                                         p)])
                             (simplify-path p2 #f)))
                         paths)]
             [r '()])
    (if (null? paths)
        (reverse r)
        (let loop2 ([path (car paths)]
                    [new (cond [(file-exists? (car paths))
                                (list (car paths))]
                               [(directory-exists? (car paths))
                                (find-files void (car paths))]
                               [else (error 'pathlist-closure
                                            "file/directory not found: ~a"
                                            (car paths))])])
          (let-values ([(base name dir?) (split-path path)])
            (if (path? base)
                (loop2 base (if (or (member base r) (member base paths))
                                new (cons base new)))
                (loop (cdr paths) (append (reverse new) r))))))))
