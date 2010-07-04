;; Expects parameters to be set before invocation.
;; Calls `exit' when done.

#lang scheme/base

(require scheme/unit
         mzlib/cm
         scheme/path
         scheme/file
         scheme/port
         scheme/match
         scheme/system
         scheme/list
         planet/planet-archives
         planet/private/planet-shared

         "option-sig.ss"
         compiler/sig
         launcher/launcher-sig

         "unpack.ss"
         "getinfo.ss"
         "dirs.ss"
         "main-collects.ss"
         "private/path-utils.ss")

(define-namespace-anchor anchor)

;; read info files without compiling them
(define getinfo
  (let ([ns (namespace-anchor->empty-namespace anchor)]
        [compile (current-compile)])
    (lambda (path)
      (parameterize ([current-namespace ns]
                     [current-compile compile]
                     [use-compiled-file-paths '()])
        (get-info/full path)))))

(provide setup@)

(define-unit setup@
  (import setup-option^
          compiler^
          (prefix compiler:option: compiler:option^)
          launcher^)
  (export)

  (define (setup-fprintf p task s . args)
    (let ([task (if task (string-append task ": ") "")])
      (apply fprintf p (string-append "setup-plt: " task s "\n") args)))

  (define (setup-printf task s . args)
    (apply setup-fprintf (current-output-port) task s args))

  (define (exn->string x) (if (exn? x) (exn-message x) (format "~s" x)))

  ;; auto-curried list-of
  (define list-of
    (case-lambda [(pred) (lambda (x) (and (list? x) (andmap pred x)))]
                 [(pred x) ((list-of pred) x)]))

  (define (relative-path-string? x) (and (path-string? x) (relative-path? x)))

  (define main-collects-dir (find-collects-dir))

  (unless (make-user)
    (current-library-collection-paths
     (if (member main-collects-dir (current-library-collection-paths))
       (list main-collects-dir)
       '())))

  (current-library-collection-paths
   (map simplify-path (current-library-collection-paths)))

  (setup-printf "version" "~a [~a]" (version) (system-type 'gc))
  (setup-printf "variants" "~a"
                (apply string-append
                       (map (lambda (s) (format " ~a" s))
                            (available-mzscheme-variants))))
  (setup-printf "main collects" "~a" (path->string main-collects-dir))
  (setup-printf "collects paths"
                (if (null? (current-library-collection-paths)) " empty!" ""))
  (for ([p (current-library-collection-paths)])
    (setup-printf #f "  ~a" (path->string p)))

  (define (call-info info flag mk-default test)
    (let ([v (info flag mk-default)]) (test v) v))

  (define mode-dir
    (if (compile-mode)
      (build-path "compiled" (compile-mode))
      (build-path "compiled")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   Errors                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errors null)
  (define (record-error cc desc go fail-k)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (fprintf (current-error-port) "~a\n" (exn->string x))
                       (set! errors (cons (list cc desc x) errors))
                       (fail-k))])
      (go)))
  (define-syntax begin-record-error
    (syntax-rules ()
      [(_ cc desc body ...) (record-error cc desc (lambda () body ...) void)]))
  (define (show-errors port)
    (for ([e (reverse errors)])
      (match-let ([(list cc desc x) e])
        (setup-fprintf port "error" "during ~a for ~a"
                       desc (if (cc? cc) (cc-name cc) cc))
        (setup-fprintf port #f "  ~a" (exn->string x)))))

  (define (done)
    (setup-printf #f "done")
    (unless (null? errors)
      (setup-printf #f "")
      (show-errors (current-error-port))
      (when (pause-on-errors)
        (fprintf (current-error-port)
                 "INSTALLATION FAILED.\nPress Enter to continue...\n")
        (read-line))
      (exit 1))
    (exit 0))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Archive Unpacking               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define x-specific-collections
    (apply append
           (specific-collections)
           (map (lambda (x)
                  (unpack x
                          (build-path main-collects-dir 'up)
                          (lambda (s) (setup-printf #f "~a" s))
                          (current-target-directory-getter)
                          (force-unpacks)
                          (current-target-plt-directory-getter)))
                (archives))))

  ;; specific-planet-dir ::=
  ;;    - (list path[directory] string[owner] string[package-name] (listof string[extra package path]) Nat[maj] Nat[min]), or
  ;;    - (list string[owner] string[package-name] string[maj as string] string[min as string])
  ;; x-specific-planet-dir ::= (listof specific-planet-dir)
  (define x-specific-planet-dirs
    (if (make-planet) (specific-planet-dirs) null))

  (define no-specific-collections?
    (and (null? x-specific-collections) (null? x-specific-planet-dirs)))

  (when (and (not (null? (archives))) no-specific-collections?)
    (done)
    (exit 0)) ; done

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;              Find Collections                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-struct cc
    (collection path name info root-dir info-path shadowing-policy)
    #:inspector #f)

  (define (make-cc* collection path root-dir info-path shadowing-policy)
    (define info
      (or (with-handlers ([exn:fail? (warning-handler #f)]) (getinfo path))
          (lambda (flag mk-default) (mk-default))))
    (define name
      (call-info
       info 'name (lambda () #f)
       (lambda (x)
         (when (and x (not (string? x)))
           (error 'setup-plt
                  "'name' result from collection ~e is not a string: ~e"
                  path x)))))
    (define path-name (path->name path))
    (define basename
      (let-values ([(base name dir?) (split-path path)])
        (if (path? name)
          (path-element->string name)
          (error 'make-cc*
                 "Internal error: cc had invalid info-path: ~e" path))))
    (when (info 'compile-subcollections (lambda () #f))
      (setup-printf "WARNING"
                    "ignoring `compile-subcollections' entry in info ~a\n"
                    path-name))
    ;; this check is also done in compiler/compiler-unit, in compile-directory
    (and (not (or (regexp-match? #rx"^[.]" basename) 
                  (equal? "compiled" basename)
                  (equal? "doc" basename)
                  (eq? 'all (info 'compile-omit-paths void))))
         (make-cc collection path
                  (if name (string-append path-name " (" name ")") path-name)
                  info root-dir info-path shadowing-policy)))

  (define ((warning-handler v) exn)
    (setup-printf "WARNING" "~a" (exn->string exn))
    v)

  ;; collection->cc : listof path -> cc/#f
  (define (collection->cc collection-p)
    (let ([root-dir
           (ormap (lambda (p)
                    (parameterize ([current-library-collection-paths (list p)])
                      (and (with-handlers ([exn:fail? (lambda (x) #f)])
                             (apply collection-path collection-p))
                           p)))
                  (current-library-collection-paths))])
      (make-cc* collection-p
                (apply collection-path collection-p)
                root-dir
                (build-path root-dir "info-domain" "compiled" "cache.ss")
                ;; by convention, all collections have "version" 1 0. This
                ;; forces them to conflict with each other.
                (list (cons 'lib (map path->string collection-p)) 1 0))))

  ;; remove-falses : listof (union X #f) -> listof X
  ;; returns the non-false elements of l in order
  (define (remove-falses l) (filter values l))

  ;; planet-spec->planet-list : (list string string nat nat) -> (list path string string (listof string) nat nat) | #f
  ;; converts a planet package spec into the information needed to create a cc structure
  (define (planet-spec->planet-list spec)
    (match spec
      [(list owner pkg-name maj-str min-str)
       (let ([maj (string->number maj-str)]
             [min (string->number min-str)])
         (unless maj
           (error 'setup-plt "bad major version for PLaneT package: ~e" maj-str))
         (unless min
           (error 'setup-plt "bad minor version for PLaneT package: ~e" min-str))
         (let ([pkg (lookup-package-by-keys owner pkg-name maj min min)])
           (if pkg
             pkg
             (error 'setup-plt "not an installed PLaneT package: (~e ~e ~e ~e)"
                    owner pkg-name maj min))))]
      [_ spec]))

  (define (planet->cc path owner pkg-file extra-path maj min)
    (unless (path? path)
      (error 'planet->cc "non-path when building package ~e" pkg-file))
    (make-cc* #f
              path
              #f ; don't need root-dir; absolute paths in cache.ss will be ok
              (get-planet-cache-path)
              (list `(planet ,owner ,pkg-file ,@extra-path) maj min)))

  ;; planet-cc->sub-cc : cc (listof bytes [encoded path]) -> cc
  ;; builds a compilation job for the given subdirectory of the given cc this
  ;; is an awful hack
  (define (planet-cc->sub-cc cc subdir)
    (match-let ([(list (list 'planet owner pkg-file extra-path ...) maj min)
                 (cc-shadowing-policy cc)])
      (planet->cc (apply build-path (cc-path cc) (map bytes->path subdir))
                  owner
                  pkg-file
                  (append extra-path subdir)
                  maj
                  min)))

  (define planet-dirs-to-compile
    (if (make-planet)
      (remove-falses (map (lambda (spec) (apply planet->cc spec))
                          (if no-specific-collections?
                            (get-all-planet-packages)
                            (remove-falses (map planet-spec->planet-list
                                                x-specific-planet-dirs)))))
      null))

  (define all-collections
    (let ([ht (make-hash)])
      (for ([cp (current-library-collection-paths)]
            #:when (directory-exists? cp)
            [collection (directory-list cp)]
            #:when (directory-exists? (build-path cp collection)))
        (hash-ref ht collection
          (lambda ()
            (let ([cc (collection->cc (list collection))])
              (when cc (hash-set! ht collection cc))))))
      (hash-map ht (lambda (k v) v))))

  ;; Close over sub-collections
  (define (collection-closure collections-to-compile make-subs)
    (define (get-subs cc)
      (let* ([info (cc-info cc)]
             [ccp (cc-path cc)]
             ;; note: `compile-omit-paths' can be the symbol `all', if this
             ;; happens then this collection should not have been included in
             ;; the first place, but we might jump in if a command-line
             ;; argument specifies coll/subcoll
             [omit (call-info info 'compile-omit-paths (lambda () '())
                              (lambda (x)
                                (unless (or (eq? 'all x) (list-of string? x))
                                  (error 'setup-plt
                                         "expected a list of path strings or 'all for compile-omit-paths, got: ~s"
                                         x))))]
             [omit (if (pair? omit) omit '())]
             [subs (filter (lambda (p)
                             (and (directory-exists? (build-path ccp p))
                                  (not (member (path->string p) omit))))
                           (directory-list ccp))])
        (remove-falses (make-subs cc subs))))
    (remove-falses
     (let loop ([l collections-to-compile])
       (apply append (map (lambda (cc) (cons cc (loop (get-subs cc)))) l)))))

  (define (plt-collection-closure collections-to-compile)
    (collection-closure
     collections-to-compile
     (lambda (cc subs)
       (map (lambda (sub)
              (collection->cc (append (cc-collection cc) (list sub))))
            subs))))

  (define (check-again-all given-ccs)
    #|
    ;; This code is better than using marker files, but an older version of it
    ;; relied on the obligatory existence of an "info.ss" file.  That is no
    ;; longer required, so it needs to identify directories and that is
    ;; currently not available.  So use the code below instead.
    (define all-cc+ids
      (map (lambda (cc)
             (cons cc (file-or-directory-identity (cc-path cc))))
           (plt-collection-closure all-collections)))
    (for ([cc given-ccs])
      (define given-id
        (file-or-directory-identity (cc-path cc)))
      (for ([found-cc+id all-cc+ids]
            #:when (not (same-collection-name? cc (car found-cc+id))))
        (when (eq? (cdr found-cc+id) given-id)
          (error 'setup-plt
                 "given collection path: ~e refers to the same info file as another path: ~e"
                 (apply build-path (cc-collection cc))
                 (apply build-path (cc-collection (car found-cc+id)))))))
    |#
    ;; Note: this is not a locking mechanism; specifically, if we find a marker
    ;; file we assume that we generated it rather than another setup-plt
    ;; process
    (define all-ccs (plt-collection-closure all-collections))
    (define (cc->name cc) (apply build-path (cc-collection cc)))
    (define all-names   (map cc->name all-ccs))
    (define given-names (map cc->name given-ccs))
    (define (cc-mark cc) (build-path (cc-path cc) ".setup-plt-marker"))
    ;; For cleanup: try to remove all files, be silent
    (define (cleanup)
      (for ([cc (append given-ccs all-ccs)])
        (let ([mark (cc-mark cc)])
          (when (file-exists? mark)
            (with-handlers ([void void]) (delete-file mark))))))
    ;; First remove all marker files if any, let it fail if we can't remove it
    (define (remove-markers)
      (for ([cc given-ccs])
        (let ([mark (cc-mark cc)])
          (when (file-exists? mark)
            (setup-printf "WARNING"
                          "found a marker file, deleting: ~a"
                          (path->name mark))
            (delete-file mark)))))
    ;; Now create all marker files, signalling an error if duplicate
    (define (put-markers)
      (for ([cc given-ccs] [name given-names])
        (let ([mark (cc-mark cc)])
          (if (file-exists? mark)
            (error 'setup-plt
                   "given collection path: ~e refers to the same directory as another given collection path"
                   name)
            (with-output-to-file mark (lambda () (printf "~a\n" name)))))))
    ;; Finally scan all ccs and look for duplicates
    (define (scan-all)
      (for ([cc all-ccs] [name all-names])
        (when (and (not (member name given-names))
                   (file-exists? (cc-mark cc)))
          (let ([given (with-input-from-file (cc-mark cc) read-line)])
            (error 'setup-plt
                   "given collection path: ~e refers to the same directory as another given collection path"
                   name)))))
    (dynamic-wind
      void
      (lambda () (remove-markers) (put-markers) (scan-all) given-ccs)
      cleanup))

  (define (sort-collections ccs)
    (sort ccs (lambda (a b) (string<? (cc-name a) (cc-name b)))))

  (define collections-to-compile
    (sort-collections
     (plt-collection-closure
      (if no-specific-collections?
        all-collections
        (check-again-all
         (remove-falses
          (map (lambda (c) (collection->cc (map string->path c)))
               x-specific-collections)))))))

  (set! planet-dirs-to-compile
        (sort-collections
         (collection-closure
          planet-dirs-to-compile
          (lambda (cc subs)
            (map (lambda (p) (planet-cc->sub-cc cc (list (path->bytes p))))
                 subs)))))

  (define ccs-to-compile (append collections-to-compile planet-dirs-to-compile))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Helpers                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (control-io-apply print-doing f args)
    (if (make-verbose)
      (begin (apply f args) #t)
      (let* ([oop (current-output-port)]
             [printed? #f]
             [on? #f]
             [dir-table (make-hash)]
             [line-accum #""]
             [op (if (verbose)
                   (current-output-port)
                   (open-output-nowhere))]
             [doing-path (lambda (path)
                           (unless printed?
                             (set! printed? #t)
                             (print-doing oop))
                           (unless (verbose)
                             (let ([path (normal-case-path (path-only path))])
                               (unless (hash-ref dir-table path (lambda () #f))
                                 (hash-set! dir-table path #t)
                                 (print-doing oop path)))))])
        (parameterize ([current-output-port op]
                       [compile-notify-handler doing-path])
          (apply f args)
          printed?))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Clean                        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (delete-file/record-dependency path dependencies)
    (when (regexp-match-positions #rx"[.]dep$" (path->bytes path))
      (let ([deps (with-handlers ([exn:fail? (lambda (x) null)])
                    (with-input-from-file path read))])
        (when (and (pair? deps) (list? deps))
          (for ([s (cdr deps)])
            (unless (and (pair? s)
                         (eq? 'ext (car s)))
              (let ([s (main-collects-relative->path s)])
                (when (path-string? s)
                  (hash-set! dependencies s #t))))))))
    (delete-file path))

  (define (delete-files-in-directory path printout dependencies)
    (for ([end-path (directory-list path)])
      (let ([path (build-path path end-path)])
        (cond [(directory-exists? path)
               (void)]
              [(file-exists? path)
               (printout)
               (delete-file/record-dependency path dependencies)]
              [else (error 'delete-files-in-directory
                           "encountered ~a, neither a file nor a directory"
                           path)]))))

  (define (clean-collection cc dependencies)
    (begin-record-error cc "Cleaning"
      (let* ([info (cc-info cc)]
             [paths (call-info
                     info
                     'clean
                     (lambda ()
                       (list mode-dir
                             (build-path mode-dir "native")
                             (build-path mode-dir "native"
                                         (system-library-subpath))))
                     (lambda (x)
                       (unless (list-of path-string? x)
                         (error 'setup-plt
                                "expected a list of path strings for 'clean, got: ~s"
                                x))))]
             [printed? #f]
             [print-message
              (lambda ()
                (unless printed?
                  (set! printed? #t)
                  (setup-printf "deleting" "in ~a"
                                (path->name (cc-path cc)))))])
        (for ([path paths])
          (let ([full-path (build-path (cc-path cc) path)])
            (when (or (file-exists? full-path) (directory-exists? full-path))
              (let ([path (find-relative-path (normalize-path (cc-path cc))
                                              (normalize-path full-path))])
                (let loop ([path path])
                  (let-values ([(base name dir?) (split-path path)])
                    (cond
                      [(path? base)
                       (loop base)]
                      [(eq? base 'relative)
                       (when (eq? name 'up)
                         (error 'clean
                                "attempted to clean files in ~s which is not a subdirectory of ~s"
                                full-path
                                (cc-path cc)))]
                      [else
                       (error 'clean
                              "attempted to clean files in ~s which is not a subdirectory of ~s"
                              full-path
                              (cc-path cc))]))))
              (cond [(directory-exists? full-path)
                     (delete-files-in-directory full-path print-message dependencies)]
                    [(file-exists? full-path)
                     (delete-file/record-dependency full-path dependencies)
                     (print-message)]
                    [else (void)])))))))

  (when (clean)
    (let ([dependencies (make-hash)])
      ;; Main deletion:
      (for ([cc ccs-to-compile]) (clean-collection cc dependencies))
      ;; Unless specific collections were named, also
      ;;  delete .zos for referenced modules and delete
      ;;  info-domain cache
      (when no-specific-collections?
        (setup-printf #f "checking dependencies")
        (let loop ([old-dependencies dependencies])
          (let ([dependencies (make-hash)]
                [did-something? #f])
            (hash-for-each
             old-dependencies
             (lambda (file _)
               (let-values ([(dir name dir?) (split-path file)])
                 (let* ([zo (build-path dir mode-dir (path-add-suffix name #".zo"))]
                        [dep (build-path dir mode-dir (path-add-suffix name #".dep"))])
                   (when (and (file-exists? dep) (file-exists? zo))
                     (set! did-something? #t)
                     (setup-printf "deleting" "~a" (path->name zo))
                     (delete-file/record-dependency zo dependencies)
                     (delete-file/record-dependency dep dependencies))))))
            (when did-something? (loop dependencies))))
        (setup-printf #f "clearing info-domain caches")
        (for ([p (current-library-collection-paths)])
          (let ([fn (build-path p "info-domain" "compiled" "cache.ss")])
            (when (file-exists? fn)
              (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
                (with-output-to-file fn void #:exists 'truncate/replace))))))))

  (when (make-zo)
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (define (do-install-part part)
    (when (if (eq? part 'post) (call-post-install) (call-install))
      (for ([cc ccs-to-compile])
        (let/ec k
          (begin-record-error cc (case part
                                   [(pre)     "Early Install"]
                                   [(general) "General Install"]
                                   [(post)    "Post Install"])
            (let ([fn (call-info (cc-info cc)
                        (case part
                          [(pre)     'pre-install-collection]
                          [(general) 'install-collection]
                          [(post)    'post-install-collection])
                        (lambda () (k #f))
                        (lambda (v)
                          (unless (relative-path-string? v)
                            (error "result is not a relative path string: " v))
                          (let ([p (build-path (cc-path cc) v)])
                            (unless (file-exists? p)
                              (error "installer file does not exist: " p)))))])
              (let ([installer
                     (with-handlers ([exn:fail?
                                      (lambda (exn)
                                        (error 'setup-plt
                                               "error loading installer: ~a"
                                               (exn->string exn)))])
                       (dynamic-require (build-path (cc-path cc) fn)
                                        (case part
                                          [(pre)     'pre-installer]
                                          [(general) 'installer]
                                          [(post)    'post-installer])))])
                (setup-printf (format "~ainstalling"
                                      (case part
                                        [(pre) "pre-"]
                                        [(post) "post-"]
                                        [else ""]))
                              "~a"
                              (cc-name cc))
                (let ([dir (build-path main-collects-dir 'up)])
                  (if (procedure-arity-includes? installer 2)
                    (installer dir (cc-path cc))
                    (installer dir))))))))))

  (do-install-part 'pre)

  (define (make-it desc compile-directory get-namespace)
    ;; To avoid polluting the compilation with modules that are already loaded,
    ;; create a fresh namespace before calling this function.
    ;; To avoid keeping modules in memory across collections, pass
    ;; `make-base-namespace' as `get-namespace', otherwise use
    ;; `current-namespace' for `get-namespace'.
    (let ([gc? #f])
      (for ([cc ccs-to-compile])
        (parameterize ([current-namespace (get-namespace)])
          (begin-record-error 
           cc "making"
           (unless (control-io-apply
                    (case-lambda
                     [(p)
                      ;; Main "doing something" message
                      (set! gc? #t)
                      (setup-fprintf p "making" "~a" (cc-name cc))]
                     [(p where)
                      ;; Doing something specifically in "where"
                      (setup-fprintf p #f " in ~a"
                                     (path->name (path->complete-path
                                                  where (cc-path cc))))])
                    compile-directory
                    (list (cc-path cc) (cc-info cc)))
             (setup-printf "making" "~a" (cc-name cc)))))
        (when gc?
          (collect-garbage)))))

  (define (with-specified-mode thunk)
    (if (not (compile-mode))
      (thunk)
      ;; Use the indicated mode
      (let ([zo-compile
             (with-handlers ([exn:fail?
                              (lambda (exn)
                                (error 'setup-plt
                                       "error loading compiler for mode ~s: ~a"
                                       (compile-mode)
                                       (exn->string exn)))])
               (dynamic-require `(lib "zo-compile.ss" ,(compile-mode))
                                'zo-compile))]
            [orig-kinds (use-compiled-file-paths)]
            [orig-compile (current-compile)]
            [orig-namespace (namespace-anchor->empty-namespace anchor)])
        (parameterize ([current-namespace (make-base-empty-namespace)]
                       [current-compile zo-compile]
                       [use-compiled-file-paths (list mode-dir)]
                       [current-compiler-dynamic-require-wrapper
                        (lambda (thunk)
                          (parameterize ([current-namespace orig-namespace]
                                         [use-compiled-file-paths orig-kinds]
                                         [current-compile orig-compile])
                            (thunk)))])
          (thunk)))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make zo                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-zo)
    (with-specified-mode
     (lambda ()
       (make-it ".zos"
                (lambda (dir info)
                  ;; Clean up bad .zos:
                  (unless (info 'assume-virtual-sources (lambda () #f))
                    (let ([c (build-path dir "compiled")])
                      (when (directory-exists? c)
                        (let ([ok-zo-files (make-immutable-hash
                                            (map (lambda (p)
                                                   (cons (path-add-suffix p #".zo") #t))
                                                 (append (directory-list dir)
                                                         (info 'virtual-sources (lambda () null)))))])
                          (for ([p (directory-list c)])
                            (when (and (regexp-match #rx#".zo$" (path-element->bytes p))
                                       (not (hash-ref ok-zo-files p #f)))
                              (setup-fprintf (current-error-port) #f " deleting ~a" (build-path c p))
                              (delete-file (build-path c p))))))))
                  ;; Make .zos
                  (compile-directory-zos dir info))
                make-base-empty-namespace))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Info-Domain Cache               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-info-domain)
    ;; Each ht maps a collection root dir to an info-domain table. Even when
    ;; `collections-to-compile' is a subset of all collections, we only care
    ;; about those collections that exist in the same root as the ones in
    ;; `collections-to-compile'.
    (let ([ht (make-hash)]
          [ht-orig (make-hash)])
      (for ([cc ccs-to-compile])
        (let* ([domain (with-handlers ([exn:fail? (lambda (x) (lambda () null))])
                         (dynamic-require
                          (build-path (cc-path cc) "info.ss")
                          '#%info-domain))]
               ;; Check whether we have a table for this cc's info-domain cache:
               [t (hash-ref ht (cc-info-path cc)
                    (lambda ()
                      ;; No table for this root, yet. Build one.
                      (let ([l (let ([p (cc-info-path cc)])
                                 (if (file-exists? p)
                                   (with-handlers ([exn:fail?
                                                    (warning-handler null)])
                                     (with-input-from-file p read))
                                   null))])
                        ;; Convert list to hash table. Incluse only well-formed
                        ;; list elements, and only elements whose corresponding
                        ;; collection exists.
                        (let ([t (make-hash)]
                              [all-ok? #f])
                          (when (list? l)
                            (set! all-ok? #t)
                            (for ([i l])
                              (match i
                                [(list
                                  (? (lambda (a)
                                       (and (bytes? a)
                                            (let ([p (bytes->path a)])
                                              ;; If we have a root directory,
                                              ;; then the path must be relative
                                              ;; to it, otherwise it must be
                                              ;; absolute:
                                              (and (if (cc-root-dir cc)
                                                     (relative-path? p)
                                                     (complete-path? p))
                                                   (file-exists?
                                                    (build-path
                                                     (if (cc-root-dir cc)
                                                       (build-path (cc-root-dir cc) p)
                                                       p)
                                                     "info.ss"))))))
                                     a)
                                  (list (? symbol? b) ...)
                                  c
                                  (? integer? d)
                                  (? integer? e))
                                 (hash-set! t a (list b c d e))]
                                [_ (set! all-ok? #f)])))
                          ;; Record the table loaded for this collection root
                          ;; in the all-roots table:
                          (hash-set! ht (cc-info-path cc) t)
                          ;; If anything in the "cache.ss" file was bad, then
                          ;; claim that the old table was empty, so that we
                          ;; definitely write the new table.
                          (hash-set! ht-orig (cc-info-path cc)
                                     (and all-ok? (hash-copy t)))
                          t))))])
          ;; Add this collection's info to the table, replacing any information
          ;; already there.
          (hash-set! t
                     (path->bytes (if (cc-root-dir cc)
                                      ;; Use relative path:
                                      (apply build-path (cc-collection cc))
                                      ;; Use absolute path:
                                      (cc-path cc)))
                     (cons (domain) (cc-shadowing-policy cc)))))
      ;; Write out each collection-root-specific table to a "cache.ss" file:
      (hash-for-each ht
        (lambda (info-path ht)
          (unless (equal? ht (hash-ref ht-orig info-path))
            (let-values ([(base name must-be-dir?) (split-path info-path)])
              (unless (path? base)
                (error 'make-info-domain
                       "Internal error: cc had invalid info-path: ~e"
                       info-path))
              (make-directory* base)
              (let ([p info-path])
                (setup-printf "updating" "~a" (path->name p))
                (with-handlers ([exn:fail? (warning-handler (void))])
                  (with-output-to-file p
                    #:exists 'truncate/replace
                    (lambda ()
                      (write (hash-map ht cons))
                      (newline)))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                       Docs                    ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-docs)
    ;; Double-check that "setup/scribble" is present.
    (unless (file-exists? (build-path (collection-path "setup") "scribble.ss"))
      (make-docs #f)))

  (define (doc:verbose)
    (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
      (dynamic-require 'setup/scribble 'verbose)))
  (define (doc:setup-scribblings)
    (parameterize ([current-namespace (namespace-anchor->empty-namespace anchor)])
      (dynamic-require 'setup/scribble 'setup-scribblings)))

  (when (make-docs)
    (setup-printf #f "--- building documentation ---")
    ((doc:verbose) (verbose))
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (setup-printf #f "docs failure: ~a" (exn->string exn)))])
      ((doc:setup-scribblings)
       (if no-specific-collections? #f (map cc-path ccs-to-compile))
       #f
       (not (null? (archives)))
       (make-user)
       (lambda (what go alt) (record-error what "Building docs" go alt))
       setup-printf)))

  (define (render-pdf file)
    (define cmd
      (format "pdflatex -interaction=batchmode \"~a\" > /dev/null" file))
    (define logfile (path-replace-suffix file #".log"))
    (let loop ([n 0])
      (when (= n 5)
        (error 'render-pdf "didn't get a stable result after ~a runs" n))
      (if (zero? n)
        (setup-printf "running" "pdflatex on ~a" file)
        (setup-printf #f " re-running ~a~a time"
                      (add1 n) (case (add1 n) [(2) 'nd] [(3) 'rd] [else 'th])))
      (unless (system cmd)
        (call-with-input-file logfile
          (lambda (log) (copy-port log (current-error-port))))
        (error 'setup-plt "pdflatex failed"))
      ;; see if we get a "Rerun" note, these seem to come in two flavors
      ;; * Label(s) may have changed. Rerun to get cross-references right.
      ;; * Package longtable Warning: Table widths have changed. Rerun LaTeX.
      (cond
        [(call-with-input-file logfile
           (lambda (log) (regexp-match? #px#"changed\\.\\s+Rerun" log)))
         (loop (add1 n))]
        [(zero? n)
         (setup-printf "WARNING" 
                       "no \"Rerun\" found in first run of pdflatex for ~a"
                       file)]))
    (path-replace-suffix file #".pdf"))

  (when (doc-pdf-dest)
    (setup-printf #f "building PDF documentation (via pdflatex)")
    (let ([dest-dir (path->complete-path (doc-pdf-dest))])
      (unless (directory-exists? dest-dir)
        (make-directory dest-dir))
      (let ([tmp-dir (build-path (find-system-path 'temp-dir)
                                 (format "pltpdfdoc~a" (current-seconds)))])
        (dynamic-wind
          void
          (lambda ()
            (make-directory tmp-dir)
            ((doc:verbose) (verbose))
            ((doc:setup-scribblings)
             (if no-specific-collections? #f (map cc-path ccs-to-compile))
             tmp-dir
             #f
             (make-user)
             (lambda (what go alt)
               (record-error what "Building pdf docs" go alt))
             setup-printf)
            (parameterize ([current-directory tmp-dir])
              (for ([f (directory-list)]
                    #:when (regexp-match? #rx#"[.]tex$" (path-element->bytes f)))
                (let* ([pdf    (render-pdf f)]
                       [target (build-path dest-dir pdf)])
                  (when (file-exists? target) (delete-file target))
                  (copy-file pdf target)))))
          (lambda ()
            (when (directory-exists? tmp-dir)
              (delete-directory/files tmp-dir)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make Launchers               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (when (make-launchers)
    (let ([name-list
           (lambda (l)
             (unless (list-of relative-path-string? l)
               (error "result is not a list of relative path strings:" l)))]
          [flags-list
           (lambda (l)
             (unless (list-of (list-of string?) l)
               (error "result is not a list of strings:" l)))]
          [or-f (lambda (f) (lambda (x) (when x (f x))))])
      (for ([cc ccs-to-compile])
        (begin-record-error cc "Launcher Setup"
          (define info (cc-info cc))
          (define (make-launcher kind
                                 launcher-names
                                 launcher-libraries
                                 launcher-flags
                                 program-launcher-path
                                 make-launcher
                                 up-to-date?)
            (define mzlns
              (call-info info launcher-names (lambda () null) name-list))
            (define mzlls
              (call-info info launcher-libraries (lambda () #f) (or-f name-list)))
            (define mzlfs
              (call-info info launcher-flags (lambda () #f) (or-f flags-list)))
            (cond
              [(null? mzlns) (void)]
              [(not (or mzlls mzlfs))
               (unless (null? mzlns)
                 (setup-printf
                  "WARNING"
                  "~s launcher name list ~s has no matching library/flags lists"
                  kind mzlns))]
              [(and (or (not mzlls) (= (length mzlns) (length mzlls)))
                    (or (not mzlfs) (= (length mzlns) (length mzlfs))))
               (for-each
                (lambda (mzln mzll mzlf)
                  (let ([p (program-launcher-path mzln)]
                        [aux (list* `(exe-name . ,mzln)
                                    '(framework-root . #f)
                                    '(dll-dir . #f)
                                    `(relative? . ,(not absolute-installation?))
                                    (build-aux-from-path
                                     (build-path (cc-path cc)
                                                 (path-replace-suffix
                                                  (or mzll mzln)
                                                  #""))))])
                    (unless (up-to-date? p aux)
                      (setup-printf
                       "launcher"
                       "~a~a"
                       (path->name p #:prefix (format "~a-bin" kind)
                                   #:base (if (equal? kind 'console)
                                            find-console-bin-dir
                                            find-gui-bin-dir))
                       (let ([v (current-launcher-variant)])
                         (if (eq? v (system-type 'gc)) "" (format " [~a]" v))))
                      (make-launcher
                       (or mzlf
                           (if (cc-collection cc)
                             (list "-l-" (string-append
                                          (apply string-append
                                                 (map (lambda (s)
                                                        (string-append
                                                         (if (path? s)
                                                           (path->string s)
                                                           s)
                                                         "/"))
                                                      (cc-collection cc)))
                                          mzll))
                             (list "-t-" (path->string (build-path (cc-path cc) mzll)))))
                       p
                       aux))))
                mzlns
                (or mzlls (map (lambda (_) #f) mzlns))
                (or mzlfs (map (lambda (_) #f) mzlns)))]
              [else
               (let ([fault (if (or (not mzlls) (= (length mzlns) (length mzlls))) 'f 'l)])
                 (setup-printf
                  "WARNING"
                  "~s launcher name list ~s doesn't match ~a list; ~s"
                  kind mzlns
                  (if (eq? 'l fault) "library" "flags")
                  (if (eq? fault 'l) mzlls mzlfs)))]))
          (for ([variant (available-mred-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher 'gui
                             'mred-launcher-names
                             'mred-launcher-libraries
                             'mred-launcher-flags
                             mred-program-launcher-path
                             make-mred-launcher
                             mred-launcher-up-to-date?)))
          (for ([variant (available-mzscheme-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher 'console
                             'mzscheme-launcher-names
                             'mzscheme-launcher-libraries
                             'mzscheme-launcher-flags
                             mzscheme-program-launcher-path
                             make-mzscheme-launcher
                             mzscheme-launcher-up-to-date?)))))))

  (do-install-part 'general)
  (do-install-part 'post)

  (done))
