;; Expects parameters to be set before invocation.
;; Calls `exit' when done.

#lang scheme/base

(require scheme/unit
         scheme/path
         scheme/file
         scheme/port
         scheme/match
         scheme/system
         scheme/list
         scheme/string
         compiler/cm
         planet/planet-archives
         planet/private/planet-shared

         "option-sig.rkt"
         compiler/sig
         launcher/launcher-sig
         dynext/dynext-sig

         "unpack.rkt"
         "getinfo.rkt"
         "dirs.rkt"
         "main-collects.rkt"
         "path-to-relative.rkt"
         "private/omitted-paths.rkt"
         "parallel-build.rkt"
         "collects.rkt")
(define-namespace-anchor anchor)

;; read info files using whatever namespace, .zo-use, and compilation
;;  configuration was in place for loading setup, instead of whatever
;;  is in place for the collections that setup is processing:
(define getinfo
  (let ([ns (namespace-anchor->empty-namespace anchor)]
        [compile (current-compile)]
        [loader (current-load/use-compiled)]
        [paths (use-compiled-file-paths)])
    (lambda (path)
      (parameterize ([current-namespace ns]
                     [current-compile compile]
                     [current-load/use-compiled loader]
                     [use-compiled-file-paths paths])
        (get-info/full path #:namespace ns)))))

(provide setup@)

(define-unit setup@
  (import setup-option^
          compiler^
          dynext:file^
          (prefix compiler:option: compiler:option^)
          launcher^)
  (export)

  (define name-str (setup-program-name))
  (define name-sym (string->symbol name-str))
  (define main-collects-dir (find-collects-dir))
  (define mode-dir
    (if (compile-mode)
      (build-path "compiled" (compile-mode))
      (build-path "compiled")))

  (unless (make-user)
    (current-library-collection-paths
     (if (member main-collects-dir (current-library-collection-paths))
       (list main-collects-dir)
       '())))

  (current-library-collection-paths
   (map simple-form-path (current-library-collection-paths)))

  (define (setup-fprintf p task s . args)
    (let ([task (if task (string-append task ": ") "")])
      (apply fprintf p (string-append name-str ": " task s "\n") args)))

  (define (setup-printf task s . args)
    (apply setup-fprintf (current-output-port) task s args))

  (define (exn->string x) (if (exn? x) (exn-message x) (format "~s" x)))

  ;; auto-curried list-of
  (define list-of
    (case-lambda [(pred) (lambda (x) (and (list? x) (andmap pred x)))]
                 [(pred x) ((list-of pred) x)]))

  (define (relative-path-string? x) (and (path-string? x) (relative-path? x)))

  (define (call-info info flag mk-default test)
    (let ([v (info flag mk-default)]) (test v) v))

  (define path->relative-string/console-bin
    (make-path->relative-string
     (list (cons find-console-bin-dir "<console-bin>/"))))
  (define path->relative-string/gui-bin
    (make-path->relative-string
     (list (cons find-gui-bin-dir "<gui-bin>/"))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                   Errors                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define errors null)
  (define (append-error cc desc exn out err type)
    (set! errors (cons (list cc desc exn out err type) errors)))
  (define (handle-error cc desc exn out err type)
      (if (verbose)
          ((error-display-handler)
           (format "~a\n" (exn->string exn))
           exn)
          (fprintf (current-error-port) "~a\n" (exn->string exn)))
      (append-error cc desc exn out err type))
  (define (record-error cc desc go fail-k)
    (with-handlers ([exn:fail?
                     (lambda (x)
                       (handle-error cc desc x "" "" "error")
                       (fail-k))])
      (go)))
  (define-syntax begin-record-error
    (syntax-rules ()
      [(_ cc desc body ...) (record-error cc desc (lambda () body ...) void)]))
  (define (show-errors port)
    (for ([e (reverse errors)])
      (match-let ([(list cc desc x out err type) e])
        (setup-fprintf port type "during ~a for ~a" desc (if (cc? cc) (cc-name cc) cc))
        (when (not (null? x)) (setup-fprintf port #f "  ~a" (exn->string x)))
        (when (not (zero? (string-length out))) (eprintf "STDOUT:\n~a=====\n" out))
        (when (not (zero? (string-length err))) (eprintf "STDERR:\n~a=====\n" err)))))

  (define (done)
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
    (append* (specific-collections)
             (for/list ([x (in-list (archives))])
               (unpack x
                       (build-path main-collects-dir 'up)
                       (lambda (s) (setup-printf #f "~a" s))
                       (current-target-directory-getter)
                       (force-unpacks)
                       (current-target-plt-directory-getter)))))

  ;; specific-planet-dir ::=
  ;;    - (list path[directory] string[owner] string[package-name] (listof string[extra package path]) Nat[maj] Nat[min]), or
  ;;    - (list string[owner] string[package-name] string[maj as string] string[min as string])
  ;; x-specific-planet-dir ::= (listof specific-planet-dir)
  (define x-specific-planet-dirs
    (if (make-planet) (specific-planet-dirs) null))

  (define no-specific-collections?
    (and (null? x-specific-collections) (null? x-specific-planet-dirs)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;              Find Collections                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-cc* collection path root-dir info-path shadowing-policy)
    (define info
      (or (with-handlers ([exn:fail? (warning-handler #f)]) (getinfo path))
          (lambda (flag mk-default) (mk-default))))
    (define name
      (call-info
       info 'name (lambda () #f)
       (lambda (x)
         (when (and x (not (string? x)))
           (error name-sym
                  "'name' result from collection ~e is not a string: ~e"
                  path x)))))
    (define path-name (path->relative-string/setup path))
    (when (info 'compile-subcollections (lambda () #f))
      (setup-printf "WARNING"
                    "ignoring `compile-subcollections' entry in info ~a"
                    path-name))
    ;; this check is also done in compiler/compiler-unit, in compile-directory
    (and (not (eq? 'all (omitted-paths path getinfo)))
         (make-cc collection path
                  (if name
                      (format "~a (~a)" path-name name)
                      path-name)
                  info root-dir info-path shadowing-policy)))

  (define ((warning-handler v) exn)
    (setup-printf "WARNING" "~a" (exn->string exn))
    v)

  ;; collection->cc : listof path -> cc/#f
  (define collection->cc-table (make-hash))
  (define (collection->cc collection-p)
    (hash-ref! collection->cc-table collection-p
      (lambda ()
        (define root-dir
          (ormap (lambda (p)
                   (parameterize ([current-library-collection-paths (list p)])
                     (and (with-handlers ([exn:fail? (lambda (x) #f)])
                            (apply collection-path collection-p))
                          p)))
                 (current-library-collection-paths)))
        (make-cc* collection-p
                  (apply collection-path collection-p)
                  root-dir
                  (build-path root-dir "info-domain" "compiled" "cache.rktd")
                  ;; by convention, all collections have "version" 1 0. This
                  ;; forces them to conflict with each other.
                  (list (cons 'lib (map path->string collection-p)) 1 0)))))

  ;; planet-spec->planet-list : (list string string nat nat) -> (list path string string (listof string) nat nat) | #f
  ;; converts a planet package spec into the information needed to create a cc structure
  (define (planet-spec->planet-list spec)
    (match spec
      [(list owner pkg-name maj-str min-str)
       (let ([maj (string->number maj-str)]
             [min (string->number min-str)])
         (unless maj
           (error name-sym "bad major version for PLaneT package: ~e" maj-str))
         (unless min
           (error name-sym "bad minor version for PLaneT package: ~e" min-str))
         (let ([pkg (lookup-package-by-keys owner pkg-name maj min min)])
           (if pkg
             pkg
             (error name-sym "not an installed PLaneT package: (~e ~e ~e ~e)"
                    owner pkg-name maj min))))]
      [_ spec]))

  (define (planet->cc path owner pkg-file extra-path maj min)
    (unless (path? path)
      (error 'planet->cc "non-path when building package ~e" pkg-file))
    (and (directory-exists? path)
         (make-cc* #f
                   path
                   #f ; don't need root-dir; absolute paths in cache.rktd will be ok
                   (get-planet-cache-path)
                   (list `(planet ,owner ,pkg-file ,@extra-path) maj min))))

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
             ;; note: omit can be 'all, if this happens then this
             ;; collection should not have been included, but we might
             ;; jump in if a command-line argument specified a
             ;; coll/subcoll
             [omit (omitted-paths ccp getinfo)]
             [subs (if (eq? 'all omit)
                     '()
                     (filter (lambda (p)
                               (and (directory-exists? (build-path ccp p))
                                    (not (member p omit))))
                             (directory-list ccp)))])
        (filter values (make-subs cc subs))))
    (filter values
            (let loop ([l collections-to-compile])
              (append-map (lambda (cc) (cons cc (loop (get-subs cc)))) l))))

  (define (collection-tree-map collections-to-compile 
              #:skip-path [orig-skip-path (and (avoid-main-installation) (find-collects-dir))])
    (define skip-path (and orig-skip-path (path->bytes 
                                           (simplify-path (if (string? orig-skip-path)
                                                              (string->path orig-skip-path)
                                                              orig-skip-path)
                                                          #f))))
    (define (skip-path? path)
      (and skip-path
           (let ([b (path->bytes (simplify-path path #f))]
                 [len (bytes-length skip-path)])
             (and ((bytes-length b) . > . len)
                  (bytes=? (subbytes b 0 len) skip-path)))
           path))
      
    (define (build-collection-tree cc)
      (define (make-child-cc parent-cc name) 
        (collection->cc (append (cc-collection parent-cc) (list name))))
      (let* ([info (cc-info cc)]
             [ccp (cc-path cc)]
             ;; note: omit can be 'all, if this happens then this
             ;; collection should not have been included, but we might
             ;; jump in if a command-line argument specified a
             ;; coll/subcoll
             [omit (omitted-paths ccp getinfo)])
          (let-values ([(dirs files)
             (if (eq? 'all omit)
                 (values null null)
                 (partition (lambda (x) (directory-exists? (build-path ccp x)))
                   (filter (lambda (p) 
                             (not (or (member p omit)
                                      (skip-path? p))))
                           (directory-list ccp))))])
            (let ([children-ccs (map build-collection-tree (filter-map (lambda (x) (make-child-cc cc x)) dirs))]
                      
                  [srcs (append
                           (filter extract-base-filename/ss files)
                           (if (make-docs)
                               (map car (call-info info 'scribblings (lambda () null) (lambda (x) #f)))
                               null))])
              (list cc srcs children-ccs)))))
    (map build-collection-tree collections-to-compile))
      

    
  (define (plt-collection-closure collections-to-compile)
    (define (make-children-ccs cc children)
      (map (lambda (child)
           (collection->cc (append (cc-collection cc) (list child))))
           children))
    (collection-closure collections-to-compile make-children-ccs))

  (define (check-again-all given-ccs)
    (define (cc->name cc)
      (string-join (map path->string (cc-collection cc)) "/"))
    (define (cc->cc+name+id cc)
      (list cc (cc->name cc) (file-or-directory-identity (cc-path cc))))
    (define all-ccs+names+ids
      (map cc->cc+name+id (plt-collection-closure all-collections)))
    ;; given collections
    (define given-ccs+names+ids (map cc->cc+name+id given-ccs))
    ;; descendants of given collections
    (define descendants-names
      (remove-duplicates
       (append-map
        (lambda (cc)
          (map cc->name (remq cc (plt-collection-closure (list cc)))))
        given-ccs)))
    ;; given collections without duplicates and without ones that are already
    ;; descendants
    (define given*-ccs+names+ids
      (remove-duplicates
       (filter (lambda (cc+name+id)
                 (not (member (cadr cc+name+id) descendants-names)))
               given-ccs+names+ids)
       (lambda (x y) (equal? (cadr x) (cadr y)))))
    ;; check that there are no bad duplicates in the given list
    (for ([given-cc+name+id (in-list given*-ccs+names+ids)])
      (cond
        [(ormap (lambda (cc+name+id)
                  (and (not (equal? (cadr cc+name+id) (cadr given-cc+name+id)))
                       (equal? (caddr cc+name+id) (caddr given-cc+name+id))
                       (cadr cc+name+id)))
                all-ccs+names+ids)
         => (lambda (bad)
              (error name-sym
                     "given collection path: \"~a\" refers to the same directory as another given collection path, \"~a\""
                     (cadr given-cc+name+id) bad))]))
    (map car given*-ccs+names+ids))

  (define (sort-collections ccs)
    (sort ccs string<? #:key cc-name))

  (define (sort-collections-tree ccs)
    (sort ccs string<? #:key (lambda (x) (cc-name (first x)))))

  (define top-level-plt-collects
    (if no-specific-collections?
      all-collections
      (check-again-all
       (filter-map
        (lambda (c)
          (collection->cc (append-map (lambda (s)
                                        (map string->path
                                             (regexp-split #rx"/" s)))
                                      c)))
        x-specific-collections))))

  (define planet-collects
    (if (make-planet)
      (filter-map (lambda (spec) (apply planet->cc spec))
                  (if no-specific-collections?
                    (get-all-planet-packages)
                    (filter-map planet-spec->planet-list
                                x-specific-planet-dirs)))
      null))
  
  (define planet-dirs-to-compile
    (sort-collections
      (collection-closure
        planet-collects
        (lambda (cc subs)
          (map (lambda (p) (planet-cc->sub-cc cc (list (path->bytes p)))) subs)))))

  (define ccs-to-compile 
    (append
      (sort-collections (plt-collection-closure top-level-plt-collects))
      planet-dirs-to-compile))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Clean                        ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (delete-file/record-dependency path dependencies)
    (when (regexp-match-positions #rx"[.]dep$" (path->bytes path))
      (let ([deps (with-handlers ([exn:fail? (lambda (x) null)])
                    (with-input-from-file path read))])
        (when (and (pair? deps) (list? deps))
          (for ([s (in-list (cddr deps))])
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
                         (error name-sym
                                "expected a list of path strings for 'clean, got: ~s"
                                x))))]
             [printed? #f]
             [print-message
              (lambda ()
                (unless printed?
                  (set! printed? #t)
                  (setup-printf "deleting" "in ~a"
                                (path->relative-string/setup (cc-path cc)))))])
        (for ([path paths])
          (let ([full-path (build-path (cc-path cc) path)])
            (when (or (file-exists? full-path) (directory-exists? full-path))
              (let ([path (find-relative-path (simple-form-path (cc-path cc))
                                              (simple-form-path full-path))])
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

  (define (clean-step)
    (setup-printf #f "--- cleaning collections ---")
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
                     (setup-printf "deleting" "~a"
                                   (path->relative-string/setup zo))
                     (delete-file/record-dependency zo dependencies)
                     (delete-file/record-dependency dep dependencies))))))
            (when did-something? (loop dependencies))))
        (setup-printf #f "clearing info-domain caches")
        (for ([p (current-library-collection-paths)])
          (let ([fn (build-path p "info-domain" "compiled" "cache.rktd")])
            (when (file-exists? fn)
              (with-handlers ([exn:fail:filesystem? (warning-handler (void))])
                (with-output-to-file fn void #:exists 'truncate/replace))))))))

  (define (do-install-part part)
    (when (if (eq? part 'post) (call-post-install) (call-install))
      (setup-printf #f (format "--- ~ainstalling collections ---"
                               (case part
                                 [(pre) "pre-"]
                                 [(general) ""]
                                 [(post) "post-"])))
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
                                        (error name-sym
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

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make zo                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax-rule (control-io print-verbose body ...)
    (if (make-verbose)
      (begin 
        body ...)
      (let* ([oop (current-output-port)]
             [dir-table (make-hash)]
             [doing-path (lambda (path)
                           (unless (verbose)
                             (let ([path (normal-case-path (path-only path))])
                               (unless (hash-ref dir-table path #f)
                                 (hash-set! dir-table path #t)
                                 (print-verbose oop path)))))])
        (parameterize ([current-output-port (if (verbose) (current-output-port) (open-output-nowhere))]
                       [compile-notify-handler doing-path])
          body ...))))

  (define (clean-cc dir info)
    ;; Clean up bad .zos:
    (unless (info 'assume-virtual-sources (lambda () #f))
      (let ([c (build-path dir "compiled")])
        (when (directory-exists? c)
          (let ([ok-zo-files
                 (make-immutable-hash
                  (map (lambda (p)
                         (cons (path-add-suffix p #".zo") #t))
                       (append (directory-list dir)
                               (info 'virtual-sources (lambda () null)))))])
            (for ([p (directory-list c)])
              (when (and (regexp-match #rx#".(zo|dep)$" (path-element->bytes p))
                         (not (hash-ref ok-zo-files (path-replace-suffix p #".zo") #f)))
                (setup-fprintf (current-error-port) #f " deleting ~a" (build-path c p))
                (delete-file (build-path c p)))))))))

  (define-syntax-rule (with-specified-mode body ...)
    (let ([thunk (lambda () body ...)])
      (if (not (compile-mode))
        (thunk)
        ;; Use the indicated mode
        (let ([zo-compile
               (with-handlers ([exn:fail?
                                (lambda (exn)
                                  (error name-sym
                                         "error loading compiler for mode ~s: ~a"
                                         (compile-mode)
                                         (exn->string exn)))])
                 (dynamic-require `(lib "zo-compile.rkt" ,(compile-mode))
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
            (thunk))))))

  ;; We keep timestamp information for all files that we try to compile.
  ;; That's O(N) for an installation of size N, but the constant is small,
  ;; and it makes a do-nothing setup complete much faster.
  (define caching-managed-compile-zo (make-caching-managed-compile-zo))

  (define (compile-cc cc gcs)
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (begin-record-error cc "making"
        (setup-printf "making" "~a" (cc-name cc))
        (control-io
          (lambda (p where)
            (set! gcs 2)
            (setup-fprintf p #f " in ~a"
                           (path->relative-string/setup
                            (path->complete-path where (cc-path cc)))))
          (let ([dir (cc-path cc)]
                [info (cc-info cc)])
            (clean-cc dir info)
            (compile-directory-zos dir info 
                                   #:managed-compile-zo caching-managed-compile-zo
                                   #:skip-path (and (avoid-main-installation) (find-collects-dir))
                                   #:skip-doc-sources? (not (make-docs)))))))
    (match gcs
      [0 0]
      [else 
        (collect-garbage)
        (sub1 gcs)]))

  ;; To avoid polluting the compilation with modules that are already loaded,
  ;; create a fresh namespace before calling this function.
  ;; To avoid keeping modules in memory across collections, pass
  ;; `make-base-namespace' as `get-namespace', otherwise use
  ;; `current-namespace' for `get-namespace'.
  (define (iterate-cct thunk cct)
    (let loop ([cct cct])
      (map (lambda (x) (thunk (first x)) (loop (third x))) cct)))

  (define (make-zo-step)
    (define (partition-cct name cct)
      (partition (lambda (x) (not (string=? (cc-name (car x)) name))) cct))
    (define (move-to-begining names cct)
      (let loop ([names (reverse (if (list? names) names (list names)))]
                 [cct cct])
        (match names
          [(list) cct]
          [(cons name names)
            (loop names
                  (call-with-values (lambda () (define-values (a b) (partition-cct name cct)) (values b a)) append))])))
    (define (move-to-end name cct) (call-with-values (lambda () (partition-cct name cct)) append))
    (setup-printf #f "--- compiling collections ---")
    (match (parallel-workers)
      [(? (lambda (x) (x . > . 1)))
        (compile-cc (collection->cc (list (string->path "racket"))) 0)
        (managed-compile-zo (collection-file-path "parallel-build-worker.rkt" "setup"))
        (with-specified-mode
          (let ([cct (move-to-begining (list "compiler" "raco" "racket") 
                                       (move-to-end "drscheme" 
                                                    (sort-collections-tree 
                                                     (collection-tree-map top-level-plt-collects))))])
            (iterate-cct (lambda (cc)
              (let ([dir (cc-path cc)]
                    [info (cc-info cc)])
                  (clean-cc dir info))) cct)
            (parallel-compile (parallel-workers) setup-fprintf handle-error cct))
          (for/fold ([gcs 0]) ([cc planet-dirs-to-compile])
            (compile-cc cc gcs)))]
      [else
        (with-specified-mode
          (for/fold ([gcs 0]) ([cc ccs-to-compile])
            (compile-cc cc gcs)))]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;               Info-Domain Cache               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-info-domain-step)
    (setup-printf #f "--- updating info-domain tables ---")
    ;; Each ht maps a collection root dir to an info-domain table. Even when
    ;; `collections-to-compile' is a subset of all collections, we only care
    ;; about those collections that exist in the same root as the ones in
    ;; `collections-to-compile'.
    (let ([ht (make-hash)]
          [ht-orig (make-hash)])
      (for ([cc ccs-to-compile])
        (let* ([domain (with-handlers ([exn:fail? (lambda (x) (lambda () null))])
                         (dynamic-require
                          (build-path (cc-path cc) "info.rkt")
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
                                                   (let ([dir (if (cc-root-dir cc)
                                                                  (build-path (cc-root-dir cc) p)
                                                                  p)])
                                                     (or (file-exists? (build-path dir "info.rkt"))
                                                         (file-exists? (build-path dir "info.ss"))))))))
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
                          ;; If anything in the "cache.rktd" file was bad, then
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
      ;; Write out each collection-root-specific table to a "cache.rktd" file:
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
                (setup-printf "updating" "~a" (path->relative-string/setup p))
                (with-handlers ([exn:fail? (warning-handler (void))])
                  (with-output-to-file p
                    #:exists 'truncate/replace
                    (lambda ()
                      (write (hash-map ht cons))
                      (newline)))))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                       Docs                    ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (scr:call name . xs)
    (parameterize ([current-namespace
                    (namespace-anchor->empty-namespace anchor)])
      (apply (dynamic-require 'setup/scribble name) xs)))

  (define (set-doc:verbose)
    (scr:call 'verbose (verbose)))

  (define (doc:setup-scribblings latex-dest auto-start-doc?)
    (scr:call 'setup-scribblings
              (parallel-workers)
              name-str
              (if no-specific-collections? #f (map cc-path ccs-to-compile))
              latex-dest auto-start-doc? (make-user)
              (lambda (what go alt) (record-error what "Building docs" go alt))
              setup-printf))

  (define (make-docs-step)
    (setup-printf #f "--- building documentation ---")
    (set-doc:verbose)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (setup-printf #f "docs failure: ~a" (exn->string exn)))])
      (doc:setup-scribblings #f (and (not (null? (archives)))
                                     (archive-implies-reindex)))))

  (define (doc-pdf-dest-step)
    (setup-printf #f "--- building PDF documentation (via pdflatex) ---")
    (let ([dest-dir (path->complete-path (doc-pdf-dest))])
      (unless (directory-exists? dest-dir)
        (make-directory dest-dir))
      (let ([tmp-dir (build-path (find-system-path 'temp-dir)
                                 (format "pltpdfdoc~a" (current-seconds)))])
        (dynamic-wind
          void
          (lambda ()
            (make-directory tmp-dir)
            (set-doc:verbose)
            (doc:setup-scribblings tmp-dir #f)
            (parameterize ([current-directory tmp-dir])
              (for ([f (directory-list)]
                    #:when (regexp-match? #rx#"[.]tex$" (path-element->bytes f)))
                (let* ([pdf (scr:call 'run-pdflatex f
                                      (lambda (fmt . xs)
                                        (apply setup-printf #f fmt xs)))]
                       [target (build-path dest-dir pdf)])
                  (when (file-exists? target) (delete-file target))
                  (copy-file pdf target)))))
          (lambda ()
            (when (directory-exists? tmp-dir)
              (delete-directory/files tmp-dir)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                  Make Launchers               ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-launchers-step)
    (setup-printf #f "--- creating launchers ---")
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
               (for ([mzln (in-list mzlns)]
                     [mzll (in-list (or mzlls (map (lambda (_) #f) mzlns)))]
                     [mzlf (in-list (or mzlfs (map (lambda (_) #f) mzlns)))])
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
                      (case kind
                        [(gui)     (path->relative-string/gui-bin p)]
                        [(console) (path->relative-string/console-bin p)]
                        [else (error 'make-launcher "internal error (~s)" kind)])
                      (let ([v (current-launcher-variant)])
                        (if (eq? v (system-type 'gc)) "" (format " [~a]" v))))
                     (make-launcher
                      (or mzlf
                          (if (cc-collection cc)
                            (list "-l-" (string-append
                                         (string-append*
                                          (map (lambda (s) (format "~a/" s))
                                               (cc-collection cc)))
                                         mzll))
                            (list "-t-" (path->string (build-path (cc-path cc) mzll)))))
                      p
                      aux))))]
              [else
               (let ([fault (if (or (not mzlls)
                                    (= (length mzlns) (length mzlls)))
                              'f 'l)])
                 (setup-printf
                  "WARNING"
                  "~s launcher name list ~s doesn't match ~a list; ~s"
                  kind mzlns
                  (if (eq? 'l fault) "library" "flags")
                  (if (eq? fault 'l) mzlls mzlfs)))]))
          (for ([variant (available-gracket-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher 'gui
                             'gracket-launcher-names
                             'gracket-launcher-libraries
                             'gracket-launcher-flags
                             gracket-program-launcher-path
                             make-gracket-launcher
                             gracket-launcher-up-to-date?)
              (make-launcher 'gui
                             'mred-launcher-names
                             'mred-launcher-libraries
                             'mred-launcher-flags
                             mred-program-launcher-path
                             make-mred-launcher
                             mred-launcher-up-to-date?)))
          (for ([variant (available-racket-variants)])
            (parameterize ([current-launcher-variant variant])
              (make-launcher 'console
                             'racket-launcher-names
                             'racket-launcher-libraries
                             'racket-launcher-flags
                             racket-program-launcher-path
                             make-racket-launcher
                             racket-launcher-up-to-date?)
              (make-launcher 'console
                             'mzscheme-launcher-names
                             'mzscheme-launcher-libraries
                             'mzscheme-launcher-flags
                             mzscheme-program-launcher-path
                             make-mzscheme-launcher
                             mzscheme-launcher-up-to-date?)))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; setup-unit Body                ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setup-printf "version" "~a [~a]" (version) (system-type 'gc))
  (setup-printf "variants" "~a" (string-join (map symbol->string (available-mzscheme-variants)) ", "))
  (setup-printf "main collects" "~a" (path->string main-collects-dir))
  (setup-printf "collects paths" (if (null? (current-library-collection-paths)) " empty!" ""))
  (for ([p (current-library-collection-paths)])
    (setup-printf #f "  ~a" (path->string p)))

  (when (and (not (null? (archives))) no-specific-collections?)
    (done))

  (when (clean) (clean-step))
  (when (make-zo)
    (compiler:option:verbose (compiler-verbose))
    (compiler:option:compile-subcollections #f))

  (do-install-part 'pre)

  (when (make-zo) (make-zo-step))
  (when (make-info-domain) (make-info-domain-step))

  (when (make-launchers) (make-launchers-step))

  (when (make-docs)
    ;; Double-check that "setup/scribble" is present.
    (when (file-exists? (collection-file-path "scribble.rkt" "setup"))
      (make-docs-step)))
  (when (doc-pdf-dest) (doc-pdf-dest-step))

  (do-install-part 'general)
  (do-install-part 'post)

  (done))
