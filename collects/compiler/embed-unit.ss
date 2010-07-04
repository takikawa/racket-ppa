
(module embed-unit scheme/base
  (require scheme/unit
	   scheme/path
           scheme/file
	   scheme/port
	   scheme/promise
           syntax/moddep
           syntax/modcollapse
           xml/plist
           setup/dirs
           setup/variant
	   "embed-sig.ss"
	   "private/winicon.ss"
           "private/winsubsys.ss"
	   "private/macfw.ss"
	   "private/mach-o.ss"
	   "private/windlldir.ss"
	   "private/collects-path.ss")
  
  (provide compiler:embed@)
  
  (define-unit compiler:embed@
    (import)
    (export compiler:embed^)
    
    (define (embedding-executable-is-directory? mred?)
      #f)
    
    (define (embedding-executable-is-actually-directory? mred?)
      (and mred? (eq? 'macosx (system-type))))
    
    (define (embedding-executable-put-file-extension+style+filters mred?)
      (case (system-type)
        [(windows) (values "exe" null '(("Executable" "*.exe")))]
        [(macosx) (if mred?
                      (values "app" '(enter-packages) '(("App" "*.app")))
                      (values #f null null))]
        [else (values #f null null)]))
    
    (define (embedding-executable-add-suffix path mred?)
      (let* ([path (if (string? path)
                       (string->path path)
                       path)]
             [fixup (lambda (re sfx)
                      (if (regexp-match re (path->bytes path))
                          path
                          (path-replace-suffix path sfx)))])
        (case (system-type)
          [(windows) (fixup #rx#"[.][eE][xX][eE]$" #".exe")]
          [(macosx) (if mred?
                        (fixup #rx#"[.][aA][pP][pP]$" #".app")
                        path)]
          [else path])))
    
    (define (mac-dest->executable dest mred?)
      (if mred?
          (let-values ([(base name dir?) (split-path dest)])
            (build-path dest
                        "Contents" "MacOS"
                        (path-replace-suffix name #"")))
          dest))
    
    ;; Find executable relative to the "mzlib"
    ;; collection.
    (define (find-exe mred? variant)
      (let* ([base (if mred?
                       (find-gui-bin-dir)
                       (find-console-bin-dir))]
             [fail
              (lambda ()
                (error 'create-embedding-executable
                       "can't find ~a executable for variant ~a"
                       (if mred? "MrEd" "MzScheme")
                       variant))])
        (let ([exe (build-path
                    base
                    (case (system-type)
                      [(macosx)
                       (cond
                         [(not mred?)
                          ;; Need MzScheme:
                          (string-append "mzscheme" (variant-suffix variant #f))]
                         [mred?
                          ;; Need MrEd:
                          (let ([sfx (variant-suffix variant #t)])
                            (build-path (format "MrEd~a.app" sfx)
                                        "Contents" "MacOS" 
                                        (format "MrEd~a" sfx)))])]
                      [(windows)
                       (format "~a~a.exe" (if mred?
                                              "MrEd"
                                              "MzScheme")
                               (variant-suffix variant #t))]
                      [(unix)
                       (format "~a~a" (if mred?
                                          "mred"
                                          "mzscheme")
                               (variant-suffix variant #f))]))])
          (unless (or (file-exists? exe)
                      (directory-exists? exe))
            (fail))
          exe)))
    
    (define exe-suffix?
      (delay (equal? #"i386-cygwin" (path->bytes (system-library-subpath)))))
    
    ;; Find the magic point in the binary:
    (define (find-cmdline what rx)
      (let ([m (regexp-match-positions rx (current-input-port))])
        (if m
            (caar m)
            (error 
             'create-embedding-executable
             (format
              "can't find ~a position in executable"
              what)))))
    
    
    (define (relativize exec-name dest adjust)
      (let ([p (find-relative-path
                (let-values ([(dir name dir?) (split-path 
                                               (normal-case-path
                                                (normalize-path dest)))])
                  dir)
                (normal-case-path (normalize-path exec-name)))])
        (if (relative-path? p)
            (adjust p)
            p)))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (prepare-macosx-mred exec-name dest aux variant)
      (let* ([name (let-values ([(base name dir?) (split-path dest)])
                     (path-replace-suffix name #""))]
             [src (build-path (collection-path "launcher")
                              "Starter.app")]
             [creator (let ([c (assq 'creator aux)])
                        (or (and c
                                 (cdr c))
                            "MrSt"))]
             [file-types (let ([m (assq 'file-types aux)])
                           (and m
                                (pair? (cdr m))
                                (cdr m)))]
             [uti-exports (let ([m (assq 'uti-exports aux)])
                            (and m
                                 (pair? (cdr m))
                                 (cdr m)))]
             [resource-files (let ([m (assq 'resource-files aux)])
                               (and m
                                    (cdr m)))])
        (when creator
          (unless (and (string? creator) (= 4 (string-length creator)))
            (error 'make-executable "creator is not a 4-character string: ~e" creator)))
        (when file-types
          (unless (and (list? file-types)
                       (andmap list? file-types)
                       (andmap (lambda (spec)
                                 (andmap (lambda (p)
                                           (and (list? p)
                                                (= 2 (length p))
                                                (string? (car p))))
                                         spec))
                               file-types))
            (error 'make-executable "bad file-types spec: ~e" file-types)))
        (when resource-files
          (unless (and (list? resource-files)
                       (andmap path-string?
                               resource-files))
            (error 'make-executable "resource-files is not a list of paths: ~e" resource-files)))
        
        (when (or (directory-exists? dest)
                  (file-exists? dest)
                  (link-exists? dest))
          (delete-directory/files dest))
        (make-directory* (build-path dest "Contents" "Resources"))
        (make-directory* (build-path dest "Contents" "MacOS"))
        (copy-file exec-name (build-path dest "Contents" "MacOS" name))
        (copy-file (build-path src "Contents" "PkgInfo")
                   (build-path dest "Contents" "PkgInfo"))
        (let ([icon (or (let ([icon (assq 'icns aux)])
                          (and icon
                               (cdr icon)))
                        (build-path src "Contents" "Resources" "Starter.icns"))])
          (copy-file icon
                     (build-path dest "Contents" "Resources" "Starter.icns")))
        (let ([orig-plist (call-with-input-file (build-path src
                                                            "Contents"
                                                            "Info.plist")
                            read-plist)]
              [plist-replace (lambda (plist . l)
                               (let loop ([plist plist][l l])
                                 (if (null? l)
                                     plist
                                     (let ([key (car l)]
                                           [val (cadr l)])
                                       (loop `(dict
                                               ,@(let loop ([c (cdr plist)])
                                                   (cond
                                                     [(null? c) (list (list 'assoc-pair key val))]
                                                     [(string=? (cadar c) key)
                                                      (cons (list 'assoc-pair key val)
                                                            (cdr c))]
                                                     [else
                                                      (cons (car c)
                                                            (loop (cdr c)))])))
                                             (cddr l))))))])
          (let* ([new-plist (plist-replace
                             orig-plist
                             
                             "CFBundleExecutable" 
                             (path->string name)
                             
                             "CFBundleSignature"
                             creator
                             
                             "CFBundleIdentifier" 
                             (format "org.plt-scheme.~a" (path->string name)))]
                 [new-plist (if uti-exports
                                (plist-replace
                                 new-plist
                                 "UTExportedTypeDeclarations"
                                 (cons 'array
                                       (map (lambda (spec)
                                              (cons
                                               'dict
                                               (map (lambda (p)
                                                      (list
                                                       'assoc-pair
                                                       (car p)
                                                       (cadr p)))
                                                    spec)))
                                            uti-exports)))
                                new-plist)]
                 [new-plist (if file-types
                                (plist-replace
                                 new-plist                                 
                                 "CFBundleDocumentTypes"
                                 (cons 'array
                                       (map (lambda (spec)
                                              (cons
                                               'dict
                                               (map (lambda (p)
                                                      (list
                                                       'assoc-pair
                                                       (car p)
                                                       (cadr p)))
                                                    spec)))
                                            file-types)))                                
                                new-plist)])
            (call-with-output-file (build-path dest 
                                               "Contents" 
                                               "Info.plist")
              #:exists 'truncate
              (lambda (port)
                (write-plist new-plist port)))))
        (call-with-output-file (build-path dest 
                                           "Contents" 
                                           "PkgInfo")
          #:exists 'truncate
          (lambda (port)
            (fprintf port "APPL~a" creator)))
        (when resource-files
          (for-each (lambda (p)
                      (let-values ([(base name dir?) (split-path p)])
                        (copy-file p (build-path dest
                                                 "Contents" 
                                                 "Resources"
                                                 name))))
                    resource-files))
        (build-path dest "Contents" "MacOS" name)))
    
    (define (finish-osx-mred dest flags exec-name keep-exe? relative?)
      (call-with-output-file (build-path dest 
                                         "Contents" 
                                         "Resources" 
                                         "starter-info")
        #:exists 'truncate
        (lambda (port)
          (write-plist 
           `(dict ,@(if keep-exe?
                        `((assoc-pair "executable name"
                                      ,(path->string 
                                        (if relative?
                                            (relativize exec-name dest
                                                        (lambda (p)
                                                          (build-path 'up 'up 'up p)))
                                            exec-name))))
                        null)
                  (assoc-pair "stored arguments"
                              (array ,@flags)))
           port))))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Represent modules with lists starting with the filename, so we
    ;; can use assoc:
    (define (make-mod normal-file-path normal-module-path code name prefix full-name relative-mappings runtime-paths)
      (list normal-file-path normal-module-path code
            name prefix full-name relative-mappings runtime-paths))
    
    (define (mod-file m) (car m))
    (define (mod-mod-path m) (cadr m))
    (define (mod-code m) (caddr m))
    (define (mod-name m) (list-ref m 3))
    (define (mod-prefix m) (list-ref m 4))
    (define (mod-full-name m) (list-ref m 5))
    (define (mod-mappings m) (list-ref m 6))
    (define (mod-runtime-paths m) (list-ref m 7))
    
    (define (generate-prefix)
      (format "#%embedded:~a:" (gensym)))
    
    (define (normalize filename)
      (let ([f (simplify-path (cleanse-path filename))])
        ;; Use normal-case-path on just the base part, to avoid
        ;; changing the filename case (which should match the
        ;; module-name case within the file):
        (let-values ([(base name dir?) (split-path f)])
          (if (path? base)
              (build-path (normal-case-path base) name)
              f))))
    
    (define (is-lib-path? a)
      (or (and (pair? a)
               (eq? 'lib (car a)))
          (symbol? a)))
      
    (define (unix-style-split p)
      (let ([m (regexp-match #rx"^([^/]*)/(.*)$" p)])
        (if m
            (cons (cadr m) (unix-style-split (caddr m)))
            (list p))))
    
    (define (extract-last l)
      (let loop ([l l][dirs null])
        (if (null? (cdr l))
            (values (reverse dirs) (car l))
            (loop (cdr l) (cons (car l) dirs)))))
    
    (define (lib-module-filename collects-dest module-path)
      (let-values ([(dir file) 
                    (let ([s (lib-path->string module-path)])
                      (extract-last (unix-style-split s)))])
        (let ([p (build-path collects-dest
                             (apply build-path dir)
                             "compiled"
                             (path-add-suffix file #".zo"))])
          (let-values ([(base name dir?) (split-path p)])
            (make-directory* base)
            p))))

    (define (file-date f)
      (with-handlers ([exn:fail:filesystem? (lambda (x) -inf.0)])
        (file-or-directory-modify-seconds f)))
    
    (define-struct extension (path))
    
    ;; Loads module code, using .zo if there, compiling from .scm if not
    (define (get-code filename module-path codes prefixes verbose? collects-dest on-extension 
                      compiler expand-namespace get-extra-imports)
      (let ([a (assoc filename (unbox codes))])
        (if a
            ;; Already have this module. Make sure that library-referenced
            ;;  modules are consistently referenced through library paths:
            (let ([found-lib? (is-lib-path? (mod-mod-path a))]
                  [look-lib? (is-lib-path? module-path)])
              (cond
                [(and found-lib? look-lib?)
                 'ok]
                [(or found-lib? look-lib?)
                 (error 'find-module
                        "module referenced both as a library and through a path: ~a"
                        filename)]
                [else 'ok]))
            ;; First use of the module. Get code and then get code for imports.
            (begin
              (when verbose?
                (fprintf (current-error-port) "Getting ~s~n" filename))
              (let ([code (get-module-code filename
                                           "compiled"
                                           compiler
                                           (if on-extension
                                               (lambda (f l?)
                                                 (on-extension f l?)
                                                 #f)
                                               (lambda (file _loader?)
                                                 (if _loader?
                                                     (error 'create-embedding-executable
                                                            "cannot use a _loader extension: ~e"
                                                            file)
                                                     (make-extension file))))
                                           #:choose
                                           ;; Prefer extensions, if we're handling them:
                                           (lambda (src zo so)
                                             (if on-extension
                                                 #f
                                                 (if (and (file-exists? so)
                                                          ((file-date so) . >= . (file-date zo)))
                                                     'so
                                                     #f))))]
                    [name (let-values ([(base name dir?) (split-path filename)])
                            (path->string (path-replace-suffix name #"")))]
                    [prefix (let ([a (assoc filename prefixes)])
                              (if a
                                  (cdr a)
                                  (generate-prefix)))])
                (cond
                  [(extension? code)
		   (when verbose?
		     (fprintf (current-error-port) " using extension: ~s~n" (extension-path code)))
                   (set-box! codes
                             (cons (make-mod filename module-path code 
                                             name prefix (string->symbol
                                                          (format "~a~a" prefix name))
                                             null null)
                                   (unbox codes)))]
                  [code
                   (let ([importss (module-compiled-imports code)])
                     (let ([all-file-imports (filter (lambda (x) 
                                                       (let-values ([(x base) (module-path-index-split x)])
                                                         (not (and (pair? x)
                                                                   (eq? 'quote (car x))))))
                                                     (apply append (map cdr importss)))]
                           [extra-paths (get-extra-imports filename code)])
                       (let ([sub-files (map (lambda (i) (normalize (resolve-module-path-index i filename)))
                                             all-file-imports)]
                             [sub-paths (map (lambda (i) (collapse-module-path-index i module-path))
                                             all-file-imports)]
                             [extra-files (map (lambda (i) (normalize (resolve-module-path-index (module-path-index-join i #f)
                                                                                                 filename)))
                                               extra-paths)])
                         ;; Get code for imports:
                         (for-each (lambda (sub-filename sub-path)
                                     (get-code sub-filename
                                               sub-path
                                               codes
                                               prefixes
                                               verbose?
                                               collects-dest
                                               on-extension
                                               compiler
                                               expand-namespace
                                               get-extra-imports))
                                   (append sub-files extra-files)
                                   (append sub-paths extra-paths))
                         (let ([runtime-paths
                                (parameterize ([current-namespace expand-namespace])
                                  (eval code)
                                  (let ([module-path
                                         (if (path? module-path)
                                             (path->complete-path module-path)
                                             module-path)])
                                    (syntax-case (expand `(,#'module m mzscheme
                                                            (require (only ,module-path)
                                                                     mzlib/runtime-path)
                                                            (runtime-paths ,module-path))) (quote)
                                      [(_ m mz (#%mb rfs req (quote (spec ...))))
                                       (syntax->datum #'(spec ...))]
                                      [_else (error 'create-empbedding-executable
                                                    "expansion mismatch when getting external paths")])))])
                           (when verbose?
                             (unless (null? runtime-paths)
                               (fprintf (current-error-port) "Runtime paths for ~s: ~s\n"
                                        filename
                                        runtime-paths)))
                           (if (and collects-dest
                                    (is-lib-path? module-path))
                               ;; Install code as .zo:
                               (begin
                                 (with-output-to-file (lib-module-filename collects-dest module-path)
                                   #:exists 'truncate/replace
                                   (lambda ()
                                     (write code)))
                                 ;; Record module as copied
                                 (set-box! codes
                                           (cons (make-mod filename module-path #f
                                                           #f #f #f #f null)
                                                 (unbox codes))))
                               ;; Build up relative module resolutions, relative to this one,
                               ;; that will be requested at run-time.
                               (let ([mappings (map (lambda (sub-i sub-filename sub-path)
                                                      (and (not (and collects-dest
                                                                     (is-lib-path? sub-path)))
                                                           (let-values ([(path base) (module-path-index-split sub-i)])
                                                             (and base ; can be #f if path isn't relative
                                                                  (begin
                                                                    ;; Assert: base should refer to this module:
                                                                    (let-values ([(path2 base2) (module-path-index-split base)])
                                                                      (when (or path2 base2)
                                                                        (error 'embed "unexpected nested module path index")))
                                                                    (let ([m (assoc sub-filename (unbox codes))])
                                                                      (cons path (mod-full-name m))))))))
                                                    all-file-imports sub-files sub-paths)])
                                 ;; Record the module
                                 (set-box! codes
                                           (cons (make-mod filename module-path code 
                                                           name prefix (string->symbol
                                                                        (format "~a~a" prefix name))
                                                           (filter (lambda (p)
                                                                     (and p (cdr p)))
                                                                   mappings)
                                                           runtime-paths)
                                                 (unbox codes)))))))))]
                  [else
                   (set-box! codes
                             (cons (make-mod filename module-path code 
                                             name #f #f
                                             null null)
                                   (unbox codes)))]))))))
    
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (compile-using-kernel e)
      (let ([ns (make-empty-namespace)])
        (namespace-attach-module (current-namespace) ''#%kernel ns)
        (parameterize ([current-namespace ns])
          (namespace-require ''#%kernel)
          (compile e))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (lib-path->string path)
      (cond
       [(null? (cddr path))
        (if (regexp-match #rx"^[^/]*[.]" (cadr path))
            ;; mzlib
            (string-append "mzlib/" (cadr path))
            ;; new-style
            (if (regexp-match #rx"^[^/.]*$" (cadr path))
                (string-append (cadr path) "/main.ss")
                (if (regexp-match #rx"^[^.]*$" (cadr path))
                    ;; need a suffix:
                    (string-append (cadr path) ".ss")
                    (cadr path))))]
       [else
        ;; old-style multi-string:
        (string-append (apply string-append
                              (map (lambda (s)
                                     (string-append s "/"))
                                   (cddr path)))
                       (cadr path))]))
    
    (define (make-module-name-resolver code-l)
      (let ([extensions (filter (lambda (m) (extension? (mod-code m))) code-l)])
        `(module #%resolver '#%kernel
           (let-values ([(orig) (current-module-name-resolver)]
                        [(reg) (namespace-module-registry (current-namespace))]
                        [(mapping-table) (quote
                                          ,(map
                                            (lambda (m)
                                              `(,(mod-full-name m)
                                                ,(mod-mappings m)))
                                            code-l))]
                        [(library-table) (quote
                                          ,(filter values
                                                   (map (lambda (m)
                                                          (let ([path (mod-mod-path m)])
                                                            (cond
                                                             [(and (pair? path)
                                                                   (eq? 'lib (car path)))
                                                              (cons (lib-path->string path)
                                                                    (mod-full-name m))]
                                                             [(and (pair? path)
                                                                   (eq? 'planet (car path)))
                                                              ;; Normalize planet path
                                                              (cons (collapse-module-path path current-directory)
                                                                    (mod-full-name m))]
                                                             [else #f])))
                                                        code-l)))])
             (letrec-values ([(embedded-resolver)
                              (case-lambda 
                               [(name)
                                ;; a notification
                                (orig name)]
                               [(name rel-to stx)
                                (embedded-resolver name rel-to stx #t)]
                               [(name rel-to stx load?)
                                (if (not (module-path? name))
                                    ;; Bad input
                                    (orig name rel-to stx load?)
                                    (if (not (eq? reg (namespace-module-registry (current-namespace))))
                                        ;; Wrong registry
                                        (orig name rel-to stx load?)
                                        ;; Have a relative mapping?
                                        (let-values ([(a) (if rel-to
                                                              (assq (resolved-module-path-name rel-to) mapping-table)
                                                              #f)])
                                          (if a
                                              (let-values ([(a2) (assoc name (cadr a))])
                                                (if a2
                                                    (make-resolved-module-path (cdr a2))
                                                    ;; No relative mapping found (presumably a lib)
                                                    (orig name rel-to stx load?)))
                                              (let-values ([(lname)
                                                            ;; normalize `lib' to single string (same as lib-path->string):
                                                            (let-values ([(name)
                                                                          (if (symbol? name)
                                                                              (list 'lib (symbol->string name))
                                                                              name)])
                                                              (if (pair? name)
                                                                  (if (eq? 'lib (car name))
                                                                      (if (null? (cddr name))
                                                                          (if (regexp-match #rx"^[^/]*[.]" (cadr name))
                                                                              ;; mzlib
                                                                              (string-append "mzlib/" (cadr name))
                                                                              ;; new-style
                                                                              (if (regexp-match #rx"^[^/.]*$" (cadr name))
                                                                                  (string-append (cadr name) "/main.ss")
                                                                                  (if (regexp-match #rx"^[^.]*$" (cadr name))
                                                                                      ;; need a suffix:
                                                                                      (string-append (cadr name) ".ss")
                                                                                      (cadr name))))
                                                                          ;; old-style multi-string
                                                                          (string-append (apply string-append
                                                                                                (map (lambda (s)
                                                                                                       (string-append s "/"))
                                                                                                     (cddr name)))
                                                                                         (cadr name)))
                                                                      (if (eq? 'planet (car name))
                                                                          (if (null? (cddr name))
                                                                              ;; need to normalize:
                                                                              (let-values ([(s) (if (symbol? (cadr name))
                                                                                                    (symbol->string (cadr name))
                                                                                                    (cadr name))])
                                                                                (letrec-values ([(split)
                                                                                                 (lambda (s rx suffix?)
                                                                                                   (let-values ([(m) (regexp-match-positions rx s)])
                                                                                                     (if m
                                                                                                         (cons (substring s 0 (caar m))
                                                                                                               (split (substring s (cdar m))
                                                                                                                      rx suffix?))
                                                                                                         (list
                                                                                                          (if suffix?
                                                                                                              (if (regexp-match? #rx"[.]" s)
                                                                                                                  s
                                                                                                                  (string-append s ".ss"))
                                                                                                              s)))))]
                                                                                                [(last-of)
                                                                                                 (lambda (l)
                                                                                                   (if (null? (cdr l)) (car l) (last-of (cdr l))))]
                                                                                                [(not-last)
                                                                                                 (lambda (l)
                                                                                                   (if (null? (cdr l))
                                                                                                       null
                                                                                                       (cons (car l) (not-last (cdr l)))))])
                                                                                  (let-values ([(parts) (split s #rx"/" #t)])
                                                                                    (let-values ([(vparts) (split (cadr parts) #rx":" #f)])
                                                                                      (cons 'planet
                                                                                            (cons (if (null? (cddr parts))
                                                                                                      "main.ss"
                                                                                                      (last-of parts))
                                                                                                  (cons
                                                                                                   (cons (car parts) 
                                                                                                         (cons (string-append (car vparts) ".plt")
                                                                                                               ;; FIXME: finish parsing version:
                                                                                                               (cdddr parts)))
                                                                                                   (not-last (cddr parts)))))))))
                                                                              ;; already in long form:
                                                                              name)
                                                                          #f))
                                                                  #f))]
                                                           [(planet-match?)
                                                            (lambda (a b)
                                                              (if (equal? (cons (car a) (cddr a))
                                                                          (cons (car b) (cddr b)))
                                                                  (let-values ([(a) (cadr a)]
                                                                               [(b) (cadr b)])
                                                                    (if (equal? (car a) (car b))
                                                                        (if (equal? (cadr a) (cadr b))
                                                                            ;; Everything matches up to the version...
                                                                            ;; FIXME: check version. (Since the version isn't checked,
                                                                            ;; this currently works only when a single version of the
                                                                            ;; package is used in the executable.)
                                                                            #t
                                                                            #f)
                                                                        #f))
                                                                  #f))])
                                                ;; A library mapping that we have? 
                                                (let-values ([(a3) (if lname
                                                                       (if (string? lname)
                                                                           ;; lib
                                                                           (assoc lname library-table)
                                                                           ;; planet
                                                                           (ormap (lambda (e)
                                                                                    (if (string? (car e))
                                                                                        #f
                                                                                        (if (planet-match? (cdar e) (cdr lname))
                                                                                            e
                                                                                            #f)))
                                                                                  library-table))
                                                                       #f)])
                                                  (if a3
                                                      ;; Have it:
                                                      (make-resolved-module-path (cdr a3))
                                                      ;; Let default handler try:
                                                      (orig name rel-to stx load?))))))))])])
               (current-module-name-resolver embedded-resolver))))))
    
    ;; Write a module bundle that can be loaded with 'load' (do not embed it
    ;; into an executable). The bundle is written to the current output port.
    (define (do-write-module-bundle verbose? modules literal-files literal-expressions collects-dest
                                    on-extension program-name compiler expand-namespace 
                                    src-filter get-extra-imports)
      (let* ([module-paths (map cadr modules)]
             [files (map
                     (lambda (mp)
                       (let ([f (resolve-module-path mp #f)])
                         (unless f
                           (error 'write-module-bundle "bad module path: ~e" mp))
                         (normalize f)))
                     module-paths)]
             [collapsed-mps (map
                             (lambda (mp)
                               (collapse-module-path mp (build-path (current-directory) "dummy.ss")))
                             module-paths)]
             [prefix-mapping (map (lambda (f m)
                                    (cons f (let ([p (car m)])
                                              (cond
                                                [(symbol? p) (symbol->string p)]
                                                [(eq? p #t) (generate-prefix)]
                                                [(not p) ""]
                                                [else (error
                                                       'write-module-bundle
                                                       "bad prefix: ~e"
                                                       p)]))))
                                  files modules)]
             ;; Each element is created with `make-mod'.
             ;; As we descend the module tree, we append to the front after
             ;; loasing imports, so the list in the right order.
             [codes (box null)])
        (for-each (lambda (f mp) (get-code f mp codes prefix-mapping verbose? collects-dest
                                           on-extension compiler expand-namespace
                                           get-extra-imports))
                  files
                  collapsed-mps)
        ;; Drop elements of `codes' that just record copied libs:
        (set-box! codes (filter mod-code (unbox codes)))
        ;; Bind `module' to get started:
        (write (compile-using-kernel '(namespace-require '(only '#%kernel module))))
        ;; Install a module name resolver that redirects
        ;; to the embedded modules
        (write (make-module-name-resolver (filter mod-code (unbox codes))))
        (write (compile-using-kernel '(namespace-require ''#%resolver)))
        ;; Write the extension table and copy module code:
        (let* ([l (reverse (unbox codes))]
               [extensions (filter (lambda (m) (extension? (mod-code m))) l)]
               [runtimes (filter (lambda (m) (pair? (mod-runtime-paths m))) l)]
               [table-mod
                (if (null? runtimes)
                    #f
                    (let* ([table-sym (module-path-index-resolve 
                                       (module-path-index-join '(lib "runtime-path-table.ss" "mzlib" "private")
                                                               #f))]
                           [table-path (resolved-module-path-name table-sym)])
                      (assoc (normalize table-path) l)))])
          (unless (null? extensions)
            ;; The extension table:`
            (write 
             `(module #%extension-table '#%kernel
                (#%require '#%utils)
                (let-values ([(eXtEnSiOn-modules) ;; this name is magic for the exe->distribution process
                              (quote ,(map (lambda (m)
                                             (let ([p (extension-path (mod-code m))])
                                               (when verbose?
                                                 (fprintf (current-error-port) "Recording extension at ~s~n" p))
                                               (list (path->bytes p)
                                                     (mod-full-name m)
                                                     ;; The program name isn't used. It just helps ensures that
                                                     ;; there's plenty of room in the executable for patching
                                                     ;; the path later when making a distribution.
                                                     (path->bytes program-name))))
                                           extensions))])
                  (for-each (lambda (pr)
                              (current-module-declare-name (make-resolved-module-path (cadr pr)))
                              (let-values ([(p) (bytes->path (car pr))])
                                (load-extension (if (relative-path? p)
                                                    (let-values ([(d) (current-directory)])
                                                      (current-directory (find-system-path 'orig-dir))
                                                      (begin0
                                                       (let-values ([(p2) (find-executable-path (find-system-path 'exec-file) p #t)])
                                                         (if p2
                                                             p2
                                                             (path->complete-path p (current-directory))))
                                                       (current-directory d)))
                                                    p))))
                            eXtEnSiOn-modules))))
            (write (compile-using-kernel '(namespace-require ''#%extension-table))))
          ;; Runtime-path table:
          (unless (null? runtimes)
            (unless table-mod
              (error 'create-embedding-executable "cannot find module for runtime-path table"))
            (write (compile-using-kernel
                    `(current-module-declare-name (make-resolved-module-path 
                                                   ',(mod-full-name table-mod)))))
            (write `(module runtime-path-table '#%kernel
                      (#%provide table)
                      (define-values (table)
                        (make-immutable-hash
                         (let-values ([(rUnTiMe-paths) ; this is a magic name for exe->distribution process
                                       ',(apply append
                                                (map (lambda (nc)
                                                       (map (lambda (p)
                                                              (list
                                                               (cons (mod-full-name nc)
                                                                     (if (path? p)
                                                                         (path->bytes p)
                                                                         p))
                                                               (let ([p (cond
                                                                         [(bytes? p) (bytes->path p)]
                                                                         [(and (list? p) (= 2 (length p)) 
                                                                               (eq? 'so (car p)))
                                                                          (let ([f (path-replace-suffix (cadr p) 
                                                                                                        (system-type 'so-suffix))])
                                                                            (ormap (lambda (p)
                                                                                     (let ([p (build-path p f)])
                                                                                       (and (file-exists? p)
                                                                                            p)))
                                                                                   (get-lib-search-dirs)))]
                                                                         [(and (list? p)
                                                                               (eq? 'lib (car p)))
                                                                          (build-path (if (null? (cddr p))
                                                                                          (collection-path "mzlib")
                                                                                          (apply collection-path (cddr p)))
                                                                                      (cadr p))]
                                                                         [else p])])
                                                                 (and p
                                                                      (path->bytes 
                                                                       (if (absolute-path? p)
                                                                           p
                                                                           (build-path (path-only (mod-file nc)) p)))))
                                                               ;; As for the extension table, a placeholder to save 
                                                               ;; room likely needed by the distribution-mangler
                                                               (bytes-append #"................." (path->bytes program-name))))
                                                            (mod-runtime-paths nc)))
                                                     runtimes))])
                           rUnTiMe-paths))))))
          ;; Copy module code:
          (for-each
           (lambda (nc)
             (unless (or (extension? (mod-code nc))
                         (eq? nc table-mod))
               (when verbose?
                 (fprintf (current-error-port) "Writing module from ~s~n" (mod-file nc)))
               (write (compile-using-kernel
                       `(current-module-declare-name 
                         (make-resolved-module-path
                          ',(mod-full-name nc)))))
               (if (src-filter (mod-file nc))
                   (with-input-from-file (mod-file nc)
                     (lambda ()
                       (copy-port (current-input-port) (current-output-port))))
                   (write (mod-code nc)))))
           l))
        (write (compile-using-kernel '(current-module-declare-name #f)))
        ;; Remove `module' binding before we start running user code:
        (write (compile-using-kernel '(namespace-set-variable-value! 'module #f #t)))
        (write (compile-using-kernel '(namespace-undefine-variable! 'module)))
        (newline)
        (for-each (lambda (f)
                    (when verbose?
                      (fprintf (current-error-port) "Copying from ~s~n" f))
                    (call-with-input-file* f
                      (lambda (i)
                        (copy-port i (current-output-port)))))
                  literal-files)
        (for-each write literal-expressions)))

    (define (write-module-bundle #:verbose? [verbose? #f]
                                 #:modules [modules null]
                                 #:literal-files [literal-files null]
                                 #:literal-expressions [literal-expressions null]
                                 #:on-extension [on-extension #f]
                                 #:expand-namespace [expand-namespace (current-namespace)]
                                 #:compiler [compiler (lambda (expr)
                                                        (parameterize ([current-namespace expand-namespace])
                                                          (compile expr)))]
                                 #:src-filter [src-filter (lambda (filename) #f)]
                                 #:get-extra-imports [get-extra-imports (lambda (filename code) null)])
      (do-write-module-bundle verbose? modules literal-files literal-expressions
                              #f ; collects-dest
                              on-extension
                              "?" ; program-name 
                              compiler expand-namespace 
                              src-filter get-extra-imports))

    
    ;; The old interface:
    (define make-embedding-executable
      (lambda (dest mred? verbose? 
                    modules 
                    literal-files literal-expression
                    cmdline
                    [aux null]
                    [launcher? #f]
                    [variant (system-type 'gc)])
        (create-embedding-executable dest
                                     #:mred? mred?
                                     #:verbose? verbose?
                                     #:modules modules
                                     #:literal-files literal-files
                                     #:literal-expression literal-expression
                                     #:cmdline cmdline
                                     #:aux aux
                                     #:launcher? launcher?
                                     #:variant variant)))
    
    ;; Use `write-module-bundle', but figure out how to put it into an executable
    (define (create-embedding-executable dest
                                         #:mred? [mred? #f]
                                         #:verbose? [verbose? #f]
                                         #:modules [modules null]
                                         #:literal-files [literal-files null]
                                         #:literal-expression [literal-expression #f]
                                         #:literal-expressions [literal-expressions
                                                                (if literal-expression
                                                                    (list literal-expression)
                                                                    null)]
                                         #:cmdline [cmdline null]
                                         #:aux [aux null]
                                         #:launcher? [launcher? #f]
                                         #:variant [variant (system-type 'gc)]
                                         #:collects-path [collects-path #f]
                                         #:collects-dest [collects-dest #f]
                                         #:on-extension [on-extension #f]
                                         #:expand-namespace [expand-namespace (current-namespace)]
                                         #:compiler [compiler (lambda (expr)
                                                                (parameterize ([current-namespace expand-namespace])
                                                                  (compile expr)))]
                                         #:src-filter [src-filter (lambda (filename) #f)]
                                         #:get-extra-imports [get-extra-imports (lambda (filename code) null)])
      (define keep-exe? (and launcher?
                             (let ([m (assq 'forget-exe? aux)])
                               (or (not m)
                                   (not (cdr m))))))
      (define unix-starter? (and (eq? (system-type) 'unix)
                                 (let ([m (assq 'original-exe? aux)])
                                   (or (not m)
                                       (not (cdr m))))))
      (define long-cmdline? (or (eq? (system-type) 'windows)
                                (and mred? (eq? 'macosx (system-type)))
                                unix-starter?))
      (define relative? (let ([m (assq 'relative? aux)])
                          (and m (cdr m))))
      (define collects-path-bytes (collects-path->bytes 
                                   ((if (and mred?
                                             (eq? 'macosx (system-type)))
                                        mac-mred-collects-path-adjust
                                        values)
                                    collects-path)))
      (unless (or long-cmdline?
                  ((apply + (length cmdline) (map (lambda (s)
                                                    (bytes-length (string->bytes/utf-8 s)))
                                                  cmdline)) . < . 50))
        (error 'create-embedding-executable "command line too long"))
      (check-collects-path 'create-embedding-executable collects-path collects-path-bytes)
      (let ([exe (find-exe mred? variant)])
        (when verbose?
          (fprintf (current-error-port) "Copying to ~s~n" dest))
        (let-values ([(dest-exe orig-exe osx?)
                      (cond
                        [(and mred? (eq? 'macosx (system-type)))
                         (values (prepare-macosx-mred exe dest aux variant) #f #t)]
                        [unix-starter?
                         (let ([starter (build-path (find-lib-dir) 
                                                    (if (force exe-suffix?)
                                                        "starter.exe"
                                                        "starter"))])
                           (when (or (file-exists? dest)
                                     (directory-exists? dest)
                                     (link-exists? dest))
                             (delete-file dest))
                           (copy-file starter dest)
                           (values dest starter #f))]
                        [else
                         (when (or (file-exists? dest)
                                   (directory-exists? dest)
                                   (link-exists? dest))
                           ;; Delete-file isn't enough if the target
                           ;;  is supposed to be a directory. But
                           ;;  currently, that happens only for MrEd 
                           ;;  on Mac OS X, which is handles above.
                           (delete-file dest))
                         (copy-file exe dest)
                         (values dest exe #f)])])
          (with-handlers ([void (lambda (x)
                                  (if osx?
                                      (when (directory-exists? dest)
                                        (delete-directory/files dest))
                                      (when (file-exists? dest)
                                        (delete-file dest)))
                                  (raise x))])
            (when (and (eq? 'macosx (system-type))
                       (not unix-starter?))
              (let ([m (assq 'framework-root aux)])
                (if m
                    (when (cdr m)
                      (update-framework-path (cdr m) 
                                             (mac-dest->executable dest mred?)
                                             mred?))
                    ;; Check whether we need an absolute path to frameworks:
                    (let ([dest (mac-dest->executable dest mred?)])
                      (when (regexp-match #rx"^@executable_path" 
                                          (get-current-framework-path dest 
                                                                      (if mred?
                                                                          "PLT_MrEd"
                                                                          "PLT_MzScheme")))
                        (update-framework-path (string-append
                                                (path->string (find-lib-dir))
                                                "/")
                                               dest
                                               mred?))))))
            (when (eq? 'windows (system-type))
              (let ([m (assq 'dll-dir aux)])
                (if m
                    (when (cdr m)
                      (update-dll-dir dest (cdr m)))
                    ;; Check whether we need an absolute path to DLLs:
                    (let ([dir (get-current-dll-dir dest)])
                      (when (relative-path? dir)
                        (let-values ([(orig-dir name dir?) (split-path 
                                                            (path->complete-path orig-exe))])
                          (update-dll-dir dest (build-path orig-dir dir))))))))
            (let ([write-module
                   (lambda ()
                     (do-write-module-bundle verbose? modules literal-files literal-expressions collects-dest
                                             on-extension
                                             (file-name-from-path dest)
                                             compiler
                                             expand-namespace
                                             src-filter
                                             get-extra-imports))])
              (let-values ([(start end)
                            (if (and (eq? (system-type) 'macosx)
                                     (not unix-starter?))
                                ;; For Mach-O, we know how to add a proper segment
                                (let ([s (open-output-bytes)])
                                  (parameterize ([current-output-port s])
                                    (write-module))
                                  (let ([s (get-output-bytes s)])
                                    (let ([start (add-plt-segment dest-exe s)])
                                      (values start
                                              (+ start (bytes-length s))))))
                                ;; Other platforms: just add to the end of the file:
                                (let ([start (file-size dest-exe)])
                                  (with-output-to-file dest-exe write-module 
						       #:exists 'append)
                                  (values start (file-size dest-exe))))])
                (when verbose?
                  (fprintf (current-error-port) "Setting command line~n"))
                (let ([start-s (number->string start)]
                      [end-s (number->string end)])
                  (let ([full-cmdline (append
                                       (if launcher?
                                           (if (and (eq? 'windows (system-type))
                                                    keep-exe?)
                                               ;; argv[0] replacement:
                                               (list (path->string 
                                                      (if relative?
                                                          (relativize exe dest-exe values)
                                                          exe)))
                                               ;; No argv[0]:
                                               null)
                                           (list "-k" start-s end-s))
                                       cmdline)])
                    (when collects-path-bytes
                      (when verbose?
                        (fprintf (current-error-port) "Setting collection path~n"))
                      (set-collects-path dest-exe collects-path-bytes))
                    (cond
                      [osx?
                       (finish-osx-mred dest full-cmdline exe keep-exe? relative?)]
                      [unix-starter?
                       (let ([numpos (with-input-from-file dest-exe 
                                       (lambda () (find-cmdline 
                                                   "configuration"
                                                   #"cOnFiG:")))]
                             [typepos (and (or mred? (eq? variant '3m))
                                           (with-input-from-file dest-exe 
                                             (lambda () (find-cmdline 
                                                         "exeuctable type"
                                                         #"bINARy tYPe:"))))]
                             [cmdline
                              (apply bytes-append
                                     (map (lambda (s)
                                            (bytes-append 
                                             (cond
                                               [(path? s) (path->bytes s)]
                                               [else (string->bytes/locale s)])
                                             #"\0"))
                                          (append
                                           (list (if relative?
                                                     (relativize exe dest-exe values)
                                                     exe)
                                                 (let ([dir (find-dll-dir)])
                                                   (if dir
                                                       (if relative?
                                                           (relativize dir dest-exe values)
                                                           dir)
                                                       "")))
                                           full-cmdline)))]
                             [out (open-output-file dest-exe #:exists 'update)])
                         (let ([cmdline-end (+ end (bytes-length cmdline))]
                               [write-num (lambda (n)
                                            (write-bytes (integer->integer-bytes n 4 #t #f) out))])
                           (dynamic-wind
                            void
                            (lambda ()
                              (when typepos
                                (when mred?
                                  (file-position out (+ typepos 13))
                                  (write-bytes #"r" out))
                                (when (eq? variant '3m)
                                  (file-position out (+ typepos 15))
                                  (write-bytes #"3" out))
                                (flush-output out))
                              (file-position out (+ numpos 7))
                              (write-bytes #"!" out)
                              (write-num start)
                              (write-num end)
                              (write-num cmdline-end)
                              (write-num (length full-cmdline))
                              (write-num (if mred? 1 0))
                              (flush-output out)
                              (file-position out end)
                              (write-bytes cmdline out)
                              (flush-output out))
                            (lambda ()
                              (close-output-port out)))))]
                      [else
                       (let ([cmdpos (with-input-from-file dest-exe 
                                       (lambda () (find-cmdline 
                                                   "cmdline"
                                                   #"\\[Replace me for EXE hack")))]
                             [anotherpos (and mred?
                                              (eq? 'windows (system-type))
                                              (let ([m (assq 'single-instance? aux)])
                                                (and m (not (cdr m))))
                                              (with-input-from-file dest-exe 
                                                (lambda () (find-cmdline 
                                                            "instance-check"
                                                            #"yes, please check for another"))))]
                             [out (open-output-file dest-exe #:exists 'update)])
                         (dynamic-wind
                          void
                          (lambda ()
                            (when anotherpos
                              (file-position out anotherpos)
                              (write-bytes #"no," out))
                            (if long-cmdline?
                                ;; write cmdline at end:
                                (file-position out end)
                                (begin
                                  ;; write (short) cmdline in the normal position:
                                  (file-position out cmdpos)
                                  (display "!" out)))
                            (for-each
                             (lambda (s)
                               (fprintf out "~a~a~c"
                                        (integer->integer-bytes 
                                         (add1 (bytes-length (string->bytes/utf-8 s)) )
                                         4 #t #f)
                                        s
                                        #\000))
                             full-cmdline)
                            (display "\0\0\0\0" out)
                            (when long-cmdline?
                              ;; cmdline written at the end;
                              ;; now put forwarding information at the normal cmdline pos
                              (let ([new-end (file-position out)])
                                (file-position out cmdpos)
                                (fprintf out "~a...~a~a"
                                         (if keep-exe? "*" "?")
                                         (integer->integer-bytes end 4 #t #f)
                                         (integer->integer-bytes (- new-end end) 4 #t #f)))))
                          (lambda ()
                            (close-output-port out)))
                         (let ([m (and (eq? 'windows (system-type))
                                       (assq 'ico aux))])
                           (when m
                             (install-icon dest-exe (cdr m))))
                         (let ([m (and (eq? 'windows (system-type))
                                       (assq 'subsystem aux))])
                           (when m
                             (set-subsystem dest-exe (cdr m)))))])))))))))
    
    ;; For Mac OS X MrEd, the actual executable is deep inside the
    ;;  nominal executable bundle
    (define (mac-mred-collects-path-adjust p)
      (cond
        [(not p) #f]
        [(list? p) (map mac-mred-collects-path-adjust p)]
        [(relative-path? p) (build-path 'up 'up 'up p)]
        [else p]))))

