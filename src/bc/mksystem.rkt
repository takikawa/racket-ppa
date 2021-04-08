(module mksystem '#%kernel
  ;; Arguments are
  ;;   <output-file> [<cpp-command> <3m-exe-suffix> <cross-target-kind> <run-racket-command> <this-racket-command>]
  (define-values (args) (current-command-line-arguments))

  (define-values (mi-target?)
    (if (> (vector-length args) 3)
        (equal? "any" (vector-ref args 3))
        #f))

  (define-values (ht)
    (if (if (= (vector-length args) 1)
            #t
            (equal? (vector-ref args (- (vector-length args) 1))
                    (vector-ref args (- (vector-length args) 2))))
        ;; Not cross-compiling
        (hash 'os (system-type 'os)
              'os* (system-type 'os*)
              'arch (system-type 'arch)
              'word (system-type 'word)
              'gc (if (= (vector-length args) 1)
                      '3m ; GC mode for suffixless executables
                      (if (string=? "" (vector-ref args 2))
                          '3m
                          'cgc))
              'vm (system-type 'vm)
              'link (system-type 'link)
              'machine (bytes->string/utf-8 (path->bytes (system-library-subpath #f)))
              'so-suffix (system-type 'so-suffix)
              'so-mode (system-type 'so-mode)
              'fs-change (system-type 'fs-change)
              'target-machine (if mi-target? #f 'racket)
              'library-subpath (path->bytes (system-library-subpath #f))
              'library-subpath-convention (system-path-convention-type))
        ;; Cross-compiling; use `cpp` to get details
        (begin
          (printf "Extracting system information for cross-compile\n")
          (let-values ([(p out in err)
                        (subprocess #f #f #f "/bin/sh" "-c" (vector-ref args 1))])
            (close-output-port in)
            (letrec-values ([(read-all) (lambda ()
                                          (let-values ([(s) (read-bytes 4096 out)])
                                            (if (eof-object? s)
                                                #""
                                              (bytes-append s (read-all)))))])
              (let-values ([(expanded) (read-all)])
                (let-values ([(get-string)
                              (lambda (var)
                                (let-values ([(m) (regexp-match (string-append " " var " = ([^\n;]*);")
                                                                expanded)])
                                  (if m
                                      (bytes->string/utf-8
                                       (regexp-replace* #rx"\\\\\\\\"
                                                        (regexp-replace* #rx"^\"|\" *\"|\"$" (cadr m) "")
                                                        "\\\\"))
                                      (error 'mksystem "not found in cpp output: ~e" var))))])
                  (let-values ([(get-symbol)
                                (lambda (var) (string->symbol (get-string var)))]
                               [(get-int)
                                (lambda (var) (string->number (get-string var)))])
                    (let-values ([(library-subpath)
                                  (get-string "system_library_subpath")]
                                 [(os) (get-symbol "system_type_os")]
                                 [(os*) (get-symbol "system_type_os_star")]
                                 [(arch) (get-symbol "system_type_arch")])
                      (hash 'os os
                            'os* os*
                            'arch arch
                            'word (* 8 (get-int "system_pointer_size"))
                            'gc (if (string=? "" (vector-ref args 2))
                                    '3m
                                    'cgc)
                            'vm 'racket
                            'link (get-symbol "system_type_link")
                            'machine library-subpath
                            'so-suffix (string->bytes/utf-8 (get-string "system_type_so_suffix"))
                            'so-mode (get-symbol "system_type_so_mode")
                            'fs-change '#(#f #f #f #f)
                            'target-machine (if mi-target? #f 'racket)
                            'library-subpath (string->bytes/utf-8 library-subpath)
                            'library-subpath-convention (if (eq? os 'windows)
                                                            'windows
                                                            'unix)))))))))))

  (call-with-output-file
   (vector-ref args 0)
   (lambda (o)
     (write ht o)
     (newline o))
   'truncate/replace))
