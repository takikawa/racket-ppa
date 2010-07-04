(module config mzscheme
  (require mzlib/file mzlib/list)

  ;; This module should be invoked when we're in the server directory
  (provide server-dir)
  (define server-dir (or (getenv "PLT_HANDINSERVER_DIR") (current-directory)))

  (define config-file (path->complete-path "config.ss" server-dir))

  (define poll-freq 2000.0) ; poll at most once every two seconds

  (define last-poll     #f)
  (define last-filetime #f)
  (define raw-config    #f)
  (define config-cache  #f)

  (provide get-conf)
  (define (get-conf key)
    (unless (and raw-config
                 (< (- (current-inexact-milliseconds) last-poll) poll-freq))
      (set! last-poll (current-inexact-milliseconds))
      (let ([filetime (file-or-directory-modify-seconds config-file)])
        (unless (and filetime (equal? filetime last-filetime))
          (set! last-filetime filetime)
          (set! raw-config
                (with-handlers ([void (lambda (_)
                                        (error 'get-conf
                                               "could not read conf (~a)"
                                               config-file))])
                  (when raw-config
                    ;; can't use log-line from logger, since it makes a cycle
                    (fprintf (current-error-port)
                             (format "loading configuration from ~a\n"
                                     config-file)))
                  (with-input-from-file config-file read)))
          (set! config-cache (make-hash-table)))))
    (hash-table-get config-cache key
      (lambda ()
        (let*-values ([(default translate) (config-default+translate key)]
                      ;; translate = #f => this is a computed value
                      [(v) (if translate
                             (translate (cond [(assq key raw-config) => cadr]
                                              [else default]))
                             default)])
          (hash-table-put! config-cache key v)
          v))))

  (define (id x) x)
  (define (rx s) (if (regexp? s) s (regexp s)))
  (define (path p) (path->complete-path p server-dir))
  (define (path/false p) (and p (path p)))
  (define (path-list l) (map path l))

  (define (config-default+translate which)
    (case which
      [(active-dirs)             (values '()                   path-list    )]
      [(inactive-dirs)           (values '()                   path-list    )]
      [(port-number)             (values 7979                  id           )]
      [(https-port-number)       (values #f                    id           )]
      [(hook-file)               (values #f                    path/false   )]
      [(session-timeout)         (values 300                   id           )]
      [(session-memory-limit)    (values 40000000              id           )]
      [(default-file-name)       (values "handin.scm"          id           )]
      [(max-upload)              (values 500000                id           )]
      [(max-upload-keep)         (values 9                     id           )]
      [(user-regexp)             (values #rx"^[a-z][a-z0-9]+$" rx           )]
      [(user-desc)               (values "alphanumeric string" id           )]
      [(username-case-sensitive) (values #f                    id           )]
      [(allow-new-users)         (values #f                    id           )]
      [(allow-change-info)       (values #f                    id           )]
      [(master-password)         (values #f                    id           )]
      [(web-base-dir)            (values #f                    path/false   )]
      [(log-output)              (values #t                    id           )]
      [(log-file)                (values "log"                 path/false   )]
      [(web-log-file)            (values #f                    path/false   )]
      [(extra-fields)
       (values '(("Full Name" #f #f)
                 ("ID#" #f #f)
                 ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
                  "a valid email address"))
               id)]
      ;; computed from the above (mark by translate = #f)
      [(all-dirs)
       (values (append (get-conf 'active-dirs) (get-conf 'inactive-dirs)) #f)]
      [(names-dirs) ; see below
       (values (paths->map (get-conf 'all-dirs)) #f)]
      [(user-fields)
       (values (filter (lambda (f) (not (eq? '- (cadr f))))
                       (get-conf 'extra-fields))
               #f)]
      [else (error 'get-conf "unknown configuration entry: ~s" which)]))

  ;; This is used below to map names to submission directory paths and back
  ;; returns a (list-of (either (list name path) (list path name)))
  (define (paths->map dirs)
    (define (path->name dir)
      (unless (directory-exists? dir)
        (error 'get-conf
               "directory entry for an inexistent directory: ~e" dir))
      (let-values ([(_1 name _2) (split-path dir)])
        (bytes->string/locale (path-element->bytes name))))
    (let ([names (map path->name dirs)])
      (append (map list names dirs) (map list dirs names))))

  ;; Translates an assignment name to a directory path or back
  (provide assignment<->dir)
  (define (assignment<->dir a/d)
    (cond [(assoc a/d (get-conf 'names-dirs)) => cadr]
          [else (error 'assignment<->dir "internal error: ~e" a/d)]))

  )
