#lang scheme
(require scheme/file
         scheme/runtime-path
         (planet jaymccarthy/job-queue)
         "metadata.ss"
         "run-collect.ss"
         "cache.ss"
         "dirstruct.ss"
         "replay.ss"
         "notify.ss"
         "path-utils.ss"
         "sema.ss"
         "svn.ss")

(define current-env (make-parameter (make-immutable-hash empty)))
(define-syntax-rule (with-env ([env-expr val-expr] ...) expr ...)
  (parameterize ([current-env
                  (for/fold ([env (current-env)])
                    ([k (in-list (list env-expr ...))]
                     [v (in-list (list val-expr ...))])
                    (hash-set env k v))])
    expr ...))

(define (build-revision rev)
  (define rev-dir (revision-dir rev))
  (define co-dir (revision-trunk-dir rev))
  (define src-dir (build-path co-dir "src"))
  (define build-dir (build-path src-dir "build"))
  (define futures-build-dir (build-path src-dir "futures-build"))
  (define log-dir (revision-log-dir rev))
  (define trunk-dir
    (revision-trunk-dir rev))
  (define setup-plt-path
    (path->string (build-path trunk-dir "bin" "setup-plt")))
  ;; Checkout the repository revision
  (cache/file/timestamp
   (build-path rev-dir "checkout-done")
   (lambda ()
     (notify! "Removing checkout directory: ~a" co-dir)
     (safely-delete-directory co-dir)
     (local [(define repo (plt-repository))
             (define to-dir 
               (path->string co-dir))]
       (notify! "Checking out ~a@~a into ~a"
                repo rev to-dir)
       (run/collect/wait/log
        ; XXX Give it its own timeout
        #:timeout (current-make-install-timeout-seconds)
        #:env (current-env)
        (build-path log-dir "svn-checkout")
        (svn-path)
        (list 
         "checkout"
         "--quiet"
         "-r" (number->string rev)
         repo
         to-dir)))))
  ;; Make the build directory
  (make-directory* build-dir)
  ;; Run Configure, Make, Make Install
  (parameterize ([current-directory build-dir])
    (run/collect/wait/log
     #:timeout (current-subprocess-timeout-seconds)
     #:env (current-env)
     (build-path log-dir "src" "build" "configure")
     (path->string (build-path src-dir "configure"))
     empty)
    (run/collect/wait/log
     #:timeout (current-make-timeout-seconds)
     #:env (current-env)
     (build-path log-dir "src" "build" "make")
     (make-path) 
     (list "-j" (number->string (number-of-cpus))))
    (run/collect/wait/log
     #:timeout (current-make-install-timeout-seconds)
     #:env (current-env)
     (build-path log-dir "src" "build" "make-install")
     (make-path) 
     (list "-j" (number->string (number-of-cpus)) "install"))))

(define (call-with-temporary-directory thunk)
  (define tempdir (symbol->string (gensym 'tmpdir)))
  (dynamic-wind
   (lambda ()
     (make-directory* tempdir))
   (lambda ()
     (parameterize ([current-directory tempdir])
       (thunk)))
   (lambda ()
     (delete-directory/files tempdir))))
(define-syntax-rule (with-temporary-directory e)
  (call-with-temporary-directory (lambda () e)))

(define (with-running-program command args thunk)
  (define-values (new-command new-args)
    (command+args+env->command+args
     #:env (current-env)
     command args))
  (define-values
    (the-process stdout stdin stderr)
    (apply subprocess
           #f #;(current-error-port) 
           #f
           #f #;(current-error-port)
           new-command new-args))
  ; Die if this program does
  (define parent
    (current-thread))
  (define waiter
    (thread
     (lambda ()
       (subprocess-wait the-process)
       (printf "Killing parent because wrapper is dead...~n")
       (kill-thread parent))))
  
  ; Run without stdin
  (close-output-port stdin)
  
  (begin0
    ; Run the thunk
    (thunk)
    
    ; Close the output ports
    (close-input-port stdout)
    (close-input-port stderr)
    
    ; Kill the guard
    (kill-thread waiter)
    
    ; Kill the process
    (subprocess-kill the-process #t)))

(define-runtime-path package-list "pkgs")
(define (planet-packages)
  (file->value package-list))

(define (test-revision rev)
  (define rev-dir (revision-dir rev))
  (define trunk-dir
    (revision-trunk-dir rev))
  (define log-dir
    (revision-log-dir rev))
  (define trunk->log
    (rebase-path trunk-dir log-dir))
  (define mzscheme-path
    (path->string (build-path trunk-dir "bin" "mzscheme")))
  (define mzc-path
    (path->string (build-path trunk-dir "bin" "mzc")))
  (define mred-text-path
    (path->string (build-path trunk-dir "bin" "mred-text")))
  (define mred-path
    (path->string (build-path trunk-dir "bin" "mred")))
  (define planet-path
    (path->string (build-path trunk-dir "bin" "planet")))
  (define collects-pth
    (build-path trunk-dir "collects"))
  (define test-workers (make-job-queue (number-of-cpus)))
  (define top-sema (make-semaphore 0))
  (define (test-directory dir-pth upper-sema)
    (define dir-log (build-path (trunk->log dir-pth) ".index.test"))
    (if (read-cache* dir-log)
        (semaphore-post upper-sema)
        (begin        
          (notify! "Testing in ~S" dir-pth)
          (local [(define files (directory-list* dir-pth))
                  (define how-many (length files))
                  (define dir-sema (make-semaphore 0))]     
            (for-each (lambda (sub-pth)
                        (define pth (build-path dir-pth sub-pth))
                        (define directory? (directory-exists? pth))
                        (if directory?
                            (test-directory pth dir-sema)
                            (local [(define log-pth (trunk->log pth))]
                              (if (file-exists? log-pth)
                                  (semaphore-post dir-sema)
                                  (local [(define pth-timeout 
                                            (or (path-timeout pth)
                                                (current-subprocess-timeout-seconds)))
                                          (define pth-cmd/general (path-command-line pth))
                                          (define pth-cmd
                                            (match pth-cmd/general
                                              [#f
                                               #f]
                                              [(list-rest "mzscheme" rst)
                                               (lambda () (list* mzscheme-path rst))]
                                              [(list-rest "mzc" rst)
                                               (lambda () (list* mzc-path rst))]
                                              [(list-rest "mred-text" rst)
                                               (lambda () (list* mred-text-path "-display" (format ":~a" (+ XSERVER-OFFSET (current-worker))) rst))]
                                              [(list-rest "mred" rst)
                                               (lambda () (list* mred-path "-display" (format ":~a" (+ XSERVER-OFFSET (current-worker))) rst))]
                                              [_
                                               #f]))]       
                                    (if pth-cmd
                                        (submit-job!
                                         test-workers
                                         (lambda ()
                                           (define l (pth-cmd))
                                           (with-env (["DISPLAY" (format ":~a" (+ XSERVER-OFFSET (current-worker)))]
                                                      ["HOME" (home-dir (current-worker))])
                                             ; XXX Maybe this should destroy the old home and copy in a new one
                                             ;     Otherwise it is a source of randomness
                                             (with-temporary-directory
                                                 (run/collect/wait/log log-pth 
                                                                       #:timeout pth-timeout
                                                                       #:env (current-env)
                                                                       (first l)
                                                                       (rest l))))
                                           (semaphore-post dir-sema)))
                                        (semaphore-post dir-sema)))))))
                      files)
            (thread
             (lambda ()
               (semaphore-wait* dir-sema how-many)
               (notify! "Done with dir: ~a" dir-pth)
               (write-cache! dir-log (current-seconds))
               (semaphore-post upper-sema)))))))
  ; Some setup
  (for ([pp (in-list (planet-packages))])
    (match pp
      [`(,auth ,pkg ,majn ,minn ,ver)
       (define maj (number->string majn))
       (define min (number->string minn))
       (run/collect/wait/log 
        ; XXX Give it its own timeout
        #:timeout (current-make-install-timeout-seconds)
        #:env (current-env)
        (build-path log-dir "planet" auth pkg maj min)
        planet-path 
        (list "install" auth pkg maj min))]))
  (run/collect/wait/log 
   #:timeout (current-subprocess-timeout-seconds)
   #:env (current-env)
   (build-path log-dir "src" "build" "set-browser.ss")
   mzscheme-path 
   (list "-t" (path->string* (build-path (drdr-directory) "set-browser.ss"))))
  ; Make home directories
  (cache/file/timestamp
   (build-path rev-dir "homedir-dup")
   (lambda ()
     (notify! "Copying home directory for each worker")
     (for ([i (in-range (number-of-cpus))])
       (with-handlers ([exn:fail? void])
         (copy-directory/files (hash-ref (current-env) "HOME") (home-dir i))))))
  ; And go
  (notify! "Starting testing")
  (test-directory collects-pth top-sema)
  (notify! "All testing scheduled... waiting for completion")
  (semaphore-wait top-sema)
  (notify! "Stopping testing")
  (stop-job-queue! test-workers))

(define (home-dir i)
  (format "~a~a"
          (hash-ref (current-env) "HOME")
          i))

(define (recur-many i r f)
  (if (zero? i)
      (f)
      (r (sub1 i) (lambda ()
                    (recur-many (sub1 i) r f)))))

(define XSERVER-OFFSET 20)

(define (integrate-revision rev)
  (define test-dir
    (build-path (revision-dir rev) "test"))
  (define planet-dir
    (build-path test-dir "planet"))
  (define home-dir
    (build-path test-dir "home"))
  (define tmp-dir
    (build-path test-dir "tmp"))
  (define trunk-dir
    (revision-trunk-dir rev))
  (cache/file/timestamp
   (build-path (revision-dir rev) "integrated")
   (lambda ()
     (make-directory* test-dir)
     (make-directory* planet-dir)
     (make-directory* home-dir)
     (make-directory* tmp-dir)
     ; We are running inside of a test directory so that random files are stored there
     (parameterize ([current-directory test-dir]
                    [current-rev rev])
       (with-env (["PLTSTDERR" "error"]
                  ["TMPDIR" (path->string tmp-dir)]
                  ["PATH" 
                   (format "~a:~a"
                           (path->string (build-path trunk-dir "bin"))
                           (getenv "PATH"))]
                  ["PLTPLANETDIR" (path->string planet-dir)]
                  ["HOME" (path->string home-dir)])
         (unless (read-cache* (revision-commit-msg rev))
           (write-cache! (revision-commit-msg rev)
                         (svn-revision-log rev (plt-repository))))
         (build-revision rev)
         (recur-many (number-of-cpus)
                     (lambda (j inner)
                       (define i (+ j XSERVER-OFFSET))
                       (notify! "Starting X server #~a" i)
                       (safely-delete-directory (format "/tmp/.X~a-lock" i))
                       (safely-delete-directory (build-path tmp-dir (format ".X~a-lock" i)))
                       (safely-delete-directory (format "/tmp/.tX~a-lock" i))
                       (safely-delete-directory (build-path tmp-dir (format ".tX~a-lock" i)))
                       (with-running-program
                           (Xvfb-path) (list (format ":~a" i) "-screen" "0" "800x600x24" "-ac" "-br" "-bs" "-kb")
                         (lambda ()
                           (with-running-program
                               (fluxbox-path) (list "-display" (format ":~a" i) "-rc" "/home/jay/.fluxbox/init")
                             inner))))
                     (lambda ()
                       (test-revision rev)))))
     ; Remove the test directory
     (safely-delete-directory test-dir))))

(provide/contract
 [integrate-revision (exact-nonnegative-integer? . -> . void)])