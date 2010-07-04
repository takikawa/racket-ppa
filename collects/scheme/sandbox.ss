#lang scheme/base

(require scheme/port
         scheme/list
         scheme/string
         syntax/moddep
         scheme/gui/dynamic)

(provide gui?
         sandbox-init-hook
         sandbox-reader
         sandbox-input
         sandbox-output
         sandbox-error-output
         sandbox-propagate-breaks
         sandbox-coverage-enabled
         sandbox-namespace-specs
         sandbox-override-collection-paths
         sandbox-path-permissions
         sandbox-security-guard
         sandbox-network-guard
         sandbox-exit-handler
         sandbox-make-inspector
         sandbox-make-code-inspector
         sandbox-make-logger
         sandbox-memory-limit
         sandbox-eval-limits
         sandbox-eval-handlers
         call-with-trusted-sandbox-configuration
         evaluator-alive?
         kill-evaluator
         break-evaluator
         set-eval-limits
         set-eval-handler
         put-input
         get-output
         get-error-output
         get-uncovered-expressions
         call-in-sandbox-context
         make-evaluator
         make-module-evaluator
         call-in-nested-thread*
         call-with-limits
         with-limits
         call-with-custodian-shutdown
         call-with-killing-threads
         exn:fail:sandbox-terminated?
         exn:fail:sandbox-terminated-reason
         exn:fail:resource?
         exn:fail:resource-resource)

(define gui? (gui-available?))

(define-syntax mz/mr ; use a value for mzscheme, or pull a mred binding
  (syntax-rules ()
    [(mz/mr mzval mrsym)
     (if gui? (gui-dynamic-require 'mrsym) mzval)]))

;; Configuration ------------------------------------------------------------

(define sandbox-init-hook    (make-parameter void))
(define sandbox-input        (make-parameter #f))
(define sandbox-output       (make-parameter #f))
(define sandbox-error-output
  (make-parameter (lambda () (dup-output-port (current-error-port)))))
(define sandbox-memory-limit (make-parameter 30))       ; 30mb total
(define sandbox-eval-limits  (make-parameter '(30 20))) ; 30sec, 20mb
(define sandbox-propagate-breaks (make-parameter #t))
(define sandbox-coverage-enabled (make-parameter #f))

(define (call-with-trusted-sandbox-configuration thunk)
  (parameterize ([sandbox-propagate-breaks    #t]
                 [sandbox-override-collection-paths '()]
                 [sandbox-security-guard      current-security-guard]
                 [sandbox-exit-handler        (exit-handler)]
                 [sandbox-make-inspector      current-inspector]
                 [sandbox-make-code-inspector current-code-inspector]
                 [sandbox-make-logger         current-logger]
                 [sandbox-memory-limit        #f]
                 [sandbox-eval-limits         #f]
                 [sandbox-eval-handlers       '(#f #f)])
    (thunk)))

(define sandbox-namespace-specs
  (make-parameter `(,(mz/mr make-base-namespace make-gui-namespace)
                    #| no modules here by default |#)))

(define (default-sandbox-reader source)
  (parameterize ([read-accept-reader #t])
    (let loop ([l '()])
      (let ([expr (read-syntax source)])
        (if (eof-object? expr)
          (reverse l)
          (loop (cons expr l)))))))

(define sandbox-reader (make-parameter default-sandbox-reader))

(define sandbox-override-collection-paths (make-parameter '()))

(define teaching-langs
  '(beginner beginner-abbr intermediate intermediate-lambda advanced))

;; Security Guard -----------------------------------------------------------

(define sep (bytes-ref (path->bytes (simplify-path "/")) 0)) ; '\' on windows

(define (simplify-path* path)
  (if (symbol? path)
      #f
      (simplify-path (cleanse-path (path->complete-path
                                    (cond [(bytes? path) (bytes->path path)]
                                          [(string? path) (string->path path)]
                                          [else path]))))))

;; 'read-bytecode is special, it's higher than 'read, but not lower than
;; 'delete.
(define permission-order '(execute write delete read-bytecode read exists))
(define (perm<=? p1 p2)
  (or (eq? p1 p2)
      (and (not (eq? 'read-bytecode p1))
           (memq p1 (memq p2 permission-order))
           #t)))

;; gets a path (can be bytes/string), returns a regexp for that path that
;; matches also subdirs (if it's a directory)
(define path->bregexp
  (let* ([sep-re    (regexp-quote (bytes sep))]
         [last-sep  (byte-regexp (bytes-append sep-re #"?$"))]
         [suffix-re (bytes-append #"(?:$|" sep-re #")")])
    (lambda (path)
      (if (byte-regexp? path)
        path
        (let* ([path (path->bytes (simplify-path* path))]
               [path (regexp-quote (regexp-replace last-sep path #""))])
          (byte-regexp (bytes-append #"^" path suffix-re)))))))

(define sandbox-path-permissions
  (make-parameter '()
    (lambda (new)
      (map (lambda (perm) (list (car perm) (path->bregexp (cadr perm))))
           new))))

;; compresses the (sandbox-path-permissions) value to a "compressed" list of
;; (permission regexp ...) where each permission appears exactly once (so it's
;; quicker to test it later, no need to scan the whole permission list).
(define compressed-path-permissions
  (let ([t (make-weak-hasheq)])
    (define (compress-permissions ps)
      (map (lambda (perm)
             (let* ([ps (filter (lambda (p) (perm<=? perm (car p))) ps)]
                    [ps (remove-duplicates (map cadr ps))])
               (cons perm ps)))
           permission-order))
    (lambda ()
      (let ([ps (sandbox-path-permissions)])
        (or (hash-ref t ps #f)
            (let ([c (compress-permissions ps)]) (hash-set! t ps c) c))))))

;; similar to the security guard, only with a single mode for simplification;
;; assumes valid mode and simplified path
(define (check-sandbox-path-permissions path needed)
  (let ([bpath (path->bytes path)]
        [perms (compressed-path-permissions)])
    (ormap (lambda (rx) (regexp-match? rx bpath)) (cdr (assq needed perms)))))

(define sandbox-network-guard
  (make-parameter (lambda (what . xs)
                    (error what "network access denied: ~e" xs))))

(define (make-default-sandbox-guard)
  (let ([orig-security (current-security-guard)])
    (make-security-guard
     orig-security
     (lambda (what path modes)
       (when path
         (let ([spath (parameterize ([current-security-guard orig-security])
                        (simplify-path* path))]
               [maxperm
                ;; assumes that the modes are valid (ie, in the above list)
                (cond [(null? modes) (error 'default-sandbox-guard
                                            "got empty mode list for ~e and ~e"
                                            what path)]
                      [(null? (cdr modes)) (car modes)] ; common case
                      [else (foldl (lambda (x max) (if (perm<=? max x) x max))
                                   (car modes) (cdr modes))])])
           (unless (check-sandbox-path-permissions spath maxperm)
             (error what "`~a' access denied for ~a"
                    (string-append* (add-between (map symbol->string modes) "+"))
                    path)))))
     (lambda args (apply (sandbox-network-guard) args)))))

(define sandbox-security-guard
  (make-parameter make-default-sandbox-guard
    (lambda (x)
      (if (or (security-guard? x)
              (and (procedure? x) (procedure-arity-includes? x 0)))
        x
        (raise-type-error
         'sandbox-security-guard
         "security-guard or a security-guard translator procedure" x)))))

;; this is never really used (see where it's used in the evaluator)
(define (default-sandbox-exit-handler _) (error 'exit "sandbox exits"))

(define sandbox-exit-handler (make-parameter default-sandbox-exit-handler))

(define sandbox-make-inspector (make-parameter make-inspector))

(define sandbox-make-code-inspector (make-parameter make-inspector))

(define sandbox-make-logger (make-parameter current-logger))

(define (compute-permissions paths+require-perms)
  (let-values ([(paths require-perms) (partition path? paths+require-perms)])
    (append (map (lambda (p) `(read ,(path->bytes p))) paths)
            (module-specs->path-permissions require-perms))))

;; computes permissions that are needed for require specs (`read-bytecode' for
;; all files and "compiled" subdirs, `exists' for the base-dir)
(define (module-specs->path-permissions mods)
  (define paths (module-specs->non-lib-paths mods))
  (define bases
    (let loop ([paths paths] [bases '()])
      (if (null? paths)
          (reverse bases)
          (let-values ([(base name dir?) (split-path (car paths))])
            (let ([base (simplify-path* base)])
              (loop (cdr paths)
                    (if (member base bases) bases (cons base bases))))))))
  (append (map (lambda (p) `(read-bytecode ,p)) paths)
          (map (lambda (b) `(read-bytecode ,(build-path b "compiled"))) bases)
          (map (lambda (b) `(exists ,b)) bases)))

;; takes a module-spec list and returns all module paths that are needed
;; ==> ignores (lib ...) modules
(define (module-specs->non-lib-paths mods)
  (define (lib? x)
    (if (module-path-index? x)
      (let-values ([(m base) (module-path-index-split x)]) (lib? m))
      (or (symbol? x) (and (pair? x) (eq? 'lib (car x))))))
  ;; turns a module spec to a simple one (except for lib specs)
  (define (simple-modspec mod)
    (cond [(and (pair? mod) (eq? 'lib (car mod))) #f]
          [(module-path? mod)
           (simplify-path* (resolve-module-path mod #f))]
          [(not (and (pair? mod) (pair? (cdr mod))))
           ;; don't know what this is, leave as is
           #f]
          [(eq? 'only (car mod))
           (simple-modspec (cadr mod))]
          [(eq? 'rename (car mod))
           (simple-modspec (cadr mod))]
          [(and (eq? 'prefix (car mod)) (pair? (cddr mod)))
           (simple-modspec (caddr mod))]
          [else #f]))
  (let loop ([todo (filter-map simple-modspec mods)]
             [r '()])
    (cond
      [(null? todo) r]
      [(member (car todo) r) (loop (cdr todo) r)]
      [else
       (let ([path (car todo)])
         (loop (append (cdr todo)
                       (filter-map
                        (lambda (i)
                          (simplify-path* (resolve-module-path-index i path)))
                        (filter (lambda (i)
                                  (and (module-path-index? i) (not (lib? i))))
                                (append-map cdr (module-compiled-imports
                                                 (get-module-code path))))))
               (cons path r)))])))

;; Resources ----------------------------------------------------------------

(define-struct (exn:fail:resource exn:fail) (resource))

(define memory-accounting? (custodian-memory-accounting-available?))

;; similar to `call-in-nested-thread', but propagates killing the thread,
;; shutting down the custodian or setting parameters and thread cells;
;; optionally with thunks to call for kill/shutdown instead.
(define (call-in-nested-thread*
         thunk
         [kill     (lambda () (kill-thread (current-thread)))]
         [shutdown (lambda () (custodian-shutdown-all (current-custodian)))])
  (let* ([p #f]
         [c (make-custodian (current-custodian))]
         [b (make-custodian-box c #t)]
         [break? (break-enabled)])
    (parameterize-break #f
      (with-handlers ([(lambda (_) (not p))
                       ;; if the after thunk was not called, then this error is
                       ;; about the thread dying unnaturally, so propagate
                       ;; whatever it did
                       (lambda (_)
                         ((if (custodian-box-value b) kill shutdown)))])
        (dynamic-wind void
          (lambda ()
            (parameterize ([current-custodian c])
              (call-in-nested-thread
               (lambda ()
                 (break-enabled break?)
                 (dynamic-wind void thunk
                   ;; this should always be called unless the thread is killed
                   ;; or the custodian is shutdown, distinguish the two cases
                   ;; through the above box
                   (lambda ()
                     (set! p (current-preserved-thread-cell-values))))))))
          (lambda () (when p (current-preserved-thread-cell-values p))))))))

;; useful wrapper around the above: run thunk, return one of:
;; - (list values val ...)
;; - (list raise exn)
;; - 'kill or 'shut
(define (nested thunk)
  (call-in-nested-thread*
   (lambda ()
     (with-handlers ([void (lambda (e) (list raise e))])
       (call-with-values thunk (lambda vs (list* values vs)))))
   (lambda () 'kill) (lambda () 'shut)))

(define (call-with-limits sec mb thunk)
  ;; note that when the thread is killed after using too much memory or time,
  ;; then all thread-local changes (parameters and thread cells) are discarded
  (let ([r #f])
    ;; memory limit, set on a new custodian so if there's an out-of-memory
    ;; error, the user's custodian is still alive
    (define-values (cust cust-box)
      (if (and mb memory-accounting?)
        (let ([c (make-custodian (current-custodian))])
          (custodian-limit-memory
           c (inexact->exact (round (* mb 1024 1024))) c)
          (values c (make-custodian-box c #t)))
        (values (current-custodian) #f)))
    (define timeout? #f)
    (define r
      (parameterize ([current-custodian cust])
        (if sec
          (nested
           (lambda ()
             ;; time limit
             (when sec
               (let ([t (current-thread)])
                 (thread (lambda ()
                           (unless (sync/timeout sec t) (set! timeout? #t))
                           (kill-thread t)))))
             (thunk)))
          (nested thunk))))
    (cond [timeout? (set! r 'time)]
          [(and cust-box (not (custodian-box-value cust-box)))
           (if (memq r '(kill shut)) ; should always be 'shut
             (set! r 'memory)
             (format "cust died with: ~a" r))]) ; throw internal error below
    (case r
      [(kill) (kill-thread (current-thread))]
      [(shut) (custodian-shutdown-all (current-custodian))]
      [(memory time)
       (raise (make-exn:fail:resource (format "with-limit: out of ~a" r)
                                      (current-continuation-marks)
                                      r))]
      [else (if (pair? r)
              (apply (car r) (cdr r))
              (error 'call-with-limits "internal error in nested: ~e" r))])))

(define-syntax with-limits
  (syntax-rules ()
    [(with-limits sec mb body ...)
     (call-with-limits sec mb (lambda () body ...))]))

;; other resource utilities

(define (call-with-custodian-shutdown thunk)
  (let* ([cust (make-custodian (current-custodian))]
         [r (parameterize ([current-custodian cust]) (nested thunk))])
    (case r
      [(kill) (kill-thread (current-thread))]
      [(shut) (custodian-shutdown-all (current-custodian))]
      [else (apply (car r) (cdr r))])))

(define (call-with-killing-threads thunk)
  (let* ([cur (current-custodian)] [sub (make-custodian cur)])
    (define r (parameterize ([current-custodian sub]) (nested thunk)))
    (let kill-all ([x sub])
      (cond [(custodian? x) (for-each kill-all (custodian-managed-list x cur))]
            [(thread? x) (kill-thread x)]))
    (case r
      [(kill) (kill-thread (current-thread))]
      [(shut) (custodian-shutdown-all (current-custodian))]
      [else (apply (car r) (cdr r))])))

(define sandbox-eval-handlers
  (make-parameter (list #f call-with-custodian-shutdown)))

;; Execution ----------------------------------------------------------------

(define (literal-identifier=? x y)
  (or (free-identifier=? x y)
      (eq? (syntax-e x) (syntax-e y))))

(define-namespace-anchor anchor)

(define (make-evaluation-namespace)
  (let* ([specs   (sandbox-namespace-specs)]
         [new-ns  ((car specs))]
         [orig-ns (namespace-anchor->empty-namespace anchor)]
         [mods    (cdr specs)])
    (parameterize ([current-namespace orig-ns])
      (for ([mod (in-list mods)]) (dynamic-require mod #f)))
    (parameterize ([current-namespace new-ns])
      (for ([mod (in-list mods)]) (namespace-attach-module orig-ns mod)))
    new-ns))

(define (extract-required language requires)
  (let* ([requires (cond [(string? language) (cons language requires)]
                         [(not (pair? language)) requires]
                         [(memq (car language) '(lib file planet quote))
                          (cons language requires)]
                         [(eq? (car language) 'begin) requires]
                         [else (error 'extract-required
                                      "bad language spec: ~e" language)])])
    requires))

(define (input->port inp)
  ;; returns #f when it can't create a port
  (cond [(input-port? inp) inp]
        [(string? inp) (open-input-string inp)]
        [(bytes?  inp) (open-input-bytes inp)]
        [(path?   inp) (open-input-file inp)]
        [else #f]))

;; Gets an input spec returns a list of syntaxes.  The input can be a list of
;; sexprs/syntaxes, or a list with a single input port spec
;; (path/string/bytes) value.
(define (input->code inps source n)
  (if (null? inps)
    '()
    (let ([p (input->port (car inps))])
      (cond [(and p (null? (cdr inps)))
             (port-count-lines! p)
             (parameterize ([current-input-port p])
               (begin0 ((sandbox-reader) source)
                 ;; close a port if we opened it
                 (unless (eq? p (car inps)) (close-input-port p))))]
            [p (error 'input->code "ambiguous inputs: ~e" inps)]
            [else (let loop ([inps inps] [n n] [r '()])
                    (if (null? inps)
                      (reverse r)
                      (loop (cdr inps) (and n (add1 n))
                            ;; 1st at line#1, pos#1, 2nd at line#2, pos#2 etc
                            ;; (starting from the `n' argument)
                            (cons (datum->syntax
                                   #f (car inps)
                                   (list source n (and n 0) n (and n 1)))
                                  r))))]))))

(define ((init-hook-for-language language))
  (cond [(or (not (pair? language))
             (not (eq? 'special (car language))))
         (void)]
        [(eq? (cadr language) 'r5rs)
         (read-case-sensitive #f)
         (read-square-bracket-as-paren #f)
         (read-curly-brace-as-paren #f)
         (read-accept-infix-dot #f)]
        [(memq (cadr language) teaching-langs)
         (read-case-sensitive #t)
         (read-decimal-as-inexact #f)
         ;; needed to make the test-engine work
         (let ([orig-ns (namespace-anchor->empty-namespace anchor)])
           (parameterize ([current-namespace orig-ns])
             (dynamic-require 'scheme/class #f))
           (namespace-attach-module orig-ns 'scheme/class))]))

;; Returns a single (module ...) or (begin ...) expression (a `begin' list
;; will be evaluated one by one -- the language might not have a `begin').
;;
;; FIXME: inserting `#%require's here is bad if the language has a
;; `#%module-begin' that processes top-level forms specially.
;; A more general solution would be to create a new module that exports
;; the given language plus all of the given extra requires.
;;
;; We use `#%requre' because, unlike the `require' of scheme/base,
;; it comes from `#%kernel', so it's always present through
;; transitive requires.
(define (build-program language requires input-program)
  (let* ([body (append (if (and (pair? requires) (eq? 'begin (car requires)))
                         (cdr requires)
                         (map (lambda (r) (list #'#%require r)) requires))
                       (input->code input-program 'program 1))]
         [use-lang (lambda (lang) `(module program ,lang . ,body))])
    (cond [(decode-language language) => use-lang]
          [(module-path? language) (use-lang language)]
          [(and (list? language) (eq? 'begin (car language)))
           (append language body)]
          [else (error 'make-evaluator "bad language spec: ~e" language)])))

(define (decode-language language)
  (cond [(and (list? language)
              (= 2 (length language))
              (eq? (car language) 'special)
              (memq (cadr language) teaching-langs))
         `(lib ,(format "htdp-~a.ss" (cadr language)) "lang")]
        [(equal? language '(special r5rs))
         `(lib "lang.ss" "r5rs")]
        [else #f]))

;; Like a toplevel (eval `(begin ,@exprs)), but the language that is used may
;; not have a begin.
(define (eval* exprs)
  (call-with-continuation-prompt
   (lambda ()
     (if (null? exprs)
       (void)
       (let ([deftag (default-continuation-prompt-tag)])
         (let loop ([expr (car exprs)] [exprs (cdr exprs)])
           (if (null? exprs)
             (eval expr)
             (begin (call-with-continuation-prompt
                     (lambda () (eval expr))
                     deftag
                     (lambda (x) (abort-current-continuation deftag x)))
                    (loop (car exprs) (cdr exprs))))))))))

;; We need a powerful enough code inspector to invoke the errortrace library
;; (indirectly through private/sandbox-coverage).  But there is a small problem
;; here -- errortrace/stacktrace.ss will grab the global code inspector value
;; at the time it is invoked.  So we grab it here too, and use it to wrap the
;; code that invokes errortrace.  If errortrace/stacktrace.ss is changed to
;; grab the current inspector, then it would be better to avoid this here, and
;; pass `evaluate-program' the inspector that was in effect when the sandbox
;; was created.
(define orig-code-inspector (current-code-inspector))

(define (evaluate-program program limit-thunk uncovered!)
  (when uncovered!
    (parameterize ([current-code-inspector orig-code-inspector])
      (eval `(,#'#%require scheme/private/sandbox-coverage))))
  (let ([ns (syntax-case* program (module) literal-identifier=?
              [(module mod . body)
               (identifier? #'mod)
               (let ([mod #'mod])
                 (lambda ()
                   (eval `(,#'require (quote ,mod)))
                   (module->namespace `(quote ,(syntax-e mod)))))]
              [_else #f])])
    ;; the actual evaluation happens under the specified limits
    ((limit-thunk (lambda ()
                    (if (and (pair? program) (eq? 'begin (car program)))
                      (eval* (cdr program))
                      (eval program))
                    (when ns (set! ns (ns))))))
    (when uncovered!
      (let ([get (let ([ns (current-namespace)])
                   (lambda () (eval '(get-uncovered-expressions) ns)))])
        (uncovered! (list (get) get))))
    (when ns (current-namespace ns))))

(define current-eventspace (mz/mr (make-parameter #f) current-eventspace))
(define make-eventspace    (mz/mr void make-eventspace))
(define run-in-bg          (mz/mr thread queue-callback))
(define null-input         (open-input-bytes #""))
(define bg-run->thread
  (if gui?
    (lambda (ignored)
      ((mz/mr void eventspace-handler-thread) (current-eventspace)))
    values))

;; special message values for the evaluator procedure, also inside the user
;; context they're used for function applications.
(define-struct evaluator-message (msg args))
(define-syntax define-evaluator-messenger
  (syntax-rules ()
    ;; with extra args
    [(define-evaluator-messenger (name arg ...) msg)
     (define (name evaluator arg ...)
       (evaluator (make-evaluator-message msg (list arg ...))))]
    [(define-evaluator-messenger (name . args) msg)
     (define (name evaluator . args)
       (evaluator (make-evaluator-message msg (list* args))))]
    ;; without
    [(define-evaluator-messenger name msg)
     (define name
       (let ([evmsg (make-evaluator-message msg '())])
         (lambda (evaluator) (evaluator evmsg))))]))

(define-evaluator-messenger evaluator-alive? 'alive?)
(define-evaluator-messenger kill-evaluator 'kill)
(define-evaluator-messenger break-evaluator 'break)
(define-evaluator-messenger (set-eval-limits secs mb) 'limits)
(define-evaluator-messenger (set-eval-handler handler) 'handler)
(define-evaluator-messenger (put-input . xs) 'input)
(define-evaluator-messenger get-output 'output)
(define-evaluator-messenger get-error-output 'error-output)
(define-evaluator-messenger (get-uncovered-expressions . xs) 'uncovered)
(define (call-in-sandbox-context evaluator thunk [unrestricted? #f])
  (evaluator (make-evaluator-message (if unrestricted? 'thunk* 'thunk)
                                     (list thunk))))

(define-struct (exn:fail:sandbox-terminated exn:fail) (reason) #:transparent)
(define (make-terminated reason)
  (make-exn:fail:sandbox-terminated
   (format "evaluator: terminated (~a)" reason)
   (current-continuation-marks)
   reason))

(define (make-evaluator* init-hook allow program-maker)
  (define orig-code-inspector (current-code-inspector))
  (define orig-security-guard (current-security-guard))
  (define orig-cust     (current-custodian))
  (define memory-cust   (make-custodian orig-cust))
  (define memory-cust-box (make-custodian-box memory-cust #t))
  (define user-cust     (make-custodian memory-cust))
  (define user-cust-box (make-custodian-box user-cust #t))
  (define coverage?     (sandbox-coverage-enabled))
  (define uncovered     #f)
  (define input-ch      (make-channel))
  (define result-ch     (make-channel))
  (define busy-sema     (make-semaphore 1))
  (define input         #f)
  (define output        #f)
  (define error-output  #f)
  (define limits        (sandbox-eval-limits))
  (define eval-handler  (car (sandbox-eval-handlers))) ; 1st handler on startup
  (define user-thread   #t) ; set later to the thread
  (define user-done-evt #t) ; set in the same place
  (define terminated?   #f) ; set to an exception value when the sandbox dies
  (define breaks-originally-enabled? (break-enabled))
  (define (limit-thunk thunk)
    (let* ([sec (and limits (car limits))]
           [mb  (and limits (cadr limits))]
           [thunk (if (or sec mb)
                    (lambda () (call-with-limits sec mb thunk))
                    thunk)]
           [thunk (if eval-handler (lambda () (eval-handler thunk)) thunk)])
      thunk))
  (define (terminated! reason)
    (unless terminated?
      (set! terminated?
            (make-terminated
             (cond
               ;; #f is used as an indication of an internal error, when we
               ;; don't know why the sandbox is killed
               [(not reason) "internal error: no termination reason"]
               ;; explicit reason given
               [(not (eq? reason #t)) reason]
               ;; reason = #t => guess the reason
               [(not (custodian-box-value memory-cust-box)) 'out-of-memory]
               [(not (custodian-box-value user-cust-box)) 'custodian-shutdown]
               [(thread-dead? user-thread) 'thread-killed]
               [else "internal error: cannot guess termination reason"])))))
  (define (user-kill)
    (when user-thread
      (let ([t user-thread])
        (set! user-thread #f)
        (terminated! #f)
        (custodian-shutdown-all user-cust)
        (kill-thread t))) ; just in case
    (void))
  (define (terminate+kill! reason raise?)
    (terminated! reason)
    (user-kill)
    (when raise? (raise terminated?)))
  (define (user-break)
    (when user-thread (break-thread user-thread)))
  (define (user-process)
    (let ([break-paramz (current-break-parameterization)])
      (parameterize-break
       #f ;; disable breaks during administrative work
       (with-handlers ([void (lambda (exn) (channel-put result-ch exn))])
         (call-with-break-parameterization
          break-paramz
          (lambda ()
            ;; enable breaks, maybe
            (when breaks-originally-enabled? (break-enabled #t))
            ;; first set up the environment
            (init-hook)
            ((sandbox-init-hook))
            ;; now read and evaluate the input program
            (evaluate-program
             (if (procedure? program-maker) (program-maker) program-maker)
             limit-thunk
             (and coverage? (lambda (es+get) (set! uncovered es+get))))))
         (channel-put result-ch 'ok))
       (set! eval-handler (cadr (sandbox-eval-handlers))) ; interactions handler
       ;; finally wait for interaction expressions
       (let ([n 0])
         (let loop ()
           (let ([expr (channel-get input-ch)])
             (when (eof-object? expr)
               (terminated! 'eof) (channel-put result-ch expr) (user-kill))
             (with-handlers ([void (lambda (exn)
                                     (channel-put result-ch (cons 'exn exn)))])
               (define run
                 (if (evaluator-message? expr)
                     (case (evaluator-message-msg expr)
                       [(thunk) (limit-thunk (car (evaluator-message-args expr)))]
                       [(thunk*) (car (evaluator-message-args expr))]
                       [else (error 'sandbox "internal error (bad message)")])
                     (limit-thunk
                      (lambda ()
                        (set! n (add1 n))
                        (eval* (map (lambda (expr) (cons '#%top-interaction expr))
                                    (input->code (list expr) 'eval n)))))))
               (channel-put result-ch (cons 'vals 
                                            (call-with-break-parameterization
                                             break-paramz
                                             (lambda ()
                                               (call-with-values run list))))))
             (loop)))))))
  (define (get-user-result)
    (if (and (sandbox-propagate-breaks)
             ;; The following test is weird. We reliably catch breaks if breaks
             ;; are enabled, except that a break just before or after isn't
             ;; reliably propagated. A `get-result/enable-breaks' function
             ;; would make more sense.
             (break-enabled))
        ;; The following loop ensures that breaks are disabled while trying
        ;; to handle a break, which ensures that we don't fail to
        ;; propagate a break.
        (parameterize-break
         #f
         (let loop ()
           (with-handlers* ([exn:break? (lambda (e) (user-break) (loop))])
             (sync/enable-break user-done-evt result-ch))))
        ;; The simple case doesn't have to deal with breaks:
        (sync user-done-evt result-ch)))
  (define (user-eval expr)
    ;; the thread will usually be running, but it might be killed outside of
    ;; the sandboxed environment, for example, if you do something like
    ;; (kill-thread (ev '(current-thread))) when there are no per-expression
    ;; limits (since then you get a different thread, which is already dead).
    (when (and user-thread (thread-dead? user-thread))
      (terminate+kill! #t #t))
    (cond [terminated? => raise]
          [(not user-thread)
           (error 'sandbox "internal error (user-thread is #f)")]
          ;; use a semaphore to know when we're currently in an evaluation, to
          ;; prevent the evaluator from calling itself (it will deadlock, and
          ;; there is no simple way to avoid it -- will require making a stream
          ;; of inputs sent to the user context, queueing them as they come in,
          ;; and for each one register a channel for a reply -- and this will
          ;; consume resources outside the user context)
          [(not (sync/timeout 0 busy-sema))
           (error 'evaluator "nested evaluator call with: ~e" expr)]
          [else (channel-put input-ch expr)
                (let ([r (get-user-result)])
                  (semaphore-post busy-sema)
                  (cond [(eof-object? r) (terminate+kill! #t #t)]
                        [(eq? (car r) 'exn) (raise (cdr r))]
                        [else (apply values (cdr r))]))]))
  (define get-uncovered
    (case-lambda
     [() (get-uncovered #t)]
     [(prog?) (get-uncovered prog? 'program)]
     [(prog? src)
      (unless uncovered
        (error 'get-uncovered-expressions "no coverage information"))
      (let ([uncovered (if prog? (car uncovered) ((cadr uncovered)))])
        (if src
            (filter (lambda (x) (equal? src (syntax-source x))) uncovered)
            uncovered))]))
  (define (output-getter p)
    (if (procedure? p) (user-eval (make-evaluator-message 'thunk (list p))) p))
  (define input-putter
    (case-lambda
     [() (input-putter input)]
     [(arg) (cond [(not input)
                   (error 'put-input "evaluator input is not 'pipe")]
                  [(or (string? arg) (bytes? arg))
                   (display arg input) (flush-output input)]
                  [(eof-object? arg) (close-output-port input)]
                  [else (error 'put-input "bad argument: ~e" arg)])]))
  (define (evaluator expr)
    (if (evaluator-message? expr)
      (let ([msg (evaluator-message-msg expr)])
        (case msg
          [(alive?)  (and user-thread (not (thread-dead? user-thread)))]
          [(kill)    (terminate+kill! 'evaluator-killed #f)]
          [(break)   (user-break)]
          [(limits)  (set! limits (evaluator-message-args expr))]
          [(handler) (set! eval-handler (car (evaluator-message-args expr)))]
          [(input)   (apply input-putter (evaluator-message-args expr))]
          [(output)  (output-getter output)]
          [(error-output) (output-getter error-output)]
          [(uncovered) (apply get-uncovered (evaluator-message-args expr))]
          [(thunk thunk*) (user-eval expr)]
          [else (error 'evaluator "internal error, bad message: ~e" msg)]))
      (user-eval expr)))
  (define (make-output what out set-out!)
    (cond [(not out) (open-output-nowhere)]
          [(and (procedure? out) (procedure-arity-includes? out 0)) (out)]
          [(output-port? out) out]
          [(eq? out 'pipe) (let-values ([(i o) (make-pipe)]) (set-out! i) o)]
          [(memq out '(bytes string))
           (let* ([bytes? (eq? out 'bytes)]
                  ;; create the port under the user's custodian
                  [out (parameterize ([current-custodian user-cust])
                         (call-in-nested-thread
                          ;; this doesn't really matter: they're the same anyway
                          (if bytes? open-output-bytes open-output-string)))])
             (set-out!
              (lambda ()
                ;; this will run in the user context
                (let ([buf (get-output-bytes out #t)])
                  (if bytes? buf (bytes->string/utf-8 buf #\?)))))
             out)]
          [else (error 'make-evaluator "bad sandox-~a spec: ~e" what out)]))
  ;; set global memory limit
  (when (and memory-accounting? (sandbox-memory-limit))
    (custodian-limit-memory
     memory-cust
     (inexact->exact (round (* (sandbox-memory-limit) 1024 1024)))
     memory-cust))
  (parameterize* ; the order in these matters
   (;; create a sandbox context first
    [current-custodian user-cust]
    [current-thread-group (make-thread-group)]
    [current-namespace (make-evaluation-namespace)]
    ;; set up the IO context
    [current-input-port
     (let ([inp (sandbox-input)])
       (cond
        [(not inp) null-input]
        [(input->port inp) => values]
        [(and (procedure? inp) (procedure-arity-includes? inp 0)) (inp)]
        [(eq? 'pipe inp)
         (let-values ([(i o) (make-pipe)]) (set! input o) i)]
        [else (error 'make-evaluator "bad sandbox-input: ~e" inp)]))]
    [current-output-port (make-output 'output (sandbox-output)
                                      (lambda (o) (set! output o)))]
    [current-error-port (make-output 'error-output (sandbox-error-output)
                                     (lambda (o) (set! error-output o)))]
    ;; paths
    [current-library-collection-paths
     (filter directory-exists?
             (append (sandbox-override-collection-paths)
                     (current-library-collection-paths)))]
    [sandbox-path-permissions
     `(,@(map (lambda (p) `(read-bytecode ,p))
              (current-library-collection-paths))
       (exists ,(find-system-path 'addon-dir))
       ,@(compute-permissions allow)
       ,@(sandbox-path-permissions))]
    ;; general info
    [current-command-line-arguments '#()]
    ;; restrict the sandbox context from this point
    [current-security-guard
     (let ([g (sandbox-security-guard)]) (if (security-guard? g) g (g)))]
    [current-logger ((sandbox-make-logger))]
    [current-inspector ((sandbox-make-inspector))]
    [current-code-inspector ((sandbox-make-code-inspector))]
    ;; The code inspector serves two purposes -- making sure that only trusted
    ;; byte-code is loaded, and avoiding using protected module bindings, like
    ;; the foreign library's `unsafe!'.  We control the first through the path
    ;; permissions -- using the 'read-bytecode permissionn level, so this
    ;; handler just checks for that permission then goes on to load the file
    ;; using the original inspector.
    [current-load/use-compiled
     (let ([handler (current-load/use-compiled)])
       (lambda (path modname)
         (if (check-sandbox-path-permissions
              (parameterize ([current-security-guard orig-security-guard])
                (simplify-path* path))
              'read-bytecode)
           (parameterize ([current-code-inspector orig-code-inspector])
             (handler path modname))
           ;; otherwise, just let the old handler throw a proper error
           (handler path modname))))]
    [exit-handler
     (let ([h (sandbox-exit-handler)])
       (if (eq? h default-sandbox-exit-handler)
         (lambda _ (terminate+kill! 'exited #f))
         h))]
    ;; Note the above definition of `current-eventspace': in MzScheme, it
    ;; is an unused parameter.  Also note that creating an eventspace
    ;; starts a thread that will eventually run the callback code (which
    ;; evaluates the program in `run-in-bg') -- so this parameterization
    ;; must be nested in the above (which is what paramaterize* does), or
    ;; it will not use the new namespace.
    [current-eventspace (parameterize-break
                         #f
                         (make-eventspace))])
   (let ([t (bg-run->thread (run-in-bg user-process))])
     (set! user-done-evt (handle-evt t (lambda (_) (terminate+kill! #t #t))))
     (set! user-thread t))
   (let ([r (get-user-result)])
     (if (eq? r 'ok)
       ;; initial program executed ok, so return an evaluator
       evaluator
       ;; program didn't execute
       (raise r)))))

(define (make-evaluator language
                        #:requires [requires null] #:allow-read [allow null]
                        . input-program)
  ;; `input-program' is either a single argument specifying a file/string, or
  ;; multiple arguments for a sequence of expressions
  (let (;; make it possible to provide #f for no language and no requires
        [lang language]
        ;; make it possible to use simple paths to files to require
        [reqs (if (not (list? requires))
                (error 'make-evaluator "bad requires: ~e" requires)
                (map (lambda (r)
                       (if (or (pair? r) (symbol? r))
                         r
                         `(file ,(path->string (simplify-path* r)))))
                     requires))])
    (make-evaluator* (init-hook-for-language lang)
                     (append (extract-required (or (decode-language lang) lang)
                                               reqs)
                             allow)
                     (lambda () (build-program lang reqs input-program)))))

(define (make-module-evaluator
         input-program #:allow-read [allow null] #:language [reqlang #f])
  ;; this is for a complete module input program
  (define (make-program)
    (let ([prog (input->code (list input-program) 'program #f)])
      (unless (= 1 (length prog))
        (error 'make-evaluator "expecting a single `module' program; ~a"
               (if (zero? (length prog))
                 "no program expressions given"
                 "got more than a single expression")))
      (syntax-case* (car prog) (module) literal-identifier=?
        [(module modname lang body ...)
         (if (or (not reqlang) (equal? reqlang (syntax->datum #'lang)))
           (car prog)
           (error 'make-evaluator
                  "module code used `~e' for a language, expecting `~e'"
                  (syntax->datum #'lang) reqlang))]
        [_else (error 'make-evaluator "expecting a `module' program; got ~e"
                      (syntax->datum (car prog)))])))
  (make-evaluator* void
                   (if (path? input-program) (cons input-program allow) allow)
                   make-program))
