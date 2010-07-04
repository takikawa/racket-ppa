
(load-relative "loadtest.ss")

(Section 'sandbox)

(require scheme/sandbox)

;; test call-in-nested-thread*
(let ()
  (define (kill) (kill-thread (current-thread)))
  (define (shut) (custodian-shutdown-all (current-custodian)))
  (define-syntax-rule (nested body ...)
    (call-in-nested-thread* (lambda () body ...)))
  (define-syntax-rule (nested* body ...)
    (call-in-nested-thread* (lambda () body ...)
                            (lambda () 'kill)
                            (lambda () 'shut)))
  (test 1 values (nested 1))
  ;; propagates parameters
  (let ([p (make-parameter #f)])
    (nested (p 1))
    (test 1 p)
    (with-handlers ([void void]) (nested (p 2) (error "foo") (p 3)))
    (test 2 p))
  ;; propagates kill-thread
  (test (void) thread-wait
        (thread (lambda ()
                  (nested (kill))
                  ;; never reach here
                  (semaphore-wait (make-semaphore 0)))))
  ;; propagates custodian-shutdown-all
  (test (void) values
        (parameterize ([current-custodian (make-custodian)]) (nested (shut))))
  ;; test handlers parameters
  (test 'kill (lambda () (nested* (kill))))
  (test 'shut (lambda () (nested* (shut)))))

(let ([ev void])
  (define (run thunk)
    (with-handlers ([void (lambda (e) (list 'exn: e))])
      (call-with-values thunk (lambda vs (cons 'vals: vs)))))
  (define (run* thunk)
    (with-handlers ([void (lambda (e) (list 'exn: e))])
      (call-with-values thunk
          (case-lambda [(x) (and x #t)] [vs (cons 'vals: vs)]))))
  (define (e-match? re run thunk)
    (let ([x (run thunk)])
      (if (and (list? x) (= 2 (length x)) (eq? 'exn: (car x)) (exn? (cadr x)))
        (let ([m (exn-message (cadr x))])
          (or (regexp-match? re m) (list 'bad-exception-message: m)))
        x)))
  (define-syntax thunk (syntax-rules () [(thunk b ...) (lambda () b ...)]))
  (define-syntax t
    (syntax-rules (--eval-- --top-- => <= =err> <err=)
      [(t -?-) (void)]
      [(t -?- --eval-- more ...) (t --eval-- more ...)]
      [(t -?- --top--  more ...) (t --top--  more ...)]
      [(t --eval-- E)         (test #t            run* (thunk (ev `E)))]
      [(t --top--  E)         (test #t            run* (thunk E))]
      [(t --eval-- E => R)    (test `(vals: ,R)   run  (thunk (ev `E)))]
      [(t --top--  E => R)    (test `(vals: ,R)   run  (thunk E))]
      [(t --eval-- E =err> R) (test #t e-match? R run  (thunk (ev `E)))]
      [(t --top--  E =err> R) (test #t e-match? R run  (thunk E))]
      [(t -?- E => R more ...)    (begin (t -?- E => R) (t -?- more ...))]
      [(t -?- E =err> R more ...) (begin (t -?- E =err> R) (t -?- more ...))]
      [(t -?- R <= E more ...)    (t -?- E => R more ...)]
      [(t -?- R <err= E more ...) (t E =err> R more ...)]
      ;; last so it doesn't match the above
      [(t -?- E more ...) (begin (t -?- E) (t -?- more ...))]))
  (define (make-prog . lines)
    (apply string-append (map (lambda (l) (string-append l "\n")) lines)))

  (t

   ;; basic stuff, limits
   --top--
   (set! ev (make-evaluator 'scheme/base
              (make-prog "(define x 1)"
                         "(define (id x) x)"
                         "(define (plus1 x) x)"
                         "(define (loop) (loop))"
                         "(define (memory x) (make-vector x))")))
   (set-eval-limits ev 0.5 5)
   --eval--
   x => 1
   (id 1) => 1
   (id (plus1 x)) => 1
   (define id2 id)
   (id2 (id x)) => 1
   blah =err> "before its definition"
   ;; using a string for an input
   "1" => 1
   "(+ 1 2) x (define y 9) y (set! y 99) y" => 99
   "bad\"string" =err> "expected a closing"
   "bad(string" =err> "expected a .\\)."
   "bad)string" =err> "unexpected .\\)."
   "(set! y 999) (string" =err> "expected a .\\)."
   y => 99
   "(set! y 999) (if)" =err> "if: bad syntax"
   y => 999
   ;; test limits
   (loop) =err> "out of time"
   --top--
   (when (custodian-memory-accounting-available?)
     (t --eval-- (memory 3000000) =err> "out of memory"))
   ;; test parameter settings (tricky to get this right since
   ;; with-limits runs stuff in a different thread)
   (set-eval-limits ev #f #f)
   --eval--
   (define p (make-parameter 0))
   (p) => 0
   (p 1)
   (p) => 1
   (thread-wait (thread (lambda () (p 100))))
   (p) => 1
   --top--
   (set-eval-limits ev 1 3)
   --eval--
   (p) => 1
   (p 2)
   (p) => 2
   (thread-wait (thread (lambda () (p 100))))
   (p) => 2
   --top--
   (set-eval-limits ev #f #f)
   --eval--
   (p) => 2
   ;; breaking
   --top--
   (thread (lambda () (sleep 1) (break-evaluator ev)))
   --eval--
   (sleep 2) =err> "user break"
   (printf "x = ~s\n" x) => (void)
   ;; termination
   --eval--
   ,eof =err> "terminated .eof.$"
   123  =err> "terminated .eof.$"
   ,eof =err> "terminated .eof.$"

   ;; other termination messages
   --top-- (set! ev (make-evaluator 'scheme/base)) (kill-evaluator ev)
   --eval-- 123 =err> "terminated .evaluator-killed.$"

   ;; eval-limits apply to the sandbox creation too
   --top--
   (set! ev (parameterize ([sandbox-eval-limits '(0.25 5)])
              (make-evaluator 'scheme/base '(sleep 2))))
   =err> "out of time"
   (when (custodian-memory-accounting-available?)
     (t --top--
        (set! ev (parameterize ([sandbox-eval-limits '(2 2)])
                   (make-evaluator 'scheme/base
                                   '(define a (for/list ([i (in-range 10)])
                                                (collect-garbage)
                                                (make-bytes 500000))))))
        =err> "out of memory"))

   ;; i/o
   --top--
   (set! ev (parameterize ([sandbox-input "3\n"]
                           [sandbox-output 'string]
                           [sandbox-error-output current-output-port])
              (make-evaluator 'scheme/base '(define x 123))))
   --eval-- (printf "x = ~s\n" x) => (void)
   --top--  (get-output ev) => "x = 123\n"
   --eval-- (printf "x = ~s\n" x) => (void)
   --top--  (get-output ev) => "x = 123\n"
   --eval-- (printf "x*2 = ~s\n" (+ x x)) => (void)
            (printf "x*10 = ~s\n" (* 10 x)) => (void)
   --top--  (get-output ev) => "x*2 = 246\nx*10 = 1230\n"
   --eval-- (printf "x*(read) = ~s\n" (* x (read))) => (void)
   --top--  (get-output ev) => "x*(read) = 369\n"
   --eval-- (begin (printf "a\n") (fprintf (current-error-port) "b\n"))
   --top--  (get-output ev) => "a\nb\n"
            (get-error-output ev) => #f
   --top--
   (set! ev (parameterize ([sandbox-output 'string]
                           [sandbox-error-output 'string])
              (make-evaluator 'scheme/base)))
   --eval-- (begin (printf "a\n") (fprintf (current-error-port) "b\n"))
   --top--  (get-output ev) => "a\n"
            (get-error-output ev) => "b\n"
   --top--
   (set! ev (parameterize ([sandbox-input 'pipe]
                           [sandbox-output 'bytes]
                           [sandbox-error-output current-output-port]
                           [sandbox-eval-limits '(0.25 10)])
              (make-evaluator 'scheme/base '(define x 123))))
   --eval--  (begin (printf "x = ~s\n" x)
                    (fprintf (current-error-port) "err\n"))
   --top--   (get-output ev) => #"x = 123\nerr\n"
             (put-input ev "blah\n")
             (put-input ev "blah\n")
   --eval--  (read-line) => "blah"
             (printf "line = ~s\n" (read-line))
   --top--   (get-output ev) => #"line = \"blah\"\n"
   --eval--  (read-line) =err> "out of time"
   --top--   (put-input ev "blah\n")
             (put-input ev eof)
   --eval--  (read-line) => "blah"
             (read-line) => eof
             (read-line) => eof
   ;; test kill-evaluator here
   --top--
   (kill-evaluator ev) => (void)
   --eval--
   x =err> "terminated .evaluator-killed.$"
   y =err> "terminated .evaluator-killed.$"
   ,eof =err> "terminated .evaluator-killed.$"
   --top--
   (let-values ([(i1 o1) (make-pipe)] [(i2 o2) (make-pipe)])
     ;; o1 -> i1 -ev-> o2 -> i2
     (set! ev (parameterize ([sandbox-input i1] [sandbox-output o2])
                (make-evaluator 'scheme/base '(define x 123))))
     (t --eval-- (printf "x = ~s\n" x) => (void)
        --top--  (read-line i2) => "x = 123"
        --eval-- (printf "x = ~s\n" x) => (void)
        --top--  (read-line i2) => "x = 123"
        --eval-- (printf "x*2 = ~s\n" (+ x x)) => (void)
                 (printf "x*10 = ~s\n" (* 10 x)) => (void)
        --top--  (read-line i2) => "x*2 = 246"
                 (read-line i2) => "x*10 = 1230"
                 (fprintf o1 "3\n")
        --eval-- (printf "x*(read) = ~s\n" (* x (read))) => (void)
        --top--  (read-line i2) => "x*(read) = 369"
        ))

   ;; sexprs as a program
   --top--
   (set! ev (make-evaluator 'scheme/base '(define id (lambda (x) x))))
   --eval--
   (id 123) => 123
   --top--
   (set! ev (make-evaluator 'scheme/base '(define id (lambda (x) x))
                                         '(define fooo 999)))
   --eval--
   (id fooo) => 999

   ;; test source locations too
   --top--
   (make-evaluator 'scheme/base 0 1 2 '(define foo))
     =err> "program:4:0: define"

   ;; empty program for clean repls
   --top--
   (set! ev (make-evaluator '(begin)))
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (set! ev (make-evaluator 'scheme/base))
   --eval--
   (define x (+ 1 2 3)) => (void)
   x => 6
   (define x (+ x 10)) => (void)
   x => 16
   --top--
   (set! ev (make-evaluator 'scheme/base '(define x (+ 1 2 3))))
   --eval--
   (define x (+ x 10)) =err> "cannot re-define a constant"

   ;; whole program argument
   --top--
   (set! ev (make-module-evaluator '(module foo scheme/base (define x 1))))
   --eval--
   x => 1
   --top--
   (set! ev (make-module-evaluator
             '(module foo scheme/base (provide x) (define x 1))))
   --eval--
   x => 1
   (define x 2) =err> "cannot re-define a constant"

   ;; limited FS access, allowed for requires
   --top--
   (let* ([tmp       (make-temporary-file "sandboxtest~a" 'directory)]
          [strpath   (lambda xs (path->string (apply build-path xs)))]
          [schemelib (strpath (collection-path "scheme"))]
          [list-lib  (strpath schemelib "list.ss")]
          [list-zo   (strpath schemelib "compiled" "list_ss.zo")]
          [test-lib  (strpath tmp "sandbox-test.ss")]
          [test-zo   (strpath tmp "compiled" "sandbox-test_ss.zo")]
          [test2-lib (strpath tmp "sandbox-test2.ss")]
          [test2-zo  (strpath tmp "compiled" "sandbox-test2_ss.zo")])
     (t --top--
        (set! ev (make-evaluator 'scheme/base))
        --eval--
        ;; reading from collects is allowed
        (list? (directory-list ,schemelib))
        (file-exists? ,list-lib) => #t
        (input-port? (open-input-file ,list-lib)) => #t
        ;; writing is forbidden
        (open-output-file ,list-lib) =err> "`write' access denied"
        ;; reading from other places is forbidden
        (directory-list ,tmp) =err> "`read' access denied"
        ;; no network too
        (require scheme/tcp)
        (tcp-listen 12345) =err> "network access denied"
        --top--
        ;; reading from a specified require is fine
        (with-output-to-file test-lib
          (lambda ()
            (printf "~s\n" '(module sandbox-test scheme/base
                              (define x 123) (provide x)))))
        (set! ev (make-evaluator 'scheme/base #:requires `(,test-lib)))
        --eval--
        x => 123
        (length (with-input-from-file ,test-lib read)) => 5
        ;; the directory is still not kosher
        (directory-list ,tmp) =err> "`read' access denied"
        --top--
        ;; should work also for module evaluators
        ;; --> NO!  Shouldn't make user code require whatever it wants
        ;; (set! ev (make-evaluator `(module foo scheme/base
        ;;                             (require (file ,test-lib)))))
        ;; --eval--
        ;; x => 123
        ;; (length (with-input-from-file ,test-lib read)) => 5
        ;; ;; the directory is still not kosher
        ;; (directory-list tmp) =err> "file access denied"
        --top--
        ;; explicitly allow access to tmp, and write access to a single file
        (make-directory (build-path tmp "compiled"))
        (set! ev (parameterize ([sandbox-path-permissions
                                 `((read ,tmp)
                                   (write ,test-zo)
                                   ,@(sandbox-path-permissions))])
                   (make-evaluator 'scheme/base)))
        --eval--
        (length (with-input-from-file ,test-lib read)) => 5
        (list? (directory-list ,tmp))
        (open-output-file ,(build-path tmp "blah")) =err> "access denied"
        (delete-directory ,(build-path tmp "blah")) =err> "access denied"
        (list? (directory-list ,schemelib))
        ;; we can read/write/delete list-zo, but we can't load bytecode from
        ;; it due to the code inspector
        (copy-file ,list-zo ,test-zo) => (void)
        (copy-file ,test-zo ,list-zo) =err> "access denied"
        (load/use-compiled ,test-lib) => (void)
        (require 'list) =err> "access from an uncertified context"
        (delete-file ,test-zo) => (void)
        (delete-file ,test-lib) =err> "`delete' access denied"
        --top--
        ;; a more explicit test of bytcode loading, allowing rw access to the
        ;; complete tmp directory, but read-bytecode only for test2-lib
        (set! ev (parameterize ([sandbox-path-permissions
                                 `((write ,tmp)
                                   (read-bytecode ,test2-lib)
                                   ,@(sandbox-path-permissions))])
                   (make-evaluator 'scheme/base)))
        --eval--
        (define (cp from to)
          (when (file-exists? to) (delete-file to))
          (copy-file from to))
        (cp ,list-lib ,test-lib)  (cp ,list-zo ,test-zo)
        (cp ,list-lib ,test2-lib) (cp ,list-zo   ,test2-zo)
        ;; bytecode from test-lib is bad, even when we can read/write to it
        (load/use-compiled ,test-zo)
        (require 'list) =err> "access from an uncertified context"
        ;; bytecode from test2-lib is explicitly allowed
        (load/use-compiled ,test2-lib)
        (require 'list) => (void))
     ((dynamic-require 'scheme/file 'delete-directory/files) tmp))

   ;; languages and requires
   --top--
   (set! ev (make-evaluator '(special r5rs) "(define x (eq? 'x 'X))"))
   --eval--
   x => #t
   --top--
   (set! ev (make-evaluator 'scheme/base "(define l null)"))
   --eval--
   (cond [null? l 0]) => 0
   (last-pair l) =err> "reference to an identifier"
   --top--
   (set! ev (make-evaluator '(special beginner)
                            (make-prog "(define l null)" "(define x 3.5)")))
   --eval--
   (cond [null? l 0]) =err> "expected an open parenthesis"
   --top--
   (eq? (ev "6") (ev "(sub1 (* 2 3.5))"))
   (eq? (ev "6") (ev "(sub1 (* 2 x))"))
   --top--
   (set! ev (make-evaluator 'scheme/base #:requires '(scheme/list)))
   --eval--
   (last-pair '(1 2 3)) => '(3)
   (last-pair null) =err> "expected argument of type"

   ;; coverage
   --top--
   (set! ev (parameterize ([sandbox-coverage-enabled #t])
              (make-evaluator 'scheme/base
                              (make-prog "(define (foo x) (+ x 1))"
                                         "(define (bar x) (+ x 2))"
                                         "(equal? (foo 3) 4)"))))
   (pair? (get-uncovered-expressions ev))
   (pair? (get-uncovered-expressions ev #t))
   --eval--
   (foo 3) => 4
   (bar 10) => 12
   --top--
   (null? (get-uncovered-expressions ev #f))
   (pair? (get-uncovered-expressions ev)) ; no-tests coverage still the same

   ;; misc parameters
   --top--
   (set! ev (parameterize ([sandbox-init-hook
                            (let ([old (sandbox-init-hook)])
                              (lambda ()
                                (old)
                                (compile-enforce-module-constants #f)
                                (compile-allow-set!-undefined #t)))])
              (make-evaluator 'scheme/base '(define x 123))))
   --eval--
   (set! x 456) ; would be an error without the `enforce' parameter
   x => 456
   (set! y 789) ; would be an error without the `set!' parameter
   y => 789

   ;; test that output is also collected under the limit
   --top--
   (set! ev (parameterize ([sandbox-output 'bytes]
                           [sandbox-error-output current-output-port]
                           [sandbox-memory-limit 2]
                           [sandbox-eval-limits '(0.25 1)])
              (make-evaluator 'scheme/base)))
   ;; GCing is needed to allow these to happen (note: the memory limit is very
   ;; tight here, this test usually fails if the sandbox library is not
   ;; compiled)
   (let ([t (lambda ()
              (t --eval-- (display (make-bytes 400000 65)) (collect-garbage)
                 --top--  (bytes-length (get-output ev)) => 400000))])
     ;; can go arbitrarily high here
     (for ([i (in-range 20)]) (t)))

   ;; test that killing the custodian works fine
   ;; first try it without limits (limits imply a nested thread/custodian)
   --top--
   (set! ev (parameterize ([sandbox-eval-limits #f])
              (make-evaluator 'scheme/base)))
   --eval--
   (kill-thread (current-thread)) =err> "terminated .thread-killed.$"
   --top--
   (set! ev (parameterize ([sandbox-eval-limits #f])
              (make-evaluator 'scheme/base)))
   --eval--
   (custodian-shutdown-all (current-custodian))
   =err> "terminated .custodian-shutdown.$"
   --top--
   ;; also happens when it's done directly
   (set! ev (parameterize ([sandbox-eval-limits #f])
              (make-evaluator 'scheme/base)))
   (call-in-sandbox-context ev (lambda () (kill-thread (current-thread))))
   =err> "terminated .thread-killed.$"
   (set! ev (parameterize ([sandbox-eval-limits #f])
              (make-evaluator 'scheme/base)))
   (call-in-sandbox-context ev
     (lambda () (custodian-shutdown-all (current-custodian))))
   =err> "terminated .custodian-shutdown.$"
   --top--
   ;; now make sure it works with per-expression limits too
   (set! ev (make-evaluator 'scheme/base))
   --eval--
   (kill-thread (current-thread)) =err> "terminated .thread-killed.$"
   --top--
   (set! ev (make-evaluator 'scheme/base))
   --eval--
   (custodian-shutdown-all (current-custodian))
   =err> "terminated .custodian-shutdown.$"
   --top--
   (set! ev (make-evaluator 'scheme/base))
   (call-in-sandbox-context ev (lambda () (kill-thread (current-thread))))
   =err> "terminated .thread-killed.$"
   (set! ev (make-evaluator 'scheme/base))
   (call-in-sandbox-context ev
     (lambda () (custodian-shutdown-all (current-custodian))))
   =err> "terminated .custodian-shutdown.$"

   ;; when an expression is out of memory, the sandbox should stay alive
   --top--
   (when (custodian-memory-accounting-available?)
     (t --top--
        (set! ev (parameterize ([sandbox-eval-limits '(2 5)]
                                [sandbox-memory-limit 100])
                   (make-evaluator 'scheme/base)))
        --eval--
        (define a '())
        (define b 1)
        (length
         (for/fold ([v null]) ([i (in-range 20)])
           ;; increases size of sandbox: it's reachable from it (outside of
           ;; this evaluation) because `a' is defined there
           (set! a (cons (make-bytes 500000) a))
           (collect-garbage)
           ;; increases size of the current evaluation
           (cons (make-bytes 500000) v)))
        =err> "out of memory"
        b => 1))

   ))

(report-errs)
