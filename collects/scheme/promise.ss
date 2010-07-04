(module promise '#%kernel
(#%require "private/small-scheme.ss"
           "private/more-scheme.ss"
           "private/define.ss"
           (rename "private/define-struct.ss" define-struct define-struct*)
           (for-syntax '#%kernel "private/stxcase-scheme.ss" "private/name.ss")
           '#%unsafe)
(#%provide force promise? promise-forced? promise-running?)

;; This module implements "lazy" (composable) promises and a `force'
;; that is iterated through them.

;; This is similar to the *new* version of srfi-45 -- see the
;; post-finalization discussion at http://srfi.schemers.org/srfi-45/ for
;; more details; specifically, this version is the `lazy2' version from
;; http://srfi.schemers.org/srfi-45/post-mail-archive/msg00013.html.
;; Note: if you use only `force'+`delay' it behaves as in Scheme (except
;; that `force' is identity for non promise values), and `force'+`lazy'
;; are sufficient for implementing the lazy language.

;; unsafe accessors
(define-syntax pref  (syntax-rules () [(_ p) (unsafe-struct-ref p 0)]))
(define-syntax pset! (syntax-rules () [(_ p x) (unsafe-struct-set! p 0 x)]))

;; ----------------------------------------------------------------------------
;; Forcers

;; force/composable iterates on composable promises
;; * (force X) = X for non promises
;; * does not deal with multiple values in the composable case
(define (force/composable root)
  (let ([v (pref root)])
    (cond
      [(procedure? v)
       ;; mark the root as running: avoids cycles, and no need to keep banging
       ;; the root promise value; it makes this non-r5rs, but the only
       ;; practical uses of these things could be ones that use state to avoid
       ;; an infinite loop.  (See the generic forcer below.)
       ;; (careful: avoid holding a reference to the thunk, to allow
       ;; safe-for-space loops)
       (pset! root (make-running (object-name v)))
       (call-with-exception-handler
        (lambda (e) (pset! root (make-reraise e)) e)
        (lambda ()
          ;; iterate carefully through chains of composable promises
          (let loop ([v (v)]) ; does not handle multiple values!
            (cond [(composable-promise? v)
                   (let ([v* (pref v)])
                     (pset! v root) ; share with root
                     (cond [(procedure? v*) (loop (v*))]
                           ;; it must be a list of one value (because
                           ;; composable promises never hold multiple values),
                           ;; or a composable promise
                           [(pair? v*) (pset! root v*) (unsafe-car v*)]
                           ;; note: for the promise case we could jump only to
                           ;; the last `let' (for `v*'), but that makes the
                           ;; code heavier, and runs slower (probably goes over
                           ;; some inlining/unfolding threshold).
                           [else (loop v*)]))]
                  ;; reached a non-composable promise: share and force it now
                  [(promise? v) (pset! root v) (force v)]
                  ;; error here for "library approach" (see above URL)
                  [else (pset! root (list v)) v]))))]
      ;; try to make the order efficient, with common cases first
      [(pair? v) (if (null? (unsafe-cdr v)) (unsafe-car v) (apply values v))]
      ;; follow all sharings (and shortcut directly to the right force)
      [(composable-promise? v) (force/composable v) (force v)]
      [(null? v) (values)]
      [else (error 'force "composable promise with invalid contents: ~e" v)])))

(define (reify-result v)
  (cond
    [(pair? v) (if (null? (unsafe-cdr v)) (unsafe-car v) (apply values v))]
    [(null? v) (values)]
    [(reraise? v) (v)]
    [else (error 'force "promise with invalid contents: ~e" v)]))

;; generic force for "old-style" promises -- they're still useful in
;; that they allow multiple values.  In general, this is slower, but has
;; more features.  (They could allow self loops, but this means holding
;; on to the procedure and its resources while it is running, and lose
;; the ability to know that it is running; the second can be resolved
;; with a new kind of `running' value that can be used again, but the
;; first cannot be solved.  I still didn't ever see any use for them, so
;; they're still forbidden.)
(define (force/generic promise)
  (reify-result
   (let ([v (pref promise)])
     (if (procedure? v)
       (begin
         (pset! promise (make-running (object-name v)))
         (call-with-exception-handler
          (lambda (e) (pset! promise (make-reraise e)) e)
          (lambda ()
            (let ([vs (call-with-values v list)]) (pset! promise vs) vs))))
       v))))

;; dispatcher for composable promises, generic promises, and other values
(define (force promise)
  (if (promise? promise)
    ((promise-forcer promise) promise) ; dispatch to specific forcer
    promise)) ; different from srfi-45: identity for non-promises

;; ----------------------------------------------------------------------------
;; Struct definitions

;; generic promise printer
(define (promise-printer promise port write?)
  (let loop ([v (pref promise)])
    (cond
      [(reraise? v)
       (let ([r (reraise-val v)])
         (if (exn? r)
           (fprintf port (if write? "#<promise!exn!~s>" "#<promise!exn!~a>")
                    (exn-message r))
           (fprintf port (if write? "#<promise!raise!~s>" "#<promise!raise!~a>")
                    r)))]
      [(running? v)
       (let ([r (running-name v)])
         (if r
           (fprintf port "#<promise:!running!~a>" r)
           (fprintf port "#<promise:!running>")))]
      [(procedure? v)
       (cond [(object-name v)
              => (lambda (n) (fprintf port "#<promise:~a>" n))]
             [else (display "#<promise>" port)])]
      [(promise? v) (loop (pref v))] ; hide sharing
      ;; values
      [(null? v) (fprintf port "#<promise!(values)>")]
      [(null? (cdr v))
       (fprintf port (if write? "#<promise!~s>" "#<promise!~a>") (car v))]
      [else (display "#<promise!(values" port)
            (let ([fmt (if write? " ~s" " ~a")])
              (for-each (lambda (x) (fprintf port fmt x)) v))
            (display ")>" port)])))

;; property value for the right forcer to use
(define-values [prop:force promise-forcer]
  (let-values ([(prop pred? get) ; no need for the predicate
                (make-struct-type-property 'forcer
                  (lambda (v info)
                    (unless (and (procedure? v)
                                 (procedure-arity-includes? v 1))
                      (raise-type-error 'prop:force "a unary function" v))
                    v))])
    (values prop get)))

;; A promise value can hold
;; - (list <value> ...): forced promise (possibly multiple-values)
;;        - composable promises deal with only one value
;; - <promise>: a shared (redirected) promise that points at another one
;;        - possible only with composable promises
;; - <thunk>: usually a delayed promise,
;;        - can also hold a `running' thunk that will throw a reentrant error
;;        - can also hold a raising-a-value thunk on exceptions and other
;;          `raise'd values (actually, applicable structs for printouts)
;; First, a generic struct, which is used for all promise-like values
(define-struct promise ([val #:mutable])
  #:property prop:custom-write promise-printer
  #:property prop:force force/generic)
;; Then, a subtype for composable promises
(define-struct (composable-promise promise) ()
  #:property prop:force force/composable)

;; template for all delay-like constructs
;; (with simple keyword matching: keywords is an alist with default exprs)
(define-for-syntax (make-delayer stx maker keywords)
  ;; no `cond', `and', `or', `let', `define', etc here
  (letrec-values
      ([(exprs+kwds)
        (lambda (stxs exprs kwds)
          (if (null? stxs)
            (values (reverse exprs) (reverse kwds))
            (if (not (keyword? (syntax-e (car stxs))))
              (exprs+kwds (cdr stxs) (cons (car stxs) exprs) kwds)
              (if (if (pair? (cdr stxs))
                    (if (assq (syntax-e (car stxs)) keywords)
                      (not (assq (syntax-e (car stxs)) kwds))
                      #f)
                    #f)
                (exprs+kwds (cddr stxs) exprs
                            (cons (cons (syntax-e (car stxs)) (cadr stxs))
                                  kwds))
                (values #f #f)))))]
       [(stxs) (syntax->list stx)]
       [(exprs kwds) (exprs+kwds (if stxs (cdr stxs) '()) '() '())]
       [(kwd-args) (if kwds
                     (map (lambda (k)
                            (let-values ([(x) (assq (car k) kwds)])
                              (if x (cdr x) (cdr k))))
                          keywords)
                     #f)]
       ;; some strange bug with `syntax-local-expand-expression' makes this not
       ;; work well with identifiers, so turn the name into a symbol to work
       ;; around this for now
       [(name0) (syntax-local-infer-name stx)]
       [(name) (if (syntax? name0) (syntax-e name0) name0)])
    (syntax-case stx ()
      [_ (pair? exprs) ; throw a syntax error if anything is wrong
         (with-syntax ([(expr ...) exprs]
                       [(kwd-arg ...) kwd-args])
           (with-syntax ([proc (syntax-property
                                (syntax/loc stx (lambda () expr ...))
                                'inferred-name name)]
                         [make maker])
             (syntax/loc stx (make proc kwd-arg ...))))])))

;; Creates a composable promise
;;   X = (force (lazy X)) = (force (lazy (lazy X))) = (force (lazy^n X))
(#%provide (rename lazy* lazy))
(define lazy make-composable-promise)
(define-syntax (lazy* stx) (make-delayer stx #'lazy '()))

;; Creates a (generic) promise that does not compose
;;   X = (force (delay X)) = (force (lazy (delay X)))
;;                         = (force (lazy^n (delay X)))
;;   X = (force (force (delay (delay X)))) != (force (delay (delay X)))
;; so each sequence of `(lazy^n o delay)^m' requires m `force's and a
;; sequence of `(lazy^n o delay)^m o lazy^k' requires m+1 `force's (for k>0)
;; (This is not needed with a lazy language (see the above URL for details),
;; but provided for regular delay/force uses.)
(#%provide (rename delay* delay))
(define delay make-promise)
(define-syntax (delay* stx) (make-delayer stx #'delay '()))

;; For simplicity and efficiency this code uses thunks in promise values for
;; exceptions: this way, we don't need to tag exception values in some special
;; way and test for them -- we just use a thunk that will raise the exception.
;; But it's still useful to refer to the exception value, so use an applicable
;; struct for them.  The same goes for a promise that is being forced: we use a
;; thunk that will throw a "reentrant promise" error -- and use an applicable
;; struct so it is identifiable.
(define-struct reraise (val)
  #:property prop:procedure (lambda (this) (raise (reraise-val this))))
(define-struct running (name)
  #:property prop:procedure (lambda (this)
                              (let ([name (running-name this)])
                                (if name
                                  (error 'force "reentrant promise ~e" name)
                                  (error 'force "reentrant promise")))))

;; ----------------------------------------------------------------------------
;; Utilities

(define (promise-forced? promise)
  (if (promise? promise)
    (let ([v (pref promise)])
      (or (not (procedure? v)) (reraise? v))) ; #f when running
    (raise-type-error 'promise-forced? "promise" promise)))

(define (promise-running? promise)
  (if (promise? promise)
    (running? (pref promise))
    (raise-type-error 'promise-running? "promise" promise)))

;; ----------------------------------------------------------------------------
;; More delay-like values, with different ways of deferring computations

(define-struct (promise/name promise) ()
  #:property prop:force (lambda (p) ((pref p))))

(#%provide (rename delay/name* delay/name))
(define delay/name make-promise/name)
(define-syntax (delay/name* stx) (make-delayer stx #'delay/name '()))

;; utility struct
(define-struct (running-thread running) (thread))

;; used in promise/sync until it's forced
(define-struct syncinfo ([thunk #:mutable] done-evt done-sema access-sema))

(define-struct (promise/sync promise) ()
  #:property prop:custom-write
  (lambda (p port write?)
    (promise-printer
     (let ([v (pref p)])
       (if (syncinfo? v) (make-promise (syncinfo-thunk v)) p))
     port write?))
  #:property prop:force
  (lambda (p)
    (reify-result
     (let ([v (pref p)])
       (cond
         ;; already forced
         [(not (syncinfo? v)) v]
         ;; being forced...
         [(running-thread? (syncinfo-thunk v))
          (let ([r (syncinfo-thunk v)])
            (if (eq? (running-thread-thread r) (current-thread))
              ;; ... by the current thread => throw the usual reentrant error
              (r)
              ;; ... by a different thread => just wait for it
              (begin (sync (syncinfo-done-evt v)) (pref p))))]
         [else
          ;; wasn't forced yet: try to do it now
          (call-with-semaphore (syncinfo-access-sema v)
            (lambda ()
              (let ([thunk (syncinfo-thunk v)] [done (syncinfo-done-sema v)])
                ;; set the thread last
                (set-syncinfo-thunk!
                 v (make-running-thread (object-name thunk) (current-thread)))
                (call-with-exception-handler
                 (lambda (e)
                   (pset! p (make-reraise e))
                   (semaphore-post done)
                   e)
                 (lambda ()
                   (pset! p (call-with-values thunk list))
                   (semaphore-post done))))))
          ;; whether it was this thread that forced it or not, the results are
          ;; now in
          (pref p)]))))
  #:property prop:evt
  (lambda (p)
    (let ([v (pref p)])
      (handle-evt (if (syncinfo? v) (syncinfo-done-evt v) always-evt) void))))

(#%provide (rename delay/sync* delay/sync))
(define (delay/sync thunk)
  (let ([done-sema (make-semaphore 0)])
    (make-promise/sync (make-syncinfo thunk
                                      (semaphore-peek-evt done-sema) done-sema
                                      (make-semaphore 1)))))
(define-syntax (delay/sync* stx) (make-delayer stx #'delay/sync '()))

;; threaded promises

(define-struct (promise/thread promise) ()
  #:property prop:force
  (lambda (p)
    (reify-result (let ([v (pref p)])
                    (if (running-thread? v)
                      (begin (thread-wait (running-thread-thread v))
                             (pref p))
                      v))))
  #:property prop:evt
  (lambda (p)
    (let ([v (pref p)])
      (handle-evt (if (running? v) (running-thread-thread v) always-evt)
                  void))))

(#%provide (rename delay/thread* delay/thread))
(define (delay/thread thunk group)
  (define (run)
    (call-with-exception-handler
     (lambda (e) (pset! p (make-reraise e)) (kill-thread (current-thread)))
     (lambda () (pset! p (call-with-values thunk list)))))
  (define p
    (make-promise/thread
     (make-running-thread
      (object-name thunk)
      (if group
        (parameterize ([current-thread-group (make-thread-group)]) (thread run))
        (thread run)))))
  p)
(define-syntax delay/thread*
  (let-values ([(kwds) (list (cons '#:group #'#t))])
    (lambda (stx) (make-delayer stx #'delay/thread kwds))))

(define-struct (promise/idle promise/thread) ()
  #:property prop:force
  (lambda (p)
    (reify-result (let ([v (pref p)])
                    (if (procedure? v)
                      ;; either running-thread, or returns the controller
                      (let ([controller (if (running-thread? v)
                                          (running-thread-thread v)
                                          (v))])
                        (thread-send controller 'force!)
                        (thread-wait controller)
                        (pref p))
                      v)))))

(#%provide (rename delay/idle* delay/idle))
(define (delay/idle thunk wait-for work-while tick use*)
  (define use (cond [(use* . <= . 0) 0] [(use* . >= . 1) 1] [else use*]))
  (define work-time (* tick use))
  (define rest-time (- tick work-time))
  (define (work)
    (call-with-exception-handler
     (lambda (e) (pset! p (make-reraise e)) (kill-thread (current-thread)))
     (lambda () (pset! p (call-with-values thunk list)))))
  (define (run)
    ;; this thread is dedicated to controlling the worker thread, so it's
    ;; possible to dedicate messages to signaling a `force'.
    (define force-evt (thread-receive-evt))
    (sync wait-for force-evt)
    (pset! p (make-running-thread (object-name thunk) controller-thread))
    (let ([worker (parameterize ([current-thread-group (make-thread-group)])
                    (thread work))])
      (cond
        [(and (use . >= . 1) (equal? work-while always-evt))
         ;; as if it was pre-forced
         (thread-wait worker)]
        [(use . <= . 0)
         ;; work only when explicitly forced
         (thread-suspend worker)
         (sync force-evt)
         (thread-wait worker)]
        [else
         (thread-suspend worker)
         (let loop ()
           ;; rest, then wait for idle time, then resume working
           (if (eq? (begin0 (or (sync/timeout rest-time force-evt)
                                (sync work-while force-evt))
                      (thread-resume worker))
                    force-evt)
             ;; forced during one of these => let it run to completion
             (thread-wait worker)
             ;; not forced
             (unless (sync/timeout work-time worker)
               (thread-suspend worker)
               (loop))))])))
  ;; I don't think that a thread-group here is needed, but it doesn't hurt
  (define controller-thread
    (parameterize ([current-thread-group (make-thread-group)])
      (thread run)))
  ;; the thunk is not really used in the above, make it a function that returns
  ;; the controller thread so it can be forced (used in the `prop:force')
  (define p (make-promise/idle
             (procedure-rename (lambda () controller-thread)
                               (or (object-name thunk) 'idle-thread))))
  p)
(define-syntax delay/idle*
  (let-values ([(kwds) (list (cons '#:wait-for #'(system-idle-evt))
                             (cons '#:work-while #'(system-idle-evt))
                             (cons '#:tick #'0.2)
                             (cons '#:use #'0.12))])
    (lambda (stx) (make-delayer stx #'delay/idle kwds))))

)

#|
Simple code for timings:
  (define (c n) (lazy (if (zero? n) (delay 'hey!) (c (sub1 n)))))
  (for ([i (in-range 9)])
    (collect-garbage) (collect-garbage) (collect-garbage)
    (time (for ([i (in-range 10000)]) (force (c 2000)))))
Also, run (force (c -1)) and check constant space
|#
