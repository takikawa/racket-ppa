;; Support for UNIX domain sockets.
#lang racket/base
(require racket/contract
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/atomic
         ffi/unsafe/custodian
         ffi/unsafe/define
         ffi/file
         "private/unix-socket-ffi.rkt")
(provide unix-socket-available?
         unix-socket-listener?
         unix-socket-path?
         (contract-out
          [unix-socket-connect
           (-> unix-socket-path? (values input-port? output-port?))]
          [unix-socket-listen
           (->* [unix-socket-path?] [exact-nonnegative-integer?]
                unix-socket-listener?)]
          [unix-socket-close-listener
           (-> unix-socket-listener? any)]
          [unix-socket-accept
           (-> unix-socket-listener? (values input-port? output-port?))]))

(define (unix-socket-path? v)
  (and (unix-socket-path->bytes v) #t))

(define (unix-socket-path->bytes path)
  (if (path-string? path)
      ;; On all platforms, normal path of up to UNIX-PATH-MAX bytes after
      ;; conversion to absolute is considered valid and shall be accepted.
      (let ([bstr (path->bytes (cleanse-path (path->complete-path path)))])
        (and (<= (bytes-length bstr) UNIX-PATH-MAX) bstr))

      ;; On Linux, paths may be in so-called abstract namespace where they
      ;; start with #\nul and do not have a corresponding socket file.
      ;; We accept such paths only as byte strings because we don't know
      ;; the correct encoding.
      (and (eq? platform 'linux)
           (bytes? path)
           (> (bytes-length path) 0)
           (<= (bytes-length path) UNIX-PATH-MAX)
           (= (bytes-ref path 0) 0)
           path)))

(define (check-available who)
  (unless unix-socket-available?
    (error who "unix domain sockets are not supported on this platform")))

;; do-make-sockaddr : Symbol Path/String -> (values Sockaddr-Pointer Nat)
(define (do-make-sockaddr who path)
  (when (path-string? path)
    (security-guard-check-file who path '(read write)))
  (define path-bytes (unix-socket-path->bytes path))
  (define sockaddr   (make-sockaddr path-bytes))
  (define addrlen    (+ (ctype-sizeof _ushort) (bytes-length path-bytes)))
  (values sockaddr addrlen))

;; do-make-socket : Symbol -> (values FD Cust-Reg)
;; Creates nonblocking socket, registers w/ custodian (returning registration).
;; Should be called in atomic mode.
(define (do-make-socket who)
  (define socket-fd  (socket AF-UNIX SOCK-STREAM 0))
  (unless (positive? socket-fd)
    (error who "failed to create socket~a"
           (errno-error-lines (saved-errno))))
  (set-fd-nonblocking who socket-fd)
  (values socket-fd (register-custodian-shutdown socket-fd close/unregister)))

;; set-fd-nonblocking : Symbol Nat -> Void
(define (set-fd-nonblocking who fd)
  (unless (zero? (fcntl fd F_SETFL O_NONBLOCK))
    (close fd)
    (error who "failed to set non-blocking mode~a"
           (errno-error-lines (saved-errno)))))

;; close/unregister : Nat Cust-Reg/#f -> Void
(define (close/unregister fd [reg #f])
  (close fd)
  (scheme_fd_to_semaphore fd MZFD_REMOVE #t)
  (when reg (unregister-custodian-shutdown fd reg)))

;; make-socket-ports : Symbol FD Cust-Reg/#f -> (values Input-Port Output-Port)
(define (make-socket-ports who socket-fd reg)
  (with-handlers ([(lambda (e) #t)
                   (lambda (exn)
                     (close/unregister socket-fd reg)
                     (raise exn))])
    (define-values (in out) (scheme_make_fd_output_port socket-fd 'unix-socket #f #f #t))
    ;; closing the ports closes socket-fd, so custodian no longer needs to manage directly
    (when reg (unregister-custodian-shutdown socket-fd reg))
    (define fd+ports (list socket-fd in out))
    (values (wrap-input-port in fd+ports) (wrap-output-port out fd+ports))))

;; wrap-output-port : Output-Port (List FD Port Port) -> Output-Port
;; Wrap port, override close to shutdown write side of socket.
(define (wrap-output-port out fd+ports)
  (define (close)
    (when out (close-output-port out)) ;; may block, so avoid in custodian shutdown
    (call-as-atomic
     (lambda ()
       (when creg (unregister-custodian-shutdown out* creg) (set! creg #f))
       (when fd+ports (do-shutdown fd+ports #t) (set! fd+ports #f)))))
  (define (get-write-evt buf start end) (write-bytes-avail-evt buf out start end))
  (define buffer-mode (make-buffer-mode-fun out))
  (define out*
    (make-output-port 'unix-socket out out close #f get-write-evt #f #f void 1 buffer-mode))
  (define creg (register-custodian-shutdown out* (lambda (p) (set! out #f) (close-output-port p))))
  out*)

;; wrap-input-port : Input-Port (List FD Port Port) -> Input-Port
(define (wrap-input-port in fd+ports)
  (define (close)
    (when in (close-input-port in))
    (call-as-atomic
     (lambda ()
       (when creg (unregister-custodian-shutdown in* creg) (set! creg #f))
       (when fd+ports (do-shutdown fd+ports #f) (set! fd+ports #f)))))
  (define (get-progress-evt) (port-progress-evt in))
  (define (commit k progress done) (port-commit-peeked k progress done in))
  (define buffer-mode (make-buffer-mode-fun in))
  (define in*
    (make-input-port 'unix-socket in in close get-progress-evt commit #f void 1 buffer-mode))
  (define creg (register-custodian-shutdown in* (lambda (p) (set! in #f) (close-input-port p))))
  in*)

(define (make-buffer-mode-fun port)
  (case-lambda [() (file-stream-buffer-mode port)]
          [(mode) (file-stream-buffer-mode port mode)]))

;; do-shutdown : (List FD Port Port) Boolean -> Void
;; Requirements:
;; - want to shutdown RD/WR when corresponding port closed
;; - want to shutdown *after* port closed to avoid low-level errors
;; - must *not* call shutdown after *both* ports closed (fd is stale)
;; So: okay to call shutdown if either of the ports is still open.
(define (do-shutdown fd+ports output?)
  (define socket-fd (car fd+ports))
  (define ports (cdr fd+ports))
  (unless (andmap port-closed? ports)
    (unless (zero? (shutdown socket-fd (if output? SHUT_WR SHUT_RD)))
      ;; ENOTCONN is okay; the other side may have disconnected.
      (unless (= (saved-errno) ENOTCONN)
        (error (if output? 'close-output-port/unix-socket 'close-input-port/unix-socket)
               "error from shutdown~a" (errno-error-lines (saved-errno)))))))

;; ============================================================
;; Connect

;; unix-socket-connect : Path/String -> (values Input-Port Output-Port)
(define (unix-socket-connect path)
  (check-available 'unix-socket-connect)
  (define-values (sockaddr addrlen) (do-make-sockaddr 'unix-socket-connect path))
  (define connect-k
    ;; Non-blocking connect may succeed immediately or require waiting to see.
    ;; - If succeeds immediately, make ports in same atomic block
    ;; - If wait, must exit atomic mode to sync
    ;; So we return a procedure to be applied in non-atomic mode that does 
    ;; whatever needs doing.
    (call-as-atomic
     (lambda ()
       (define-values (socket-fd reg) (do-make-socket 'unix-socket-connect))
       (define r (connect socket-fd sockaddr addrlen))
       (define errno (saved-errno))
       (cond [(= r 0) ;; connected
              (define-values (in out) (make-socket-ports 'unix-socket-connect socket-fd reg))
              (lambda () (values in out))]
             [(= errno EINPROGRESS) ;; wait and see
              (define sema (scheme_fd_to_semaphore socket-fd MZFD_CREATE_WRITE #t))
              (lambda () ;; called in non-atomic mode!
                (sync sema)
                ;; FIXME: check custodian hasn't been shut down?
                (call-as-atomic
                 (lambda ()
                   (define errno (getsockopt socket-fd SOL_SOCKET SO_ERROR))
                   (cond [(= errno 0)
                          (make-socket-ports 'unix-socket-connect socket-fd reg)]
                         [else
                          (close/unregister socket-fd reg)
                          (error 'unix-socket-connect
                                 "failed to connect socket (non-blocking)\n  path: ~e~a"
                                 path (errno-error-lines errno))]))))]
             [else
              (close/unregister socket-fd reg)
              (error 'unix-socket-connect "failed to connect socket\n  path: ~e~a"
                     path (errno-error-lines (saved-errno)))]))))
  (connect-k))


;; ============================================================
;; Listen & Accept

;; A Listener is (unix-socket-listener Nat/#f (U #f (Custodian-Boxof Cust-Reg/#f)) Semaphore)
;; Invariant: fd is #f <=> reg-box is #f <=> sema is ready
;; If fd is #f or reg-box contains #f, then listener is considered closed.
(struct unix-socket-listener (fd reg-box sema)
  #:mutable
  #:property prop:evt (lambda (self)
                        ;; ready when fd is readable OR if custodian is closed OR listener is closed
                        (call-as-atomic
                         (lambda ()
                           (define fd (unix-socket-listener-fd self))
                           (define reg-box (unix-socket-listener-reg-box self))
                           (define sema (unix-socket-listener-sema self))
                           (wrap-evt
                            (choice-evt (if fd (scheme_fd_to_semaphore fd MZFD_CREATE_READ #t) never-evt)
                                        (or reg-box never-evt)
                                        (semaphore-peek-evt sema))
                            (lambda (evt) self))))))

;; unix-socket-listen : Path/String [Nat] -> Unix-Socket-Listener
(define (unix-socket-listen path [backlog 4])
  (check-available 'unix-socket-listen)
  (define-values (sockaddr addrlen) (do-make-sockaddr 'unix-socket-listen path))
  (call-as-atomic
   (lambda ()
     (define-values (socket-fd reg) (do-make-socket 'unix-socket-listen))
     (unless (zero? (bind socket-fd sockaddr addrlen))
       (close/unregister socket-fd reg)
       (error 'unix-socket-listen "failed to bind socket\n  path: ~e~a"
              path (errno-error-lines (saved-errno))))
     (unless (zero? (listen socket-fd backlog))
       (close/unregister socket-fd reg)
       (error 'unix-socket-listen "failed to listen\n  path: ~e~a"
              path (errno-error-lines (saved-errno))))
     (unix-socket-listener socket-fd
                           (make-custodian-box (current-custodian) reg)
                           (make-semaphore 0)))))

;; unix-socket-close-listener : Listener -> Void
(define (unix-socket-close-listener l)
  (call-as-atomic
   (lambda ()
     (define fd (unix-socket-listener-fd l))
     (define sema (unix-socket-listener-sema l))
     (define reg-box (unix-socket-listener-reg-box l))
     (define reg (and reg-box (custodian-box-value reg-box)))
     (when fd
       (set-unix-socket-listener-fd! l #f)
       (set-unix-socket-listener-reg-box! l #f)
       (close/unregister fd reg)
       (semaphore-post sema))
     (void))))

;; unix-socket-accept : Unix-Socket-Listener -> (values Input-Port Output-Port)
(define (unix-socket-accept l)
  (sync l)
  (define accept-k
    (call-as-atomic
     (lambda ()
       (define lfd (listener-fd/check-open 'unix-socket-accept l))
       (define fd (accept lfd))
       (cond [(< fd 0)
              (let ([errno (saved-errno)])
                (cond [(or (= errno EAGAIN) (= errno EWOULDBLOCK) (= errno EINTR))
                       (lambda () ;; called in non-atomic mode
                         (unix-socket-accept l))]
                      [else
                       (error 'unix-socket-accept "failed to accept socket~a"
                              (errno-error-lines errno))]))]
             [else
              ;; (set-fd-nonblocking 'unix-socket-accept fd) ;; Needed?
              (define-values (in out) (make-socket-ports 'unix-socket-accept fd #f))
              (lambda () (values in out))]))))
  (accept-k))

(define (listener-fd/check-open who l)
  (define fd (unix-socket-listener-fd l))
  (define reg-box (unix-socket-listener-reg-box l))
  (unless (and fd reg-box (custodian-box-value reg-box))
    (error who "unix socket listener is closed"))
  fd)
