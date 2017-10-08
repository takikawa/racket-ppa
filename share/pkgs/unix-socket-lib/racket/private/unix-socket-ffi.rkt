;; FFI functions, constants, and types for unix domain sockets (unsafe)
#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (protect-out (all-defined-out)))

;; platform : (U 'bsd 'linux #f)
;; Data structures and constants differ between platforms.
;; Mac OS X and the BSDs I tried seem to have consistent definitions.
(define platform
  (case (system-type 'os)
    [(macosx) 'bsd]
    [(unix)
     (define machine
       ;; security guard may prevent executing uname
       (with-handlers ([exn:fail? (lambda (e) "unknown")])
         (system-type 'machine)))
     (cond [(regexp-match? #rx"^Linux" machine) 'linux]
           [(regexp-match? #rx"^[a-zA-Z]*BSD" machine) 'bsd]
           [else #f])]
    [else #f]))

(define unix-socket-available?
  (and platform #t))

;; ========================================
;; Constants

;; linux: bits/socket.h; bsd/macosx: sys/socket.h
(define AF-UNIX 1)
(define SOCK-STREAM 1)

;; linux: sys/socket.h; bsd/macosx: sys/socket.h
(define SHUT_RD 0)
(define SHUT_WR 1)

;; linux: asm-generic/{errno-base,errno}.h; bsd/macosx: sys/errno.h
(define EINTR           4)
(define EAGAIN          (case platform [(linux) 11]  [(bsd) 35]))
(define EWOULDBLOCK     EAGAIN)
(define EINPROGRESS     (case platform [(linux) 115] [(bsd) 36]))
(define ENOTCONN        (case platform [(linux) 107] [(bsd) 57]))

;; linux: asm-generic/fcntl.h; bsd/macosx: sys/fcntl.h
(define F_SETFL         4)
(define O_NONBLOCK      (case platform [(linux) #o4000] [(bsd) 4]))

;; linux: asm-generic/socket.h; bsd/macosx: sys/socket.h
(define SOL_SOCKET      (case platform [(linux) 1] [(bsd) #xFFFF]))
(define SO_ERROR        (case platform [(linux) 4] [(bsd) #x1007]))

;; linux: sys/un.h; bsd/macosx: sys/un.h
(define UNIX-PATH-MAX   (case platform [(linux) 108] [else 104]))

;; linux: bits/sockaddr.h; bsd/macosx: sys/un.h
(define _sa_family (case platform [(linux) _ushort] [else _ubyte]))

;; linux: bits/types.h; bsd/macosx: i386/_types.h
(define _socklen_t _uint32)

(define-cstruct _linux_sockaddr_un
  ([sun_family _sa_family]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define-cstruct _bsd_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _sa_family]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define _sockaddr_un-pointer
  (case platform
    [(linux)  _linux_sockaddr_un-pointer]
    [(bsd) _bsd_sockaddr_un-pointer]
    [else     _pointer]))

(define (make-sockaddr path-bytes)
  (case platform
    [(linux)
     (make-linux_sockaddr_un AF-UNIX path-bytes)]
    [(bsd)
     (make-bsd_sockaddr_un (bytes-length path-bytes) AF-UNIX path-bytes)]))

;; ========================================
;; System functions

(define-ffi-definer define-libc (ffi-lib #f)
  #:default-make-fail make-not-available)

(define-libc socket
  (_fun #:save-errno 'posix
        _int _int _int -> _int))

(define-libc connect
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int -> _int))

(define-libc bind
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int -> _int))

(define-libc listen
  (_fun #:save-errno 'posix
        _int _int -> _int))

(define-libc accept
  (_fun #:save-errno 'posix
        _int (_pointer = #f) (_pointer = #f)
        -> _int))

(define-libc close
  (_fun #:save-errno 'posix
        _int -> _int))

(define-libc shutdown
  (_fun #:save-errno 'posix
        _int _int -> _int))

(define-libc fcntl
  (_fun #:save-errno 'posix
        _int _int _int -> _int))

(define-libc getsockopt
  (_fun #:save-errno 'posix
        _int _int _int (value : (_ptr io _int) = 0) (len : (_ptr io _uint32) = (ctype-sizeof _int))
        -> (result : _int)
        -> (cond [(zero? result)
                   value]
                  [else
                   (error 'getsockopt "error~a" (errno-error-lines (saved-errno)))])))

(define strerror-name
  (case platform
    [(linux) "__xpg_strerror_r"]
    [else    "strerror_r"]))

(define strerror_r
  (get-ffi-obj strerror-name #f
               (_fun (errno) ::
                     (errno : _int)
                     (buf : _bytes = (make-bytes 1000))
                     (buf-len : _size = (bytes-length buf))
                     -> _void
                     -> (cast buf _bytes _string/locale))
               (lambda ()
                 (lambda (errno) #f))))

(define (errno-error-lines errno)
  (define err (strerror_r errno))
  (format "\n  errno: ~a~a" errno (if err (format "\n  error: ~a" err) "")))


;; ========================================
;; Racket constants and functions

(define MZFD_CREATE_READ 1)
(define MZFD_CREATE_WRITE 2)
(define MZFD_REMOVE 5)

(define-libc scheme_make_fd_output_port
  (_fun _int _racket _bool _bool _bool -> _racket))

(define-libc scheme_socket_to_ports
  (_fun _intptr _string _bool (in : (_ptr o _racket)) (out : (_ptr o _racket))
        -> _void
        -> (values in out)))

(define-libc scheme_fd_to_semaphore
  (_fun _intptr _int _bool -> _racket))

;; ============================================================
;; Testing

;; The unix socket code is difficult to test completely, because there
;; are errors/conditions that the kernel may return that are
;; infeasible to deliberately provoke. So optionally replace certain
;; system calls here with mock versions just for testing.

;; An alternative would be to use units; that would allow testing with
;; the mocked system calls without editing the source, but I don't
;; want the overhead of units :/

(when #f
  ;; -- mock for connect returning EINPROGRESS
  (let ([real-connect connect]
        [real-fd_to_sema scheme_fd_to_semaphore])
    ;; connecting-fds : hash[nat => #t]
    (define connecting-fds (make-hash))
    (set! connect
          (lambda (s addr len)
            (define r (real-connect s addr len))
            (cond [(zero? r)
                   (hash-set! connecting-fds s #t)
                   (saved-errno EINPROGRESS)
                   (eprintf "** mock connect: setting EINPROGRESS\n")
                   -1]
                  [else r])))
    (set! scheme_fd_to_semaphore
          (lambda (fd kind reg?)
            (cond [(and (= kind MZFD_CREATE_WRITE)
                        (hash-ref connecting-fds fd #f))
                   (define sema (make-semaphore))
                   (eprintf "** mock fd_to_sema: creating semaphore\n")
                   (thread (lambda ()
                             (sleep 1)
                             (eprintf "** mock fd_to_sema: posting to semaphore\n")
                             (semaphore-post sema)))
                   (hash-remove! connecting-fds fd)
                   sema]
                  [else
                   (real-fd_to_sema fd kind reg?)])))))

(when #f
  ;; - mock for accept returning EWOULDBLOCK/EAGAIN
  (let ([real-accept accept]
        [real-fd_to_sema scheme_fd_to_semaphore])
    ;; accepting-fds : hash[nat => #t]
    (define accepting-fds (make-hash))
    (set! accept
          (lambda (s)
            (cond [(hash-ref accepting-fds s #f)
                   (hash-remove! accepting-fds s)
                   (real-accept s)]
                  [else
                   (eprintf "** mock accept: setting EWOULDBLOCK\n")
                   (hash-set! accepting-fds s #t)
                   (saved-errno EWOULDBLOCK)
                   -1])))
    (set! scheme_fd_to_semaphore
          (lambda (fd kind reg?)
            (cond [(and (= kind MZFD_CREATE_READ)
                        (hash-ref accepting-fds fd #f))
                   (define sema (make-semaphore))
                   (eprintf "** mock fd_to_sema: creating semaphore\n")
                   (thread (lambda ()
                             (sleep 1)
                             (eprintf "** mock fd_to_sema: posting to semaphore\n")
                             (semaphore-post sema)))
                   sema]
                  [else
                   (real-fd_to_sema fd kind reg?)])))))
