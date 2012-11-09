#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base))

(provide libcrypto
         libcrypto-load-fail-reason)

(define libcrypto-load-fail-reason #f)

;; We need to declare because they might be distributed with PLT Scheme
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libcrypto-so
  (case (system-type)
    [(windows) '(so "libeay32")]
    [else '(so "libcrypto")]))

(define libcrypto
  (with-handlers ([exn:fail? (lambda (x)
                               (set! libcrypto-load-fail-reason (exn-message x))
                               #f)])
    (ffi-lib libcrypto-so '("" "1.0.0" "1.0" "0.9.8b" "0.9.8" "0.9.7")
             ;; On OpenBSD, libssl is linked in a way that requires libcrypto 
             ;; to be opened as global:
             #:global? (member (path->bytes (system-library-subpath #f))
                               '(#"i386-openbsd"
                                 #"x86_64-openbsd")))))
