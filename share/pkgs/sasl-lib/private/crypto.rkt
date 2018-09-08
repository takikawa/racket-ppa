#lang racket/base
(require openssl/libcrypto
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define)
(provide (protect-out (all-defined-out)))

(define-ffi-definer define-libcrypto libcrypto
  #:default-make-fail make-not-available)

(define-cpointer-type _EVP_MD)
(define-cpointer-type _EVP_MD_CTX)

(define-libcrypto EVP_MD_size (_fun _EVP_MD -> _int))
(define-libcrypto EVP_MD_block_size (_fun _EVP_MD -> _int))

(define-libcrypto EVP_md5    (_fun -> _EVP_MD))
(define-libcrypto EVP_sha1   (_fun -> _EVP_MD))
(define-libcrypto EVP_sha256 (_fun -> _EVP_MD))

;; In libcrypto 1.1, EVP_MD_CTX_{create,destroy} renamed to {new,free}.
(define-libcrypto EVP_MD_CTX_new (_fun -> _EVP_MD_CTX))
(define-libcrypto EVP_MD_CTX_free (_fun _EVP_MD_CTX -> _void))
(define-libcrypto EVP_MD_CTX_destroy (_fun _EVP_MD_CTX -> _void)
  #:fail (lambda () EVP_MD_CTX_free)
  #:wrap (deallocator))
(define-libcrypto EVP_MD_CTX_create (_fun -> _EVP_MD_CTX)
  #:fail (lambda () EVP_MD_CTX_new)
  #:wrap (allocator EVP_MD_CTX_destroy))
(define-libcrypto EVP_MD_CTX_md (_fun _EVP_MD_CTX -> _EVP_MD))

(define-libcrypto EVP_DigestInit_ex
  (_fun _EVP_MD_CTX _EVP_MD (_pointer = #f) -> _int))
(define-libcrypto EVP_DigestUpdate
  (_fun (ctx data) ::
        (ctx : _EVP_MD_CTX)
        (data : _bytes)
        (_size = (bytes-length data))
        -> _int))
(define-libcrypto EVP_DigestFinal_ex
  (_fun (ctx) ::
        (ctx : _EVP_MD_CTX)
        (out : _bytes = (make-bytes (EVP_MD_size (EVP_MD_CTX_md ctx))))
        (_pointer = #f)
        -> (r : _int)
        -> (and (positive? r) out)))

(define-libcrypto HMAC
  (_fun (alg key data [mdlen (EVP_MD_size alg)]) ::
        (alg : _EVP_MD)
        (key : _bytes)
        (_int = (bytes-length key))
        (data : _bytes)
        (_int = (bytes-length data))
        (md : _bytes = (make-bytes (EVP_MD_size alg)))
        (_pointer = #f)
        -> (r : _pointer)
        -> (and r md)))

(define-libcrypto PKCS5_PBKDF2_HMAC
  (_fun (digest password salt iters keylen) ::
        (password : _bytes)
        (_int = (bytes-length password))
        (salt : _bytes)
        (_int = (bytes-length salt))
        (iters : _int)
        (digest : _EVP_MD)
        (keylen : _int)
        (key : _bytes = (make-bytes keylen))
        -> (r : _int)
        -> (and (positive? r) key)))

;; ----

(define (md digest-name data)
  (define alg (get-evp-md 'digest digest-name))
  (define ctx (EVP_MD_CTX_create))
  (EVP_DigestInit_ex ctx alg)
  (EVP_DigestUpdate ctx (->bytes data))
  (define md (EVP_DigestFinal_ex ctx))
  (EVP_MD_CTX_destroy ctx)
  md)

(define (hmac digest-name key data)
  (define alg (get-evp-md 'hmac digest-name))
  (HMAC alg key (->bytes data)))

(define (pbkdf2 digest-name password salt iters [keylen #f])
  (define alg (get-evp-md 'pbkdf2 digest-name))
  (PKCS5_PBKDF2_HMAC alg password salt iters (or keylen (EVP_MD_size alg))))

(define (get-evp-md who digest)
  (case digest
    [(md5) (EVP_md5)]
    [(sha1) (EVP_sha1)]
    [(sha256) (EVP_sha256)]
    [else (error who "unsupported digest\n  digest: ~e" digest)]))

(define (bytes-xor bs1 bs2)
  (define r (bytes-copy bs1))
  (for ([b1 (in-bytes r)] [b2 (in-bytes bs2)] [i (in-naturals)])
    (bytes-set! r i (bitwise-xor b1 b2)))
  r)

(define (->bytes s) (if (bytes? s) s (string->bytes/utf-8 s)))
