#lang racket/base
(require racket/match
         racket/string
         racket/random
         racket/list
         net/base64
         "saslprep.rkt"
         "base.rkt"
         "crypto.rkt")
(provide (all-defined-out))

(define-syntax-rule (define/ref h (var ...))
  (begin (define var (hash-ref h 'var)) ...))
(define-syntax-rule (define! h var rhs)
  (begin (define var rhs) (hash-set! h 'var var)))

;; FIXME: wrap base64 and other error-raising helpers so ctx fails

;; ============================================================

(struct scram-client-ctx sasl-ctx (h))

;; ============================================================

;; 1: C->S

(define (make-scram-client-ctx digest authcid password
                               #:authorization-id [authzid #f]
                               #:client-nonce [client-nonce0 #f])
  (define h (make-hasheq))
  (hash-set! h 'digest digest)
  (define! h cbind #f)
  (define! h p-authcid (saslprep authcid #:who 'make-scram-client-ctx))
  (define! h p-authzid (and authzid (saslprep authzid #:who 'make-scram-client-ctx)))
  (define! h p-password (string->bytes/utf-8 (saslprep password)))
  (define! h gs2-header
    (string-append (cond [(string? cbind) (format "p=~a" cbind)]
                         [else (if cbind "y" "n")])
                   "," (if p-authzid (format "a=~a" (encode-name p-authzid)) "") ","))
  (define! h client-nonce (or client-nonce0 (generate-client-nonce)))
  (define! h msg-c1/bare (format "n=~a,r=~a" (encode-name p-authcid) client-nonce))
  (define! h msg-c1 (string-append gs2-header msg-c1/bare))
  (scram-client-ctx msg-c1 scram-client-receive-1 h))

(define CLIENT-NONCE-SIZE 24)
(define (generate-client-nonce)
  ;; How long? SCRAM example has 24 chars.
  ;; Must be printable: %x21-2B / %x2D-7E
  (define out (open-output-string))
  ;; filter, keep printable chars (but not #\,)
  (for ([b (in-bytes (crypto-random-bytes (* 2 CLIENT-NONCE-SIZE)))])
    (let ([b (bitwise-and #x7F b)])
      (when (or (<= #x21 b #x2B) (<= #x2D b #x7E))
        (write-byte b out))))
  (define r (get-output-string out))
  (cond [(>= (string-length r) CLIENT-NONCE-SIZE)
         (substring r 0 CLIENT-NONCE-SIZE)]
        [else ;; unlikely!
         (generate-client-nonce)]))

;; 2: S->C
;; 3: C->S

;; scram-client-receive-1 : Ctx String -> Void
(define (scram-client-receive-1 ctx msg-s1)
  (match-define (scram-client-ctx _ _ h) ctx)
  (define/ref h (client-nonce digest gs2-header p-password cbind msg-c1/bare))
  (hash-set! h 'msg-s1 msg-s1)
  (define records (split-message ctx msg-s1 '(#\i #\s #\r)))

  (define! h iters
    (let ([iters0 (hash-ref records #\i)])
      ;; FIXME: check >= 4096 or customizable limit?
      (unless (regexp-match? #rx"^[0-9]+$" iters0)
        (fatal ctx "got bad iteration count from server\n  got: ~e" iters0))
      (string->number iters0)))
  (define! h salt (base64->bytes (hash-ref records #\s)))
  ;; FIXME: check salt length?
  (define! h nonce (hash-ref records #\r))
  ;; FIXME: check nonce only printable chars?
  (unless (and (> (string-length nonce) (string-length client-nonce))
               (equal? client-nonce (substring nonce 0 (string-length client-nonce))))
    (fatal ctx "got bad nonce from server (not extension of chosen prefix)"))

  ;; SaltedPassword  := Hi(Normalize(password), salt, i)
  (define! h salted-password (pbkdf2 digest p-password salt iters))
  ;; ClientKey       := HMAC(SaltedPassword, "Client Key")
  (define! h client-key (hmac digest salted-password #"Client Key"))
  ;; ServerKey       := HMAC(SaltedPassword, "Server Key")
  (define! h server-key (hmac digest salted-password #"Server Key"))

  ;; cbind-input = gs2-header [ cbind-data ] -- present iff gs2-cbind-flag="p"
  (define cbind-data (if (string? cbind) (fatal ctx "unimplemented") ""))
  (define! h cbind-input (string-append gs2-header cbind-data))
  ;; channel-binding = "c=" <base64 encoding of cbind-input>
  ;; client-final-message-wo-proof = channel-binding "," nonce ["," extensions]
  (define! h msg-c2/no-proof (format "c=~a,r=~a" (->base64-bytes cbind-input) nonce))

  ;; StoredKey       := H(ClientKey)
  (define! h stored-key (md digest client-key))
  ;; AuthMessage     := client-first-message-bare + "," +
  ;;                    server-first-message + "," +
  ;;                    client-final-message-without-proof
  (define! h auth-message (string-append msg-c1/bare "," msg-s1 "," msg-c2/no-proof))
  ;; ClientSignature := HMAC(StoredKey, AuthMessage)
  (define! h client-signature (hmac digest stored-key auth-message))
  ;; ClientProof     := ClientKey XOR ClientSignature
  (define! h client-proof (bytes-xor client-key client-signature))
  ;; ServerSignature := HMAC(ServerKey, AuthMessage)
  (define! h server-signature (hmac digest server-key auth-message))
  ;; client-final-message = client-final-message-wo-proof "," proof
  (define! h msg-c2 (format "~a,p=~a" msg-c2/no-proof (base64-encode client-proof "")))
  (set-sasl! ctx msg-c2 scram-client-receive-2))

;; 4: S->C

(define (scram-client-receive-2 ctx msg-s2)
  (match-define (scram-client-ctx _ _ h) ctx)
  (define/ref h (server-signature))
  (hash-set! h 'msg-s2 msg-s2)
  (define records (split-message ctx msg-s2 '()))
  (cond [(hash-ref records #\v)
         => (lambda (verifier)
              (define! h signature (base64->bytes verifier))
              (unless (equal? signature server-signature)
                (fatal ctx "received invalid signature from server"))
              (set-sasl! ctx #f 'done))]
        [(hash-ref records #\e)
         => (lambda (server-error)
              (hash-set! h 'error server-error)
              (fatal ctx "received error from server\n  error: ~a" server-error))]
        [else (fatal ctx "received unknown response from server (expected signature or error)")]))

;; ------------------------------------------------------------

(define (encode-name s)
  (if (regexp-match? #rx"[=,]" s)
      (let ([out (open-output-string)])
        (for ([c (in-string s)])
          (case c
            [(#\,) (write-string "=2C" out)]
            [(#\=) (write-string "=3D" out)]
            [else (write-char c out)]))
        (get-output-string out))
      s))

(define (split-message ctx msg required-keys)
  (define (decode-attr-val s)
    (define (bad) (fatal ctx "error parsing attribute in received message\n  input: ~e" s))
    (unless (>= (string-length s) 2) (bad))
    (unless (alpha-char? (string-ref s 0)) (bad))
    (unless (equal? (substring s 1 2) "=") (bad))
    (cons (string-ref s 0) (substring s 2)))
  (define attrs (map decode-attr-val (string-split msg "," #:trim? #f)))
  (define keys (map car attrs))
  (let ([dup (check-duplicates keys)])
    (when dup (fatal ctx "duplicate attribute in received message\n  attribute: ~e" dup)))
  (for ([key (in-list required-keys)])
    (unless (member key keys)
      (fatal ctx "missing required attribute in received message\n  attribute: ~e" key)))
  (make-hash attrs))

(define (alpha-char? c)
  (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))
