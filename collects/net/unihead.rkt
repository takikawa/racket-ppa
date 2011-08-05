#lang racket/base
(require net/base64 net/qp racket/string)

(provide encode-for-header decode-for-header generalize-encoding)

(define re:ascii #rx"^[\u0-\u7F]*$")

(define (encode-for-header s)
  (if (regexp-match? re:ascii s)
    s
    (let ([l (regexp-split #rx"\r\n" s)])
      (apply string-append
             (map encode-line-for-header l)))))

(define (encode-line-for-header s)
  (define (loop s string->bytes charset encode encoding)
    ;; Find ASCII (and no "=") prefix before a space
    (let ([m (regexp-match #rx"^([\u0-\u3c\u3e-\u7F]* )(.*)$" s)])
      (if m
        (string-append
         (cadr m)
         (loop (caddr m) string->bytes charset encode encoding))
        ;; Find ASCII (and no "=") suffix after a space
        (let ([m (regexp-match #rx"^(.*?)( [\u0-\u3c\u3e-\u7F]*)$" s)])
          (if m
            (string-append
             (loop (cadr m) string->bytes charset encode encoding)
             (caddr m))
            (format "=?~a?~a?~a?="
                    charset encoding
                    (regexp-replace* #rx#"[\r\n]+$"
                                     (encode (string->bytes s))
                                     #"")))))))
  (cond
    [(regexp-match? re:ascii s)
     ;; ASCII - do nothing
     s]
    [(regexp-match? #rx"[^\u0-\uFF]" s)
     ;; Not Latin-1, so use UTF-8
     (loop s string->bytes/utf-8 "UTF-8" base64-encode "B")]
    [else
     ;; use Latin-1
     (loop s string->bytes/latin-1 "ISO-8859-1"
           (lambda (s)
             (regexp-replace #rx#" " (qp-encode s) #"_"))
           "Q")]))

;; ----------------------------------------

(define re:encoded #rx#"^(.*?)=[?]([^?]+)[?]([qQbB])[?](.*?)[?]=(.*)$")

(define (generalize-encoding encoding)
  ;; Treat Latin-1 as Windows-1252 and also threat GB and GB2312
  ;; as GBK, because some mailers are broken.
  (cond [(or (regexp-match? #rx#"^(?i:iso-8859-1)$" encoding)
             (regexp-match? #rx#"^(?i:us-ascii)$" encoding))
         (if (bytes? encoding) #"WINDOWS-1252" "WINDOWS-1252")]
        [(regexp-match? #rx#"^(?i:gb(?:2312)?)$" encoding)
         (if (bytes? encoding) #"GBK" "GBK")]
        [(regexp-match? #rx#"^(?i:ks_c_5601-1987)$" encoding)
         (if (bytes? encoding) #"CP949" "CP949")]
        [else encoding]))

(define (decode-for-header s)
  (and s
       (let ([m (regexp-match re:encoded
                              (string->bytes/latin-1 s (char->integer #\?)))])
         (if m
           (let ([s ((if (member (cadddr m) '(#"q" #"Q"))
                       ;; quoted-printable, with special _ handling
                       (lambda (x)
                         (qp-decode (regexp-replace* #rx#"_" x #" ")))
                       ;; base64:
                       base64-decode)
                     (cadddr (cdr m)))]
                 [encoding (caddr m)])
             (string-append
              (decode-for-header (bytes->string/latin-1 (cadr m)))
              (let ([encoding (generalize-encoding encoding)])
                (cond
                  [(regexp-match? #rx#"^(?i:utf-8)$" encoding)
                   (bytes->string/utf-8 s #\?)]
                  [else (let ([c (bytes-open-converter
                                  (bytes->string/latin-1 encoding)
                                  "UTF-8")])
                          (if c
                            (let-values ([(r got status)
                                          (bytes-convert c s)])
                              (bytes-close-converter c)
                              (if (eq? status 'complete)
                                (bytes->string/utf-8 r #\?)
                                (bytes->string/latin-1 s)))
                            (bytes->string/latin-1 s)))]))
              (let ([rest (cadddr (cddr m))])
                (let ([rest
                       ;; A CR-LF-space-encoding sequence means that we
                       ;; should drop the space.
                       (if (and (> (bytes-length rest) 4)
                                (= 13 (bytes-ref rest 0))
                                (= 10 (bytes-ref rest 1))
                                (= 32 (bytes-ref rest 2))
                                (let ([m (regexp-match-positions
                                          re:encoded rest)])
                                  (and m (= (caaddr m) 5))))
                         (subbytes rest 3)
                         rest)])
                  (decode-for-header (bytes->string/latin-1 rest))))))
           s))))
