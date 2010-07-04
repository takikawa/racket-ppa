#lang scheme
(require "path-utils.ss")

(define (value->bytes v)
  (with-output-to-bytes (lambda () (write v))))
(define (bytes->value bs ? err)
  (define v (with-input-from-bytes bs read))
  (unless (? v) (err))
  v)

(define (create-archive archive-path root)
  (define start 0)
  (define vals empty)
  (define (make-table path)
    (for/hash ([p (in-list (directory-list path))])
      (define fp (build-path path p))
      (define directory?
        (directory-exists? fp))
      (define val 
        (if directory?
            (value->bytes (make-table fp))
            (file->bytes fp)))
      (define len (bytes-length val))
      (begin0
        (values (path->string p)
                (vector directory? start len))
        (set! start (+ start len))
        (set! vals (cons val vals)))))
  (define root-table 
    (value->bytes (make-table root)))
  
  (with-output-to-file archive-path
    #:exists 'replace
    (lambda ()
      (write (path->string* root))
      (write root-table)
      
      (for ([v (in-list (reverse vals))])
        (write-bytes v)))))

(define (read/? p ? err)
  (with-handlers ([exn:fail? (lambda (x) (err))])
    (define v (read p))
    (if (? v) v
        (err))))

(define (archive-extract-path archive-path p)
  (define ps (explode-path p))
  (define (not-in-archive)
    (error 'archive-extract-path "~e is not in the archive" p))
  (define (bad-archive)
    (error 'archive-extract-path "~e is not a valid archive" archive-path))
  (call-with-input-file
      archive-path
    (lambda (fport)
      (dynamic-wind
       void
       (lambda ()
         (define root-string (read/? fport string? bad-archive))
         (define root (string->path root-string))
         (define roots (explode-path root))
         (define root-len (length roots))
         (unless (root-len . <= . (length ps))
           (not-in-archive))
         (local [(define ps-roots (list-tail ps root-len))
                 (define root-table-bytes (read/? fport bytes? bad-archive))
                 (define root-table (bytes->value root-table-bytes hash? bad-archive))
                 (define heap-start (file-position fport))
                 (define (extract-bytes t p)
                   (match (hash-ref t (path->string p) not-in-archive)
                     [(vector directory? file-start len)
                      ; Jump ahead in the file
                      (file-position fport (+ heap-start file-start))
                      ; Read the bytes
                      (local [(define bs (read-bytes len fport))]
                        (unless (= (bytes-length bs) len)
                          (bad-archive))
                        (values directory? bs))]))
                 (define (extract-table t p)
                   (define-values (dir? bs) (extract-bytes t p))
                   (if dir?
                       (bytes->value bs hash? bad-archive)
                       (not-in-archive)))
                 (define (find-file ps-roots table)
                   (match ps-roots
                     [(list p)
                      (extract-bytes table p)]
                     [(list-rest p rst)
                      (find-file rst (extract-table table p))]))]
           (if (empty? ps-roots)
               (values #t root-table-bytes)
               (find-file ps-roots root-table))))
       (lambda ()
         (close-input-port fport))))))

(define (archive-extract-file archive-path fp)
  (define-values (dir? bs) (archive-extract-path archive-path fp))
  (if dir?
      (error 'archive-extract-file "~e is not a file" fp)
      bs))

(define (archive-directory-list archive-path fp)
  (define (bad-archive)
    (error 'archive-directory-list "~e is not a valid archive" archive-path))
  (define-values (dir? bs) (archive-extract-path archive-path fp))
  (if dir?
      (for/list ([k (in-hash-keys (bytes->value bs hash? bad-archive))])
        (build-path k))
      (error 'archive-directory-list "~e is not a directory" fp)))

(define (archive-directory-exists? archive-path fp)
  (define-values (dir? _) 
    (with-handlers ([exn:fail? (lambda (x) (values #f #f))])
      (archive-extract-path archive-path fp)))
  dir?)

(provide/contract
 [create-archive
  (-> path-string? path-string?
      void)]
 [archive-extract-file
  (-> path-string? path-string?
      bytes?)]
 [archive-directory-list
  (-> path-string? path-string?
      (listof path?))]
 [archive-directory-exists?
  (-> path-string? path-string?
      boolean?)])