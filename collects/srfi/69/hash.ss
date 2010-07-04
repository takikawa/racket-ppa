; SRFI 69
; Chongkai Zhu   mrmathematica@yahoo.com
; 01-Nov-2005

(module hash mzscheme
  
  (require (lib "etc.ss"))
  
  (provide (rename my-make-hash-table s:make-hash-table)
           (rename my-hash-table? s:hash-table?)
           alist->hash-table
           (rename my-hash-table-equivalence-function hash-table-equivalence-function)
           (rename my-hash-table-hash-function hash-table-hash-function)
           hash-table-ref
           hash-table-ref/default
           hash-table-set!
           hash-table-delete!
           hash-table-exists?
           hash-table-update!
           hash-table-update!/default
           (rename my-hash-table-size hash-table-size)
           hash-table-keys
           hash-table-values
           hash-table-walk
           hash-table-fold
           hash-table->alist
           (rename my-hash-table-copy s:hash-table-copy)
           hash-table-merge!
           hash
           string-hash
           string-ci-hash
           hash-by-identity)
  
  (define hash
    (case-lambda
      ((obj) (equal-hash-code obj))
      ((obj bound) (modulo (equal-hash-code obj) bound))))
  
  (define (string-hash . args)
    (unless (string? (car args))
      (raise-type-error 'string-hash "string" 1 args))
    (apply hash args))
  
  (define (string-ci-hash s . maybe-bound)
    (unless (string? s)
      (raise-type-error 'string-hash "string" s))
    (apply hash (string-downcase s) maybe-bound))
  
  (define hash-by-identity
    (case-lambda
      ((obj) (eq-hash-code obj))
      ((obj bound) (modulo (eq-hash-code obj) bound))))
  
  (define-struct my-hash-table (size hash-function equivalence-function association-function entries))

  (define *default-table-size* 64)
  
  (define (appropriate-hash-function-for comparison)
    (cond ((eq? comparison eq?) hash-by-identity)
          ((eq? comparison string=?) string-hash)
          ((eq? comparison string-ci=?) string-ci-hash)
          (else hash)))
  
  (define my-make-hash-table
    (opt-lambda ([comparison equal?]
                 [hash (appropriate-hash-function-for comparison)]
                 [size *default-table-size*]
                 [association (cond ((eq? comparison eq?) assq)
                                    ((eq? comparison eqv?) assv)
                                    ((eq? comparison equal?) assoc)
                                    (else (letrec ((associate
                                                   (lambda (val alist)
                                                     (cond ((null? alist) #f)
                                                           ((comparison val (caar alist)) (car alist))
                                                           (else (associate val (cdr alist)))))))
                                            associate)))])
      (make-my-hash-table 0 hash comparison association (make-vector size '()))))
  
  (define (%hash-table-hash hash-table key)
    ((my-hash-table-hash-function hash-table)
     key (vector-length (my-hash-table-entries hash-table))))
  
  (define (%hash-table-find entries associate hash key)
    (associate key (vector-ref entries hash)))
  
  (define (%hash-table-add! entries hash key value)
    (vector-set! entries hash
                 (cons (cons key value)
                       (vector-ref entries hash))))
  
  (define (%hash-table-delete! entries compare hash key)
    (let ((entrylist (vector-ref entries hash)))
      (cond ((null? entrylist) #f)
            ((compare key (caar entrylist))
             (vector-set! entries hash (cdr entrylist)) #t)
            (else
             (let loop ((current (cdr entrylist)) (previous entrylist))
               (cond ((null? current) #f)
                     ((compare key (caar current))
                      (set-cdr! previous (cdr current)) #t)
                     (else (loop (cdr current) current))))))))
  
  (define (%hash-table-walk proc entries)
    (do ((index (- (vector-length entries) 1) (- index 1)))
      ((< index 0)) (for-each proc (vector-ref entries index))))
  
  (define (%hash-table-maybe-resize! hash-table)
    (let* ((old-entries (my-hash-table-entries hash-table))
           (hash-length (vector-length old-entries)))
      (if (> (my-hash-table-size hash-table) hash-length)
          (let* ((new-length (* 2 hash-length))
                 (new-entries (make-vector new-length '()))
                 (hash (my-hash-table-hash-function hash-table)))
            (%hash-table-walk
              (lambda (node)
                (%hash-table-add! new-entries
                                  (hash (car node) new-length)
                                  (car node) (cdr node)))
              old-entries)
            (set-my-hash-table-entries! hash-table new-entries)))))
  
  (define (hash-table-ref hash-table key . maybe-default)
    (cond ((%hash-table-find (my-hash-table-entries hash-table)
                             (my-hash-table-association-function hash-table)
                             (%hash-table-hash hash-table key) key)
            => cdr)
          ((null? maybe-default)
           (raise-mismatch-error 'hash-table-ref "no value associated with " key))
          (else ((car maybe-default)))))
  
  (define-syntax hash-table-ref/default
    (syntax-rules ()
      ((_ hash-table key default)
       (hash-table-ref hash-table key (lambda () default)))))
  
  (define (hash-table-set! hash-table key value)
    (let ((hash (%hash-table-hash hash-table key))
          (entries (my-hash-table-entries hash-table)))
      (cond ((%hash-table-find entries
                               (my-hash-table-association-function hash-table)
                               hash key)
              => (lambda (node) (set-cdr! node value)))
            (else (%hash-table-add! entries hash key value)
                  (set-my-hash-table-size! hash-table
                                           (+ 1 (my-hash-table-size hash-table)))
                  (%hash-table-maybe-resize! hash-table)))))
  
  (define (hash-table-update! hash-table key function . maybe-default)
    (let ((hash (%hash-table-hash hash-table key))
          (entries (my-hash-table-entries hash-table)))
      (cond ((%hash-table-find entries
                               (my-hash-table-association-function hash-table)
                               hash key)
              => (lambda (node)
                   (set-cdr! node (function (cdr node)))))
            ((null? maybe-default)
             (raise-mismatch-error 'hash-table-update "no value exists for key " key))
            (else (%hash-table-add! entries hash key
                                    (function ((car maybe-default))))
                  (set-my-hash-table-size! hash-table
                                           (+ 1 (my-hash-table-size hash-table)))
                  (%hash-table-maybe-resize! hash-table)))))
  
  (define-syntax hash-table-update!/default
    (syntax-rules ()
      ((_ hash-table key function default)
       (hash-table-update! hash-table key function (lambda () default)))))
  
  (define (hash-table-delete! hash-table key)
    (if (%hash-table-delete! (my-hash-table-entries hash-table)
                             (my-hash-table-equivalence-function hash-table)
                             (%hash-table-hash hash-table key) key)
        (set-my-hash-table-size! hash-table (- (my-hash-table-size hash-table) 1))))
  
  (define (hash-table-exists? hash-table key)
    (and (%hash-table-find (my-hash-table-entries hash-table)
                           (my-hash-table-association-function hash-table)
                           (%hash-table-hash hash-table key) key) #t))
  
  (define (hash-table-walk hash-table proc)
    (%hash-table-walk
      (lambda (node) (proc (car node) (cdr node)))
      (my-hash-table-entries hash-table)))
  
  (define (hash-table-fold hash-table f acc)
    (hash-table-walk hash-table 
                     (lambda (key value) (set! acc (f key value acc))))
    acc)
  
  (define alist->hash-table
    (opt-lambda (alist
                 [comparison equal?]
                 [hash (appropriate-hash-function-for comparison)]
                 [size (max *default-table-size* (* 2 (length alist)))]
                 [hash-table (my-make-hash-table comparison hash size)])
      (for-each (lambda (elem)
                  (hash-table-set! hash-table (car elem) (cdr elem)))
                alist)
      hash-table))
  
  (define (hash-table->alist hash-table)
    (hash-table-fold hash-table
                     (lambda (key val acc) (cons (cons key val) acc)) '()))
  
  (define (my-hash-table-copy hash-table)
    (let ((new (my-make-hash-table (my-hash-table-equivalence-function hash-table)
                                   (my-hash-table-hash-function hash-table)
                                   (max *default-table-size*
                                        (* 2 (my-hash-table-size hash-table))))))
      (hash-table-walk hash-table
                       (lambda (key value) (hash-table-set! new key value)))
      new))
  
  (define (hash-table-merge! hash-table1 hash-table2)
    (hash-table-walk hash-table2
                     (lambda (key value)
                       (hash-table-set! hash-table1 key value)))
    hash-table1)
  
  (define (hash-table-keys hash-table)
    (hash-table-fold hash-table (lambda (key val acc) (cons key acc)) '()))
  
  (define (hash-table-values hash-table)
    (hash-table-fold hash-table (lambda (key val acc) (cons val acc)) '()))
  
  )
 