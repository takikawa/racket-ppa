#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(require "ffi-constants.rkt")
(provide (all-from-out "ffi-constants.rkt")
         (protect-out (all-defined-out)))

(define sqlite-lib
  (case (system-type)
    [(windows) (ffi-lib "sqlite3.dll" #:fail (lambda () #f))]
    [else (ffi-lib "libsqlite3" '("0" #f) #:fail (lambda () #f))]))

(define-ffi-definer define-sqlite
  sqlite-lib
  #:default-make-fail make-not-available)

; Types
(define-cpointer-type _sqlite3_database)
(define-cpointer-type _sqlite3_statement)

;; -- Functions --

;; -- DB --

(define-sqlite sqlite3_libversion_number
  (_fun -> _int))

(define-sqlite sqlite3_open
  (_fun (filename ignored-flags) ::
        (filename : _bytes)
        (db : (_ptr o _sqlite3_database))
        -> (result : _int)
        -> (values db result)))

(define-sqlite sqlite3_open_v2
  (_fun (filename flags) ::
        (filename : _bytes)
        (db : (_ptr o _sqlite3_database))
        (flags : _int)
        (vfs : _pointer = #f)
        -> (result : _int)
        -> (values db result))
  #:fail (lambda () sqlite3_open))

(define-sqlite sqlite3_close
  (_fun _sqlite3_database
        -> _int))

;; -- Stmt --

(define (copy-buffer buffer)
  (let* ([buffer (string->bytes/utf-8 buffer)]
         [n (bytes-length buffer)]
         [rawcopy (malloc (add1 n) 'atomic-interior)]
         [copy (make-sized-byte-string rawcopy n)])
    (memcpy copy buffer n)
    (ptr-set! rawcopy _byte n 0)
    copy))

(define-sqlite sqlite3_prepare
  (_fun (db sql) ::
        (db : _sqlite3_database)
        (sql-buffer : _bytes = (copy-buffer sql))
        ((bytes-length sql-buffer) : _int)
        (statement : (_ptr o _sqlite3_statement/null))
        (tail : (_ptr o _bytes)) ;; points into sql-buffer (atomic-interior)
        -> (result : _int)
        -> (values result statement (and tail (positive? (bytes-length tail))))))

(define-sqlite sqlite3_prepare_v2
  (_fun (db sql) ::
        (db : _sqlite3_database)
        (sql-buffer : _bytes = (copy-buffer sql))
        ((bytes-length sql-buffer) : _int)
        ;; bad prepare statements set statement to NULL, with no error reported
        (statement : (_ptr o _sqlite3_statement/null))
        (tail : (_ptr o _bytes)) ;; points into sql-buffer (atomic-interior)
        -> (result : _int)
        -> (values result statement (and tail (positive? (bytes-length tail)))))
  #:fail (lambda () sqlite3_prepare))

(define-sqlite sqlite3_finalize
  (_fun _sqlite3_statement
        -> _int
        ;; sqlite3_finalize returns error code of last stmt execution,
        ;; not of finalization; so just ignore
        -> (void)))

(define-sqlite sqlite3_bind_parameter_count
  (_fun _sqlite3_statement
        -> _int))

(define-sqlite sqlite3_column_count
  (_fun _sqlite3_statement
        -> _int))
(define-sqlite sqlite3_column_name
  (_fun _sqlite3_statement _int
        -> _string))
(define-sqlite sqlite3_column_decltype
  (_fun _sqlite3_statement _int
        -> _string))

;; ----------------------------------------

(define-sqlite sqlite3_errcode
  (_fun _sqlite3_database -> _int))
(define-sqlite sqlite3_errmsg
  (_fun _sqlite3_database -> _string))

;; ----------------------------------------

(define-sqlite sqlite3_bind_int
  (_fun _sqlite3_statement _int _int -> _int))
(define-sqlite sqlite3_bind_int64
  (_fun _sqlite3_statement _int _int64 -> _int))
(define-sqlite sqlite3_bind_double
  (_fun _sqlite3_statement _int _double -> _int))
(define-sqlite sqlite3_bind_text
  (_fun (stmt col the-string) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (string-ptr : _string = the-string)
        (string-len : _int = (string-utf-8-length the-string))
        (destructor : _intptr = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_blob
  (_fun (stmt col the-bytes) ::
        (stmt : _sqlite3_statement)
        (col : _int)
        (byte-ptr : _bytes = the-bytes)
        (byte-len : _int = (bytes-length the-bytes))
        (destructor : _intptr = SQLITE_TRANSIENT)
        -> _int))
(define-sqlite sqlite3_bind_null
  (_fun _sqlite3_statement _int -> _int))

(define-sqlite sqlite3_reset
  (_fun _sqlite3_statement -> _int))

(define-sqlite sqlite3_clear_bindings
  (_fun _sqlite3_statement -> _int))

;; ----------------------------------------

(define-sqlite sqlite3_step
  (_fun _sqlite3_statement -> _int))

(define-sqlite sqlite3_column_type
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_int64
  (_fun _sqlite3_statement _int -> _int64))
(define-sqlite sqlite3_column_double
  (_fun _sqlite3_statement _int -> _double))
(define-sqlite sqlite3_column_text
  (_fun _sqlite3_statement _int -> _string))
(define-sqlite sqlite3_column_bytes
  (_fun _sqlite3_statement _int -> _int))
(define-sqlite sqlite3_column_blob
  (_fun (stmt : _sqlite3_statement)
        (col : _int)
        -> (blob : _bytes)
        -> (let ([len (sqlite3_column_bytes stmt col)])
             (bytes-copy (make-sized-byte-string blob len)))))

;; ----------------------------------------

(define-sqlite sqlite3_get_autocommit
  (_fun _sqlite3_database
        -> _bool))

(define-sqlite sqlite3_next_stmt
  (_fun _sqlite3_database _sqlite3_statement/null
        -> _sqlite3_statement/null))

(define-sqlite sqlite3_sql
  (_fun _sqlite3_statement
        -> _string))

(define-sqlite sqlite3_changes
  (_fun _sqlite3_database
        -> _int))

(define-sqlite sqlite3_total_changes
  (_fun _sqlite3_database
        -> _int))

(define-sqlite sqlite3_last_insert_rowid
  (_fun _sqlite3_database
        -> _int64))

;; ----------------------------------------

#|
(require (rename-in racket/contract [-> c->]))

(define status? exact-nonnegative-integer?)

;; Contracts
(provide/contract
 [status?
  (c-> any/c boolean?)]
 [sqlite3_open_v2
  (c-> bytes? exact-nonnegative-integer?
       (values sqlite3_database? status?))]
 [sqlite3_close
  (c-> sqlite3_database? status?)]
 [sqlite3_prepare_v2
  (c-> sqlite3_database? string?
       (values status? (or/c sqlite3_statement? false/c) string?))]
 [sqlite3_errmsg
  (c-> sqlite3_database? string?)]
 [sqlite3_step
  (c-> sqlite3_statement? status?)]
 [sqlite3_bind_parameter_count
  (c-> sqlite3_statement? exact-nonnegative-integer?)]
 [sqlite3_bind_int64
  (c-> sqlite3_statement? exact-nonnegative-integer? integer? status?)]
 [sqlite3_bind_double
  (c-> sqlite3_statement? exact-nonnegative-integer? number? status?)]
 [sqlite3_bind_text
  (c-> sqlite3_statement? exact-nonnegative-integer? string? status?)]
 [sqlite3_bind_null
  (c-> sqlite3_statement? exact-nonnegative-integer? status?)]
 [sqlite3_bind_blob
  (c-> sqlite3_statement? exact-nonnegative-integer? bytes? status?)]
 [sqlite3_column_count
  (c-> sqlite3_statement? exact-nonnegative-integer?)]
 [sqlite3_column_name
  (c-> sqlite3_statement? exact-nonnegative-integer? string?)]
 [sqlite3_column_type
  (c-> sqlite3_statement? exact-nonnegative-integer? exact-nonnegative-integer?)]
 [sqlite3_column_decltype
  (c-> sqlite3_statement? exact-nonnegative-integer? (or/c string? false/c))]
 [sqlite3_column_blob
  (c-> sqlite3_statement? exact-nonnegative-integer? bytes?)]
 [sqlite3_column_text
  (c-> sqlite3_statement? exact-nonnegative-integer? string?)]
 [sqlite3_column_int64
  (c-> sqlite3_statement? exact-nonnegative-integer? integer?)]
 [sqlite3_column_double
  (c-> sqlite3_statement? exact-nonnegative-integer? number?)]
 [sqlite3_reset
  (c-> sqlite3_statement? status?)]
 [sqlite3_clear_bindings
  (c-> sqlite3_statement? status?)]
 [sqlite3_finalize
  (c-> sqlite3_statement? status?)]
 [sqlite3_get_autocommit
  (c-> sqlite3_database? boolean?)])
|#
