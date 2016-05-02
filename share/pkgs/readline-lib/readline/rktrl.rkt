#lang racket/base

(require ffi/unsafe
         (only-in '#%foreign ffi-obj)
         setup/dirs)
(provide readline readline-bytes
         add-history add-history-bytes
         history-length history-get history-delete
         set-completion-function!
         readline-newline readline-redisplay)

;; libncurses and/or libtermcap needed on some platforms
(void (ffi-lib "libcurses" #:fail (lambda () #f)))
(void (ffi-lib "libtermcap" #:fail (lambda () #f)))

; find-libreadline : (U path #f) -> (U ffi-lib? #f)
(define (find-libreadline path)
  (and path
       (let ([libreadline (build-path path "readline-lib.rkt")])
         (and (file-exists? libreadline)
              (module-path? libreadline)
              (dynamic-require libreadline 'readline-library
                               (lambda () #f))))))

;; Deal with old versions of libedit:
;;  - history-get is 0 indexed in old versions of libedit, rather 1 indexed like in libreadline
;;  - history-delete doesn't re-index the history buffer.
(define old-libedit #f)
(define libreadline
  (or
   (let ([readline-path (getenv "PLT_READLINE_LIB")])
     (and readline-path
          (ffi-lib readline-path (lambda () #f))))
   (find-libreadline (find-user-share-dir))
   (find-libreadline (find-share-dir))
   ;; Old versions of libedit have a 1 indexed history rather than a 0 indexed history.
   ;; Thus, if the user is running an old version of libedit, fail to load the module.
   ;; XREPL should still run without linediting support.
   (let ([lib (ffi-lib "libedit" '("2.11") #:fail (lambda () #f))])
     (when lib
       (set! old-libedit #t))
     lib)
   (ffi-lib "libedit" '("3" "2" "0.0.43" "0.0.53" ""))))

(define make-byte-string ; helper for the two types below
  (get-ffi-obj "scheme_make_byte_string" #f (_fun _pointer -> _scheme)))

(define _bytes/eof/free ; register a finalizer on the resulting bytes
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) x))
    (lambda (x)
      (if x
        (let ([b (make-byte-string x)])
          (register-finalizer b (lambda (_) (free x)))
          b)
        eof))))

(define _string/eof/free ; make a Scheme str from C str & free immediately
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) (string->bytes/utf-8 x)))
    (lambda (x)
      (if x
        (let ([s (bytes->string/utf-8 (make-byte-string x))]) (free x) s)
        eof))))

(define readline
  (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof/free)))

(define readline-bytes
  (get-ffi-obj "readline" libreadline (_fun _bytes -> _bytes/eof/free)))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

(define add-history-bytes
  (get-ffi-obj "add_history" libreadline (_fun _bytes -> _void)))

(define history-length
  (let ([hl (ffi-obj #"history_length" libreadline)])
    (lambda () (ptr-ref hl _int))))
(define history-base
  (let ([hb (ffi-obj #"history_base" libreadline)])
    (lambda () (ptr-ref hb _int))))

;; The history library has this great feature: *some* function consume
;; an index that is relative to history_base, and *some* get a plain
;; offset.  Someone just had so much fun they had to share.  This
;; deals with this absurdity, checks the range of the index, and deals
;; with negative offsets.
(define (hist-idx who idx base?)
  (let* ([len (history-length)]
         [idx (cond [(<= 0 idx (sub1 len)) idx]
                    [(<= (- len) idx -1)   (+ len idx)]
                    [else (error who "index out of history range, -~a - ~a"
                                 len (sub1 len))])]
         [idx (if (and old-libedit (equal? who 'history-get))
                  (sub1 idx)
                  idx)])
    (if base? (+ idx (history-base)) idx)))

;; actually, returns a pointer to a struct with the string, but all we
;; care about is the string...
(define history-get
  (get-ffi-obj "history_get" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-get i #t)) -> (_ptr o _string))))

(define history-remove ; returns HIST_ENTRY* that history_free() frees
  (get-ffi-obj "remove_history" libreadline
    (_fun (i) :: (_int = (hist-idx 'history-delete i #f)) -> _pointer)))
(define history-free ; ignore histdata_t return value
  (get-ffi-obj "free_history_entry" libreadline (_fun _pointer -> _void)
               ;; if not available, use free
               (lambda () free)))
(define clear-history ; for old versions of libreadline where history-remove doesn't re-index history0
  (get-ffi-obj "clear_history" libreadline (_fun -> _void)))

;; If libedit is old, history-remove doesn't properly re-index buffer, so clear it and
;; re-add everything except the removed element
(define (history-delete idx)
  (cond
    [old-libedit
     (define len (history-length))
     (define idx* (hist-idx 'history-get idx #t))
     (define new-hist
       (for/list ([i (in-range len)]
                  #:unless (= i idx*))
         (history-get i)))
     (clear-history)
     (for ([i new-hist])
       (add-history i))]
    [else
     (let ([line (history-remove idx)])
        (and line (history-free line)))]))

;; Simple completion: use this with a (string -> (list-of string)) function
;; that returns the completions for a given string (can be used with other
;; input string types too, depending on the `type' argument).  Use #f to remove
;; a completion function that was previously set.
(define set-completion-function!
  (case-lambda
    [(func) (set-completion-function! func _string)]
    [(func type)
     (if func
       (set-ffi-obj! "rl_completion_entry_function" libreadline
                     (_fun type _int -> _pointer)
                     (completion-function func))
       (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f))]))

(define (completion-function func)
  (let ([cur '()])
    (define (complete str state)
      (if (zero? state)
        (begin (set! cur (func str)) (complete #f 1))
        (and (pair? cur)
             (let* ([cur (begin0 (car cur) (set! cur (cdr cur)))]
                    [cur (if (string? cur) (string->bytes/utf-8 cur) cur)])
               (malloc (add1 (bytes-length cur)) cur 'raw)))))
    complete))

(set-ffi-obj! "rl_readline_name" libreadline _pointer
              (let ([s #"mzscheme"])
                (define m (malloc (add1 (bytes-length s)) 'atomic-interior))
                (memcpy m s (add1 (bytes-length s)))
                m))

;; need to capture the real input port below
(define real-input-port (current-input-port))
(unless (eq? 'stdin (object-name real-input-port))
  (log-warning "mzrl warning: could not capture the real input port\n"))
(unless (terminal-port? real-input-port)
  (log-warning "mzrl warning: input port is not a terminal\n"))


;; We need to tell readline to pull content through our own function,
;; to avoid buffering issues between C and Racket, and to allow
;; racket threads to run while waiting for input.
(set-ffi-obj! "rl_getc_function" libreadline (_fun _pointer -> _int)
              (lambda (_)
                (define next-byte (read-byte real-input-port))
                (if (eof-object? next-byte) -1 next-byte)))


;; force cursor on a new line
(define readline-newline
  (get-ffi-obj "rl_crlf" libreadline (_fun -> _void)
               (lambda ()
                 (get-ffi-obj "rl_newline" libreadline (_fun -> _void)))))

;; force redisplay of prompt and current user input
(define readline-redisplay
  (get-ffi-obj "rl_forced_update_display" libreadline (_fun -> _void)))
