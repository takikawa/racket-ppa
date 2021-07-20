#lang racket/base

(require ffi/unsafe
         ffi/unsafe/os-thread
         ffi/unsafe/os-async-channel
         ffi/unsafe/vm
         ffi/unsafe/schedule
         ffi/unsafe/atomic
         (only-in '#%foreign ffi-obj)
         setup/dirs)
(provide readline readline-bytes
         add-history add-history-bytes
         history-length history-get history-delete
         (protect-out set-completion-function!)
         set-completion-append-character!
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
          (ffi-lib readline-path #:fail (lambda () #f))))
   (find-libreadline (find-user-share-dir))
   (find-libreadline (find-share-dir))
   ;; Old versions of libedit have a 1 indexed history rather than a 0 indexed history.
   ;; Thus, if the user is running an old version of libedit, fail to load the module.
   ;; XREPL should still run without linediting support.
   (let ([lib (ffi-lib "libedit" '("2.11") #:fail (lambda () #f))])
     (when lib
       (set! old-libedit #t))
     lib)
   (ffi-lib "libedit" '("3" "2" "0.0.43" "0.0.53" "0" ""))))

(define _bytes/eof/free ; copies bytes and frees result pointer
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) x))
    (lambda (x)
      (if x
          (begin0
            (bytes-copy (cast x _pointer _bytes))
            (free x))
          eof))))

;; Keep some values alive as long as the current place exists
(define alive-values (box null))
(void (malloc-immobile-cell alive-values))
(define (keep-alive! v)
  (set-box! alive-values (cons v (unbox alive-values))))

(define _string/eof/free ; make a Scheme str from C str & free immediately
  (make-ctype _pointer
    (lambda (x) (and (not (eof-object? x)) (string->bytes/utf-8 x)))
    (lambda (x)
      (if x
          (begin0
            (cast x _pointer _string)
            (free x))
          eof))))

(define use-cs-other-thread?
  (eq? 'chez-scheme (system-type 'vm)))

(define-values (readline
                readline-bytes
                callback-for-potentially-foreign-thread
                read-byte/maybe-foreign-thread
                read-char/maybe-foreign-thread)
  (let ()
    (cond
      [use-cs-other-thread?
       ;; On CS, rl_getc_function has to be atomic, but we want to
       ;; read from a Racket input port. So, run `readline` or
       ;; `readline-bytes` in a separate Scheme thread, and have it
       ;; request via Scheme-level synchronization.
       (define readline-ptr (get-ffi-obj "readline" libreadline _fpointer))
       (define readline-bytes-ptr (get-ffi-obj "readline" libreadline _fpointer))
       ;; A `context` struct communicates information about an enclosing
       ;; `readline` request to the callback installed for reading characters/bytes:
       (struct context (request-ch response-ch break-esc))
       (define current-context (vm-eval '(make-thread-parameter #f)))
       ;; To handle errors/breaks, we'll arrange for a request to
       ;; jump out of an enclosing `readline` call:
       (define call-with-exit-proc
         ;; From the Chez Scheme manual, causes a continuation jump to pop C frames:
         (vm-eval '(lambda (p)
                     (define th (lambda () (call/cc p)))
                     (define-ftype ->ptr (function () ptr))
                     (let ([fptr (make-ftype-pointer ->ptr th)])
                       (let ([v ((ftype-ref ->ptr () fptr))])
                         (unlock-object
                          (foreign-callable-code-object
                           (ftype-pointer-address fptr)))
                         v)))))
       ;; Convert a procedure pointer to a Scheme foreign function:
       (define (proc-ptr-to-proc proc-ptr)
         (vm-eval `(foreign-procedure __collect_safe ,(cast proc-ptr _pointer _uintptr)
                                      (uptr)
                                      uptr)))
       ;; Wrapper for a readline proc:
       (define (make-readline readline to-bytes _result)
         (lambda (prompt)
           ;; Convert prompt to an immobile, nul-terminated byte array:
           (define prompt-bytes (to-bytes prompt))
           (define len (bytes-length prompt-bytes))
           (define prompt-mem (malloc (add1 len) 'atomic-interior))
           (memcpy prompt-mem prompt-bytes len)
           (ptr-set! prompt-mem _byte len 0)
           (define prompt-addr (cast prompt-mem _pointer _uintptr))
           ;; Request to Scheme thread for reading characters:
           (define request-ch (make-os-async-channel))
           ;; Response from Scheme thread reading characters:
           (define response-ch (make-os-async-channel))
           ;; Response from readline thread (which sends requests to Scheme thread
           ;; and receives its responses):
           (define readline-ch (make-os-async-channel))
           ;; Start a Racket thread can can serve `read-char`/`read-byte` requests,
           ;; of which there is at most one active at a time:
           (define request-server
             (thread
              (lambda ()
                (let loop ()
                  (define proc (sync request-ch))
                  (define result (proc))
                  (os-async-channel-put response-ch result)
                  (loop)))))
           ;; Run readline in a new thread:
           (call-in-os-thread
            (lambda ()
              ;; in non-Racket Scheme thread
              (call-with-exit-proc
               (lambda (k)
                 (current-context (context request-ch response-ch k))
                 (os-async-channel-put readline-ch (readline prompt-addr))
                 (void/reference-sink prompt-mem)))))
           ;; Wait for the result, and on escape (due to an error or
           ;; break), communicate the escape also to any request in
           ;; flight:
           (dynamic-wind
            void
            (lambda ()
              (define addr (sync readline-ch))
              ;; Convert result to a [byte] string:
              (cast addr _uintptr _result))
            (lambda ()
              (kill-thread request-server)
              (set! request-server #f)
              ;; On behalf of `request-server`, reply with 'escape:
              (os-async-channel-put response-ch 'escape)))))
       (define (make-reader read-byte-or-char)
         ;; Called in a non-Racket Scheme thread:
         (lambda (input-port)
           (define ctx (current-context))
           ;; Bounce the request over to the current request-handler
           ;; Racket thread by using the current thread's request box
           (os-async-channel-put (context-request-ch ctx) (lambda () (read-byte-or-char input-port)))
           (define result (os-async-channel-get (context-response-ch ctx)))
           (if (eq? result 'escape)
               ((context-break-esc ctx) #f)
               result)))
       (values (make-readline (proc-ptr-to-proc readline-ptr) string->bytes/utf-8 _string/eof/free)
               (make-readline (proc-ptr-to-proc readline-bytes-ptr) values _bytes/eof/free)
               (lambda (proc)
                 (vm-eval `(let ([x (foreign-callable __collect_safe ',proc (uptr) int)])
                             (lock-object x)
                             (foreign-callable-entry-point x))))
               (make-reader read-byte)
               (make-reader read-char))]
      [else
       ;; When callbacks can be non-atomic, then it's all straightforward:
       (define readline
         (get-ffi-obj "readline" libreadline (_fun _string -> _string/eof/free)))
       (define readline-bytes
         (get-ffi-obj "readline" libreadline (_fun _bytes/nul-terminated -> _bytes/eof/free)))
       (values readline
               readline-bytes
               values ; callback-for-potentially-foreign-thread
               read-byte
               read-char)])))

(define-values (start-nonatomic end-nonatomic)
  (cond
    [(and (not use-cs-other-thread?)
          (eq? 'chez-scheme (system-type 'vm)))
     (values end-atomic start-atomic)]
    [else
     (values void void)]))

(define add-history
  (get-ffi-obj "add_history" libreadline (_fun _string -> _void)))

(define add-history-bytes
  (get-ffi-obj "add_history" libreadline (_fun _bytes/nul-terminated -> _void)))

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
                       (_fun #:keep keep-alive!
                             #:async-apply (and use-cs-other-thread?
                                                (lambda (p) (p)))
                             type _int -> _pointer)
                       (completion-function func))
         (set-ffi-obj! "rl_completion_entry_function" libreadline _pointer #f))]))

(define (completion-function func)
  (let ([cur '()])
    (define (complete str state)
      (if (zero? state)
        (begin (set! cur (func str)) (complete #f 1))
        (and (pair? cur)
             (let* ([cur (begin0 (car cur) (set! cur (cdr cur)))]
                    [cur (if (string? cur) (string->bytes/utf-8 cur) cur)]
                    [len (bytes-length cur)]
                    [p (malloc (add1 len) 'raw)])
                 (memcpy p cur len)
                 (ptr-set! p _byte len 0)
                 p))))
    complete))

(define (set-completion-append-character! c)
  (cond [(char? c)
         (set-ffi-obj! "rl_completion_append_character"
                       libreadline
                       _int
                       (char->integer c))]
        [else (raise-argument-error 'set-completion-append-character!
                                    "char?"
                                    c)]))

(set-ffi-obj! "rl_readline_name" libreadline _pointer
              (let ([s #"mzscheme"])
                (define len (bytes-length s))
                (define m (malloc (add1 len) 'atomic-interior))
                (memcpy m s len)
                (ptr-set! m _byte len 0)
                (keep-alive! m)
                m))

;; need to capture the real input port below
(define real-input-port (current-input-port))
(unless (eq? 'stdin (object-name real-input-port))
  (log-warning "mzrl warning: could not capture the real input port\n"))
(unless (terminal-port? real-input-port)
  (log-warning "mzrl warning: input port is not a terminal\n"))

;; We need to tell readline to pull content through our own function,
;; to avoid buffering issues between C and Racket, and to allow racket
;; threads to run while waiting for input. Beware that the function is
;; called in a non-Racket thread in CS when other-thread mode is on.
(set-ffi-obj! "rl_getc_function" libreadline (if use-cs-other-thread?
                                                 _uintptr
                                                 (_fun #:keep keep-alive! _pointer -> _int))
              (callback-for-potentially-foreign-thread
               ;; How does rl_getc_function return Unicode characters?
               ;; On readline, returns one byte of UTF-8 encoding per call.
               ;; On libedit w/ "widec" support, returns one whole wchar_t per call.
               ;; - option (--enable-widec) since version "0:35:0" (2010-04-24)
               ;; - always enabled since version "0:54:0" (2016-06-18)
               ;; - no known dynamic test to tell whether enabled, so just assume yes
               (cond
                 [(get-ffi-obj "el_wgets" libreadline _fpointer (lambda () #f))
                  ;; libedit (has el_wgets since 2009-12-30)
                  (lambda (_)
                    ;; Racket CS other-thread mode: not currently in a Racket thread
                    (start-nonatomic)
                    (define next-char (read-char/maybe-foreign-thread real-input-port))
                    (end-nonatomic)
                    (if (eof-object? next-char) -1 (char->integer next-char)))]
                 [else
                  ;; libreadline
                  (lambda (_)
                    ;; Racket CS other-thread mode: not currently in a Racket thread
                    (start-nonatomic)
                    (define next-byte (read-byte/maybe-foreign-thread real-input-port))
                    (end-nonatomic)
                    (if (eof-object? next-byte) -1 next-byte))])))


;; force cursor on a new line
(define readline-newline
  (get-ffi-obj "rl_crlf" libreadline (_fun -> _void)
               (lambda ()
                 (get-ffi-obj "rl_newline" libreadline (_fun -> _void)))))

;; force redisplay of prompt and current user input
(define readline-redisplay
  (get-ffi-obj "rl_forced_update_display" libreadline (_fun -> _void)))
