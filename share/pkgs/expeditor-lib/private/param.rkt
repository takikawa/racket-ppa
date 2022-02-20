#lang racket/base
(require racket/fixnum)

;; See "../main.rkt"

;;; parameters

(provide ee-common-identifiers
         ee-default-repeat
         ee-auto-indent
         ee-auto-paren-balance
         ee-flash-parens
         ee-paren-flash-delay
         ee-noisy
         ee-standard-indent
         ee-history-limit
         current-expeditor-lexer
         current-expeditor-reader
         current-expeditor-parentheses
         current-expeditor-post-skipper
         current-expeditor-ready-checker
         current-expeditor-completer
         current-expeditor-grouper
         current-expeditor-history
         current-expeditor-indenter
         current-expeditor-color-enabled
         current-expeditor-history-whitespace-trim-enabled
         current-ee-backward-history-point
         current-ee-forward-history-point)

(define (fxnonnegative? v)
  (and (fixnum? v)
       (fx>= v 0)))

(define ee-common-identifiers
  (make-parameter
   ; general theory: exclude short ids and ids that will come up early
   ; in an alphabetical search with short prefix.  include common ids that
   ; come up annoyingly late in such a search.
   '(append apply call/cc call-with-values define display display-string
            define-syntax define-record null? quote quotient reverse read-char
            substring string-ref string-length string? string=? string-set!
            syntax-case syntax-rules unless vector-ref vector-length vector?
            vector-set! vector)
    (lambda (x)
      (unless (and (list? x) (andmap symbol? x))
        (error 'ee-common-identifiers "~s is not a list of symbols" x))
      x)))

;;; default repeat value for ^U
(define ee-default-repeat
  (make-parameter 4
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-default-repeat "~s is not an integer" x))
      x)))

(define ee-auto-indent (make-parameter #t (lambda (x) (and x #t))))

(define ee-auto-paren-balance (make-parameter #t (lambda (x) (and x #t))))

(define ee-flash-parens (make-parameter #t (lambda (x) (and x #t))))

;;; paren balance delay factor in milliseconds
(define ee-paren-flash-delay
  (make-parameter 100
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-paren-flash-delay "~s is not an integer" x))
      x)))

;;; enable/disable bell
(define ee-noisy (make-parameter #f (lambda (x) (and x #t))))

;;; standard indent length
(define ee-standard-indent
  (make-parameter 2
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-standard-indent "~s is not an integer" x))
      x)))

(define ee-history-limit
  (make-parameter 256
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        (error 'ee-history-length "~s is not a nonnegative fixnum" x))
      x)))

(define current-expeditor-lexer
  (make-parameter (lambda (ip)
                    (define start (add1 (file-position ip)))
                    (define ch (read-char ip))
                    (define end (add1 (file-position ip)))
                    (case ch
                      [(#\( #\) #\[ #\] #\{ #\})
                       (values (string ch) 'parenthesis (string->symbol (string ch)) start end)]
                      [else
                       (if (eof-object? ch)
                           (values eof 'eof #f start end)
                           (values (string ch) 'atomic #f start end))]))
                  (lambda (p)
                    p)))

(define current-expeditor-parentheses
  (make-parameter '((|(| |)|)
                    (|[| |]|)
                    (|{| |}|))
                  (lambda (p)
                    (unless (and (list? p)
                                 (andmap (lambda (v)
                                           (and (list? v)
                                                (= 2 (length v))
                                                (symbol? (car v))
                                                (symbol? (cadr v))))
                                         p))
                      (raise-argument-error 'current-expeditor-parentheses
                                            "(listof (list/c symbol? symbol?))"))
                     p)))

(define current-expeditor-reader
  (make-parameter (lambda (ip)
                    (read ip))
                  (lambda (p)
                    p)))

(define current-expeditor-post-skipper
  (make-parameter (lambda (ip)
                    (let loop ([n 0])
                      (define ch (read-char ip))
                      (cond
                        [(eof-object? ch) n]
                        [(char-whitespace? ch) (loop (add1 n))]
                        [else n])))
                  (lambda (p)
                    p)))

(define current-expeditor-ready-checker
  (make-parameter (lambda (ip)
                    (let loop ([first? #t])
                      (define status
                        (with-handlers ([exn:fail:read:eof? (lambda (exn) #f)]
                                        [exn:fail:read? (lambda (exn) #t)])
                          (define v (read ip))
                          (if (eof-object? v)
                              v
                              #t)))
                      (if (eof-object? status)
                          (not first?)
                          (and status
                               (loop #f)))))
                  (lambda (p)
                    p)))

(define current-expeditor-history
  (make-parameter null))

(define current-expeditor-completer
  (make-parameter (lambda (prefix)
                    (values (namespace-mapped-symbols)
                            (ee-common-identifiers)))))

(define current-expeditor-grouper
  (make-parameter (lambda (obj start limit direction) #t)))


(define current-expeditor-indenter
  (make-parameter (lambda (obj start auto?) #f)))

(define current-expeditor-color-enabled
  (make-parameter #t
                  (lambda (v) (and v #t))))

(define current-expeditor-history-whitespace-trim-enabled
  (make-parameter #t
                  (lambda (v) (and v #t))))

(define (check-point-position who v)
  (unless (memq v '(start top bottom end))
    (raise-argument-error who "(or/c 'start 'top 'bottom 'end)" v))
  v)

(define current-ee-backward-history-point
  (make-parameter 'top
                  (lambda (v)
                    (check-point-position 'current-ee-backward-history-point v))))

(define current-ee-forward-history-point
  (make-parameter 'bottom
                  (lambda (v)
                    (check-point-position 'current-ee-forward-history-point v))))
