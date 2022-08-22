#lang racket/base
(require racket/contract/base
         racket/contract/option)
(provide lexer/c
         lexer*/c
         lexer*/c-without-random-testing
         (struct-out dont-stop)
         (contract-out
          [check-colorer-results-match-port-before-and-after
           (-> symbol? any/c
               (or/c exact-positive-integer? #f) (or/c exact-positive-integer? #f)
               (or/c exact-positive-integer? #f) (or/c exact-positive-integer? #f)
               void?)]))

(struct dont-stop (val) #:transparent)

(define lexer/c
  (option/c
   (or/c (->i ([in (and/c input-port? port-counts-lines?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]))
         (->i ([in (and/c input-port? port-counts-lines?)]
               [offset exact-nonnegative-integer?]
               [mode (not/c dont-stop?)])
              (values [txt any/c]
                      [type symbol?]
                      [paren (or/c symbol? #f)]
                      [start (or/c exact-positive-integer? #f)]
                      [end (start type) (end/c start type)]
                      [backup exact-nonnegative-integer?]
                      [new-mode any/c])))
   #:tester (λ (lexer) (try-some-random-streams lexer))))

(define lexer*/c-without-option
  (or/c (->i ([in (and/c input-port? port-counts-lines?)])
             (values [txt any/c]
                     [type (or/c symbol? (hash/c symbol? any/c #:immutable #t))]
                     [paren (or/c symbol? #f)]
                     [start (or/c exact-positive-integer? #f)]
                     [end (start type) (end/c start type)]))
        (->i ([in (and/c input-port? port-counts-lines?)]
              [offset exact-nonnegative-integer?]
              [mode (not/c dont-stop?)])
             (values [txt any/c]
                     [type (or/c symbol? (hash/c symbol? any/c #:immutable #t))]
                     [paren (or/c symbol? #f)]
                     [start (or/c exact-positive-integer? #f)]
                     [end (start type) (end/c start type)]
                     [backup exact-nonnegative-integer?]
                     [new-mode any/c]))))

(define lexer*/c-without-random-testing
  (option/c
   lexer*/c-without-option))

(define lexer*/c
  (option/c
   lexer*/c-without-option
   #:tester (λ (lexer) (try-some-random-streams lexer))))

(define (try-some-random-streams lexer)
  (define 3ary-lexer
    (cond
      [(procedure-arity-includes? lexer 1)
       (λ (in offset mode)
         (define-values (txt type paren start end) (lexer in))
         (values txt type paren start end 0 #f))]
      [else lexer]))
  (define initial-state (pseudo-random-generator->vector
                         (current-pseudo-random-generator)))
  (define latest-input-string #f)
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (raise
                      (make-exn:fail
                       (format (string-append "try-some-random-streams:"
                                              " random testing of lexer failed\n"
                                              "  lexer: ~e\n"
                                              "  pseudo-random state: ~s\n"
                                              "  latest input string: ~s\n"
                                              "  error message: ~s")
                               lexer
                               initial-state
                               latest-input-string
                               (exn-message exn))
                       (exn-continuation-marks exn))))])
    (for ([x (in-range 10)])
      (define size (random 100))

      (define opens '())
      (define (update-opens c)
        (define (update-open c) (set! opens (cons c opens)))
        (case c
          [(#\") (update-open #\")]
          [(#\|) (update-open #\|)]
          [(#\() (update-open #\))]
          [(#\[) (update-open #\])]
          [(#\{) (update-open #\})])
        c)

      (define (quash-backslash-r c)
        ;; it isn't clear the spec is right in
        ;; the case of \r\n combinations, so we
        ;; punt for now
        (if (equal? c #\return) #\newline c))

      (define (char-at-random)
        (update-opens
         (quash-backslash-r
          (case (random 3)
            [(0)
             (define s " ()@{}\"λΣ\0|")
             (string-ref s (random (string-length s)))]
            [(1 2)
             (integer->char (random 255))]))))

      (define (pick-a-char)
        (cond
          [(null? opens)
           (char-at-random)]
          [else
           (case (random 4)
             [(0)
              (begin0 (car opens)
                      (set! opens (cdr opens)))]
             [else (char-at-random)])]))

      (define s (build-string size (λ (c) (pick-a-char))))
      (set! latest-input-string s)
      (define in (open-input-string s))
      (port-count-lines! in)
      (let loop ([mode #f][offset 0])
        (define-values (txt type paren start end backup new-mode)
          (3ary-lexer in offset mode))
        (cond
         [(equal? type 'eof) #t]
         [(< end size) (loop new-mode end)]
         [else #f])))))

(define (end/c start type)
  (cond
    [(equal? 'eof type) 
     (or/c exact-positive-integer? #f)]
    [start
     (and/c exact-positive-integer?
            (>/c start))]
    [else
     #f]))

(define (check-colorer-results-match-port-before-and-after
         who type pos-before new-token-start new-token-end pos-after)
  (unless (equal? 'eof type)
    (unless (<= pos-before new-token-start pos-after)
      (error who
             "expected the token start to be between ~s and ~s, got ~s"
             pos-before pos-after new-token-start))
    (unless (<= pos-before new-token-end pos-after)
      (error who
             "expected the token end to be between ~s and ~s, got ~s"
             pos-before pos-after new-token-end))))
