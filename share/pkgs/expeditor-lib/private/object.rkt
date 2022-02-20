#lang racket/base
(require racket/class
         syntax-color/color-textoid
         "param.rkt")

(provide new-object)

(define (new-object s)
  (new like-text% [content s]))

(struct token (type paren start end))

(define (lex-all ip)
  (define lex (current-expeditor-lexer))
  (let loop ([state #f])
    (define-values (lexeme type paren start end backup new-state)
      (if (procedure-arity-includes? lex 3)
          (lex ip 0 state)
          (let-values ([(lexeme type paren start end) (lex ip)])
            (values lexeme type paren start end 0 state))))
    (cond
      [(eq? type 'eof) '()]
      [else (cons (token type paren (sub1 start) (sub1 end))
                  (loop new-state))])))

(define like-text%
  (class* object% (color-textoid<%>)
    (init-field content)

    (super-new)

    (define-values (position-paragraphs paragraph-starts)
      (let loop ([pos 0] [para 0] [pos-para #hasheqv()] [para-pos #hasheqv((0 . 0))])
        (cond
          [(= pos (string-length content))
           (values (hash-set pos-para pos para) para-pos)]
          [(char=? #\newline (string-ref content pos))
           (loop (add1 pos) (add1 para)
                 (hash-set pos-para pos para)
                 (hash-set para-pos (add1 para) (add1 pos)))]
          [else
           (loop (add1 pos) para (hash-set pos-para pos para) para-pos)])))
    
    ;; position -> token
    (define mapping
      (let ([tokens (lex-all (let ([p (open-input-string content)])
                               (port-count-lines! p)
                               p))])
        (let loop ([tokens tokens] [pos 0] [mapping #hasheqv()])
          (cond
            [(null? tokens) mapping]
            [else
             (define t (car tokens))
             (unless (= pos (token-start t))
               (error 'editor "token discontinuity ~s vs. ~s" pos (token-start t)))
             (loop (cdr tokens)
                   (token-end t)
                   (for/fold ([mapping mapping]) ([i (in-range (- (token-end t) (token-start t)))])
                     (hash-set mapping (+ pos i) t)))]))))

    (define/public (get-text s e)
      (substring content s e))

    (define/public (get-character s)
      (if (s . >= . (string-length content))
          #\nul
          (string-ref content s)))

    (define/private (get-token who pos)
      (or (hash-ref mapping pos #f)
          (hash-ref mapping (sub1 pos) #f) ; make end position work
          (error who "lookup failed: ~e" pos)))

    (define/public (classify-position* pos)
      (define t (get-token 'classify-position pos))
      (token-type t))

    (define/public (classify-position pos)
      (define attribs (classify-position* pos))
      (if (symbol? attribs)
          attribs
          (hash-ref attribs 'type 'unknown)))

    (define/public (get-token-range pos)
      (define t (hash-ref mapping pos #f))
      (if t
          (values (token-start t)
                  (token-end t))
          (values #f #f)))

    (define/private (get-paren pos)
      (define t (get-token 'get-paren pos))
      (token-paren t))

    (define/public (last-position)
      (string-length content))

    (define/public (position-paragraph pos [eol? #f])
      (or (hash-ref position-paragraphs pos #f)
          (error 'position-paragraph "lookup failed: ~e" pos)))
    
    (define/public (paragraph-start-position para)
      (or (hash-ref paragraph-starts para #f)
          (error 'paragraph-start-position "lookup failed: ~e" para)))

    (define/public (paragraph-end-position para)
      (define n (hash-ref paragraph-starts (add1 para) #f))
      (if n
          (sub1 n)
          (last-position)))

    (define/public (backward-match pos cutoff)
      (backward-matching-search pos cutoff 'one))

    (define/public (backward-containing-sexp pos cutoff)
      (backward-matching-search pos cutoff 'all))


    (define/private (backward-matching-search init-pos cutoff mode)
      (define start-pos (if (and (eq? mode 'all)
                                 (init-pos . <= . cutoff))
                            cutoff
                            (sub1 init-pos)))
      (let loop ([pos start-pos] [depth (if (eq? mode 'one) -1 0)] [need-close? (eq? mode 'one)])
        (cond
          [(pos . < . cutoff) #f]
          [else
           (define-values (s e) (get-token-range pos))
           (define (atom)
             (if need-close?
                 s
                 (loop (sub1 s) depth #f)))
           (define sym (get-paren s))
           (cond
             [sym
              (let paren-loop ([parens (current-expeditor-parentheses)])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
                  [(eq? sym (caar parens))
                   (and (not need-close?)
                        (if (= depth 0)
                            (cond
                              [(eq? mode 'all)
                               ;; color:text% method skips back over whitespace, but
                               ;; doesn't go beyond the starting position
                               (min (skip-whitespace e 'forward #f)
                                    init-pos)]
                              [else s])
                            (loop (sub1 s) (sub1 depth) #f)))]
                  [(eq? sym (cadar parens))
                   (cond
                     [(e . > . init-pos)
                      ;; started in middle of closer
                      (if (eq? mode 'one)
                          s
                          (loop (sub1 s) depth #f))]
                     [else (loop (sub1 s) (add1 depth) #f)])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment)
                 (loop (sub1 s) depth need-close?)]
                [else (atom)])])])))

    (define/public (forward-match pos cutoff)
      (let loop ([pos pos] [depth 0])
        (define-values (s e) (get-token-range pos))
        (cond
          [(not s) #f]
          [else
           (define sym (get-paren s))
           (define (atom)
             (if (zero? depth)
                 e ;; didn't find paren to match, so finding token end
                 (loop e depth)))
           (cond
             [sym
              (let paren-loop ([parens (current-expeditor-parentheses)])
                (cond
                  [(null? parens)
                   ;; treat an unrecognized parenthesis like an atom
                   (atom)]
                  [(eq? sym (caar parens))
                   (if (eqv? pos s) ; don't count the middle of a parenthesis token
                       (loop e (add1 depth))
                       e)]
                  [(eq? sym (cadar parens))
                   (cond
                     [(depth . <= . 0) #f]
                     [(depth . = . 1) e]
                     [else (loop e (sub1 depth))])]
                  [else
                   (paren-loop (cdr parens))]))]
             [else
              (define category (classify-position pos))
              (case category
                [(white-space comment) (loop e depth)]
                [else (atom)])])])))

    (define/public (skip-whitespace pos dir comments?)
      (define (skip? category)
        (or (eq? category 'white-space)
            (and comments? (eq? category 'comment))))
      (case dir
        [(forward)
         (let loop ([pos pos])
           (define category (classify-position pos))
           (cond
             [(skip? category)
              (define-values (s e) (get-token-range pos))
              (if e
                  (loop e)
                  pos)]
             [else pos]))]
        [(backward)
         (cond
           [(zero? pos) 0]
           [else
            (let loop ([pos (sub1 pos)] [end-pos pos])
              (define category (classify-position pos))
              (cond
                [(skip? category)
                 (define-values (s e) (get-token-range pos))
                 (loop (sub1 s) s)]
                [else end-pos]))])]
        [else
         (error 'skip-whitespace "bad direction: ~e" dir)]))

    (define/public (get-backward-navigation-limit pos) 0)

    (define/public (get-regions) '((0 end)))))
