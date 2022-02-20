#lang racket/base
(require racket/class
         racket/contract/base
         "racket-navigation.rkt"
         "private/racket-tabify.rkt"
         "color-textoid.rkt")

(provide
 (contract-out
  [racket-amount-to-indent (->* ((is-a?/c color-textoid<%>)
                                 exact-nonnegative-integer?)
                                (#:head-sexp-type (-> string? (or/c #f 'lambda 'define 'begin 'for/fold 'other))
                                 #:graphical-width (or/c #f (-> (is-a?/c color-textoid<%>)
                                                                exact-nonnegative-integer?
                                                                exact-nonnegative-integer?
                                                                exact-nonnegative-integer?)))
                                (or/c #f exact-nonnegative-integer?))]
  [racket-tabify-table->head-sexp-type (-> (list/c hash?
                                                   (or/c #f regexp?)
                                                   (or/c #f regexp?)
                                                   (or/c #f regexp?)
                                                   (or/c #f regexp?))
                                           (-> string? (or/c #f 'lambda 'define 'begin 'for/fold 'other)))]
  [racket-tabify-default-table (list/c hash? regexp? regexp? regexp? (or/c #f regexp?))]))

(define (racket-amount-to-indent t pos
                                 #:head-sexp-type [get-head-sexp-type
                                                   (racket-tabify-table->head-sexp-type
                                                    racket-tabify-default-table)]
                                 #:graphical-width [graphical-width #f])
  (with-method ([last-position (t last-position)]
                [get-character (t get-character)]
                [get-text (t get-text)]
                [position-paragraph (t position-paragraph)]
                [paragraph-start-position (t paragraph-start-position)]
                [classify-position (t classify-position)]
                [backward-match (t backward-match)]
                [backward-containing-sexp (t backward-containing-sexp)]
                [get-backward-navigation-limit (t get-backward-navigation-limit)])
    (define (find-offset start-pos)
      (define tab-char? #f)
      (define end-pos
        (let loop ([p start-pos])
          (let ([c (get-character p)])
            (cond
              [(char=? c #\tab)
               (set! tab-char? #t)
               (loop (add1 p))]
              [(char=? c #\newline)
               p]
              [(char-whitespace? c)
               (loop (add1 p))]
              [else
               p]))))
      (define gwidth
        (cond
          [graphical-width
           (graphical-width t start-pos end-pos)]
          [else
           ;; if there is no display available, approximate the graphical
           ;; width on the assumption that we are using a fixed-width font
           (- end-pos start-pos)]))
      (values gwidth end-pos tab-char?))

    ;; returns #t if `pos` is in a symbol (or keyword) that consists entirely
    ;; of hyphens and has at least three hyphens; returns #f otherwise
    (define (sexp-is-all-hyphens? pos)
      (define fst-end (racket-forward-sexp t pos))
      (and fst-end
           (let ([fst-start (racket-backward-sexp t fst-end)])
             (and fst-start
                  (memq (classify-position fst-start) '(symbol keyword))
                  (>= (- fst-end fst-start) 3)
                  (let loop ([i fst-start])
                    (cond
                      [(< i fst-end)
                       (and (equal? #\- (get-character i)) (loop (+ i 1)))]
                      [else #t]))))))

    ;; returns #t if `contains' is at a position on a line with an sexp, an ellipsis and nothing else.
    ;; otherwise, returns #f
    (define (second-sexp-is-ellipsis? contains)
      (let ([fst-end (racket-forward-sexp t contains)])
        (and fst-end
             (let ([snd-end (racket-forward-sexp t fst-end)])
               (and snd-end
                    (let ([snd-start (racket-backward-sexp t snd-end)])
                      (and snd-start
                           (equal? (get-text snd-start snd-end)
                                   "...")
                           (let ([thrd-start (racket-forward-sexp t snd-end)])
                             (and (or (not thrd-start)
                                      (not (= (position-paragraph thrd-start)
                                              (position-paragraph snd-start)))))))))))))

    (define (first-sexp-is-keyword? contains)
      (let ([fst-end (racket-forward-sexp t contains)])
        (and fst-end
             (let ([fst-start (racket-backward-sexp t fst-end)])
               (and fst-start
                    (equal? (classify-position fst-start) 'hash-colon-keyword))))))

    (define limit (get-backward-navigation-limit pos))
    (define last-pos (last-position))
    (define para (position-paragraph pos))
    (define is-tabbable?
      (and (> para 0)
           (not (memq (classify-position (- (paragraph-start-position para) 1))
                      '(comment string error)))))
    (define end (if is-tabbable? (paragraph-start-position para) 0))

    ;; "contains" is the start of the initial sub-S-exp
    ;;  in the S-exp that contains "pos". If pos is outside
    ;;  all S-exps, this will be the start of the initial
    ;;  S-exp
    (define contains
      (if is-tabbable?
          (backward-containing-sexp end limit)
          #f))
    (define contain-para (and contains
                              (position-paragraph contains)))

    ;; last is the start of the S-exp just before "pos"
    (define last
      (if contains
          (let ([p (racket-backward-sexp t end)])
            (if (and p (p . >= . limit))
                p
                (backward-match end limit)))
          #f))
    (define last-para (and last (position-paragraph last)))

    ;; last2 is the start of the S-exp just before the one before "pos"
    (define last2
      (if last
          (let ([p (racket-backward-sexp t last)])
            (if (and p (p . >= . limit))
                p
                (backward-match last limit)))
          #f))

    (define (visual-offset pos)
      (let loop ([p (sub1 pos)])
        (if (= p -1)
            0
            (let ([c (get-character p)])
              (cond
                [(char=? c #\null) 0]
                [(char=? c #\tab)
                 (let ([o (loop (sub1 p))])
                   (+ o (- 8 (modulo o 8))))]
                [(char=? c #\newline) 0]
                [else (add1 (loop (sub1 p)))])))))

    (define (get-proc)
      (define id-end (racket-forward-sexp t contains))
      (and (and id-end (> id-end contains))
           (let ([text (get-text contains id-end)])
             (cond
               [(member (classify-position contains) '(keyword symbol))
                (get-head-sexp-type text)]
               [else
                'other]))))
    (define (procedure-indent)
      (case (get-proc)
        [(begin define) 1]
        [(lambda) 3]
        [else 0]))
    (define (define-or-lambda-style?)
      (define proc-name (get-proc))
      (or (equal? proc-name 'define)
          (equal? proc-name 'lambda)))
    (define (for/fold-style?)
      (define proc-name (get-proc))
      (equal? proc-name 'for/fold))

    (define (indent-first-arg start)
      (define-values (gwidth curr-offset tab-char?) (find-offset start))
      gwidth)

    (when (and is-tabbable?
               (not (char=? (get-character (sub1 end))
                            #\newline)))
      ;; Oh no, `insert` is not in `classified-text<%>`...
      ;; but it seems like this shouldn't happen, because
      ;; para > 0, and `end` is the start of the paragraph,
      ;; so the revious character should be a newline
      (send t insert #\newline (paragraph-start-position para)))

    (define amt-to-indent
      (cond
        [(not is-tabbable?)
         (if (= para 0)
             0
             #f)]
        [(let-values ([(gwidth real-start tab-char?) (find-offset end)])
           (and (<= (+ 3 real-start) (last-position))
                (string=? ";;;"
                          (get-text real-start
                                    (+ 2 real-start)))))
         #f]
        [(not contains)
         ;; Something went wrong matching. Should we get here?
         0]
        [(not last)
         ;; We can't find a match backward from pos,
         ;;  but we seem to be inside an S-exp, so
         ;;  go "up" an S-exp, and move forward past
         ;;  the associated paren
         (define enclosing (racket-up-sexp t pos))
         (if enclosing
             (+ (visual-offset enclosing) 1)
             0)]
        [(= contains last)
         ;; this is the first expression in the define
         (+ (visual-offset contains)
            (procedure-indent))]
        [(and (for/fold-style?)
              last2
              (= contains last2))
         (- last (paragraph-start-position last-para))]
        [(or (define-or-lambda-style?)
             (for/fold-style?))
         ;; In case of "define", etc., ignore the position of last
         ;;  and just indent under the "define"
         (add1 (visual-offset contains))]
        [(= contain-para last-para)
         ;; So far, the S-exp containing "pos" was all on
         ;;  one line (possibly not counting the opening paren),
         ;;  so indent to follow the first S-exp's end
         ;;  unless
         ;;    - there are just two sexps earlier and the second is an ellipsis.
         ;;      in that case, we just ignore the ellipsis or
         ;;    - the sexp we are indenting is a bunch of hypens;
         ;;      in that case, we match the opening paren
         (define id-end (racket-forward-sexp t contains))
         (define name-length
           (if id-end
               (- id-end contains)
               0))
         (cond
           [(or (first-sexp-is-keyword? contains)
                (sexp-is-all-hyphens? contains))
            (visual-offset contains)]
           [(second-sexp-is-ellipsis? contains)
            (visual-offset contains)]
           [(sexp-is-all-hyphens? pos)
            (visual-offset contains)]
           [(not (racket-up-sexp t pos))
            (visual-offset contains)]
           [else
            (+ (visual-offset contains)
               name-length
               (indent-first-arg (+ contains
                                    name-length)))])]
        [else
         ;; No particular special case, so indent to match first
         ;; S-expr that starts on the previous line
         (let loop ([last last][last-para last-para])
           (let* ([next-to-last (backward-match last limit)]
                  [next-to-last-para (and next-to-last
                                          (position-paragraph next-to-last))])
             (if (equal? last-para next-to-last-para)
                 (loop next-to-last next-to-last-para)
                 (visual-offset last))))]))
    amt-to-indent))
