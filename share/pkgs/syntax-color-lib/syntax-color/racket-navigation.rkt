#lang racket/base
(require racket/class
         racket/contract/base
         racket/string
         "color-textoid.rkt")

(provide
 (contract-out [racket-forward-sexp
                (-> (is-a?/c color-textoid<%>)
                    exact-nonnegative-integer?
                    (or/c #f exact-nonnegative-integer?))]
               [racket-backward-sexp
                (->* ((is-a?/c color-textoid<%>)
                      exact-nonnegative-integer?)
                     (#:limit exact-nonnegative-integer?)
                    (or/c #f exact-nonnegative-integer?))]
               [racket-up-sexp
                (->* ((is-a?/c color-textoid<%>)
                      exact-nonnegative-integer?)
                     (#:limit exact-nonnegative-integer?)
                     (or/c #f exact-nonnegative-integer?))]
               [racket-down-sexp
                (-> (is-a?/c color-textoid<%>)
                    exact-nonnegative-integer?
                    (or/c #f exact-nonnegative-integer?))]

               [racket-grouping-position
                (-> (is-a?/c color-textoid<%>)
                    exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    (or/c 'up 'down 'backward 'forward)
                    (or/c #f exact-nonnegative-integer?))]

               [racket-stick-to-next-sexp?
                (-> (is-a?/c color-textoid<%>)
                    exact-nonnegative-integer?
                    boolean?)]))

(define (racket-forward-sexp t start-pos)
  ;; loop to work properly with quote, etc.
  (let loop ([one-forward (send t forward-match start-pos (send t last-position))])
    (cond
      [(and one-forward (not (= 0 one-forward)))
       (let ([bw (send t backward-match one-forward 0)])
         (cond
           [(and bw
                 (racket-stick-to-next-sexp? t bw))
            (let ([two-forward (send t forward-match one-forward (send t last-position))])
              (if two-forward
                  (loop two-forward)
                  one-forward))]
           [else
            one-forward]))]
      [else one-forward])))

(define (racket-backward-sexp t start-pos
                              #:limit [limit (send t get-backward-navigation-limit start-pos)])
  (let* ([end-pos (send t backward-match start-pos limit)]
         [min-pos (send t backward-containing-sexp start-pos limit)])
    (if (and end-pos
             (or (not min-pos)
                 (end-pos . >= . min-pos)))
        ;; Can go backward, but check for preceding quote, unquote, etc.
        (let loop ([end-pos end-pos])
          (let ([next-end-pos (send t backward-match end-pos limit)])
            (if (and next-end-pos
                     (or (not min-pos)
                         (end-pos . >= . min-pos))
                     (racket-stick-to-next-sexp? t next-end-pos))
                (loop next-end-pos)
                end-pos)))
        ;; can't go backward at all:
        #f)))

(define (racket-up-sexp t start-pos
                        #:limit [limit (send t get-backward-navigation-limit start-pos)])
  (define exp-pos (send t backward-containing-sexp start-pos limit))
  (cond
    [exp-pos
     (define in-start-pos (send t skip-whitespace exp-pos 'backward #t))
     (define-values (s e) (send t get-token-range (sub1 in-start-pos)))
     s]
    [else #f]))

(define (racket-down-sexp t start-pos)
  (let loop ([pos start-pos])
    (let ([next-pos (racket-forward-sexp t pos)])
      (if (and next-pos (> next-pos pos))
          (let ([back-pos
                 (send t backward-containing-sexp (sub1 next-pos) pos)])
            (if (and back-pos
                     (> back-pos pos))
                back-pos
                (loop next-pos)))
          #f))))

;; ----------------------------------------

(define (racket-grouping-position t start-pos limit mode)
  (case mode
    [(up)       (racket-up-sexp t start-pos #:limit limit)]
    [(down)     (racket-down-sexp t start-pos)]
    [(backward) (racket-backward-sexp t start-pos #:limit limit)]
    [(forward)  (racket-forward-sexp t start-pos)]))

;; ----------------------------------------

;; stick-to-next-sexp?: natural -> boolean
(define stick-to-patterns
  '("'" "," ",@" "`" "#'" "#," "#`" "#,@"
        "#&" "#;" "#hash" "#hasheq" "#ci" "#cs"))

(define stick-to-patterns-union
  (regexp (string-append
           "^("
           (string-join (map regexp-quote stick-to-patterns) "|")
           ")")))

(define stick-to-patterns-union-anchored
  (regexp (string-append
           "^("
           (string-join (map regexp-quote stick-to-patterns) "|")
           ")$")))

(define stick-to-max-pattern-length
  (apply max (map string-length stick-to-patterns)))

(define (racket-stick-to-next-sexp? t start-pos)
  ;; Optimization: speculatively check whether the string will
  ;; match the patterns; at time of writing, forward-match can be
  ;; really expensive.
  (define snippet
    (send t get-text
          start-pos
          (min (send t last-position)
               (+ start-pos stick-to-max-pattern-length))))
  (and (regexp-match? stick-to-patterns-union snippet)
       (let ([end-pos (send t forward-match start-pos (send t last-position))])
         (and end-pos
              (regexp-match? stick-to-patterns-union-anchored
                             (send t get-text start-pos end-pos))))))
