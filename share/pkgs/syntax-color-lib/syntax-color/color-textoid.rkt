#lang racket/base
(require racket/class)

(provide color-textoid<%>)

(define color-textoid<%>
  (interface ()
    get-text
    get-character

    last-position
    position-paragraph
    paragraph-start-position
    paragraph-end-position
    
    skip-whitespace
    backward-match
    backward-containing-sexp
    forward-match
    classify-position
    classify-position*
    get-token-range
    get-backward-navigation-limit
    get-regions))
