#lang scheme/base
(require "../struct.ss"
         "../decode.ss"
         "../basic.ss"
         scheme/list)

(provide spacer doc-prefix
         to-flow
         flow-spacer flow-empty-line
         make-table-if-necessary
         max-proto-width)

(define spacer (hspace 1))

(define (doc-prefix doc s)
  (if doc (list (module-path-prefix->string doc) s) s))

(define (to-flow e)
  (make-flow (list (make-omitable-paragraph (list e)))))
(define flow-spacer (to-flow spacer))
(define flow-empty-line (to-flow (tt 'nbsp)))

(define (make-table-if-necessary style content)
  (if (= 1 (length content))
    (let ([paras (append-map flow-paragraphs (car content))])
      (if (andmap paragraph? paras)
        (list (make-omitable-paragraph (append-map paragraph-content paras)))
        (list (make-table style content))))
    (list (make-table style content))))

(define max-proto-width 65)
