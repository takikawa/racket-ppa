; Render markup to text
; private version with everything exported for testing
#lang racket/base
(require racket/contract)
(provide (all-defined-out)) ; specific provides + contracts in non-private module
(require racket/list
         simple-tree-text-markup/data)

(define (listify thing)
  (if (or (null? thing)
          (pair? thing))
      thing
      (list thing)))

(define (block-width block)
  (if (null? block)
      0
      (string-length (car block))))

(define (block-adjust block height)
  (let ((block-height (length block))
        (width (block-width block)))
    (if (< block-height height)
        (let ((height-diff (abs (- height block-height))))
          (append (make-list (quotient height-diff 2) (make-string width #\space))
                  block
                  (make-list (quotient (+ height-diff 1) 2) (make-string width #\space))))
        block)))
      
(define (append-blocks-2 block1 block2)
  (let ((height1 (length block1))
        (height2 (length block2)))
    (let ((height (max height1 height2)))
      (map string-append
           (block-adjust block1 height)
           (block-adjust block2 height)))))

(define (append-blocks . blocks)
  (foldr append-blocks-2 '() blocks))

(define (block-box block)
  (let ((width (block-width block)))
    (append (list (string-append "┌─" (make-string width #\─) "─┐"))
            (map (lambda (line)
                   (string-append "│ " line " │"))
                 block)
            (list (string-append "└─" (make-string width #\─) "─┘")))))

(define (block-display port block)
  (for-each (lambda (line)
              (display line port)
              (newline port))
            block))

(define (pad-to line width)
  (let ((diff (max 0 (- width (string-length line)))))
    (if (zero? diff)
        line
        (string-append line (make-string diff #\space)))))

(define (normalize-lines lines)
  (let ((width (apply max (map string-length lines))))
    (map (lambda (line)
           (pad-to line width))
         lines)))

(define (markup->block markup)
  (cond
    ((string? markup) (list markup))
    ((empty-markup? markup) '())
    ((horizontal-markup? markup)
     (apply append-blocks
            (map markup->block (horizontal-markup-markups markup))))
    ((vertical-markup? markup)
     (normalize-lines
      (append-map markup->block (vertical-markup-markups markup))))
    ((srcloc-markup? markup)
     (markup->block (srcloc-markup-markup markup)))
    ((framed-markup? markup)
     (block-box (markup->block (framed-markup-markup markup))))))

(define display-markup
  (case-lambda
    ((markup) (display-markup markup (current-output-port)))
    ((markup port)
     (block-display port (markup->block markup)))))

