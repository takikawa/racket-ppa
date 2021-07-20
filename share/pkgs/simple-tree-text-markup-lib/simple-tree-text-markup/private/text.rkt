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
     (block-box (markup->block (framed-markup-markup markup))))
    ((image-markup? markup)
     (markup->block (image-markup-alt-markup markup)))
    ((number-markup? markup)
     (list (number-markup->string (number-markup-number markup)
                                  #:exact-prefix (number-markup-exact-prefix markup)
                                  #:inexact-prefix (number-markup-inexact-prefix markup)
                                  #:fraction-view (number-markup-fraction-view markup))))))

(define (number-markup->string number
                               #:exact-prefix [exact-prefix 'never]
                               #:inexact-prefix [inexact-prefix 'never]
                               #:fraction-view [fraction-view #f])
  (cond
    [(inexact? number)
     (string-append
      (if (eq? inexact-prefix 'always) "#i" "")
      (number->decimal-string number ""))]
    [(eq? fraction-view 'decimal)
     (string-append
      (if (eq? exact-prefix 'always) "#e" "")
      (number->decimal-string number (if (eq? exact-prefix 'when-necessary) "#e" "")))]
    [else
     (string-append
      (if (eq? exact-prefix 'always) "#e" "")
      (number->string number))]))

;; stolen from racket/pretty, but can't use because we're sitting inside a pretty-print-print-hook probably
(define (number->decimal-string x exact-prefix)
  (cond
    [(or (inexact? x)
         (integer? x))
     (number->string x)]
    [(not (real? x))
     (let ([r (real-part x)]
           [i (imag-part x)])
       (format "~a~a~ai"
               (number->decimal-string r "")
               (if (negative? i)
                   ""
                   "+")
               (number->decimal-string i "")))]
    [else
     (let ([n (numerator x)]
           [d (denominator x)])
       ;; Count powers of 2 in denomintor
       (let loop ([v d][2-power 0])
         (if (and (positive? v)
                  (even? v))
             (loop (arithmetic-shift v -1) (add1 2-power))
             ;; Count powers of 5 in denominator
             (let loop ([v v][5-power 0])
               (if (zero? (remainder v 5))
                   (loop (quotient v 5) (add1 5-power))
                   ;; No more 2s or 5s. Anything left?
                   (if (= v 1)
                       ;; Denominator = (* (expt 2 2-power) (expt 5 5-power)).
                       ;; Print number as decimal.
                       (let* ([10-power (max 2-power 5-power)]
                              [scale (* (expt 2 (- 10-power 2-power))
                                        (expt 5 (- 10-power 5-power)))]
                              [s (number->string (* (abs n) scale))]
                              [orig-len (string-length s)]
                              [len (max (add1 10-power) orig-len)]
                              [padded-s (if (< orig-len len)
                                            (string-append
                                             (make-string (- len orig-len) #\0)
                                             s)
                                            s)])
                         (format "~a~a~a.~a"
                                 exact-prefix
                                 (if (negative? n) "-" "")
                                 (substring padded-s 0 (- len 10-power))
                                 (substring padded-s (- len 10-power) len)))
                       ;; d has factor(s) other than 2 and 5.
                       ;; Print as a fraction.
                       (number->string x)))))))]))

(define display-markup
  (case-lambda
    ((markup) (display-markup markup (current-output-port)))
    ((markup port)
     (block-display port (markup->block markup)))))
