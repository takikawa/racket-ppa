#lang racket/base
(require racket/fixnum)

(provide utf-8-encode!
         utf-8-encode-dispatch)

;; Returns (values chars-used bytes-written (or/c 'complete 'continues))
;; where 'continues is the result when the result byte string doesn't
;; have enough room
(define (utf-8-encode! in-str in-start in-end
                       out-bstr out-start out-end)  ; `out-bstr` and `out-end` can be #f no bytes result needed
  ;; Iterate through the given string
  (let loop ([i in-start] [j out-start])
    (cond
     [(fx= i in-end)
      (values (fx- in-end in-start) (fx- j out-start) 'complete)]
     [else
      (define b (char->integer (string-ref in-str i)))
      (define (continue next-j) (loop (fx+ i 1) next-j))
      (utf-8-encode-dispatch b
                             in-start i
                             out-bstr out-start out-end j
                             continue)])))

(define-syntax-rule (utf-8-encode-dispatch b
                                           in-start i
                                           out-bstr out-start out-end j
                                           continue)
  (cond
    [(b . fx<= . #x7F)
     (cond
       [(and out-end (fx= j out-end))
        (values (fx- i in-start) (fx- j out-start) 'continues)]
       [else
        (when out-bstr (bytes-set! out-bstr j b))
        (continue (fx+ j 1))])]
    [(b . fx<= . #x7FF)
     (cond
       [(and out-end ((fx+ j 1) . fx>= . out-end))
        (values (fx- i in-start) (fx- j out-start) 'continues)]
       [else
        (when out-bstr
          (bytes-set! out-bstr j (fxior #b11000000 (fxrshift b 6)))
          (bytes-set! out-bstr (fx+ j 1) (fxior #b10000000 (fxand b #b111111))))
        (continue (+ j 2))])]
    [(b . fx<= . #xFFFF)
     (cond
       [(and out-end ((fx+ j 2) . fx>= . out-end))
        (values (fx- i in-start) (fx- j out-start) 'continues)]
       [else
        (when out-bstr
          (bytes-set! out-bstr j (fxior #b11100000 (fxrshift b 12)))
          (bytes-set! out-bstr (fx+ j 1) (fxior #b10000000 (fxand (fxrshift b 6)
                                                                  #b111111)))
          (bytes-set! out-bstr (fx+ j 2) (fxior #b10000000 (fxand b #b111111))))
        (continue (fx+ j 3))])]
    [else
     (cond
       [(and out-end ((fx+ j 3) . fx>= . out-end))
        (values (fx- i in-start) (fx- j out-start) 'continues)]
       [else
        (when out-bstr
          (bytes-set! out-bstr j (fxior #b11110000 (fxrshift b 18)))
          (bytes-set! out-bstr (fx+ j 1) (fxior #b10000000 (fxand (fxrshift b 12)
                                                                  #b111111)))
          (bytes-set! out-bstr (fx+ j 2) (fxior #b10000000 (fxand (fxrshift b 6)
                                                                  #b111111)))
          (bytes-set! out-bstr (fx+ j 3) (fxior #b10000000 (fxand b #b111111))))
        (continue (fx+ j 4))])]))
