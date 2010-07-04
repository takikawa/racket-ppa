(module pread mzscheme
  (require (lib "readline.ss" "readline") (lib "file.ss"))

  ;; configuration
  (define current-prompt   (make-parameter #"> "))
  (define show-all-prompts (make-parameter #t))
  (define max-history      (make-parameter 100))
  (define keep-duplicates  (make-parameter #f))
  (define keep-blanks      (make-parameter #f))
  (provide current-prompt show-all-prompts
           max-history keep-duplicates keep-blanks)

  ;; History management

  (define local-history
    (let ([hist (get-preference 'readline-input-history (lambda () null))])
      (for-each add-history hist)
      (reverse hist)))

  (define (save-history)
    (put-preferences '(readline-input-history) (list (reverse local-history))))

  (define (readline-bytes/hist p)
    (let ([s (readline-bytes p)])
      (when (and (bytes? s)
                 (or (keep-blanks) (not (zero? (bytes-length s))))
                 (or (keep-duplicates)
                     (null? local-history)
                     (not (equal? s (car local-history)))))
        (add-history-bytes s)
        (set! local-history (cons s local-history))
        (let loop ([n (max-history)] [l local-history])
          (cond [(null? l) 'done]
                [(zero? n) (set-cdr! l '())]
                [else (loop (sub1 n) (cdr l))])))
      s))

  (exit-handler
   (let ([old (exit-handler)])
     (lambda (v) (save-history) (old v))))

  ;; implement an input port that goes through readline

  ;; readline-prompt can be
  ;;   #f: no prompt (normal state),
  ;;   bytes: a prompt to use
  ;;   'space: a port has been used, now use spaces instead
  ;;           (from readline-prompt-spaces)
  (provide readline-prompt)
  (define readline-prompt (make-parameter #f))
  (define readline-prompt-spaces (make-parameter #"  "))

  (define (get-current-prompt)
    (let ([p (readline-prompt)])
      (case p
        [(#f) #""]
        [(space) (readline-prompt-spaces)]
        [else (unless (= (bytes-length (readline-prompt-spaces))
                         (bytes-length p))
                (readline-prompt-spaces (make-bytes (bytes-length p) 32)))
              (readline-prompt 'space) ; use spaces next time
              p])))

  (provide readline-input)
  (define readline-input
    (let ([buffer  #f]
          [skip    #f]
          [blen    #f]
          [LF      (bytes-ref #"\n" 0)])
      (define (close!) (set! buffer eof) (save-history))
      (define (reader tgt)
        (let loop ()
          (cond [(eof-object? buffer) eof]
                [(not buffer)
                 (set! buffer (readline-bytes/hist (get-current-prompt)))
                 (if (eof-object? buffer)
                   (begin (save-history) buffer)
                   (begin (set! skip 0)
                          (set! blen (bytes-length buffer))
                          (reader tgt)))]
                [else
                 ;; copy bytes
                 (let ([tgtlen (bytes-length tgt)]
                       [left (- blen skip)])
                   (cond [(< tgtlen left) ; not enough target space
                          (let ([end (+ skip tgtlen)])
                            (bytes-copy! tgt 0 buffer skip end)
                            (set! skip end)
                            tgtlen)]
                         [(= tgtlen left) ; enough room for text but no newline
                          (bytes-copy! tgt 0 buffer skip blen)
                          (set! skip blen)
                          left]
                         [else ; enough room for text with newline
                          (bytes-copy! tgt 0 buffer skip blen)
                          (bytes-set! tgt left LF)
                          (set! buffer #f)
                          (add1 left)]))])))
      (make-input-port 'readline reader #f close!)))

  ;; like read-syntax, but waits until valid input is ready
  (define read-complete-syntax
    (let ([leftovers '()]
          [counter 1])
      (lambda ()
        (if (pair? leftovers)
          (begin0 (car leftovers)
            (set! leftovers (cdr leftovers)))
          (let big-loop ()
            (let loop ([s (read-line)] [next-pos 1] [force? #f])
              (if (eof-object? s)
                (begin (save-history) s)
                (with-handlers ([(if force? (lambda (x) #f) exn:fail:read:eof?)
                                 (lambda (exn)
                                   (let ([v (read-line)])
                                     (loop (string-append
                                            s "\n" (if (eof-object? v) "" v))
                                           (add1 next-pos)
                                           (eof-object? v))))])
                  (let ([p (open-input-string (string-append s "\n"))])
                    (port-count-lines! p)
                    (let ([rs (let loop ()
                                (let ([r (read-syntax
                                          (string->symbol
                                           (format "repl-~a" counter))
                                          p)])
                                  (if (eof-object? r) '() (cons r (loop)))))])
                      (if (null? rs)
                        (big-loop)
                        (begin0 (car rs)
                          (set! counter (add1 counter))
                          (set! leftovers (cdr rs))))))))))))))

  ;; a function that can be used for current-prompt-read
  (provide read-cmdline-syntax)
  (define (read-cmdline-syntax)
    (define prompt (current-prompt))
    ;; needs to set `readline-prompt' to get a prompt when reading
    (parameterize ([read-accept-reader #t]
                   [readline-prompt prompt])
      (unless (eq? readline-input (current-input-port))
        ;; not the readline port -- print the prompt (changing the
        ;; readline-prompt and using read-complete-syntax below should still
        ;; work fine)
        (display prompt) (flush-output))
      (if (show-all-prompts) (read-syntax) (read-complete-syntax))))

  )
