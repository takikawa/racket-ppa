;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/

(module k-nucleotide mzscheme
  (require (lib "list.ss")
           (lib "string.ss")
           (only (lib "13.ss" "srfi") string-pad-right))

  (define (all-counts len dna)
    (let ([table (make-hash-table)]
          [seq (make-string len)])
      (let loop ([s (- (string-length dna) len)])
        (string-copy! seq 0 dna s (+ s len))
        (let ([key (string->symbol seq)])
          (let ([cnt (hash-table-get table key 0)])
            (hash-table-put! table key (add1 cnt))))
        (unless (zero? s)
          (loop (sub1 s))))
      table))

  (define (write-freqs table)
    (let* ([content (hash-table-map table cons)]
           [total (exact->inexact (apply + (map cdr content)))])
      (for-each
       (lambda (a)
         (printf "~a ~a\n" 
                 (car a) 
                 (real->decimal-string (* 100 (/ (cdr a) total)) 3)))
       (sort content (lambda (a b) (> (cdr a) (cdr b)))))))

  (define (write-one-freq table key)
    (let ([cnt (hash-table-get table key 0)])
      (printf "~a\t~a\n" cnt key)))

  (define dna
    (begin
      ;; Skip to ">THREE ..."
      (regexp-match #rx#"(?m:^>THREE.*$)" (current-input-port))
      (let ([s (open-output-string)])
        ;; Copy everything but newlines to s:
        (let loop ()
          (when (regexp-match #rx#"\n" (current-input-port) 0 #f s)
            (loop)))
        ;; Extract the string from s:
        (string-upcase (get-output-string s)))))

  ;; 1-nucleotide counts:
  (write-freqs (all-counts 1 dna))
  (newline)

  ;; 2-nucleotide counts:
  (write-freqs (all-counts 2 dna))
  (newline)

  ;; Specific sequences:
  (for-each (lambda (seq)
              (write-one-freq (all-counts (string-length seq) dna)
                              (string->symbol seq)))
            '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT"))
  
  )

  
  