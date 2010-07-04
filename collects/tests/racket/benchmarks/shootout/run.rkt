(module run mzscheme
  (require (only scheme/runtime-path define-runtime-path))
  (define input-map
    `(
      ("ackermann.rkt" "11")
      ("ary.rkt" "9000")
      ("binarytrees.rkt" "16")
      ("chameneos.rkt" "1000000")
      ("cheapconcurrency.rkt" "15000")
      ("echo.rkt" "150000")
      ("except.rkt" "2500000")
      ("fannkuch.rkt" "10")
      ("fasta.rkt" "25000000")
      ("fibo.rkt" "32")
      ("hash.rkt" "100000")
      ("hash2.rkt" "200")
      ("heapsort.rkt" "100000")
      ("lists.rkt" "18")
      ("mandelbrot.rkt" "3000")
      ("matrix.rkt" "600")
      ("moments.rkt") ; 200 somethings...
      ("nbody.rkt" "20000000")
      ("nestedloop.rkt" "18")
      ("nsieve.rkt" "9")
      ("nsievebits.rkt" "11")
      ("partialsums.rkt" "2500000")
      ("pidigits.rkt" "2500")
      ("pidigits1.rkt")
      ("random.rkt" "900000")
      ("recursive.rkt" "11")
      ("regexmatch.rkt")
      ("regexpdna.rkt" #f ,(lambda () (mk-regexpdna-input)))
      ("reversecomplement.rkt" #f ,(lambda () (mk-revcomp-input)))
      ("k-nucleotide.rkt" #f ,(lambda () (mk-knuc-input)))
      ("reversefile.rkt")
      ("sieve.rkt" "1200")
      ("spellcheck.rkt")
      ("spectralnorm.rkt" "5500")
      ("spectralnorm-unsafe.rkt" "5500")
      ("strcat.rkt" "40000")
      ("sumcol.rkt" #f ,(lambda () (mk-sumcol-input)))
      ("wc.rkt")
      ("wordfreq.rkt")
      ))

  (define-runtime-path here ".")

  (define (dynreq f)
    (parameterize ([current-load-relative-directory here])
      (dynamic-require f #f)))

  (define (mk-fasta n suffix)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "fasta-" suffix))])
      (unless (file-exists? f)
        (printf "Building FASTA ~a output for input: ~a\n" n f)
        (with-output-to-file f
          (lambda ()
            (parameterize ([current-command-line-arguments (vector n)])
              (dynreq "fasta.rkt")))))
      f))

  (define (mk-revcomp-input)
    (mk-fasta "2500000" "2m5"))

  (define (mk-knuc-input)
    (mk-fasta "1000000" "1m"))

  (define (mk-regexpdna-input)
    (mk-fasta "5000000" "5m"))

  (define (mk-sumcol-input)
    (let ([f (build-path (find-system-path 'temp-dir) "sumcol-21k")])
      (unless (file-exists? f)
        (printf "Building sumcol 21000 input: ~a\n" f)
        (let ([c (with-input-from-file (build-path (collection-path "tests")
                                                   "mzscheme"
                                                   "benchmarks"
                                                   "shootout"
                                                   "sumcol-input.txt")
                   (lambda ()
                     (read-bytes 10000)))])
          (with-output-to-file f
            (lambda ()
              (let loop ([n 21000])
                (unless (zero? n)
                  (printf "~a" c)
                  (loop (sub1 n))))))))
      f))

  (define iters
    (let ([len (vector-length (current-command-line-arguments))])
      (unless (<= 1 len 2)
        (printf "provide ~athe name of a benchmark on the command line and an optional iteration count\n"
                (if (zero? len) "" "ONLY "))
        (exit))
      (if (= len 2)
          (string->number (vector-ref (current-command-line-arguments) 1))
          1)))
      
  (let ([prog (vector-ref (current-command-line-arguments) 0)])
    (let ([m (assoc prog input-map)])
      (unless m
        (error 'run "cannot find input for ~a" prog))
      (when (null? (cdr m))
        (error 'run "don't know input for ~a" prog))
      (let loop ([n iters])
        (parameterize ([current-command-line-arguments 
                        (if (cadr m)
                            (vector (cadr m))
                            (vector))]
                       [current-input-port
                        (if (null? (cddr m))
                            (current-input-port)
                            (open-input-file ((caddr m))))])
          (parameterize ([current-namespace (make-namespace)])
            (collect-garbage)
            (time (dynreq prog))))
        (unless (= n 1)
          (loop (sub1 n)))))))
