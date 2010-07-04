#lang scheme/gui
(require "drscheme-test-util.ss" mzlib/etc framework scheme/string)

(provide test t rx run-test in-here write-test-modules)

;; utilities to use with scribble/reader
(define t string-append)
(define (rx . strs)
  (regexp (regexp-replace* #rx" *\n *" (string-append* strs) ".*")))

(define-struct test (definitions   ; string
                     interactions  ; (union #f string)
                     result        ; string
                     all?          ; boolean (#t => compare all of the text between the 3rd and n-1-st line)
                     error-ranges  ; (or/c 'dont-test
                                   ;       (-> (is-a?/c text)
                                   ;           (is-a?/c text)
                                   ;           (or/c #f (listof ...))))
                                   ;    fn => called with defs & ints, result must match get-error-ranges method's result
                      line)        ; number or #f: the line number of the test case
                      
  #:omit-define-syntaxes)

(define in-here
  (let ([here (this-expression-source-directory)])
    (lambda (file) (format "~s" (path->string (build-path here file))))))

(define tests '())
(define-syntax (test stx)
  (syntax-case stx ()
    [(_  args ...)
     (with-syntax ([line (syntax-line stx)])
       #'(test/proc line args ...))]))
(define (test/proc line definitions interactions results [all? #f] #:error-ranges [error-ranges 'dont-test])
  (set! tests (cons (make-test (if (string? definitions) 
                                   definitions 
                                   (format "~s" definitions))
                               interactions 
                               results 
                               all? 
                               error-ranges
                               line)
                    tests)))

(define temp-files '())
(define (write-test-modules* name code)
  (let ([file (build-path (this-expression-source-directory) (format "~a.ss" name))])
    (set! temp-files (cons file temp-files))
    (with-output-to-file file #:exists 'truncate
      (lambda () (printf "~s\n" code)))))
(define-syntax write-test-modules
  (syntax-rules (module)
    [(_ (module name lang x ...) ...)
     (begin (write-test-modules* 'name '(module name lang x ...)) ...)]))

(define drs (wait-for-drscheme-frame))
(define interactions-text (send drs get-interactions-text))
(define definitions-text (send drs get-definitions-text))

(define (single-test test)
  (let/ec k
    (clear-definitions drs)
    (insert-in-definitions drs (test-definitions test))
    (do-execute drs)

    (let ([ints (test-interactions test)])

      (when ints
        (let ([after-execute-output
               (send interactions-text
                     get-text
                     (send interactions-text paragraph-start-position 2)
                     (send interactions-text paragraph-end-position 2))])
          (unless (or (test-all? test) (string=? "> " after-execute-output))
            (printf "FAILED (line ~a): ~a\n        ~a\n        expected no output after execution, got: ~s\n"
                    (test-line test)
                    (test-definitions test)
                    (or (test-interactions test) 'no-interactions)
                    after-execute-output)
            (k (void)))
          (type-in-interactions drs ints)
          (test:keystroke #\return)
          (wait-for-computation drs)))

      (let* ([text
              (if (test-all? test)
                  (let* ([para (- (send interactions-text position-paragraph
                                        (send interactions-text last-position))
                                  1)])
                    (send interactions-text
                          get-text
                          (send interactions-text paragraph-start-position 2)
                          (send interactions-text paragraph-end-position para)))
                  (let* ([para (- (send interactions-text position-paragraph
                                        (send interactions-text last-position))
                                  1)])
                    (send interactions-text
                          get-text
                          (send interactions-text paragraph-start-position para)
                          (send interactions-text paragraph-end-position para))))]
             [output-passed? (let ([r (test-result test)])
                               ((cond [(string? r) string=?]
                                      [(regexp? r) regexp-match?]
                                      [else 'module-lang-test "bad test value: ~e" r])
                                r text))])
        (unless output-passed?
          (printf "FAILED (line ~a): ~a\n        ~a\n  expected: ~s\n       got: ~s\n"
                  (test-line test)
                  (test-definitions test)
                  (or (test-interactions test) 'no-interactions)
                  (test-result test)
                  text))
        (cond
          [(eq? (test-error-ranges test) 'dont-test)
           (void)]
          [else
           (let ([error-ranges-expected
                  ((test-error-ranges test) definitions-text interactions-text)])
             (unless (equal? error-ranges-expected (send interactions-text get-error-ranges))
               (printf "FAILED (line ~a; ranges): ~a\n  expected: ~s\n       got: ~s\n"
                       (test-line test)
                       (test-definitions test)
                       error-ranges-expected
                       (send interactions-text get-error-ranges))))])))))

(define (run-test)
  (set-language-level! '("Module") #t)
  (for-each single-test (reverse tests))
  (clear-definitions drs)
  (send (send drs get-definitions-text) set-modified #f)
  (for ([file temp-files]) (when (file-exists? file) (delete-file file))))
