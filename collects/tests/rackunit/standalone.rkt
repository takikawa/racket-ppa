#lang racket/base
(require racket/runtime-path
         rackunit
         racket/path)

(define-runtime-path here ".")
(define collects 
  (normalize-path (build-path here ".." "..")))
(define (collect-trim bs)
  (regexp-replace* (regexp-quote (path->bytes collects)) bs #"PLTHOME/collects"))
  
(define (require&catch path)
  (define out-bs (open-output-bytes))
  (define err-bs (open-output-bytes))
  (parameterize ([current-output-port out-bs]
                 [current-error-port err-bs])
    (dynamic-require path #f))
  (close-output-port out-bs)
  (close-output-port err-bs)
  (values (collect-trim (get-output-bytes out-bs))
          (collect-trim (get-output-bytes err-bs))))

(define-syntax-rule (test-file pth out err)
  (begin
    (define-runtime-module-path mod (file pth))
    (define-values (cout cerr) (require&catch mod))
    (check-equal? cout out)
    (check-equal? cerr err)))

(test-file "standalone-check-test.rkt"
           #"Oh HAI!\nI didn't run\n"
           #"--------------------\nERROR\nOutta here!\n\n === context ===\nPLTHOME/collects/tests/rackunit/standalone-check-test.rkt:40:12: temp7\nPLTHOME/collects/rackunit/private/check.rkt:122:29\nPLTHOME/collects/racket/private/more-scheme.rkt:209:2: call-handled-body\nPLTHOME/collects/rackunit/private/check.rkt:55:0: top-level-check-around\n\n\n--------------------\n--------------------\nFAILURE\nname:       check\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-check-test.rkt> 44 0 1344 17)\nexpression: (check = 1 2)\nparams:     (#<procedure:=> 1 2)\nmessage:    0.0\n\nCheck failure\n--------------------\n")

(test-file "standalone-test-case-test.rkt"
           #"#t\n#t\n"
           #"--------------------\nERROR\nFirst Outta here!\n\n === context ===\nPLTHOME/collects/racket/private/more-scheme.rkt:209:2: call-handled-body\n\n\n--------------------\n--------------------\nerror\nERROR\nSecond Outta here!\n\n === context ===\nPLTHOME/collects/racket/private/more-scheme.rkt:209:2: call-handled-body\n\n\n--------------------\n--------------------\nFAILURE\nname:       check-eq?\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 19 12 520 15)\nexpression: (check-eq? 1 2)\nactual:     1\nexpected:   2\n\nCheck failure\n--------------------\n--------------------\nfailure\nFAILURE\nname:       check-eq?\nlocation:   (#<path:PLTHOME/collects/tests/rackunit/standalone-test-case-test.rkt> 20 21 558 15)\nexpression: (check-eq? 1 2)\nactual:     1\nexpected:   2\n\nCheck failure\n--------------------\n")

