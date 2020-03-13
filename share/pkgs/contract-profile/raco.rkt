#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "main.rkt")

;; raco contract-profile
;; profile the main submodule (if there is one), or the top-level module

(define module-graph-view-file #f)
(define boundary-view-file #f)
(define boundary-view-key-file #f)
(define report-space-efficient? #f)
(define file
  (command-line #:program (short-program+command-name)
                #:once-each
                [("--module-graph-view-file") file
                 "Output module graph view to <file>"
                 (set! module-graph-view-file file)]
                [("--boundary-view-file") file
                 "Output boundary view to <file>"
                 (set! boundary-view-file file)]
                [("--boundary-view-key-file") file
                 "Output boundary view key to <file>"
                 (set! boundary-view-key-file file)]
                [("--report-space-efficient")
                 "Distinguish space-efficient contracts from non"
                 (set! report-space-efficient? #t)]
                #:args (filename)
                filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(contract-profile
 #:module-graph-view-file module-graph-view-file
 #:boundary-view-file boundary-view-file
 #:boundary-view-key-file boundary-view-key-file
 #:report-space-efficient? report-space-efficient?
 (dynamic-require (module-to-profile file) #f))

(module test racket/base) ; don't run for testing
