#lang racket/base

(require racket/cmdline
         raco/command-name
         profile/raco-utils
         "main.rkt")

;; raco contract-profile
;; profile the main submodule (if there is one), or the top-level module

(define module-graph-file #f)
(define boundary-view-file #f)
(define boundary-view-key-file #f)
(define file
  (command-line #:program (short-program+command-name)
                #:once-each
                [("--module-graph-file") file
                 "Output module view to <file>"
                 (set! module-graph-file file)]
                [("--boundary-view-file") file
                 "Output boundary view to <file>"
                 (set! boundary-view-file file)]
                [("--boundary-view-key-file") file
                 "Output boundary view key to <file>"
                 (set! boundary-view-key-file file)]
                #:args (filename)
                filename))

(collect-garbage)
(collect-garbage)
(collect-garbage)

(contract-profile
 #:module-graph-file module-graph-file
 #:boundary-view-file boundary-view-file
 #:boundary-view-key-file boundary-view-key-file
 (dynamic-require (module-to-profile file) #f))

(module test racket/base) ; don't run for testing
