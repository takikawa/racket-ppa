#lang info

(define name "DeinProgramm")

(define tools '("sdp/private/sdp-langs.rkt" "DMdA/private/DMdA-langs.rkt"))

(define tool-icons '(("logo-small.png" "deinprogramm") ("dmda-logo.png" "deinprogramm")))
(define tool-names '("DeinProgramm SDP" "DeinProgramm DMdA"))
(define tool-urls '("http://www.deinprogramm.de/" "http://www.deinprogramm.de/dmda/"))

(define compile-omit-paths
  '("DMdA/define-record-procedures.scm"
    "DMdA/private/convert-explicit.scm"
    "DMdA/teachpack/line3d.scm"
    "sdp/record.scm"
    "sdp/private/convert-explicit.scm"))
(define test-omit-paths compile-omit-paths)

(define get-textbook-pls
  '("textbook-pls-spec.rkt" textbook-pls))
