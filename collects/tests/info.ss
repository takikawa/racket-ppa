#lang setup/infotab

(define name "Test Suites")
(define tools '(("time-keystrokes.ss" "drscheme")))
(define tool-names '("Time Keystrokes"))

(define compile-omit-paths
  '("2htdp"
    "aligned-pasteboard"
    "deinprogramm"
    "future"
    "honu"
    "match"
    "macro-debugger"
    "mred"
    "mysterx"
    "mzcom"
    "mzscheme"
    "plai"
    "planet"
    "plot"
    "profj"
    "r6rs"
    "schemeunit"
    "srfi"
    "srpersist"
    "stepper"
    "stxparse"
    "syntax-color"
    "typed-scheme"
    "units"
    "unstable"
    "xml"
    "html"
    "web-server"))
