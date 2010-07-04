#lang setup/infotab

(define name "Test Suites")
(define tools '(("tool.ss" "drscheme") ("time-keystrokes.ss" "drscheme")))
(define tool-names '("DrScheme Test Suites" "Time Keystrokes"))

(define compile-omit-paths
  '("aligned-pasteboard"
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
    "srfi"
    "srpersist"
    "stepper"
    "stxclass"
    "syntax-color"
    "typed-scheme"
    "units"
    "xml"
    "html"
    "web-server"))
