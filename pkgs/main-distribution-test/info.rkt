#lang info

(define collection 'multi)

(define deps '("racket-test"
               "rackunit-test"
               "gui-test"
               "db-test"
               "htdp-test"
               "html-test"
               "redex-test"
               "drracket-test"
               "profile-test"
               "srfi-test"
               "errortrace-test"
               "r6rs-test"
               "web-server-test"
               "typed-racket-test"
               "xrepl-test"
               "scribble-test"
               "unstable-test"
               "compiler-test"
               "compatibility-test"
               "data-test"
               "net-test"
               "planet-test"
               "syntax-color-test"
               "images-test"
               "plot-test"
               "math-test"
               "racket-benchmarks"
               "drracket-tool-test"))

(define pkg-desc "tests for \"main-distribution\"")

(define pkg-authors '(eli jay matthias mflatt robby))
