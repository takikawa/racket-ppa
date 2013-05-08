#lang racket/base
(require rackunit
         racket/system
         unstable/debug
         racket/match
         (for-syntax racket/base
                     syntax/parse)
         racket/file
         racket/runtime-path
         racket/path
         racket/list
         pkg/util
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (shelly-begin
  (initialize-indexes)

  (shelly-case
   "dependencies"

   $ "test -f test-pkgs/pkg-test2.zip"
   (with-fake-root
    (shelly-case
     "local - fail (default)"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test2.zip" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test1.zip" =exit> 0
     $ "raco pkg install test-pkgs/pkg-test2.zip" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - looks at all packages given on cmdline"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install test-pkgs/pkg-test2.zip test-pkgs/pkg-test1.zip" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - fail"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps fail test-pkgs/pkg-test2.zip" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - force"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps force test-pkgs/pkg-test2.zip"
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 1
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - search-ask [y]"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-ask test-pkgs/pkg-test2.zip" =exit> 0 <input= "y\n"
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - search-ask []"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-ask test-pkgs/pkg-test2.zip" =exit> 0 <input= "\n"
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - search-ask [n]"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-ask test-pkgs/pkg-test2.zip" =exit> 1 <input= "n\n"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "local - search-auto"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps search-auto test-pkgs/pkg-test2.zip" =exit> 0
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "remote - search-ask (default) [y]"
     $ "raco pkg config --set indexes http://localhost:9990"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install pkg-test2" =exit> 0 <input= "y\n"
     $ "racket -e '(require pkg-test2)'" =exit> 0
     $ "racket -e '(require pkg-test2/contains-dep)'" =exit> 0
     $ "raco pkg remove pkg-test2"
     $ "racket -e '(require pkg-test2)'" =exit> 1))

   (with-fake-root
    (shelly-case
     "remote - fail"
     $ "racket -e '(require pkg-test2)'" =exit> 1
     $ "raco pkg install --deps fail pkg-test2" =exit> 1
     $ "racket -e '(require pkg-test2)'" =exit> 1)))))
