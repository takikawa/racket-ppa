#lang distro-build/config
(require racket/format racket/runtime-path racket/string)

(define-runtime-path here ".")

(define source-dir (path->string (build-path here ".." "..")))

(define (build-dir-name)
  (case (current-mode)
    [("release") "release-build"]
    [else "build"]))
(define (dest-dir-name)
  (case (current-mode)
    [("release") "ci-release"]
    [else (~a (current-stamp))]))

(define server-base-url (~a "https://ci-snapshot.racket-lang.org/" (dest-dir-name) "/"))

(define distro-content
  '("main-distribution"))

(define (prebuilt-racket)
  (or (and (getenv "PLTHOME") (~a (getenv "PLTHOME") "/racket/" "bin/" "racket"))
      "/usr/bin/racket"))

(define windows        "{1} Windows")
(define macosx         "{2} Mac OS X")
(define linux          "{3} Linux")
(define unix-platforms "{8} Unix")
(define all-platforms  "{9} All Platforms")

(define (convert-log-name name)
  (define v (string-trim (regexp-replace* #rx"{[0-9]}|[\\(\\)\\|]" name "")))
  (string-append (string-downcase (regexp-replace* (regexp "[ ,]+") v "_")) ".txt"))



(define (machine/sh+tgz #:name name)
  ;; Create all Linux installer forms
  (sequential
   (machine #:name (~a name " | {1} Installer"))
   (machine #:name (~a name " | {3} Tarball")
            #:tgz? #t)))

(define (cs-machine #:name name #:pkgs [pkgs distro-content])
  (machine #:dir "cs-build"
           #:repo source-dir
           #:pull? #f
           #:j 2
           #:timeout (* 60 60 (if (null? pkgs) 1/2 2)) ;; 2 hours for the full build
           #:variant 'cs
           #:dist-suffix (if (null? pkgs) "min-cs" "cs")
           #:versionless? #t
           #:pkgs pkgs
           #:log-file (convert-log-name name)
           #:name name))

(define (bc-machine #:name name #:pkgs [pkgs distro-content])
  (machine #:versionless? #t
           #:pkgs pkgs
           #:j 2
           #:dist-suffix (if (null? pkgs) "min" "")
           #:log-file (convert-log-name name)
           #:name name))

;; The overall configuration:
(sequential
 #:pkgs distro-content
 #:dist-base-url server-base-url
 #:site-dest (build-path (or (getenv "DISTRO_BUILD_SITE_DEST") "/tmp/racket-snapshots/") (dest-dir-name))
 #:plt-web-style? #t
 #:site-title (format "Snapshot: ~a" (current-stamp))
 #:fail-on-client-failures #f
 (sequential
  (bc-machine #:name "Racket BC (Ubuntu 18.04, x86_64)")
  (bc-machine #:name "Minimal Racket BC (Ubuntu 18.04, x86_64)" #:pkgs null))
 (sequential
  (cs-machine #:name "Racket CS (Ubuntu 18.04, x86_64)")
  (cs-machine #:name "Minimal Racket CS (Ubuntu 18.04, x86_64)" #:pkgs null)))
