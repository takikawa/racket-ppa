#lang racket/base
(require racket/path
         racket/system
         racket/format
         racket/file
         racket/pretty
         "cmdline.rkt")

(define sign-as #f)

(define dest-dir
  (build-command-line
   #:once-each
   [("--sign-as") id "Sign Mac OS X libraries"
    (set! sign-as id)]
   #:args (dest-dir)
   dest-dir))

;; Hack to make AArch64 Mac OS libraries look like other Macs:
(define renames
  `(("libffi.7" "libffi.6")))

(define libs
  `("libffi.6"
    "libgio-2.0.0"
    "libgmodule-2.0.0"
    "libgthread-2.0.0"
    "libglib-2.0.0"
    "libgobject-2.0.0"
    "libintl.9"
    "libharfbuzz.0"
    "libfribidi.0"
    "libpango-1.0.0"
    "libpangocairo-1.0.0"
    "libpangoft2-1.0.0"
    "libatk-1.0.0"
    "libexpat.1"
    "libfontconfig.1"
    "libfreetype.6"
    "libcairo.2"
    "libpixman-1.0"
    "libpng16.16"
    "libgmp.10"
    "libmpfr.4"
    "libjpeg.9"
    "libpoppler.44"
    "libpoppler-glib.8"))

(define win-libs
  '("libiconv-2"
    "libeay32"
    "ssleay32"
    "sqlite3"
    "longdouble"
    "zlib1"
    "libpangowin32-1.0.0"))

(define mac-libs
  '("libedit.0"))

(define mac64-libs
  '("MMTabBarView.framework"))

(define macx86-libs
  '("PSMTabBarControl.framework"))

(define nonwin-libs
  '("libcrypto.1.1"
    "libssl.1.1"
    "libuuid.1"))

(define no-copy-libs
  '("PSMTabBarControl.framework"
    "MMTabBarView.framework"))

(define linux-libs
  (append
   '("libXau.6"
     "libxcb-shm.0"
     "libxcb-render.0"
     "libxcb.1"
     "libX11.6"
     "libXext.6"
     "libXrender.1"
     "fonts")
   '("libz.1"
     "libsqlite3.0")
   '("libgtk-x11-2.0.0"
     "libgdk-x11-2.0.0"
     "libgdk_pixbuf-2.0.0")))
(define linux-remove-libs
  '("libintl.9"))

(define package-mapping
  `(["draw"        ; pkg name
     "-3"          ; pkg suffix (increment after "-" when library versions change)
     "racket/draw" ; subdir
     "" ; extra for "LICENSE.txt"
     #t ; dynamic libraries (as opposed to shared files)
     #f ; for-pkg name (e.g., "base"), of #f if the same as the pkg name
     #f ; version
     (["libffi" "libffi - Copyright (c) 1996-2014  Anthony Green, Red Hat, Inc and others."]
      ["libglib" "GLib is released under the GNU Library General Public License (GNU LGPL)."]
      "libgio"
      "libgmodule"
      "libgobject"
      "libgthread"
      ["libintl" "libintl is released under the GNU Library General Public License (GNU LGPL)."]
      ["libharfbuzz" "HarfBuzz is released under a MIT license."]
      ["libfribidi" "FriBidi is released under the GNU Library General Public License (GNU LGPL)."]
      ["libpango" "Pango is released under the GNU Library General Public License (GNU LGPL)."]
      "libpangocairo"
      "libpangoft2"
      "libpangowin32"
      "libexpat"
      ["libuuid" "libuuid is released under a Modified BSD license."]
      ["libfontconfig" ,(~a "FontConfig:\n"
                            " Copyright © 2000,2001,2002,2003,2004,2006,2007 Keith Packard\n"
                            " Copyright © 2005 Patrick Lam\n"
                            " Copyright © 2009 Roozbeh Pournader\n"
                            " Copyright © 2008,2009 Red Hat, Inc.\n"
                            " Copyright © 2008 Danilo Šegan\n"
                            " Copyright © 2012 Google, Inc.")]
      ["libfreetype" "Pixman is released under the FreeType project license."]
      ["libcairo" "Cairo is released under the GNU Library General Public License (GNU LGPL)."]
      ["libpixman" "Pixman is released under a MIT license."]
      ["libpng" "Libpng is released under the libpng license."]
      ["libjpeg" "This software is based in part on the work of the Independent JPEG Group."]
      ["zlib1" "zlib is by Jean-loup Gailly and Mark Adler."]
      ["libz" "zlib is by Jean-loup Gailly and Mark Adler."])]
    ["racket"
     "-3"
     "racket"
     ""
     #t
     #f
     #f
     (["libeay32" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (http://www.openssl.org/).\n"
                       "\n"
                       "Eric Young is the author of libeay and ssleay.")]
      "ssleay32"
      ["libssl" ,(~a "This product includes software developed by the OpenSSL Project for\n"
                       "use in the OpenSSL Toolkit (http://www.openssl.org/).\n")]
      "libcrypto"
      ["libiconv-2" "libiconv is released under the GNU Lesser General Public License (GNU LGPL)."]
      ["longdouble" ,(~a "The source to longdouble is included with the Racket source code,\n"
                         "which is available from\n"
                         "  http://www.racket-lang.org/")]
      ["libedit" ,(~a "This package includes libedit software developed for NetBSD under the\n"
                      "NetBSD license.")])]

    ["math"
     ""
     "math"
     ""
     #t
     #f
     #f
     (["libgmp" "GNU MP is released under the GNU Lesser General Public License (GNU LGPL)."]
      ["libmpfr" "MPFR is released under the GNU Lesser General Public License (GNU LGPL)."])]

    ["draw-x11"
     ""
     "racket/draw/x11"
     ""
     #t
     "draw"
     #f
     (["libX11.6" "libX11 is released under the X.Org Foundation license."]
      ["libXau.6" "libXau - Copyright 1988, 1993, 1994, 1998  The Open Group"]
      ["libxcb-shm.0" "libxcb - Copyright (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett."]
      "libxcb-render.0"
      "libxcb.1"
      ["libXext.6" "libXext - Copyright 1986, 1987, 1988, 1989, 1994, 1998  The Open Group"]
      ["libXrender.1" "libXrender - Copyright © 2001,2003 Keith Packard"])]
    ["draw-ttf"
     ""
     "racket/draw/ttf"
     ""
     #f
     "draw"
     #f
     (["fonts" ,(~a "Fonts:\n"
                    " Copyright © 2000,2001,2002,2003,2004,2006,2007 Keith Packard\n"
                    " Copyright © 2005 Patrick Lam\n"
                    " Copyright © 2009 Roozbeh Pournader\n"
                    " Copyright © 2008,2009 Red Hat, Inc.\n"
                    " Copyright © 2008 Danilo Šegan\n"
                    " Copyright © 2012 Google, Inc.")])]

    ["gui"
     ""
     "racket/gui"
     ""
     #t
     #f
     "1.3" ; version
     (["libgtk-x11-2.0.0" "GTK+ is released under the GNU Library General Public License (GNU LGPL)."]
      ["libatk" "ATK is released under the GNU Library General Public License (GNU LGPL)."]
      "libgdk-x11-2.0.0"
      "libgdk_pixbuf-2.0.0"
      ["PSMTabBarControl.framework" "PSMTabBarControl is BSD licensed.\nSee: http://www.positivespinmedia.com/dev/PSMTabBarControl.html"]
      ["MMTabBarView.framework" "MMTabBarView is BSD licensed.\nSee: http://mimo42.github.io/MMTabBarView/"])]

    ["db"
     ""
     "db"
     ""
     #t
     "base"
     #f
     (["libsqlite3.0" "SQLite3 is in the public domain."]
      ["sqlite3" "SQLite3 is in the public domain."])]
    
    ["poppler"
     ""
     "racket-poppler"
     ""
     #t
     "racket-poppler"
     #f
     (["libpoppler"
       ;; Note: Poppler is GPL and *not* in the main Racket distribution (which is LGPL)
       "Poppler is released under the GNU General Public License (GNU GPL)."])]))

(define (libs-of-pkg p) (list-ref p 7))

(define (framework? p)
  (regexp-match? #rx"[.]framework" p))

(define (plain-path? p)
  (or (equal? p "fonts")
      (framework? p)))

(define (revert-name p)
  (or (for/or ([pr (in-list renames)])
	(and (equal? (cadr pr) p)
	     (car pr)))
      p))

(define from (build-path (current-directory) "dest" (if win? "bin" "lib")))

(define (find-pkg lib)
  (define pkg (for/or ([p (in-list package-mapping)])
                (define nl
                  (for/or ([nl (in-list (libs-of-pkg p))])
                    (define n (if (pair? nl) (car nl) nl))
                    (cond
                     [(equal? n lib) nl]
                     [else
                      (define len (string-length n))
                      (and ((string-length lib) . > . (add1 len))
                           (string=? n (substring lib 0 len))
                           (regexp-match? #rx"[-.0-9]" (string (string-ref lib len)))
                           nl)])))
                (and nl
                     (list (car p) (cadr p) (caddr p)
                           (and (pair? nl) (cadr nl))))))
  (unless pkg
    (error 'install "cannot find package for library: ~e" lib))
  (apply values pkg))

(define (gen-info platform i-platform for-pkg pkg-name subdir libs lics lic-end lib? vers)
  (define dest (build-path dest-dir pkg-name))
  (define lib-path (build-path dest subdir "info.rkt"))
  (define top-path (build-path dest "info.rkt"))
  (define same? (equal? lib-path top-path))
  (define (write-libs o)
    (newline o)
    (pretty-write `(define install-platform ,i-platform) o)
    (newline o)
    (pretty-write `(define ,(if lib?
                                'copy-foreign-libs
                                'copy-shared-files)
                    (quote ,libs))
                  o)
    (define dirs (filter (lambda (lib)
                           (or (framework? lib)
                               (directory-exists? (build-path dest subdir lib))))
                         libs))
    (unless (null? dirs)
      (newline o)
      (pretty-write `(define compile-omit-paths (quote ,dirs)) o)))
  (define (write-pkg o)
    (newline o)
    (pretty-write `(define collection 'multi) o)
    (pretty-write `(define deps '("base")) o)
    (newline o)
    (pretty-write `(define pkg-desc ,(format "native libraries for \"~a\" package" for-pkg)) o)
    (newline o)
    (pretty-write `(define pkg-authors '(mflatt)) o)
    (when vers
      (newline o)
      (pretty-write `(define version ,vers) o)))
  (unless same?
    (printf "Write ~a\n" lib-path)
    (call-with-output-file*
     lib-path
     #:exists 'truncate
     (lambda (o)
       (displayln "#lang setup/infotab" o)
       (write-libs o))))
  (printf "Write ~a\n" top-path)
  (call-with-output-file*
   top-path
   #:exists 'truncate
   (lambda (o)
     (displayln "#lang setup/infotab" o)
     (write-pkg o)
     (when same?
       (write-libs o))))
  (define lic-path (build-path dest "LICENSE.txt"))
  (printf "Write ~a\n" lic-path)
  (call-with-output-file*
   lic-path
   #:exists 'truncate
   (lambda (o)
     (displayln pkg-name o)
     (newline o)
     (displayln "This package is distributed under the Apache 2.0 and MIT licenses. The" o)
     (displayln "user can choose the license under which they will be using the" o)
     (displayln "software. There may be other licenses within the distribution with" o)
     (displayln "which the user must also comply." o)
     (for ([l (in-list lics)])
       (newline o)
       (displayln l o))
     (display lic-end o))))

(define (install platform i-platform so fixup libs)
  (define pkgs (make-hash))
  (define pkgs-lic (make-hash))

  (define (install lib)
    (define-values (p orig-p)
      (let ()
	(define (both v) (values v v))
	(cond
	 [(plain-path? lib) (both lib)]
	 [(procedure? so) (both (so lib))]
	 [else
	  (define (make lib) (format "~a.~a" lib so))
	  (values (make lib) (make (revert-name lib)))])))
    (define-values (pkg suffix subdir lic) (find-pkg lib))
    (define dir (build-path dest-dir
                            (~a pkg "-" platform suffix)
                            subdir))
    (define dest (build-path dir p))
    (let-values ([(base name dir?) (split-path dest)])
      (make-directory* base))
    (unless (member p no-copy-libs)
      (cond
        [(file-exists? dest) (delete-file dest)]
        [(directory-exists? dest) (delete-directory/files dest)])
      (define src (build-path from orig-p))
      (if (directory-exists? src)
          (copy-directory/files src dest)
          (copy-file src dest)))
    (unless (plain-path? p)
      (fixup p dest))

    (hash-update! pkgs pkg (lambda (l) (cons p l)) '())
    (when lic
      (hash-update! pkgs-lic pkg (lambda (l) (cons lic l)) '())))
  
  (for-each install libs)

  (for ([(pkg libs) (in-hash pkgs)])
    (define a (assoc pkg package-mapping))
    (gen-info platform 
              i-platform
              (or (list-ref a 5) pkg)
              (~a pkg "-" platform (list-ref a 1))
              (list-ref a 2)
              libs
              (reverse (hash-ref pkgs-lic pkg null))
              (list-ref a 3)
              (list-ref a 4)
              (list-ref a 6))))

(define (install-mac)
  (define (fixup p p-new)
    (unless (framework? p)
      (printf "Fixing ~s\n" p-new)
      (when aarch64?
	(system (format "codesign --remove-signature ~a" p-new)))
      (unless (memq 'write (file-or-directory-permissions p-new))
        (file-or-directory-permissions p-new #o744))
      (system (format "install_name_tool -id ~a ~a" (file-name-from-path p-new) p-new))
      (for-each (lambda (s)
                  (system (format "install_name_tool -change ~a @loader_path/~a ~a"
                                  (format "~a/~a.dylib" from (revert-name s))
                                  (format "~a.dylib" s)
                                  p-new)))
                (append libs nonwin-libs))
      (system (format "strip -S ~a" p-new))
      (when sign-as
	(system (format "codesign -s ~s --timestamp ~a" sign-as p-new)))))

  (define platform (~a (if m32? 
                           (if ppc? "ppc" "i386")
			   (if aarch64? "aarch64" "x86_64"))
                       "-macosx"))

  (install platform platform "dylib" fixup (append libs
                                                   mac-libs
                                                   (cond
                                                     [m32? '()]
                                                     [else mac64-libs])
                                                   (cond
                                                     [aarch64? '()]
                                                     [else macx86-libs])
                                                   nonwin-libs)))

(define (install-win)
  (define exe-prefix (if m32?
                         "i686-w64-mingw32"
                         "x86_64-w64-mingw32"))

  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (system (~a exe-prefix "-strip -S " p-new)))

  (parameterize ([current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (putenv "PATH" (~a (if m32?
                           "/usr/local/mw32/bin:/usr/mw32/bin:"
                           "/usr/local/mw64/bin:/usr/mw64/bin:")
                       (getenv "PATH")))

    (install (~a "win32-" (if m32? "i386" "x86_64"))
             (~a "win32\\" (if m32? "i386" "x86_64"))
             "dll"
             fixup
             (for/list ([s (in-list (append libs
                                            win-libs))])
               (regexp-replace #rx"!"
                               (regexp-replace* #rx"[.]"
                                                (regexp-replace #rx"[.](?=.*[.])" s "!")
                                                "-")
                               ".")))))

(define (install-linux)
  (define (fixup p p-new)
    (printf "Fixing ~s\n" p-new)
    (file-or-directory-permissions p-new #o755)
    (unless (system (format "strip -S ~a" p-new))
      (error "strip failed"))
    ;; Might fail if there are no external references:
    (system (format "chrpath -r '$ORIGIN' ~a" p-new)))

  (define platform (~a (if m32?
                           "i386"
                           "x86_64")
                       "-linux-natipkg"))

  (define (add-so orig-p)
    (let loop ([p orig-p] [suffix ""])
      (define p-so (string-append p ".so" suffix))
      (cond
       [(file-exists? (build-path from p-so))
        p-so]
       [else
        (define m (regexp-match #rx"^(.*)[.](.*)$" p))
        (cond
         [m
          (loop (cadr m) (string-append "." (caddr m) suffix))]
         [else
          (error 'add-so "not found: ~s" orig-p)])])))

  (install platform platform add-so fixup (append (remove* linux-remove-libs
							   libs)
                                                  nonwin-libs
						  linux-libs)))

(cond
 [win? (install-win)]
 [linux? (install-linux)]
 [else (install-mac)])
