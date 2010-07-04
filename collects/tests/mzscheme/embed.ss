
(load-relative "loadtest.ss")

(Section 'embed)

(require (lib "embed.ss" "compiler")
	 (lib "process.ss"))

(define (mk-dest mred?)
  (build-path (find-system-path 'temp-dir) 
	      (case (system-type)
		[(windows) "e.exe"]
		[(unix) "e"]
		[(macosx) (if mred?
			      "e.app"
			      "e")])))

(define mz-dest (mk-dest #f))
(define mr-dest (mk-dest #t))

(define (prepare exe src)
  (printf "Making ~a with ~a...~n" exe src)
  (when (file-exists? exe)
    (delete-file exe)))

(define (try-exe exe expect mred?)
  (let ([plthome (getenv "PLTHOME")]
	[collects (getenv "PLTCOLLECTS")])
    ;; Try to hide usual collections:
    (when plthome
      (putenv "PLTHOME" (path->string (find-system-path 'temp-dir))))
    (when collects
      (putenv "PLTCOLLECTS" (path->string (find-system-path 'temp-dir))))
    ;; Execute:
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (when (file-exists? "stdout")
	(delete-file "stdout"))
      (system* (if (and mred? (eq? 'macosx (system-type)))
		   (let-values ([(base name dir?) (split-path exe)])
		     (build-path exe "Contents" "MacOS"
				 (path-replace-suffix name #"")))
		   exe)))
    (when plthome
      (putenv "PLTHOME" plthome))
    (when collects
      (putenv "PLTCOLLECTS" collects))
    (test expect with-input-from-file (build-path (find-system-path 'temp-dir) "stdout") 
	  (lambda () (read-string 5000)))))

(define (mz-tests mred?)
  (define dest (if mred? mr-dest mz-dest))
  (define (flags s)
    (string-append "-" (if mred? "Z" "") "mvq" s))
  (define (one-mz-test filename expect)
    ;; Try simple mode: one module, launched from cmd line:
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme")))
     null
     null
     `(,(flags "L") ,filename "tests/mzscheme"))
    (try-exe dest expect mred?)

    ;; Try explicit prefix:
    (let ([w/prefix
	   (lambda (pfx)
	     (prepare dest filename)
	     (make-embedding-executable 
	      dest mred? #f
	      `((,pfx (lib ,filename "tests" "mzscheme")))
	      null
	      null
	      `(,(flags "e") ,(format "(require ~a~a)" 
				      (or pfx "")
				      (regexp-replace #rx"[.].*$" filename ""))))
	     (try-exe dest expect mred?))])
      (w/prefix #f)
      (w/prefix 'before:))

    ;; Try full path, and use literal S-exp to start
    (prepare dest filename)
    (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
      (make-embedding-executable 
       dest mred? #f
       `((#t ,path))
       null
       `(require (file ,(path->string path)))
       `(,(flags ""))))
    (try-exe dest expect mred?)

    ;; Use `file' form:
    (prepare dest filename)
    (let ([path (build-path (collection-path "tests" "mzscheme") filename)])
      (make-embedding-executable 
       dest mred? #f
       `((#t (file ,(path->string path))))
       null
       `(require (file ,(path->string path)))
       `(,(flags ""))))
    (try-exe dest expect mred?)

    ;; Use relative path
    (prepare dest filename)
    (parameterize ([current-directory (collection-path "tests" "mzscheme")])
      (make-embedding-executable 
       dest mred? #f
       `((#f ,filename))
       null
       `(require ,(string->symbol (regexp-replace #rx"[.].*$" filename "")))
       `(,(flags ""))))
    (try-exe dest expect mred?)

    ;; Try multiple modules
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme"))
       (#t (lib "embed-me3.ss" "tests" "mzscheme")))
     null
     `(begin
	(require (lib "embed-me3.ss" "tests" "mzscheme"))
	(require (lib ,filename "tests" "mzscheme")))
     `(,(flags "")))
    (try-exe dest (string-append "3 is here, too? #t\n" expect) mred?)

    ;; Try a literal file
    (prepare dest filename)
    (make-embedding-executable 
     dest mred? #f
     `((#t (lib ,filename "tests" "mzscheme")))
     (list (build-path (collection-path "tests" "mzscheme") "embed-me4.ss"))
     `(with-output-to-file "stdout"
	(lambda () (display "... and more!\n"))
	'append)
     `(,(flags "L") ,filename "tests/mzscheme"))
    (try-exe dest (string-append 
		      "This is the literal expression 4.\n" 
		      "... and more!\n"
		      expect)
	     mred?))

  (one-mz-test "embed-me1.ss" "This is 1\n")
  (one-mz-test "embed-me2.ss" "This is 1\nThis is 2: #t\n")

  ;; Try unicode expr and cmdline:
  (prepare dest "unicode")
  (make-embedding-executable 
   dest mred? #f
   null
   null
   `(begin 
      (define (out s)
	(with-output-to-file "stdout"
	  (lambda () (printf s))
	  'append))
      (out "\uA9, \u7238, and \U1D670\n"))
   `(,(flags "e") "(out \"\u7237...\U1D671\n\")"))
  (try-exe dest "\uA9, \u7238, and \U1D670\n\u7237...\U1D671\n" mred?))

(mz-tests #f)
(mz-tests #t)

(prepare mr-dest "embed-me5.ss")
(make-embedding-executable 
 mr-dest #t #f
 `((#t (lib "embed-me5.ss" "tests" "mzscheme")))
 null
 null
 `("-ZmvqL" "embed-me5.ss" "tests/mzscheme"))
(try-exe mr-dest "This is 5: #<struct:class:button%>\n" #t)

;; Try the mzc interface:
(require (lib "dirs.ss" "setup")
	 (lib "file.ss"))
(define mzc (build-path (find-console-bin-dir) "mzc"))

(define (mzc-tests mred?)
  (parameterize ([current-directory (find-system-path 'temp-dir)])

    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me1.ss")))
    (try-exe (mk-dest mred?) "This is 1\n" mred?)

    ;; Check that etc.ss isn't found if it's not included:
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\nno etc.ss\n" mred?)

    ;; And it is found if it is included:
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "++lib" "etc.ss" "mzlib"
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

    ;; Or, it's found if we set the collection path:
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "--collects-path"
	     (path->string (find-collects-dir))
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\n#t\n" mred?)

    ;; Try --collects-dest mode
    (system* mzc 
	     (if mred? "--gui-exe" "--exe")
	     (path->string (mk-dest mred?))
	     "++lib" "etc.ss" "mzlib"
	     "--collects-dest" "cts"
	     "--collects-path" "cts"
	     (path->string (build-path (collection-path "tests" "mzscheme") "embed-me6.ss")))
    (try-exe (mk-dest mred?) "This is 6\n#t\n" mred?)
    (delete-directory/files "cts")
    (try-exe (mk-dest mred?) "This is 6\nno etc.ss\n" mred?)

    (void)))

(mzc-tests #t)
(mzc-tests #f)

;; One MrEd-specific test with mzc:
(parameterize ([current-directory (find-system-path 'temp-dir)])
  (system* mzc 
	   "--gui-exe"
	   (path->string (mk-dest #t))
	   (path->string (build-path (collection-path "tests" "mzscheme") "embed-me5.ss")))
  (try-exe (mk-dest #t) "This is 5: #<struct:class:button%>\n" #t))



(report-errs)
