#lang at-exp scheme
(require scheme/date
         scheme/runtime-path
         xml
         "config.ss"
         "diff.ss"
         "list-count.ss"
         "cache.ss"
         (except-in "dirstruct.ss"
                    revision-trunk-dir)
         "status.ss"
         "monitor-scm.ss"
         (only-in "metadata.ss"
                  PROP:command-line
                  PROP:timeout)
         "formats.ss"
         "path-utils.ss"
         "analyze.ss")

(define (base-path pth)
  (define rev (current-rev))
  (define log-dir (revision-log-dir rev))
  ((rebase-path log-dir "/") pth))

(define-runtime-path static "static")

(define (snoc l x) (append l (list x)))
(define (list-head l n)
  (if (zero? n)
      empty
      (list* (first l)
             (list-head (rest l) (sub1 n)))))
(define (all-but-last l) (list-head l (sub1 (length l))))

(define (to-index i)
  (cond
    [(<= i 0) "."]
    [else
     (apply string-append (snoc (add-between (make-list i "..") "/") "/"))]))

(define (current-depth log-pth directory?)
  (define new-pth ((rebase-path (revision-log-dir (current-rev)) "/") log-pth))
  (define depth (sub1 (length (explode-path new-pth))))
  (if directory? 
      depth
      (sub1 depth)))

(define (next-rev)
  (init-revisions!)
  (local [(define end (newest-completed-revision))]
    (let loop ([rev (add1 (current-rev))])
      (cond
        [(<= end rev)
         end]
        [(read-cache* (build-path (revision-dir rev) "analyzed"))
         rev]
        [else
         (loop (add1 rev))]))))

(define (path->breadcrumb pth directory?)
  (define the-rev (current-rev))
  (define new-pth ((rebase-path (revision-log-dir the-rev) "/") pth))
  (define parts (rest (explode-path new-pth)))
  (define string-parts (list* (format "R~a" the-rev) (map path->string parts)))
  (define (parent-a href sp)
    `(a ([class "parent"] [href ,href]) ,sp))
  (define the-base-path*
    (format "~a~a"
            (base-path pth)
            (if directory? "/" "")))
  (define the-base-path
    (if (string=? the-base-path* "//")
        "/"
        the-base-path*))
  (define prev-rev-url (format "/~a~a" (previous-rev) the-base-path))
  (define next-rev-url (format "/~a~a" (next-rev) the-base-path))
  (define cur-rev-url (format "/~a~a" "current" the-base-path))
  ; XXX Don't special case top level
  (values (apply string-append (add-between (list* "DrDr" string-parts) " / "))
          `(span
            (span ([class "breadcrumb"])
                 ,(parent-a "/" "DrDr") " / "
                 ,@(add-between
                    (snoc
                     (for/list ([sp (in-list (all-but-last string-parts))]
                                [from-root (in-naturals)])
                       (define the-depth (current-depth pth directory?))
                       (parent-a (to-index (- the-depth from-root)) sp))
                     `(span ([class "this"]) 
                            ,(last string-parts)))
                    " / "))
            (span ([class "revnav"])
                  (a ([href ,prev-rev-url]) (img ([src "/images/rewind.png"])))
                  (a ([href ,next-rev-url]) (img ([src "/images/fast-forward.png"])))
                  (a ([href ,cur-rev-url]) (img ([src "/images/skip-forward1.png"])))))))

(define (looks-like-directory? pth)
  (and (regexp-match #rx"/$" pth) #t))

(define (svn-date->nice-date date)
  (regexp-replace "^(....-..-..)T(..:..:..).*Z$" date "\\1 \\2"))
(define (git-date->nice-date date)
  (regexp-replace "^(....-..-..) (..:..:..).*$" date "\\1 \\2"))
(define (log->url log)
  (define start-commit (git-push-start-commit log))
  (define end-commit (git-push-end-commit log))
  (if (string=? start-commit end-commit)
      (format "http://github.com/plt/racket/commit/~a" end-commit)
      (format "http://github.com/plt/racket/compare/~a...~a" (git-push-previous-commit log) end-commit)))
  
(define (format-commit-msg)
  (define pth (revision-commit-msg (current-rev)))
  (define (timestamp pth)
    (with-handlers ([exn:fail? (lambda (x) "")])
      (date->string (seconds->date (read-cache (build-path (revision-dir (current-rev)) pth))) #t)))
  (define bdate/s (timestamp "checkout-done"))
  (define bdate/e (timestamp "integrated"))
  (match (read-cache* pth)
    [(and gp (struct git-push (num author commits)))
     (define start-commit (git-push-start-commit gp))
     (define end-commit (git-push-end-commit gp))
     `(table ([class "data"])
             (tr ([class "author"]) (td "Author:") (td ,author))
             (tr ([class "date"]) (td "Build Start:") (td ,bdate/s))
             (tr ([class "date"]) (td "Build End:") (td ,bdate/e))
             (tr ([class "hash"]) (td "Diff:") (td (a ([href ,(log->url gp)]) ,(substring start-commit 0 8) ".." ,(substring end-commit 0 8))))
             ,@(append-map
                (match-lambda
                  [(struct git-merge (hash author date msg from to))
                   #;`((tr ([class "hash"]) (td "Commit:") (td (a ([href ,(format "http://github.com/plt/racket/commit/~a" hash)]) ,hash)))
                     (tr ([class "date"]) (td "Date:") (td ,(git-date->nice-date date)))
                     (tr ([class "author"]) (td "Author:") (td ,author))
                     (tr ([class "msg"]) (td "Log:") (td (pre ,@msg)))
                     (tr ([class "merge"]) (td "Merge:") (td "From " ,from " to " ,to)))
                   ; Don't display these "meaningless" commits
                   empty]
                  [(struct git-diff (hash author date msg mfiles))
                   (define cg-id (symbol->string (gensym 'changes)))
                   (define ccss-id (symbol->string (gensym 'changes)))
                   `((tr ([class "hash"]) (td "Commit:") (td (a ([href ,(format "http://github.com/plt/racket/commit/~a" hash)]) ,hash)))
                     (tr ([class "date"]) (td "Date:") (td ,(git-date->nice-date date)))
                     (tr ([class "author"]) (td "Author:") (td ,author))
                     (tr ([class "msg"]) (td "Log:") (td (pre ,@msg)))
                     (tr ([class "changes"]) 
                         (td 
                          (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" cg-id ccss-id)])
                             (span ([id ,cg-id]) 9658) "Changes:"))
                         (td
                          (div ([id ,ccss-id]
                                [style "display: none;"])
                               ,@(for/list ([path (in-list mfiles)])
                                         `(p ([class "output"])
                                             ,(if (regexp-match #rx"^collects" path)
                                                  (local [(define path-w/o-trunk
                                                            (apply build-path (explode-path path)))
                                                          (define html-path
                                                            (if (looks-like-directory? path)
                                                                (format "~a/" path-w/o-trunk)
                                                                path-w/o-trunk))
                                                          (define path-url
                                                            (path->string* html-path))
                                                          (define path-tested?
                                                            #t)]
                                                    (if path-tested?
                                                        `(a ([href ,path-url]) ,path)
                                                        path))
                                                  path)))))))])
                commits))]
     
    [(struct svn-rev-log (num author date msg changes))
     (define url (format "http://svn.racket-lang.org/view?view=rev&revision=~a" num))
     (define cg-id (symbol->string (gensym 'changes)))
     (define ccss-id (symbol->string (gensym 'changes)))
     `(table ([class "data"])
              (tr ([class "author"]) (td "Author:") (td ,author))
              (tr ([class "date"]) (td "Build Start:") (td ,bdate/s))
              (tr ([class "date"]) (td "Build End:") (td ,bdate/e))
              (tr ([class "rev"]) (td "Commit:") (td (a ([href ,url]) ,(number->string num))))
              (tr ([class "date"]) (td "Date:") (td ,(svn-date->nice-date date)))
              (tr ([class "msg"]) (td "Log:") (td (pre ,msg)))
              (tr ([class "changes"]) 
                  (td 
                   (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" cg-id ccss-id)])
                      (span ([id ,cg-id]) 9658) "Changes:"))
                  (td
                   (div ([id ,ccss-id]
                         [style "display: none;"])
                        ,@(map (match-lambda
                                 [(struct svn-change (action path))
                                  `(p ([class "output"])
                                      ,(symbol->string action) " " 
                                      ,(if (regexp-match #rx"^/trunk/collects" path)
                                           (local [(define path-w/o-trunk
                                                     (apply build-path (list-tail (explode-path path) 2)))
                                                   (define html-path
                                                     (if (looks-like-directory? path)
                                                         (format "~a/" path-w/o-trunk)
                                                         path-w/o-trunk))
                                                   (define path-url
                                                     (path->string* html-path))
                                                   (define path-tested?
                                                     #t)]
                                             (if path-tested?
                                                 `(a ([href ,path-url]) ,path)
                                                 path))
                                           path))])
                               changes)))))]
    [else
     '" "]))

(define (footer)
  `(div ([id "footer"])
        "Powered by " (a ([href "http://racket-lang.org/"]) "Racket") ". "
        "Written by " (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "
        (a ([href "/help"])
           "Need help?")
        (br)
        "Current time: " ,(date->string (seconds->date (current-seconds)) #t)))

(define (render-event e)
  (with-handlers ([exn:fail?
                   (lambda (x)
                     `(pre ([class "unprintable"]) "UNPRINTABLE"))])
    (match e
      [(struct stdout (bs))
       `(pre ([class "stdout"]) ,(bytes->string/utf-8 bs))]
      [(struct stderr (bs))
       `(pre ([class "stderr"]) ,(bytes->string/utf-8 bs))])))

(define (render-log log-pth)
  (match (log-rendering log-pth)
    [#f
     (file-not-found log-pth)]
    [(and the-log-rendering (struct rendering (_ _ dur _ _ _ responsible changed)))
     (match (read-cache log-pth)
       [(and log (struct status (_ _ command-line output-log)))
        (define-values (title breadcrumb) (path->breadcrumb log-pth #f))
        (define the-base-path
          (base-path log-pth))
        (define scm-url
          (if ((current-rev) . < . 20000)
              (format "http://svn.racket-lang.org/view/trunk/~a?view=markup&pathrev=~a"
                      the-base-path
                      (current-rev))
              (local [(define msg (read-cache* (revision-commit-msg (current-rev))))]
                (if msg
                    (format "http://github.com/plt/racket/blob/~a~a"
                            (git-push-end-commit msg) the-base-path)
                    "#"))))
        (define prev-rev-url (format "/~a~a" (previous-rev) the-base-path))
        (define cur-rev-url (format "/~a~a" "current" the-base-path))
        (define output (map render-event output-log))
        `(html (head (title ,title)
                     (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
               (body 
                (div ([class "log, content"])
                     ,breadcrumb
                     (table ([class "data"])
                            (tr (td "Responsible:")
                                (td ,responsible))
                            (tr (td "Command-line:") 
                                (td ,@(add-between
                                       (map (lambda (s)
                                              `(span ([class "commandline"]) ,s))
                                            command-line)
                                       " ")))
                            (tr (td "Duration:") (td ,(format-duration-ms dur)
                                                     nbsp (a ([href ,(format "/data~a" (path-add-suffix the-base-path #".timing"))])
                                                             "(timing data)")))
                            (tr (td "Timeout:") (td ,(if (timeout? log) checkmark-entity "")))
                            (tr (td "Exit Code:") (td ,(if (exit? log) (number->string (exit-code log)) "")))
                            (tr (td " ") (td (a ([href ,scm-url]) "View File"))))
                     ,(if (lc-zero? changed)
                          ""
                          `(div ([class "error"])
                                  "This result of executing this file has changed since the previous push."
                                  " "
                                  (a ([href ,(format "/diff/~a/~a~a" (current-rev) (previous-rev) the-base-path)])
                                     "See the difference")))
                     ,@(if (empty? output)
                           '()
                           `((div ([class "output"]) " "
                                  ,@output)))
                     ,(with-handlers ([exn:fail?
                                       ; XXX Remove this eventually
                                       (lambda (x)
                                         ; XXX use dirstruct functions
                                         (define png-path
                                           (format "/data~a" (path-add-suffix (path-add-suffix the-base-path #".timing") #".png")))
                                         `(div ([class "timing"])
                                               (a ([href ,png-path])
                                                  (img ([src ,png-path])))))])
                        (make-cdata
                         #f #f
                         (local [(define content 
                                   (file->string
                                    (path-timing-html (substring (path->string* the-base-path) 1))))]
                           #;(regexp-replace* #rx"&(?![a-z]+;)" content "\\&amp;\\1")
                           (regexp-replace* #rx"&gt;" content ">"))
                         ))
                     ,(footer))))])]))

(define (number->string/zero v)
  (cond 
    [(zero? v)
     '" "]
    [else
     (number->string v)]))

(define checkmark-entity
  10004)

(define (render-logs/dir dir-pth #:show-commit-msg? [show-commit-msg? #f])
  (match (dir-rendering dir-pth)
    [#f
     (dir-not-found dir-pth)]
    [(and pth-rendering (struct rendering (tot-start tot-end tot-dur tot-timeout tot-unclean tot-stderr tot-responsible tot-changes)))
     (define files
       (foldl (lambda (sub-pth files)
                (define pth (build-path dir-pth sub-pth))
                (define directory? (cached-directory-exists? pth))
                (define pth-rendering
                  (if directory?
                      (dir-rendering pth)
                      (log-rendering pth)))
                (list* (list directory? sub-pth pth-rendering) files))
              empty
              (cached-directory-list* dir-pth)))
     (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
     `(html (head (title ,title)
                  (script ([src "/sorttable.js"]) " ")
                  (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
            (body
             (div ([class "dirlog, content"])
                  ,breadcrumb
                  ,(if show-commit-msg?
                        (format-commit-msg)
                        "")
                  ,(local [(define (path->url pth)
                             (format "http://drdr.racket-lang.org/~a~a" (current-rev) pth))
                           
                           (define responsible->problems
                             (rendering->responsible-ht (current-rev) pth-rendering))
                           (define last-responsible->problems
                             (with-handlers ([exn:fail? (lambda (x) (make-hash))])
                               (define prev-dir-pth ((rebase-path (revision-log-dir (current-rev))
                                                                  (revision-log-dir (previous-rev)))
                                                     dir-pth))
                               (define previous-pth-rendering
                                 (parameterize ([current-rev (previous-rev)])
                                   (dir-rendering prev-dir-pth)))
                               (rendering->responsible-ht (previous-rev) previous-pth-rendering)))
                           (define new-responsible->problems
                             (responsible-ht-difference last-responsible->problems responsible->problems))
                           
                           (define (render-responsible->problems tag responsible->problems)
                             (if (zero? (hash-count responsible->problems))
                                 ""
                                 `(div ([class "status"])
                                       (div ([class "tag"]) ,tag)
                                       ,@(for/list ([(responsible ht) (in-hash responsible->problems)])
                                           (define rcss-id (symbol->string (gensym)))
                                           (define rg-id (symbol->string (gensym 'glyph)))
                                           (define summary
                                             (for/fold ([s ""])
                                               ([id (in-list responsible-ht-severity)])
                                               (define llc (hash-ref ht id empty))
                                               (if (empty? llc)
                                                   s
                                                   (format "~a [~a: ~a]" s id (length llc)))))                                  
                                           `(div (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" rg-id rcss-id)])
                                                    (span ([id ,rg-id]) 9658) " "
                                                    ,responsible
                                                    " " ,summary)
                                                 (blockquote 
                                                  ([id ,rcss-id]
                                                   [style "display: none;"])
                                                  ,@(local [(define i 0)]
                                                      (for/list ([id (in-list responsible-ht-severity)])
                                                        (define llc (hash-ref ht id empty))
                                                        (if (empty? llc)
                                                            ""
                                                            (local [(define display? (< i 2))
                                                                    (define css-id (symbol->string (gensym 'ul)))
                                                                    (define glyph-id (symbol->string (gensym 'glyph)))]
                                                              (set! i (add1 i))
                                                              `(div (a ([href ,(format "javascript:TocviewToggle(\"~a\",\"~a\");" glyph-id css-id)])
                                                                       (span ([id ,glyph-id]) ,(if display? 9660 9658)) " "
                                                                       ,(hash-ref responsible-ht-id->str id))
                                                                    (ul ([id ,css-id] 
                                                                         [style ,(format "display: ~a"
                                                                                         (if display? "block" "none"))])
                                                                        ,@(for/list ([p llc])
                                                                            `(li (a ([href ,(path->url p)]) ,(path->string p))))))))))))))))]
                     `(div ,(render-responsible->problems "all" responsible->problems)
                           ,(render-responsible->problems "new" new-responsible->problems)))
                  (table ([class "sortable, dirlist"])
                         (thead
                          (tr (td "Path")
                              (td "Duration (Abs)")
                              (td "Duration (Sum)")
                              (td "Timeout?")
                              (td "Unclean Exit?")
                              (td "STDERR Output")
                              (td "Changes")
                              (td "Responsible")))
                         (tbody
                          ,@(map (match-lambda
                                   [(list directory? sub-pth (struct rendering (start end dur timeout unclean stderr responsible-party changes)))
                                    (define name (path->string sub-pth))
                                    (define abs-dur (- end start))
                                    (define url 
                                      (if directory?
                                          (format "~a/" name)
                                          name))
                                    `(tr ([class ,(if directory? "dir" "file")]
                                          [onclick ,(format "document.location = ~S" url)])
                                         (td ([sorttable_customkey 
                                               ,(format "~a:~a"
                                                        (if directory? "dir" "file")
                                                        name)])
                                             (a ([href ,url]) ,name ,(if directory? "/" "")))
                                         (td ([sorttable_customkey ,(number->string abs-dur)])
                                             ,(format-duration-ms abs-dur))
                                         (td ([sorttable_customkey ,(number->string dur)])
                                             ,(format-duration-ms dur))
                                         ,@(map (lambda (vv)
                                                  (define v (lc->number vv))
                                                  `(td ([sorttable_customkey ,(number->string v)])
                                                       ,(if directory?
                                                            (number->string/zero v)
                                                            (if (zero? v)
                                                                '" "
                                                                checkmark-entity))))
                                                (list timeout unclean stderr changes))
                                         (td ,responsible-party))])
                                 (sort files
                                       (match-lambda*
                                         [(list (list dir?1 name1 _)
                                                (list dir?2 name2 _))
                                          (cond
                                            [(and dir?1 dir?2)
                                             (string<=? (path->string name1)
                                                        (path->string name2))]
                                            [dir?1 #t]
                                            [dir?2 #f])]))))
                         (tfoot
                          (tr ([class "total"])
                              (td "Total")
                              (td ,(format-duration-ms (- tot-end tot-start)))
                              (td ,(format-duration-ms tot-dur))
                              (td ,(number->string/zero (lc->number tot-timeout)))
                              (td ,(number->string/zero (lc->number tot-unclean)))
                              (td ,(number->string/zero (lc->number tot-stderr)))
                              (td ,(number->string/zero (lc->number tot-changes)))
                              (td " "))))
                  ,(footer))))]))

(define (show-help req)
  `(html
    (head (title "DrDr > Help")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "dirlog, content"])
          ; XXX Use same function as above
          (span ([class "breadcrumb"])
                (a ([class "parent"] [href "/"]) "DrDr") " / "
                (span ([class "this"]) 
                      "Help"))
          @div[[(class "help")]]{
            @h1{What is DrDr?}
            @p{DrDr is a server at @a[[(href "http://www.byu.edu")]]{Brigham Young University} that builds
               and "tests" every push to the Racket code base.}
            
            @h1{What kind of server?}
            @p{A 64-bit Linux 2.6.28-15 server running Ubuntu 9.04 with @,(number->string (number-of-cpus)) cores.}
            
            @h1{How is the build run?}
            @p{Every push is built from a clean checkout with the standard separate build directory command sequence, except that @code{make}
               is passed @code{-j} with the number of cores. Each push also has a fresh home directory and PLaneT cache.}
               
            @h1{How long does it take for a build to start after a check-in?}
            @p{Only one build runs at a time and when none is running the git repository is polled every @,(number->string (current-monitoring-interval-seconds)) seconds.}
            
            @h1{How is the push "tested"?}
            @p{Each file's @code{@,PROP:command-line} property is consulted. If it is the empty string, the file is ignored. If it is a string, then a single @code{~s} is replaced with the file's path, @code{racket} and @code{mzc} with their path (for the current push), and @code{gracket} and @code{gracket-text} with @code{gracket-text}'s path (for the current push); then the resulting command-line is executed. 
               (Currently no other executables are allowed, so you can't @code{rm -fr /}.)
               If there is no property value, the default @code{racket -qt ~s} is used if the file's suffix is @code{.rkt}, @code{.ss}, @code{.scm}, or @code{.scrbl} and @code{racket -f ~s} is used if the file's suffix is @code{.rktl}.}
                    
            @p{The command-line is always executed with a fresh empty current directory which is removed after the run. But all the files share the same home directory and X server, which are both removed after each push's testing is complete.}
            
            @h1{How many files are "tested" concurrently?}
            @p{One per core, or @,(number->string (number-of-cpus)).}
            
            @h1{How long may a file run?}
            @p{The execution timeout is @,(number->string (current-subprocess-timeout-seconds)) seconds by default, but the @code{@,PROP:timeout} property is used if @code{string->number} returns a number on it.}
            
            @h1{May these settings be set on a per-directory basis?}
            @p{Yes; if the property is set on any ancestor directory, then its value is used for its descendents when theirs is not set.
               }
            
            @h1{What data is gathered during these runs?}
            @p{When each file is run the following is recorded: the start time, the command-line, the STDERR and STDOUT output, the exit code (unless there is a timeout), and the end time. All this information is presented in the per-file DrDr report page.}
            
            @h1{How is the data analyzed?}
            @p{From the data collected from the run, DrDr computes the total test time and whether output has "changed" since the last time the file was tested.}
            
            @h1{What output patterns constitute a "change"?}
            @p{At the most basic level, if the bytes are different. However, there are two subtleties. First, DrDr knows to ignore the result of @code{time}. Second, the standard output and standard error streams are compared independently. The difference display pages present changed lines with a @span[([class "difference"])]{unique background}.}
            
            @h1{How is this site organized?}
            @p{Each file's test results are displayed on a separate page, with a link to the previous push on changes. All the files in a directory are collated and indexed recursively. On these pages each column is sortable and each row is clickable. The root of a push also includes the git commit messages with links to the test results of the modified files. The top DrDr page displays the summary information for all the tested pushes.}
            
            @h1{What is the difference between @code{Duration (Abs)} and @code{Duration (Sum)}?}
            @p{@code{Duration (Abs)} is the difference between the earliest start time and the latest end time in the collection.}
            @p{@code{Duration (Sum)} is the sum of each file's difference between the start time and end time.}
            @p{The two are often different because of parallelism in the testing process. (Long absolute durations indicate DrDr bugs waiting to get fixed.)}
            
            @h1{What do the graphs mean?}
            @p{There is a single graph for each file, i.e., graphs are not kept for old pushs.}
            @p{The X-axis is the tested push. The Y-axis is the percentage of the time of the slowest push.}
            @p{The gray, horizontal lines show where 0%, 25%, 50%, 75%, and 100% are in the graph.}
            @p{The black line shows the times for overall running of the file. The colored lines show the results from @code{time}. For each color, the "real" time is the darkest version of it and the "cpu" and "gc" time are 50% and 25% of the darkness, respectively.}
            @p{If the number of calls to @code{time} change from one push to the next, then there is a gray, vertical bar at that point. Also, the scaling to the slowest time is specific to each horizontal chunk.}
            @p{The graph is split up into panes that each contain approximately 300 pushes. The green arrowheads to the left
               and right of the image move between panes.}
            @p{The legend at the bottom of the graph shows the current pane, as well as the push number and any timing information from that push.}
            @p{Click on the graph to jump to the DrDr page for a specific push.}
            
            @h1{What is the timing data format?}
            @p{The timing files are a list of S-expressions. Their grammar is: @code{(push duration ((cpu real gc) ...))} where @code{push} is an integer, @code{duration} is an inexact millisecond, and @code{cpu}, @code{real}, and @code{gc} are parsed from the @code{time-apply} function.}

            @h1{Why are some pushes missing?}
            @p{Some pushes are missing because they only modify branches. Only pushes that change the @code{master} branch are tested.}
            
            @h1{How do I make the most use of DrDr?}
            @p{So DrDr can be effective with all testing packages and untested code, it only pays attention to error output and non-zero exit codes. You can make the most of this strategy by ensuring that when your tests are run successfully they have no STDERR output and exit cleanly, but have both when they fail.}
            
            @h1{How do I fix the reporting of an error in my code?}
            @p{If you know you code does not have a bug, but DrDr thinks it does, you can probably fix it by setting its properties: allow it to run longer with @code{@,PROP:timeout} (but be kind and perhaps change the program to support work load selection on the command-line) or make sure it is run with the right command-line using @code{@,PROP:command-line}.}
            
            @h1{How can I do the most for DrDr?}
            @p{The most important thing you can do is eliminate false positives by configuring DrDr for your code and removing spurious error output.}
            @p{The next thing is to structure your code so DrDr does not do the same work many times. For example, because DrDr will load every file if your test suite is broken up into different parts that execute when loaded @em{and} they are all loaded by some other file, then DrDr will load and run them twice. The recommended solution is to have DrDr ignore the combining file or change it so a command-line argument is needed to run everything but is not provided by DrDr, that way the combining code is compiled but the tests are run once.}
            
            }                                           
          ,(footer)))))

(define (list-limit len offset l)
  (take (drop l offset) len))

(define (string-first-line s)
  (define v
    (with-input-from-string s read-line))
  (if (eof-object? v)
      "" v))

(define log->committer+title 
  (match-lambda
    [(struct git-push (num author commits))
     (define lines (append-map (λ (c) (if (git-merge? c) empty (git-commit-msg c))) commits))
     (define title
       (if (empty? lines)
           ""
           (first lines)))
     (values author title)]
    [(struct svn-rev-log (num author date msg changes))
     (define commit-msg (string-first-line msg))
     (define title 
       (format "~a - ~a"
               (svn-date->nice-date date)
               commit-msg))
     (values author title)]))

(require web-server/servlet-env
         web-server/http
         web-server/dispatch
         "scm.ss")
(define how-many-revs 45)
(define (show-revisions req)
  (define builds-pth (plt-build-directory))
  (define offset
    (match (bindings-assq #"offset" (request-bindings/raw req))
      [(struct binding:form (_ val))
       (string->number (bytes->string/utf-8 val))]
      [_
       0]))
  (define future-revs
    (map (curry cons 'future)
         (sort (directory-list* (plt-future-build-directory))
               >
               #:key (compose string->number path->string))))
  (define how-many-future-revs
    (length future-revs))
  (define built-or-building-revs
    (map (curry cons 'past)
         (sort (directory-list* builds-pth)
               >
               #:key (compose string->number path->string))))
  (define all-revs
    (append future-revs built-or-building-revs))
  (define how-many-total-revs
    (length all-revs))
  `(html
    (head (title "DrDr")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "dirlog, content"])
          ; XXX Use same function as above
          (span ([class "breadcrumb"])
                (span ([class "this"]) 
                      "DrDr"))
          (table ([class "dirlist"])
                 (thead
                  (tr (td "Push#")
                      (td "Duration (Abs)")
                      (td "Duration (Sum)")
                      (td "Timeout?")
                      (td "Unclean Exit?")
                      (td "STDERR Output")
                      (td "Changes")
                      (td "Pusher")))
                 (tbody
                  ,@(map (match-lambda
                           [(cons 'future rev-pth)
                            (define name (path->string rev-pth))
                            (define rev (string->number name))
                            (define log (read-cache (future-record-path rev)))
                            (define-values (committer title)
                              (log->committer+title log))
                            (define url (log->url log))
                            `(tr ([class "dir"]
                                  [title ,title])
                                 (td (a ([href ,url]) ,name))
                                 (td ([class "building"] [colspan "6"])
                                     "")
                                 (td ([class "author"]) ,committer))]
                           [(cons 'past rev-pth)
                            (define name (path->string rev-pth))
                            (define url (format "~a/" name))
                            (define rev (string->number name))
                            (define log-pth (revision-commit-msg rev))
                            (define log (read-cache log-pth))
                            (define-values (committer title)
                              (log->committer+title log))
                            (define (no-rendering-row)
                              (define mtime 
                                (file-or-directory-modify-seconds log-pth))
                              
                              `(tr ([class "dir"]
                                    [title ,title])
                                   (td (a ([href "#"]) ,name))
                                   (td ([class "building"] [colspan "6"])
                                       "Build in progress. Started "
                                       ,(format-duration-m
                                         (/ (- (current-seconds) mtime) 60))
                                       " ago.")
                                   (td ([class "author"]) ,committer)))
                            (parameterize ([current-rev rev])
                              (with-handlers 
                                  ([(lambda (x)
                                      (regexp-match #rx"No cache available" (exn-message x)))
                                    (lambda (x)
                                      (no-rendering-row))])
                                ; XXX One function to generate
                                (match (dir-rendering (revision-log-dir rev))
                                  [#f
                                   (no-rendering-row)]
                                  [(struct rendering 
                                           (start end dur timeout unclean
                                                  stderr responsible-party changes))
                                   (define abs-dur (- end start))
                                   
                                   `(tr ([class "dir"]
                                         [title ,title]
                                         [onclick ,(format "document.location = ~S" url)])
                                        (td (a ([href ,url]) ,name))
                                        (td ([sorttable_customkey ,(number->string abs-dur)])
                                            ,(format-duration-ms abs-dur))
                                        (td ([sorttable_customkey ,(number->string dur)])
                                            ,(format-duration-ms dur))
                                        ,@(map (lambda (vv)
                                                 (define v (lc->number vv))
                                                 `(td ([sorttable_customkey ,(number->string v)])
                                                      ,(number->string/zero v)))
                                               (list timeout unclean stderr changes))
                                        (td ,responsible-party))])))])
                                (list-limit
                                 how-many-revs offset
                                 all-revs))))
          (table ([id "revnav"] [width "100%"])
                 (tr (td ([align "left"])
                         (span ([class "revnav"])
                               (a ([href ,(top-url show-revisions)])
                                  (img ([src "/images/skip-backward1.png"])))
                               (a ([href ,(format "~a?offset=~a"
                                                  (top-url show-revisions)
                                                  (max 0 (- offset how-many-revs)))])
                                  (img ([src "/images/rewind.png"])))))
                     (td ([align "right"])
                         (span ([class "revnav"])
                               (a ([href ,(format "~a?offset=~a"
                                                  (top-url show-revisions)
                                                  (min (- how-many-total-revs how-many-revs)
                                                       (+ offset how-many-revs)))])
                                  (img ([src "/images/fast-forward.png"])))
                               (a ([href ,(format "~a?offset=~a"
                                                  (top-url show-revisions)
                                                  (- how-many-total-revs how-many-revs))])
                                  (img ([src "/images/skip-forward1.png"])))))))
          ,(footer)))))

(define (show-revision req rev)
  (define log-dir (revision-log-dir rev))
  (parameterize ([current-rev rev]
                 [previous-rev (find-previous-rev rev)])
    (with-handlers ([(lambda (x)
                       (regexp-match #rx"No cache available" (exn-message x)))
                     (lambda (x)
                       (rev-not-found log-dir rev))])
      (render-logs/dir log-dir #:show-commit-msg? #t))))

(define (file-not-found file-pth)
  (define-values (title breadcrumb) (path->breadcrumb file-pth #f))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "This file does not exist in push #" ,(number->string (current-rev)) " or has not been tested.")
          ,(footer)))))
(define (dir-not-found dir-pth)
  (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "This directory does not exist in push #" ,(number->string (current-rev)) " or has not been tested.")
          ,(footer)))))
(define (rev-not-found dir-pth path-to-file)
  (define-values (title breadcrumb) (path->breadcrumb dir-pth #t))
  `(html
    (head (title ,title " > Not Found")
          (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
    (body
     (div ([class "content"])
          ,breadcrumb
          (div ([class "error"])
               "Push #" ,(number->string (current-rev)) " does not exist or has not been tested.")
          ,(footer)))))

(define (find-previous-rev this-rev)
  (if (zero? this-rev)
      #f
      (local [(define maybe (sub1 this-rev))]
        (if (cached-directory-exists? (revision-log-dir maybe))
            maybe
            (find-previous-rev maybe)))))

(define (show-file req rev path-to-file)
  (define log-dir (revision-log-dir rev))
  (parameterize ([current-rev rev]
                 [previous-rev (find-previous-rev rev)])
    (if (member "" path-to-file)
        (local [(define dir-pth
                  (apply build-path log-dir (all-but-last path-to-file)))]
          (with-handlers ([(lambda (x)
                             (regexp-match #rx"No cache available" (exn-message x)))
                           (lambda (x)
                             (dir-not-found dir-pth))])
            (render-logs/dir dir-pth)))
        (local [(define file-pth
                  (apply build-path log-dir path-to-file))]
          (with-handlers ([(lambda (x)
                             (regexp-match #rx"No cache available" (exn-message x)))
                           (lambda (x)
                             (file-not-found file-pth))])
            (render-log file-pth))))))

(define (show-revision/current req)
  (init-revisions!)
  (redirect-to
   (top-url show-revision (newest-completed-revision))))
(define (show-file/current req . args)
  (init-revisions!)
  (redirect-to
   (apply top-url show-file (newest-completed-revision) args)))

(define (show-diff req r1 r2 f)
  (define f1 (apply build-path (revision-log-dir r1) f))
  (with-handlers ([(lambda (x)
                     (regexp-match #rx"File is not cached" (exn-message x)))
                   (lambda (x)
                     ; XXX Make a little nicer
                     (parameterize ([current-rev r1])
                       (file-not-found f1)))])
    (define l1 (status-output-log (read-cache f1)))
    (define f2 (apply build-path (revision-log-dir r2) f))
    (with-handlers ([(lambda (x)
                       (regexp-match #rx"File is not cached" (exn-message x)))
                     (lambda (x)
                       ; XXX Make a little nicer
                       (parameterize ([current-rev r2])
                         (file-not-found f2)))])
      (define l2 (status-output-log (read-cache f2)))
      (define f-str (path->string (apply build-path f)))
      (define title 
        (format "DrDr / File Difference / ~a (~a:~a)"
                f-str r1 r2))
      
      `(html (head (title ,title)
                   (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
             (body 
              (div ([class "log, content"])
                   (span ([class "breadcrumb"])
                         (a ([class "parent"] [href "/"])
                            "DrDr")
                         " / "
                         (span ([class "this"]) 
                               "File Difference"))
                   (table ([class "data"])
                          (tr (td "First Push:") (td (a ([href ,(format "/~a/~a" r1 f-str)]) ,(number->string r1))))
                          (tr (td "Second Push:") (td (a ([href ,(format "/~a/~a" r2 f-str)]) ,(number->string r2))))
                          (tr (td "File:") (td "/" ,f-str)))
                   (div ([class "output"])
                        (table ([class "diff"])
                               ,@(for/list ([d (in-list (render-log-difference l1 l2))])
                                   (match d
                                     [(struct difference (old new))
                                      `(tr ([class "difference"])
                                           (td ,(render-event old))
                                           (td ,(render-event new)))]
                                     [(struct same-itude (e))
                                      `(tr (td ([colspan "2"]) ,(render-event e)))]))))
                   ,(footer)))))))

(define-values (top-dispatch top-url)
  (dispatch-rules
   [("help") show-help]
   [("") show-revisions]
   [("diff" (integer-arg) (integer-arg) (string-arg) ...) show-diff]
   [("current" "") show-revision/current]
   [("current" (string-arg) ...) show-file/current]
   [((integer-arg) "") show-revision]
   [((integer-arg) (string-arg) ...) show-file]))

#;(define (xml-dispatch req)
  (define xe (top-dispatch req))
  (define full
    (make-xexpr-response xe #:mime-type #"application/xhtml+xml"))
  (struct-copy response/full full
               [body (list* 
                      #"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"
                      (response/full-body full))]))

(date-display-format 'iso-8601)
(cache/file-mode 'no-cache)
(serve/servlet top-dispatch
               #:port 9000
               #:listen-ip #f
               #:quit? #f
               #:launch-browser? #f
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:extra-files-paths (list static))
