#reader scribble/reader
#lang scheme/base

(require scribble/decode
         scribble/decode-struct
         scribble/basic
         scribble/struct
         scribble/manual-struct
         scheme/list
         scheme/string
         scheme/match
         net/url
         (only-in scheme/class send)
         (only-in xml xexpr->string)
         (only-in setup/dirs find-doc-dir)
         "utils.ss"
         scheme/runtime-path)

(provide make-search)

(define-runtime-path search-script "search.js")

(define (quote-string str)
  (define (hex4 ch)
    (let ([s (number->string (char->integer (string-ref ch 0)) 16)])
      (string-append "\\u" (case (string-length s)
                             [(1) (string-append "000" s)]
                             [(2) (string-append "00" s)]
                             [(3) (string-append "0" s)]
                             [else s]))))
  ;; use ~s to create a javascript-quoted string, then quote unicode chars
  (regexp-replace* #px"[^[:ascii:]]" (format "~s" str) hex4))

(define (make-script user-dir? renderer sec ri)
  (define dest-dir (send renderer get-dest-directory #t))
  (define span-classes null)
  ;; To make the index smaller, html contents is represented as one of these:
  ;; - a string
  ;; - an array of contents to be concatenated
  ;; - a two-item array [idx, content], where idx is an index into the
  ;;   span-classes table holding a class name.
  ;; In addition, a "file:/main-doc.../path..." url is saved as ">path..."
  ;; This function does the url compacting.
  (define main-url ; (make sure that it teminates with a slash)
    (if user-dir?
      (regexp-replace #rx"/*$" (url->string (path->url (find-doc-dir))) "/")
      "../"))
  (define compact-url
    (let ([rx (regexp (string-append "^" (regexp-quote main-url)))])
      (lambda (url) (regexp-replace rx url ">"))))
  ;; This function does the html compacting.
  (define (compact-body xexprs)
    (define (compact xexprs)
      (match xexprs
        [`() xexprs]
        [`("" . ,r) (compact r)]
        [`(,(? string? s1) ,(? string? s2) . ,r)
         (compact `(,(string-append s1 s2) . ,r))]
        [`((span ([class ,c]) . ,b1) (span ([class ,c]) . ,b2) . ,r)
         (compact `((span ([class ,c]) ,@b1 ,@b2) . ,r))]
        [`((span ([class ,c]) . ,b) . ,r)
         (let ([c (cond [(assoc c span-classes) => cdr]
                        [else (let ([n (length span-classes)])
                                (set! span-classes
                                      (cons (cons c n) span-classes))
                                n)])])
           (cons `(,c . ,(compact-body b)) (compact r)))]
        [`(,x . ,r) (cons (xexpr->string x) (compact r))]))
    ;; generate javascript array code
    (let loop ([body (compact xexprs)])
      (if (andmap string? body)
        (quote-string (string-append* body))
        (let ([body (map (lambda (x)
                           (if (string? x)
                             (quote-string x)
                             (format "[~a,~a]" (car x) (cdr x))))
                         body)])
          (if (= 1 (length body))
            (car body)
            (string-append* `("[" ,@(add-between body ",") "]")))))))
  (define manual-refs (make-hash))
  (define l
    (for/list ([i (get-index-entries sec ri)] [idx (in-naturals)])
      ;; i is (list tag (text ...) (element ...) index-desc)
      (define-values (tag texts elts desc) (apply values i))
      (define text (string-downcase (string-join texts " ")))
      (define-values (href html)
        (let* ([e (add-between elts ", ")]
               ;; !!HACK!! The index entry for methods should have the extra
               ;; text in it (when it does, this should go away)
               [e (if (method-index-desc? desc)
                    `(,@e ,(make-element "smaller"
                             `(" (method of "
                               ,(make-element 
                                 "schemesymbol"
                                 (list
                                  (make-element 
                                   "schemevaluelink"
                                   (list (symbol->string
                                          (exported-index-desc-name desc))))))
                               ")")))
                    e)]
               [e (make-link-element "indexlink" e tag)]
               [e (send renderer render-element e sec ri)])
          (match e ; should always render to a single `a'
            [`((a ([href ,href] [class "indexlink"]) . ,body))
             (cond [(and (part-index-desc? desc)
                         (regexp-match #rx"(?:^|/)([^/]+)/index\\.html$" href))
                    => (lambda (man) (hash-set! manual-refs (cadr man) idx))])
             (let (;; throw away tooltips, we don't need them
                   [body (match body
                           [`((span ((title ,label)) . ,body))
                            (if (regexp-match? #rx"^Provided from: " label)
                              body
                              ;; if this happens, this code should be updated
                              (error "internal error: unexpected tooltip"))]
                           [else body])])
               (values (compact-url href) (compact-body body)))]
            [else (error "something bad happened")])))
      (define (lib->name lib)
        (quote-string (let loop ([lib lib])
                        (match lib
                          [`',lib (string-append "'" (loop lib))]
                          [else (format "~s" lib)]))))
      (define from-libs
        (cond
          [(exported-index-desc? desc)
           (let ([libs (map lib->name (exported-index-desc-from-libs desc))])
             (string-append* `("[" ,@(add-between libs ",") "]")))]
          [(module-path-index-desc? desc) "\"module\""]
          [else "false"]))
      (string-append "[" (quote-string text) ","
                         (quote-string href) ","
                         html "," from-libs "]")))

  (with-output-to-file (build-path dest-dir "plt-index.js") #:exists 'truncate
    (lambda ()
      (for-each
       display
       @`{// the url of the main doc tree, for compact url
          // representation (see also the UncompactUrl function)
          plt_main_url = @,(quote-string main-url);@"\n"
          // classes to be used for compact representation of html strings in
          // plt_search_data below (see also the UncompactHtml function)
          plt_span_classes = [
            @,@(add-between (map (lambda (x) (quote-string (car x)))
                                 (reverse span-classes))
                            ",\n  ")];@"\n"
          // this array has an entry of four items for each index link:
          // - text is a string holding the indexed text
          // - url holds the link (">" prefix means relative to plt_main_url)
          // - html holds either a string, or [idx, html] where idx is an
          //   index into plt_span_classes (note: this is recursive)
          // - from_lib is an array of module names for bound identifiers,
          //   or the string "module" for a module entry
          plt_search_data = [
          @,@(add-between l ",\n")];@"\n"
          // array of pointers to the previous array, for items that are manuals
          plt_manual_ptrs = {
            @,@(let* ([ms (hash-map manual-refs cons)]
                      [ms (sort ms < #:key cdr)]
                      [ms (map (lambda (x)
                                 (string-append (quote-string (car x)) ": "
                                                (number->string (cdr x))))
                               ms)])
                 (add-between ms ",\n  "))};
          @||})))

  (let ([js (build-path dest-dir "search.js")])
    (when (file-exists? js) (delete-file js))
    (copy-file search-script js))

  (list
   (script-ref "plt-index.js"
     #:noscript @list{Sorry, you must have JavaScript to use this page.})
   (script-ref "search.js")
   (make-element (make-with-attributes #f '((id . "plt_search_container")))
                 null)))

(define (make-search user-dir?)
  (make-delayed-block (lambda (r s i)
                        (make-paragraph (make-script user-dir? r s i)))))
