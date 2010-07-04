#lang scheme/base

(require "struct.ss"
         mzlib/class
         mzlib/serialize
         scheme/file
         scheme/path
         setup/main-collects
         setup/path-relativize
         "render-struct.ss")

(provide render%)

(define render%
  (class object%

    (init-field dest-dir
                [refer-to-existing-files #f]
                [root-path #f]
                [prefix-file #f]
                [style-file #f]
                [style-extra-files null]
                [extra-files null])

    (define/public (get-dest-directory [create? #f])
      (when (and dest-dir create? (not (directory-exists? dest-dir)))
        (make-directory* dest-dir))
      dest-dir)

    (define/public (get-substitutions) null)

    (define/public (get-suffix) #".txt")

    (define/public (index-manual-newlines?)
      #f)

    (define/public (format-number number sep)
      (if (or (null? number)
              (andmap not number))
        null
        (cons (let ([s (apply
                        string-append
                        (map (lambda (n) (if n (format "~s." n) ""))
                             (reverse number)))])
                (substring s 0 (sub1 (string-length s))))
              sep)))

    (field [report-output?? #f])
    (define/public (report-output?) report-output??)
    (define/public (report-output!) (set! report-output?? #t))

    ;; ----------------------------------------

    (define/public (extract-part-style-files d ri tag stop-at-part?)
      (let loop ([p d][up? #t][only-up? #f])
        (let ([s (part-style p)])
          (apply
           append
           (if up?
               (let ([p (collected-info-parent (part-collected-info p ri))])
                 (if p
                     (loop p #t #t)
                     null))
               null)
           (if (list? s)
               (filter
                values
                (map (lambda (s)
                       (and (list? s)
                            (= 2 (length s))
                            (eq? (car s) tag)
                            (path-string? (cadr s))
                            (cadr s)))
                     s))
               null)
           (if only-up?
               null
               (map (lambda (p)
                      (if (stop-at-part? p)
                          null
                          (loop p #f #f)))
                    (part-parts p)))))))
  
    ;; ----------------------------------------

    (define root (make-mobile-root root-path))
    
    (define-values (:path->root-relative
                    :root-relative->path)
      (if root-path
          (make-relativize (lambda () root-path)
                           root
                           'path->root-relative
                           'root-relative->path)
          (values #f #f)))

    (define/public (path->root-relative p)
      (if root-path
          (:path->root-relative p)
          p))

    (define/public (root-relative->path p)
      (if (root-relative? p)
          (apply build-path (mobile-root-path (car p))
                 (map bytes->path-element (cdr p)))
          p))

    (define/public (root-relative? p)
      (and (pair? p)
           (mobile-root? (car p))))

    ;; ----------------------------------------
    ;; marshal info

    (define/public (get-serialize-version)
      2)

    (define/public (serialize-info ri)
      (parameterize ([current-serialize-resolve-info ri])
        (serialize (cons root
                         (collect-info-ht (resolve-info-ci ri))))))

    (define/public (deserialize-info v ci #:root [root-path #f])
      (let ([root+ht (deserialize v)]
            [in-ht (collect-info-ext-ht ci)])
        (when root-path
          (set-mobile-root-path! (car root+ht) root-path))
        (for ([(k v) (cdr root+ht)])
          (hash-set! in-ht k v))))

    (define/public (get-defined ci)
      (hash-map (collect-info-ht ci) (lambda (k v) k)))

    (define/public (get-undefined ri)
      (hash-map (resolve-info-undef ri) (lambda (k v) k)))

    (define/public (transfer-info ci src-ci)
      (let ([in-ht (collect-info-ext-ht ci)])
        (for ([(k v) (collect-info-ext-ht src-ci)])
          (hash-set! in-ht k v))))

    ;; ----------------------------------------
    ;; global-info collection

    (define/public (collect ds fns)
      (let ([ci (make-collect-info (make-hash)
                                   (make-hash)
                                   (make-hasheq)
                                   (make-hasheq)
                                   null
                                   (make-hasheq)
                                   null)])
        (start-collect ds fns ci)
        ci))

    (define/public (start-collect ds fns ci)
      (map (lambda (d) (collect-part d #f ci null))
           ds))

    (define/public (collect-part d parent ci number)
      (let ([p-ci (make-collect-info
                   (make-hash)
                   (collect-info-ext-ht ci)
                   (collect-info-parts ci)
                   (collect-info-tags ci)
                   (if (part-tag-prefix d)
                       (append (collect-info-gen-prefix ci)
                               (list (part-tag-prefix d)))
                       (collect-info-gen-prefix ci))
                   (collect-info-relatives ci)
                   (cons d (collect-info-parents ci)))])
        (hash-set! (collect-info-parts ci)
                   d
                   (make-collected-info number
                                        parent
                                        (collect-info-ht p-ci)))
        (when (part-title-content d)
          (collect-content (part-title-content d) p-ci))
        (collect-part-tags d p-ci number)
        (collect-content (part-to-collect d) p-ci)
        (collect-flow (part-flow d) p-ci)
        (let loop ([parts (part-parts d)]
                   [pos 1])
          (unless (null? parts)
            (let ([s (car parts)])
              (collect-part s d p-ci
                            (cons (if (or (unnumbered-part? s) 
                                          (part-style? s 'unnumbered))
                                      #f 
                                      pos)
                                  number))
              (loop (cdr parts)
                    (if (or (unnumbered-part? s) 
                            (part-style? s 'unnumbered))
                        pos
                        (add1 pos))))))
        (let ([prefix (part-tag-prefix d)])
          (for ([(k v) (collect-info-ht p-ci)])
            (when (cadr k)
              (collect-put! ci (if prefix
                                   (convert-key prefix k) 
                                   k) 
                            v))))))

    (define/private (convert-key prefix k)
      (case (car k)
        [(part tech cite)
         (let ([rhs (cadr k)])
           (if (or (string? rhs) (pair? rhs))
               (list (car k) (cons prefix (if (pair? rhs) rhs (list rhs))))
               k))]
        [(index-entry)
         (let ([v (convert-key prefix (cadr k))])
           (if (eq? v (cadr k)) k (list 'index-entry v)))]
        [else k]))

    (define/public (collect-part-tags d ci number)
      (for ([t (part-tags d)])
        (hash-set! (collect-info-ht ci)
                   (generate-tag t ci)
                   (list (or (part-title-content d) '("???")) number))))

    (define/public (collect-content c ci)
      (for ([i c]) (collect-element i ci)))

    (define/public (collect-paragraph p ci)
      (collect-content (paragraph-content p) ci))

    (define/public (collect-flow p ci)
      (for ([p (flow-paragraphs p)])
        (collect-block p ci)))

    (define/public (collect-block p ci)
      (cond [(table? p) (collect-table p ci)]
            [(itemization? p) (collect-itemization p ci)]
            [(blockquote? p) (collect-blockquote p ci)]
            [(delayed-block? p) (void)]
            [else (collect-paragraph p ci)]))

    (define/public (collect-table i ci)
      (for ([d (apply append (table-flowss i))])
        (when (flow? d) (collect-flow d ci))))

    (define/public (collect-itemization i ci)
      (for ([d (itemization-flows i)])
        (collect-flow d ci)))

    (define/public (collect-blockquote i ci)
      (for ([d (blockquote-paragraphs i)])
        (collect-block d ci)))

    (define/public (collect-element i ci)
      (if (part-relative-element? i)
        (let ([content (or (hash-ref (collect-info-relatives ci) i #f)
                           (let ([v ((part-relative-element-collect i) ci)])
                             (hash-set! (collect-info-relatives ci) i v)
                             v))])
          (collect-content content ci))
        (begin (when (target-element? i) (collect-target-element i ci))
               (when (index-element? i) (collect-index-element i ci))
               (when (collect-element? i) ((collect-element-collect i) ci))
               (when (element? i)
                 (for ([e (element-content i)]) (collect-element e ci))))))

    (define/public (collect-target-element i ci)
      (collect-put! ci (generate-tag (target-element-tag i) ci) (list i)))

    (define/public (collect-index-element i ci)
      (collect-put! ci
                    `(index-entry ,(generate-tag (index-element-tag i) ci))
                    (list (index-element-plain-seq i)
                          (index-element-entry-seq i)
                          (index-element-desc i))))

    ;; ----------------------------------------
    ;; global-info resolution

    (define/public (resolve ds fns ci)
      (let ([ri (make-resolve-info ci (make-hasheq) (make-hash) (make-hash))])
        (start-resolve ds fns ri)
        ri))

    (define/public (start-resolve ds fns ri)
      (map (lambda (d) (resolve-part d ri)) ds))

    (define/public (resolve-part d ri)
      (when (part-title-content d)
        (resolve-content (part-title-content d) d ri))
      (resolve-flow (part-flow d) d ri)
      (for ([p (part-parts d)])
        (resolve-part p ri)))

    (define/public (resolve-content c d ri)
      (for ([i c])
        (resolve-element i d ri)))

    (define/public (resolve-paragraph p d ri)
      (resolve-content (paragraph-content p) d ri))

    (define/public (resolve-flow p d ri)
      (for ([p (flow-paragraphs p)])
        (resolve-block p d ri)))

    (define/public (resolve-block p d ri)
      (cond
        [(table? p) (resolve-table p d ri)]
        [(itemization? p) (resolve-itemization p d ri)]
        [(blockquote? p) (resolve-blockquote p d ri)]
        [(delayed-block? p) 
         (let ([v ((delayed-block-resolve p) this d ri)])
           (hash-set! (resolve-info-delays ri) p v)
           (resolve-block v d ri))]
        [else (resolve-paragraph p d ri)]))

    (define/public (resolve-table i d ri)
      (for ([f (apply append (table-flowss i))])
        (when (flow? f) (resolve-flow f d ri))))

    (define/public (resolve-itemization i d ri)
      (for ([f (itemization-flows i)])
        (resolve-flow f d ri)))

    (define/public (resolve-blockquote i d ri)
      (for ([f (blockquote-paragraphs i)])
        (resolve-block f d ri)))

    (define/public (resolve-element i d ri)
      (cond
        [(part-relative-element? i)
         (resolve-content (part-relative-element-content i ri) d ri)]
        [(delayed-element? i)
         (resolve-content (or (hash-ref (resolve-info-delays ri) i #f)
                              (let ([v ((delayed-element-resolve i) this d ri)])
                                (hash-set! (resolve-info-delays ri) i v)
                                v))
                          d ri)]
        [(element? i)
         (cond
           [(index-element? i)
            (let ([e (index-element-desc i)])
              (when (delayed-index-desc? e)
                (let ([v ((delayed-index-desc-resolve e) this d ri)])
                  (hash-set! (resolve-info-delays ri) e v))))]
           [(link-element? i)
            (resolve-get d ri (link-element-tag i))])
         (for ([e (element-content i)])
           (resolve-element e d ri))]))

    ;; ----------------------------------------
    ;; render methods

    (define/public (install-extra-files)
      (for ([fn extra-files]) (install-file fn)))

    (define/public (render ds fns ri)
      ;; maybe this should happen even if fns is empty or all #f?
      ;; or maybe it should happen for each file rendered (when d is not #f)?
      (unless (andmap not ds) (install-extra-files))
      (map (lambda (d fn)
             (define (one) (render-one d ri fn))
             (when (report-output?) (printf " [Output to ~a]\n" fn))
             (if fn
               (with-output-to-file fn #:exists 'truncate/replace one)
               ;; a #f filename means return the contents as a string
               (let ([o (open-output-string)])
                 (parameterize ([current-output-port o])
                   (one)
                   (get-output-string o)))))
           ds
           fns))

    (define/public (render-one d ri fn)
      (render-part d ri))

    (define/public (render-part d ri)
      (list
       (when (part-title-content d)
         (render-content (part-title-content d) d ri))
       (render-flow (part-flow d) d ri #f)
       (map (lambda (s) (render-part s ri))
            (part-parts d))))

    (define/public (render-content c part ri)
      (apply append (map (lambda (i) (render-element i part ri)) c)))

    (define/public (render-paragraph p part ri)
      (render-content (paragraph-content p) part ri))

    (define/public (render-flow p part ri start-inline?)
      (if (null? (flow-paragraphs p))
          null
          (append
           (render-block (car (flow-paragraphs p))
                         part ri start-inline?)
           (apply append
                  (map (lambda (p)
                         (render-block p part ri #f))
                       (cdr (flow-paragraphs p)))))))

    (define/public (render-block p part ri inline?)
      (cond
        [(table? p) (if (auxiliary-table? p)
                      (render-auxiliary-table p part ri)
                      (render-table p part ri inline?))]
        [(itemization? p) (render-itemization p part ri)]
        [(blockquote? p) (render-blockquote p part ri)]
        [(delayed-block? p) 
         (render-block (delayed-block-blocks p ri) part ri inline?)]
        [else (render-paragraph p part ri)]))

    (define/public (render-auxiliary-table i part ri)
      null)

    (define/public (render-table i part ri inline?)
      (map (lambda (d) (if (flow? i) (render-flow d part ri #f) null))
           (apply append (table-flowss i))))

    (define/public (render-itemization i part ri)
      (map (lambda (d) (render-flow d part ri #t))
           (itemization-flows i)))

    (define/public (render-blockquote i part ri)
      (map (lambda (d) (render-block d part ri #f))
           (blockquote-paragraphs i)))

    (define/public (render-element i part ri)
      (cond
        [(string? i) (render-other i part ri)] ; short-cut for common case
        [(and (link-element? i)
              (null? (element-content i)))
         (let ([v (resolve-get part ri (link-element-tag i))])
           (if v
             (render-content (strip-aux (car v)) part ri)
             (render-content (list "[missing]") part ri)))]
        [(element? i)
         (when (render-element? i)
           ((render-element-render i) this part ri))
         (render-content (element-content i) part ri)]
        [(delayed-element? i)
         (render-content (delayed-element-content i ri) part ri)]
        [(part-relative-element? i)
         (render-content (part-relative-element-content i ri) part ri)]
        [else (render-other i part ri)]))

    (define/public (render-other i part ri)
      (list i))

    ;; ----------------------------------------

    (define copied-srcs (make-hash))
    (define copied-dests (make-hash))

    (define/public (install-file fn)
      (if refer-to-existing-files
          (if (string? fn)
              (string->path fn)
              fn)
          (let ([normalized (normal-case-path (simplify-path (path->complete-path fn)))])
            (or (hash-ref copied-srcs normalized #f)
                (let ([src-dir (path-only fn)]
                      [dest-dir (get-dest-directory #t)]
                      [fn (file-name-from-path fn)])
                  (let ([src-file (build-path (or src-dir (current-directory)) fn)]
                        [dest-file (build-path (or dest-dir (current-directory)) fn)]
                        [next-file-name (lambda (dest)
                                          (let-values ([(base name dir?) (split-path dest)])
                                            (build-path
                                             base
                                             (let ([s (path-element->string (path-replace-suffix name #""))])
                                               (let ([n (regexp-match #rx"^(.*)_([0-9]+)$" s)])
                                                 (format "~a_~a~a"
                                                         (if n (cadr n) s)
                                                         (if n (add1 (string->number (caddr n))) 2)
                                                         (let ([ext (filename-extension name)])
                                                           (if ext
                                                               (bytes-append #"." ext)
                                                               ""))))))))])
                    (let-values ([(dest-file normalized-dest-file)
                                  (let loop ([dest-file dest-file])
                                    (let ([normalized-dest-file 
                                           (normal-case-path (simplify-path (path->complete-path dest-file)))])
                                      (if (file-exists? dest-file)
                                          (cond
                                           [(call-with-input-file*
                                             src-file
                                             (lambda (src)
                                               (call-with-input-file* 
                                                dest-file
                                                (lambda (dest)
                                                  (or (equal? (port-file-identity src)
                                                              (port-file-identity dest))
                                                      (let loop ()
                                                        (let ([s (read-bytes 4096 src)]
                                                              [d (read-bytes 4096 dest)])
                                                          (and (equal? s d)
                                                               (or (eof-object? s) (loop))))))))))
                                            ;; same content at that destination
                                            (values dest-file normalized-dest-file)]
                                           [(hash-ref copied-dests normalized-dest-file #f)
                                            ;; need a different file
                                            (loop (next-file-name dest-file))]
                                           [else
                                            ;; replace the file
                                            (delete-file dest-file)
                                            (values dest-file normalized-dest-file)])
                                          ;; new file
                                          (values dest-file normalized-dest-file))))])
                      (unless (file-exists? dest-file)
                        (copy-file src-file dest-file))
                      (hash-set! copied-dests normalized-dest-file #t)
                      (let ([result (path->string (file-name-from-path dest-file))])
                        (hash-set! copied-srcs normalized result)
                        result))))))))

    ;; ----------------------------------------

    (define/private (do-table-of-contents part ri delta quiet depth)
      (make-table #f (generate-toc part
                                   ri
                                   (+ delta
                                      (length (collected-info-number
                                               (part-collected-info part ri))))
                                   #t
                                   quiet
                                   depth
                                   null)))

    (define/public (table-of-contents part ri)
      (do-table-of-contents part ri -1 not +inf.0))

    (define/public (local-table-of-contents part ri style)
      (do-table-of-contents part ri -1 not (if (eq? style 'immediate-only) 
                                               1
                                               +inf.0)))

    (define/public (quiet-table-of-contents part ri)
      (do-table-of-contents part ri 1 (lambda (x) #t) +inf.0))

    (define/private (generate-toc part ri base-len skip? quiet depth prefixes)
      (let* ([number (collected-info-number (part-collected-info part ri))]
             [prefixes (if (part-tag-prefix part)
                           (cons (part-tag-prefix part) prefixes)
                           prefixes)]
             [subs
              (if (and (quiet (and (part-style? part 'quiet)
                                   (not (= base-len (sub1 (length number))))))
                       (positive? depth))
                  (apply append (map (lambda (p)
                                       (generate-toc p ri base-len #f quiet (sub1 depth) prefixes))
                                     (part-parts part)))
                  null)])
        (if skip?
            subs
            (let ([l (cons
                      (list (make-flow
                             (list
                              (make-paragraph
                               (list
                                (make-element
                                 'hspace
                                 (list (make-string (* 2 (- (length number)
                                                            base-len))
                                                    #\space)))
                                (make-link-element
                                 (if (= 1 (length number)) "toptoclink" "toclink")
                                 (append
                                  (format-number
                                   number
                                   (list (make-element 'hspace '(" "))))
                                  (or (part-title-content part) '("???")))
                                 (for/fold ([t (car (part-tags part))])
                                     ([prefix (in-list prefixes)])
                                   (convert-key prefix t))))))))
                      subs)])
              (if (and (= 1 (length number))
                       (or (not (car number)) ((car number) . > . 1)))
                  (cons (list (make-flow
                               (list (make-paragraph
                                      (list (make-element 'hspace (list "")))))))
                        l)
                  l)))))

    ;; ----------------------------------------

    (super-new)))
