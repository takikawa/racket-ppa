#lang scheme/base

(provide find-relative-path
         explode-path
         simple-form-path
         normalize-path
         filename-extension
         file-name-from-path
         path-only)

(define (simple-form-path p)
  (unless (path-string? p)
    (raise-type-error 'simple-form-path "path or string" p))
  (simplify-path (path->complete-path p)))

;; Note that normalize-path does not normalize the case
(define normalize-path
  (letrec ([resolve-all
            (lambda (path wrt)
              (let ([orig-path (if (and wrt (not (complete-path? path)))
                                   (path->complete-path path wrt)
                                   path)])
                (let loop ([full-path orig-path][seen-paths (list orig-path)])
                  (let ([resolved (resolve-path full-path)])
                    (if (equal? resolved full-path)
                        (do-normalize-path resolved #f)
                        (let ([path (if (relative-path? resolved)
                                        (build-path
                                         (let-values ([(base name dir?) (split-path full-path)])
                                           base)
                                         resolved)
                                        resolved)])
                          (if (member path seen-paths)
                              (error 'normalize-path "circular reference at ~s" path)
                              (let ([spath
                                     ;; Use simplify-path to get rid of ..s, which can
                                     ;;  allow the path to grow indefinitely in a cycle.
                                     ;; An exception must mean a cycle of links.
                                     (with-handlers ([exn:fail:filesystem?
                                                      (lambda (x)
                                                        (error 'normalize-path "circular reference at ~s" path))])
                                       (simplify-path path))])
                                (loop spath (cons path seen-paths))))))))))]
           [resolve
            (lambda (path)
              (if (equal? path (resolve-path path))
                  path
                  (resolve-all path #f)))]
           [normalize-path
            (case-lambda
             [(orig-path) (do-normalize-path orig-path (current-directory))]
             [(orig-path wrt)
              (unless (complete-path? wrt)
                (raise-type-error 'normalize-path "complete path" wrt))
              (do-normalize-path orig-path wrt)])]
           [error-not-a-dir
            (lambda (path)
              (error 'normalize-path
                     "~s (within the input path) is not a directory or does not exist"
                     path))]
           [do-normalize-path
            (lambda (orig-path wrt)
              (let normalize ([path (cleanse-path orig-path)])
                (let-values ([(base name dir?) (split-path path)])
                  (cond
                   [(eq? name 'up)
                    (let up ([base (if (eq? base 'relative)
                                       wrt
                                       (resolve-all base wrt))])
                      (if (directory-exists? base)
                          (let-values ([(prev name dir?) (split-path base)])
                            (cond
                             [(not prev) 
                              (error 'normalize-path
                                     "root has no parent directory: ~s"
                                     orig-path)]
                             [else
                              (let ([prev
                                     (if (eq? prev 'relative)
                                         wrt
                                         (normalize prev))])
                                (cond
                                 [(eq? name 'same) (up prev)]
                                 [(eq? name 'up) (up (up prev))]
                                 [else prev]))]))
                          (error-not-a-dir base)))]
                   [(eq? name 'same)
                    (cond
                     [(eq? base 'relative) wrt]
                     [else (let ([n (normalize base)])
                             (if (directory-exists? n)
                                 n
                                 (error-not-a-dir n)))])]
                   [(not base) (path->complete-path path)]
                   [else
                    (let* ([base (if (eq? base 'relative)
                                     (normalize wrt)
                                     (normalize base))]
                           [path (if (directory-exists? base)
                                     (build-path base name)
                                     (error-not-a-dir base))]
                           [resolved (cleanse-path (resolve path))])
                      (cond
                       [(relative-path? resolved)
                        (normalize (build-path base resolved))]
                       [(complete-path? resolved)
                        resolved]
                       [else (path->complete-path resolved base)]))]))))])
    normalize-path))

;; Argument must be in simple form
(define (do-explode-path who orig-path simple?)
  (let loop ([path orig-path][rest '()])
    (let-values ([(base name dir?) (split-path path)])
      (when simple?
        (when (or (and base (not (path? base)))
                  (not (path? name)))
          (raise-type-error who 
                            "path in simple form (absolute, complete, and with no same- or up-directory indicators)"
                            orig-path)))
      (if (path? base)
          (loop base (cons name rest))
          (cons name rest)))))

(define (explode-path orig-path)
  (unless (path-string? orig-path)
    (raise-type-error 'explode-path "path or string" orig-path))
  (do-explode-path 'explode-path orig-path #f))

;; Arguments must be in simple form
(define (find-relative-path directory filename)
  (let ([dir (do-explode-path 'find-relative-path directory #t)]
        [file (do-explode-path 'find-relative-path filename #t)])
    (if (equal? (car dir) (car file))
        (let loop ([dir (cdr dir)]
                   [file (cdr file)])
          (cond [(null? dir) (if (null? file) filename (apply build-path file))]
                [(null? file) (apply build-path (map (lambda (x) 'up) dir))]
                [(equal? (car dir) (car file))
                 (loop (cdr dir) (cdr file))]
                [else
                 (apply build-path (append (map (lambda (x) 'up) dir) file))]))
        filename)))

(define (file-name who name)
  (unless (path-string? name)
    (raise-type-error who "path or string" name))
  (let-values ([(base file dir?) (split-path name)])
    (and (not dir?) (path? file) file)))

(define (file-name-from-path name)
  (file-name 'file-name-from-path name))

(define (path-only name)
  (unless (path-string? name)
    (raise-type-error 'path-only "path or string" name))
  (let-values ([(base file dir?) (split-path name)])
    (cond [dir? name]
          [(path? base) base]
          [else #f])))

;; name can be any string; we just look for a dot
(define (filename-extension name)
  (let* ([name (file-name 'filename-extension name)]
         [name (and name (path->bytes name))])
    (cond [(and name (regexp-match #rx#"[.]([^.]+)$" name)) => cadr]
          [else #f])))
