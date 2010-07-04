#lang scheme/base

#|

This file is used by the contract system's
implementation, so it does not have contracts.
Use syntax/modcollapse instead.

|#

(require scheme/string
         scheme/list
         "modhelp.ss")

(define (collapse-module-path s relto-mp)
  ;; relto-mp should be a path, '(lib relative-path collection) or symbol,
  ;;   or '(file path) or a thunk that produces one of those
  
  ;; Used for 'lib paths, so it's always Unix-style
  (define (attach-to-relative-path-string elements relto)
    (let ([elem-str
           (substring
            (apply string-append
                   (map (lambda (i)
                          (string-append
                           "/"
                           (cond [(bytes? i) (bytes->string/locale i)]
                                 [(path? i) (path->string i)]
                                 [(eq? i 'up) ".."]
                                 [else i])))
                        (filter (lambda (x) (not (eq? x 'same)))
                                elements)))
            1)])
      (if (or (regexp-match #rx"^[.]/+[^/]*" relto)
              (not (regexp-match #rx"/" relto)))
          elem-str
          (let ([m (regexp-match #rx"^(.*/)/*[^/]*$" relto)])
            (string-append (cadr m) elem-str)))))
  
  (define (simpler-relpath path)
    (let loop ([s (regexp-replace* #px"(?<![.])[.]/" path "")])
      (let ([s2 (regexp-replace #rx"([^/.]*)/[.][.]/" s "")])
        (if (equal? s s2)
            s
            (loop s2)))))
  
  (define (add-main s)
    (if (regexp-match #rx"[.][^/]*$" s)
        s
        (string-append s "/main.ss")))
  
  (define (combine-relative-elements elements)
    
    (define (extract-base relto)
      (let-values ([(base n d?) (split-path relto)])
        (if (eq? base 'relative) 
            'same 
            (if (not base)
                relto ; strange case: relto is a root directory
                base))))
    
    ;; Used for 'file paths, so it's platform specific:
    (define (attach-to-relative-path relto)
      (apply build-path
             (extract-base relto)
             (map (lambda (i) (if (bytes? i) (bytes->path i) i))
                  elements)))
    
    (when (procedure? relto-mp) (set! relto-mp (relto-mp)))
    (when (symbol? relto-mp) (set! relto-mp `(lib ,(symbol->string relto-mp))))
    (cond
      [(or (path? relto-mp) (and (string? relto-mp) (ormap path? elements)))
       (apply build-path
              (extract-base relto-mp)
              (map (lambda (x) (if (bytes? x) (bytes->path x) x))
                   elements))]
      [(string? relto-mp)
       (bytes->string/locale
        (apply
         bytes-append
         (cond [(regexp-match #rx#"^(.*)/[^/]*$"
                              (string->bytes/locale relto-mp))
                => cadr]
               [else #"."])
         (map (lambda (e)
                (cond [(eq? e 'same) #"/."]
                      [(eq? e 'up) #"/.."]
                      [else (bytes-append
                             #"/" (if (path? e) (path->bytes e) e))]))
              elements)))]
      [(eq? (car relto-mp) 'file)
       (let ([path ((if (ormap path? elements) values path->string)
                    (attach-to-relative-path (cadr relto-mp)))])
         (if (path? path) path `(file ,path)))]
      [(eq? (car relto-mp) 'lib)
       (let ([relto-mp (if (null? (cddr relto-mp))
                           ;; old style => add 'mzlib
                           ;; new style => add main.ss or split
                           (let ([m (regexp-match-positions #rx"[/]" (cadr relto-mp))])
                             (if m
                                 ;; new style: split
                                 `(lib ,(substring (cadr relto-mp) (cdar m))
                                       ,(substring (cadr relto-mp) 0 (caar m)))
                                 (if (regexp-match? #rx"[.]" (cadr relto-mp))
                                     ;; old style:
                                     `(lib ,(cadr relto-mp) "mzlib")
                                     ;; new style, add "main.ss":
                                     `(lib "main.ss" ,(cadr relto-mp)))))
                           ;; already has at least two parts:
                           relto-mp)])
         (let ([path (attach-to-relative-path-string
                      elements (apply string-append
                                      (append
                                       (map (lambda (s)
                                              (string-append s "/"))
                                            (cddr relto-mp))
                                       (list (cadr relto-mp)))))])
           (let ([simpler (simpler-relpath path)])
             (let ([m (regexp-match #rx"^(.*)/([^/]*)$" simpler)])
               (if m
                   (normalize-lib `(lib ,(caddr m) ,(cadr m)))
                   (error 'combine-relative-elements
                          "relative path escapes collection: ~s relative to ~s"
                          elements relto-mp))))))]
      [(eq? (car relto-mp) 'planet)
       (let ([relto-mp
              ;; make sure relto-mp is in long form:
              (if (null? (cddr relto-mp))
                  (normalize-planet relto-mp)
                  relto-mp)])
         (let ([pathstr (simpler-relpath
                         (attach-to-relative-path-string
                          elements 
                          (apply string-append
                                 (append
                                  (map (lambda (s)
                                         (string-append s "/"))
                                       (cdddr relto-mp))
                                  (list (cadr relto-mp))))))])
           (normalize-planet `(planet ,pathstr ,(caddr relto-mp)))))]
      [else (error 'combine-relative-elements
                   "don't know how to deal with: ~s" relto-mp)]))
  
  (define (normalize-lib s)
    (if (null? (cddr s))
        ;; single-string version:
        (let ([e (cadr s)])
          (cond
            [(regexp-match? #rx"[.]" e)
             ;; It has a suffix:
             (if (regexp-match? #rx"/" e)
                 ;; It has a path, so it's fine:
                 s
                 ;; No path, so add "mzlib/":
                 `(lib ,(string-append "mzlib/" e)))]
            [(regexp-match? #rx"/" e)
             ;; It has a separator, so add a suffix:
             `(lib ,(string-append e ".ss"))]
            [else
             ;; No separator or suffix, so add "/main.ss":
             `(lib ,(string-append e "/main.ss"))]))
        ;; multi-string version:
        (if (regexp-match? #rx"[.]" (cadr s))
            ;; there's a suffix, so we can collapse to a single string:
            `(lib ,(string-join (append (cddr s) 
                                        (list (cadr s)))
                                "/"))
            ;; No suffix, so we must keep the old style:
            s)))
  
  (define (normalize-planet s)
    (cond
      [(symbol? (cadr s))
       ;; normalize via string form:
       (normalize-planet `(planet ,(symbol->string (cadr s))))]
      [(null? (cddr s))
       ;; normalize to long form:
       (let* ([strs (regexp-split #rx"/" (cadr s))])
         (let ([owner (car strs)]
               [pkg+vers (regexp-split #rx":" (cadr strs))]
               [path (cddr strs)])
           `(planet ,(if (null? path)
                         "main.ss"
                         (let ([str (last path)])
                           (if (regexp-match? #rx"[.]" str)
                               str
                               (string-append str ".ss"))))
                    (,owner
                     ,(string-append (car pkg+vers) ".plt")
                     ,@(if (null? (cdr pkg+vers))
                           null
                           `(,(string->number (cadr pkg+vers))
                             . ,(if (null? (cddr pkg+vers))
                                    null
                                    (list
                                     (let ([vers (caddr pkg+vers)])
                                       (cond
                                         [(regexp-match? #rx"<=" vers)
                                          `(- ,(string->number (substring vers 2)))]
                                         [(regexp-match? #rx">=" vers)
                                          (string->number (substring vers 2))]
                                         [(regexp-match? #rx"=" vers)
                                          `(= ,(string->number (substring vers 1)))]
                                         [(regexp-match #rx"(.*)-(.*)" vers)
                                          => (lambda (m)
                                               `(,(string->number (cadr m))
                                                 ,(string->number (caddr m))))]
                                         [(string->number vers)
                                          => (lambda (n) n)]
                                         [else (error 'collapse-module-path
                                                      "confused when normalizing planet path: ~e"
                                                      s)])))))))
                    ,@(if (null? path)
                          null
                          (reverse (cdr (reverse path)))))))]
      [else 
       ;; Long form is the normal form, but see if we need to split up the
       ;; path elements:
       (let ([base (cadr s)]
             [rest (cdddr s)]
             [split? (lambda (s)
                       (regexp-match? #rx"/" s))])
         (if (or (split? base)
                 (ormap split? rest))
             ;; need to split some paths:
             (let ([split (lambda (s)
                            (regexp-split #rx"/" s))])
               (let ([bases (split base)]
                     [rests (map split rest)])
                 (list* (car s)
                        (last bases)
                        (caddr s)
                        (append
                         (apply append rests)
                         (drop-right bases 1)))))
             ;; already in normal form:
             s))]))
  
  (cond [(string? s)
         ;; Parse Unix-style relative path string
         (combine-relative-elements (explode-relpath-string s))]
        [(symbol? s)
         ;; Convert to `lib' form:
         (normalize-lib `(lib ,(symbol->string s)))]
        [(and (or (not (pair? s)) (not (list? s))) (not (path? s)))
         #f]
        [(or (path? s) (eq? (car s) 'file))
         (let ([p (if (path? s) s (cadr s))])
           (if (absolute-path? p)
               s
               (let loop ([p p] [elements null])
                 (let-values ([(base name dir?) (split-path p)])
                   (cond [(eq? base 'relative)
                          (combine-relative-elements (cons name elements))]
                         [else (loop base (cons name elements))])))))]
        [(eq? (car s) 'lib) (normalize-lib s)]
        [(eq? (car s) 'planet) (normalize-planet s)]
        [(eq? (car s) 'quote) s]
        [else #f]))

(define (collapse-module-path-index mpi relto-mp)
  (define (force-relto relto-mp)
    (if (procedure? relto-mp) 
        (relto-mp)
        relto-mp))
  (let-values ([(path base) (module-path-index-split mpi)])
    (if path
        (collapse-module-path
         path
         (lambda ()
           (cond
            [(module-path-index? base)
             (collapse-module-path-index base relto-mp)]
            [(resolved-module-path? base)
             (let ([n (resolved-module-path-name base)])
               (if (path? n)
                   n
                   (force-relto relto-mp)))]
            [else (force-relto relto-mp)])))
        (force-relto relto-mp))))

(provide collapse-module-path
         collapse-module-path-index)
