#lang racket/base
(require net/url
         racket/list
         racket/match
         racket/contract)
(require web-server/private/util)
(define url->path/c
  ((url?) () . ->* . (values path? (listof path-piece?))))

(provide/contract
 [url->path/c contract?]
 [make-url->path (path-string? . -> . url->path/c)]
 [make-url->valid-path (url->path/c . -> . url->path/c)]
 [filter-url->path (regexp? url->path/c . -> . url->path/c)])

(define (restrict l)
  (define count
    (let loop ([end-in-file? #f] [depth 0] [l l])
      (and (not (negative? depth))
           (match l
             [(list)
              (if end-in-file?
                  (sub1 depth)
                  depth)]
             [(list-rest (or ".." 'up) rst)
              (loop #f (sub1 depth) rst)]
             [(list-rest (or "" 'same) rst)
              (loop #f depth rst)]
             [(list-rest _ rst)
              (loop #t (add1 depth) rst)]))))
  (and count (not (negative? count))))

(module+ test
  (require rackunit)

  (check-equal? (restrict (list))
                #t)
  (check-equal? (restrict (list 'up))
                #f)
  (check-equal? (restrict (list ".."))
                #f)
  (check-equal? (restrict (list 'same))
                #t)
  (check-equal? (restrict (list 'same ".."))
                #f)

  (check-equal? (restrict (list "foo" 'up "bar"))
                #t)
  (check-equal? (restrict (list "foo" 'up 'up "bar"))
                #f)
  (check-equal? (restrict (list 'up "bar"))
                #f)
  (check-equal? (restrict (list "foo" "bar" 'up "bar"))
                #t)
  (check-equal? (restrict (list "foo" "bar" 'up 'up 'up "bar"))
                #f)

  (check-equal? (restrict (list "foo" 'same "bar" 'up 'up 'up "bar"))
                #f)
  (check-equal? (restrict (list "foo" "" "bar" 'up 'up 'up "bar"))
                #f)
  (check-equal? (restrict (list "foo" "bar" 'up "bar" 'same))
                #t)
  (check-equal? (restrict (list "foo" "bar" 'up "bar" ""))
                #t))

(define (build-path* . l)
  (if (empty? l)
      (build-path 'same)
      (apply build-path l)))

(define ((make-url->path base) u)
  (define nbase (path->complete-path base))
  ;; construct a new url with just the path so that we can use `url->path`
  ;; otherwise we'll get an absolute path
  (define path-from-url (url->path (make-url #f #f #f #f #f (url-path u) null #f)))
  (unless (restrict (explode-path path-from-url))
    (error 'url->path "Illegal path: ~e outside base: ~e" 
           path-from-url
           base))
  (define the-path (path->complete-path path-from-url nbase))
  (define w/o-base (path-without-base nbase the-path))
  (values the-path w/o-base))

(module+ test
  (define-syntax-rule (first-value e)
    (call-with-values (λ () e) (λ a (car a))))
  (define-syntax-rule (second-value e)
    (call-with-values (λ () e) (λ a (cadr a))))
  (check-exn exn:fail? (λ () ((make-url->path "/home/samth/tmp/")
                              (string->url "http://localhost/..%2f..%2f..%2f..%2f..%2f..%2f..%2f..%2ffoo%2fbar"))))
  (check-exn exn:fail? (λ () ((make-url->path "/home/samth/tmp/")
                              (string->url "http://localhost/../x/y"))))
  (check-equal? (string->path "/home/samth/tmp/x/y")
                (first-value
                 ((make-url->path "/home/samth/tmp/")
                  (string->url "http://localhost/x/y"))))
  (check-equal? (simplify-path
                 (first-value
                  ((make-url->path "/home/samth/tmp/")
                   (string->url "http://localhost/x/../y")))
                 #f)
                (string->path "/home/samth/tmp/y"))
  (check-equal? (explode-path "x/y")
                (second-value
                 ((make-url->path "/home/samth/tmp/")
                  (string->url "http://localhost/x/y")))))

(define ((make-url->valid-path url->path) u)
  (let loop ([up (url-path u)])
    #;(printf "~S\n" `(url->valid-path ,(url->string u) ,up))
    (with-handlers ([exn:fail? (lambda (exn)
                                 #;((error-display-handler) (exn-message exn) exn)
                                 (if (empty? up)
                                     (raise exn)
                                     (loop (reverse (rest (reverse up))))))])
      (define-values (p w/o-base)
        (url->path (url-replace-path (lambda _ up) u)))
      (unless (or (file-exists? p) (link-exists? p))
        (raise (make-exn:fail:filesystem:exists (string->immutable-string (format "No valid path: ~a" p)) (current-continuation-marks))))
      (values p w/o-base))))

(define ((filter-url->path regex url->path) u)
  (define-values (p w/o-base) (url->path u))
  (if (regexp-match regex (path->string p))
      (values p w/o-base)
      (raise (make-exn:fail:filesystem:exists (string->immutable-string (format "Does not pass filter: ~a" p))
                                              (current-continuation-marks)))))
