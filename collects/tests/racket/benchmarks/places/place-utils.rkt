#lang racket/base
(require racket/place
         racket/file
         (for-syntax racket/base
                     racket/file))

(provide splat
         splat-tmp
         barrier-m
         barrier
         places-wait
         place/base
         time-n)


(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define (splat-tmp txt)
  (define fn (make-temporary-file "place-benchmark~a" #f (current-directory)))
  (splat txt fn)
  fn)

(define (barrier-m pls)
  (for ([ch pls]) (place-channel-get ch))
  (for ([ch pls]) (place-channel-put ch 1)))

(define (barrier ch)
  (place-channel-put ch 0)
  (place-channel-get ch))

(define (places-wait pls)
  (for ([p pls]) (place-wait p)))

(define-syntax (place/base stx)
  (syntax-case stx ()
    [(_ (name ch) body ...)
      (begin
        (define (splat txt fn)
          (call-with-output-file fn #:exists 'replace
              (lambda (out)
                (write txt out))))

        (define module-path (make-temporary-file "place-worker-~a.rkt" #f))
        (define-values (base file-name isdir) (split-path module-path))
        (define worker-syntax
          (with-syntax ([module-name (datum->syntax #'name (string->symbol (path->string (path-replace-suffix file-name ""))))])
            #'(module module-name racket/base
                (require racket/place)
                (provide name)
                (define (name ch)
                    body ...))))
        (define module-path-str (path->string module-path))

        (splat (syntax->datum worker-syntax) module-path-str)

        (define place-syntax #`(dynamic-place #,module-path (quote name)))
        ;(write (syntax->datum place-syntax)) (newline)
        place-syntax)]))

(define-syntax (time-n stx)
  (syntax-case stx ()
    [(_ msg cnt body ...)
      #'(let-values ([(r ct rt gct)
                      (time-apply (lambda () body ...) null)])
          (displayln (list msg cnt ct rt gct))
          (if (pair? r) (car r) r))
#|
      #'(time body ...)
|#
]))
          
