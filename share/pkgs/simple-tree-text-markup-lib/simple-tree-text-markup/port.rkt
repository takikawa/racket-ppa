#lang racket/base
(require racket/contract
         (only-in simple-tree-text-markup/data markup?))
(provide
 (contract-out
  (make-markup-output-port/unsafe ((any/c . -> . markup?) . -> . (values output-port? (-> markup?))))
  (make-markup-output-port ((any/c . -> . markup?) . -> . (values output-port? (-> markup?))))))

(require simple-tree-text-markup/construct
         (only-in racket/list dropf)
         (only-in racket/string string-split))

(define (markup-accumulator special->markup)
  (let ((horizontals '())
        (verticals '()))
    (define (add-to-horizontals! fragment)
      (set! horizontals (cons fragment horizontals)))

    (define (write-out bytes start end non-block? breakable?)
      (let* ((text (bytes->string/utf-8 bytes #\? start end))
             (lines (string-split text "\n" #:trim? #f)))
        (when (pair? lines)
          (add-to-horizontals! (car lines))
          (when (pair? (cdr lines))
            (let ((rev-lines (reverse (cdr lines))))
              (set! verticals (append (cdr rev-lines)
                                      (cons (apply horizontal (reverse horizontals)) verticals)))
              (set! horizontals '())
              (add-to-horizontals! (car rev-lines))))))
      (- end start))

    (define (write-out-special thing non-block? breakable?)
      (set! horizontals (cons (special->markup thing) horizontals))
      #t)

    (define (get-markup)
      (apply vertical
             (reverse
              ;; drop trailing newlines
              (dropf (if (null? horizontals)
                         verticals
                         (cons (apply horizontal (reverse horizontals))
                               verticals))
                     (lambda (h)
                       (equal? h ""))))))

    (values write-out write-out-special get-markup)))

(define (make-markup-output-port/unsafe special->markup)

  (define-values (write-out write-out-special get-markup) (markup-accumulator special->markup))
    
  (values 
   (make-output-port 'markup always-evt write-out void write-out-special)
   get-markup))


; see documentation for make-output-port
(define (make-markup-output-port special->markup)
  (let* ((lock (make-semaphore 1))
         (lock-peek-evt (semaphore-peek-evt lock)))
    
    (define-values (write-out write-out-special get-markup) (markup-accumulator special->markup))

    (values 
     (make-output-port 'markup
                       lock-peek-evt
                       (lambda (bytes start end non-block? breakable?)
                         (if (semaphore-try-wait? lock)
                             (let ((result (write-out bytes start end non-block? breakable?)))
                               (semaphore-post lock)
                               result)
                             ; Cheap strategy: block until the list is unlocked,
                             ;   then return 0, so we get called again
                             (wrap-evt
                              lock-peek-evt
                              (lambda (x) 0))))
                       void ; close
                       (lambda (thing non-block? breakable?)
                         (if (semaphore-try-wait? lock)
                             (begin
                               (write-out-special thing non-block? breakable?)
                               (semaphore-post lock)
                               #t)
                             (wrap-evt
                              lock-peek-evt
                              (lambda (x) #f)))))
     (lambda ()
       (call-with-semaphore lock get-markup)))))

