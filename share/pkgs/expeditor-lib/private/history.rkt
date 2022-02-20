#lang racket/base
(require racket/fixnum
         "param.rkt"
         "ee.rkt")

;; See "../main.rkt"

(provide history-search-bwd history-search-fwd
         update-history! history-fast-forward! entry-modified?
         ee-get-history ee-set-history!)

(define search
  (lambda (ee pred? get-bwd set-bwd! get-fwd set-fwd!)
    (let loop ([bwd (get-bwd ee)]
               [now (eestate-histnow ee)]
               [fwd (get-fwd ee)])
      (and (not (null? bwd))
           (let ([s (car bwd)])
             (if (pred? s)
                 (begin
                   (set-bwd! ee (cdr bwd))
                   (set-eestate-histnow! ee s)
                   (set-fwd! ee (cons now fwd))
                   s)
                 (loop (cdr bwd) s (cons now fwd))))))))

(define history-search-bwd
  (lambda (ee pred?)
    (search ee pred? eestate-histbwd set-eestate-histbwd!
            eestate-histfwd set-eestate-histfwd!)))

(define history-search-fwd
  (lambda (ee pred?)
    (search ee pred? eestate-histfwd set-eestate-histfwd!
            eestate-histbwd set-eestate-histbwd!)))

(define history->list
  (lambda (ee)
    (cdr `(,@(reverse (eestate-histfwd ee))
           ,(eestate-histnow ee)
           ,@(eestate-histbwd ee)))))

(define trim-history
  (lambda (ls)
    (let ([n (ee-history-limit)])
      (if (> (length ls) n)
          (for/list ([s (in-list ls)]
                     [i (in-range n)])
            s)
          ls))))

(define (clean-history-item s)
  (cond
    [(current-expeditor-history-whitespace-trim-enabled)
     (regexp-replace #px"\\s+$" s "")]
    [else s]))

(define update-history!
  (lambda (ee entry)
    (define (all-whitespace? s)
      (let ([n (string-length s)])
        (let f ([i 0])
          (or (fx= i n)
              (and (memv (string-ref s i) '(#\space #\newline))
                   (f (fx+ i 1)))))))
    (let ([s (clean-history-item (entry->string entry))] [ls (history->list ee)])
      (set-eestate-histbwd! ee
                            (if (or (all-whitespace? s)
                                    (and (not (null? ls))
                                         (equal? s (car ls))))
                                ls
                                (begin
                                  (set-eestate-histnew! ee (fx+ (eestate-histnew ee) 1))
                                  (trim-history (cons s ls))))))
    (set-eestate-histnow! ee "")
    (set-eestate-histfwd! ee '())))

(define history-fast-forward!
  (lambda (ee)
    (set-eestate-histbwd! ee (history->list ee))
    (set-eestate-histnow! ee "")
    (set-eestate-histfwd! ee '())))

(define (entry-modified? ee entry)
  (not (string=? (entry->string entry) (eestate-histnow ee))))

(define ee-get-history
  (lambda (ee)
    (history->list ee)))

(define ee-set-history!
  (lambda (ee ls)
    (unless (and (list? ls) (andmap string? ls))
      (error 'ee-load-history "not a list of strings: ~e" ls))
    (set-eestate-histnew! ee 0)
    (set-eestate-histbwd! ee ls)
    (set-eestate-histnow! ee "")
    (set-eestate-histfwd! ee '())))
