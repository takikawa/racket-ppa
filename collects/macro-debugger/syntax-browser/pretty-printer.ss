
;; FIXME: Need to disable printing of structs with custom-write property

#lang scheme/base
(require scheme/list
         scheme/class
         scheme/pretty
         scheme/gui
         "pretty-helper.ss"
         "interfaces.ss"
         "params.ss"
         "prefs.ss")

(provide pretty-print-syntax)

;; pretty-print-syntax : syntax port partition -> range%
(define (pretty-print-syntax stx port primary-partition)
  (define range-builder (new range-builder%))
  (define-values (datum ht:flat=>stx ht:stx=>flat)
    (syntax->datum/tables stx primary-partition
                          (length (current-colors))
                          (current-suffix-option)))
  (define identifier-list
    (filter identifier? (hash-map ht:stx=>flat (lambda (k v) k))))
  (define (flat=>stx obj)
    (hash-ref ht:flat=>stx obj #f))
  (define (stx=>flat stx)
    (hash-ref ht:stx=>flat stx))
  (define (current-position)
    (let-values ([(line column position) (port-next-location port)])
      (sub1 position)))
  (define (pp-pre-hook obj port)
    (send range-builder set-start obj (current-position)))
  (define (pp-post-hook obj port)
    (let ([start (send range-builder get-start obj)]
          [end (current-position)]
          [stx (flat=>stx obj)])
      (when (and start stx)
        (send range-builder add-range stx (cons start end)))))
  (define (pp-extend-style-table identifier-list)
    (let* ([syms (map (lambda (x) (stx=>flat x)) identifier-list)]
           [like-syms (map syntax-e identifier-list)])
      (pretty-print-extend-style-table (pp-better-style-table)
                                       syms
                                       like-syms)))


  (unless (syntax? stx)
    (raise-type-error 'pretty-print-syntax "syntax" stx))
  (parameterize 
   ([pretty-print-pre-print-hook pp-pre-hook]
    [pretty-print-post-print-hook pp-post-hook]
    [pretty-print-size-hook pp-size-hook]
    [pretty-print-print-hook pp-print-hook]
    [pretty-print-current-style-table (pp-extend-style-table identifier-list)]
    [pretty-print-columns (current-default-columns)]
    ;; Printing parameters (mzscheme manual 7.9.1.4)
    [print-unreadable #t]
    [print-graph #f]
    [print-struct #f]
    [print-box #t]
    [print-vector-length #t]
    [print-hash-table #f]
    [print-honu #f])
   (pretty-print datum port)
   (new range%
        (range-builder range-builder)
        (identifier-list identifier-list))))

(define (pp-print-hook obj display-like? port)
  (cond [(syntax-dummy? obj)
         ((if display-like? display write) (syntax-dummy-val obj) port)]
        [(is-a? obj editor-snip%)
         (write-special obj port)]
        [else 
         (error 'pretty-print-hook "unexpected special value: ~e" obj)]))

(define (pp-size-hook obj display-like? port)
  (cond [(is-a? obj editor-snip%)
         (pretty-print-columns)]
        [(syntax-dummy? obj)
         (let ((ostring (open-output-string)))
           ((if display-like? display write) (syntax-dummy-val obj) ostring)
           (string-length (get-output-string ostring)))]
        [else #f]))

(define (pp-better-style-table)
  (basic-style-list)
  #; ;; Messes up formatting too much :(
  (let* ([pref (pref:tabify)]
         [table (car pref)]
         [begin-rx (cadr pref)]
         [define-rx (caddr pref)]
         [lambda-rx (cadddr pref)])
    (let ([style-list (hash-table-map table cons)])
      (pretty-print-extend-style-table
       (basic-style-list)
       (map car style-list)
       (map cdr style-list)))))

(define (basic-style-list)
  (pretty-print-extend-style-table
   (pretty-print-current-style-table)
   (map car basic-styles)
   (map cdr basic-styles)))
(define basic-styles
  '((define-values          . define)
    (define-syntaxes        . define-syntax)))

(define-local-member-name range:get-ranges)

;; range-builder%
(define range-builder%
  (class object%
    (define starts (make-hasheq))
    (define ranges (make-hasheq))

    (define/public (set-start obj n)
      (hash-set! starts obj n))

    (define/public (get-start obj)
      (hash-ref starts obj (lambda _ #f)))

    (define/public (add-range obj range)
      (hash-set! ranges obj (cons range (get-ranges obj))))

    (define (get-ranges obj)
      (hash-ref ranges obj (lambda () null)))

    (define/public (range:get-ranges) ranges)

    (super-new)))

;; range%
(define range%
  (class* object% (range<%>)
    (init range-builder)
    (init-field identifier-list)
    (super-new)

    (define ranges (hash-copy (send range-builder range:get-ranges)))

    (define/public (get-ranges obj)
      (hash-ref ranges obj (lambda _ null)))

    (define/public (all-ranges)
      sorted-ranges)

    (define/public (get-identifier-list)
      identifier-list)

    (define sorted-ranges
      (sort 
       (apply append 
              (hash-map
               ranges
               (lambda (k vs)
                 (map (lambda (v) (make-range k (car v) (cdr v))) vs))))
       (lambda (x y)
         (>= (- (range-end x) (range-start x))
             (- (range-end y) (range-start y))))))))

