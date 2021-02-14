#lang racket/base
(require (prefix-in pl- '#%place)
         '#%boot
         (only-in '#%paramz parameterization-key)
         '#%place-struct
         '#%flfxnum
         (only-in '#%unsafe unsafe-make-custodian-at-root)
         (only-in '#%foreign cpointer?)
         (only-in '#%network tcp-port? tcp-listener?))

(provide th-dynamic-place
         ;th-dynamic-place*
         th-place-wait
         th-place-kill
         th-place-break
         th-place-channel
         th-place-channel-put
         th-place-channel-get
         th-place-channel?
         th-place
         th-place?
         th-place-message-allowed?
         th-place-dead-evt
         )

(define-syntax-rule (copiers fXvector-copy! fXvector-set! fXvector-ref)
  (define (fXvector-copy! vec dest-start flv start end)
    (let ([len (- end start)])
      (for ([i (in-range len)])
        (fXvector-set! vec (+ i dest-start)
                       (fXvector-ref flv (+ i start)))))))

(copiers fxvector-copy! fxvector-set! fxvector-ref)
(copiers flvector-copy! flvector-set! flvector-ref)



(define-struct TH-place (th ch cust cust-box result-box)
  #:property prop:evt (lambda (x) (TH-place-channel-in (TH-place-ch x))))
(define th-place? TH-place?)
(define th-place TH-place)

(define (make-th-async-channel)
  (define ch (make-channel))
  (values
    (thread
      (lambda ()
        (let loop ()
          (let ([v (thread-receive)])
            (channel-put ch v)
            (loop)))))
    ch))

(define (th-dynamic-place mod funcname)
  (unless (or (module-path? mod) (path? mod) (resolved-module-path? mod))
    (raise-argument-error 'dynamic-place "(or/c module-path? path? resolved-module-path?)" 0 mod funcname))
  (unless (symbol? funcname)
    (raise-argument-error 'dynamic-place "symbol?" 1 mod funcname))
  (define-values (pch cch) (th-place-channel))
  (define cust (unsafe-make-custodian-at-root))
  (define cust-box (make-custodian-box cust #t))
  (define result-box (box 0))
  (define plumber (make-plumber))
  (define done? #f)
  (define th
    (parameterize ([current-custodian cust])
      (thread
       (lambda ()
         (with-continuation-mark
          parameterization-key
          (get-original-parameterization)
          (parameterize ([current-namespace (make-base-namespace)]
                         [current-custodian cust]
                         [exit-handler (lambda (v)
                                         (plumber-flush-all plumber)
                                         (set-box! result-box (if (byte? v) v 0))
                                         (custodian-shutdown-all cust))]
                         [current-plumber plumber])
            (dynamic-wind
             void
             (lambda ()
               ((dynamic-require mod funcname) cch)
               (plumber-flush-all plumber)
               (set! done? #t))
             (lambda ()
               (unless done?
                 (set-box! result-box 1))))))))))
  (parameterize ([current-custodian cust])
    ;; When main thread ends, all threads, etc., should end:
    (thread (lambda () (thread-wait th) (custodian-shutdown-all cust))))
  (TH-place th pch cust cust-box result-box))

(define (th-place-wait pl) (sync (TH-place-cust-box pl)) (unbox (TH-place-result-box pl)))
(define (th-place-kill pl) (set-box! (TH-place-result-box pl) 1) (custodian-shutdown-all (TH-place-cust pl)))
(define (th-place-break pl kind) (break-thread (TH-place-th pl) kind))
(define (th-place-dead-evt pl) (wrap-evt (TH-place-cust-box pl) (lambda (v) pl)))
(define (th-place-channel)
  (define-values (as ar) (make-th-async-channel))
  (define-values (bs br) (make-th-async-channel))
  (define pch (TH-place-channel ar bs))
  (define cch (TH-place-channel br as))
  (values pch cch))

(define (deep-copy x)
  (define ht (make-hasheq))
  (define (record v new-v)
    (hash-set! ht v new-v)
    new-v)
  (define (with-placeholder o mk)
    (define ph (make-placeholder #f))
    (hash-set! ht o ph)
    (define new-o (mk))
    (placeholder-set! ph new-o)
    new-o)
  (define (dcw o)
    (cond
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? keyword? TH-place-channel?)) o]
      [(hash-ref ht o #f)
       => values]
      [(cond
        [(path-for-some-system? o) o]
        [(bytes? o) (if (pl-place-shared? o) o (record o (bytes-copy o)))]
        [(fxvector? o) (if (pl-place-shared? o)
                           o
                           (let* ([c (make-fxvector (fxvector-length o))])
                             (fxvector-copy! c 0 o 0 (fxvector-length o))
                             (record o c)))]
        [(flvector? o) (if (pl-place-shared? o)
                           o
                           (let* ([c (make-flvector (flvector-length o))])
                             (flvector-copy! c 0 o 0 (flvector-length o))
                             (record o c)))]
        [else #f])
        => values]
      [(TH-place? o) (dcw (TH-place-ch o))]
      [(pair? o)
       (with-placeholder
        o
        (lambda ()
          (cons (dcw (car o)) (dcw (cdr o)))))]
      [(vector? o)
       (define new-v (make-vector (vector-length o)))
       (vector-copy! new-v 0 o)
       (define r (record o new-v))
       (for ([i (in-naturals)]
             [v (in-vector r)])
         (vector-set! new-v i (dcw v)))
       r]
      [(hash? o)
       (with-placeholder
        o
        (lambda ()
          (cond
           [(hash-equal? o)
            (for/fold ([nh (hash)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))]
           [(hash-eq? o)
            (for/fold ([nh (hasheq)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))]
           [else ; (hash-eqv? o)
            (for/fold ([nh (hasheqv)]) ([p (in-hash-pairs o)])
              (hash-set nh (dcw (car p)) (dcw (cdr p))))])))]
      [(and (port? o)
            (or (file-stream-port? o)
                (tcp-port? o)))
       o]
      [(cpointer? o) o]
      [(tcp-listener? o) o]
      [(and (struct? o)
            (prefab-struct-key o))
       =>
       (lambda (key)
         (with-placeholder
          o
          (lambda ()
            (apply make-prefab-struct
                   key
                   (map dcw (cdr (vector->list (struct->vector o))))))))]
      [else (raise-mismatch-error 'place-channel-put "cannot transmit a message containing value: " o)]))

  (make-reader-graph (dcw x)))


(define (th-place-channel-put pl msg)
  (define th
    (cond
      [(TH-place? pl) (TH-place-channel-out (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-out pl)]
      [else (raise-argument-error 'place-channel-put "(or/c place? place-channel?)" pl)]))
  (void (thread-send th (deep-copy msg) #f)))

(define (th-place-channel-get pl)
  (channel-get
    (cond
      [(TH-place? pl) (TH-place-channel-in (TH-place-ch pl))]
      [(TH-place-channel? pl) (TH-place-channel-in pl)]
      [else (raise-argument-error 'place-channel-get "(or/c place? place-channel?)" pl)])))

(define (th-place-channel? pl)
  (or (TH-place? pl)
      (TH-place-channel? pl)))

(define (th-place-message-allowed? x)
  (define (dcw o)
    (cond
      [(ormap (lambda (x) (x o)) (list number? char? boolean? null? void? string? symbol? keyword? TH-place-channel?
                                       path? bytes? fxvector? flvector? TH-place?)) #t]
      [(pair? o) (and (dcw (car o)) (dcw (cdr o)))]
      [(vector? o)
       (for/fold ([nh #t]) ([i (in-vector o)])
        (and nh (dcw i)))]
      [(hash? o)
       (for/fold ([nh #t]) ([p (in-hash-pairs o)])
         (and nh (dcw (car p)) (dcw (cdr p))))]
      [(struct? o)
        (define key (prefab-struct-key o))
        (when (not key)
          (error "Must be a prefab struct"))
        (for/fold ([nh #t]) ([p (cdr (vector->list (struct->vector o)))])
          (and nh (dcw p)))]
      [(port? o) (or (file-stream-port? o) (tcp-port? o))]
      [(cpointer? o) #t]
      [(tcp-listener? o) #t]
      [else #f]))

  (dcw x)
  #t)
