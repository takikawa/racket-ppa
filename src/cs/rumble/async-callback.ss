
(define-record async-callback-queue (lock condition in wakeup))

(define (current-async-callback-queue)
  (place-async-callback-queue))

(define (async-callback-place-init!)
  (place-async-callback-queue (make-async-callback-queue (make-mutex)     ; ordered *before* `interrupts-disable`-as-lock
                                                         (make-condition)
                                                         '()
                                                         ;; Reset by `reset-async-callback-poll-wakeup!`:
                                                         void)))

(define (call-as-asynchronous-callback thunk)
  (async-callback-queue-call (current-async-callback-queue) thunk #f #t #t))

(define (post-as-asynchronous-callback thunk)
  (async-callback-queue-call (current-async-callback-queue) thunk #f #t #f)
  (void))

(define (async-callback-queue-call async-callback-queue thunk interrupts-disabled? need-atomic? wait-for-result?)
  (let* ([result-done? (box #f)]
         [result #f]
         [q async-callback-queue]
         [m (async-callback-queue-lock q)])
    (when interrupts-disabled? (enable-interrupts)) ; interrupt "lock" ordered after mutex
    (when need-atomic? (scheduler-start-atomic)) ; don't abandon engine after mutex is acquired
    (mutex-acquire m)
    (set-async-callback-queue-in! q (cons (lambda ()
                                            (set! result (thunk))
                                            (mutex-acquire m)
                                            (set-box! result-done? #t)
                                            (condition-broadcast (async-callback-queue-condition q))
                                            (mutex-release m))
                                          (async-callback-queue-in q)))
    ((async-callback-queue-wakeup q))
    (when wait-for-result?
      (let loop ()
        (unless (unbox result-done?)
          ;; Interrupts must be enabled so that the thread is deactivated
          ;; when we wait on the condition
          (condition-wait (async-callback-queue-condition q) m)
          (loop))))
    (mutex-release m)
    (when need-atomic? (scheduler-end-atomic))
    (when interrupts-disabled? (enable-interrupts))
    result))

(define make-async-callback-poll-wakeup (lambda () void))
(define (set-make-async-callback-poll-wakeup! make-wakeup)
  (set! make-async-callback-poll-wakeup make-wakeup)
  (reset-async-callback-poll-wakeup!))
(define (reset-async-callback-poll-wakeup!)
  (set-async-callback-queue-wakeup! (current-async-callback-queue) (make-async-callback-poll-wakeup)))

;; Returns callbacks to run in atomic mode. Interrupts must not be disabled
;; when ths function is called.
(define (poll-async-callbacks)
  (let ([q (current-async-callback-queue)])
    (mutex-acquire (async-callback-queue-lock q))
    (let ([in (async-callback-queue-in q)])
      (cond
       [(null? in)
        (mutex-release (async-callback-queue-lock q))
        '()]
       [else
        (set-async-callback-queue-in! q '())
        (mutex-release (async-callback-queue-lock q))
        (reverse in)]))))
