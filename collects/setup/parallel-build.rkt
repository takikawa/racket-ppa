#lang racket/base

(require compiler/cm
         racket/list
         racket/match
         racket/path
         setup/collects
         setup/parallel-do
         racket/class
         racket/future
         compiler/find-exe
         racket/place
         (for-syntax racket/base))

(provide parallel-compile
         parallel-compile-files)


(define Lock-Manager% (class object%
  (field (locks (make-hash)))
  (define/public (lock fn wrkr)
    (let ([v (hash-ref locks fn #f)])
      (hash-set! locks fn
        (if v
          (match v [(list w waitlst) (list w (append waitlst (list wrkr)))])
          (begin
            (wrkr/send wrkr (list 'locked))
            (list wrkr null))))
      (not v)))
  (define/public (unlock fn)
    (match (hash-ref locks fn)
      [(list w waitlst)
        (for ([x (second (hash-ref locks fn))])
          (wrkr/send x (list 'compiled)))
        (hash-remove! locks fn)]))
  (super-new)))

(define/class/generics Lock-Manager%
  (lm/lock lock fn wrkr)
  (lm/unlock unlock fn))

(define (->bytes x)
  (cond [(path? x) (path->bytes x)]
        [(string? x) (string->bytes/locale x)]))

(define CollectsQueue% (class* object% (WorkQueue<%>) 
  (init-field cclst printer append-error)
  (field (lock-mgr (new Lock-Manager%)))
  (field (hash (make-hash)))
  (inspect #f)

  (define/public (work-done work wrkr msg)
    (match (list work msg)
      [(list (list cc file last) (list result-type out err))
        (begin0
          (match result-type
            [(list 'ERROR msg)
              (append-error cc "making" (exn msg (current-continuation-marks)) out err "error")
              #t]
            [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
            [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
            ['DONE
              (define (string-!empty? s) (not (zero? (string-length s))))
              (when (ormap string-!empty? (list out err))
                (append-error cc "making" null out err "output"))
              (when last (printer (current-output-port) "made" "~a" (cc-name cc)))
              #t]
            [else (eprintf "Failed trying to match:\n~v\n" result-type)]))]
      [else
        (match work 
          [(list-rest (list cc file last) message)
            (append-error cc "making" null "" "" "error")
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            #t]
          [else
            (eprintf "work-done match cc failed.\n")
            (eprintf "trying to match:\n~a\n" (list work msg))
            (eprintf "FATAL\n")
            (exit 1)])]))
         
    ;; assigns a collection to each worker to be compiled
    ;; when it runs out of collections, steals work from other workers collections
    (define/public (get-job workerid)
      (define (find-job-in-cc cc id)
        (define (retry) (get-job workerid))
        (define (build-job cc file last)
          (values
            (list cc file last) 
            (list (->bytes (cc-name cc)) 
                  (->bytes (cc-path cc))
                  (->bytes file))))
        (match cc
          [(list)
            (hash-remove! hash id) (retry)]
          [(list (list cc (list) (list)))       ;empty collect
            (hash-remove! hash id) (retry)]
          [(cons (list cc (list) (list)) tail)  ;empty parent collect
            (hash-set! hash id tail) (retry)]
          [(cons (list cc (list) subs) tail)    ;empty srcs list
            (hash-set! hash id (append subs tail)) (retry)]
          [(cons (list cc (list file) subs) tail)
            (hash-set! hash id (append subs tail))
            (build-job cc file #t)]
          [(cons (list cc (cons file ft) subs) tail)
            (hash-set! hash id (cons (list cc ft subs) tail))
            (build-job cc file #f)]
          [else
            (eprintf "get-job match cc failed.\n")
            (eprintf "trying to match:\n~v\n" cc)]))


      ; find a cc 
      (cond
        ; lookup already assigned cc 
        [(hash-ref hash workerid #f) => (lambda (x)
          (find-job-in-cc x workerid))]
        ; get next cc from cclst
        [(pair? cclst)
          (define workercc (list (car cclst)))
          (set! cclst (cdr cclst))
          (hash-set! hash workerid workercc)
          (find-job-in-cc workercc workerid)]
        ; try to steal work from another workers cc
        [(hash-iterate-first hash) => (lambda (x)
          (find-job-in-cc (hash-iterate-value hash x)
                          (hash-iterate-key hash x)))]))
        ; no work left
        ; should never get here, get-job only called when the queue has work

    (define/public (has-jobs?)
      (define (hasjob?  cct)
        (let loop ([cct cct])
          (ormap (lambda (x) (or ((length (second x)) . > . 0) (loop (third x)))) cct)))

      (or (hasjob? cclst)
          (for/or ([cct (in-hash-values hash)])
            (hasjob? cct))))

    (define/public (jobs-cnt)
      (define (count-cct cct)
        (let loop ([cct cct])
          (apply + (map (lambda (x) (+ (length (second x)) (loop (third x)))) cct))))

      (+ (count-cct cclst)
         (for/fold ([cnt 0]) ([cct (in-hash-values hash)])
            (+ cnt (count-cct cct)))))
    (define/public (get-results) (void))
    (super-new)))

(define FileListQueue% (class* object% (WorkQueue<%>) 
  (init-field filelist handler)
  (field (lock-mgr (new Lock-Manager%)))
  (inspect #f)

  (define/public (work-done work wrkr msg)
    (match msg
      [(list result-type out err)
        (match result-type
          [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
          [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
          [(list 'ERROR msg) (handler 'error work msg out err) #t]
          ['DONE
            (define (string-!empty? s) (not (zero? (string-length s))))
            (if (ormap string-!empty? (list out err))
              (handler 'output work "" out err)
              (handler 'done work "" "" ""))
            #t])]
      [else
        (handler 'fatalerror (format "Error matching work: ~a queue ~a" work filelist) "" "") #t]))
         
    (define/public (get-job workerid)
      (match filelist
        [(cons hd tail)
            (define-values (dir file b) (split-path hd))
            (set! filelist tail)
            (values hd (list (->bytes hd) (->bytes dir) (->bytes file)))]
        [(list) null]))
    (define/public (has-jobs?) (not (null? filelist)))
    (define/public (jobs-cnt) (length filelist))
    (define/public (get-results) (void))
    (super-new)))


(define (build-parallel-build-worker-args)
  (list (find-exe #f)
        "-X"
        (path->string (current-collects-path))
        "-l"
        "setup/parallel-build-worker.rkt"))
  
(define (parallel-compile-files list-of-files
  #:worker-count [worker-count (processor-count)]
  #:handler [handler void])

  (parallel-do-event-loop #f
                          values ; identity function
                          (build-parallel-build-worker-args)
                          (make-object FileListQueue% list-of-files handler)
                          worker-count 999999999))

(define (parallel-compile worker-count setup-fprintf append-error collects-tree)
  (setup-fprintf (current-output-port) #f "--- parallel build using ~a processor cores ---" worker-count)
  (define collects-queue (make-object CollectsQueue% collects-tree setup-fprintf append-error))
  (if (place-enabled?)
    (places-parallel-build collects-queue worker-count 999999999)
    (parallel-do-event-loop #f values (build-parallel-build-worker-args) collects-queue worker-count 999999999)))

(define-syntax-rule (define-syntax-case (N a ...) b ...)
  (define-syntax (N stx)
    (syntax-case stx ()
      [(_ a ...) b ...])))

(define PlaceWorker% (class* object% (Worker<%>)
  (init-field [id 0]              
              [pl null])
             
  (define/public (send/msg msg) (place-channel-send pl msg))
  (define/public (recv/msg) (place-channel-receive pl))
  (define/public (get-id) id) 
  (define/public (get-out) pl)
  (define/public (kill) #f)
  (define/public (wait) (place-wait pl))
  (super-new))) 

(define-syntax-case (place/anon (ch) body ...)
 (with-syntax ([interal-def-name
                (syntax-local-lift-expression #'(lambda (ch) body ...))]
               [funcname #'OBSCURE_FUNC_NAME_%#%])
  (syntax-local-lift-provide #'(rename interal-def-name funcname))
  #'(let ([module-path (resolved-module-path-name
          (variable-reference->resolved-module-path
           (#%variable-reference)))])
   (place module-path (quote funcname)))))

(define (places-parallel-build jobqueue nprocs stopat)
  (define ps 
   (for/list ([i (in-range nprocs)])
     (place/anon (ch)
      (let ([cmc ((dynamic-require 'compiler/cm 'make-caching-managed-compile-zo))])
       (let loop ()
         (match (place-channel-receive ch)
           [(list 'DIE) void]
           [(list name dir file)
             (let ([dir (bytes->path dir)]
                   [file (bytes->path file)])
              (let ([out-str-port (open-output-string)]
                    [err-str-port (open-output-string)])
                (define (send/msg msg)
                  (place-channel-send ch msg))
                (define (send/resp type)
                  (send/msg (list type (get-output-string out-str-port) (get-output-string err-str-port))))
                (define (lock-client cmd fn)
                  (match cmd
                    ['lock 
                      (send/msg (list (list 'LOCK (path->bytes fn)) "" ""))
                      (match (place-channel-receive ch)
                        [(list 'locked) #t]
                        [(list 'compiled) #f])]
                    ['unlock (send/msg (list (list 'UNLOCK (path->bytes fn)) "" ""))]))
                (with-handlers ([exn:fail? (lambda (x)
                                 (send/resp (list 'ERROR (exn-message x))))])
                  (parameterize ([parallel-lock-client lock-client]
                                 [current-namespace (make-base-empty-namespace)]
                                 [current-directory dir]
                                 [current-load-relative-directory dir]
                                 [current-input-port (open-input-string "")]
                                 [current-output-port out-str-port]
                                 [current-error-port err-str-port])

                    (cmc (build-path dir file)))
                  (send/resp 'DONE))))
              (loop)]))))))


  (define workers (for/list ([i (in-range nprocs)]
                             [p ps]) 
                    (make-object PlaceWorker% i p)))
  (define (jobs?) (queue/has jobqueue))
  (define (empty?) (not (queue/has jobqueue)))

  (let loop ([idle workers]
             [inflight null]
             [count 0])
    (cond
      [(= count stopat) (printf "DONE AT LIMIT\n")]   
      [(and (empty?) (null? inflight)) (set! workers idle)] ; ALL DONE
      [(and (jobs?) (pair? idle))
        (match-define (cons wrkr idle-rest) idle)
        (define-values (job cmd-list) (queue/get jobqueue (wrkr/id wrkr)))
        (wrkr/send wrkr cmd-list)
        (loop idle-rest (cons (list job wrkr) inflight) count)]

      [else
        (define (gen-node-handler node-worker)
          (match-define (list node wrkr) node-worker)
          (handle-evt (wrkr/out wrkr) (λ (msg)
            (if (queue/work-done jobqueue node wrkr msg)
                (loop (cons wrkr idle) (remove node-worker inflight) (add1 count))
                (loop idle inflight count)))))
      
        (apply sync (map gen-node-handler inflight))]))

  (for ([p workers]) (wrkr/send p (list 'DIE)))
  (for ([p ps]) (place-wait p)))
