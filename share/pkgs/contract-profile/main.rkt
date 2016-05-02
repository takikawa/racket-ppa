#lang racket/base

(require racket/list racket/match racket/format racket/set
         racket/contract/combinator
         profile/sampler profile/utils profile/analyzer
         "utils.rkt"
         "boundary-view.rkt" "module-graph-view.rkt"
         (for-syntax racket/base syntax/parse))

(define limit-dots " ... ")

;; (listof (U blame? #f)) profile-samples -> contract-profile struct
(define (correlate-contract-samples contract-samples* samples*)
  ;; car of samples* is total time, car of each sample is thread id
  ;; for now, we just assume a single thread. fix this eventually.
  (define total-time (car samples*))
  ;; reverse is there to sort samples in forward time, which get-times
  ;; needs.
  (define samples    (get-times (map cdr (reverse (cdr samples*)))))
  (define contract-samples
    (for/list ([c-s (in-list contract-samples*)])
      ;; In some cases, blame information is missing a party, in which.
      ;; case the contract system provides a pair of the incomplete blame
      ;; and the missing party. We combine the two here.
      (if (and (pair? c-s))
          (if (blame-missing-party? (car c-s))
              (blame-add-missing-party (car c-s) (cdr c-s))
              (car c-s))
          c-s)))
  ;; combine blame info and stack trace info. samples should line up
  (define aug-contract-samples
    ;; If the sampler was stopped after recording a contract sample, but
    ;; before recording the corresponding time sample, the two lists may
    ;; be of different lengths. That's ok, just drop the extra sample.
    (for/list ([c-s (in-list contract-samples)]
               [s   (in-list samples)])
      (cons c-s s)))
  (define live-contract-samples (filter car aug-contract-samples))
  (define all-blames
    (set->list (for/set ([b (in-list contract-samples)]
                         #:when b)
                 ;; An original blamed and its swapped version are the same
                 ;; for our purposes.
                 (if (blame-swapped? b)
                     (blame-swap b) ; swap back
                     b))))
  (define regular-profile (analyze-samples samples*))
  ;; all blames must be complete, otherwise we get bogus profiles
  (for ([b (in-list all-blames)])
    (unless (not (blame-missing-party? b))
      (error (string-append "contract-profile: incomplete blame:\n"
                            (format-blame b)))))
  (contract-profile
   total-time live-contract-samples all-blames regular-profile))


(define (analyze-contract-samples
         contract-samples
         samples*
         #:module-graph-file [module-graph-file #f]
         #:boundary-view-file [boundary-view-file #f]
         #:boundary-view-key-file [boundary-view-key-file #f])
  (define correlated (correlate-contract-samples contract-samples samples*))
  (print-breakdown correlated)
  (when module-graph-file
    (module-graph-view correlated module-graph-file))
  (when boundary-view-file
    (boundary-view correlated boundary-view-file boundary-view-key-file)))


;;---------------------------------------------------------------------------
;; Break down contract checking time by contract, then by callee and by chain
;; of callers.

(define (print-breakdown correlated [show-by-caller? #f])
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
    correlated)

  (define total-contract-time (samples-time live-contract-samples))
  (define contract-ratio (/ total-contract-time (max total-time 1) 1.0))
  (printf "Running time is ~a% contracts\n"
          (~r (* 100 contract-ratio) #:precision 2))
  (printf "~a/~a ms\n\n"
          (~r total-contract-time #:precision 0)
          total-time)

  (define shorten-source
    (make-srcloc-shortener all-blames blame-source))
  (define (format-contract/loc c s)
    (string-append
     (~a (blame-contract c) #:limit-marker limit-dots #:width location-width)
     (~a (format-samples-time s) "\n")
     (~a (srcloc->string (shorten-source c))
         #:limit-marker limit-dots
         #:limit-prefix? #t
         #:width (- location-width 1))))
  (define (format-samples-time s)
    (format "~a ms" (~r (samples-time s) #:precision 2)))

  (define samples-by-contract
    (sort (group-by (lambda (x) (blame-contract (car x)))
                    live-contract-samples)
          > #:key length #:cache-keys? #t))

  (define location-width 65)
  (for ([g (in-list samples-by-contract)])
    (define representative (caar g))
    (displayln (format-contract/loc representative g))
    (for ([x (sort
              (group-by (lambda (x)
                          (blame-value (car x))) ; callee source, maybe
                        g)
              > #:key length)])
      (display (~a "    " (blame-value (caar x)) #:limit-marker limit-dots #:width location-width))
      (displayln (format-samples-time x)))
    (newline))

  (when show-by-caller?
    (define samples-by-contract-by-caller
      (for/list ([g (in-list samples-by-contract)])
        (sort (group-by cddr ; pruned stack trace
                        (map sample-prune-stack-trace g))
              > #:key length)))
    (displayln "\nBY CALLER\n")
    (for* ([g samples-by-contract-by-caller]
           [c g])
      (define representative (car c))
      (displayln (format-contract/loc (car representative) c))
      (for ([frame (in-list (cddr representative))])
        (printf "  ~a @ ~a\n" (car frame) (srcloc->string (cdr frame))))
      (newline))))

;; Unrolls the stack until it hits a function on the negative side of the
;; contract boundary (based on module location info).
;; Will give bogus results if source location info is incomplete.
(define (sample-prune-stack-trace sample)
  (match-define (list blame timestamp stack-trace ...) sample)
  (define caller-module (blame-negative blame))
  (define new-stack-trace
    (dropf stack-trace
           (match-lambda
            [(cons name loc)
             (or (not loc)
                 (not (equal? (srcloc-source loc) caller-module)))])))
  (list* blame timestamp new-stack-trace))


;;---------------------------------------------------------------------------
;; Entry point

(provide (rename-out [contract-profile/user contract-profile])
         contract-profile-thunk
         analyze-contract-samples) ; for feature-specific profiler

;; TODO have kw args for sampler, etc.
(define-syntax (contract-profile/user stx)
  (syntax-parse stx
    [(_ (~or
         ;; these arguments are: (or/c filename 'stdout #f) ; #f = disabled
         ;; absent means default filename
         (~optional (~seq #:module-graph-file module-graph-file:expr)
                    #:defaults ([module-graph-file #'#f]))
         (~optional (~seq #:boundary-view-file boundary-view-file:expr)
                    #:defaults ([boundary-view-file #'#f]))
         (~optional (~seq #:boundary-view-key-file boundary-view-key-file:expr)
                    #:defaults ([boundary-view-key-file #'#f])))
        ...
        body:expr ...)
     #`(let ([sampler (create-sampler (current-thread) 0.005 (current-custodian)
                                      (list contract-continuation-mark-key))])
         (begin0 (begin body ...)
           (let ()
             (sampler 'stop)
             (define samples (sampler 'get-snapshots))
             (define contract-samples
               (for/list ([s (in-list (sampler 'get-custom-snapshots))])
                 (and (not (empty? s)) (vector-ref (car s) 0))))
             (analyze-contract-samples
              contract-samples
              samples
              #:module-graph-file module-graph-file
              #:boundary-view-file boundary-view-file
              #:boundary-view-key-file boundary-view-key-file))))]))

(define (contract-profile-thunk f
                                #:module-graph-file [module-graph-file #f]
                                #:boundary-view-file [boundary-view-file #f]
                                #:boundary-view-key-file [boundary-view-key-file #f])
  (contract-profile/user
    #:module-graph-file module-graph-file
    #:boundary-view-file boundary-view-file
    #:boundary-view-key-file boundary-view-key-file
    (f)))
