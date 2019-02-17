#lang racket/base

(require racket/list racket/match racket/format racket/set racket/promise
         racket/contract/combinator
         profile/sampler profile/utils profile/analyzer
         "utils.rkt"
         "boundary-view.rkt" "module-graph-view.rkt"
         (for-syntax racket/base syntax/parse))

(define limit-dots " ... ")

;; using dynamic-require, to also work on versions that don't have it
(define space-efficient-key
  (dynamic-require '(lib "racket/contract/combinator")
                   'space-efficient-contract-continuation-mark-key
                   (lambda _ #f)))

;; (listof (U (vector (U blame? (cons blame? blame-party)) boolean?) #f))
;; profile-samples
;;   -> contract-profile?
;; boolean denotes whether the sampled contract was space-efficient
(define (correlate-contract-samples contract-samples time+samples)
  ;; car of time+samples is total time, car of each sample is thread id
  ;; for now, we just assume a single thread. fix this eventually.
  (define total-time (car time+samples))
  ;; reverse is there to sort samples in forward time, which get-times
  ;; needs.
  (define samples
    (for/list ([s (in-list (get-times (map cdr (reverse (cdr time+samples)))))])
      ;; don't want fractions for printing
      (cons (real->double-flonum (car s)) (cdr s))))
  ;; combine blame info and stack trace info. samples should line up
  (define live-contract-samples
    ;; If the sampler was stopped after recording a contract sample, but
    ;; before recording the corresponding time sample, the two lists may
    ;; be of different lengths. That's ok, just drop the extra sample.
    (for/list ([c-s (in-list contract-samples)]
               [s   (in-list samples)]
               #:when c-s)
      (match-define `#(,-blame ,space-efficient?) c-s)
      ;; In some cases, blame information is missing a party, in which.
      ;; case the contract system provides a pair of the incomplete blame
      ;; and the missing party. We combine the two here.
      (define blame
        (if (pair? -blame)
            (blame-add-missing-party (car -blame) (cdr -blame))
            -blame))
      (contract-sample blame space-efficient? s)))
  (define all-blames
    (set->list (for/set ([c-s (in-list live-contract-samples)])
                 (define b (contract-sample-blame c-s))
                 ;; all blames must be complete, otherwise we get bogus profiles
                 (when (blame-missing-party? b)
                   (error (string-append "contract-profile: incomplete blame:\n"
                                         (format-blame b))))
                 ;; An original blamed and its swapped version are the same
                 ;; for our purposes.
                 (if (blame-swapped? b)
                     (blame-swap b) ; swap back
                     b))))
  (define regular-profile (delay (analyze-samples time+samples)))
  (contract-profile
   total-time live-contract-samples all-blames regular-profile))


(define (analyze-contract-samples
         contract-samples
         samples
         #:report-space-efficient? [report-space-efficient? #f]
         #:module-graph-view-file  [module-graph-view-file  #f]
         #:boundary-view-file      [boundary-view-file      #f]
         #:boundary-view-key-file  [boundary-view-key-file  #f])
  (define correlated (correlate-contract-samples contract-samples samples))
  (print-breakdown correlated #:report-space-efficient? report-space-efficient?)
  (when module-graph-view-file
    (module-graph-view correlated module-graph-view-file))
  (when boundary-view-file
    (boundary-view correlated boundary-view-file boundary-view-key-file)))


;;---------------------------------------------------------------------------
;; Break down contract checking time by contract, then by callee and by chain
;; of callers.

(define (/. num den) (/ num (max den 1) 1.0))

(define (print-breakdown correlated
                         [show-by-caller? #f]
                         #:report-space-efficient? [report-space-efficient? #f])
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
    correlated)

  (define total-contract-time (samples-time live-contract-samples))
  (define contract-ratio (/. total-contract-time total-time))
  (printf "Running time is ~a% contracts\n"
          (~r (* 100 contract-ratio) #:precision 2))
  (printf "~a/~a ms\n\n"
          (~r total-contract-time #:precision 0)
          total-time)
  (define (only-space-efficient samples)
    (filter contract-sample-space-efficient? samples))
  (define space-efficient-samples (only-space-efficient live-contract-samples))
  (when report-space-efficient?
    (define total-space-efficient-time (samples-time space-efficient-samples))
    (define space-efficient-ratio
      (/. total-space-efficient-time total-contract-time))
    (printf "(of those, ~a% (~a/~a ms) are space-efficient)\n\n"
            (~r (* 100 space-efficient-ratio) #:precision 2)
            (~r total-space-efficient-time #:precision 0)
            (~r total-contract-time #:precision 0)))

  (define shorten-source
    (make-srcloc-shortener all-blames blame-source))
  (define (format-contract/loc c s)
    (string-append
     (~s (blame-contract c) #:limit-marker limit-dots #:width location-width)
     (~a (format-samples-time s) "\n")
     (~a (srcloc->string (shorten-source c))
         #:limit-marker limit-dots
         #:limit-prefix? #t
         #:width (- location-width 1))))
  (define (format-samples-time s)
    (define total-time (samples-time s))
    (format "~a ms~a"
            (~r total-time #:precision 2)
            (if report-space-efficient?
                (format " (~a% space-efficient)"
                        (~r (* 100 (/. (samples-time (only-space-efficient s))
                                       total-time))
                            #:precision 2))
                "")))

  (define samples-by-contract
    (sort (group-by (lambda (x) (blame-contract (contract-sample-blame x)))
                    live-contract-samples)
          > #:key length #:cache-keys? #t))

  (define location-width 65)
  (for ([g (in-list samples-by-contract)])
    (define representative (contract-sample-blame (car g)))
    (displayln (format-contract/loc representative g))
    (for ([x (sort
              (group-by (lambda (x)
                          ;; callee source, maybe
                          (blame-value (contract-sample-blame x)))
                        g)
              > #:key length)])
      (define indent "    ")
      (display (string-append
                indent
                (~s (blame-value (contract-sample-blame (car x)))
                    #:limit-marker limit-dots
                    #:width (- location-width (string-length indent)))))
      (displayln (format-samples-time x)))
    (newline))

  (when show-by-caller?
    (define samples-by-contract-by-caller
      (for/list ([g (in-list samples-by-contract)])
        (sort (group-by (lambda (x)
                          ;; pruned stack trace
                          (cdr (contract-sample-profile-sample)))
                        (map sample-prune-stack-trace g))
              > #:key length)))
    (displayln "\nBY CALLER\n")
    (for* ([g samples-by-contract-by-caller]
           [c g])
      (define representative (car c))
      (displayln (format-contract/loc (contract-sample-blame representative) c))
      (for ([frame (in-list
                    (cdr (contract-sample-profile-sample representative)))])
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
         (~optional (~seq #:module-graph-view-file module-graph-view-file:expr)
                    #:defaults ([module-graph-view-file #'#f]))
         (~optional (~seq #:boundary-view-file boundary-view-file:expr)
                    #:defaults ([boundary-view-file #'#f]))
         (~optional (~seq #:boundary-view-key-file boundary-view-key-file:expr)
                    #:defaults ([boundary-view-key-file #'#f]))
         (~optional (~seq #:report-space-efficient? report-space-efficient?:expr)
                    #:defaults ([report-space-efficient? #'#f])))
        ...
        body:expr ...)
     #`(let ([sampler (create-sampler
                       (current-thread) 0.005 (current-custodian)
                       (list contract-continuation-mark-key
                             (or space-efficient-key
                                 ;; won't be found, so we'll just get `#f`s
                                 (gensym))))])
         (begin0 (begin body ...)
           (let ()
             (sampler 'stop)
             (define samples (sampler 'get-snapshots))
             (define contract-samples
               (for/list ([s (in-list (sampler 'get-custom-snapshots))])
                 ;; TODO taking the car loses information. even without
                 ;;   re-entrant contracts, I've seen samples that have a
                 ;;   2-element list, e.g., in:
                 ;; (require contract-profile)
                 ;; (define v (contract (vectorof integer?)
                 ;;                     (contract (vectorof integer?) (vector 1 2)
                 ;;                               'pos 'neg)
                 ;;                     'pos 'neg))
                 ;; (define w (contract (vectorof integer?) (vector 1 2)
                 ;;                     'pos 'neg))
                 ;; (contract-profile (for/fold ([acc 0])
                 ;;                             ([i (in-range 10000000)])
                 ;;                     (+ acc (vector-ref v 0) (vector-ref w 0))))
                 (and (not (empty? s)) (car s))))
             (analyze-contract-samples
              contract-samples
              samples
              #:module-graph-view-file module-graph-view-file
              #:boundary-view-file boundary-view-file
              #:boundary-view-key-file boundary-view-key-file
              #:report-space-efficient? report-space-efficient?))))]))

(define (contract-profile-thunk f
                                #:module-graph-view-file [module-graph-view-file #f]
                                #:boundary-view-file [boundary-view-file #f]
                                #:boundary-view-key-file [boundary-view-key-file #f]
                                #:report-space-efficient? [report-space-efficient? #f])
  (contract-profile/user
    #:module-graph-view-file module-graph-view-file
    #:boundary-view-file boundary-view-file
    #:boundary-view-key-file boundary-view-key-file
    #:report-space-efficient? report-space-efficient?
    (f)))
