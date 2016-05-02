#lang racket/base

(require racket/contract/combinator
         racket/format racket/list racket/match racket/set
         "utils.rkt" "dot.rkt")

(provide module-graph-view)

;; Show graph of modules, with contract boundaries and contract costs for each
;; boundary.
;; Typed modules are in green, untyped modules are in red.

(define (module-graph-view correlated module-graph-file)
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
    correlated)

  ;; first, enumerate all the relevant modules
  (define-values (nodes edge-samples)
    (for/fold ([nodes (set)] ; set of modules
               ;; maps pos-neg edges (pairs) to lists of samples
               [edge-samples (hash)])
        ([s (in-list live-contract-samples)])
      (match-define (list blame sample-time stack-trace ...) s)
      (when (empty? stack-trace)
        (log-warning "contract profiler: sample had empty stack trace"))
      (define pos (blame-positive blame))
      (define neg (blame-negative blame))
      ;; We consider original blames and their swapped versions to be the same.
      (define edge-key (if (blame-swapped? blame)
                           (cons neg pos)
                           (cons pos neg)))
      (values (set-add (set-add nodes pos) neg) ; add all new modules
              (hash-update edge-samples edge-key
                           (lambda (ss) (cons s ss))
                           '()))))

  (define nodes->typed?
    (for/hash ([n nodes]
               ;; Needs to be either a file or a submodule.
               ;; I've seen 'unit and 'not-enough-info-for-blame go by here,
               ;; and we can't do anything with either.
               #:when (or (path? n) (pair? n)))
      ;; typed modules have a #%type-decl submodule
      (define submodule? (not (path? n)))
      (define filename (if submodule? (car n) n))
      (define typed?
        (with-handlers
            ([(lambda (e)
                (and (exn:fail:contract? e)
                     (or (regexp-match "^dynamic-require: unknown module"
                                       (exn-message e))
                         (regexp-match "^path->string"
                                       (exn-message e)))))
              (lambda _ #f)])
          (dynamic-require
           (append (list 'submod (list 'file (path->string filename)))
                   (if submodule? (cdr n) '())
                   '(#%type-decl))
           #f)
          #t))
      (values n typed?)))

  ;; graphviz output
  (with-output-to-dot
   module-graph-file
   (printf "digraph {\n")
   (printf "rankdir=LR\n")
   (define nodes->names (for/hash ([n nodes]) (values n (gensym))))
   (define node->labels (make-shortener nodes))
   (for ([n nodes])
     (printf "~a[label=\"~a\"][color=\"~a\"]\n"
             (hash-ref nodes->names n)
             (node->labels n)
             (if (hash-ref nodes->typed? n #f) "green" "red")))
   (for ([(k v) (in-hash edge-samples)])
     (match-define (cons pos neg) k)
     (printf "~a -> ~a[label=\"~ams\"]\n"
             (hash-ref nodes->names neg)
             (hash-ref nodes->names pos)
             (~r (samples-time v) #:precision 2)))
   (printf "}\n")))
