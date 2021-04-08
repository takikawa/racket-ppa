#lang racket/base

(provide profile->json json->profile)

(require "structs.rkt" racket/dict racket/format racket/list racket/string)

(define (node-loc node)
  (cons (node-id node) (node-src node)))

(define (profile->json p)
  (define nodes (cons (profile-*-node p) (profile-nodes p)))
  (define loc-hash
    (for/hash ([node (in-list nodes)] [n (in-naturals)])
      (values (node-loc node) n)))
  (define (loc-edge->json edge) (edge->json loc-hash edge))
  (define node-hash
    (for/hash ([node (in-list nodes)])
      (values (node-loc node) node)))

  (hash 'total_time (exact->inexact (profile-total-time p))
        'cpu_time (exact->inexact (profile-cpu-time p))
        'sample_number (profile-sample-number p)
        'thread_times
        (for/list ([(id time) (in-dict (profile-thread-times p))])
          (hash 'id id 'time (exact->inexact time)))
        'nodes
        (for/list ([node nodes])
          (hash 'id (and (node-id node) (~a (node-id node)))
                'src (and (node-src node) (srcloc->string (node-src node)))
                'thread_ids (node-thread-ids node)
                'total (exact->inexact (node-total node))
                'self (exact->inexact (node-self node))
                'callers (map loc-edge->json (node-callers node))
                'callees (map loc-edge->json (node-callees node))))))

(define (edge->json loc-hash edge)
  (hash 'total (exact->inexact (edge-total edge))
        'caller (hash-ref loc-hash (node-loc (edge-caller edge)))
        'caller_time (exact->inexact (edge-caller-time edge))
        'callee (hash-ref loc-hash (node-loc (edge-callee edge)))
        'callee_time (exact->inexact (edge-callee-time edge))))

(define (string->srcloc s)
  (define-values (path-parts lc) (split-at-right (string-split s ":" #:trim? #f) 2))
  (define line (string->number (first lc)))
  (define col (string->number (second lc)))
  (define path (string->path (string-join path-parts ":")))
  (srcloc path line (and line col) #f #f))

(define (json->profile j)
  (define nodes
    (for/vector ([n (hash-ref j 'nodes)])
      (node (hash-ref n 'id)
            (and (hash-ref n 'src) (string->srcloc (hash-ref n 'src)))
            (hash-ref n 'thread_ids)
            (hash-ref n 'total)
            (hash-ref n 'self)
            '() '())))
  (for ([n (in-list (hash-ref j 'nodes))] [n* (in-vector nodes)])
    (set-node-callees! n*
     (for/list ([e (hash-ref n 'callees)])
       (edge (hash-ref e 'total)
             (vector-ref nodes (hash-ref e 'caller))
             (hash-ref e 'caller_time)
             (vector-ref nodes (hash-ref e 'callee))
             (hash-ref e 'callee_time))))
    (set-node-callers! n*
     (for/list ([e (hash-ref n 'callers)])
       (edge (hash-ref e 'total)
             (vector-ref nodes (hash-ref e 'caller))
             (hash-ref e 'caller_time)
             (vector-ref nodes (hash-ref e 'callee))
             (hash-ref e 'callee_time)))))
  (profile (hash-ref j 'total_time)
           (hash-ref j 'cpu_time)
           (hash-ref j 'sample_number)
           (for/list ([t (hash-ref j 'thread_times)])
             (cons (hash-ref t 'id) (hash-ref t 'time)))
           (rest (vector->list nodes))
           (vector-ref nodes 0)))

