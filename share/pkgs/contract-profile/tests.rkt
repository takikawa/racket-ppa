#lang racket/base

(require racket/port
         contract-profile
         (only-in contract-profile/utils make-shortener))

(module+ test
  (require rackunit)

  ;; reported by Greg Hendershott
  (define res
    (with-output-to-string
      (lambda ()
        (check-true (contract-profile #:module-graph-file #f
                                      #:boundary-view-file #f
                                      #:boundary-view-key-file #f
                                      #t)))))
  (check-regexp-match #rx"^Running time is 0% contracts" res)

  ;; test options for `contract-profile-thunk`
  (let ([res
         (with-output-to-string
           (lambda ()
             (check-false
               (contract-profile-thunk
                 #:module-graph-file #f
                 #:boundary-view-file #f
                 #:boundary-view-key-file #f
                 (lambda () (string? 4))))))])
    (check-regexp-match #rx"0% contracts" res))

  (require math)
  (let ()
    (define dim 200)
    (define big1 (build-matrix dim dim (lambda (i j) (random))))
    (define big2 (build-matrix dim dim (lambda (i j) (random))))
    (define (main) (matrix* big1 big2))
    (check-true (parameterize ([current-output-port (open-output-nowhere)])
                  (matrix? (contract-profile (main))))))

  ;; test path shortening
  (define paths '("a/b/c.rkt" "a/b/d.rkt" ("a/b/e.rkt" f) (something else)))
  (define shortener (make-shortener paths))
  (check-equal? (map shortener paths)
                (list (build-path "c.rkt")
                      (build-path "d.rkt")
                      (list (build-path "e.rkt") 'f)
                      '(something else)))

  ;; test that instrumentation for TR contract combinators works
  ;; (tests for instrumentation of other contracts is in the contract tests)
  (let ([res
         (with-output-to-string
           (lambda ()
             (parameterize ([current-namespace (make-base-namespace)])
               (eval '(module server1 typed/racket
                        (provide v)
                        (: v Any)
                        (define v (vector 0))))
               (eval '(require 'server1))
               (eval '(require contract-profile))
               (eval '(contract-profile
                       (for ([i (in-range 10000000)])
                         (vector-ref v 0))))
               )))])
    (check-regexp-match #rx"Any" res))

  ;; Note: The next two tests originally featured single-argument methods.
  ;; However, TR's contract generation improved (main by using simple-result->
  ;; more often) which made the costs of the contracts not be directly
  ;; observable anymore. Because of this, these tests now use methods that take
  ;; more arguments.
  ;; Note to the note: That's not to say that TR's optimization eliminated the
  ;; cost of contracts altogether. The direct costs seem to be basically gone,
  ;; but most of the opportunity costs seem to remain.
  (let ([res
         (with-output-to-string
           (lambda ()
             (parameterize ([current-namespace (make-base-namespace)])
               (eval '(module u racket
                        (define (mixin cls)
                          (class cls
                            (super-new)
                            (define/public (n x a b c d) (add1 x))))
                        (provide mixin)))
               (eval '(module t typed/racket
                        ;; expects a mixin that adds n
                        (require/typed
                         'u
                         [mixin
                             (All (r #:row)
                                  (-> (Class #:row-var r)
                                      (Class #:row-var r
                                             [n (-> Integer Integer Integer Integer Integer Integer)])))])
                        (define c%
                          (mixin (class object%
                                   (super-new)
                                   (define/public (m x) x))))
                        (require/typed
                         contract-profile
                         [contract-profile-thunk ((-> Any) -> Any)])
                        (define x (new c%))
                        (contract-profile-thunk
                         (lambda ()
                           (for ([i (in-range 1000000)]) (send x n 1 2 3 4 5))))))
               (eval '(require 't))
               )))])
    (check-regexp-match #rx"mixin" res))

  (let ([res
         (with-output-to-string
           (lambda ()
             (parameterize ([current-namespace (make-base-namespace)])
               (eval '(module a racket
                        (provide c%)
                        (define c%
                          (class object%
                            (super-new)
                            (define/public (m x a b c d) (void))))))
               (eval '(module b typed/racket
                        (require/typed 'a
                                       [c% (Class [m (-> Integer Integer Integer Integer Integer Void)])])
                        (provide o)
                        (: o (Object))
                        (define o (new (class c%
                                         (super-new)
                                         (define/public (n) (void)))))))
               (eval '(require 'b contract-profile racket/class))
               (eval '(contract-profile
                       (for ([i (in-range 3000000)])
                         (send o m 1 2 3 4 5))))
               )))])
    (check-regexp-match #rx"c%" res))

  )
