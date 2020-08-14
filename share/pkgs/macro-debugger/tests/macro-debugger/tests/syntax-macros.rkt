#lang racket/base
(require "../gentest-framework.rkt")
(provide proto:macros)

(define-tests proto:macros "Macros"
  [#:suite 
   "Macros"
   (test "id"
         (id 'a)
         [#:steps (macro 'a)]
         #:no-hidden-steps)
   (test "Tid"
         (Tid 'a)
         [#:steps (macro 'a)]
         #:same-hidden-steps)
   (test "pre-id"
         (pre-id 'a)
         [#:steps (macro (id 'a))
                  (macro 'a)]
         #:no-hidden-steps)
   (test "myor (base)"
         (myor 'a)
         [#:steps (macro 'a)]
         #:no-hidden-steps)
   (test "myor (recursive 1)" 
         (myor 'a 'b)
         [#:steps (macro (let ((t 'a)) (if t t (myor 'b))))
                  (macro (let-values (((t) 'a)) (if t t (myor 'b))))
                  (rename-letX (let-values (((t) 'a)) (if t t (myor 'b))))
                  (macro (let-values (((t) 'a)) (if t t 'b)))]
         #:no-hidden-steps)

   (test "leid with id"
         (leid (id 'a))
         [#:steps (macro 'a (leid (id 'a)))
                  (macro (#%expression 'a))]
         #:no-hidden-steps)
   (test "leid with Tid"
         (leid (Tid 'a))
         [#:steps (macro 'a (leid (Tid 'a)))
                  (macro (#%expression 'a))]
         [#:hidden-steps (macro (leid 'a))])]

  (test "lift"
        (lift 'a)
        [#:steps (remark local-lift 'a (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression #rx"^lifted")))]
        #:no-hidden-steps)
  (test "lift with id"
        (lift (id 'a))
        [#:steps (remark local-lift (id 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))]
        #:no-hidden-steps)

  (test "lift with Tid"
        (lift (Tid 'a))
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))]
        ;; FIXME:
        ;;  maybe don't show lifts, but do find (Tid 'a), show in orig ctx
        ;;  but maybe not a good idea
        #| 
        [#:hidden-steps (macro (lift 'a))]
        |#)

  (test "Tlift"
        (Tlift 'a)
        [#:steps (remark local-lift 'a (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression #rx"^lifted")))]
        [#:hidden-steps (remark local-lift 'a (#rx"^lifted"))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with id"
        (Tlift (id 'a))
        [#:steps (remark local-lift (id 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))]
        [#:hidden-steps (remark local-lift (id 'a) (#rx"^lifted"))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with Tid"
        (Tlift (Tid 'a))
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))]
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))])

  (test "pid0 with Tid"
        (pid0 (Tid 'a))
        [#:steps
         (macro (Tid 'a))
         (macro 'a)]
        [#:hidden-steps
         (macro (pid0 'a))])

  (test "pid1 with Tid"
        (pid1 (Tid 'a))
        [#:steps
         (macro (#%plain-app values (Tid 'a)))
         (macro (#%plain-app values 'a))]
        [#:hidden-steps
         (macro (pid1 'a))])

  (test "pid2 with Tid"
        (pid2 (Tid 'a))
        [#:steps
         (macro (let-values () (Tid 'a)))
         (rename-letX _)
         (macro (let-values () 'a))]
        [#:hidden-steps
         (macro (pid2 'a))])

  (test "pid3 with Tid"
        (pid3 (Tid 'a))
        [#:steps
         (macro (let ([x (Tid 'a)]) x))
         (macro (let-values ([(x) (Tid 'a)]) x))
         (rename-letX _)
         (macro (let-values ([(x) 'a]) x))]
        [#:hidden-steps
         (macro (pid3 'a))])

  (test "pid4 with Tid"
        (pid4 (Tid 'a))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (pid4 'a))])

  (test "pid5 with Tid"
        (pid5 (Tid 'a))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (pid5 'a))])

  (test "pidn with Tid"
        (pidn (Tid 'a))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (pidn 'a))])

  (test "pidn with Tid x2"
        (pidn (Tid (pidn (Tid 'a))))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (pidn (pidn (Tid 'a))))
         (macro (pidn (pidn 'a)))])

  (test "sclist2 with Tid x2"
        (sclist2 (Tid 'a) (Tid 'b))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (sclist2 'a (Tid 'b)))
         (macro (sclist2 'a 'b))])

  (test "scplist2 with Tid x2"
        (scplist2 (Tid 'a) (Tid 'b))
        #;[#:steps ...] ;; complicated, skip it
        [#:hidden-steps
         (macro (scplist2 'a (Tid 'b)))
         (macro (scplist2 'a 'b))])

  (test "protected scplist2 from pid0"
        (pid0 (scplist2 (Tid 'a) (Tid 'b)))
        [#:hidden-steps
         (macro (pid0 (scplist2 'a (Tid 'b))))
         (macro (pid0 (scplist2 'a 'b)))])

  (test "protected scplist2 from pid1"
        (pid1 (scplist2 (Tid 'a) (Tid 'b)))
        [#:hidden-steps
         (macro (pid1 (scplist2 'a (Tid 'b))))
         (macro (pid1 (scplist2 'a 'b)))])

  (test "protected scplist2e from pid0 in let-values"
        (let-values () (pid0 (scplist2e (Tid 'a) (Tid 'b))))
        [#:hidden-steps
         (rename-letX _)
         (macro (let-values () (pid0 (scplist2e 'a (Tid 'b)))))
         (macro (let-values () (pid0 (scplist2e 'a 'b))))])

  (test "protected scplist2 from pid0 in let-values"
        (let-values () (pid0 (scplist2 (Tid 'a) (Tid 'b))))
        [#:hidden-steps
         (rename-letX _)
         (macro (let-values () (pid0 (scplist2 'a (Tid 'b)))))
         (macro (let-values () (pid0 (scplist2 'a 'b))))])

  (test "protected scplist2 from pid1 in let-values"
        (let-values () (pid1 (scplist2 (Tid 'a) (Tid 'b))))
        [#:hidden-steps
         (rename-letX _)
         (macro (let-values () (pid1 (scplist2 'a (Tid 'b)))))
         (macro (let-values () (pid1 (scplist2 'a 'b))))])

  [#:suite "set! macros"
           (test "set! (macro)"
                 (set! the-current-output-port 'a)
                 [#:steps
                  (macro (#%plain-app current-output-port 'a))]
                 #:no-hidden-steps)]
  
  )
