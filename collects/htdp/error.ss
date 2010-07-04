#lang mzscheme

(require mzlib/etc 
         mzlib/list)

;; --------------------------------------------------------------------------
(provide check-arg check-arity check-proc check-result check-list-list
         check-fun-res
         natural?
         find-non tp-exn? number->ord)

(define (natural? w)
  (and (number? w) (integer? w) (>= w 0)))

;; (_ -> Boolean) (listof X) -> (union X false)
(define (find-non pred? l)
  (let ([r (filter (compose not pred?) l)])
    (if (null? r) #f (car r))))


;(: check-fun-res (∀ (γ) (∀ (β α ...) (α ...α -> β)) (_ -γ-> boolean) _ -> γ))
(define (check-fun-res f pred? type)
  (lambda x 
    (define r (apply f x))
    (check-result (object-name f) pred? type r)
    r))

#| Tests ------------------------------------------------------------------
  (not (find-non list? '((1 2 3) (a b c))))
  (symbol? (find-non number? '(1 2 3 a)))
  (symbol? (find-non list? '((1 2 3) a (b c))))
  |#

(define-struct (tp-exn exn) ())

(define (tp-error name fmt . args)
  (raise 
   (make-exn:fail:contract #; make-tp-exn 
    (string-append (format "~a: " name) (apply format fmt args))
    (current-continuation-marks))))

(define (number->ord i)
  (if (= i 0)
      "zeroth"
      (case (modulo i 10)
        [(0 4 5 6 7 8 9) (format "~ath" i)]
        [(1) (format "~ast" i)]
        [(2) (format "~and" i)]
        [(3) (format "~ard" i)])))

;; Symbol (union true String) String X -> void
(define (check-list-list pname condition pred given)
  (when (string? condition)
    (tp-error pname (string-append condition (format "~nin ~e" given)))))

;; Symbol (_ -> Boolean) String X  X *-> X 
(define (check-result pname pred? expected given . other-given)
  (if (pred? given)
      given
      (tp-error pname "result of type <~a> expected, your function produced ~a" expected 
                (if (pair? other-given)
                    (car other-given)
                    given))))

;; check-arg : sym bool str str TST -> void
(define (check-arg pname condition expected arg-posn given)
  (unless condition
    (tp-error pname "expected <~a> as ~a argument, given: ~e"
              expected arg-posn given)))

;; check-arity : sym num (list-of TST) -> void
(define (check-arity name arg# args)
  (if (= (length args) arg#)
      (void)
      (tp-error name "expects ~a arguments, given ~e" arg# (length args))))

;; check-proc :
;;   sym (... *->* ...) num (union sym str) (union sym str) -> void
(define (check-proc proc f exp-arity arg# arg-err)
  (unless (procedure? f)
    (tp-error proc "procedure expected as ~a argument; given ~e" arg# f))
  (let ([arity-of-f (procedure-arity f)])
    (unless  (and (number? arity-of-f) (>= arity-of-f exp-arity))
      (tp-error proc "procedure of ~a expected as ~a argument; given procedure of ~a "
                arg-err arg# 
                (cond
                  [(number? arity-of-f)
                   (if (= arity-of-f 1)
                       (format "1 argument")
                       (format "~s arguments" arity-of-f))]
                  [(arity-at-least? arity-of-f) "variable number of arguments"]
                  [else (format "multiple arities (~s)" arity-of-f)])))))
