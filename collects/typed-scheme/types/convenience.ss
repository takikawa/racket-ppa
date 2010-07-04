#lang scheme/base  
(require "../utils/utils.ss"
         (rep type-rep filter-rep object-rep)
         (utils tc-utils)
         "abbrev.ss"
	 (types comparison printer union subtype utils)
         scheme/list scheme/match scheme/promise
         (for-syntax syntax/parse scheme/base)
         (for-template scheme/base))

(provide (all-defined-out)
         (all-from-out "abbrev.ss")
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function
         (rep-out filter-rep object-rep))

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (Un/eff . args)
  (apply Un (map tc-result-t args)))


;; if t is of the form (Pair t* (Pair t* ... (Listof t*)))
;; return t*
;; otherwise, return t
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
        [(Mu: var (Union: (list (Value: '()) (Pair: _ (F: var))))) t*]
        [(Pair: t1 (Value: '())) (-lst t1)]
        [(Pair: t1 t2)
         (let ([t-new (loop t2)])
           (if (type-equal? (-lst t1) t-new)
               t-new
               (exit t)))]
        [_ (exit t)]))))


;; DO NOT USE if t contains #f
(define (-opt t) (Un (-val #f) t))

(define In-Syntax
  (-mu e
       (*Un -Number -Boolean -Symbol -String -Keyword -Char
            (make-Vector (-Syntax e))
            (make-Box (-Syntax e))
            (-mu list
                 (*Un (-val '())
                      (-pair (-Syntax e)
                             (*Un (-Syntax e) list)))))))

(define Any-Syntax (-Syntax In-Syntax))

(define (-Sexpof t)
  (-mu sexp
       (Un -Number -Boolean -Symbol -String -Keyword -Char
           (-val '())
           (-pair sexp sexp)
           (make-Vector sexp)
           (make-Box sexp)
           t)))

(define -Sexp (-Sexpof (Un)))

(define Syntax-Sexp (-Sexpof Any-Syntax))

(define Ident (-Syntax -Symbol))
