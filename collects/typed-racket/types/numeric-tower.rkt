#lang racket/base

(require "../utils/utils.rkt"
         (rename-in (types numeric-predicates base-abbrev)
                    [simple-Un *Un])
         (rename-in (rep type-rep) [make-Base make-Base*])
         racket/match
         racket/function
         unstable/function
         (for-template racket/base racket/contract/base racket/flonum (types numeric-predicates)))

(provide portable-fixnum? portable-index?
         -Zero -One -PosByte -Byte -PosIndex -Index
         -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
         -PosInt -Nat -NegInt -NonPosInt -Int
         -PosRat -NonNegRat -NegRat -NonPosRat -Rat
         -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
         -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -SingleFlonumNan -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
         -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
         -RealZero -PosReal -NonNegReal -NegReal -NonPosReal -Real
         -ExactImaginary -FloatImaginary -SingleFlonumImaginary -InexactImaginary -Imaginary
         -ExactNumber -ExactComplex -FloatComplex -SingleFlonumComplex -InexactComplex -Number
         (rename-out (-Int -Integer)))

;; all the types defined here are numeric
(define (make-Base name contract predicate marshaled)
  (make-Base* name contract predicate marshaled #t))


;; Numeric hierarchy
;; All built as unions of non-overlapping base types.
;; This should make encoding mathematical properties in the base env easier.
;; The base types that don't have an interesting mathematical meaning
;; (e.g. -Byte>1, -PosFixnumNotIndex, etc.) should not be used anywhere, as
;; they should not be exposed to the user and could easily be misused in
;; the base type environment. They are not provided.
;; A lot of these contracts will be overriden in type->contract, so their
;; hairiness should not be of much consequence.

;; Is the number a fixnum on *all* the platforms Racket supports?  This
;; works because Racket compiles only on 32+ bit systems.  This check is
;; done at compile time to typecheck literals -- so use it instead of
;; `fixnum?' to avoid creating platform-dependent .zo files.
(define (portable-fixnum? n)
  (and (exact-integer? n)
       (< n (expt 2 30))
       (>= n (- (expt 2 30)))))
;; same, for indexes
(define (portable-index? n)
  (and (exact-integer? n)
       (< n (expt 2 28))
       (>= n 0)))

;; Singletons
(define -Zero (make-Value 0)) ; exact
(define -One  (make-Value 1))

;; Integers
(define -Byte>1 (make-Base 'Byte-Larger-Than-One ; unsigned
                           #'(and/c byte? (lambda (x) (> x 1)))
                           (conjoin byte? (lambda (x) (> x 1)))
                           #'-Byte>1))
(define -PosByte (*Un -One -Byte>1))
(define -Byte    (*Un -Zero -PosByte))
(define -PosIndexNotByte
  (make-Base 'Positive-Index-Not-Byte
             ;; index? will be checked at runtime, can be platform-specific
             ;; portable-index? will be checked at compile-time, must be portable
             #'(and/c index? positive? (not/c byte?))
             (lambda (x) (and (portable-index? x)
                              (positive? x)
                              (not (byte? x))))
             #'-PosIndexNotByte))
(define -PosIndex    (*Un -One -Byte>1 -PosIndexNotByte))
(define -Index       (*Un -Zero -PosIndex))
(define -PosFixnumNotIndex
  (make-Base 'Positive-Fixnum-Not-Index
             #'(and/c fixnum? positive? (not/c index?))
             (lambda (x) (and (portable-fixnum? x)
                              (positive? x)
                              (not (portable-index? x))))
             #'-PosFixnumNotIndex))
(define -PosFixnum    (*Un -PosFixnumNotIndex -PosIndex))
(define -NonNegFixnum (*Un -PosFixnum -Zero))
(define -NegFixnum
  (make-Base 'Negative-Fixnum
             #'(and/c fixnum? negative?)
             (lambda (x) (and (portable-fixnum? x)
                              (negative? x)))
             #'-NegFixnum))
(define -NonPosFixnum (*Un -NegFixnum -Zero))
(define -Fixnum       (*Un -NegFixnum -Zero -PosFixnum))
;; This type, and others like it, should *not* be exported, or used for
;; anything but building unions. Especially, no literals should be given
;; these types.
(define -PosIntNotFixnum
  (make-Base 'Positive-Integer-Not-Fixnum
             #'(and/c exact-integer? positive? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (positive? x)
                              (not (portable-fixnum? x))))
             #'-PosIntNotFixnum))
(define -PosInt    (*Un -PosIntNotFixnum -PosFixnum))
(define -NonNegInt (*Un -PosInt -Zero))
(define -Nat       -NonNegInt)
(define -NegIntNotFixnum
  (make-Base 'Negative-Integer-Not-Fixnum
             #'(and/c exact-integer? negative? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (negative? x)
                              (not (portable-fixnum? x))))
             #'-NegIntNotFixnum))
(define -NegInt    (*Un -NegIntNotFixnum -NegFixnum))
(define -NonPosInt (*Un -NegInt -Zero))
(define -Int       (*Un -NegInt -Zero -PosInt))

;; Rationals
(define -PosRatNotInt
  (make-Base 'Positive-Rational-Not-Integer
             #'(and/c exact-rational? positive? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (positive? x)
                              (not (exact-integer? x))))
             #'-PosRatNotInt))
(define -PosRat    (*Un -PosRatNotInt -PosInt))
(define -NonNegRat (*Un -PosRat -Zero))
(define -NegRatNotInt
  (make-Base 'Negative-Rational-Not-Integer
             #'(and/c exact-rational? negative? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (negative? x)
                              (not (exact-integer? x))))
             #'-NegRatNotInt))
(define -NegRat    (*Un -NegRatNotInt -NegInt))
(define -NonPosRat (*Un -NegRat -Zero))
(define -Rat       (*Un -NegRat -Zero -PosRat))

;; Floating-point numbers
;; NaN is included in all floating-point types
(define -FlonumNan (make-Base 'Float-Nan
                              #'(and/c flonum? (lambda (x) (eqv? x +nan.0)))
                              (lambda (x) (and (flonum? x) (eqv? x +nan.0)))
                              #'-FlonumNan))
(define -FlonumPosZero (make-Base 'Float-Positive-Zero
                                  #'(lambda (x) (eqv? x 0.0))
                                  (lambda (x) (eqv? x 0.0))
                                  #'-FlonumPosZero))
(define -FlonumNegZero (make-Base 'Float-Negative-Zero
                                  #'(lambda (x) (eqv? x -0.0))
                                  (lambda (x) (eqv? x -0.0))
                                  #'-FlonumNegZero))
(define -FlonumZero (*Un -FlonumPosZero -FlonumNegZero -FlonumNan))
(define -PosFlonumNoNan
  (make-Base 'Positive-Float-No-NaN
             #'(and/c flonum? positive?)
             (lambda (x) (and (flonum? x) (positive? x)))
             #'-PosFlonumNoNan))
(define -PosFlonum (*Un -PosFlonumNoNan -FlonumNan))
(define -NonNegFlonum (*Un -PosFlonum -FlonumZero))
(define -NegFlonumNoNan
  (make-Base 'Negative-Float-No-NaN
             #'(and/c flonum? negative?)
             (lambda (x) (and (flonum? x) (negative? x)))
             #'-NegFlonumNoNan))
(define -NegFlonum (*Un -NegFlonumNoNan -FlonumNan))
(define -NonPosFlonum (*Un -NegFlonum -FlonumZero))
(define -Flonum (*Un -NegFlonumNoNan -FlonumNegZero -FlonumPosZero -PosFlonumNoNan -FlonumNan)) ; 64-bit floats
;; inexact reals can be flonums (64-bit floats) or 32-bit floats
(define -SingleFlonumNan (make-Base 'Single-Flonum-Nan
                                    #'(and/c single-flonum? (lambda (x) (eqv? x +nan.f)))
                                    (lambda (x) (and (single-flonum? x) (eqv? x +nan.f)))
                                    #'-SingleFlonumNan))
(define -SingleFlonumPosZero ; disjoint from Flonum 0s
  (make-Base 'Single-Flonum-Positive-Zero
             ;; eqv? equates 0.0f0 with itself, but not eq?
             #'(lambda (x) (eqv? x 0.0f0))
             (lambda (x) (eqv? x 0.0f0))
             #'-SingleFlonumPosZero))
(define -SingleFlonumNegZero
  (make-Base 'Single-Flonum-Negative-Zero
             #'(lambda (x) (eqv? x -0.0f0))
             (lambda (x) (eqv? x -0.0f0))
             #'-SingleFlonumNegZero))
(define -SingleFlonumZero (*Un -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumNan))
(define -InexactRealNan     (*Un -FlonumNan -SingleFlonumNan))
(define -InexactRealPosZero (*Un -SingleFlonumPosZero -FlonumPosZero))
(define -InexactRealNegZero (*Un -SingleFlonumNegZero -FlonumNegZero))
(define -InexactRealZero    (*Un -InexactRealPosZero
                                 -InexactRealNegZero
                                 -InexactRealNan))
(define -PosSingleFlonumNoNan
  (make-Base 'Positive-Single-Flonum-No-Nan
             #'(and/c single-flonum? positive?)
             (lambda (x) (and (single-flonum? x) (positive? x)))
             #'-PosSingleFlonumNoNan))
(define -PosSingleFlonum    (*Un -PosSingleFlonumNoNan -SingleFlonumNan))
(define -PosInexactReal     (*Un -PosSingleFlonum -PosFlonum))
(define -NonNegSingleFlonum (*Un -PosSingleFlonum -SingleFlonumZero))
(define -NonNegInexactReal  (*Un -PosInexactReal -InexactRealZero))
(define -NegSingleFlonumNoNan
  (make-Base 'Negative-Single-Flonum-No-Nan
             #'(and/c single-flonum? negative?)
             (lambda (x) (and (single-flonum? x) (negative? x)))
             #'-NegSingleFlonumNoNan))
(define -NegSingleFlonum    (*Un -NegSingleFlonumNoNan -SingleFlonumNan))
(define -NegInexactReal     (*Un -NegSingleFlonum -NegFlonum))
(define -NonPosSingleFlonum (*Un -NegSingleFlonum -SingleFlonumZero))
(define -NonPosInexactReal  (*Un -NegInexactReal -InexactRealZero))
(define -SingleFlonum       (*Un -NegSingleFlonum -SingleFlonumNegZero -SingleFlonumPosZero -PosSingleFlonum -SingleFlonumNan))
(define -InexactReal        (*Un -SingleFlonum -Flonum))

;; Reals
(define -RealZero   (*Un -Zero -InexactRealZero))
(define -PosReal    (*Un -PosRat -PosInexactReal))
(define -NonNegReal (*Un -NonNegRat -NonNegInexactReal))
(define -NegReal    (*Un -NegRat -NegInexactReal))
(define -NonPosReal (*Un -NonPosRat -NonPosInexactReal))
(define -Real       (*Un -Rat -InexactReal))

;; Complexes
;; We could go into _much_ more precision here.
;; We could have types that reflect the size/exactness of both components
;; (e.g. PosFixnumNonNegIntComplex), to give more interesting types to
;; real-part, imag-part and others.
;; We could have Complex be a 2-argument type constructor (although it
;; could construct uninhabitable types like (Complex Integer Float), which
;; can't exist in Racket (parts must be both exact, both inexact, or one is
;; exact-zero)).  That's future work.

;; Thus, the only possible kinds of complex numbers are:
;; Zero/Rat, Zero/Flonum, Zero/SingleFlonum.
;; Rat/Rat, Flonum/Flonum, SingleFlonum/SingleFlonum.
(define -ExactImaginary
  (make-Base 'Exact-Imaginary
             #'(and/c number?
                      (not/c real?)
                      (lambda (x)
                        (and
                          (eqv? 0 (real-part x))
                          (exact? (imag-part x)))))
             (lambda (x) (and (number? x)
                              (not (real? x))
                              (eqv? 0 (real-part x))
                              (exact? (imag-part x))))
             #'-ExactImaginary))
(define -ExactComplex
  (make-Base 'Exact-Complex
             #'(and/c number?
                      (not/c real?)
                      (lambda (x)
                        (and
                          (not (eqv? 0 (real-part x)))
                          (exact? (real-part x))
                          (exact? (imag-part x)))))
             (lambda (x) (and (number? x)
                              (not (real? x))
                              (not (eqv? 0 (real-part x)))
                              (exact? (real-part x))
                              (exact? (imag-part x))))
             #'-ExactComplex))
(define -FloatImaginary (make-Base 'Float-Imaginary
                                   #'(and/c number?
                                            (lambda (x)
                                              (and (flonum? (imag-part x))
                                                   (eqv? 0 (real-part x)))))
                                   (lambda (x)
                                     (and (number? x)
                                          (flonum? (imag-part x))
                                          (eqv? 0 (real-part x))))
                                   #'-FloatImaginary))
(define -SingleFlonumImaginary (make-Base 'Single-Flonum-Imaginary
                                          #'(and/c number?
                                                   (lambda (x)
                                                     (and (single-flonum? (imag-part x))
                                                          (eqv? 0 (real-part x)))))
                                          (lambda (x)
                                            (and (number? x)
                                                 (single-flonum? (imag-part x))
                                                 (eqv? 0 (real-part x))))
                                          #'-SingleFlonumImaginary))
(define -FloatComplex (make-Base 'Float-Complex
                                 #'(and/c number?
                                          (lambda (x)
                                            (and (flonum? (imag-part x))
                                                 (flonum? (real-part x)))))
                                 (lambda (x)
                                   (and (number? x)
                                        (flonum? (imag-part x))
                                        (flonum? (real-part x))))
                                 #'-FloatComplex))
(define -SingleFlonumComplex (make-Base 'Single-Flonum-Complex
                                      #'(and/c number?
                                               (lambda (x)
                                                 (and (single-flonum? (imag-part x))
                                                      (single-flonum? (real-part x)))))
                                      (lambda (x)
                                        (and (number? x)
                                             (single-flonum? (imag-part x))
                                             (single-flonum? (real-part x))))
                                      #'-SingleFlonumComplex))
(define -ExactNumber (*Un -ExactImaginary -ExactComplex -Rat))
(define -InexactImaginary (*Un -FloatImaginary -SingleFlonumImaginary))
(define -Imaginary (*Un -ExactImaginary -InexactImaginary))
(define -InexactComplex (*Un -FloatComplex -SingleFlonumComplex))
(define -Complex (*Un -Real -Imaginary -ExactComplex -InexactComplex))
(define -Number -Complex)
