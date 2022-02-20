#lang racket/base
(provide (all-from-out teachpack/2htdp/image)
	 (all-from-out lang/posn)
         image=?
	 image mode image-color color y-place x-place
	 pulled-point angle side-count step-count
	 posn real-valued-posn
	 pen pen-style pen-cap pen-join)

(require teachpack/2htdp/image)
(require (only-in lang/imageeq image=?))
(require lang/posn)
(require (only-in racket/draw the-color-database))
(require racket/class)
(require deinprogramm/signature/signature)
(require deinprogramm/signature/signature-syntax)
(require deinprogramm/signature/signature-german)
(require (prefix-in quickcheck: deinprogramm/quickcheck/quickcheck))

(define (color-name->color name)
  (color-object->color (send the-color-database find-color name)))

(define (color-object->color obj)
  (make-color (send obj red)
              (send obj green)
              (send obj blue)
              (send obj alpha)))

(define sample-images
  (list (circle 20 "solid" "red")
        (rectangle 20 10 "solid" "blue")
        (rectangle 10 20 "solid" "green")))

(define image (signature/arbitrary
               (apply quickcheck:arbitrary-one-of image=? sample-images)
               image (predicate image?)))
(define mode (signature/arbitrary
              (quickcheck:arbitrary-one-of string=? "solid" "outline")
              mode (predicate mode?)))
(define image-color (signature/arbitrary
                     (apply quickcheck:arbitrary-one-of string=? (send the-color-database get-names))
                     image-color (predicate image-color?)))
(define color (signature/arbitrary
               (apply quickcheck:arbitrary-one-of equal? (map color-name->color (send the-color-database get-names)))
               color (predicate color?)))
(define y-place (signature/arbitrary
                 (quickcheck:arbitrary-one-of string=? "top" "bottom" "middle" "center" "baselien" "pinhole")
                 y-place (predicate y-place?)))
(define x-place (signature/arbitrary
                 (quickcheck:arbitrary-one-of string=? "left" "right" "middle" "center" "pinhole")
                 x-place (predicate x-place?)))
(define pulled-point (signature pulled-point (predicate pulled-point?)))
(define angle (signature angle (predicate angle?)))
(define side-count (signature side-count (predicate side-count?)))
(define step-count (signature step-count (predicate step-count?)))
(define posn (signature posn (predicate posn?)))
(define real-valued-posn (signature real-valued-posn (predicate real-valued-posn?)))
(define pen (signature pen (predicate pen?)))
(define pen-style (signature pen-style (predicate pen-style?)))
(define pen-cap (signature pen-cap (predicate pen-cap?)))
(define pen-join (signature pen-joint (predicate pen-join?)))




