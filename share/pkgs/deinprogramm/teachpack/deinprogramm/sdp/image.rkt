#lang racket/base
(provide (all-from-out teachpack/2htdp/image)
	 image mode image-color color y-place x-place
	 pulled-point angle side-count step-count
	 real-valued-posn
	 pen pen-style pen-cap pen-join)

(require teachpack/2htdp/image)
(require deinprogramm/signature/signature)
(require deinprogramm/signature/signature-syntax)
(require deinprogramm/signature/signature-german)

(define image (signature image (predicate image?)))
(define mode (signature mode (predicate mode?)))
(define image-color (signature image-color (predicate image-color?)))
(define color (signature color (predicate color?)))
(define y-place (signature y-place (predicate y-place?)))
(define x-place (signature x-place (predicate x-place?)))
(define pulled-point (signature pulled-point (predicate pulled-point?)))
(define angle (signature angle (predicate angle?)))
(define side-count (signature side-count (predicate side-count?)))
(define step-count (signature step-count (predicate step-count?)))
(define real-valued-posn (signature real-valued-posn (predicate real-valued-posn?)))
(define pen (signature pen (predicate pen?)))
(define pen-style (signature pen-style (predicate pen-style?)))
(define pen-cap (signature pen-cap (predicate pen-cap?)))
(define pen-join (signature pen-joint (predicate pen-join?)))




