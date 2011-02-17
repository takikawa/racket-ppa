;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname world0-stops) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/universe)
(require 2htdp/image)

(define draw (lambda (x) (circle 3 'solid 'red)))

"does big-bang stop when the initial world is already a final world?"
(big-bang 0 (stop-when zero?) (on-tick add1) (to-draw draw))

"does big bang stop when the initial world is a stop world?"
(big-bang (stop-with 0) (on-tick add1) (to-draw draw))

(define-struct stop (x))
