#lang scheme

(require (lib "htdch/draw/support.scm")
         mzlib/unit)

(define void-or-true (void))
(define (imperative w@t+1 w@t) w@t+1)
  
(define-values/invoke-unit/infer canvas-native@)

(provide-signature-elements canvas-native^)
