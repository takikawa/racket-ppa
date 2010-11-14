#lang racket/base
(require '#%flfxnum 
         "private/vector-wraps.rkt")

(provide fl+ fl- fl* fl/
         flabs flsqrt flexp fllog
         flsin flcos fltan flasin flacos flatan
         flfloor flceiling flround fltruncate
         fl= fl< fl<= fl> fl>= flmin flmax
         ->fl fl->exact-integer
         flvector? flvector make-flvector 
         shared-flvector make-shared-flvector
         flvector-length flvector-ref flvector-set!
         flvector-copy
         flreal-part flimag-part make-flrectangular
         in-flvector for/flvector for*/flvector)

(define-vector-wraps "flvector"
  flvector? flvector-length flvector-ref flvector-set! make-flvector
  in-flvector*
  in-flvector
  for/flvector
  for*/flvector
  flvector-copy)
