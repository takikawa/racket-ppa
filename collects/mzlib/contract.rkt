#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide arrow contracts from our local copy
;;

(require "private/contract-arrow.rkt")
(provide (all-from-out "private/contract-arrow.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  provide contracts for objects
;;
(require "private/contract-object.rkt")
(provide (all-from-out "private/contract-object.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style define/contract
;;

(require "private/contract-define.rkt")
(provide (all-from-out "private/contract-define.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat mutable contracts
;;
(require "private/contract-mutable.rkt")
(provide (all-from-out "private/contract-mutable.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; old-style flat struct contracts
;;
(require "private/contract-struct.rkt")
(provide (all-from-out "private/contract-struct.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; provide everything from the racket/ implementation
;; except the arrow contracts
;;

(require racket/contract/private/base
         racket/contract/private/misc
         racket/contract/private/provide
         racket/contract/private/guts
         racket/contract/private/prop
         racket/contract/private/blame
         racket/contract/private/ds
         racket/contract/private/opt
         racket/contract/private/basic-opters)

(provide 
 opt/c define-opt/c ;(all-from "private/contract-opt.rkt")
 (except-out (all-from-out racket/contract/private/ds)
             contract-struct)
 
 (all-from-out racket/contract/private/base)
 (all-from-out racket/contract/private/provide)
 (except-out (all-from-out racket/contract/private/misc)
             check-between/c
             string-len/c
             check-unary-between/c)
 (rename-out [or/c union])
 (rename-out [string-len/c string/len])
 (except-out (all-from-out racket/contract/private/guts)
             check-flat-contract
             check-flat-named-contract)
 (all-from-out racket/contract/private/prop
               racket/contract/private/blame))

