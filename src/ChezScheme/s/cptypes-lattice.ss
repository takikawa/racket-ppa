;;; cptypes-lattice.ss
;;; Copyright 1984-2020 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

; bottom -> empty set / the expression raised an error
; <something> -> some other set
; ptr -> all single values expressions
; #f -> a result that may be single or multiple valued.

; bottom => <something> => ptr => #f

; properties of bottom:
; (implies? x bottom): only for x=bottom
; (implies? bottom y): always
; (disjoint? x bottom): always
; (disjoint? bottom y): always
; remember to check (implies? x bottom) before (implies? x something)

#|
 - predicate: They may be:
              * a symbol to indicate the type, like 'vector 'pair 'fixnum
                (there are a few fake values, in particular 'bottom is used
                 for an empty set, and in particular as the result of an
                 expression that always raises an error)
              * a nanopass-quoted value that is okay-to-copy?, like
                `(quote 0) `(quote 5) `(quote #t) `(quote '())
                (this doesn't include `(quote <record-type-descriptor>))
              * a record #[pred-singleton <fixnum>] that is a bit field for the
                union of singletons, in particular all immediates except chars
              * a record #[pred-multiplet <fixnum>] that is a bit field for the 
                union of multiplets, like symbols, chars and numbers except exact integers
              * a record #[pred-$record/rtd <rtd>] to signal that it's a
                record of type <rtd>
              * a record #[pred-$record/ref <ref>] to signal that it's a
                record of a type that is stored in the variable <ref>
                (these may collide with other records)
              * TODO?: add something to indicate that x is a procedure that is the
                       constructor/setter/getter/predicate of a record of that type
              * a record #[pred-or <sin> <mul> <nor> <exi> <rec>] where:
                  <sim> is a predicate for singletons
                  <mul> is a predicate for multiplets
                  <nor> is a predicate for things not in the other fields
                  <exi> is a predicate for exact integers
                  <rec> is a predicate for records
|#

(module cptypes-lattice
  (primref-name/nqm->predicate
   ptr-pred
   char-pred
   maybe-char-pred
   symbol-pred
   maybe-symbol-pred
   flonum-pred
   real-pred
   number-pred
   flzero-pred
   $fixmediate-pred
   $list-pred ; immutable lists
   boolean-pred
   true-pred ; anything that is not #f
   true-rec  ; only the #t object
   false-rec
   void-rec
   null-rec
   eof-rec
   bwp-rec
   predicate-is-ptr?
   predicate-implies?
   predicate-disjoint?
   predicate-intersect
   predicate-union
   predicate-substract
   make-pred-$record/rtd
   make-pred-$record/ref)

  (define-record-type pred-or
    (fields sin mul nor exi rec)
    (nongenerative #{pred-or nlomo7xtc1nguv2umpzwho0dt-2})
    (sealed #t))

  (define-record-type pred-singleton
    (fields mask)
    (nongenerative #{pred-singleton m4e7t2fuam2my9kt17zpmxnzc-2})
    (sealed #t))

  (define-record-type pred-multiplet
    (fields mask)
    (nongenerative #{pred-multiplets c2w1h2t2wzoyn0sq8jicb77lw-0})
    (sealed #t))

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref maybe-rtd)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-1})
    (sealed #t))

  (define ($black-hole) '#6=#6#)
  (include "base-lang.ss")
  (with-output-language (Lsrc Expr)
    (define void-rec `(quote ,(void)))
    (define true-rec `(quote #t))
    (define false-rec `(quote #f))
    (define null-rec `(quote ()))
    (define eof-rec `(quote #!eof))
    (define bwp-rec `(quote #!bwp))
    (define black-hole-rec `(quote ,($black-hole)))
    (define unbound-rec `(quote ,($unbound-object))))

  (module (singleton-rec->mask
           build-pred-singleton
           singleton-pred-mask
           singleton-pred
           true-singleton-pred
           immediate*-pred
           boolean-pred)

    (define false-object-mask               #b0000000001)
    (define true-object-mask                #b0000000010)
    (define null-object-mask                #b0000000100)
    (define void-object-mask                #b0000001000)
    (define eof-object-mask                 #b0000010000)
    (define bwp-object-mask                 #b0000100000)
    (define black-hole-object-mask          #b0001000000)
    (define unbound-object-mask             #b0010000000)

    (define immediate*-pred-mask            #b0011111111)
    (define singleton-pred-mask             #b0011111111) ; for the check in is-ptr?

    (define boolean-pred-mask (fxior true-object-mask false-object-mask))
    (define true-singleton-pred-mask (fxand singleton-pred-mask (fxnot false-object-mask)))

    (define (singleton-rec->mask x)
      (cond
        [(Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d)
            (cond
              [(not d) false-object-mask]
              [(eq? d #t) true-object-mask]
              [(null? d) null-object-mask]
              [(eq? d (void)) void-object-mask]
              [(eof-object? d) eof-object-mask]
              [(bwp-object? d) bwp-object-mask]
              [(eq? d ($black-hole)) black-hole-object-mask]
              [($unbound-object? d) unbound-object-mask]
              [else ($oops 'singleton-rec->mask "invalid value ~s" d)])]
           [else ($oops 'singleton-rec->mask "invalid expression ~s" x)])]
        [else ($oops 'singleton-rec->mask "invalid expression ~s" x)]))

    (define (mask->singleton-rec y)
      (cond
        [(fx= y false-object-mask) false-rec]
        [(fx= y true-object-mask) true-rec]
        [(fx= y null-object-mask) null-rec]
        [(fx= y void-object-mask) void-rec]
        [(fx= y eof-object-mask) eof-rec]
        [(fx= y bwp-object-mask) bwp-rec]
        [(fx= y black-hole-object-mask) black-hole-rec]
        [(fx= y unbound-object-mask) unbound-rec]
        [else ($oops 'mask->singleton-rec "invalid mask number ~s" y)]))

    (define (build-pred-singleton mask x y)
      (cond
        [(and x (fx= (pred-singleton-mask x) mask)) x]
        [(and y (fx= (pred-singleton-mask y) mask)) y]
        [else
         (case (fxbit-count mask)
           [(0) 'bottom]
           [(1) (mask->singleton-rec mask)]
           [else (make-pred-singleton mask)])]))

    (define boolean-pred (make-pred-singleton boolean-pred-mask))
    (define immediate*-pred (make-pred-singleton immediate*-pred-mask))
    (define singleton-pred (make-pred-singleton singleton-pred-mask))
    (define true-singleton-pred (make-pred-singleton true-singleton-pred-mask))
  )

  (module (multiplet-rec->mask
           build-pred-multiplet
           multiplet-pred-mask multiplet-pred
           number*-pred real*-pred ratnum-pred
           flonum-pred flinteger-pred flzero-pred
           exact*-pred inexact-pred
           exact-complex-pred inexact-complex-pred
           char-pred
           symbol-pred interned-symbol-pred uninterned-symbol-pred gensym-pred)

    (define exact-complex-mask               #b0000000001)
    (define ratnum-mask                      #b0000000010)
    (define inexact-complex-mask             #b0000000100)
    (define flonum*-mask                     #b0000001000)
    (define flinteger*-mask                  #b0000010000)
    (define flzero-mask                      #b0000100000)
    ; fixnum and bignum are in other field

    (define char-mask                        #b0001000000)
    (define interned-symbol-mask             #b0010000000)
    (define uninterned-symbol-mask           #b0100000000)
    (define gensym-mask                      #b1000000000)

    (define number*-pred-mask                #b0000111111)
    (define symbol-pred-mask                 #b1110000000)
    (define multiplet-pred-mask                  #b1111111111) ; for the check in is-ptr?

    (define flonum-pred-mask (fxior flonum*-mask flinteger*-mask flzero-mask))
    (define flinteger-pred-mask (fxior flinteger*-mask flzero-mask))
    (define real*-pred-mask (fxior ratnum-mask flonum-pred-mask))
    (define exact*-pred-mask (fxior ratnum-mask exact-complex-mask))
    (define inexact-pred-mask (fxior flonum-pred-mask inexact-complex-mask))

    (define (multiplet-rec->mask x)
      (cond
        [(Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d)
            (cond
              [(char? d) char-mask]
              [($exactnum? d) exact-complex-mask]
              [(ratnum? d) ratnum-mask]
              [($inexactnum? d) inexact-complex-mask]
              [(gensym? d) gensym-mask]
              [(uninterned-symbol? d) uninterned-symbol-mask]
              [(interned-symbol? d) interned-symbol-mask]
              [(flzero? d) flzero-mask]
              [(flinteger? d) flinteger*-mask]
              [(flonum? d) flonum*-mask]
              [else ($oops 'multiplet-rec->mask "invalid value ~s" d)])]
           [else ($oops 'multiplet-rec->mask "invalid expression ~s" x)])]
        [else ($oops 'multiplet-rec->mask "invalid expression ~s" x)]))

    (define (build-pred-multiplet mask x y)
      (cond
        [(and x (fx= (pred-multiplet-mask x) mask)) x]
        [(and y (fx= (pred-multiplet-mask y) mask)) y]
        [(fx= mask 0) 'bottom]
        [else (make-pred-multiplet mask)]))

    (define number*-pred (make-pred-multiplet number*-pred-mask))
    (define real*-pred (make-pred-multiplet real*-pred-mask))
    (define ratnum-pred (make-pred-multiplet ratnum-mask))
    (define flonum-pred (make-pred-multiplet flonum-pred-mask))
    (define flinteger-pred (make-pred-multiplet flinteger-pred-mask))
    (define flzero-pred (make-pred-multiplet flzero-mask))
    (define exact*-pred (make-pred-multiplet exact*-pred-mask))
    (define inexact-pred (make-pred-multiplet inexact-pred-mask))
    (define exact-complex-pred (make-pred-multiplet exact-complex-mask))
    (define inexact-complex-pred (make-pred-multiplet inexact-complex-mask))
    (define char-pred (make-pred-multiplet char-mask))
    (define symbol-pred (make-pred-multiplet symbol-pred-mask))
    (define interned-symbol-pred (make-pred-multiplet interned-symbol-mask))
    (define uninterned-symbol-pred (make-pred-multiplet uninterned-symbol-mask))
    (define gensym-pred (make-pred-multiplet gensym-mask))
    (define multiplet-pred (make-pred-multiplet multiplet-pred-mask))
  )

  (define immediate-pred (make-pred-or immediate*-pred char-pred 'bottom 'bottom 'bottom))
  (define true-pred (make-pred-or true-singleton-pred multiplet-pred 'normalptr 'exact-integer '$record))
  (define ptr-pred (make-pred-or singleton-pred multiplet-pred 'normalptr 'exact-integer '$record))
  (define null-or-pair-pred (make-pred-or null-rec 'bottom 'pair 'bottom 'bottom))
  (define $list-pred (make-pred-or null-rec 'bottom '$list-pair 'bottom 'bottom))
  (define $fixmediate-pred (make-pred-or immediate*-pred char-pred 'bottom 'fixnum 'bottom))
  (define maybe-fixnum-pred (make-pred-or false-rec 'bottom 'bottom 'fixnum 'bottom))
  (define eof/fixnum-pred (make-pred-or eof-rec 'bottom 'bottom 'fixnum 'bottom))
  (define maybe-exact-integer-pred (make-pred-or false-rec 'bottom 'bottom 'exact-integer 'bottom))
  (define maybe-flonum-pred (make-pred-or false-rec flonum-pred 'bottom 'bottom 'bottom))
  (define integer-pred (make-pred-or 'bottom flinteger-pred 'bottom 'exact-integer 'bottom))
  (define exact-pred (make-pred-or 'bottom exact*-pred 'bottom 'exact-integer 'bottom))
  (define real-pred (make-pred-or 'bottom real*-pred 'bottom 'exact-integer 'bottom))
  (define number-pred (make-pred-or 'bottom number*-pred 'bottom 'exact-integer 'bottom))
  (define maybe-number-pred (make-pred-or false-rec number*-pred 'bottom 'exact-integer 'bottom))
  (define maybe-symbol-pred (make-pred-or false-rec symbol-pred 'bottom 'bottom 'bottom))
  (define maybe-procedure-pred (make-pred-or false-rec 'bottom 'procedure 'bottom 'bottom))
  (define maybe-string-pred (make-pred-or false-rec 'bottom 'string 'bottom 'bottom))
  (define eof/string-pred (make-pred-or eof-rec 'bottom 'string 'bottom 'bottom))
  (define maybe-bytevector-pred (make-pred-or false-rec 'bottom 'bytevector 'bottom 'bottom))
  (define eof/bytevector-pred (make-pred-or eof-rec 'bottom 'bytevector 'bottom 'bottom))
  (define maybe-pair-pred (make-pred-or false-rec 'bottom 'pair 'bottom 'bottom))
  (define maybe-port-pred (make-pred-or false-rec 'bottom 'port 'bottom 'bottom))
  (define maybe-symbol/string-pred (make-pred-or false-rec symbol-pred 'string 'bottom 'bottom))
  (define maybe-$record-pred (make-pred-or false-rec 'bottom 'bottom 'bottom '$record))
  (define maybe-char-pred (make-pred-or false-rec char-pred 'bottom 'bottom 'bottom))
  (define eof/char-pred (make-pred-or eof-rec char-pred 'bottom 'bottom 'bottom))

  ; This can be implemented with implies?
  ; but let's use the straightforward test.
  (define (predicate-is-ptr? x)
    (and (pred-or? x)
         (let ([i (pred-or-sin x)])
           (and (pred-singleton? i)
                (fx= (pred-singleton-mask i) singleton-pred-mask)))
         (let ([u (pred-or-mul x)])
           (and (pred-multiplet? u)
                (fx= (pred-multiplet-mask u) multiplet-pred-mask)))
         (eq? (pred-or-nor x) 'normalptr)
         (eq? (pred-or-exi x) 'exact-integer)
         (eq? (pred-or-rec x) '$record)))

  ; don't use rtd-* as defined in record.ss in case we're building a patch
  ; file for cross compilation, because the offsets may be incorrect
  (define rtd-ancestors (csv7:record-field-accessor #!base-rtd 'ancestors))
  (define rtd-flds (csv7:record-field-accessor #!base-rtd 'flds))

  ;could be a ctrtd
  (define (pred-$record-maybe-rtd x)
    (cond
     [(pred-$record/rtd? x) (pred-$record/rtd-rtd x)]
     [(pred-$record/ref? x) (pred-$record/ref-maybe-rtd x)]
     [else #f]))

  (define (rtd-obviously-incompatible? x y)
    (let ([x-flds (rtd-flds x)]
          [y-flds (rtd-flds y)])
      (or (and (fixnum? x-flds) (not (fixnum? y-flds)))
          (and (not (fixnum? x-flds)) (fixnum? y-flds)))))

  ;true when x is an ancestor of y
  ;includes the case when they are the same
  (define (rtd-ancestor*? x y)
    (or (eq? x y)
        (let ()
          (define ax (rtd-ancestors x))
          (define lx (vector-length ax))
          (define ay (rtd-ancestors y))
          (define ly (vector-length ay))
          (and (fx<= lx ly)
               (eq? x (vector-ref ay (fx- lx 1)))))))

  ;includes the case when they are the same
  ;or when one is the ancestor of the other
  (define (rdt-last-common-ancestor* x y)
    (cond 
      [(eq? x y) x]
      [else
       (let ()
         (define ax (rtd-ancestors x))
         (define lx (vector-length ax))
         (define ay (rtd-ancestors y))
         (define ly (vector-length ay))
         (cond
           [(and (fx<= lx ly)
                 (eq? x (vector-ref ay (fx- lx 1))))
            x]
           [(and (fx<= ly lx)
                 (eq? y (vector-ref ax (fx- ly 1))))
            y]
           [else
            ;; binary search to find a common prefix, given that
            ;; no elements are the same after a common prefix
            (let loop ([lo 0] [hi (fxmin lx ly)])
              (cond
                [(fx= lo hi) #f]
                [else (let* ([i (fxquotient (fx+ lo hi) 2)]
                             [v (vector-ref ax i)])
                        (cond
                          [(eq? v (vector-ref ay i))
                           (or (loop (fx+ i 1) hi)
                               v)]
                          [else
                           (loop lo i)]))]))]))]))

  ; nqm: no question mark
  ; Transform the types used in primdata.ss
  ; to the internal representation used here
  ; When extend is #f the result is a predicate that recognizes less values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #t and (something x) ==> (#3%something x)
  ; When extend is #t the result is a predicate that recognizes more values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #f and (something x) ==> <error>
  ; In case the non extended version is not #f, the extended version must be not #f
  (define (primref-name/nqm->predicate name extend?)
    (cond
      [(not name)
       #f]
      [(pair? name)
       (cond
         [(equal? name '(ptr . ptr))
          'pair]
         [else
          (if (not extend?) 'bottom 'pair)])]
      [else
       (let ([r (do-primref-name/nqm->predicate name extend?)])
         (cond
           [(pair? r)
            (if extend? (cdr r) (car r))]
           [else
            r]))]))

  (define (do-primref-name/nqm->predicate name extend?)
    (case name
      [bottom 'bottom]
      [ptr ptr-pred]
      [sub-ptr (cons 'bottom ptr-pred)]

      [char char-pred]
      [maybe-char maybe-char-pred]
      [eof/char eof/char-pred]
      [boolean boolean-pred]
      [true true-pred]
      [false false-rec]
      [void void-rec]
      [null null-rec]
      [eof-object eof-rec]
      [bwp-object bwp-rec]
      [$immediate immediate-pred]

      [pair 'pair]
      [maybe-pair maybe-pair-pred]
      [list (cons $list-pred null-or-pair-pred)]
      [list-assuming-immutable $list-pred]
      [box 'box]
      [vector 'vector]
      [string 'string]
      [sub-string '(bottom . string)]
      [maybe-string maybe-string-pred]
      [eof/string eof/string-pred]
      [bytevector 'bytevector]
      [maybe-bytevector maybe-bytevector-pred]
      [eof/bytevector eof/bytevector-pred]
      [fxvector 'fxvector]
      [flvector 'flvector]
      [pathname 'string]
      [maybe-pathname maybe-string-pred]
      [procedure 'procedure]
      [maybe-procedure maybe-procedure-pred]
      [maybe-who maybe-symbol/string-pred]

      [gensym gensym-pred]
      [uninterned-symbol uninterned-symbol-pred]
      [interned-symbol interned-symbol-pred]
      [symbol symbol-pred]
      [maybe-symbol maybe-symbol-pred]
      [sub-symbol (cons 'bottom symbol-pred)]
      [maybe-sub-symbol (cons false-rec maybe-symbol-pred)]

      [fixnum 'fixnum]
      [(sub-fixnum bit length sub-length ufixnum sub-ufixnum pfixnum index sub-index u8 s8 u8/s8) '(bottom . fixnum)]
      [maybe-fixnum maybe-fixnum-pred]
      [maybe-ufixnum (cons false-rec maybe-fixnum-pred)]
      [(eof/length eof/u8) (cons eof-rec eof/fixnum-pred)]
      [bignum 'bignum]
      [(exact-integer sint) 'exact-integer]
      [(uint sub-uint nzuint exact-uinteger sub-sint) '(bottom . exact-integer)]
      [maybe-uint (cons false-rec maybe-exact-integer-pred)]
      [ratnum ratnum-pred]
      [flonum flonum-pred]
      [sub-flonum (cons 'bottom flonum-pred)]
      [maybe-flonum maybe-flonum-pred]
      [real real-pred]
      [rational (cons 'exact-integer real-pred)]
      [integer integer-pred]
      [(uinteger sub-integer) (cons 'bottom integer-pred)]
      [inexact-number inexact-pred]
      [exact-number exact-pred]
      [$inexactnum inexact-complex-pred]
      [$exactnum exact-complex-pred]
      [integer integer-pred]
      [flinteger flinteger-pred]
      [number number-pred]
      [sub-number (cons 'bottom number-pred)]
      [maybe-number maybe-number-pred]

      [port 'port]
      [(textual-input-port textual-output-port textual-port
        binary-input-port binary-output-port binary-port
        input-port output-port file-port) '(bottom . port)]
      [(sub-port) '(bottom . normalptr)]
      [(maybe-textual-input-port maybe-textual-output-port
        maybe-binary-input-port maybe-binary-output-port) (cons false-rec maybe-port-pred)]

      [$record '$record]
      [(record rtd) '(bottom . $record)] ; not sealed
      [(maybe-rtd) (cons false-rec maybe-$record-pred)]
      [(transcoder) '(bottom . $record)]  ; opaque, sealed
      [(maybe-transcoder) (cons false-rec maybe-$record-pred)]
      [(rcd sfd timeout) '(bottom . $record)] ; not opaque, sealed
      [(maybe-rcd maybe-sub-rcd maybe-sfd maybe-timeout) (cons false-rec maybe-$record-pred)]

      [else (cons 'bottom true-pred)])); for all other types that exclude #f

  (define (check-constant-is? x pred?)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (pred? d)]
           [else #f])))

  (define (check-constant-eqv? x v)
    (and (Lsrc? x)
         (nanopass-case (Lsrc Expr) x
           [(quote ,d) (eqv? d v)]
           [else #f])))

  (define (constant-value x)
    (if (Lsrc? x)
        (nanopass-case (Lsrc Expr) x
          [(quote ,d) d]
          [else ($oops 'constant-value "invalid expression ~s" x)])
        ($oops 'constant-value "invalid expression ~s" x)))

  (define (exact-integer? x)
    (and (integer? x) (exact? x)))

  (define (interned-symbol? x)
    (and (symbol? x)
         (not (gensym? x))
         (not (uninterned-symbol? x))))

  (define (predicate-union/singleton x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(Lsrc? x)
       (let ([mx (singleton-rec->mask x)])
         (cond
           [(Lsrc? y)
            (cond
              [(eqv? (constant-value x)
                     (constant-value y))
               y]
              [else
               (let ([my (singleton-rec->mask y)])
                 (build-pred-singleton (fxior mx my) #f #f))])]
           [(pred-singleton? y)
            (let ([my (pred-singleton-mask y)])
              (build-pred-singleton (fxior mx my) y #f))]
           [else 
            ($oops 'predicate-union/singleton "invalid expression ~s" y)]))]
      [(pred-singleton? x)
       (let ([mx (pred-singleton-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (singleton-rec->mask y)])
              (build-pred-singleton (fxior mx my) x #f))]
           [(pred-singleton? y)
            (let ([my (pred-singleton-mask y)])
              (build-pred-singleton (fxior mx my) y x))]
           [else
            ($oops 'predicate-union/singleton "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-union/singleton "invalid expression ~s" x)]))

  (define (predicate-union/multiplet x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(Lsrc? x)
       (let ([mx (multiplet-rec->mask x)])
         (cond
           [(Lsrc? y)
            (cond
              [(eqv? (constant-value x)
                     (constant-value y))
               y]
              [else
               (let ([my (multiplet-rec->mask y)])
                 (build-pred-multiplet (fxior mx my) #f #f))])]
           [(pred-multiplet? y)
            (let ([my (pred-multiplet-mask y)])
              (build-pred-multiplet (fxior mx my) y #f))]
           [else 
            ($oops 'predicate-union/multiplet "invalid expression ~s" y)]))]
      [(pred-multiplet? x)
       (let ([mx (pred-multiplet-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (multiplet-rec->mask y)])
              (build-pred-multiplet (fxior mx my) x #f))]
           [(pred-multiplet? y)
            (let ([my (pred-multiplet-mask y)])
              (build-pred-multiplet (fxior mx my) y x))]
           [else
            ($oops 'predicate-union/multiplet "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-union/multiplet "invalid expression ~s" x)]))

  (define (union/simple x pred? y)
    (cond
      [(or (check-constant-is? x pred?)
           (eq? x y))
       y]
      [else
       'normalptr]))

  (define (predicate-union/normal x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y 'normalptr) y]
      [(eq? x 'normalptr) x]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             y]
            [(or (number? dy) (char? dy) (symbol? dy))
             (if (or (check-constant-is? x number?)
                     (check-constant-is? x char?)
                     (check-constant-is? x symbol?)
                     (pred-multiplet? x))
                 (predicate-union/multiplet x y)
                 'normalptr)]
            [(vector? dy) (union/simple x vector? 'vector)]; i.e. #()
            [(string? dy) (union/simple x string? 'string)]; i.e. ""
            [(bytevector? dy) (union/simple x bytevector? 'bytevector)] ; i.e. '#vu8()
            [(fxvector? dy) (union/simple x fxvector? 'fxvector)] ; i.e. '#vfx()
            [(flvector? dy) (union/simple x flvector? 'flvector)] ; i.e. '#vfl()
            [else
             'normalptr])])]
      [(pred-multiplet? y)
       (if (or (check-constant-is? x number?)
               (check-constant-is? x char?)
               (check-constant-is? x symbol?)
               (pred-multiplet? x))
           (predicate-union/multiplet x y)
           'normalptr)]
      [else
       (case y
         [(pair $list-pair)
          (cond 
	        [(or (eq? x 'pair)
		         (eq? x '$list-pair))
	         'pair]
	        [else
	         'normalptr])]
         [(vector) (union/simple x vector? y)]; i.e. #()
         [(string) (union/simple x string? y)]; i.e. ""
         [(bytevector) (union/simple x bytevector? y)] ; i.e. '#vu8()
         [(fxvector) (union/simple x fxvector? y)] ; i.e. '#vfx()
         [(flvector) (union/simple x flvector? y)] ; i.e. '#vfl()
         [else
          'normalptr])]))

  (define (predicate-union/exact-integer x y)
    (or (cond
          [(eq? x y) y]
          [(eq? x 'bottom) y]
          [(eq? y 'bottom) x]
          [(eq? y 'exact-integer) 'exact-integer]
          [(eq? x 'exact-integer) 'exact-integer]
          [(eq? y 'fixnum)
           (and (check-constant-is? x target-fixnum?)
                'fixnum)]
          [(eq? y 'bignum)
           (and (check-constant-is? x target-bignum?)
                'bignum)]
          [(eq? x 'fixnum)
           (and (check-constant-is? y target-fixnum?)
                'fixnum)]
          [(eq? x 'bignum)
           (and (check-constant-is? y target-bignum?)
                'bignum)]
          [else
           (let ([dx (constant-value x)]
                 [dy (constant-value y)])
             (cond
               [(eqv? dx dy)
                y]
               [(target-fixnum? dx)
                (and (target-fixnum? dy)
                     'fixnum)]
               [else #;(target-bignum? dx)
                (and (target-bignum? dy)
                     'bignum)]))])
        'exact-integer))

  (define (predicate-union/record x y)
    (cond
      [(eq? x y) y]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(eq? y '$record) y]
      [(eq? x '$record) x]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               y]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor*? y-rtd x-rtd) y '$record)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor*? x-rtd y-rtd) x '$record)]
              [else
               (let ([lca-rtd (rdt-last-common-ancestor* x-rtd y-rtd)])
                 (cond
                   [(not lca-rtd) '$record]
                   [(eqv? lca-rtd y-rtd) y]
                   [(eqv? lca-rtd x-rtd) x]
                   [else (make-pred-$record/rtd lca-rtd)]))]))]
         [else
          '$record])]
      [(pred-$record/ref? y)
       (cond
         [(pred-$record/ref? x)
          (if (eq? (pred-$record/ref-ref x)
                   (pred-$record/ref-ref y))
             y
             '$record)]
         [else
          '$record])]
      [else
       '$record]))

  (define (predicate-intersect/singleton x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x) (constant-value y))
              x
              'bottom)]
         [(pred-singleton? y)
          (let ([mx (singleton-rec->mask x)]
                [my (pred-singleton-mask y)])
            (if (not (fx= (fxand mx my) 0))
                x
                'bottom))]
         [else
          ($oops 'predicate-intersect/singleton "invalid expression ~s" y)])]
      [(pred-singleton? x)
       (let ([mx (pred-singleton-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (singleton-rec->mask y)])
              (if (not (fx= (fxand mx my) 0))
                  y
                  'bottom))]
           [(pred-singleton? y)
            (let ([my (pred-singleton-mask y)])
              (build-pred-singleton (fxand mx my) x y))]
           [else
            ($oops 'predicate-intersect/singleton "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-intersect/singleton "invalid expression ~s" x)]))

  (define (predicate-intersect/multiplet x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x) (constant-value y))
              x
              'bottom)]
         [(pred-multiplet? y)
          (let ([mx (multiplet-rec->mask x)]
                [my (pred-multiplet-mask y)])
            (if (not (fx= (fxand mx my) 0))
                x
                'bottom))]
         [else
          ($oops 'predicate-intersect/multiplet "invalid expression ~s" y)])]
      [(pred-multiplet? x)
       (let ([mx (pred-multiplet-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (multiplet-rec->mask y)])
              (if (not (fx= (fxand mx my) 0))
                  y
                  'bottom))]
           [(pred-multiplet? y)
            (let ([my (pred-multiplet-mask y)])
              (build-pred-multiplet (fxand mx my) x y))]
           [else
            ($oops 'predicate-intersect/multiplet "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-intersect/multiplet "invalid expression ~s" x)]))

  (define (intersect/simple x pred? qpred y)
     (cond
       [(and pred? (check-constant-is? x pred?))
        x]
       [(eq? x qpred)
        y]
       [else
        'bottom]))

  (define (predicate-intersect/normal x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y 'normalptr) x]
      [(eq? x 'normalptr) y]
      [(Lsrc? y)
       (nanopass-case (Lsrc Expr) y
         [(quote ,d1)
          (define dy d1)
          (cond
            [(check-constant-eqv? x dy)
             x]
            [(or (number? dy) (char? dy) (symbol? dy))
             (if (or (check-constant-is? x number?)
                     (check-constant-is? x char?)
                     (check-constant-is? x symbol?)
                     (pred-multiplet? x))
                 (predicate-intersect/multiplet x y)
                 'bottom)]
            [(vector? dy) (intersect/simple x #f 'vector y)]; i.e. #()
            [(string? dy) (intersect/simple x #f 'string y)]; i.e. ""
            [(bytevector? dy) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
            [(fxvector? dy) (intersect/simple x #f 'fxvector y)] ; i.e. '#vfx()
            [(flvector? dy) (intersect/simple x #f 'flvector y)] ; i.e. '#vfl()
            [else
             'bottom])])]
      [(pred-multiplet? y)
       (if (or (check-constant-is? x number?)
               (check-constant-is? x char?)
               (check-constant-is? x symbol?)
               (pred-multiplet? x))
           (predicate-intersect/multiplet x y)
           'bottom)]
      [else
       (case y
         [(pair $list-pair)
          (cond
            [(or (eq? x 'pair)
                 (eq? x '$list-pair))
             '$list-pair]
            [else
             'bottom])]
         [(vector) (intersect/simple x vector? 'vector y)]; i.e. #()
         [(string) (intersect/simple x string? 'string y)]; i.e. ""
         [(bytevector) (intersect/simple x bytevector? 'bytevector y)] ; i.e. '#vu8()
         [(fxvector) (intersect/simple x fxvector? 'fxvector y)] ; i.e. '#vfx()
         [(flvector) (intersect/simple x flvector? 'flvector y)] ; i.e. '#vfl()
         [else
          'bottom])]))

  (define (predicate-intersect/exact-integer x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y 'exact-integer) x]
      [(eq? x 'exact-integer) y]
      [(Lsrc? x)
       (let ([dx (constant-value x)])
         (if (cond
               [(check-constant-eqv? y dx)
                #t]
               [(target-fixnum? dx)
                (eq? y 'fixnum)]
               [else #;(target-bignum? dx)
                (eq? y 'bignum)])
             x
             'bottom))]
      [else
       (if (cond
		     [(eq? x 'fixnum)
              (check-constant-is? y target-fixnum?)]
             [else #;(eq? x 'bignum)
  		      (check-constant-is? y target-bignum?)])
          y
          'bottom)]))

  (define (intersect/record x y)
    (cond
      [(or (pred-$record/ref? x)
           (pred-$record/rtd? x))
       x]
      [(eq? x '$record)
       y]
      [else
       'bottom]))

  (define (predicate-intersect/record x y)
    (cond
      [(eq? x y) x]
      [(eq? y 'bottom) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y '$record) x]
      [(eq? x '$record) y]
      [(pred-$record/rtd? y)
       (cond
         [(pred-$record/rtd? x)
          (let ([x-rtd (pred-$record/rtd-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (cond
              [(eqv? x-rtd y-rtd)
               x]
              [(record-type-sealed? x-rtd)
               (if (rtd-ancestor*? y-rtd x-rtd) x 'bottom)]
              [(record-type-sealed? y-rtd)
               (if (rtd-ancestor*? x-rtd y-rtd) y 'bottom)]
              [else
               (cond
                 [(rtd-ancestor*? y-rtd x-rtd) x]
                 [(rtd-ancestor*? x-rtd y-rtd) y]
                 [else 'bottom])]))]
         [(pred-$record/ref? x)
          (let ([x-rtd (pred-$record/ref-maybe-rtd x)]
                [y-rtd (pred-$record/rtd-rtd y)])
            (if (and x-rtd (rtd-obviously-incompatible? x-rtd y-rtd))
                'bottom
                (intersect/record x y)))]
         [else
          (intersect/record x y)])]
      [(pred-$record/ref? y)
       (let ([y-rtd (pred-$record/ref-maybe-rtd y)]
             [x-rtd (pred-$record-maybe-rtd x)])
         (if (and x-rtd y-rtd (rtd-obviously-incompatible? x-rtd y-rtd))
             'bottom
             (intersect/record x y)))]
      [else
       (case y
         [($record)
          (intersect/record x y)]
         [else
          'bottom])]))

  (define (predicate-substract/singleton x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? y 'bottom) x]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x)
                    (constant-value y))
              'bottom
              x)]
         [(pred-singleton? y)
          (let ([my (pred-singleton-mask y)]
                [mx (singleton-rec->mask x)])
            (if (not (fx= (fxand mx my) 0))
                'bottom
                x))]
         [else
          ($oops 'predicate-substract/singleton "invalid expression ~s" y)])]
      [(pred-singleton? x)
       (let ([mx (pred-singleton-mask x)])
         (cond
           [(Lsrc? y)
            (let ([my (singleton-rec->mask y)])
              (build-pred-singleton (fxand mx (fxnot my)) x #f))]
           [(pred-singleton? y)
            (let ([my (pred-singleton-mask y)])
              (build-pred-singleton (fxand mx (fxnot my)) x #f))]
           [else
            ($oops 'predicate-substract/singleton "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-substract/singleton "invalid expression ~s" x)]))

  (define (predicate-substract/multiplet x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? y 'bottom) x]
      [(eq? x 'bottom) 'bottom]
      [(Lsrc? x)
       (cond
         [(Lsrc? y)
          (if (eqv? (constant-value x)
                    (constant-value y))
              'bottom
              x)]
         [(pred-multiplet? y)
          (let ([my (pred-multiplet-mask y)]
                [mx (multiplet-rec->mask x)])
            (if (not (fx= (fxand mx my) 0))
                'bottom
                x))]
         [else
          ($oops 'predicate-substract/multiplet "invalid expression ~s" y)])]
      [(pred-multiplet? x)
       (let ([mx (pred-multiplet-mask x)])
         (cond
           [(Lsrc? y)
            x]
           [(pred-multiplet? y)
            (let ([my (pred-multiplet-mask y)])
              (build-pred-multiplet (fxand mx (fxnot my)) x #f))]
           [else
            ($oops 'predicate-substract/multiplet "invalid expression ~s" y)]))]
      [else
       ($oops 'predicate-substract/multiplet "invalid expression ~s" x)]))

  (define (predicate-substract/normal x y)
    (if (predicate-implies?/normal x y)
        'bottom
         x))

  (define (predicate-substract/exact-integer x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? y 'exact-integer) 'bottom]
      [(eq? x 'exact-integer)
       (case y
         [(fixnum) 'bignum]
         [(bignum) 'fixnum]
         [else 'exact-integer])]
      [(Lsrc? x)
       (let ([dx (constant-value x)])
         (if (cond
               [(Lsrc? y)
                (eqv? dx (constant-value y))]
               [(target-fixnum? dx)
                (eq? y 'fixnum)]
               [else #;(target-bignum? dx)
                (eq? y 'bignum)])
            'bottom
            x))]
      [else x]))

  (define (predicate-substract/record x y)
    (cond
      [(eq? x y) 'bottom]
      [(eq? x 'bottom) 'bottom]
      [(eq? y '$record) 'bottom]
      [(pred-$record/rtd? x)
       (if (and (pred-$record/rtd? y)
                (rtd-ancestor*? (pred-$record/rtd-rtd y)
                                (pred-$record/rtd-rtd x)))
           'bottom
           x)]
      [(pred-$record/ref? x)
       (if (and (pred-$record/ref? y)
                (eq? (pred-$record/ref-ref x)
                     (pred-$record/ref-ref y)))
          'bottom
          x)]
      [else x]))

  (define (predicate-implies? x y)
    (eq? (predicate-union x y) y))

  (define (predicate-implies?/normal x y)
    (eq? (predicate-union/normal x y) y))

  (define (predicate-disjoint? x y)
    (eq? (predicate-intersect x y) 'bottom))

  (define (predicate->class x)
    (cond
      #;[(eq? x 'bottom) 'bottom]
      [(check-constant-is? x char?)
       'multiplet]
      [(or (check-constant-is? x $immediate?)
           (pred-singleton? x))
       'singleton]
      [(or (check-constant-is? x exact-integer?)
           (memq x '(fixnum bignum exact-integer)))
       'exact-integer]
      [(or (check-constant-is? x number?)
           (check-constant-is? x symbol?)
           (pred-multiplet? x))
       'multiplet]
      [(or (eq? x '$record)
           (pred-$record/rtd? x)
           (pred-$record/ref? x))
       '$record]
      [else
       'normalptr]))

  (define (only-one s m n e r)
    (let loop ([l (list s m n e r)]
               [one 'bottom])
      (cond
        [(null? l)
         one]
        [(eq? (car l) 'bottom)
         (loop (cdr l) one)]
        [(eq? one 'bottom)
         (loop (cdr l) (car l))]
        [else
         #f])))

  (define build-pred-or
    (case-lambda
      [(s m n e r)
       (build-pred-or s m n e r #f #f)]
      [(s m n e r x)
       (build-pred-or s m n e r x #f)]
      [(s m n e r x y)
       (cond
         [(and x
               (eq? (pred-or-sin x) s)
               (eq? (pred-or-mul x) m)
               (eq? (pred-or-nor x) n)
               (eq? (pred-or-exi x) e)
               (eq? (pred-or-rec x) r))
          x]
         [(and y
               (eq? (pred-or-sin y) s)
               (eq? (pred-or-mul y) m)
               (eq? (pred-or-nor y) n)
               (eq? (pred-or-exi y) e)
               (eq? (pred-or-rec y) r))
          y]
         [(only-one s m n e r)
          => (lambda (x) x)]
         [else
          (make-pred-or s m n e r)])]))
  
  ;If x and y are equivalent, they result must be eq? to y
  ;so it's easy to test in predicate-implies?.
  ;The result may be bigger than the actual union. 
  (define (predicate-union x y)
    (cond
      [(or (not x) (not y)) #f]
      [(eq? x 'bottom) y]
      [(eq? y 'bottom) x]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define s (predicate-union/singleton (pred-or-sin x) (pred-or-sin y)))
         (define m (predicate-union/multiplet (pred-or-mul x) (pred-or-mul y)))
         (define n (predicate-union/normal (pred-or-nor x) (pred-or-nor y)))
         (define e (predicate-union/exact-integer (pred-or-exi x) (pred-or-exi y)))
         (define r (predicate-union/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or s m n e r y x))]
      [(pred-or? x)
       (case (predicate->class y)
         [(singleton)
          (build-pred-or (predicate-union/singleton (pred-or-sin x) y)
                         (pred-or-mul x)
                         (pred-or-nor x)
                         (pred-or-exi x)
                         (pred-or-rec x)
                         x)]
         [(multiplet)
          (build-pred-or (pred-or-sin x)
                         (predicate-union/multiplet (pred-or-mul x) y)
                         (pred-or-nor x)
                         (pred-or-exi x)
                         (pred-or-rec x)
                         x)]
         [(normalptr)
          (build-pred-or (pred-or-sin x)
                         (pred-or-mul x)
                         (predicate-union/normal (pred-or-nor x) y)
                         (pred-or-exi x)
                         (pred-or-rec x)
                         x)]
         [(exact-integer)
          (build-pred-or (pred-or-sin x)
                         (pred-or-mul x)
                         (pred-or-nor x)
                         (predicate-union/exact-integer (pred-or-exi x) y)
                         (pred-or-rec x)
                         x)]
         [($record)
          (build-pred-or (pred-or-sin x)
                         (pred-or-mul x)
                         (pred-or-nor x)
                         (pred-or-exi x)
                         (predicate-union/record (pred-or-rec x) y)
                         x)])]
      [(pred-or? y)
       (case (predicate->class x)
         [(singleton)
          (build-pred-or (predicate-union/singleton x (pred-or-sin y))
                         (pred-or-mul y)
                         (pred-or-nor y)
                         (pred-or-exi y)
                         (pred-or-rec y)
                         y)]
         [(multiplet)
          (build-pred-or (pred-or-sin y)
                         (predicate-union/multiplet x (pred-or-mul y))
                         (pred-or-nor y)
                         (pred-or-exi y)
                         (pred-or-rec y)
                         y)]
         [(normalptr)
          (build-pred-or (pred-or-sin y)
                         (pred-or-mul y)
                         (predicate-union/normal x (pred-or-nor y))
                         (pred-or-exi y)
                         (pred-or-rec y)
                         y)]
         [(exact-integer)
          (build-pred-or (pred-or-sin y)
                         (pred-or-mul y)
                         (pred-or-nor y)
                         (predicate-union/exact-integer x (pred-or-exi y))
                         (pred-or-rec y)
                         y)]
         [($record)
          (build-pred-or (pred-or-sin y)
                         (pred-or-mul y)
                         (pred-or-nor y)
                         (pred-or-exi y)
                         (predicate-union/record x (pred-or-rec y))
                         y)])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(eq? cx cy)
            (case cx
              [(singleton)
               (predicate-union/singleton x y)]
              [(multiplet)
               (predicate-union/multiplet x y)]
              [(normalptr)
               (predicate-union/normal x y)]
              [(exact-integer)
               (predicate-union/exact-integer x y)]
              [($record)
               (predicate-union/record x y)])]
           [else
            (let ()
              (define i (cond
                          [(eq? cx 'singleton) x]
                          [(eq? cy 'singleton) y]
                          [else 'bottom]))
              (define u (cond
                          [(eq? cx 'multiplet) x]
                          [(eq? cy 'multiplet) y]
                          [else 'bottom]))
              (define n (cond
                          [(eq? cx 'normalptr) x]
                          [(eq? cy 'normalptr) y]
                          [else 'bottom]))
              (define e (cond
                          [(eq? cx 'exact-integer) x]
                          [(eq? cy 'exact-integer) y]
                          [else 'bottom]))
              (define r (cond
                          [(eq? cx '$record) x]
                          [(eq? cy '$record) y]
                          [else 'bottom]))
              (build-pred-or i u n e r))]))]))

  ;The result may be bigger than the actual intersection 
  ;if there is no exact result, it must be at least included in x
  ;so it's possible to make decreasing sequences.
  ;Anyway, for now the result is exact.
  (define (predicate-intersect x y)
    (cond
      [(not x) y]
      [(not y) x]
      [(or (eq? x 'bottom)
           (eq? y 'bottom))
       'bottom]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define s (predicate-intersect/singleton (pred-or-sin x) (pred-or-sin y)))
         (define m (predicate-intersect/multiplet (pred-or-mul x) (pred-or-mul y)))
         (define n (predicate-intersect/normal (pred-or-nor x) (pred-or-nor y)))
         (define e (predicate-intersect/exact-integer (pred-or-exi x) (pred-or-exi y)))
         (define r (predicate-intersect/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or s m n e r x y))]
      [(pred-or? x)
       (case (predicate->class y)
         [(singleton)
          (predicate-intersect/singleton (pred-or-sin x) y)]
         [(multiplet)
          (predicate-intersect/multiplet (pred-or-mul x) y)]
         [(normalptr)
          (predicate-intersect/normal (pred-or-nor x) y)]
         [(exact-integer)
          (predicate-intersect/exact-integer (pred-or-exi x) y)]
         [($record)
          (predicate-intersect/record (pred-or-rec x) y)])]
      [(pred-or? y)
       (case (predicate->class x)
         [(singleton)
          (predicate-intersect/singleton x (pred-or-sin y))]
         [(multiplet)
          (predicate-intersect/multiplet x (pred-or-mul y))]
         [(normalptr)
          (predicate-intersect/normal x (pred-or-nor y))]
         [(exact-integer)
          (predicate-intersect/exact-integer x (pred-or-exi y))]
         [($record)
          (predicate-intersect/record x (pred-or-rec y))])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(not (eq? cx cy))
            'bottom]
           [else
            (case cx
              [(singleton)
               (predicate-intersect/singleton x y)]
              [(multiplet)
               (predicate-intersect/multiplet x y)]
              [(normalptr)
               (predicate-intersect/normal x y)]
              [(exact-integer)
               (predicate-intersect/exact-integer x y)]
              [($record)
               (predicate-intersect/record x y)])]))]))

  ;The result may be bigger than the actual intersection 
  ;if there is no exact result.
  ;Anyway, it must be included in x, 
  ;and in many cases, for now the result is just x.
  (define (predicate-substract x y)
    (cond
      [(not x) x]
      [(not y) x]
      [(eq? x 'bottom)
       'bottom]
      [(eq? y 'bottom)
       x]
      [(and (pred-or? x)
            (pred-or? y))
       (let ()
         (define s (predicate-substract/singleton (pred-or-sin x) (pred-or-sin y)))
         (define m (predicate-substract/multiplet (pred-or-mul x) (pred-or-mul y)))
         (define n (predicate-substract/normal (pred-or-nor x) (pred-or-nor y)))
         (define e (predicate-substract/exact-integer (pred-or-exi x) (pred-or-exi y)))
         (define r (predicate-substract/record (pred-or-rec x) (pred-or-rec y)))
         (build-pred-or s m n e r x))]
      [(pred-or? x)
       (let ([s (pred-or-sin x)]
             [m (pred-or-mul x)]
             [n (pred-or-nor x)]
             [e (pred-or-exi x)]
             [r (pred-or-rec x)])
         (case (predicate->class y)
           [(singleton)
            (build-pred-or (predicate-substract/singleton s y) m n e r x)]
           [(multiplet)
            (build-pred-or s (predicate-substract/multiplet m y) n e r x)]
           [(normalptr)
            (build-pred-or s m (predicate-substract/normal n y) e r x)]
           [(exact-integer)
            (build-pred-or s m n (predicate-substract/exact-integer e y) r x)]
           [($record)
            (build-pred-or s m n e (predicate-substract/record r y) x)]))]
      [(pred-or? y)
       (case (predicate->class x)
         [(singleton)
          (predicate-substract/singleton x (pred-or-sin y))]
         [(multiplet)
          (predicate-substract/multiplet x (pred-or-mul y))]
         [(normalptr)
          (predicate-substract/normal x (pred-or-nor y))]
         [(exact-integer)
          (predicate-substract/exact-integer x (pred-or-exi y))]
         [($record)
          (predicate-substract/record x (pred-or-rec y))])]
      [else
       (let ()
         (define cx (predicate->class x))
         (define cy (predicate->class y))
         (cond
           [(not (eq? cx cy))
            x]
           [else
            (case cx
              [(singleton)
               (predicate-substract/singleton x y)]
              [(multiplet)
               (predicate-substract/multiplet x y)]
              [(normalptr)
               (predicate-substract/normal x y)]
              [(exact-integer)
               (predicate-substract/exact-integer x y)]
              [($record)
               (predicate-substract/record x y)])]))]))
)