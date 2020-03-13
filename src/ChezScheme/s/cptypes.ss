"cptypes.ss"
;;; cptypes.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
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

#|
Notes:
 - (cptypes ir ctxt types) -> (values ir ret types t-types f-types)
   + arguments
     ir: expression to be optimized
     ctxt: 'effect 'test 'value
     types: an immutable dictionary (currently an intmap).
            The dictionary connects the counter of a prelex with the types
            discovered previously.
            (fxmap ([prelex-counter x] . 'pair)
                   ([prelex-counter y] . 'vector)
                   ([prelex-counter z] . `(quote 0)))
   + results
     ir: the optimized expression
     ret: type of the result of the expression
     types: like the types in the argument, with the addition of the types
            discover during the optimization of the expression
     t-types: types to be used in case the expression is not #f, to be used in
              the "then" branch of an if.
              This is usually only filled in a text context.
              It may be #f, and in this case the `if` clause will use the value
              of types as a replacement.
              (Also the clauses for `let[rec/*]` handle the #f case specialy.)
     f-types: idem for the "else" branch. (if x (something) <here x is #f>)


 - predicate: They may be:
              * a symbol to indicate the type, like 'vector 'pair 'number
                (there are a few fake values, in particular 'bottom is used to
                 signal that there is an error)
              * a nanopass-quoted value that is okay-to-copy?, like
                `(quote 0) `(quote 5) `(quote #t) `(quote '())
                (this doesn't includes `(quote <record-type-descriptor>))
              * a record #[pred-$record/rtd <rtd>] to signal that it's a
                record of type <rtd>
              * a record #[pred-$record/ref <ref>] to signal that it's a
                record of a type that is stored in the variable <ref>
                (these may collide with other records)
              * TODO?: add something to indicate that x is a procedure to
                       create/setter/getter/predicate of a record of that type

 - Primitives are marked as procedures, without distinction.
 - Most of the time I'm using eq? and eqv? as if they were equivalent.
   I assume that the differences are hidden by unspecified behavior.

|#


(define $cptypes)
(let ()
  (import (nanopass))
  (include "base-lang.ss")
  (include "fxmap.ss")

  (define-pass cptypes : Lsrc (ir) -> Lsrc ()
    (definitions
  (define prelex-counter
    (let ()
      (define count 0)
      (lambda (x)
        (or (prelex-operand x)
            (let ([c count])
              (set! count (fx+ count 1))
              (prelex-operand-set! x c)
              c)))))

  (with-output-language (Lsrc Expr)
    (define void-rec `(quote ,(void)))
    (define true-rec `(quote #t))
    (define false-rec `(quote #f))
    (define null-rec `(quote ()))
    (define empty-vector-rec `(quote #()))
    (define empty-string-rec `(quote ""))
    (define empty-bytevector-rec `(quote #vu8()))
    (define empty-fxvector-rec `(quote #vfx()))
    (define eof-rec `(quote #!eof))
    (define bwp-rec `(quote #!bwp))

    (define (simple? e) ; Simplified version copied from cp0. TODO: copy the rest.
      (nanopass-case (Lsrc Expr) e
        [(quote ,d) #t]
        [(ref ,maybe-src ,x) #t]
        [(case-lambda ,preinfo ,cl* ...) #t]
        [,pr #t]
        [(moi) #t]
        [(record-type ,rtd ,e) (simple? e)]
        [else #f]
        #;[else ($oops who "unrecognized record ~s" e)]))

    ; TODO: Remove discardable operations in e1. (vector (f) (g)) => (begin (f) (g))
    (define make-seq
      ; ensures that the right subtree of the output seq is not a seq if the
      ; second argument is similarly constrained, to facilitate result-exp
      (lambda (ctxt e1 e2)
        (if (simple? e1)
            e2
            (if (and (eq? ctxt 'effect) (simple? e2))
                e1
                (let ([e1 (nanopass-case (Lsrc Expr) e1
                            [(seq ,e11 ,e12)
                             (guard (simple? e12))
                             e11]
                            [else e1])])
                  (nanopass-case (Lsrc Expr) e2
                    [(seq ,e21 ,e22) `(seq (seq ,e1 ,e21) ,e22)]
                    [else `(seq ,e1 ,e2)]))))))

    #;(define make-seq* ; requires at least one operand
        (lambda (ctxt e*)
          (if (null? (cdr e*))
              (car e*)
              (make-seq ctxt (car e*) (make-seq* ctxt (cdr e*))))))
  )

  (define-record-type pred-$record/rtd
    (fields rtd)
    (nongenerative #{pred-$record/rtd wnquzwrp8wl515lhz2url8sjc-0})
    (sealed #t))

  (define-record-type pred-$record/ref
    (fields ref)
    (nongenerative #{pred-$record/ref zc0e8e4cs8scbwhdj7qpad6k3-0})
    (sealed #t))

  (module (pred-env-empty
           pred-env-add pred-env-remove/base pred-env-lookup
           pred-env-intersect/base pred-env-union/super-base
           pred-env-rebase
           pred-intersect pred-union)
    (import fxmap)

    (define pred-env-empty empty-fxmap)

    (define (pred-env-add/key types key pred)
      (cond
        [(and pred
              (not (eq? pred 'ptr))) ; filter 'ptr to reduce the size
         (let ([old (fxmap-ref types key #f)])
           (cond
             [(not old)
              (fxmap-set types key pred)]
             [else (let ([new (pred-intersect old pred)])
                     (if (eq? old new)
                         types
                        (fxmap-set types key new)))]))]
        [else
         types]))

    (define (pred-env-add types x pred)
      (cond
        [(and x (not (prelex-assigned x)))
         (pred-env-add/key types (prelex-counter x) pred)]
        [else types]))

    (define (pred-env-remove/base types x base)
      (fxmap-remove/base types (prelex-counter x) base))

    (define (pred-env-lookup types x)
      (and (not (prelex-assigned x))
           (fxmap-ref types (prelex-counter x) #f)))

    ; This is conceptually the intersection of the types in `types` and `from`
    ; but since 'ptr is not stored to save space and time, the implementation
    ; looks like an union of the fxmaps.
    ; [missing 'ptr] _and_ 'vector -> 'vector
    ; 'box _and_ 'vector -> 'bottom
    ; 'number _and_ 'exact-integer -> 'exact-integer
    (define (pred-env-intersect/base types from base)
      (cond
        [(fx> (fxmap-changes from) (fxmap-changes types))
         (pred-env-intersect/base from types base)]
        [else
        (let ([ret types])
          (fxmap-for-each/diff (lambda (key x y)
                                 (let ([z (fxmap-ref types key #f)])
                                   ;x-> from
                                   ;y-> base
                                   ;z-> types
                                   (set! ret (pred-env-add/key ret key (pred-intersect x z)))))
                               (lambda (key x)
                                 (set! ret (pred-env-add/key ret key x)))
                               (lambda (key x)
                                 ($impoops 'pred-env-intersect/base "unexpected value ~s in base environment ~s" x base))
                               from
                               base)
           ret)]))

    (define (pred-intersect x y)
      (cond
        [(predicate-implies? x y) x]
        [(predicate-implies? y x) y]
        [(or (predicate-implies-not? x y)
             (predicate-implies-not? y x))
         'bottom]
        [(or (and (eq? x 'boolean) (eq? y 'true))
             (and (eq? y 'boolean) (eq? x 'true)))
         true-rec]
        [else (or x y)])) ; if there is no exact option, at least keep the old value

    ; This is conceptually the union of the types in `types` and `from`
    ; but since 'ptr is not stored to save space and time, the implementation
    ; looks like an intersection of the fxmaps.
    ; [missing 'ptr] _or_ 'vector -> [missing 'ptr]
    ; 'box _or_ 'boolean -> [missing 'ptr]
    ; 'number _or_ 'exact-integer -> 'number
    (define (pred-env-union/from from base types new-base)
      ; Calculate the union of types and from, and intersect it with new-base
      ; Iterate over the difference of from and base.
      (let ([ret new-base])
        (fxmap-for-each/diff (lambda (key x y)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> from
                                 ;y-> base
                                 ;z-> types
                                 (set! ret (pred-env-add/key ret key (pred-union x z)))))
                             (lambda (key x)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> from
                                 ;z-> types
                                 (set! ret (pred-env-add/key ret key (pred-union x z)))))
                             (lambda (key x)
                               ($impoops 'pred-env-union/from "unexpected value ~s in base environment ~s" x base))
                             from
                             base)
          ret))

    (define (pred-env-union/super-base types types/b
                                       from from/b
                                       base
                                       new-base)
      ; Calculate the union of types and from, and intersect it with new-base
      ; Use the intermediate bases to minimize the amount of operations
      ; required. In particular, base should be the base of types/b and from/b.
      (let ([size-types (fx- (fxmap-changes types) (fxmap-changes base))]
            [size-from (fx- (fxmap-changes from) (fxmap-changes base))]
            [size-new (fx+ (fx- (fxmap-changes types) (fxmap-changes types/b))
                           (fx- (fxmap-changes from) (fxmap-changes from/b)))])
        (cond
          [(and (fx<= size-types size-from) (fx<= size-types size-new))
           (pred-env-union/from types base from new-base)]
          [(fx<= size-from size-new)
           (pred-env-union/from from base types new-base)]
          [else
           (let ([temp (pred-env-union/from from from/b types new-base)])
             (pred-env-union/from types types/b from temp))])))

    (define (pred-union x y)
      (cond
        [(predicate-implies? y x) x]
        [(predicate-implies? x y) y]
        [(find (lambda (t)
                 (and (predicate-implies? x t)
                      (predicate-implies? y t)))
               '(char null-or-pair $record
                 gensym uninterned-symbol interned-symbol symbol
                 fixnum exact-integer flonum real number
                 boolean true ptr))] ; ensure they are order from more restrictive to less restrictive
        [else #f]))

    (define (pred-env-rebase types base new-base)
      (let ([ret types])
        (fxmap-for-each/diff (lambda (key x y)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> new-base
                                 ;y-> base
                                 ;z-> types
                                 (if (eq? x z)
                                     (set! ret (fxmap-reset/base ret key new-base))
                                     (set! ret (fxmap-advance/base ret key new-base)))))
                             (lambda (key x)
                               (let ([z (fxmap-ref types key #f)])
                                 ;x-> new-base
                                 ;z-> types
                                 (if (eq? x z)
                                     (set! ret (fxmap-reset/base ret key new-base))
                                     (set! ret (fxmap-advance/base ret key new-base)))))
                             (lambda (key x)
                               ($impoops 'pred-env-rebase "unexpected value ~s in base environment ~s" x base))
                             new-base
                             base)
        ret))
 )

  (define (pred-env-add/ref types r pred)
    (nanopass-case (Lsrc Expr) r
      [(ref ,maybe-src ,x)
       (pred-env-add types x pred)]
      [else types]))

  ;copied from cp0.ss
  (define (arity-okay? arity n)
    (or (not arity) ; presumably system routine w/no recorded arity
        (ormap
          (lambda (a)
            (or (fx= n a)
                (and (fx< a 0) (fx>= n (fx- -1 a)))))
          arity)))

  ;copied from cp0.ss
  (define okay-to-copy?
    (lambda (obj)
      ; okay to copy obj if (eq? (faslin (faslout x)) x) => #t or (in the case of numbers and characters)
      ; the value of (eq? x x) is unspecified
      (or (and (symbol? obj) (not (uninterned-symbol? obj)))
          (number? obj)
          (char? obj)
          (boolean? obj)
          (null? obj)
          (eqv? obj "")
          (eqv? obj '#())
          (eqv? obj '#vu8())
          (eqv? obj '#vfx())
          (eq? obj (void))
          (eof-object? obj)
          (bwp-object? obj)
          (eq? obj '#6=#6#)
          ($unbound-object? obj)
          (record-type-descriptor? obj)))) ;removed in datum->predicate

  (define (datum->predicate d ir)
    (cond
      [(#3%$record? d) '$record] ;check first to avoid double representation of rtd
      [(okay-to-copy? d) ir]
      [(and (integer? d) (exact? d)) 'exact-integer]
      [(pair? d) 'pair]
      [(box? d) 'box]
      [(vector? d) 'vector]
      [(string? d) 'string]
      [(bytevector? d) 'bytevector]
      [(fxvector? d) 'fxvector]
      [else #f]))

  (define (rtd->record-predicate rtd)
    (cond
      [(Lsrc? rtd)
       (nanopass-case (Lsrc Expr) rtd
         [(quote ,d)
          (guard (record-type-descriptor? d))
          (make-pred-$record/rtd d)]
         [(ref ,maybe-src ,x)
          (guard (not (prelex-assigned x)))
          (make-pred-$record/ref x)]
         [(record-type ,rtd ,e)
          (rtd->record-predicate e)]
         [else '$record])]
      [else '$record]))

  ; when extend is #f the result is a predicate that recognizes less values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #t and (something x) ==> (#3%something x)
  ; when extend is #t the result is a predicate that recognizes more values
  ; than the one in name. This is useful for reductions like
  ; (pred? x) ==> #f and (something x) ==> <error>
  ; in case the non extended version is not #f, the extended version must be not #f
  (define (primref-name->predicate name extend?)
    (case name
      [pair? 'pair]
      [box? 'box]
      [$record? '$record]
      [fixnum? 'fixnum]
      [flonum? 'flonum]
      [real? 'real]
      [number? 'number]
      [vector? 'vector]
      [string? 'string]
      [bytevector? 'bytevector]
      [fxvector? 'fxvector]
      [gensym? 'gensym]
      [uninterned-symbol? 'uninterned-symbol]
      #;[interned-symbol? 'interned-symbol]
      [symbol? 'symbol]
      [char? 'char]
      [boolean? 'boolean]
      [procedure? 'procedure]
      [not false-rec]
      [null? null-rec]
      [eof-object? eof-rec]
      [bwp-object? bwp-rec]
      [list? (if (not extend?) null-rec 'null-or-pair)]
      [else ((if extend? cdr car)
             (case name
               [(record? record-type-descriptor?) '(bottom . $record)]
               [(integer? rational?) '(exact-integer . real)]
               [(cflonum?) '(flonum . number)]
               [else '(#f . #f)]))])) ; this is used only to detect predicates.

  ; nqm: no question mark
  ; this is almost duplicated code, but with more cases
  ; it's also useful to avoid the allocation
  ; of the temporal strings to transform: vector -> vector?
  (define (primref-name/nqm->predicate name extend?)
    (case name
      [pair 'pair]
      [box 'box]
      [$record '$record]
      [fixnum 'fixnum]
      [flonum 'flonum]
      [real 'real]
      [number 'number]
      [vector 'vector]
      [string 'string]
      [bytevector 'bytevector]
      [fxvector 'fxvector]
      [gensym 'gensym]
      [uninterned-symbol 'uninterned-symbol]
      [interned-symbol 'interned-symbol]
      [symbol 'symbol]
      [char 'char]
      [bottom 'bottom] ;pseudo-predicate
      [ptr 'ptr] ;pseudo-predicate
      [boolean 'boolean]
      [procedure 'procedure]
      [exact-integer 'exact-integer] ;fake-predicate
      [void void-rec] ;fake-predicate
      [null null-rec]
      [eof-object eof-rec]
      [bwp-object bwp-rec]
      [list (if (not extend?) null-rec 'null-or-pair)] ;fake-predicate
      [else ((if extend? cdr car)
             (case name
               [(record rtd) '(bottom . $record)]
               [(bit length ufixnum pfixnum) '(bottom . fixnum)]
               [(uint sub-uint) '(bottom . exact-integer)]
               [(sint) '(fixnum . exact-integer)]
               [(uinteger) '(bottom . real)]
               [(integer rational) '(exact-integer . real)]
               [(cflonum) '(flonum . number)]
               [else '(bottom . ptr)]))])) ; this is used only to analyze the signatures.

  (define (primref->predicate pr extend?)
    (primref-name->predicate (primref-name pr) extend?))

  (define (check-constant-is? x pred?)
    (nanopass-case (Lsrc Expr) x
      [(quote ,d) (pred? d)]
      [else #f]))

  ; strange properties of bottom here:
  ; (implies? x bottom): only for x=bottom
  ; (implies? bottom y): always
  ; (implies-not? x bottom): never
  ; (implies-not? bottom y): never
  ; check (implies? x bottom) before (implies? x something)
  (define (predicate-implies? x y)
    (and x
         y
         (or (eq? x y)
             (eq? x 'bottom)
             (cond
               [(Lsrc? y)
                (and (Lsrc? x)
                     (nanopass-case (Lsrc Expr) y
                       [(quote ,d1)
                        (nanopass-case (Lsrc Expr) x
                          [(quote ,d2) (eqv? d1 d2)]
                          [else #f])]
                       [else #f]))]
               [(pred-$record/rtd? y)
                (and (pred-$record/rtd? x)
                     (let ([x-rtd (pred-$record/rtd-rtd x)]
                           [y-rtd (pred-$record/rtd-rtd y)])
                       (cond
                         [(record-type-sealed? y-rtd)
                          (eqv? x-rtd y-rtd)]
                         [else
                          (let loop ([x-rtd x-rtd])
                            (or (eqv? x-rtd y-rtd)
                                (let ([xp-rtd (record-type-parent x-rtd)])
                                  (and xp-rtd (loop xp-rtd)))))])))]
               [(pred-$record/ref? y)
                (and (pred-$record/ref? x)
                     (eq? (pred-$record/ref-ref x)
                          (pred-$record/ref-ref y)))]
               [(case y
                  [(null-or-pair) (or (eq? x 'pair)
                                      (check-constant-is? x null?))]
                  [(fixnum) (check-constant-is? x target-fixnum?)]
                  [(exact-integer)
                   (or (eq? x 'fixnum)
                       (check-constant-is? x (lambda (x) (and (integer? x)
                                                              (exact? x)))))]
                  [(flonum) (check-constant-is? x flonum?)]
                  [(real) (or (eq? x 'fixnum)
                              (eq? x 'exact-integer)
                              (eq? x 'flonum)
                              (check-constant-is? x real?))]
                  [(number) (or (eq? x 'fixnum)
                                (eq? x 'exact-integer)
                                (eq? x 'flonum)
                                (eq? x 'real)
                                (check-constant-is? x number?))]
                  [(gensym) (check-constant-is? x gensym?)]
                  [(uninterned-symbol) (check-constant-is? x uninterned-symbol?)]
                  [(interned-symbol) (check-constant-is? x (lambda (x)
                                                             (and (symbol? x)
                                                                  (not (gensym? x))
                                                                  (not (uninterned-symbol? x)))))]
                  [(symbol) (or (eq? x 'gensym)
                                (eq? x 'uninterned-symbol)
                                (eq? x 'interned-symbol)
                                (check-constant-is? x symbol?))]
                  [(char) (check-constant-is? x char?)]
                  [(boolean) (check-constant-is? x boolean?)]
                  [(true) (and (not (check-constant-is? x not))
                               (not (eq? x 'boolean))
                               (not (eq? x 'ptr)))] ; only false-rec, boolean and ptr may be `#f
                  [($record) (or (pred-$record/rtd? x)
                                 (pred-$record/ref? x)
                                 (check-constant-is? x #3%$record?))]
                  [(vector) (check-constant-is? x vector?)] ; i.e. '#()
                  [(string) (check-constant-is? x string?)] ; i.e. ""
                  [(bytevector) (check-constant-is? x bytevector?)] ; i.e. '#vu8()
                  [(fxvector) (check-constant-is? x fxvector?)] ; i.e. '#vfx()
                  [(ptr) #t]
                  [else #f])]
               [else #f]))))

  (define (predicate-implies-not? x y)
    (and x
         y
         ; a pred-$record/ref may be any other kind or record
         (not (and (pred-$record/ref? x)
                   (predicate-implies? y '$record)))
         (not (and (pred-$record/ref? y)
                   (predicate-implies? x '$record)))
         ; boolean and true may be a #t
         (not (and (eq? x 'boolean)
                   (eq? y 'true)))
         (not (and (eq? y 'boolean)
                   (eq? x 'true)))
         ; the other types are included or disjoint
         (not (predicate-implies? x y))
         (not (predicate-implies? y x))))

  (define (signature->result-predicate signature)
    (let ([results (cdr signature)])
      (and (fx= (length results) 1)
           (let ([result (car results)])
             (cond
               [(symbol? result)
                (primref-name/nqm->predicate result #t)]
               [(equal? result '(ptr . ptr))
                'pair]
               [(pair? result)
                'pair]
               [else
                'ptr])))))

  (define primref->result-predicate/cache (make-hashtable equal-hash equal?))

  (define (primref->result-predicate pr)
    (let ([key (primref-name pr)])
      (if (hashtable-contains? primref->result-predicate/cache key)
          (hashtable-ref primref->result-predicate/cache key #f)
          (let ([new (primref->result-predicate/no-cache pr)])
            (hashtable-set! primref->result-predicate/cache key new)
            new))))

  (define (primref->result-predicate/no-cache pr)
    (let ([pred/flags
           (let ([flags (primref-flags pr)])
             (cond
               [(all-set? (prim-mask abort-op) flags)
                'bottom]
               [(all-set? (prim-mask true) flags)
                'true]
               [(all-set? (prim-mask boolean-valued) flags)
                'boolean]
               [else
                #f]))]
          [pred/signatures
           (let ([signatures (primref-signatures pr)])
             (and (not (null? signatures))
                  (let ([results (map (lambda (s) (signature->result-predicate s)) signatures)])
                    (fold-left pred-union 'bottom results))))])
      (pred-intersect pred/flags pred/signatures)))

  (define (signature->argument-predicate signature pos extend?)
    (let* ([arguments (car signature)]
           [dots (memq '... arguments)])
      (cond
        [(and dots (null? (cdr dots)))
         (cond
           [(< pos (- (length arguments) 2))
            (primref-name/nqm->predicate (list-ref arguments pos) extend?)]
           [else
            (primref-name/nqm->predicate (list-ref arguments (- (length arguments) 2)) extend?)])]
         [dots #f] ; TODO: Extend to handle this case, perhaps knowing the argument count.
         [else
          (cond
            [(< pos (length arguments))
             (let ([argument (list-ref arguments pos)])
               (cond
                 [(equal? argument '(ptr . ptr))
                  'pair]
                 [(and extend? (pair? argument))
                  'pair]
                 [else
                  (primref-name/nqm->predicate argument extend?)]))]
            [else
             'bottom])])))

  (define primref->argument-predicate/cache (make-hashtable equal-hash equal?))

  (define (primref->argument-predicate pr pos extend?)
    (let ([key (list (primref-name pr) pos extend?)])
      (if (hashtable-contains? primref->argument-predicate/cache key)
          (hashtable-ref primref->argument-predicate/cache key #f)
          (let ([new (primref->argument-predicate/no-cache pr pos extend?)])
            (when (<= pos 10)
              (hashtable-set! primref->argument-predicate/cache key new))
            new))))

  (define (primref->argument-predicate/no-cache pr pos extend?)
    (let ([signatures (primref-signatures pr)])
      (and (>= (length signatures) 1)
           (let ([vals (map (lambda (signature)
                              (signature->argument-predicate signature pos extend?))
                            signatures)])
             (fold-left (if extend? pred-union pred-intersect) (car vals) (cdr vals))))))

  (define (primref->unsafe-primref pr)
    (lookup-primref 3 (primref-name pr)))
)
    (Expr : Expr (ir ctxt types) -> Expr (ret types t-types f-types)
      [(quote ,d)
       (values ir (datum->predicate d ir) types #f #f)]
      [(ref ,maybe-src ,x)
       (case ctxt
         [(test)
          (let ([t (pred-env-lookup types x)])
            (cond
              [(predicate-implies-not? t false-rec)
               (values true-rec true-rec types #f #f)]
              [(predicate-implies? t false-rec)
               (values false-rec false-rec types #f #f)]
              [else
               (values ir t
                      types
                      (pred-env-add/ref types ir 'true) ; don't confuse it with true-rec
                      (pred-env-add/ref types ir false-rec))]))]
         [else
           (let ([t (pred-env-lookup types x)])
            (cond
              [(Lsrc? t)
               (nanopass-case (Lsrc Expr) t
                 [(quote ,d)
                  (values t t types #f #f)]
                 [else
                  (values ir t types #f #f)])]
               [else
                (values ir t types #f #f)]))])]
      [(seq ,[e1 'effect types -> e1 ret1 types t-types f-types] ,e2)
       (cond
         [(predicate-implies? ret1 'bottom)
          (values e1 ret1 types #f #f)]
         [else
          (let-values ([(e2 ret types t-types f-types)
                        (Expr e2 ctxt types)])
            (values (make-seq ctxt e1 e2) ret types t-types f-types))])]
      [(if ,[e1 'test types -> e1 ret1 types1 t-types1 f-types1] ,e2 ,e3)
       (cond
         [(predicate-implies? ret1 'bottom) ;check bottom first
          (values e1 ret1 types #f #f)]
         [(predicate-implies-not? ret1 false-rec)
          (let-values ([(e2 ret types t-types f-types)
                        (Expr e2 ctxt types1)])
            (values (make-seq ctxt e1 e2) ret types t-types f-types))]
         [(predicate-implies? ret1 false-rec)
          (let-values ([(e3 ret types t-types f-types)
                        (Expr e3 ctxt types1)])
            (values (make-seq ctxt e1 e3) ret types t-types f-types))]
         [else
          (let*-values ([(t-types1) (or t-types1 types1)]
                        [(f-types1) (or f-types1 types1)]
                        [(e2 ret2 types2 t-types2 f-types2)
                         (Expr e2 ctxt t-types1)]
                        [(t-types2) (or t-types2 types2)]
                        [(f-types2) (or f-types2 types2)]
                        [(e3 ret3 types3 t-types3 f-types3)
                         (Expr e3 ctxt f-types1)]
                        [(t-types3) (or t-types3 types3)]
                        [(f-types3) (or f-types3 types3)])
            (let ([ir `(if ,e1 ,e2 ,e3)])
              (cond
                [(and (predicate-implies? ret2 'bottom)  ;check bottom first
                      (predicate-implies? ret3 'bottom)) ;check bottom first
                 (values ir ret3 types3 t-types3 f-types3)]
                [(predicate-implies? ret2 'bottom) ;check bottom first
                 (values (make-seq ctxt `(if ,e1 ,e2 ,void-rec) e3)
                         ret3 types3 t-types3 f-types3)]
                [(predicate-implies? ret3 'bottom) ;check bottom first
                 (values (make-seq ctxt `(if ,e1 ,void-rec ,e3) e2)
                         ret2 types2 t-types2 f-types2)]
                [else
                 (let ([new-types (pred-env-union/super-base types2 t-types1
                                                             types3 f-types1
                                                             types1
                                                             types1)])
                   (values ir
                           (cond
                             [(and (eq? ctxt 'test)
                                   (predicate-implies-not? ret2 false-rec)
                                   (predicate-implies-not? ret3 false-rec))
                              true-rec]
                             [else
                              (pred-union ret2 ret3)])
                           new-types
                           (cond
                             [(not (eq? ctxt 'test))
                              #f] ; don't calculate t-types outside a test context
                             [(predicate-implies? ret2 false-rec)
                              (pred-env-rebase t-types3 types1 new-types)]
                             [(predicate-implies? ret3 false-rec)
                              (pred-env-rebase t-types2 types1 new-types)]
                             [(and (eq? types2 t-types2)
                                   (eq? types3 t-types3))
                              #f] ; don't calculate t-types when it will be equal to new-types
                             [else
                              (pred-env-union/super-base t-types2 t-types1
                                                         t-types3 f-types1
                                                         types1
                                                         new-types)])
                           (cond
                             [(not (eq? ctxt 'test))
                              #f] ; don't calculate f-types outside a test context
                             [(predicate-implies-not? ret2 false-rec)
                              (pred-env-rebase f-types3 types1 new-types)]
                             [(predicate-implies-not? ret3 false-rec)
                              (pred-env-rebase f-types2 types1 new-types)]
                             [(and (eq? types2 f-types2)
                                   (eq? types3 f-types3))
                              #f] ; don't calculate t-types when it will be equal to new-types
                             [else
                              (pred-env-union/super-base f-types2 t-types1
                                                         f-types3 f-types1
                                                         types1
                                                         new-types)])))])))])]
      [(set! ,maybe-src ,x ,[e 'value types -> e ret types t-types f-types])
       (values `(set! ,maybe-src ,x ,e) void-rec types #f #f)]
      [(call ,preinfo ,pr ,[e* 'value types -> e* r* t* t-t* f-t*] ...)
       (let* ([t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)]
              [ret (primref->result-predicate pr)])
         (let-values ([(ret t)
                       (let loop ([e* e*] [r* r*] [n 0] [ret ret] [t t])
                         (if (null? e*)
                             (values ret t)
                             (let ([pred (primref->argument-predicate pr n #t)])
                               (loop (cdr e*)
                                     (cdr r*)
                                     (fx+ n 1)
                                     (if (predicate-implies-not? (car r*) pred)
                                         'bottom
                                         ret)
                                     (pred-env-add/ref t (car e*) pred)))))])
           (cond
             [(predicate-implies? ret 'bottom)
              (values `(call ,preinfo ,pr ,e* ...) ret t #f #f)]
             [(not (arity-okay? (primref-arity pr) (length e*)))
              (values `(call ,preinfo ,pr ,e* ...) 'bottom t #f #f)]
             [(and (fx= (length e*) 2)
                   (or (eq? (primref-name pr) 'eq?)
                       (eq? (primref-name pr) 'eqv?)))
                (let ([r1 (car r*)]
                      [r2 (cadr r*)]
                      [e1 (car e*)]
                      [e2 (cadr e*)])
                  (cond
                    [(or (predicate-implies-not? r1 r2)
                         (predicate-implies-not? r2 r1))
                     (values (make-seq ctxt (make-seq 'effect e1 e2) false-rec)
                             false-rec t #f #f)]
                    [else
                     (values `(call ,preinfo ,pr ,e* ...)
                             ret
                             types
                             (and (eq? ctxt 'test)
                                  (pred-env-add/ref
                                   (pred-env-add/ref t e1 r2)
                                   e2 r1))
                             #f)]))]
             [(and (fx= (length e*) 1)
                   (primref->predicate pr #t))
              (let ([var (car r*)]
                    [pred (primref->predicate pr #f)])
                (cond
                    [(predicate-implies? var pred)
                     (values (make-seq ctxt (car e*) true-rec)
                             true-rec t #f #f)]
                    [else
                     (let ([pred (primref->predicate pr #t)])
                       (cond
                         [(predicate-implies-not? var pred)
                          (values (make-seq ctxt (car e*) false-rec)
                                  false-rec t #f #f)]
                         [else
                          (values `(call ,preinfo ,pr ,e* ...)
                                  ret
                                  types
                                  (and (eq? ctxt 'test)
                                       (pred-env-add/ref t (car e*) pred))
                                  #f)]))]))]
             [(and (fx>= (length e*) 1)
                   (eq? (primref-name pr) '$record))
              (values `(call ,preinfo ,pr ,e* ...) (rtd->record-predicate (car e*)) t #f #f)]
             [(and (fx= (length e*) 2)
                   (or (eq? (primref-name pr) 'record?)
                       (eq? (primref-name pr) '$sealed-record?)))
              (let ([pred (rtd->record-predicate (cadr e*))]
                    [var (car r*)])
                (cond
                  [(predicate-implies-not? var pred)
                   (cond
                     [(or (all-set? (prim-mask unsafe) (primref-flags pr))
                          (nanopass-case (Lsrc Expr) (cadr e*) ; ensure that it is actually a rtd
                            [(quote ,d)
                             (record-type-descriptor? d)]
                            [(record-type ,rtd ,e) #t]
                            [else #f]))
                      (values (make-seq ctxt (make-seq 'effect (car e*) (cadr e*)) false-rec)
                              false-rec t #f #f)]
                     [else
                      (values (make-seq ctxt ir false-rec)
                              false-rec t #f #f)])]
                  [(and (not (eq? pred '$record)) ; assume that the only extension is '$record
                        (predicate-implies? var pred))
                   (values (make-seq ctxt (make-seq 'effect (car e*) (cadr e*)) true-rec)
                           true-rec t #f #f)]
                  [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                        (nanopass-case (Lsrc Expr) (cadr e*) ; check that it is a rtd
                          [(quote ,d)
                           (record-type-descriptor? d)]
                          [(record-type ,rtd ,e) #t]
                          [else #f]))
                   (let ([pr (primref->unsafe-primref pr)])
                     (values `(call ,preinfo ,pr ,e* ...)
                             ret types
                             (and (eq? ctxt 'test)
                                  (pred-env-add/ref types (car e*) pred))
                             #f))]
                  [else
                   (values `(call ,preinfo ,pr ,e* ...)
                           ret
                           types
                           (and (eq? ctxt 'test)
                                (pred-env-add/ref types (car e*) pred))
                           #f)]))]
             ; TODO: special case for call-with-values.
             [(eq? (primref-name pr) 'list)
              (cond 
                [(null? e*)
                 ;should have be reduced by cp0
                 (values null-rec null-rec t #f #f)]
                [else
                 (values `(call ,preinfo ,pr ,e* ...) 'pair t #f #f)])]
             [(and (fx= (length e*) 1)
                   (eq? (primref-name pr) 'exact?))
              (cond
                [(predicate-implies? (car r*) 'exact-integer)
                 (values (make-seq ctxt (car e*) true-rec)
                         true-rec t #f #f)]
                [(predicate-implies? (car r*) 'flonum)
                 (values (make-seq ctxt (car e*) false-rec)
                         false-rec t #f #f)]
                [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                      (predicate-implies? (car r*) 'number))
                 (let ([pr (primref->unsafe-primref pr)])
                   (values `(call ,preinfo ,pr ,e* ...)
                           ret t #f #f))]
                [else
                 (values `(call ,preinfo ,pr ,e* ...) ret t #f #f)])]
             [(and (fx= (length e*) 1)
                   (eq? (primref-name pr) 'inexact?))
              (cond
                [(predicate-implies? (car r*) 'exact-integer)
                 (values (make-seq ctxt (car e*) false-rec)
                         false-rec t #f #f)]
                [(predicate-implies? (car r*) 'flonum)
                 (values (make-seq ctxt (car e*) true-rec)
                         true-rec t #f #f)]
                [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                      (predicate-implies? (car r*) 'number))
                 (let ([pr (primref->unsafe-primref pr)])
                   (values `(call ,preinfo ,pr ,e* ...)
                           ret t #f #f))]
                [else
                 (values `(call ,preinfo ,pr ,e* ...) ret t #f #f)])]
             [(and (not (all-set? (prim-mask unsafe) (primref-flags pr)))
                   (all-set? (prim-mask safeongoodargs) (primref-flags pr))
                   (andmap (lambda (r n)
                             (predicate-implies? r
                                                 (primref->argument-predicate pr n #f)))
                           r* (enumerate r*)))
               (let ([pr (primref->unsafe-primref pr)])
                 (values `(call ,preinfo ,pr ,e* ...)
                         ret types #f #f))]
             [else
              (values `(call ,preinfo ,pr ,e* ...) ret t #f #f)])))]
      [(case-lambda ,preinfo ,cl* ...)
       (let ([cl* (map (lambda (cl)
                        (nanopass-case (Lsrc CaseLambdaClause) cl
                          [(clause (,x* ...) ,interface ,body)
                           (let-values ([(body ret types t-types f-types)
                                         (Expr body 'value types)])
                             (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
                             (with-output-language (Lsrc CaseLambdaClause)
                               `(clause (,x* ...) ,interface ,body)))]))
                       cl*)])
         (values `(case-lambda ,preinfo ,cl* ...) 'procedure types #f #f))]
      [(call ,preinfo (case-lambda ,preinfo2 (clause (,x** ...) ,interface* ,body*) ...)
             ,[e* 'value types -> e* r* t* t-t* f-t*] ...)
       ;; pulled from cpnanopass
       (define find-matching-clause
         (lambda (len x** interface* body* kfixed kvariable kfail)
           (let f ([x** x**] [interface* interface*] [body* body*])
             (if (null? interface*)
                 (kfail)
                 (let ([interface (car interface*)])
                   (if (fx< interface 0)
                       (let ([nfixed (fxlognot interface)])
                         (if (fx>= len nfixed)
                             (kvariable nfixed (car x**) (car body*))
                             (f (cdr x**) (cdr interface*) (cdr body*))))
                       (if (fx= interface len)
                           (kfixed (car x**) (car body*))
                           (f (cdr x**) (cdr interface*) (cdr body*)))))))))
       (define finish
         (lambda (x* interface body t)
           (let-values ([(body ret n-types t-types f-types)
                         (Expr body ctxt t)])
             (let* ([new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                    [t-types (and (eq? ctxt 'test)
                                  t-types
                                  (not (eq? n-types t-types))
                                  (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                    [f-types (and (eq? ctxt 'test)
                                  f-types
                                  (not (eq? n-types f-types))
                                  (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
               (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
               (values
                 `(call ,preinfo (case-lambda ,preinfo2 (clause (,x* ...) ,interface ,body)) ,e* ...)
                 ret new-types t-types f-types)))))
       (let ([t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)]
             [len (length e*)])
         (find-matching-clause (length e*) x** interface* body*
           (lambda (x* body) (finish x* len body (fold-left pred-env-add t x* r*)))
           (lambda (nfixed x* body)
             (finish x* (fxlognot nfixed) body
               (fold-left pred-env-add t x*
                 (let f ([i nfixed] [r* r*])
                   (if (fx= i 0)
                       (list (if (null? r*) null-rec 'pair))
                       (cons (car r*) (f (fx- i 1) (cdr r*))))))))
           (lambda () (values ir 'bottom types #f #f))))]
      [(call ,preinfo ,[e0 'value types -> e0 ret0 types0 t-types0 f-types0]
             ,[e* 'value types -> e* r* t* t-t* f-t*]  ...)
       (values `(call ,preinfo ,e0 ,e* ...)
               #f
               (pred-env-add/ref
                 (pred-env-intersect/base
                   (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)
                   types0 types)
                 e0 'procedure)
               #f #f)]
      [(letrec ((,x* ,[e* 'value types -> e* r* t* t-t* t-f*]) ...) ,body)
       (let* ([t (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)]
              [t (fold-left pred-env-add t x* r*)])
        (let-values ([(body ret n-types t-types f-types)
                      (Expr body ctxt t)])
          (let* ([new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                 [t-types (and (eq? ctxt 'test)
                               t-types
                               (not (eq? n-types t-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                 [f-types (and (eq? ctxt 'test)
                               f-types
                               (not (eq? n-types f-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
            (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
            (values `(letrec ([,x* ,e*] ...) ,body)
                    ret new-types t-types f-types))))]
      [(letrec* ((,x* ,e*) ...) ,body)
       (let*-values ([(e* types)
                      (let loop ([x* x*] [e* e*] [types types] [rev-e* '()]) ; this is similar to an ordered-map
                        (if (null? x*)
                          (values (reverse rev-e*) types)
                          (let-values ([(e ret types t-types f-types)
                                        (Expr (car e*) 'value types)])
                            (let ([types (pred-env-add types (car x*) ret)])
                              (loop (cdr x*) (cdr e*) types (cons e rev-e*))))))])
        (let-values ([(body ret n-types t-types f-types)
                      (Expr body ctxt types)])
          (let* ([new-types (fold-left (lambda (f x) (pred-env-remove/base f x types)) n-types x*)]
                 [t-types (and (eq? ctxt 'test)
                               t-types
                               (not (eq? n-types t-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) t-types x*))]
                 [f-types (and (eq? ctxt 'test)
                               f-types
                               (not (eq? n-types f-types))
                               (fold-left (lambda (f x) (pred-env-remove/base f x new-types)) f-types x*))])
            (for-each (lambda (x) (prelex-operand-set! x #f)) x*)
            (values `(letrec* ([,x* ,e*] ...) ,body)
                    ret new-types t-types f-types))))]
      [,pr
       (values ir
               (and (all-set? (prim-mask proc) (primref-flags pr)) 'procedure)
               types #f #f)]
      [(foreign (,conv* ...) ,name ,[e 'value types -> e ret types t-types f-types] (,arg-type* ...) ,result-type)
       (values `(foreign (,conv* ...) ,name ,e (,arg-type* ...) ,result-type)
               #f types #f #f)]
      [(fcallable (,conv* ...) ,[e 'value types -> e ret types t-types f-types] (,arg-type* ...) ,result-type)
       (values `(fcallable (,conv* ...) ,e (,arg-type* ...) ,result-type)
               #f types #f #f)]
      [(record ,rtd ,[rtd-expr 'value types -> rtd-expr ret-re types-re t-types-re f-types-re]
               ,[e* 'value types -> e* r* t* t-t* f-t*] ...)
       (values `(record ,rtd ,rtd-expr ,e* ...)
               (rtd->record-predicate rtd-expr)
               (fold-left (lambda (f x) (pred-env-intersect/base f x types)) types t*)
               #f #f)]
      [(record-ref ,rtd ,type ,index ,[e 'value types -> e ret types t-types f-types])
       (values `(record-ref ,rtd ,type ,index ,e)
               #f
               (pred-env-add/ref types e '$record)
               #f #f)]
      [(record-set! ,rtd ,type ,index ,[e1 'value types -> e1 ret1 types1 t-types1 f-types1]
                    ,[e2 'value types -> e2 ret2 types2 t-types2 f-types2])
       (values `(record-set! ,rtd ,type ,index ,e1 ,e2)
               void-rec
               (pred-env-add/ref (pred-env-intersect/base types1 types2 types)
                                 e1 '$record)
               #f #f)]
      [(record-type ,rtd ,[e 'value types -> e ret types t-types f-types])
       (values `(record-type ,rtd ,e)
               #f types #f #f)]
      [(record-cd ,rcd ,rtd-expr ,[e 'value types -> e ret types t-types f-types])
       (values `(record-cd ,rcd ,rtd-expr ,e)
               #f types #f #f)]
      [(immutable-list (,[e* 'value types -> e* r* t* t-t* f-t*] ...)
                       ,[e 'value types -> e ret types t-types f-types])
       (values `(immutable-list (,e*  ...) ,e)
               ret types #f #f)]
      [(moi) (values ir #f types #f #f)]
      [(pariah) (values ir void-rec types #f #f)]
      [(cte-optimization-loc ,box ,[e 'value types -> e ret types t-types f-types])
       (values `(cte-optimization-loc ,box ,e)
               ret types #f #f)]
      [(cpvalid-defer ,e) (sorry! who "cpvalid leaked a cpvalid-defer form ~s" ir)]
      [(profile ,src) (values ir #f types #f #f)]
      [else ($oops who "unrecognized record ~s" ir)])
    (let-values ([(ir ret types t-types f-types)
                  (Expr ir 'value pred-env-empty)])
      ir))

  (set! $cptypes cptypes)

)
