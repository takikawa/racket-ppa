#lang scheme/base

(require (rename-in "../utils/utils.rkt" [infer infer-in]))
(require (rename-in (types subtype convenience remove-intersect union)                   
                    [-> -->]
                    [->* -->*]
                    [one-of/c -one-of/c])
         (infer-in infer)
         (rep type-rep object-rep)
         (utils tc-utils)
         (types resolve)
         (only-in (env type-environments lexical-env) env? update-type/lexical env-map env-props replace-props)
         scheme/contract scheme/match
         mzlib/trace unstable/debug unstable/struct
         (typecheck tc-metafunctions)
         (for-syntax scheme/base))

(define (replace-nth l i f)
  (cond [(null? l) (error 'replace-nth "list not long enough" l i f)]
        [(zero? i) (cons (f (car l)) (cdr l))]
        [else (cons (car l) (replace-nth (cdr l) (sub1 i) f))]))

;(trace replace-nth)

(define/contract (update t lo)
  (Type/c Filter/c . -> . Type/c)
  (match* ((resolve t) lo)
    ;; pair ops
    [((Pair: t s) (TypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (-filter u x rst)) s)]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CarPE:)) x))
     (make-Pair (update t (-not-filter u x rst)) s)]
    [((Pair: t s) (TypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (-filter u x rst)))]
    [((Pair: t s) (NotTypeFilter: u (list rst ... (CdrPE:)) x))
     (make-Pair t (update s (-not-filter u x rst)))]
    
    ;; syntax ops
    [((Syntax: t) (TypeFilter: u (list rst ... (SyntaxPE:)) x))
     (make-Syntax (update t (-filter u x rst)))]
    [((Syntax: t) (NotTypeFilter: u (list rst ... (SyntaxPE:)) x))
     (make-Syntax (update t (-not-filter u x rst)))]
    
    ;; struct ops
    [((Struct: nm par flds proc poly pred cert acc-ids maker-id) 
      (TypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))     
     (make-Struct nm par 
                  (replace-nth flds idx 
                               (lambda (e) (update e (-filter u x rst))))
                  proc poly pred cert acc-ids maker-id)]
    [((Struct: nm par flds proc poly pred cert acc-ids maker-id) 
      (NotTypeFilter: u (list rst ... (StructPE: (? (lambda (s) (subtype t s)) s) idx)) x))
     (make-Struct nm par (replace-nth flds idx (lambda (e) (update e (-not-filter u x rst)))) proc poly pred cert acc-ids maker-id)]
    
    ;; otherwise
    [(t (TypeFilter: u (list) _))
     (restrict t u)]
    [(t (NotTypeFilter: u (list) _))
     (remove t u)]
    [((Union: ts) lo)
     (apply Un (map (lambda (t) (update t lo)) ts))]
    [(t* lo)
     #;
     (int-err "update along ill-typed path: ~a ~a ~a" t t* lo)
     t]))

;; sets the flag box to #f if anything becomes (U)
(d/c (env+ env fs flag)
  (env? (listof Filter/c) (box/c #t). -> . env?)
  (define-values (props atoms) (combine-props fs (env-props env) flag))
  (for/fold ([Γ (replace-props env (append atoms props))]) ([f atoms])
    (match f
      [(Bot:) (set-box! flag #f) (env-map (lambda (x) (cons (car x) (Un))) Γ)]
      [(or (TypeFilter: _ _ x) (NotTypeFilter: _ _ x))
       (update-type/lexical (lambda (x t) (let ([new-t (update t f)])
                                            (when (type-equal? new-t (Un))
                                              (set-box! flag #f))
                                            new-t))
                            x Γ)]
      [_ Γ])))

(p/c [env+ (env? (listof Filter/c) (box/c #t). -> . env?)])