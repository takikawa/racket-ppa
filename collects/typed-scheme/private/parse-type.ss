#lang scheme/base

(provide parse-type parse-type/id)

(require (except-in "type-rep.ss" make-arr)
         "type-effect-convenience.ss"
         (only-in "type-effect-convenience.ss" [make-arr* make-arr])
         "tc-utils.ss"
         "union.ss"
         syntax/stx
         (except-in "type-environments.ss")
         "type-name-env.ss"
         "type-alias-env.ss"
         "type-utils.ss"
         scheme/match)

(define enable-mu-parsing (make-parameter #t))


(define (parse-type/id loc datum)
  #;(printf "parse-type/id id : ~a~n ty: ~a~n" (syntax-object->datum loc) (syntax-object->datum stx))
  (let* ([stx* (datum->syntax loc datum loc loc)])
    (parse-type stx*)))

(define (stx-cadr stx) (stx-car (stx-cdr stx)))


(define (parse-type stx)    
  (parameterize ([current-orig-stx stx])
    (syntax-case* stx ()
      symbolic-identifier=?             
      [(fst . rst)
       (not (syntax->list #'rst))
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(Class (pos-args ...) ([fname fty . rest] ...) ([mname mty] ...))
       (eq? (syntax-e #'Class) 'Class)
       (make-Class
        (map parse-type (syntax->list #'(pos-args ...)))
        (map list
             (map syntax-e (syntax->list #'(fname ...)))
             (map parse-type (syntax->list #'(fty ...)))
             (map (lambda (e) (syntax-case e ()
                                [(#t) #t]
                                [_ #f]))
                  (syntax->list #'(rest ...))))
        (map list
             (map syntax-e (syntax->list #'(mname ...)))
             (map parse-type (syntax->list #'(mty ...)))))]
      [(Instance t)
       (eq? (syntax-e #'Instance) 'Instance)
       (let ([v (parse-type #'t)])
         (if (or (Mu? v) (Class? v) (Union? v))
             (begin (tc-error/delayed "Argument to Instance must be a class type, got ~a" v)
                    (make-Instance (Un)))
             (make-Instance v)))]
      [(Tuple ts ...)
       (or (eq? (syntax-e #'Tuple) 'Tuple)
           (eq? (syntax-e #'Tuple) 'List))
       (begin
         (add-type-name-reference (stx-car stx))
         (-Tuple (map parse-type (syntax->list #'(ts ...)))))]
      [(cons fst rst)
       (eq? (syntax-e #'cons) 'cons)
       (-pair (parse-type #'fst) (parse-type #'rst))]
      [(pred t) 
       (eq? (syntax-e #'pred) 'pred)
       (make-pred-ty (parse-type #'t))]
      [(dom -> rng : pred-ty)
       (and 
        (eq? (syntax-e #'->) '->)
        (eq? (syntax-e #':) ':))
       (begin
         (add-type-name-reference (stx-cadr stx))
         (make-pred-ty (list (parse-type #'dom)) (parse-type #'rng) (parse-type #'pred-ty)))]
      [(dom ... rest ::: -> rng)
       (and (eq? (syntax-e #'->) '->) 
            (or (symbolic-identifier=? #'::: (quote-syntax ..))
                (symbolic-identifier=? #'::: (quote-syntax ...))))
       (begin
         (add-type-name-reference #'->)
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-type #'rest) (parse-type #'rng)))]
      ;; has to be below the previous one
      [(dom ... -> rng) 
       (eq? (syntax-e #'->) '->)
       (begin
         (add-type-name-reference #'->)
         (->* (map parse-type (syntax->list #'(dom ...))) (parse-type #'rng)))]
      [(values tys ...) 
       (eq? (syntax-e #'values) 'values)
       (-values (map parse-type (syntax->list #'(tys ...))))]
      [(case-lambda tys ...) 
       (eq? (syntax-e #'case-lambda) 'case-lambda)
       (make-Function (map (lambda (ty) 
                             (syntax-case* ty (->) symbolic-identifier=?
                               [(dom ... -> rng)
                                (make-arr 
                                 (map parse-type (syntax->list #'(dom ...))) 
                                 (parse-type #'rng))]))
                           (syntax->list #'(tys ...))))]
      ;; I wish I could write this
      #;[(case-lambda ([dom ... -> rng] ...)) (make-funty (list (make-arr (list (parse-type #'dom) ...) (parse-type #'rng)) ...))]
      #;[(list-of t) (make-lst (parse-type #'t))]
      #;[(Listof t) (make-lst (parse-type #'t))]
      [(Vectorof t) 
       (eq? (syntax-e #'Vectorof) 'Vectorof)
       (begin
         (add-type-name-reference #'Vectorof)
         (make-Vector (parse-type #'t)))]
      [(mu x t) 
       (and (identifier? #'x)
            (memq (syntax-e #'mu) '(mu Rec))
            (enable-mu-parsing))
       (let* ([var (syntax-e #'x)]
              [tvar (make-F var)])
         (add-type-name-reference #'mu)           
         (parameterize ([current-tvars (extend-env (list var) (list tvar) (current-tvars))])
           (let ([t (parse-type #'t)])
             (if (memq var (fv t))
                 (make-Mu var t)
                 t))))]
      [(U ts ...)
       (eq? (syntax-e #'U) 'U)
       (begin
         (add-type-name-reference #'U)
         (apply Un (map parse-type (syntax->list #'(ts ...)))))]
      [(Un-pat ts ...) 
       (eq? (syntax-e #'Un-pat) 'Un)
       (apply Un (map parse-type (syntax->list #'(ts ...))))]
      [(quot t)
       (eq? (syntax-e #'quot) 'quote)
       (-val (syntax-e #'t))]
      [(All (vars ...) t) 
       (and (eq? (syntax-e #'All) 'All)
            (andmap identifier? (syntax->list #'(vars ...))))
       (let* ([vars (map syntax-e (syntax->list #'(vars ...)))]
              [tvars (map make-F vars)])
         (add-type-name-reference #'All)
         (parameterize ([current-tvars (extend-env vars tvars (current-tvars))])
           (make-Poly vars (parse-type #'t))))]        
      [(Opaque p?) 
       (eq? (syntax-e #'Opaque) 'Opaque)
       (begin
         (add-type-name-reference #'Opaque)
         (make-Opaque #'p? (syntax-local-certifier)))]
      [(Parameter t) 
       (eq? (syntax-e #'Parameter) 'Parameter)
       (let ([ty (parse-type #'t)])
         (add-type-name-reference #'Parameter)
         (-Param ty ty))]
      [(Parameter t1 t2)
       (eq? (syntax-e #'Parameter) 'Parameter)
       (begin
         (add-type-name-reference #'Parameter)
         (-Param (parse-type #'t1) (parse-type #'t2)))]
      
      [id
       (identifier? #'id)
       (cond 
         ;; if it's a type variable, we just produce the corresponding reference (which is in the HT)
         [(lookup (current-tvars) (syntax-e #'id) (lambda (_) #f))]
         ;; if it's a type alias, we expand it (the expanded type is stored in the HT)
         [(lookup-type-alias #'id parse-type (lambda () #f))
          =>
          (lambda (t)
            ;(printf "found a type alias ~a~n" #'id)
            (add-type-name-reference #'id)
            t)]
         ;; if it's a type name, we just use the name
         [(lookup-type-name #'id (lambda () #f))
          (add-type-name-reference #'id)
          ;(printf "found a type name ~a~n" #'id)
          (make-Name #'id)]
         [else
          (tc-error/delayed "unbound type ~a" (syntax-e #'id))
          Univ])]

      [(All . rest) (eq? (syntax-e #'All) 'All) (tc-error "All: bad syntax")]
      [(Opaque . rest) (eq? (syntax-e #'Opaque) 'Opqaue) (tc-error "Opaque: bad syntax")]
      [(U . rest) (eq? (syntax-e #'U) 'U) (tc-error "Union: bad syntax")]
      [(Vectorof . rest) (eq? (syntax-e #'Vectorof) 'Vectorof) (tc-error "Vectorof: bad syntax")]
      [(mu . rest) (eq? (syntax-e #'mu) 'mu) (tc-error "mu: bad syntax")]
      [(Un . rest) (eq? (syntax-e #'Un) 'Un) (tc-error "Union: bad syntax")]
      [(t ... -> . rest) (eq? (syntax-e #'->) '->) (tc-error "->: bad syntax")]
      

      [(id arg args ...)
       (let loop 
         ([rator (parse-type #'id)]
          [args (map parse-type (syntax->list #'(arg args ...)))])
         (match rator
           [(Name: _)
            (make-App rator args stx)]
           [(Poly: ns _)
            (unless (= (length args) (length ns))
              (tc-error "Wrong number of arguments to type ~a, expected ~a but got ~a" rator (length ns) (length args)))
            (instantiate-poly rator args)]
           [(Mu: _ _) (loop (unfold rator) args)]
           [_ (tc-error/delayed "Type ~a cannot be applied, arguments were: ~a" rator args)
              Univ]))
       #;
       (let ([ty (parse-type #'id)])
         #;(printf "ty is ~a" ty)
         (unless (Poly? ty)
           (tc-error "not a polymorphic type: ~a" (syntax-e #'id)))
         (unless (= (length (syntax->list #'(arg args ...))) (Poly-n ty))
           (tc-error "wrong number of arguments to type constructor ~a: expected ~a, got ~a" 
                     (syntax-e #'id)
                     (Poly-n ty)
                     (length (syntax->list #'(arg args ...)))))
         (instantiate-poly ty (map parse-type (syntax->list #'(arg args ...)))))]
      [t
       (or (boolean? (syntax-e #'t)) (number? (syntax-e #'t))
           (string? (syntax-e #'t)))
       (-val (syntax-e #'t))]
      [_ (tc-error "not a valid type: ~a" (syntax->datum stx))])))
