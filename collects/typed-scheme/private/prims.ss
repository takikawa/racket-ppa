#lang scheme/base

#|

This file defines two sorts of primitives. All of them are provided into any module using the typed scheme language.

1. macros for defining type annotated code. 
   this includes: lambda:, define:, etc
   potentially, these macros should be replacements for the mzscheme ones in the user program
   however, it's nice to be able to use both when the sugar is more limited
   for example: (define ((f x) y) (+ x y))

2. macros for defining 'magic' code
   examples: define-typed-struct, require/typed
   these expand into (ignored) mzscheme code, and declarations that a typechecker understands
   in order to protect the declarations, they are wrapped in `#%app void' so that local-expand of the module body
   will not expand them on the first pass of the macro expander (when the stop list is ignored)

|#


(provide (all-defined-out)
         :
	 (rename-out [define-typed-struct define-struct:]
                     [lambda: λ:]
                     [define-typed-struct/exec define-struct/exec:]))

(require "../utils/utils.ss"
	 (for-syntax 
          syntax/parse
	  syntax/private/util
          scheme/base
          (rep type-rep)
          mzlib/match
          "parse-type.ss" "annotate-classes.ss"
          syntax/struct
          syntax/stx
          scheme/struct-info
          (private internal)
	  (except-in (utils utils tc-utils))
          (env type-name-env)
          "type-contract.ss"))

(require (utils require-contract)
         "colon.ss"
         (typecheck internal-forms)
         (except-in mzlib/contract ->)
         (only-in mzlib/contract [-> c->])
         mzlib/struct
         "base-types.ss"
         "base-types-extra.ss")

(define-for-syntax (ignore stx) (syntax-property stx 'typechecker:ignore #t))


(define-syntax (require/typed stx)
  (define-syntax-class opt-rename
    #:attributes (nm spec)
    (pattern nm:id
             #:with spec #'nm)
    (pattern (orig-nm:id internal-nm:id)
	     #:with spec #'(orig-nm internal-nm)
	     #:with nm #'internal-nm))
  (define-syntax-class simple-clause
    #:attributes (nm ty)
    (pattern [nm:opt-rename ty]))
  (define-syntax-class struct-clause
    ;#:literals (struct)
    #:attributes (nm (body 1))
    (pattern [struct nm:opt-rename (body ...)]
             #:fail-unless (eq? 'struct (syntax-e #'struct)) #f))
  (define-syntax-class opaque-clause
    ;#:literals (opaque)
    #:attributes (ty pred opt)
    (pattern [opaque ty:id pred:id]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
             #:with opt #'())
    (pattern [opaque ty:id pred:id #:name-exists]
             #:fail-unless (eq? 'opaque (syntax-e #'opaque)) #f
             #:with opt #'(#:name-exists)))
  (syntax-parse stx
    [(_ lib (~or sc:simple-clause strc:struct-clause oc:opaque-clause) ...)
     #'(begin 
	 (require/opaque-type oc.ty oc.pred lib . oc.opt) ...
	 (require/typed sc.nm sc.ty lib) ... 
	 (require-typed-struct strc.nm (strc.body ...) lib) ...)]
    [(_ nm:opt-rename ty lib (~optional [~seq #:struct-maker parent]) ...)
     (with-syntax ([cnt* (generate-temporary #'nm.nm)]
		   [sm (if (attribute parent)
                           #'(#:struct-maker parent)
                           #'())])
       (let ([prop-name (if (attribute parent)
                            'typechecker:contract-def/maker
                            'typechecker:contract-def)])
         (quasisyntax/loc stx 
           (begin 
             #,(syntax-property (syntax-property #'(define cnt #f)
                                                 prop-name #'ty)
                                'typechecker:ignore #t)
             #,(internal #'(require/typed-internal nm.nm ty . sm))
             #,(syntax-property #'(require/contract nm.spec cnt lib)
                                'typechecker:ignore #t)))))]))

(define-syntax (require/opaque-type stx)
  (define-syntax-class name-exists-kw
    (pattern #:name-exists))
  (syntax-parse stx
    [(_ ty:id pred:id lib (~optional ne:name-exists-kw) ...)
     (register-type-name #'ty (make-Opaque #'pred (syntax-local-certifier)))
     (quasisyntax/loc stx
       (begin 
         #,(syntax-property #'(define pred-cnt (any/c . c-> . boolean?))
                            'typechecker:ignore #t)
         #,(internal #'(require/typed-internal pred (Any -> Boolean : (Opaque pred))))
         #,(if (attribute ne)
               (internal (syntax/loc stx (define-type-alias-internal ty (Opaque pred))))
               (syntax/loc stx (define-type-alias ty (Opaque pred))))
         #,(syntax-property #'(require/contract pred pred-cnt lib)
                            'typechecker:ignore #t)))]))

(define-syntax (plambda: stx)
  (syntax-parse stx 
    [(plambda: (tvars:id ...) formals . body)
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (lambda: formals . body))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: (tvars:id ...) cl ...)
     (quasisyntax/loc stx
       (#%expression
        #,(syntax-property (syntax/loc stx (case-lambda: cl ...))
                           'typechecker:plambda
                           #'(tvars ...))))]))

(define-syntax (pdefine: stx)
  (syntax-parse stx #:literals (:)
    [(pdefine: (tvars:id ...) (nm:id . formals:annotated-formals) : ret-ty . body)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (define: nm : type
           (plambda: (tvars ...) formals . body))))]))

(define-syntax (ann stx)
  (syntax-parse stx #:literals (:)
    [(_ (~or (~seq arg : ty) (~seq arg ty)))
     (syntax-property #'arg 'type-ascription #'ty)]))

(define-syntax (inst stx)
  (syntax-parse stx #:literals (:)
    [(_ arg : . tys)
     (syntax/loc stx (inst arg . tys))]    
    [(_ arg tys ... ty ddd b:id)
     #:when (eq? (syntax-e #'ddd) '...)
     (syntax-property #'arg 'type-inst #'(tys ... (ty . b)))]
    [(_ arg tys ...)
     (syntax-property #'arg 'type-inst #'(tys ...))]))

(define-syntax (define: stx)
  (syntax-parse stx #:literals (:)
    [(define: (nm:id . formals:annotated-formals) (~describe "return type annotation" (~seq : ret-ty)) body ...)
     (with-syntax ([arrty (syntax/loc stx (formals.arg-ty ... -> ret-ty))])
       (syntax/loc stx
         (define: nm : arrty
           (lambda: formals body ...))))]
    [(define: nm:id ~! (~describe "type annotation" (~seq : ty)) body)
     (identifier? #'nm)
     (with-syntax ([new-nm (syntax-property #'nm 'type-label #'ty)])
       (syntax/loc stx (define new-nm body)))]
    [(define: (tvars:id ...) (nm:id . formals:annotated-formals) : ret-ty body ...)
     (with-syntax ([type (syntax/loc #'ret-ty (All (tvars ...) (formals.arg-ty ... -> ret-ty)))])
       (syntax/loc stx
         (define: nm : type
           (plambda: (tvars ...) formals body ...))))]))

(define-syntax (lambda: stx)
  (syntax-parse stx
    [(lambda: formals:annotated-formals . body)
     (syntax/loc stx (lambda formals.ann-formals . body))]))

(define-syntax (case-lambda: stx)
  (syntax-parse stx
    [(case-lambda: [formals:annotated-formals . body] ...)
     (syntax/loc stx (case-lambda [formals.ann-formals . body] ...))]))

(define-syntaxes (let-internal: let*: letrec:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:annotated-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let) (mk #'let*) (mk #'letrec))))

(define-syntaxes (let-values: let*-values: letrec-values:)
  (let ([mk (lambda (form)
              (lambda (stx)
                (syntax-parse stx
                  [(_ (bs:annotated-values-binding ...) . body)
                   (quasisyntax/loc stx (#,form (bs.binding ...) . body))])))])
    (values (mk #'let-values) (mk #'let*-values) (mk #'letrec-values))))

(define-syntax (let: stx)
  (syntax-parse stx #:literals (:)
    [(let: nm:id ~! : ret-ty (bs:annotated-binding ...) . body)
     (syntax/loc stx ((letrec: ([nm : (bs.ty ... -> ret-ty) (lambda (bs.ann-name ...) . body)]) nm) bs.rhs ...))]
    [(let: . rest)
     (syntax/loc stx (let-internal: . rest))]))

(define-syntax (define-type-alias stx)
  (syntax-parse stx
    [(_ tname:id rest) 
     #`(begin
         #,(ignore #'(define-syntax tname (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))))
         #,(internal (syntax/loc stx (define-type-alias-internal tname rest))))]
    [(_ (tname:id args:id ...) rest)
     (syntax/loc stx (define-type-alias tname (All (args ...) rest)))]))

(define-syntax (define-typed-struct/exec stx)
  (syntax-parse stx #:literals (:)
    [(_ nm ((~describe "field specification" [fld:annotated-name]) ...) [proc : proc-ty])
     (with-syntax* 
      ([proc* (syntax-property #'(ann proc : proc-ty) 'typechecker:with-type #t)]
       [d-s (syntax-property (syntax/loc stx (define-struct/properties nm (fld.name ...)
                                               ([prop:procedure proc*])))
                             'typechecker:ignore-some #t)]
       [dtsi (internal (syntax/loc stx (define-typed-struct/exec-internal nm (fld ...) proc-ty)))])
      #'(begin d-s dtsi))]))

(define-syntax (with-handlers: stx)
  (syntax-parse stx
    [(_ ([pred? action] ...) . body)
     (with-syntax ([(pred?* ...) (map (lambda (s) (syntax-property #`(ann #,s : (Any -> Any)) 'typechecker:with-type #t))
                                      (syntax->list #'(pred? ...)))]
                   [(action* ...)
                    (map (lambda (s) (syntax-property s 'typechecker:exn-handler #t)) (syntax->list #'(action ...)))]
                   [body* (syntax-property #'(let-values () . body) 'typechecker:exn-body #t)])
       (syntax-property #'(with-handlers ([pred?* action*] ...) body*)
                        'typechecker:with-handlers
                        #t))]))

(define-syntax (define-typed-struct stx)
  (define-syntax-class fld-spec
    #:literals (:)
    #:description "[field-name : type]"
    (pattern [fld:id : ty]))
  (define-syntax-class struct-name
    #:description "struct name (with optional super-struct name)"
    #:attributes (name super)
    (pattern (name:id super:id))
    (pattern name:id
	     #:with super #f))
  (syntax-parse stx
    [(_ nm:struct-name (fs:fld-spec ...) . opts)
     (let ([mutable (if (memq '#:mutable (syntax->datum #'opts))
                        '(#:mutable)
                        '())])
       (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fs.fld ...) . opts))
                                           'typechecker:ignore #t)]
                     [dtsi (internal (quasisyntax/loc stx (define-typed-struct-internal nm (fs ...) #,@mutable)))])
         #'(begin d-s dtsi)))]
    [(_ (vars:id ...) nm:struct-name (fs:fld-spec ...) . opts)
     (with-syntax ([d-s (syntax-property (syntax/loc stx (define-struct nm (fs.fld ...) . opts))
                                         'typechecker:ignore #t)]
                   [dtsi (internal (syntax/loc stx (define-typed-struct-internal (vars ...) nm (fs ...))))])
       #'(begin d-s dtsi))]))

(define-syntax (require-typed-struct stx)
  (syntax-parse stx #:literals (:)
    [(_ nm:id ([fld : ty] ...) lib)
     (with-syntax* ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                    [(mut ...) (map (lambda _ #'#f) (syntax->list #'(sel ...)))])
                   (quasisyntax/loc stx
                     (begin
                       (require (only-in lib struct-info))
                       (define-syntax nm (make-struct-info 
                                          (lambda ()
                                            (list #'struct-info
                                                  #'maker
                                                  #'pred
                                                  (reverse (list #'sel ...))
                                                  (list mut ...)
                                                  #f))))
                       #,(internal #'(define-typed-struct-internal nm ([fld : ty] ...) #:type-only))
                       #,(ignore #'(require/contract pred (any/c . c-> . boolean?) lib))
                       #,(internal #'(require/typed-internal pred (Any -> Boolean : nm)))
                       (require/typed maker nm lib #:struct-maker #f)
                       (require/typed lib 
                         [sel (nm -> ty)]) ...)))]
    [(_ (nm parent) ([fld : ty] ...) lib)
     (and (identifier? #'nm) (identifier? #'parent))
     (with-syntax* ([(struct-info maker pred sel ...) (build-struct-names #'nm (syntax->list #'(fld ...)) #f #t)]
                    [(mut ...) (map (lambda _ #'#f) (syntax->list #'(sel ...)))]
                    #;[(parent-tys ...) (Struct-flds (parse-type #'parent))])
                   #`(begin
                       (require (only-in lib struct-info))
                       (define-syntax nm (make-struct-info 
                                          (lambda ()
                                            (list #'struct-info
                                                  #'maker
                                                  #'pred
                                                  (list #'sel ...)
                                                  (list mut ...)
                                                  #f))))
                       #,(internal #'(define-typed-struct-internal (nm parent) ([fld : ty] ...) #:type-only))
                       #,(ignore #'(require/contract pred (any/c . c-> . boolean?) lib))
                       #,(internal #'(require/typed-internal pred (Any -> Boolean : nm)))
                       (require/typed maker nm lib #:struct-maker parent)
                       (require/typed lib                          
                         [sel (nm -> ty)]) ...))]))

(define-syntax (do: stx)
  (syntax-parse stx #:literals (:)
    [(_ : ty 
        ((var:annotated-name init (~optional step:expr #:defaults ([step #'var]))) ...) 
        (stop?:expr (~optional (~seq finish:expr ...) #:defaults ([(finish 1) #'((void))])))
        c:expr ...)
     (syntax/loc
         stx
       (let: doloop : ty ([var.name : var.ty init] ...)
         (if stop?
             (begin finish ...)
             (begin c ... (doloop step ...)))))]))

(define-syntax (provide: stx)
  (syntax-parse stx
    [(_ [i:id t] ...)
     (syntax/loc stx
       (begin (: i t) ...
              (provide i ...)))]))

(define-syntax (declare-refinement stx)
  (syntax-parse stx
    [(_ p:id)
     (quasisyntax/loc stx #,(internal #'(declare-refinement-internal p)))]))

(define-syntaxes (let/cc: let/ec:)
  (let ()
    (define ((mk l/c) stx)      
      (syntax-parse stx
       [(_ k:annotated-name . body)
	(quasisyntax/loc stx (#,l/c k.name . body))]))
    (values (mk #'let/cc) (mk #'let/ec))))
