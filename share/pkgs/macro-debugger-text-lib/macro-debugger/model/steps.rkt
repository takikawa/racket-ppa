#lang racket/base
(require racket/contract/base
         "stx-util.rkt")
(provide (struct-out protostep)
         (struct-out step)
         (struct-out misstep)
         (struct-out remarkstep)
         (struct-out state)
         (struct-out bigframe)
         reduction-sequence/c
         context-fill
         state-term
         step-term1
         step-term2
         misstep-term1
         bigframe-term
         step-type?
         step-type->string
         rewrite-step?
         rename-step?)

;; A Step is one of
;;  - (step StepType State State)
;;  - (misstep StepType State Exn)
;;  - (remarkstep StepType State (Listof (U String Syntax 'arrow)))
(struct protostep (type s1) #:transparent)
(struct step protostep (s2) #:transparent)
(struct misstep protostep (exn) #:transparent)
(struct remarkstep protostep (contents) #:transparent)

;; A ReductionSequence is (Listof Step)
(define reduction-sequence/c (listof protostep?))

;; A State is (state Stx Stxs Context BigContext Ids Ids Stxs Nat/#f)
(struct state (e foci ctx lctx binders uses frontier seq) #:transparent)

;; A Context is (Listof Frame)
;; A Frame is (Syntax -> Syntax)

;; A BigContext is (list-of BigFrame)
;; A BigFrame is (make-bigframe Context Syntaxes Syntax)
(struct bigframe (ctx foci e))

;; context-fill : Context Syntax -> Syntax
(define (context-fill ctx stx)
  (datum->artificial-syntax
   (let loop ([ctx ctx] [stx stx])
     (if (null? ctx)
         stx
         (let ([frame0 (car ctx)])
           (loop (cdr ctx) (frame0 stx)))))))

(define (state-term s)
  (context-fill (state-ctx s) (state-e s)))

(define (step-term1 s)
  (state-term (protostep-s1 s)))
(define (step-term2 s)
  (state-term (step-s2 s)))

(define (misstep-term1 s)
  (state-term (protostep-s1 s)))

(define (bigframe-term bf)
  (context-fill (bigframe-ctx bf) (bigframe-e bf)))

;; A StepType is a Symbol in the following alist.

(define step-type-meanings
  ;; Kinds: rw=rewrite, sc=scope, tr=track, ac=action, er=error
  '((macro            rw "Macro transformation")

    (tag-module-begin rw "Add explicit #%module-begin")
    (tag-app          rw "Add explicit #%app")
    (tag-datum        rw "Add explicit #%datum")
    (tag-top          rw "Add explicit #%top")

    (finish-block     rw "Finish block")
    (finish-lsv       rw "Finish letrec-syntaxes+values")
    (finish-expr      rw "Finish #%expression")

    (block->letrec    rw "Transform block to letrec")
    (splice-block     rw "Splice block-level begin")
    (splice-module    rw "Splice module-level begin")
    (splice-lifts     rw "Splice definitions from lifted expressions")
    (splice-end-lifts rw "Splice lifted module declarations")
    (capture-lifts    rw "Capture lifts")

    (provide          rw "Expand provide-specs")

    (rename-lambda    sc "Introduce scope for local bindings")
    (rename-letX      sc "Introduce scope for local bindings")
    (rename-block     sc "Introduce scope for internal definition context")
    (rename-module    sc "Introduce scope for module")
    (rename-mod-shift sc "Shift the self module-path-index")
    (rename-modbeg    sc "Introduce scope for module body")

    (resolve-variable sc "Resolve variable (remove extra scopes)") ;; rw?

    (local-lift       ac "Lift")
    (remark           ac "Macro made a remark")

    (sync             -- "Sync with expander")
    (error            er "Error")))

(define (step-type->string x)
  (cond [(assq x step-type-meanings) => caddr]
        [(string? x) x]
        [else (error 'step-type->string "not a step type: ~s" x)]))

(define (step-type? x #:kinds [kinds #f])
  (cond [(assq x step-type-meanings)
         => (lambda (c)
              (define kind (cadr c))
              (or (not kinds) (and (memq kind kinds) #t)))]
        [else #f]))

(define (rename-step? x) (step-type? x #:kinds '(sc)))
(define (rewrite-step? x) (step-type? x #:kinds '(rw er)))
