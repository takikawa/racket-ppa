#lang racket/base
(require parser-tools/lex
         "deriv.rkt")
(provide (all-defined-out))

(define-tokens basic-empty-tokens
  (start                ; .
   start-top            ; .
   start-ecte           ; .
   next                 ; .
   next-group           ; .
   phase-up             ; .
   ...                  ; .
   EOF                  ; .
   enter-bind           ; .
   exit-bind            ; .
   exit-local-bind      ; .
   IMPOSSIBLE           ; useful for error-handling clauses that have no
                        ; NoError counterpart
   top-non-begin        ; .
   prepare-env          ; .
   ))

(define-tokens basic-tokens
  (visit                ; syntax
   resolve              ; identifier
   enter-macro          ; syntax
   macro-pre-x          ; syntax
   macro-post-x         ; (cons syntax syntax)
   exit-macro           ; syntax
   enter-prim           ; syntax
   exit-prim            ; syntax
   return               ; syntax
   enter-block          ; syntaxes
   block->list          ; syntaxes
   block->letrec        ; syntax(es?)
   splice               ; syntaxes
   enter-list           ; syntaxes
   exit-list            ; syntaxes
   enter-check          ; syntax
   exit-check           ; syntax
   module-body          ; (list-of (cons syntax boolean))
   syntax-error         ; exn
   lift-loop            ; syntax = new form (let or begin; let if for_stx)
   letlift-loop         ; syntax = new let form
   module-lift-loop     ; syntaxes = def-lifts, in reverse order lifted (???)
   module-lift-end-loop ; syntaxes = statement-lifts ++ provide-lifts, in order lifted
   lift-expr            ; (cons (listof id) syntax)
   lift-statement       ; syntax
   lift-require         ; (cons syntax (cons syntax syntax))
   lift-provide         ; syntax

   enter-local          ; syntax
   local-pre            ; syntax
   local-post           ; syntax
   exit-local           ; syntax

   local-bind           ; (listof identifier)
   opaque-expr          ; opaque-syntax

   variable             ; (cons identifier identifier)
   tag                  ; syntax

   rename-one           ; syntax
   rename-list          ; (list-of syntax)
   lambda-renames       ; (cons syntax syntax)
   let-renames          ; (cons (listof syntax) syntax)
   letrec-syntaxes-renames ; (cons (listof syntax) (cons (listof syntax) syntax))
   block-renames        ; (cons syntax syntax) ... contains both pre+post

   top-begin            ; identifier

   local-remark         ; (listof (U string syntax))
   local-artificial-step ; (list syntax syntax syntax syntax)

   track-origin         ; (cons stx stx)
   local-value          ; identifier
   local-value-result   ; boolean
   local-value-binding  ; result of identifier-binding; added by trace.rkt, not expander
   local-mess           ; (listof event)
   ))

;; Empty tokens
(define-tokens prim-tokens
  (prim-module prim-module-begin
   prim-define-syntaxes prim-define-values
   prim-if prim-with-continuation-mark
   prim-begin prim-begin0 prim-#%app prim-lambda
   prim-case-lambda prim-let-values prim-letrec-values 
   prim-letrec-syntaxes+values prim-#%datum prim-#%top prim-stop
   prim-quote prim-quote-syntax prim-require prim-require-for-syntax
   prim-require-for-template prim-provide
   prim-set!
   prim-#%expression
   prim-#%variable-reference
   prim-#%stratified
   prim-begin-for-syntax
   prim-submodule prim-submodule*
   ))

;; ** Events to tokens

;; token-mapping : Hash[ Symbol => TokenConstructor/#t ]
(define token-mapping
  (hasheq
   'EOF                     #t
   'error                   token-syntax-error
   'start                   token-start
   'start-top               token-start-top
   'start-ecte              token-start-ecte
   'top-begin               token-top-begin
   'top-non-begin           token-top-non-begin
   'local-remark            token-local-remark
   'local-artificial-step   token-local-artificial-step
   'local-value-binding     token-local-value-binding
   'local-mess              token-local-mess

   'visit                   token-visit
   'resolve                 token-resolve
   'return                  token-return
   'next                    token-next
   'enter-list              token-enter-list
   'exit-list               token-exit-list
   'enter-prim              token-enter-prim
   'exit-prim               token-exit-prim
   'enter-macro             token-enter-macro
   'exit-macro              token-exit-macro
   'enter-block             token-enter-block
   'splice                  token-splice
   'block->list             token-block->list
   'next-group              token-next-group
   'block->letrec           token-block->letrec
   'let-renames             token-let-renames
   'lambda-renames          token-lambda-renames
   'letrec-syntaxes-renames token-letrec-syntaxes-renames
   'macro-pre-x             token-macro-pre-x
   'macro-post-x            token-macro-post-x
   'module-body             token-module-body
   'block-renames           token-block-renames
   'phase-up                #t
   'prepare-env             #t
   'exit-local-bind         #t

   'prim-stop               #t
   'prim-module             #t
   'prim-module-begin       #t
   'prim-define-syntaxes    #t
   'prim-define-values      #t
   'prim-if                 #t
   'prim-with-continuation-mark #t
   'prim-begin              #t
   'prim-begin0             #t
   'prim-#%app              #t
   'prim-lambda             #t
   'prim-case-lambda        #t
   'prim-let-values         #t
   'prim-letrec-values      #t
   'prim-letrec-syntaxes+values #t
   'prim-#%datum            #t
   'prim-#%top              #t
   'prim-quote              #t
   'prim-quote-syntax       #t
   'prim-require            #t
   'prim-require-for-syntax #t
   'prim-require-for-template #t
   'prim-provide            #t
   'prim-set!               #t
   'prim-let*-values        #t
   'prim-#%variable-reference #t
   'prim-#%expression       #t
   'prim-#%stratified       #t
   'prim-begin-for-syntax   #t
   'prim-submodule          #t
   'prim-submodule*         #t

   'variable                token-variable
   'enter-check             token-enter-check
   'exit-check              token-exit-check
   'lift-loop               token-lift-loop
   'lift-expr               token-lift-expr
   'enter-local             token-enter-local
   'exit-local              token-exit-local
   'local-pre               token-local-pre
   'local-post              token-local-post
   'lift-statement          token-lift-statement
   'module-lift-end-loop    token-module-lift-end-loop
   'letlift-loop            token-letlift-loop
   'module-lift-loop        token-module-lift-loop
   'start                   token-start
   'tag                     token-tag
   'local-bind              token-local-bind
   'enter-bind              token-enter-bind
   'exit-bind               token-exit-bind
   'opaque-expr             token-opaque-expr
   'rename-list             token-rename-list
   'rename-one              token-rename-one
   'lift-require            token-lift-require
   'lift-provide            token-lift-provide
   'track-origin            token-track-origin
   'local-value             token-local-value
   'local-value-result      token-local-value-result
   'start-top               token-start-top
   ))

(define (tokenize key val pos)
  (cond [(hash-ref token-mapping key #f)
         => (lambda (make-token)
              (if (procedure? make-token)
                  (make-position-token (make-token val) pos pos)
                  (make-position-token key pos pos)))]
        [else (error 'tokenize "bad signal: ~s" key)]))
