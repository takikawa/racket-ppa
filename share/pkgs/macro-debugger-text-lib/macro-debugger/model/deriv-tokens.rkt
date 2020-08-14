#lang racket/base
(require (except-in parser-tools/lex define-tokens)
         "parser-util.rkt" "deriv.rkt")
(provide (all-defined-out))

;;(define-tokens error-tokens
;;  (ERROR              ; exn
;;   ))

(define-tokens macro-expansion-tokens
  #:empty-tokens
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
   IMPOSSIBLE           ; useful for error-handling clauses that have no NoError counterpart
   top-non-begin        ; .
   prepare-env          ; .
   enter-begin-for-syntax
   exit-begin-for-syntax
   )

  #:tokens
  (visit                ; Syntax
   resolve              ; identifier
   enter-macro          ; (list* Syntax Syntax)   -- (list* disarmed orig)
   macro-pre-x          ; Syntax                  -- marked-cleaned
   macro-post-x         ; (list* Syntax Syntax)   -- (list* transformed marked-cleaned)
   exit-macro           ; (list* Syntax Syntax)   -- (list* rearmed-unmarked unmarked)
   enter-prim           ; Syntax
   exit-prim            ; Syntax
   return               ; Syntax
   stop/return          ; Syntax
   exit-prim/return     ; Syntax
   enter-block          ; Syntaxes
   finish-block         ; (list Syntax)           -- list w/ one {let,letrec}-values form
   block->list          ; #f -- FIXME
   block->letrec        ; (list* Syntaxes Syntaxes Syntaxes) -- (list* idss rhss bodys)
   splice               ; Syntaxes                -- contains (append spliced-stxs previous-stxs)
   enter-list           ; Syntaxes
   exit-list            ; Syntaxes
   enter-check          ; syntax
   exit-check           ; syntax
   module-body          ; (list-of (cons syntax boolean))
   lift-loop            ; syntax = new form (let or begin; let if for_stx)
   letlift-loop         ; syntax = new let form
   module-lift-loop     ; syntaxes = def-lifts, in reverse order lifted (???)
   module-lift-end-loop ; syntaxes = statement-lifts ++ provide-lifts, in order lifted
   lift-expr            ; (list* Ids Syntax Syntax)    -- (list* gen-ids orig-s renamed-s)
   lift-end-decl        ; (list* Syntax Syntax Syntax) -- (list* orig-s renamed-s wrapped-s)
   lift-require         ; (list* Syntax Syntax Syntax) -- (list* wrapped-req orig-use-s end-use-s)
   lift-provide         ; Syntax
   lift-module          ; (list* Syntax Syntax)        -- (list* orig renamed)
   rename-transformer   ; Syntax

   module-end-lifts     ; Syntaxes
   module-pass1-lifts   ; (list* Syntaxes Syntaxes Syntaxes) -- (list* defs reqs mods)
   module-pass1-case    ; Syntax
   exit-case            ; Stx
   module-pass2-lifts   ; (list* Syntaxes Syntaxes Syntaxes) -- (list* reqs mods defs)

   enter-local          ; syntax
   local-pre            ; syntax
   local-post           ; syntax
   exit-local           ; syntax

   local-bind           ; (listof identifier)
   opaque-expr          ; opaque-syntax

   variable             ; (cons identifier identifier)
   tag                  ; Syntax
   tag2                 ; (list* Syntax Syntax)
   tag/context          ; Syntax

   rename-one           ; syntax
   rename-list          ; (listof syntax)
   lambda-renames       ; (list* Syntax Syntaxes)   -- (list* renamed-formals renamed-body)
   letX-renames         ; (list* Syntaxes Syntaxes Syntaxes Syntaxes Syntaxes)
   block-renames        ; (list* Syntaxes Syntaxes) -- (list* renamed-stxs orig-stxs)

   top-begin            ; identifier

   local-remark         ; (listof (U string syntax))
   local-artificial-step ; (list syntax syntax syntax syntax)

   track-syntax         ; (list* Symbol Syntax Syntax) -- op new old
   local-value          ; identifier
   local-value-result   ; boolean
   local-value-binding  ; result of identifier-binding; added by trace.rkt, not expander
   local-mess           ; (listof event)
   )

  #:tokens
  ;; each prim-X token carries Syntax/#f
  ;; - Syntax -- disarmed term (specifically, the disarmed term used for matching, rec expansion)
  ;; - #f     -- doesn't disarm, or disarmed not used for expansion (eg, stop)
  (prim-module
   prim-module-begin
   prim-define-syntaxes
   prim-define-values
   prim-if
   prim-with-continuation-mark
   prim-begin
   prim-begin0
   prim-#%app
   prim-lambda
   prim-case-lambda
   prim-let-values
   prim-letrec-values 
   prim-letrec-syntaxes+values
   prim-#%datum
   prim-#%top
   prim-stop
   prim-quote
   prim-quote-syntax
   prim-require
   prim-require-for-syntax
   prim-require-for-template
   prim-provide
   prim-set!
   prim-#%expression
   prim-#%variable-reference
   prim-#%stratified
   prim-begin-for-syntax
   prim-submodule
   prim-submodule*
   prim-declare
   ))

;; ** Events to tokens

;; token-mapping : Hash[ Symbol => TokenConstructor/#t ]
(define token-mapping
  (hasheq
   'EOF                     #t
   'error                   token-ERROR
   'start                   token-start
   'start-top               token-start-top
   'start-ecte              token-start-ecte
   'top-begin               token-top-begin
   'top-non-begin           token-top-non-begin
   'local-remark            token-local-remark
   'local-artificial-step   token-local-artificial-step
   'local-value-binding     token-local-value-binding
   'local-mess              token-local-mess
   'enter-begin-for-syntax  token-enter-begin-for-syntax
   'exit-begin-for-syntax   token-exit-begin-for-syntax

   'visit                   token-visit
   'resolve                 token-resolve
   'return                  token-return
   'next                    token-next
   'enter-list              token-enter-list
   'exit-list               token-exit-list
   'enter-prim              token-enter-prim
   'exit-prim               token-exit-prim
   'exit-prim/return        token-exit-prim/return
   'enter-macro             token-enter-macro
   'exit-macro              token-exit-macro
   'enter-block             token-enter-block
   'splice                  token-splice
   'block->list             token-block->list
   'next-group              token-next-group
   'finish-block            token-finish-block
   'block->letrec           token-block->letrec
   'lambda-renames          token-lambda-renames
   'letX-renames            token-letX-renames
   'macro-pre-x             token-macro-pre-x
   'macro-post-x            token-macro-post-x
   'module-body             token-module-body
   'block-renames           token-block-renames
   'phase-up                #t
   'prepare-env             #t
   'exit-local-bind         #t
   'stop/return             token-stop/return
   'tag/context             token-tag/context
   'rename-transformer      token-rename-transformer

   'module-end-lifts        token-module-end-lifts
   'module-pass1-lifts      token-module-pass1-lifts
   'module-pass1-case       token-module-pass1-case
   'module-pass2-lifts      token-module-pass2-lifts
   'exit-case               token-exit-case

   'prim-stop               token-prim-stop
   'prim-module             token-prim-module
   'prim-module-begin       token-prim-module-begin
   'prim-define-syntaxes    token-prim-define-syntaxes
   'prim-define-values      token-prim-define-values
   'prim-if                 token-prim-if
   'prim-with-continuation-mark token-prim-with-continuation-mark
   'prim-begin              token-prim-begin
   'prim-begin0             token-prim-begin0
   'prim-#%app              token-prim-#%app
   'prim-lambda             token-prim-lambda
   'prim-case-lambda        token-prim-case-lambda
   'prim-let-values         token-prim-let-values
   'prim-letrec-values      token-prim-letrec-values
   'prim-letrec-syntaxes+values token-prim-letrec-syntaxes+values
   'prim-#%datum            token-prim-#%datum
   'prim-#%top              token-prim-#%top
   'prim-quote              token-prim-quote
   'prim-quote-syntax       token-prim-quote-syntax
   'prim-require            token-prim-require
   'prim-require-for-syntax token-prim-require-for-syntax
   'prim-require-for-template token-prim-require-for-template
   'prim-provide            token-prim-provide
   'prim-set!               token-prim-set!
   'prim-#%variable-reference token-prim-#%variable-reference
   'prim-#%expression       token-prim-#%expression
   'prim-#%stratified       token-prim-#%stratified
   'prim-begin-for-syntax   token-prim-begin-for-syntax
   'prim-submodule          token-prim-submodule
   'prim-submodule*         token-prim-submodule*
   'prim-declare            token-prim-declare

   'variable                token-variable
   'enter-check             token-enter-check
   'exit-check              token-exit-check
   'lift-loop               token-lift-loop
   'lift-expr               token-lift-expr
   'enter-local             token-enter-local
   'exit-local              token-exit-local
   'local-pre               token-local-pre
   'local-post              token-local-post
   'lift-end-decl           token-lift-end-decl
   'module-lift-end-loop    token-module-lift-end-loop
   'letlift-loop            token-letlift-loop
   'module-lift-loop        token-module-lift-loop
   'start                   token-start
   'tag                     token-tag
   'tag2                    token-tag2
   'local-bind              token-local-bind
   'enter-bind              token-enter-bind
   'exit-bind               token-exit-bind
   'opaque-expr             token-opaque-expr
   'rename-list             token-rename-list
   'rename-one              token-rename-one
   'lift-require            token-lift-require
   'lift-provide            token-lift-provide
   'lift-module             token-lift-module
   'track-syntax            token-track-syntax
   'local-value             token-local-value
   'local-value-result      token-local-value-result
   'start-top               token-start-top
   ))

(define (tokenize key val pos)
  (cond [(hash-ref token-mapping key #f)
         => (lambda (make-token)
              (cond [(and (procedure? make-token)
                          (procedure-arity-includes? make-token 1))
                     (make-position-token (make-token val) pos pos)]
                    [val (error 'tokenize "unexpected payload (key = ~s): ~e" key val)]
                    [else (make-position-token key pos pos)]))]
        [else (error 'tokenize "bad signal: ~s" key)]))
