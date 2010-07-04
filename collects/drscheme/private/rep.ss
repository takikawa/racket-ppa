#lang scheme/base

#|

TODO
- should a GC should happen on each execution? (or perhaps better, each kill?)
- front-end methods have new signature


|#
; =Kernel= means in DrScheme's thread and parameterization
; 
; =User= means the user's thread and parameterization
; 
; =Handler= means in the handler thread of some eventspace; it must
;  be combined with either =Kernel= or =User=

;; WARNING: printf is rebound in this module to always use the 
;;          original stdin/stdout of drscheme, instead of the 
;;          user's io ports, to aid any debugging printouts.
;;          (esp. useful when debugging the users's io)

(require scheme/class
         scheme/path
         scheme/pretty
         scheme/unit
         scheme/list
         "drsig.ss"
         string-constants
         setup/xref
         scheme/gui/base
         framework
         browser/external)

(provide rep@ with-stacktrace-name)

(define stacktrace-runtime-name
  (string->uninterned-symbol "this-is-the-funny-name"))

;; this function wraps its argument expression in some code in a non-tail manner
;; so that a new name gets put onto the mzscheme stack. DrScheme's exception
;; handlers trims the stack starting at this point to avoid showing drscheme's
;; internals on the stack in the REPL.
(define call-with-stacktrace-name
  (eval `(let ([,stacktrace-runtime-name
                (lambda (thunk)
                  (begin0
                    (thunk)
                    (void)))])
           ,stacktrace-runtime-name)
        (make-base-namespace)))

(define-syntax-rule (with-stacktrace-name expr)
  (call-with-stacktrace-name (lambda () expr)))

(define no-breaks-break-parameterization
  (parameterize-break 
   #f
   (current-break-parameterization)))

(define-unit rep@
  (import (prefix drscheme:init: drscheme:init^)
          (prefix drscheme:language-configuration: drscheme:language-configuration/internal^)
          (prefix drscheme:language: drscheme:language^)
          (prefix drscheme:app: drscheme:app^)
          (prefix drscheme:frame: drscheme:frame^)
          (prefix drscheme:unit: drscheme:unit^)
          (prefix drscheme:text: drscheme:text^)
          (prefix drscheme:help-desk: drscheme:help-desk^)
          (prefix drscheme:debug: drscheme:debug^)
          [prefix drscheme:eval: drscheme:eval^])
  (export (rename drscheme:rep^
                  [-text% text%]
                  [-text<%> text<%>]))
  
  (define -text<%>
    (interface ((class->interface text%)
                text:ports<%>
                editor:file<%>
                scheme:text<%>
                color:text<%>
                text:ports<%>)
      reset-highlighting
      highlight-errors
      highlight-errors/exn
      
      get-user-custodian
      get-user-eventspace
      get-user-thread
      get-user-namespace
      
      get-definitions-text
      
      kill-evaluation
      
      display-results
      
      run-in-evaluation-thread
      after-many-evals
      
      shutdown
      
      get-error-ranges
      reset-error-ranges
      
      reset-console
      
      copy-prev-previous-expr
      copy-next-previous-expr
      copy-previous-expr
      
      
      initialize-console
      
      reset-pretty-print-width
      
      get-prompt
      insert-prompt
      get-context))
  
  (define context<%>
    (interface ()
      ensure-rep-shown   ;; (interactions-text -> void)
      ;; make the rep visible in the frame
      
      needs-execution   ;; (-> boolean)
      ;; ask if things have changed that would mean the repl is out
      ;; of sync with the program being executed in it.
      
      enable-evaluation  ;; (-> void)
      ;; make the context enable all methods of evaluation
      ;; (disable buttons, menus, etc)
      
      disable-evaluation ;; (-> void)
      ;; make the context disable all methods of evaluation
      ;; (disable buttons, menus, etc)
      
      set-breakables ;; (union thread #f) (union custodian #f) -> void
      ;; the context might initiate breaks or kills to
      ;; the thread passed to this function
      
      get-breakables ;; -> (values (union thread #f) (union custodian #f))
      ;; returns the last values passed to set-breakables.
      
      reset-offer-kill ;; (-> void)
      ;; the next time the break button is pushed, it will only
      ;; break. (if the break button is clicked twice without
      ;; this method being called in between, it will offer to
      ;; kill the user's program)
      
      update-running        ;; (boolean -> void)
      ;; a callback to indicate that the repl may have changed its running state
      ;; use the repls' get-in-evaluation? method to find out what the current state is.
      
      clear-annotations  ;; (-> void)
      ;; clear any error highlighting context
      
      get-directory      ;; (-> (union #f string[existing directory]))
      ;; returns the directory that should be the default for
      ;; the `current-directory' and `current-load-relative-directory'
      ;; parameters in the repl.
      ))
  
  (define sized-snip<%>
    (interface ((class->interface snip%))
      ;; get-character-width : -> number
      ;; returns the number of characters wide the snip is,
      ;; for use in pretty printing the snip.
      get-character-width))
  
  ;; current-language-settings : (parameter language-setting)
  ;; set to the current language and its setting on the user's thread.
  (define current-language-settings (make-parameter #f))
  
  ;; current-rep : (parameter (union #f (instanceof rep:text%)))
  ;; the repl that controls the evaluation in this thread.
  (define current-rep (make-parameter #f))
  
  ;; a port that accepts values for printing in the repl
  (define current-value-port (make-parameter #f))
  
  ;; drscheme-error-display-handler : (string (union #f exn) -> void
  ;; =User=
  ;; the timing is a little tricky here. 
  ;; the file icon must appear before the error message in the text, so that happens first.
  ;; the highlight must be set after the error message, because inserting into the text resets
  ;;     the highlighting.
  (define (drscheme-error-display-handler msg exn)
    (let* ([cut-stack (if (and (exn? exn)
                               (main-user-eventspace-thread?))
                          (cut-out-top-of-stack exn)
                          '())]
           [srclocs-stack (filter values (map cdr cut-stack))]
           [stack 
            (filter
             values
             (map (λ (srcloc)
                    (let ([source (srcloc-source srcloc)]
                          [pos (srcloc-position srcloc)]
                          [span (srcloc-span srcloc)])
                      (and source pos span 
                           srcloc)))
                  srclocs-stack))]
           [src-locs (if (exn:srclocs? exn)
                         ((exn:srclocs-accessor exn) exn)
                         (if (null? stack)
                             '()
                             (list (car srclocs-stack))))])
      
      ;; for use in debugging the stack trace stuff
      #;
      (when (exn? exn)
        (print-struct #t)
        (for-each 
         (λ (frame) (printf " ~s\n" frame))
         (continuation-mark-set->context (exn-continuation-marks exn)))
        (printf "\n"))
      
      (drscheme:debug:error-display-handler/stacktrace msg exn stack)))
  
  (define (main-user-eventspace-thread?)
    (let ([rep (current-rep)])
      (and rep
           (eq? (eventspace-handler-thread (send rep get-user-eventspace))
                (current-thread)))))
  
  (define (cut-out-top-of-stack exn)
    (let ([initial-stack (continuation-mark-set->context (exn-continuation-marks exn))])
      (let loop ([stack initial-stack])
        (cond
          [(null? stack) 
           (unless (exn:break? exn)
             ;; give break exn's a free pass on this one.
             ;; sometimes they get raised in a funny place. 
             ;; (see call-with-break-parameterization below)
             (unless (null? initial-stack)
               ;; sometimes, mzscheme just doesn't have any backtrace all. in that case,
               ;; don't print anything either.
               (fprintf (current-error-port) "ACK! didn't find drscheme's stackframe when filtering\n")))
           initial-stack]
          [else 
           (let ([top (car stack)])
             (cond
               [(cut-here? top) null]
               [else (cons top (loop (cdr stack)))]))]))))
  
  ;; is-cut? : any symbol -> boolean
  ;; determines if this stack entry is drscheme's barrier in the stacktrace
  (define (cut-here? top)
    (and (pair? top)
         (let ([fn-name (car top)])
           (eq? fn-name stacktrace-runtime-name))))
  
  (define drs-bindings-keymap (make-object keymap:aug-keymap%))

  (let* ([get-frame
          (λ (obj)
            (and (is-a? obj editor<%>)
                 (let ([canvas (send obj get-canvas)])
                   (and canvas
                        (let ([frame (send canvas get-top-level-window)])
                          (and (is-a? frame drscheme:unit:frame%)
                               frame))))))]
         [add-drs-function
          (λ (name f)
            (send drs-bindings-keymap add-function name
                  (λ (obj evt) (cond [(get-frame obj) => f]))))])
    (send drs-bindings-keymap add-function "search-help-desk"
          (λ (obj evt)
            (if (not (and (is-a? obj text%) (get-frame obj))) ; is `get-frame' needed?
              (drscheme:help-desk:help-desk)
              (let* ([start (send obj get-start-position)]
                     [end (send obj get-end-position)]
                     [str (if (= start end)
                            (drscheme:unit:find-symbol obj start)
                            (send obj get-text start end))])
                (if (or (not str) (equal? "" str))
                  (drscheme:help-desk:help-desk)
                  (let* ([l (send obj get-canvas)]
                         [l (and l (send l get-top-level-window))]
                         [l (and l (is-a? l drscheme:unit:frame<%>) (send l get-definitions-text))]
                         [l (and l (send l get-next-settings))]
                         [l (and l (drscheme:language-configuration:language-settings-language l))]
                         [ctxt (and l (send l capability-value 'drscheme:help-context-term))]
                         [name (and l (send l get-language-name))])
                    (drscheme:help-desk:help-desk
                     str (and ctxt (list ctxt name)))))))))
    (add-drs-function "execute"  (λ (frame) (send frame execute-callback)))
    (add-drs-function "next-tab" (λ (frame) (send frame next-tab)))
    (add-drs-function "prev-tab" (λ (frame) (send frame prev-tab)))
    (add-drs-function "collapse" (λ (frame) (send frame collapse)))
    (add-drs-function "split"    (λ (frame) (send frame split))))
  
  (send drs-bindings-keymap map-function "f5" "execute")
  (send drs-bindings-keymap map-function "f1" "search-help-desk")
  (send drs-bindings-keymap map-function "c:tab" "next-tab")
  (send drs-bindings-keymap map-function "c:s:tab" "prev-tab")
  (send drs-bindings-keymap map-function "d:s:right" "next-tab")
  (send drs-bindings-keymap map-function "d:s:left" "prev-tab")
  (send drs-bindings-keymap map-function "c:pagedown" "next-tab")
  (send drs-bindings-keymap map-function "c:pageup" "prev-tab")
  
  (send drs-bindings-keymap map-function "c:x;0" "collapse")
  (send drs-bindings-keymap map-function "c:x;2" "split")
  
  (define (get-drs-bindings-keymap) drs-bindings-keymap)
  
  ;; drs-bindings-keymap-mixin :
  ;;   ((implements editor:keymap<%>) -> (implements editor:keymap<%>))
  ;;   for any x that is an instance of the resulting class,
  ;;     (is-a? (send (send x get-canvas) get-top-level-frame) drscheme:unit:frame%)
  (define drs-bindings-keymap-mixin
    (mixin (editor:keymap<%>) (editor:keymap<%>)
      (define/override (get-keymaps)
        (cons drs-bindings-keymap (super get-keymaps)))
      (super-instantiate ())))
  
  ;; Max length of output queue (user's thread blocks if the
  ;; queue is full):
  (define output-limit-size 2000)
  
  (define (printf . args) (apply fprintf drscheme:init:original-output-port args))
  
  (define setup-scheme-interaction-mode-keymap
    (λ (keymap)
      (send keymap add-function "put-previous-sexp"
            (λ (text event) 
              (send text copy-prev-previous-expr)))
      (send keymap add-function "put-next-sexp"
            (λ (text event) 
              (send text copy-next-previous-expr)))
      
      (keymap:send-map-function-meta keymap "p" "put-previous-sexp")
      (keymap:send-map-function-meta keymap "n" "put-next-sexp")
      (send keymap map-function "c:up" "put-previous-sexp")
      (send keymap map-function "c:down" "put-next-sexp")))
  
  (define scheme-interaction-mode-keymap (make-object keymap:aug-keymap%))
  (setup-scheme-interaction-mode-keymap scheme-interaction-mode-keymap)
  
  (define drs-font-delta (make-object style-delta% 'change-family 'decorative))
  
  (define output-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
  (define result-delta (make-object style-delta%)) ; used to be 'change-weight 'bold
  (define error-delta (make-object style-delta%
                        'change-style
                        'italic))
  (send error-delta set-delta-foreground (make-object color% 255 0 0))
  (send result-delta set-delta-foreground (make-object color% 0 0 175))
  (send output-delta set-delta-foreground (make-object color% 150 0 150))
  
  (define error-text-style-delta (make-object style-delta%))
  (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))
  
  (define grey-delta (make-object style-delta%))
  (send grey-delta set-delta-foreground "GREY")
  
  (define welcome-delta (make-object style-delta% 'change-family 'decorative))
  (define click-delta (gui-utils:get-clickback-delta))
  (define red-delta (make-object style-delta%))
  (define dark-green-delta (make-object style-delta%))
  (send* red-delta
    (copy welcome-delta)
    (set-delta-foreground "RED"))  
  (send* dark-green-delta
    (copy welcome-delta)
    (set-delta-foreground "dark green"))
  (define warning-style-delta (make-object style-delta% 'change-bold))
  (send* warning-style-delta
    (set-delta-foreground "BLACK")
    (set-delta-background "YELLOW"))
  (define (get-welcome-delta) welcome-delta)
  (define (get-dark-green-delta) dark-green-delta)
  
  ;; is-default-settings? : language-settings -> boolean
  ;; determines if the settings in `language-settings'
  ;; correspond to the default settings of the language.
  (define (is-default-settings? language-settings)
    (send (drscheme:language-configuration:language-settings-language language-settings)
          default-settings?
          (drscheme:language-configuration:language-settings-settings language-settings)))
  
  (define (extract-language-name language-settings)
    (send (drscheme:language-configuration:language-settings-language language-settings)
          get-language-name))
  (define (extract-language-style-delta language-settings)
    (send (drscheme:language-configuration:language-settings-language language-settings)
          get-style-delta))
  (define (extract-language-url language-settings)
    (send (drscheme:language-configuration:language-settings-language language-settings)
          get-language-url))
  
  (define-struct sexp (left right prompt))
  
  (define console-max-save-previous-exprs 30)
  (let* ([list-of? (λ (p?)
                     (λ (l)
                       (and (list? l)
                            (andmap p? l))))]
         [snip/string? (λ (s) (or (is-a? s snip%) (string? s)))]
         [list-of-snip/strings? (list-of? snip/string?)]
         [list-of-lists-of-snip/strings? (list-of? list-of-snip/strings?)])
    (preferences:set-default
     'drscheme:console-previous-exprs
     null
     list-of-lists-of-snip/strings?))
  (let ([marshall 
         (λ (lls)
           (map (λ (ls)
                  (list
                   (apply
                    string-append
                    (reverse
                     (map (λ (s)
                            (cond
                              [(is-a? s string-snip%)
                               (send s get-text 0 (send s get-count))]
                              [(string? s) s]
                              [else "'non-string-snip"]))
                          ls)))))
                lls))]
        [unmarshall (λ (x) x)])
    (preferences:set-un/marshall
     'drscheme:console-previous-exprs
     marshall unmarshall))
  
  (define color? ((get-display-depth) . > . 8))
  
  ;; instances of this interface provide a context for a rep:text%
  ;; its connection to its graphical environment (ie frame) for
  ;; error display and status infromation is all mediated
  ;; through an instance of this interface.
  
  (define file-icon
    (let ([bitmap
           (make-object bitmap%
             (build-path (collection-path "icons") "file.gif"))])
      (if (send bitmap ok?)
          (make-object image-snip% bitmap)
          (make-object string-snip% "[open file]"))))
  
  
  ;; insert/delta : (instanceof text%) (union snip string) (listof style-delta%) *-> (values number number)
  ;; inserts the string/stnip into the text at the end and changes the
  ;; style of the newly inserted text based on the style deltas.
  (define (insert/delta text s . deltas)
    (let ([before (send text last-position)])
      (send text insert s before before #f)
      (let ([after (send text last-position)])
        (for-each (λ (delta)
                    (when (is-a? delta style-delta%)
                      (send text change-style delta before after)))
                  deltas)
        (values before after))))
  
  (define text-mixin
    (mixin ((class->interface text%)
            text:ports<%>
            editor:file<%>
            scheme:text<%>
            color:text<%>
            text:ports<%>)
      (-text<%>)
      (init-field context)
      (inherit auto-wrap
               begin-edit-sequence
               change-style
               clear-box-input-port
               clear-undos
               clear-input-port
               clear-output-ports
               delete
               delete/io
               end-edit-sequence
               erase
               find-snip
               find-string
               freeze-colorer
               get-active-canvas
               get-admin
               get-can-close-parent
               get-canvases
               get-character
               get-end-position
               get-err-port
               get-extent
               get-focus-snip
               get-in-port
               get-in-box-port
               get-insertion-point
               get-out-port
               get-regions
               get-snip-position
               get-start-position
               get-styles-fixed
               get-style-list
               get-text
               get-top-level-window
               get-unread-start-point
               get-value-port
               in-edit-sequence?
               insert
               insert-before
               insert-between
               invalidate-bitmap-cache
               is-frozen?
               is-locked?
               last-position
               line-location
               lock
               paragraph-start-position
               position-line
               position-paragraph
               port-name-matches?
               release-snip
               reset-input-box
               reset-regions
               run-after-edit-sequence
               scroll-to-position
               send-eof-to-in-port 
               set-allow-edits
               set-caret-owner
               set-clickback
               set-insertion-point
               set-position
               set-styles-sticky
               set-styles-fixed
               set-unread-start-point
               split-snip
               thaw-colorer)
      
      (define definitions-text 'not-yet-set-definitions-text)
      (define/public (set-definitions-text dt) (set! definitions-text dt))
      (define/public (get-definitions-text) definitions-text)
      
      (unless (is-a? context context<%>)
        (error 'drscheme:rep:text% 
               "expected an object that implements drscheme:rep:context<%> as initialization argument, got: ~e"
               context))
      
      (define/public (get-context) context)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;				           ;;;
      ;;;            User -> Kernel                ;;;
      ;;;				           ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; =User= (probably doesn't matter)
      (define/private queue-system-callback
        (λ (ut thunk [always? #f])
          (parameterize ([current-eventspace drscheme:init:system-eventspace])
            (queue-callback 
             (λ ()
               (when (or always? (eq? ut (get-user-thread)))
                 (thunk)))
             #f))))
      
      ;; =User=
      (define/private queue-system-callback/sync
        (λ (ut thunk)
          (let ([s (make-semaphore 0)])
            (queue-system-callback 
             ut 
             (λ ()
               (when (eq? ut (get-user-thread))
                 (thunk))
               (semaphore-post s))
             #t)
            (semaphore-wait s))))
      
      ;; display-results : (listof TST) -> void
      ;; prints each element of anss that is not void as values in the REPL.
      (define/public (display-results anss) ; =User=, =Handler=, =Breaks=
        (display-results/void (filter (λ (x) (not (void? x))) anss)))
      
      ;; display-results : (listof TST) -> void
      ;; prints each element of anss in the REPL.
      (define/public (display-results/void anss) ; =User=, =Handler=, =Breaks=
        (for-each 
         (λ (v) 
           (let* ([ls (current-language-settings)]
                  [lang (drscheme:language-configuration:language-settings-language ls)]
                  [settings (drscheme:language-configuration:language-settings-settings ls)])
             (send lang render-value/format
                   v
                   settings
                   (get-value-port)
                   (get-repl-char-width))))
         anss))
      
      ;; get-repl-char-width : -> (and/c exact? integer?)
      ;; returns the width of the repl in characters, or 80 if the
      ;; answer cannot be found.
      (define/private (get-repl-char-width)
        (let ([admin (get-admin)]
              [standard (send (get-style-list) find-named-style "Standard")])
          (if (and admin standard)
              (let ([bw (box 0)])
                (send admin get-view #f #f bw #f)
                (let* ([dc (send admin get-dc)]
                       [standard-font (send standard get-font)]
                       [old-font (send dc get-font)])
                  (send dc set-font standard-font)
                  (let* ([char-width (send dc get-char-width)]
                         [answer (inexact->exact (floor (/ (unbox bw) char-width)))])
                    (send dc set-font old-font)
                    answer)))
              80)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;            Error Highlighting              ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; error-ranges : (union false? (cons (list file number number) (listof (list file number number))))
      (define error-ranges #f)
      (define/public (get-error-ranges) error-ranges)
      (define internal-reset-callback void)
      (define internal-reset-error-arrows-callback void)
      (define/public (reset-error-ranges) 
        (internal-reset-callback)
        (internal-reset-error-arrows-callback))
      
      ;; highlight-error : file number number -> void
      (define/public (highlight-error file start end)
        (highlight-errors (list (make-srcloc file #f #f start (- end start))) #f))
      
      ;; highlight-errors/exn : exn -> void
      ;; highlights all of the errors associated with the exn (incl. arrows)
      (define/public (highlight-errors/exn exn)
        (let ([locs (cond
                      [(exn:srclocs? exn)
                       ((exn:srclocs-accessor exn) exn)]
                      [else '()])])
          (highlight-errors locs #f)))
      
      ;; =Kernel= =handler=
      ;; highlight-errors :    (listof srcloc)
      ;;                       (union #f (listof srcloc))
      ;;                    -> (void)
      (define/public (highlight-errors raw-locs raw-error-arrows)
        (let* ([cleanup-locs
                (λ (locs)
                  (let ([ht (make-hasheq)])
                    (filter (λ (loc) (and (is-a? (srcloc-source loc) text:basic<%>)
                                          (number? (srcloc-position loc))
                                          (number? (srcloc-span loc))))
                            (map (λ (srcloc)
                                   (cond
                                     [(hash-ref ht (srcloc-source srcloc) #f)
                                      =>
                                      (λ (e) 
                                        (make-srcloc e
                                                     (srcloc-line srcloc)
                                                     (srcloc-column srcloc)
                                                     (srcloc-position srcloc)
                                                     (srcloc-span srcloc)))]
                                     [(send definitions-text port-name-matches? (srcloc-source srcloc))
                                      (hash-set! ht (srcloc-source srcloc) definitions-text)
                                      (make-srcloc definitions-text
                                                   (srcloc-line srcloc)
                                                   (srcloc-column srcloc)
                                                   (srcloc-position srcloc)
                                                   (srcloc-span srcloc))]
                                     [(port-name-matches? (srcloc-source srcloc))
                                      (hash-set! ht (srcloc-source srcloc) this)
                                      (make-srcloc this
                                                   (srcloc-line srcloc)
                                                   (srcloc-column srcloc)
                                                   (srcloc-position srcloc)
                                                   (srcloc-span srcloc))]
                                     [(and (symbol? (srcloc-source srcloc))
                                           (text:lookup-port-name (srcloc-source srcloc)))
                                      =>
                                      (lambda (editor)
                                        (make-srcloc editor
                                                     (srcloc-line srcloc)
                                                     (srcloc-column srcloc)
                                                     (srcloc-position srcloc)
                                                     (srcloc-span srcloc)))]
                                     [else srcloc]))
                                 locs))))]
               [locs (cleanup-locs raw-locs)]
               [error-arrows (and raw-error-arrows (cleanup-locs raw-error-arrows))])
          
          (reset-highlighting)
          
          (set! error-ranges locs)
          
          (for-each (λ (loc) (send (srcloc-source loc) begin-edit-sequence)) locs)
          
          (when color?
            (let ([resets
                   (map (λ (loc)
                          (let* ([file (srcloc-source loc)]
                                 [start (- (srcloc-position loc) 1)]
                                 [span (srcloc-span loc)]
                                 [finish (+ start span)])
                            (send file highlight-range start finish (drscheme:debug:get-error-color) #f 'high)))
                        locs)])
              
              (when (and definitions-text error-arrows)
                (let ([filtered-arrows
                       (remove-duplicate-error-arrows
                        (filter
                         (λ (arr) (embedded-in? (srcloc-source arr) definitions-text))
                         error-arrows))])
                  (send definitions-text set-error-arrows filtered-arrows)))
              
              (set! internal-reset-callback
                    (λ ()
                      (set! error-ranges #f)
                      (when definitions-text
                        (send definitions-text set-error-arrows #f))
                      (set! internal-reset-callback void)
                      (for-each (λ (x) (x)) resets)))))
          
          (let* ([first-loc (and (pair? locs) (car locs))]
                 [first-file (and first-loc (srcloc-source first-loc))]
                 [first-start (and first-loc (- (srcloc-position first-loc) 1))]
                 [first-span (and first-loc (srcloc-span first-loc))])
            
            (when (and first-loc first-start first-span)
              (let ([first-finish (+ first-start first-span)])
                (when (eq? first-file definitions-text) ;; only move set the cursor in the defs window
                  (send first-file set-position first-start first-start))
                (send first-file scroll-to-position first-start #f first-finish)))
            
            (for-each (λ (loc) (send (srcloc-source loc) end-edit-sequence)) locs)
            
            (when first-loc
              (send first-file set-caret-owner (get-focus-snip) 'global)))))
      
      (define highlights-can-be-reset (make-parameter #t))
      (define/public (reset-highlighting)
        (when (highlights-can-be-reset) (reset-error-ranges)))
      (define/public (call-without-reset-highlighting thunk)
        (parameterize ([highlights-can-be-reset #f])
          (thunk)))
      
      ;; remove-duplicate-error-arrows : (listof X) -> (listof X)
      ;; duplicate arrows point from and to the same place -- only
      ;; need one arrow for each pair of locations they point to.
      (define/private (remove-duplicate-error-arrows error-arrows)
        (let ([ht (make-hash)])
          (let loop ([arrs error-arrows]
                     [n 0])
            (unless (null? arrs)
              (hash-set! ht (car arrs) n)
              (loop (cdr arrs) (+ n 1))))
          (let* ([unsorted (hash-map ht list)]
                 [sorted (sort unsorted (λ (x y) (<= (cadr x) (cadr y))))]
                 [arrs (map car sorted)])
            arrs)))
      
      (define/private (embedded-in? txt-inner txt-outer)
        (let loop ([txt-inner txt-inner])
          (cond
            [(eq? txt-inner txt-outer) #t]
            [else (let ([admin (send txt-inner get-admin)])
                    (and (is-a? admin editor-snip-editor-admin<%>)
                         (loop (send (send (send admin get-snip) get-admin) get-editor))))])))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  specialization
      ;;
      
      (define/override (after-io-insertion)
        (super after-io-insertion)
        (let ([canvas (get-active-canvas)])
          (when canvas
            (let ([frame (send canvas get-top-level-window)])
              (let ([tab (send definitions-text get-tab)])
                (when (eq? (send frame get-current-tab) tab)
                  (send context ensure-rep-shown this)))))))
      
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (cond
          [(in-edit-sequence?) 
           (set! had-an-insert (cons (cons start len) had-an-insert))]
          [else (update-after-insert start len)]))
      
      ;; private field
      (define had-an-insert '())
      
      (define/augment (after-edit-sequence)
        (inner (void) after-edit-sequence)
        (let ([to-clean had-an-insert])
          (set! had-an-insert '())
          (for-each
           (lambda (pr)
             (update-after-insert (car pr) (cdr pr)))
           to-clean)))
      
      (define/private (update-after-insert start len)
        (unless inserting-prompt?
          (reset-highlighting))
        (when (and prompt-position (< start prompt-position))
          
          ;; trim extra space, according to preferences
          #;
          (let* ([start (get-repl-header-end)]
                 [end (get-insertion-point)]
                 [space (- end start)]
                 [pref (preferences:get 'drscheme:repl-buffer-size)])
            (when (car pref)
              (let ([max-space (* 1000 (cdr pref))])
                (when (space . > . max-space)
                  (let ([to-delete-end (+ start (- space max-space))])
                    (delete/io start to-delete-end))))))
          
          (set! prompt-position (get-unread-start-point))
          (reset-regions (append (all-but-last (get-regions))
                                 (list (list prompt-position 'end))))))
      
      (define/augment (after-delete x y)
        (unless inserting-prompt?
          (reset-highlighting))
        (inner (void) after-delete x y))
      
      (define/override get-keymaps
        (λ ()
          (cons scheme-interaction-mode-keymap (super get-keymaps))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                            ;;;
      ;;;                Evaluation                  ;;;
      ;;;                                            ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define/public (eval-busy?)
        (not (and (get-user-thread)
                  (thread-running? (get-user-thread)))))
      
      (field (user-language-settings #f)
             (user-custodian-parent #f)
             (memory-killed-thread #f)
             (user-custodian #f)
             (custodian-limit (and (custodian-memory-accounting-available?)
                                   (preferences:get 'drscheme:memory-limit)))
             (user-eventspace-box (make-weak-box #f))
             (user-namespace-box (make-weak-box #f))
             (user-eventspace-main-thread #f)
             (user-break-parameterization #f)
             
             ;; user-exit-code (union #f (integer-in 0 255))
             ;; #f indicates that exit wasn't called. Integer indicates exit code
             (user-exit-code #f))
      
      (define/public (get-user-language-settings) user-language-settings)
      (define/public (get-user-custodian) user-custodian)
      (define/public (get-user-eventspace) (weak-box-value user-eventspace-box))
      (define/public (get-user-thread) user-eventspace-main-thread)
      (define/public (get-user-namespace) (weak-box-value user-namespace-box))
      (define/pubment (get-user-break-parameterization) user-break-parameterization) ;; final method
      (define/pubment (get-custodian-limit) custodian-limit)
      (define/pubment (set-custodian-limit c) (set! custodian-limit c))
      (field (in-evaluation? #f)
             (ask-about-kill? #f))
      (define/public (get-in-evaluation?) in-evaluation?)
      
      (define/public-final (insert-warning message)
        (let ([locked? (is-locked?)])
          (when locked? (lock #f))
          (begin-edit-sequence)
          (let ([start (get-unread-start-point)])
            (insert-before message)
            (let ([end (get-unread-start-point)])
              (change-style warning-style-delta start end)
              (insert-before "\n")))
          (end-edit-sequence)
          (when locked? (lock #t))))
      
      (field (already-warned? #f)
             (show-no-user-evaluation-message? #t))
      
      ;; use this to be able to kill the evaluator without the popup dialog
      (define/public (set-show-no-user-evaluation-message? b)
        (set! show-no-user-evaluation-message? b))
      
      (define/private (cleanup)
        (set! in-evaluation? #f)
        (update-running #f)
        (unless (and (get-user-thread) (thread-running? (get-user-thread)))
          (lock #t)
          (when (and show-no-user-evaluation-message? (not shutting-down?))
            (no-user-evaluation-message
             (let ([canvas (get-active-canvas)])
               (and canvas
                    (send canvas get-top-level-window)))
             user-exit-code
             (not (thread-running? memory-killed-thread))))
          (set! show-no-user-evaluation-message? #t)))
      
      (field (need-interaction-cleanup? #f))
      
      (define/private (no-user-evaluation-message frame exit-code memory-killed?)
        (let* ([new-limit (and custodian-limit (+ (* 1024 1024 128) custodian-limit))]
               [ans (message-box/custom
                     (string-constant evaluation-terminated)
                     (string-append
                      (string-constant evaluation-terminated-explanation)
                      (if exit-code
                          (string-append
                           "\n\n"
                           (if (zero? exit-code)
                               (string-constant exited-successfully)
                               (format (string-constant exited-with-error-code) exit-code)))
                          "")
                      (if memory-killed?
                          (string-append 
                           "\n\n"
                           (string-constant program-ran-out-of-memory))
                          ""))
                     (string-constant ok)
                     #f
                     (and memory-killed?
                          new-limit
                          (format "Increase memory limit to ~a megabytes" 
                                  (floor (/ new-limit 1024 1024))))
                     frame
                     '(default=1 stop)
                     )])
          (when (equal? ans 3)
            (set-custodian-limit new-limit)
            (preferences:set 'drscheme:memory-limit new-limit))
          (set-insertion-point (last-position))
          (insert-warning "\nInteractions disabled")))
      
      (define/private (cleanup-interaction) ; =Kernel=, =Handler=
        (set! need-interaction-cleanup? #f)
        (begin-edit-sequence)
        (set-caret-owner #f 'display)
        (cleanup)
        (end-edit-sequence)
        (send context set-breakables #f #f)
        (send context enable-evaluation))
      
      (define/augment (submit-to-port? key)
        (and prompt-position
             (only-whitespace-after-insertion-point)
             (submit-predicate this prompt-position)))
      
      (define/private (only-whitespace-after-insertion-point)
        (let ([start (get-start-position)]
              [end (get-end-position)])
          (and (= start end)
               (let loop ([pos start])
                 (cond
                   [(= pos (last-position)) #t]
                   [else (and (char-whitespace? (get-character pos))
                              (loop (+ pos 1)))])))))
      
      (define/augment (on-submit)
        (inner (void) on-submit)
        (when (and (get-user-thread) 
                   (thread-running? (get-user-thread)))
          ;; the -2 drops the last newline from history (why -2 and not -1?!)
          (save-interaction-in-history prompt-position (- (last-position) 2))
          
          (let* ([old-regions (get-regions)]
                 [abl (all-but-last old-regions)]
                 [lst (last old-regions)])
            (reset-regions (append abl (list (list (list-ref lst 0) (last-position))))))
          
          (let ([needs-execution (send context needs-execution)])
            (when (if (preferences:get 'drscheme:execute-warning-once)
                      (and (not already-warned?)
                           needs-execution)
                      needs-execution)
              (set! already-warned? #t)
              (insert-warning needs-execution)))
          
          ;; lets us know we are done with this one interaction
          ;; (since there may be multiple expressions at the prompt)
          (send-eof-to-in-port)
          
          (set! prompt-position #f)
          (evaluate-from-port
           (get-in-port) 
           #f
           (λ ()
             ;; clear out the eof object if it wasn't consumed
             (clear-input-port)))))
      
      (inherit get-backward-sexp)
      (define/override (on-local-char key)
        (let ([start (get-start-position)]
              [end (get-end-position)]
              [code (send key get-key-code)])
          (cond
            [(not (or (eq? code 'numpad-enter)
                      (equal? code #\return)
                      (equal? code #\newline)))
             (super on-local-char key)]
            [(not prompt-position) 
             ;; evaluating? just drop the keypress
             (void)] 
            [(and (< end prompt-position)
                  (= start end)
                  (get-backward-sexp end))
             =>
             (λ (sexp-start)
               (copy-down sexp-start end))]
            [(and (< end prompt-position)
                  (not (= start end)))
             (copy-down start end)]
            [else
             (super on-local-char key)])))
      
      (define/private (copy-down start end)
        (begin-edit-sequence)
        (split-snip start)
        (split-snip end)
        (let loop ([snip (find-snip start 'after-or-none)])
          (when snip
            (let ([pos (+ (get-snip-position snip)
                          (send snip get-count))])
              (when (<= pos end)
                (insert (send snip copy) (last-position) (last-position))
                (loop (send snip next))))))
        (set-position (last-position) (last-position))
        (end-edit-sequence))
      
      ;; prompt-position : (union #f integer)
      ;; the position just after the last prompt
      (field (prompt-position #f))
      (define inserting-prompt? #f)
      (define/public (get-prompt) "> ")
      (define/public (insert-prompt)
        (set! inserting-prompt? #t)
        (begin-edit-sequence)
        (reset-input-box)
        (let* ([pmt (get-prompt)]
               [prompt-space (string-length pmt)])
          
          ;; insert the prompt, possibly inserting a newline first
          (let* ([usp (get-unread-start-point)]
                 [usp-para (position-paragraph usp)]
                 [usp-para-start (paragraph-start-position usp-para)])
            (unless (equal? usp usp-para-start)
              (insert-between "\n")
              (set! prompt-space (+ prompt-space 1)))
            (insert-between pmt))
          
          (let ([sp (get-unread-start-point)])
            (set! prompt-position sp)
            (reset-regions (append (get-regions) (list (list sp 'end))))))
        (end-edit-sequence)
        (set! inserting-prompt? #f))
      
      (field [submit-predicate (λ (text prompt-position) #t)])
      (define/public (set-submit-predicate p)
        (set! submit-predicate p))
      
      (define/public (evaluate-from-port port complete-program? cleanup) ; =Kernel=, =Handler=
        (send context disable-evaluation)
        (send context reset-offer-kill)
        (send context set-breakables (get-user-thread) (get-user-custodian))
        (reset-pretty-print-width)
        (set! in-evaluation? #t)
        (update-running #t)
        (set! need-interaction-cleanup? #t)
        
        (run-in-evaluation-thread
         (λ () ; =User=, =Handler=, =No-Breaks=
           (let* ([settings (current-language-settings)]
                  [lang (drscheme:language-configuration:language-settings-language settings)]
                  [settings (drscheme:language-configuration:language-settings-settings settings)]
                  [dummy-value (box #f)]
                  [get-sexp/syntax/eof 
                   (if complete-program?
                       (send lang front-end/complete-program port settings)
                       (send lang front-end/interaction port settings))])
             
             ; Evaluate the user's expression. We're careful to turn on
             ;   breaks as we go in and turn them off as we go out.
             ;   (Actually, we adjust breaks however the user wanted it.)
             
             (call-with-continuation-prompt
              (λ ()
                (call-with-break-parameterization
                 user-break-parameterization
                 (λ ()
                   (let loop ()
                     (let ([sexp/syntax/eof (with-stacktrace-name (get-sexp/syntax/eof))])
                       (unless (eof-object? sexp/syntax/eof)
                         (call-with-values
                          (λ () 
                            (call-with-continuation-prompt
                             (λ () (with-stacktrace-name (eval-syntax sexp/syntax/eof)))
                             (default-continuation-prompt-tag)
                             (and complete-program?
                                  (λ args
                                    (abort-current-continuation 
                                     (default-continuation-prompt-tag))))))
                          (λ x (for-each (λ (x) ((current-print) x)) x)))
                         (loop)))))))
              (default-continuation-prompt-tag)
              (λ args (void)))
             
             (when complete-program?
               (call-with-continuation-prompt
                (λ ()
                  (call-with-break-parameterization
                   user-break-parameterization
                   (λ ()
                     (send lang front-end/finished-complete-program settings))))
                (default-continuation-prompt-tag)
                (λ args (void))))
             
             (set! in-evaluation? #f)
             (update-running #f)
             (cleanup)
             (flush-output (get-value-port))
             (queue-system-callback/sync
              (get-user-thread)
              (λ () ; =Kernel=, =Handler= 
                (after-many-evals)
                (cleanup-interaction)
                (insert-prompt)))))))
      
      (define/pubment (after-many-evals) (inner (void) after-many-evals))
      
      (define/private shutdown-user-custodian ; =Kernel=, =Handler=
        ; Use this procedure to shutdown when in the middle of other cleanup
        ;  operations, such as when the user clicks "Execute".
        ; Don't use it to kill a thread where other, external cleanup
        ;  actions must occur (e.g., the exit handler for the user's
        ;  thread). In that case, shut down user-custodian directly.
        (λ ()
          (when user-custodian
            (custodian-shutdown-all user-custodian))
          (set! user-custodian #f)
          (set! user-eventspace-main-thread #f)))
      
      (define/public (kill-evaluation) ; =Kernel=, =Handler=
        (when user-custodian
          (custodian-shutdown-all user-custodian))
        (set! user-custodian #f))
      
      (field (eval-thread-thunks null)
             (eval-thread-state-sema 'not-yet-state-sema)
             (eval-thread-queue-sema 'not-yet-thread-sema)
             
             (cleanup-sucessful 'not-yet-cleanup-sucessful)
             (cleanup-semaphore 'not-yet-cleanup-semaphore)
             (thread-killed 'not-yet-thread-killed))
      (define/private (initialize-killed-thread) ; =Kernel=
        (when (thread? thread-killed)
          (kill-thread thread-killed))
        (set! thread-killed
              (thread
               (λ () ; =Kernel=
                 (let ([ut (get-user-thread)])
                   (thread-wait ut)
                   (queue-system-callback
                    ut
                    (λ () ; =Kernel=, =Handler=
                      (if need-interaction-cleanup?
                          (cleanup-interaction)
                          (cleanup))
                      ;; HACK: lock the interactions now; the reason for this
                      ;; is that `cleanup-interaction' invokes
                      ;; `enable-evaluation', and in "unit.ss" this is defined
                      ;; to unlock the interactions which might make sense in
                      ;; that context.
                      (lock #t))))))))
      
      (define/public (run-in-evaluation-thread thunk) ; =Kernel=
        (semaphore-wait eval-thread-state-sema)
        (set! eval-thread-thunks (append eval-thread-thunks (list thunk)))
        (semaphore-post eval-thread-state-sema)
        (semaphore-post eval-thread-queue-sema))
      
      (define/private (init-evaluation-thread) ; =Kernel=
        (set! user-language-settings (send definitions-text get-next-settings))
        
        (set! user-custodian-parent (make-custodian))
        (set! user-custodian (parameterize ([current-custodian user-custodian-parent])
                               (make-custodian)))
        (set! memory-killed-thread 
              (parameterize ([current-custodian user-custodian-parent])
                (thread (λ () (semaphore-wait (make-semaphore 0))))))
        (when custodian-limit
          (custodian-limit-memory user-custodian-parent 
                                  custodian-limit
                                  user-custodian-parent))
        (let ([user-eventspace (parameterize ([current-custodian user-custodian])
                                 (make-eventspace))])
          (set! user-eventspace-box (make-weak-box user-eventspace))
          (set! user-break-parameterization (parameterize-break 
                                             #t 
                                             (current-break-parameterization)))
          (set! eval-thread-thunks null)
          (set! eval-thread-state-sema (make-semaphore 1))
          (set! eval-thread-queue-sema (make-semaphore 0))
          (set! user-exit-code #f)
          
          (let* ([init-thread-complete (make-semaphore 0)]
                 [goahead (make-semaphore)])
            
            ; setup standard parameters
            (let ([snip-classes
                   ; the snip-classes in the DrScheme eventspace's snip-class-list
                   (drscheme:eval:get-snip-classes)]
                  [drs-eventspace (current-eventspace)])
              (queue-user/wait
               (λ () ; =User=, =No-Breaks=
                 ; No user code has been evaluated yet, so we're in the clear...
                 (break-enabled #f)
                 (set! user-eventspace-main-thread (current-thread))
                 
                 (let ([drscheme-exit-handler
                        (λ (x)
                          (parameterize-break
                           #f
                           (let ([s (make-semaphore)])
                             (parameterize ([current-eventspace drs-eventspace])
                               (queue-callback
                                (λ ()
                                  (set! user-exit-code 
                                        (if (and (integer? x)
                                                 (<= 0 x 255))
                                            x
                                            0))
                                  (semaphore-post s))))
                             (semaphore-wait s)
                             (custodian-shutdown-all user-custodian))))])
                   (exit-handler drscheme-exit-handler))
                 (initialize-parameters snip-classes))))
            
            ;; disable breaks until an evaluation actually occurs
            (send context set-breakables #f #f)
            
            ;; initialize the language
            (send (drscheme:language-configuration:language-settings-language user-language-settings)
                  on-execute
                  (drscheme:language-configuration:language-settings-settings user-language-settings)
                  (let ([run-on-user-thread (lambda (t) (queue-user/wait t))])
                    run-on-user-thread))
            
            ;; setup the special repl values
            (let ([raised-exn? #f]
                  [exn #f])
              (queue-user/wait
               (λ () ; =User=, =No-Breaks=
                 (with-handlers ((void (λ (x) 
                                         (set! exn x)
                                         (set! raised-exn? #t))))
                   (drscheme:language:setup-setup-values))))
              (when raised-exn?
                (fprintf 
                 (current-error-port)
                 "copied exn raised when setting up snip values (thunk passed as third argume to drscheme:language:add-snip-value)\n")
                (raise exn)))
            
            (parameterize ([current-eventspace user-eventspace])
              (queue-callback
               (λ ()
                 (set! in-evaluation? #f)
                 (update-running #f)
                 (send context set-breakables #f #f)
                 
                 ;; after this returns, future event dispatches
                 ;; will use the user's break parameterization
                 (initialize-dispatch-handler)
                 
                 ;; let init-thread procedure return,
                 ;; now that parameters are set
                 (semaphore-post init-thread-complete)
                 
                 ; We're about to start running user code.
                 
                 ; Pause to let killed-thread get initialized
                 (semaphore-wait goahead)
                 
                 (let loop () ; =User=, =Handler=, =No-Breaks=
                   ; Wait for something to do
                   (unless (semaphore-try-wait? eval-thread-queue-sema)
                     ; User event callbacks run here; we turn on
                     ;  breaks in the dispatch handler.
                     (yield eval-thread-queue-sema))
                   ; About to eval something
                   (semaphore-wait eval-thread-state-sema)
                   (let ([thunk (car eval-thread-thunks)])
                     (set! eval-thread-thunks (cdr eval-thread-thunks))
                     (semaphore-post eval-thread-state-sema)
                     ; This thunk evals the user's expressions with appropriate
                     ;   protections.
                     (thunk))
                   (loop)))))
            (semaphore-wait init-thread-complete)
            ; Start killed-thread
            (initialize-killed-thread)
            ; Let user expressions go...
            (semaphore-post goahead))))
      
      (define/private (queue-user/wait thnk)
        (let ([wait (make-semaphore 0)])
          (parameterize ([current-eventspace (get-user-eventspace)])
            (queue-callback
             (λ ()
               (thnk)
               (semaphore-post wait))))
          (semaphore-wait wait)))
      
      (field (shutting-down? #f))
      
      (define/override (allow-close-with-no-filename?) #t)
      (define/augment (can-close?)
        (and (cond
               [in-evaluation?
                (equal? (message-box/custom
                         (string-constant drscheme)
                         (string-constant program-is-still-running)
                         (string-constant close-anyway)
                         (string-constant cancel)
                         #f
                         (or (get-top-level-window) (get-can-close-parent))
                         '(default=1 caution)
                         2)
                        1)]
               [(let ([user-eventspace (get-user-eventspace)])
                  (and user-eventspace
                       (parameterize ([current-eventspace user-eventspace])
                         (not (null? (get-top-level-windows))))))
                (equal? (message-box/custom
                         (string-constant drscheme)
                         (string-constant program-has-open-windows)
                         (string-constant close-anyway)
                         (string-constant cancel)
                         #f
                         (or (get-top-level-window) (get-can-close-parent))
                         '(default=1 caution)
                         2)
                        1)]
               [else #t])
             (inner #t can-close?)))
      
      (define/augment (on-close)
        (shutdown)
        (preferences:set 'drscheme:console-previous-exprs 
                         (trim-previous-exprs
                          (append 
                           (preferences:get 'drscheme:console-previous-exprs)
                           local-previous-exprs)))
        (inner (void) on-close))
      
      (define/public (shutdown) ; =Kernel=, =Handler=
        (set! shutting-down? #t)
        (when (thread? thread-killed)
          (kill-thread thread-killed)
          (set! thread-killed #f))
        (shutdown-user-custodian))
      
      (define/private update-running ; =User=, =Handler=, =No-Breaks=
        (λ (bool)
          (queue-system-callback
           (get-user-thread)
           (λ ()
             (send context update-running bool)))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;                                          ;;;
      ;;;                Execution                 ;;;
      ;;;                                          ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; initialize-paramters : (listof snip-class%) -> void
      (define/private (initialize-parameters snip-classes) ; =User=
        
        (current-language-settings user-language-settings)
        (error-print-source-location #f)
        (error-display-handler drscheme-error-display-handler)
        (current-load-relative-directory #f)
        (current-custodian user-custodian)
        (current-load text-editor-load-handler)
        
        (drscheme:eval:set-basic-parameters snip-classes)
        (current-rep this)
        (let ([dir (or (send context get-directory)
                       drscheme:init:first-dir)])
          (current-directory dir)
          (current-load-relative-directory dir))
        
        (set! user-namespace-box (make-weak-box (current-namespace)))
        
        (current-output-port (get-out-port))
        (current-error-port (get-err-port))
        (current-value-port (get-value-port))
        (current-input-port (get-in-box-port))
        
        (current-print (lambda (v) (display-results (list v)))))
      
      (define/private (initialize-dispatch-handler) ;;; =User=
        (let* ([primitive-dispatch-handler (event-dispatch-handler)])
          (event-dispatch-handler
           (letrec ([drscheme-event-dispatch-handler ; <= a name for #<...> printout
                     (λ (eventspace) ; =User=, =Handler=
                       ; Breaking is enabled if the user turned on breaks and
                       ;  is in a `yield'. If we get a break, that's ok, because
                       ;  the kernel never queues an event in the user's eventspace.
                       (cond
                         [(eq? eventspace (get-user-eventspace))
                          ; =User=, =Handler=
                          
                          ; We must distinguish between "top-level" events and
                          ;  those within `yield' in the user's program.
                          (cond
                            [(not in-evaluation?)
                             ;; at this point, we must not be in a nested dispatch, so we can
                             ;; just disable breaks and rely on call-with-break-parameterization
                             ;; to restore them to the user's setting.
                             (call-with-break-parameterization
                              no-breaks-break-parameterization
                              (λ ()
                                ; =No-Breaks=
                                (send context reset-offer-kill)
                                (send context set-breakables (get-user-thread) (get-user-custodian))
                                (call-with-continuation-prompt
                                 (λ () ; =User=, =Handler=, =No-Breaks=
                                   (call-with-break-parameterization
                                    user-break-parameterization
                                    (λ () (primitive-dispatch-handler eventspace)))))
                                
                                ;; in principle, the line below might cause
                                ;; "race conditions" in the GUI. That is, there might
                                ;; be many little events that the user won't quite
                                ;; be able to break.
                                (send context set-breakables #f #f)))]
                            [else
                             ; Nested dispatch; don't adjust interface
                             (primitive-dispatch-handler eventspace)])]
                         [else 
                          ; =User=, =Non-Handler=, =No-Breaks=
                          (primitive-dispatch-handler eventspace)]))])
             drscheme-event-dispatch-handler))))
      
      (define/public (new-empty-console)
        (queue-user/wait
         (λ () ; =User=, =No-Breaks=
           (send (drscheme:language-configuration:language-settings-language user-language-settings)
                 first-opened))))
      
      (define/public (reset-console)
        (when (thread? thread-killed)
          (kill-thread thread-killed))
        (send context clear-annotations)
        (drscheme:debug:hide-backtrace-window)
        (shutdown-user-custodian)
        (clear-input-port)
        (clear-box-input-port)
        (clear-output-ports)
        (set-allow-edits #t)
        
        ;; in case the last evaluation thread was killed, clean up some state.
        (lock #f)
        (set! in-evaluation? #f)
        (update-running #f)
        
        ;; clear out repl first before doing any work.
        (begin-edit-sequence)
        (set! prompt-position #f)
        (reset-input-box)
        (delete (paragraph-start-position 1) (last-position))
        (end-edit-sequence)
        
        ;; must init-evaluation-thread before determining
        ;; the language's name, since this updates user-language-settings
        (init-evaluation-thread)
        
        (begin-edit-sequence)
        (set-position (last-position) (last-position))
        
        (set! setting-up-repl? #t)
        (insert/delta this (string-append (string-constant language) ": ") welcome-delta)
        (let-values (((before after)
                      (insert/delta
                       this
                       (extract-language-name user-language-settings)
                       dark-green-delta
                       (extract-language-style-delta user-language-settings)))
                     ((url) (extract-language-url user-language-settings)))
          (when url
            (set-clickback before after (λ args (send-url url))
                           click-delta)))
        (unless (is-default-settings? user-language-settings)
          (insert/delta this (string-append " " (string-constant custom)) dark-green-delta))
        (when custodian-limit
          (insert/delta this 
                        "; memory limit: "
                        welcome-delta)
          (insert/delta this
                        (format "~a megabytes" (floor (/ custodian-limit 1024 1024)))
                        dark-green-delta))
        (insert/delta this ".\n" welcome-delta)
        
        (let ([osf (get-styles-fixed)])
          (set-styles-fixed #f)
          (send (drscheme:language-configuration:language-settings-language user-language-settings)
                extra-repl-information
                (drscheme:language-configuration:language-settings-settings user-language-settings)
                (open-output-text-editor this 'end))
          (set-styles-fixed osf))
        
        (set! setting-up-repl? #f)
        
        (set! already-warned? #f)
        (reset-regions (list (list (last-position) (last-position))))
        (set-unread-start-point (last-position))
        (set-insertion-point (last-position))
        (set-allow-edits #f)
        (set! repl-header-end #f)
        (end-edit-sequence))
      
      (define/public (initialize-console)
        (begin-edit-sequence)
        (freeze-colorer)
        (set! setting-up-repl? #t)
        (insert/delta this (string-append (string-constant welcome-to) " ") welcome-delta)
        (let-values ([(before after)
                      (insert/delta this 
                                    (string-constant drscheme)
                                    click-delta
                                    drs-font-delta)])
          (insert/delta this (format (string-append ", " (string-constant version) " ~a [~a].\n") 
                                     (version:version) (system-type 'gc))
                        welcome-delta)
          (set-clickback before after 
                         (λ args (drscheme:app:about-drscheme))
                         click-delta))
        (set! setting-up-repl? #f)
        (thaw-colorer)
        (send context disable-evaluation)
        (reset-console)
        
        (queue-user/wait
         (λ () ; =User=, =No-Breaks=
           (send (drscheme:language-configuration:language-settings-language user-language-settings)
                 first-opened)))
        
        (insert-prompt)
        (send context enable-evaluation)
        (end-edit-sequence)
        (clear-undos))
      
      ;; avoid calling paragraph-start-position very often.
      (define repl-header-end #f)
      (define/private (get-repl-header-end)
        (if repl-header-end
            repl-header-end
            (begin (set! repl-header-end (paragraph-start-position 2))
                   repl-header-end)))
      
      (define setting-up-repl? #f)
      (define/augment (can-change-style? start len)
        (and (inner #t can-change-style? start len)
             (or setting-up-repl?
                 (start . >= . (get-repl-header-end)))))
      
      (define/private (last-str l)
        (if (null? (cdr l))
            (car l)
            (last-str (cdr l))))
      
      (field (previous-expr-pos -1))
      
      (define/public (copy-previous-expr)
        (when prompt-position
          (let ([snip/strings (list-ref (get-previous-exprs) previous-expr-pos)])
            (begin-edit-sequence)
            (delete prompt-position (last-position) #f)
            (for-each (λ (snip/string)
                        (insert (if (is-a? snip/string snip%)
                                    (send snip/string copy)
                                    snip/string)
                                prompt-position))
                      snip/strings)
            (set-position (last-position))
            (end-edit-sequence))))
      
      (define/public (copy-next-previous-expr)
        (let ([previous-exprs (get-previous-exprs)])
          (unless (null? previous-exprs)
            (set! previous-expr-pos
                  (if (< (add1 previous-expr-pos) (length previous-exprs))
                      (add1 previous-expr-pos)
                      0))
            (copy-previous-expr))))
      (define/public (copy-prev-previous-expr)
        (let ([previous-exprs (get-previous-exprs)])
          (unless (null? previous-exprs)
            (set! previous-expr-pos
                  (if (previous-expr-pos . <= . 0)
                      (sub1 (length previous-exprs))
                      (sub1 previous-expr-pos)))
            (copy-previous-expr))))
      
      ;; private fields
      (define global-previous-exprs (preferences:get 'drscheme:console-previous-exprs))
      (define local-previous-exprs null)
      (define/private (get-previous-exprs)
        (append global-previous-exprs local-previous-exprs))
      (define/private (add-to-previous-exprs snips)
        (let* ([new-previous-exprs 
                (let* ([trimmed-previous-exprs (trim-previous-exprs local-previous-exprs)])
                  (let loop ([l trimmed-previous-exprs])
                    (if (null? l)
                        (list snips)
                        (cons (car l) (loop (cdr l))))))])
          (set! local-previous-exprs new-previous-exprs)))
      
      (define/private (trim-previous-exprs lst)
        (if ((length lst). >= .  console-max-save-previous-exprs)
            (cdr lst)
            lst))
      
      (define/private (save-interaction-in-history start end)
        (split-snip start)
        (split-snip end)
        (let ([snips
               (let loop ([snip (find-snip start 'after-or-none)]
                          [snips null])
                 (cond
                   [(not snip) snips]
                   [((get-snip-position snip) . <= . end)
                    (loop (send snip next)
                          (cons (send snip copy) snips))]
                   [else snips]))])
          (set! previous-expr-pos -1)
          (add-to-previous-exprs snips)))
      
      (define/public (reset-pretty-print-width)
        (let* ([standard (send (get-style-list) find-named-style "Standard")])
          (when standard
            (let* ([admin (get-admin)]
                   [width
                    (let ([bw (box 0)]
                          [b2 (box 0)])
                      (send admin get-view b2 b2 bw b2)
                      (unbox bw))]
                   [dc (send admin get-dc)]
                   [new-font (send standard get-font)]
                   [old-font (send dc get-font)])
              (send dc set-font new-font)
              (let* ([char-width (send dc get-char-width)]
                     [min-columns 50]
                     [new-columns (max min-columns 
                                       (floor (/ width char-width)))])
                (send dc set-font old-font)
                (pretty-print-columns new-columns))))))
      (super-new)
      (auto-wrap #t)
      (set-styles-sticky #f)
      
      (inherit set-max-undo-history)
      (set-max-undo-history 'forever)))
  
  (define (all-but-last lst)
    (let loop ([o lst])
      (cond
        [(null? o) null]
        [(null? (cdr o)) null]
        [else (cons (car o) (loop (cdr o)))])))
  
  (define input-delta (make-object style-delta%))
  (send input-delta set-delta-foreground (make-object color% 0 150 0))
  
  ;; insert-error-in-text : (is-a?/c text%)
  ;;                        (union #f (is-a?/c drscheme:rep:text<%>))
  ;;                        string?
  ;;                        exn?
  ;;                        (union false? (and/c string? directory-exists?))
  ;;                        ->
  ;;                        void?
  (define (insert-error-in-text text interactions-text msg exn user-dir)
    (insert-error-in-text/highlight-errors
     text
     (λ (l) (send interactions-text highlight-errors l))
     msg
     exn
     user-dir))
  
  ;; insert-error-in-text/highlight-errors : (is-a?/c text%)
  ;;                                         ((listof (list text% number number)) -> void)
  ;;                                         string?
  ;;                                         exn?
  ;;                                         (union false? (and/c string? directory-exists?))
  ;;                                         ->
  ;;                                         void?
  (define (insert-error-in-text/highlight-errors text highlight-errors msg exn user-dir)
    (let ([locked? (send text is-locked?)]
          [insert-file-name/icon
           ;; insert-file-name/icon : string number number number number -> void
           (λ (source-name start span row col)
             (let ([range-spec
                    (cond
                      [(and row col)
                       (format ":~a:~a" row col)]
                      [start
                       (format "::~a" start)]
                      [else ""])])
               (cond
                 [(file-exists? source-name)
                  (let* ([normalized-name (normalize-path source-name)]
                         [short-name (if user-dir
                                         (find-relative-path user-dir normalized-name)
                                         source-name)])
                    (let-values ([(icon-start icon-end) (insert/delta text (send file-icon copy))]
                                 [(space-start space-end) (insert/delta text " ")]
                                 [(name-start name-end) (insert/delta text short-name)]
                                 [(range-start range-end) (insert/delta text range-spec)]
                                 [(colon-start colon-ent) (insert/delta text ": ")])
                      (when (number? start)
                        (send text set-clickback icon-start range-end
                              (λ (_1 _2 _3)
                                (open-file-and-highlight normalized-name
                                                         (- start 1) 
                                                         (if span
                                                             (+ start -1 span)
                                                             start)))))))]
                 [else
                  (insert/delta text source-name)
                  (insert/delta text range-spec)
                  (insert/delta text ": ")])))])
      (send text begin-edit-sequence)
      (send text lock #f)
      (cond
        [(exn:fail:syntax? exn)
         (for-each
          (λ (expr)
            (let ([src (and (syntax? expr) (syntax-source expr))]
                  [pos (and (syntax? expr) (syntax-position expr))]
                  [span (and (syntax? expr) (syntax-span expr))]
                  [col (and (syntax? expr) (syntax-column expr))]
                  [line (and (syntax? expr) (syntax-line expr))])
              (when (and (string? src)
                         (number? pos)
                         (number? span)
                         (number? line)
                         (number? col))
                (insert-file-name/icon src pos span line col))
              (insert/delta text (format "~a" (exn-message exn)) error-delta)
              (when (syntax? expr)
                (insert/delta text " in: ")
                (insert/delta text (format "~s" (syntax->datum expr)) error-text-style-delta))
              (insert/delta text "\n")
              (when (and (is-a? src text:basic%)
                         (number? pos)
                         (number? span))
                (highlight-errors (list (list src (- pos 1) (+ pos -1 span)))))))
          (exn:fail:syntax-exprs exn))]
        [(exn:fail:read? exn)
         '(let ([src (exn:read-source exn)]
                [pos (exn:read-position exn)]
                [span (exn:read-span exn)]
                [line (exn:read-line exn)]
                [col (exn:read-column exn)])
            (when (and (string? src)
                       (number? pos)
                       (number? span)
                       (number? line)
                       (number? col))
              (insert-file-name/icon src pos span line col))
            (insert/delta text (format "~a" (exn-message exn)) error-delta)
            (insert/delta text "\n")
            (when (and (is-a? src text:basic%)
                       (number? pos)
                       (number? span))
              (highlight-errors (list (list src (- pos 1) (+ pos -1 span))))))]
        [(exn? exn)
         (insert/delta text (format "~a" (exn-message exn)) error-delta)
         (insert/delta text "\n")]
        [else
         (insert/delta text "uncaught exception: " error-delta)
         (insert/delta text (format "~s" exn) error-delta)
         (insert/delta text "\n")])
      (send text lock locked?)
      (send text end-edit-sequence)))
  
  
  ;; open-file-and-highlight : string (union number #f) (union number #f)
  ;; =Kernel, =Handler=
  ;; opens the file named by filename. If position is #f,
  ;; doesn't highlight anything. If position is a number and other-position
  ;; is #f, highlights the range from position to the end of sexp.
  ;; if other-position is a number, highlights from position to 
  ;; other position.
  (define (open-file-and-highlight filename position other-position)
    (let ([file (handler:edit-file filename)])
      (when (and (is-a? file drscheme:unit:frame%)
                 position)
        (if other-position
            (send (send file get-interactions-text)
                  highlight-error
                  (send file get-definitions-text)
                  position
                  other-position)
            (send (send file get-interactions-text)
                  highlight-error/forward-sexp
                  (send file get-definitions-text)
                  position)))))
  
  (define drs-autocomplete-mixin
    (λ (get-defs x)
      (class (text:autocomplete-mixin x)
        (define/override (get-all-words)
          (let* ([definitions-text (get-defs this)]
                 [settings (send definitions-text get-next-settings)]
                 [language (drscheme:language-configuration:language-settings-language settings)])
            (send language capability-value 'drscheme:autocomplete-words)))
        (super-new))))
  
  (define -text% 
    (drs-bindings-keymap-mixin
     (text-mixin 
      (text:ports-mixin
       (scheme:text-mixin
        (color:text-mixin
         (text:info-mixin
          (editor:info-mixin
           (text:searching-mixin
            (mode:host-text-mixin
             (drs-autocomplete-mixin 
              (λ (txt) (send txt get-definitions-text))
              (text:foreground-color-mixin
               text:clever-file-format%)))))))))))))
