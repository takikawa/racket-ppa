#lang scheme/base
#|

closing:
  warning messages don't have frame as parent.....

tab panels new behavior:
  - save all tabs (pr 6689?)
  
module browser threading seems wrong.

|#

  (require scheme/contract
           scheme/unit
           scheme/class
           scheme/path
           scheme/port
           scheme/list
           string-constants
           framework
           mrlib/name-message
           mrlib/bitmap-label
           mrlib/include-bitmap
           mrlib/switchable-button
           mrlib/cache-image-snip
           (prefix-in image-core: mrlib/image-core)
           mrlib/include-bitmap
           mrlib/close-icon
           net/sendurl
           net/url
           
           "drsig.ss"
           "auto-language.ss"
           "insert-large-letters.ss"
           (prefix-in drscheme:arrow: "../arrow.ss")
           
           mred
           (prefix-in mred: mred)
           
           mzlib/date)
  
  (provide unit@)
  
  (define module-browser-progress-constant (string-constant module-browser-progress))
  (define status-compiling-definitions (string-constant module-browser-compiling-defns))
  (define show-lib-paths (string-constant module-browser-show-lib-paths/short))
  (define show-planet-paths (string-constant module-browser-show-planet-paths/short))
  (define refresh (string-constant module-browser-refresh))

  (define define-button-long-label "(define ...)")
  
  (define-unit unit@
    (import [prefix help-desk: drscheme:help-desk^]
            [prefix drscheme:app: drscheme:app^]
            [prefix drscheme:frame: drscheme:frame^]
            [prefix drscheme:text: drscheme:text^]
            [prefix drscheme:rep: drscheme:rep^]
            [prefix drscheme:language-configuration: drscheme:language-configuration/internal^]
            [prefix drscheme:language: drscheme:language^]
            [prefix drscheme:get/extend: drscheme:get/extend^]
            [prefix drscheme:module-overview: drscheme:module-overview^]
            [prefix drscheme:tools: drscheme:tools^]
            [prefix drscheme:eval: drscheme:eval^]
            [prefix drscheme:init: drscheme:init^]
            [prefix drscheme:module-language: drscheme:module-language^]
            [prefix drscheme:module-language-tools: drscheme:module-language-tools^]
            [prefix drscheme:modes: drscheme:modes^]
            [prefix drscheme:debug: drscheme:debug^])
    (export (rename drscheme:unit^
                    [-frame% frame%]
                    [-frame<%> frame<%>]))
    
    (define-local-member-name
      get-visible-defs
      set-visible-defs
      set-focus-d/i
      get-i
      set-i)
    (define tab<%>
      (interface (drscheme:rep:context<%>)
        get-frame
        get-defs
        get-ints
        get-visible-defs
        set-visible-defs
        set-visible-ints
        set-focus-d/i
        get-i
        set-i
        break-callback
        is-current-tab?
        get-enabled
        on-close
        can-close?
        toggle-log))
    
    (define definitions-text<%> 
      (interface ()
        begin-metadata-changes
        end-metadata-changes
        get-tab
        get-next-settings
        after-set-next-settings
        set-needs-execution-message))
    
    (define-struct teachpack-callbacks 
      (get-names   ;; settings -> (listof string)
       add ;; settings path -> settings
       remove  ;; string[returned from teachpack-names] settings -> settings
       remove-all ;; settings -> settings
       ))
    
    ;; get rid of set-user-teachpack-cache method
    
    (keymap:add-to-right-button-menu
     (let ([old (keymap:add-to-right-button-menu)])
       (λ (menu text event)
         (old menu text event)
         (when (and (is-a? text text%)
                    (or (is-a? text (get-definitions-text%))
                        (is-a? text drscheme:rep:text%))
                    (is-a? event mouse-event%))
           
           (let ([add-sep
                  (let ([added? #f])
                    (λ ()
                      (unless added?
                        (set! added? #t)
                        (new separator-menu-item% [parent menu]))))])
             
             (let* ([end (send text get-end-position)]
                    [start (send text get-start-position)])
               (unless (= 0 (send text last-position))
                 (let* ([str (if (= end start)
                                 (find-symbol
                                  text
                                  (call-with-values
                                   (λ ()
                                     (send text dc-location-to-editor-location
                                           (send event get-x)
                                           (send event get-y)))
                                   (λ (x y)
                                     (send text find-position x y))))
                                 (send text get-text start end))]
                        ;; almost the same code as "search-help-desk" in "rep.ss"
                        [l (send text get-canvas)]
                        [l (and l (send l get-top-level-window))]
                        [l (and l (is-a? l -frame<%>) (send l get-definitions-text))]
                        [l (and l (send l get-next-settings))]
                        [l (and l (drscheme:language-configuration:language-settings-language l))]
                        [ctxt (and l (send l capability-value 'drscheme:help-context-term))]
                        [name (and l (send l get-language-name))])
                   (unless (string=? str "")
                     (add-sep)
                     (let ([short-str (shorten-str str 50)])
                       (make-object menu-item%
                         (gui-utils:format-literal-label
                          (string-constant search-help-desk-for) 
                          (if (equal? short-str str)
                              str
                              (string-append short-str "...")))
                         menu
                         (λ x (help-desk:help-desk str (list ctxt name)))))))))
           
           (when (is-a? text editor:basic<%>)
             (let-values ([(pos text) (send text get-pos/text event)])
               (when (and pos (is-a? text text%))
                 (send text split-snip pos)
                 (send text split-snip (+ pos 1))
                 (let ([snip (send text find-snip pos 'after-or-none)])
                   (when (or (is-a? snip image-snip%)
                             (is-a? snip image-core:image%)
                             (is-a? snip cache-image-snip%))
                     (add-sep)
                     (new menu-item%
                          [parent menu]
                          [label (string-constant save-image)]
                          [callback
                           (λ (_1 _2)
                             (let ([fn (put-file #f 
                                                 (send text get-top-level-window)
                                                 #f "untitled.png" "png")])
                               (when fn
                                 (let ([kind (filename->kind fn)])
                                   (cond
                                     [kind
                                      (cond
                                        [(or (is-a? snip image-snip%)
                                             (is-a? snip cache-image-snip%))
                                         (send (send snip get-bitmap) save-file fn kind)]
                                        [else
                                         (image-core:save-image-as-bitmap snip fn kind)])]
                                     [else
                                      (message-box 
                                       (string-constant drscheme)
                                       "Must choose a filename that ends with either .png, .jpg, .xbm, or .xpm")])))))]))))))
           
           (void))))))
    
    (define (filename->kind fn)
      (let ([ext (filename-extension fn)])
        (and ext
             (let ([sym (string->symbol (bytes->string/utf-8 ext))])
               (ormap (λ (pr) (and (eq? sym (car pr)) (cadr pr)))
                      allowed-extensions)))))
    
    (define allowed-extensions '((png png)
                                 (jpg jpeg)
                                 (xbm xbm)
                                 (xpm xpm)))
    
    
    
    ;; find-symbol : number -> string
    ;; finds the symbol around the position `pos' (approx)
    (define (find-symbol text pos)
      (send text split-snip pos)
      (send text split-snip (+ pos 1))
      (let ([snip (send text find-snip pos 'after)])
        (if (is-a? snip string-snip%)
            (let* ([before
                    (let loop ([i (- pos 1)]
                               [chars null])
                      (if (< i 0)
                          chars
                          (let ([char (send text get-character i)])
                            (if (non-letter? char)
                                chars
                                (loop (- i 1)
                                      (cons char chars))))))]
                   [after
                    (let loop ([i pos])
                      (if (< i (send text last-position))
                          (let ([char (send text get-character i)])
                            (if (non-letter? char)
                                null
                                (cons char (loop (+ i 1)))))
                          null))])
              (apply string (append before after)))
            "")))
    
    ;; non-letter? : char -> boolean
    ;; returns #t if the character belongs in a symbol (approx) and #f it is
    ;; a divider between symbols (approx)
    (define (non-letter? x)
      (or (char-whitespace? x)
          (memq x '(#\` #\' #\, #\; #\"
                        #\{ #\( #\[ #\] #\) #\}))))      
    (define (shorten-str str len)
      (if ((string-length str) . <= . len)
          str
          (substring str 0 len)))
    
    
    ;                                                                                              
    ;                                                                                              
    ;                                                                                              
    ;    ;;;                         ;                           ;   ;          ;                  
    ;   ;                                                        ;              ;                  
    ;   ;                       ;                                ;              ;                  
    ;  ;;;;  ; ;  ;;;     ;;;  ;;;;  ;    ;;;    ; ;;         ;; ;   ;   ;;;    ;    ;;;     ;; ;  
    ;   ;    ;;  ;   ;   ;   ;  ;    ;   ;   ;   ;;  ;       ;  ;;   ;  ;   ;   ;   ;   ;   ;  ;;  
    ;   ;    ;       ;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;      ;   ;  ;     ; ;    ;  
    ;   ;    ;    ;;;;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;   ;;;;   ;  ;     ; ;    ;  
    ;   ;    ;   ;   ;  ;       ;    ;  ;     ;  ;   ;      ;    ;   ;  ;   ;   ;  ;     ; ;    ;  
    ;   ;    ;   ;   ;   ;   ;  ;    ;   ;   ;   ;   ;       ;  ;;   ;  ;   ;   ;   ;   ;   ;  ;;  
    ;   ;    ;    ;;;;;   ;;;    ;;  ;    ;;;    ;   ;        ;; ;   ;   ;;;;;  ;    ;;;     ;; ;  
    ;                                                                                           ;  
    ;                                                                                      ;    ;  
    ;                                                                                       ;;;;   
    
    (define (get-fraction-from-user parent)
      (let* ([dlg (make-object dialog% (string-constant enter-fraction))]
             [hp (make-object horizontal-panel% dlg)]
             [_1 (make-object message% (string-constant whole-part) hp)]
             [whole (make-object text-field% #f hp void)]
             [vp (make-object vertical-panel% hp)]
             [hp2 (make-object horizontal-panel% vp)]
             [num (make-object text-field% #f hp2 void)]
             [num-m (make-object message% (string-constant numerator) hp2)]
             [hp3 (make-object horizontal-panel% vp)]
             [den (make-object text-field% #f hp3 void)]
             [den-m (make-object message% (string-constant denominator) hp3)]
             [bp (make-object horizontal-panel% dlg)]
             [ok? #f]
             [validate-number
              (λ ()
                (let ([num-s (string->number (send num get-value))]
                      [den-s (string->number (send den get-value))]
                      [whole-s (if (string=? (send whole get-value) "")
                                   0
                                   (string->number (send whole get-value)))])
                  (cond
                    [(or (not whole-s) (not (integer? whole-s)))
                     (string-constant insert-number/bad-whole-part)]
                    [(or (not num-s) (not (integer? num-s)) (< num-s 0))
                     (string-constant insert-number/bad-numerator)]
                    [(or (not den-s) (not (integer? den-s)) (<= den-s 0))
                     (string-constant insert-number/bad-denominator)]
                    [else
                     (if (< whole-s 0)
                         (- whole-s (/ num-s den-s))
                         (+ whole-s (/ num-s den-s)))])))]
             [ok-callback
              (λ () 
                (let ([v (validate-number)])
                  (cond
                    [(number? v)
                     (set! ok? #t)
                     (send dlg show #f)]
                    [else 
                     (message-box
                      (string-constant drscheme)
                      v
                      dlg)])))]
             [cancel-callback 
              (λ () (send dlg show #f))])
        (let-values ([(ok cancel) 
                      (gui-utils:ok/cancel-buttons
                       bp
                       (λ (x y) (ok-callback))
                       (λ (x y) (cancel-callback)))])
          (let ([mw (max (send den-m get-width) (send num-m get-width))])
            (send den-m min-width mw)
            (send num-m min-width mw))
          (send bp set-alignment 'right 'center)
          (send dlg show #t)
          (and ok?
               (let ([v (validate-number)])
                 (and (number? v)
                      v))))))
    
    ;; create-executable : (instanceof drscheme:unit:frame<%>) -> void
    (define (create-executable frame)
      (let* ([definitions-text (send frame get-definitions-text)]
             [program-filename (send definitions-text get-filename)])
        (cond
          [(not program-filename)
           (message-box (string-constant create-executable-title)
                        (string-constant must-save-before-executable)
                        frame)]
          [else
           (when (or (not (send definitions-text is-modified?))
                     (gui-utils:get-choice
                      (string-constant definitions-not-saved)
                      (string-constant yes)
                      (string-constant no)
                      (string-constant drscheme)
                      #f
                      frame))
             (let ([settings (send definitions-text get-next-settings)])
               (send (drscheme:language-configuration:language-settings-language settings)
                     create-executable
                     (drscheme:language-configuration:language-settings-settings settings)
                     frame
                     program-filename)))])))
    
    (define execute-bitmap (make-object bitmap% (build-path (collection-path "icons") "run.png") 'png/mask))
    (define break-bitmap (make-object bitmap% (build-path (collection-path "icons") "break.png") 'png/mask))
    (define save-bitmap (make-object bitmap% (build-path (collection-path "icons") "save.png") 'png/mask))
    
    (define-values (get-program-editor-mixin add-to-program-editor-mixin)
      (let* ([program-editor-mixin
              (mixin (editor:basic<%> (class->interface text%)) () 
                (init-rest args) 
                (inherit get-top-level-window) 
                
                (define/private (reset-highlighting) 
                  (let ([f (get-top-level-window)]) 
                    (when (and f 
                               (is-a? f -frame<%>)) 
                      (let ([interactions-text (send f get-interactions-text)]) 
                        (when (object? interactions-text) 
                          (send interactions-text reset-highlighting)))))) 
                                
                (define/augment (after-insert x y) 
                  (reset-highlighting) 
                  (inner (void) after-insert x y)) 
                
                (define/augment (after-delete x y) 
                  (reset-highlighting) 
                  (inner (void) after-delete x y)) 
                
                (apply super-make-object args))]
             [get-program-editor-mixin
              (λ ()
                (drscheme:tools:only-in-phase 'drscheme:unit:get-program-editor-mixin 'phase2 'init-complete)
                program-editor-mixin)]
             [add-to-program-editor-mixin
              (λ (mixin)
                (drscheme:tools:only-in-phase 'drscheme:unit:add-to-program-editor-mixin 'phase1)
                (let ([old program-editor-mixin])
                  (set! program-editor-mixin (λ (x) (mixin (old x))))))])
        (values get-program-editor-mixin
                add-to-program-editor-mixin)))
    
    ;; this sends a message to it's frame when it gets the focus
    (define make-searchable-canvas%
      (λ (%)
        (class %
          (inherit get-top-level-window)
          (define/override (on-focus on?)
            (when on?
              (send (get-top-level-window) make-searchable this))
            (super on-focus on?))
          (super-new))))
    
    (define interactions-canvas% 
      (class (make-searchable-canvas%
              (canvas:info-mixin
               (canvas:wide-snip-mixin
                (canvas:info-mixin
                 canvas:color%))))
        (init [style '()])
        (super-new (style (cons 'auto-hscroll style)))))
    
    
    (define definitions-canvas%
      (class (make-searchable-canvas% (canvas:delegate-mixin (canvas:info-mixin canvas:color%)))
        (init [style '()])
        (super-new (style (cons 'auto-hscroll style)))))
    
    ;                                                                                                  
    ;                                                                                                  
    ;                                                                                                  
    ;       ;           ;;;            ;        ;                                                      
    ;       ;          ;    ;                                                                          
    ;       ;          ;                   ;                                   ;                   ;   
    ;    ;; ;    ;;;  ;;;;;;;  ; ;;    ;  ;;;;  ;    ;;;    ; ;;     ;;;      ;;;;   ;;;  ;     ; ;;;; 
    ;   ;  ;;   ;   ;  ;    ;  ;;  ;   ;   ;    ;   ;   ;   ;;  ;   ;          ;    ;   ;  ;   ;   ;   
    ;  ;    ;  ;    ;  ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;   ;;         ;   ;    ;   ; ;    ;   
    ;  ;    ;  ;;;;;;  ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;    ;;        ;   ;;;;;;    ;     ;   
    ;  ;    ;  ;       ;    ;  ;   ;   ;   ;    ;  ;     ;  ;   ;      ;       ;   ;        ; ;    ;   
    ;   ;  ;;   ;      ;    ;  ;   ;   ;   ;    ;   ;   ;   ;   ;      ;       ;    ;      ;   ;   ;   
    ;    ;; ;    ;;;;  ;    ;  ;   ;   ;    ;;  ;    ;;;    ;   ;   ;;;         ;;   ;;;; ;     ;   ;; 
    ;                                                                                                  
    ;                                                                                                  
    ;                                                                                                  
    
    
    (define get-definitions-text%
      (let ([definitions-text% #f])
        (λ ()
          (drscheme:tools:only-in-phase 'phase2 'init-complete)
          (unless definitions-text%
            (set! definitions-text% (make-definitions-text%)))
          definitions-text%)))

    (define (make-definitions-text%)
      (let ([definitions-super%
              ((get-program-editor-mixin)
               (text:first-line-mixin
                (drscheme:module-language:module-language-put-file-mixin
                 (scheme:text-mixin
                  (color:text-mixin
                   (drscheme:rep:drs-bindings-keymap-mixin
                    (mode:host-text-mixin
                     (text:delegate-mixin
                      (text:foreground-color-mixin
                       (drscheme:rep:drs-autocomplete-mixin
                        (λ (x) x)
                        (text:normalize-paste-mixin
                         text:info%)))))))))))])
        (class* definitions-super% (definitions-text<%>)
          (inherit get-top-level-window is-locked? lock while-unlocked highlight-first-line)
          
          (define interactions-text #f)
          (define/public (set-interactions-text it)
            (set! interactions-text it))
          
          (define tab #f)
          (define/public (get-tab) tab)
          (define/public (set-tab t) (set! tab t))
          
          (inherit get-surrogate set-surrogate)
          (define/public (set-current-mode mode)
            (let ([surrogate (drscheme:modes:mode-surrogate mode)])
              (set-surrogate surrogate)
              (when interactions-text
                (send interactions-text set-surrogate surrogate)
                (send interactions-text set-submit-predicate
                      (drscheme:modes:mode-repl-submit mode)))))
          
          (define/public (is-current-mode? mode)
            (let ([surrogate (drscheme:modes:mode-surrogate mode)])
              (eq? surrogate (get-surrogate))))
          
          (define/public (change-mode-to-match)
            (let* ([language-settings (get-next-settings)]
                   [language-name (and language-settings
                                       (send (drscheme:language-configuration:language-settings-language
                                              language-settings)
                                             get-language-position))])
              (let loop ([modes (drscheme:modes:get-modes)])
                (cond
                  [(null? modes) (error 'change-mode-to-match
                                        "didn't find a matching mode")]
                  [else (let ([mode (car modes)])
                          (if ((drscheme:modes:mode-matches-language mode) language-name)
                              (unless (is-current-mode? mode)
                                (set-current-mode mode))
                              (loop (cdr modes))))]))))
          
          (inherit begin-edit-sequence end-edit-sequence
                   delete insert last-position paragraph-start-position
                   get-character)
          
          (define save-file-metadata #f)
          
          (define/pubment (begin-metadata-changes)
            (set! ignore-edits? #t)
            (inner (void) begin-metadata-changes))
          (define/pubment (end-metadata-changes)
            (set! ignore-edits? #f)
            (inner (void) end-metadata-changes))
          
          (define/augment (on-save-file filename fmt)
            (inner (void) on-save-file filename fmt)
            (let* ([lang (drscheme:language-configuration:language-settings-language next-settings)]
                   [settings (drscheme:language-configuration:language-settings-settings next-settings)]
                   [name-mod (send lang get-reader-module)])
              (when name-mod ;; the reader-module method's result is used a test of whether or not the get-metadata method is used for this language
                (let ([metadata (send lang get-metadata (filename->modname filename) settings)])
                  (begin-edit-sequence #f)
                  (begin-metadata-changes)
                  (let ([locked? (is-locked?)])
                    (when locked? (lock #f))
                    (set! save-file-metadata metadata)
                    (while-unlocked
                     (λ ()
                       (insert metadata 0 0)))
                    (when locked? (lock #t)))))))
          (define/private (filename->modname filename)
            (let-values ([(base name dir) (split-path filename)])
              (string->symbol (regexp-replace #rx"\\.[^.]*$"
                                              (path->string name)
                                              ""))))
          
          (define/augment (after-save-file success?)
            (when success?
              (let ([filename (get-filename)])
                (when filename
                  ;; if a filesystem error happens, just give up
                  ;; on setting the file creator and type.
                  (with-handlers ([exn:fail:filesystem? void])
                    (let-values ([(creator type) (file-creator-and-type filename)])
                      (file-creator-and-type filename #"DrSc" type))))))
            (when save-file-metadata
              (let ([modified? (is-modified?)]
                    [locked? (is-locked?)])
                (when locked? (lock #f))
                (while-unlocked
                 (λ ()
                   (delete 0 (string-length save-file-metadata))))
                (when locked? (lock #t))
                (set! save-file-metadata #f)
                ;; restore modification status to where it was before the metadata is removed
                (set-modified modified?)
                (end-metadata-changes)
                (end-edit-sequence)))
            (inner (void) after-save-file success?))
          
          (define/augment (on-load-file filename format)
            (inner (void) on-load-file filename format)
            (begin-edit-sequence #f))
          (define/augment (after-load-file success?)
            (when success?
              (let-values ([(module-language module-language-settings)
                            (get-module-language/settings)])
                (let-values ([(matching-language settings)
                              (pick-new-language
                               this
                               (drscheme:language-configuration:get-languages)
                               module-language
                               module-language-settings)])
                  (when matching-language
                    (set-next-settings
                     (drscheme:language-configuration:make-language-settings 
                      matching-language
                      settings)
                     #f))))
              (set-modified #f))
            
            (end-edit-sequence)
            (inner (void) after-load-file success?))
          
          (inherit is-modified? run-after-edit-sequence)
          (define/override (set-modified mod?)
            (super set-modified mod?)
            (run-after-edit-sequence
             (λ ()
               (let ([f (get-top-level-window)])
                 (when (and f
                            (is-a? f -frame<%>))
                   (send f update-save-button))))))
          (define/override set-filename
            (case-lambda
              [(fn) (set-filename fn #f)]
              [(fn tmp?)
               (super set-filename fn tmp?)
               (let ([f (get-top-level-window)])
                 (when (and f
                            (is-a? f -frame<%>))
                   (send f update-save-message)))]))
          
          (field
           [needs-execution-state #f]
           [already-warned-state #f]
           [execute-settings (preferences:get drscheme:language-configuration:settings-preferences-symbol)]
           [next-settings execute-settings])
          
          (define/private (set-needs-execution-state! s) (set! needs-execution-state s))
          
          ;; get-needs-execution-message : -> (or/c string #f)
          ;; returns the current warning message if "Run" should be clicked (ie, if the
          ;; state of the REPL is out of sync with drscheme).
          (define/public (get-needs-execution-message)
            (or (and (not (this-and-next-language-the-same?))
                     (string-constant needs-execute-language-changed))
                needs-execution-state))
          
          (define/pubment (get-next-settings) next-settings)
          (define/pubment (set-next-settings _next-settings [update-prefs? #t])
            (when (or (send (drscheme:language-configuration:language-settings-language _next-settings)
                            get-reader-module)
                      (send (drscheme:language-configuration:language-settings-language next-settings)
                            get-reader-module))
              (set-modified #t))
            (set! next-settings _next-settings)
            (change-mode-to-match)
            (let ([f (get-top-level-window)])
              (when (and f
                         (is-a? f -frame<%>))
                (send f language-changed)))
            
            (highlight-first-line
             (is-a? (drscheme:language-configuration:language-settings-language _next-settings)
                    drscheme:module-language:module-language<%>))
            
            (let ([lang (drscheme:language-configuration:language-settings-language next-settings)]
                  [sets (drscheme:language-configuration:language-settings-settings next-settings)])
              (preferences:set
               'drscheme:recent-language-names
               (limit-length
                (remove-duplicate-languages
                 (cons (cons (send lang get-language-name)
                             (send lang marshall-settings sets))
                       (preferences:get 'drscheme:recent-language-names)))
                10)))
            
            (when update-prefs?
              (preferences:set
               drscheme:language-configuration:settings-preferences-symbol
               next-settings))
            
            (remove-auto-text)
            (insert-auto-text)
            (after-set-next-settings _next-settings))
          
          (define/pubment (after-set-next-settings s)
            (inner (void) after-set-next-settings s))
          
          (define/public (this-and-next-language-the-same?)
            (let ([execute-lang (drscheme:language-configuration:language-settings-language execute-settings)]
                  [next-lang (drscheme:language-configuration:language-settings-language next-settings)])
              (and (eq? execute-lang next-lang)
                   (equal?
                    (send execute-lang marshall-settings 
                          (drscheme:language-configuration:language-settings-settings execute-settings))
                    (send execute-lang marshall-settings 
                          (drscheme:language-configuration:language-settings-settings next-settings))))))
          
          (define/pubment (set-needs-execution-message msg)
            (set-needs-execution-state! msg))
          (define/pubment (teachpack-changed)
            (set-needs-execution-state! (string-constant needs-execute-teachpack-changed)))
          (define/pubment (just-executed)
            (set! execute-settings next-settings)
            (set-needs-execution-state! #f)
            (send tab clear-execution-state)
            (set! already-warned-state #f))
          (define/pubment (already-warned?)
            already-warned-state)
          (define/pubment (already-warned)
            (set! already-warned-state #t))

          (define really-modified? #f)
          (define ignore-edits? #f)

          (define/augment (after-insert x y)
            (unless ignore-edits?
              (set! really-modified? #t)
              (set-needs-execution-state! (string-constant needs-execute-defns-edited)))
            (inner (void) after-insert x y))
          (define/augment (after-delete x y)
            (unless ignore-edits?
              (set! really-modified? #t)
              (set-needs-execution-state! (string-constant needs-execute-defns-edited)))
            (inner (void) after-delete x y))
          
          (define/override (is-special-first-line? l) 
            (and (preferences:get 'drscheme:module-language-first-line-special?)
                 (is-lang-line? l)))
          
          (inherit get-filename)
          (field
           [tmp-date-string #f])
          
          (inherit get-filename/untitled-name)
          (define/private (get-date-string)
            (string-append
             (date->string (seconds->date (current-seconds)))
             " "
             (get-filename/untitled-name)))
          
          (define/override (on-paint before dc left top right bottom dx dy draw-caret)
            (when (and before
                       (or (is-a? dc post-script-dc%)
                           (is-a? dc printer-dc%)))
              (set! tmp-date-string (get-date-string))
              (let-values ([(w h d s) (send dc get-text-extent tmp-date-string)])
                (send (current-ps-setup) set-editor-margin 0 (inexact->exact (ceiling h)))))
            (super on-paint before dc left top right bottom dx dy draw-caret)
            (when (and (not before)
                       (or (is-a? dc post-script-dc%)
                           (is-a? dc printer-dc%)))
              (send dc draw-text (get-date-string) 0 0)
              (void))
            
            ;; draw the arrows
            (when before
              (when error-arrows
                (let ([old-pen (send dc get-pen)])
                  (send dc set-pen (send the-pen-list find-or-create-pen "red" 1 'solid))
                  (let loop ([pts error-arrows])
                    (cond
                      [(null? pts) (void)]
                      [(null? (cdr pts)) (void)]
                      [else (let ([pt1 (car pts)]
                                  [pt2 (cadr pts)])
                              (draw-arrow dc dx dy pt1 pt2)
                              (loop (cdr pts)))]))
                  (send dc set-pen old-pen)))))
          
          (define/private (draw-arrow dc dx dy pt1 pt2)
            (let-values ([(x1 y1) (find-poss (srcloc-source pt1) (- (srcloc-position pt1) 1) (srcloc-position pt1))]
                         [(x2 y2) (find-poss (srcloc-source pt2) (- (srcloc-position pt2) 1) (srcloc-position pt2))])
              (drscheme:arrow:draw-arrow dc x1 y1 x2 y2 dx dy)))
          
          (inherit dc-location-to-editor-location)
          (define/private (find-poss text left-pos right-pos)
            (let ([xlb (box 0)]
                  [ylb (box 0)]
                  [xrb (box 0)]
                  [yrb (box 0)])
              (send text position-location left-pos xlb ylb #t)
              (send text position-location right-pos xrb yrb #f)
              (let*-values ([(xl-off yl-off) (send text editor-location-to-dc-location (unbox xlb) (unbox ylb))]
                            [(xl yl) (dc-location-to-editor-location xl-off yl-off)]
                            [(xr-off yr-off) (send text editor-location-to-dc-location (unbox xrb) (unbox yrb))]
                            [(xr yr) (dc-location-to-editor-location xr-off yr-off)])
                (values (/ (+ xl xr) 2)
                        (/ (+ yl yr) 2)))))

          (define/public (still-untouched?)
            (and (or (= (last-position) 0) (not really-modified?))
                 (not (is-modified?))
                 (not (get-filename))))
          ;; inserts the auto-text if any, and executes the text if so
          (define/private (insert-auto-text)
            (define lang
              (drscheme:language-configuration:language-settings-language
               next-settings))
            (define auto-text
              (and (not really-modified?)
                   (not (get-filename))
                   (is-a? lang drscheme:module-language:module-language<%>)
                   (send lang get-auto-text
                         (drscheme:language-configuration:language-settings-settings
                          next-settings))))
            (when auto-text
              (begin-edit-sequence #f)
              (insert auto-text)
              (set-modified #f)
              (end-edit-sequence)
              (set! really-modified? #f)
              ;; HACK: click run; would be better to override on-execute and
              ;; make it initialize a working repl, but the problem is that
              ;; doing that in module-language.ss means that it'll either need
              ;; to find if the current text is the auto-text and analyze it to
              ;; get this initialization, or it will need to do that for all
              ;; possible contents, which means that it'll work when opening
              ;; exiting files too (it might be feasible once we have a #lang
              ;; parser).
              (send (get-top-level-window) execute-callback)))
          (define/private (remove-auto-text)
            (when (and (not really-modified?)
                       (not (get-filename))
                       (> (last-position) 0))
              (begin-edit-sequence #f)
              (send this erase)
              (set-modified #f)
              (end-edit-sequence)
              (set! really-modified? #f)))
          
          (inherit invalidate-bitmap-cache)
          (define/public (set-error-arrows arrows)
            (set! error-arrows arrows)
            (invalidate-bitmap-cache))
          
          (define error-arrows #f)
          
          (super-new)
          
          ;; insert the default-text
          (queue-callback (lambda () (insert-auto-text)))
          (highlight-first-line
           (is-a? (drscheme:language-configuration:language-settings-language next-settings)
                  drscheme:module-language:module-language<%>))
          (inherit set-max-undo-history)
          (set-max-undo-history 'forever))))
    
    ;; is-lang-line? : string -> boolean
    ;; given the first line in the editor, this returns #t if it is a #lang line.
    (define (is-lang-line? l)
      (let ([m (regexp-match #rx"^#(!|(lang ))([-+_/a-zA-Z0-9]+)(.|$)" l)])
        (and m
             (let ([lang-name (list-ref m 3)]
                   [last-char (list-ref m 4)])
               (and (not (char=? #\/ (string-ref lang-name 0)))
                    (not (char=? #\/ (string-ref lang-name (- (string-length lang-name) 1))))
                    (or (string=? "" last-char)
                        (char-whitespace? (string-ref last-char 0))))))))
    
    ;; test cases for is-lang-line?
    #;
    (list (is-lang-line? "#lang x")
          (is-lang-line? "#lang scheme")
          (is-lang-line? "#lang scheme ")
          (not (is-lang-line? "#lang schemeα"))
          (not (is-lang-line? "#lang scheme/ "))
          (not (is-lang-line? "#lang /scheme "))
          (is-lang-line? "#lang sch/eme ")
          (is-lang-line? "#lang r6rs")
          (is-lang-line? "#!r6rs")
          (is-lang-line? "#!r6rs ")
          (not (is-lang-line? "#!/bin/sh")))
    
    (define (get-module-language/settings)
      (let* ([module-language
              (and (preferences:get 'drscheme:switch-to-module-language-automatically?)
                   (ormap 
                    (λ (lang)
                      (and (is-a? lang drscheme:module-language:module-language<%>)
                           lang))
                    (drscheme:language-configuration:get-languages)))]
             [module-language-settings
              (let ([prefs-setting (preferences:get 
                                    drscheme:language-configuration:settings-preferences-symbol)])
                (cond
                  [(eq? (drscheme:language-configuration:language-settings-language prefs-setting)
                        module-language)
                   (drscheme:language-configuration:language-settings-settings prefs-setting)]
                  [else 
                   (and module-language
                        (send module-language default-settings))]))])
        (values module-language module-language-settings)))
    
    
    

;                                                       
;                                                       
;                                                       
;                                                       
;      ;;;          ;;;;;;;                             
;      ;;;         ;;;                                  
;   ;; ;;;   ;;;; ;;;;; ;;; ;;; ;;    ;;;;              
;  ;;;;;;;  ;; ;;;;;;;; ;;; ;;;;;;;  ;; ;;;             
;  ;;; ;;; ;;; ;;; ;;;  ;;; ;;; ;;; ;;; ;;;             
;  ;;; ;;; ;;;;;;; ;;;  ;;; ;;; ;;; ;;;;;;;             
;  ;;; ;;; ;;;     ;;;  ;;; ;;; ;;; ;;;     ;;; ;;; ;;; 
;  ;;;;;;;  ;;;;;; ;;;  ;;; ;;; ;;;  ;;;;;; ;;; ;;; ;;; 
;   ;; ;;;   ;;;;  ;;;  ;;; ;;; ;;;   ;;;;  ;;; ;;; ;;; 
;                                                       
;                                                       
;                                                       
;                                                       
    
    ;; get-pos : text mouse-event% -> (union #f number)
    (define (get-pos text event)
      (let*-values ([(event-x event-y)
                     (values (send event get-x)
                             (send event get-y))]
                    [(x y) (send text dc-location-to-editor-location
                                 event-x 
                                 event-y)])
        (let* ([on-it? (box #f)]
               [pos (send text find-position x y #f on-it?)])
          (and (unbox on-it?)
               pos))))
    
    (let ([old (keymap:add-to-right-button-menu)])
      (keymap:add-to-right-button-menu
       (λ (menu editor event)
         (when (is-a? editor text%)
           (let* ([canvas (send editor get-canvas)]
                  [frame (and canvas (send canvas get-top-level-window))])
             (when (is-a? frame -frame<%>)
               (let* ([language-settings (send (send frame get-definitions-text) get-next-settings)]
                      [new-language (drscheme:language-configuration:language-settings-language language-settings)]
                      [capability-info (send new-language capability-value 'drscheme:define-popup)])
                 (when capability-info
                   (let* ([current-pos (get-pos editor event)]
                          [current-word (and current-pos (get-current-word editor current-pos))]
                          [defn (and current-word
                                     (ormap (λ (defn) (and (string=? current-word (defn-name defn))
                                                           defn))
                                            (get-definitions (car capability-info)
                                                             #f
                                                             editor)))])
                     (when defn
                       (new separator-menu-item% (parent menu))
                       (new menu-item%
                            (parent menu)
                            (label (gui-utils:format-literal-label (string-constant jump-to-defn) (defn-name defn)))
                            (callback (λ (x y)
                                        (send editor set-position (defn-start-pos defn))))))))))))
         (old menu editor event))))
    
    ;; get-current-word : editor number -> string
    ;; returns the string that is being clicked on
    (define (get-current-word editor pos)
      (let* ([search
              (λ (dir offset)
                (let loop ([pos pos])
                  (cond
                    [(or (= pos 0) 
                         (= pos (send editor last-position)))
                     pos]
                    [(memq (send editor get-character pos) '(#\space #\return #\newline #\( #\) #\[ #\] #\tab))
                     (offset pos)]
                    [else (loop (dir pos))])))]
             [before (search sub1 add1)]
             [after (search add1 (λ (x) x))])
        (send editor get-text before after)))
    
    (define func-defs-canvas%
      (class name-message%
        (init-field frame)
        
        (unless (is-a? frame -frame<%>)
          (error 'func-defs-canvas "frame is not a drscheme:unit:frame<%>"))
        
        (define sort-by-name? (preferences:get 'drscheme:defns-popup-sort-by-name?))
        (define sorting-name (if sort-by-name?
                                 (string-constant sort-by-position) 
                                 (string-constant sort-by-name)))
        (define/private (change-sorting-order)
          (set! sort-by-name? (not sort-by-name?))
          (preferences:set 'drscheme:defns-popup-sort-by-name? sort-by-name?)
          (set! sorting-name (if sort-by-name?
                                 (string-constant sort-by-position) 
                                 (string-constant sort-by-name))))
        
        (define drscheme:define-popup-capability-info
          (drscheme:language:get-capability-default 'drscheme:define-popup))
        
        (inherit set-message set-hidden?)
        (define/public (language-changed new-language vertical?)
          (set! drscheme:define-popup-capability-info (send new-language capability-value 'drscheme:define-popup))
          (let ([define-name (get-drscheme:define-popup-name drscheme:define-popup-capability-info
                                                             vertical?)])
            (cond
              [define-name
                (set-message #f define-name)
                (set-hidden? #f)]
              [else
               (set-hidden? #t)])))
        (define/override (fill-popup menu reset)
          (when drscheme:define-popup-capability-info
            (let* ([text (send frame get-definitions-text)]
                   [unsorted-defns (get-definitions (car drscheme:define-popup-capability-info)
                                                    (not sort-by-name?)
                                                    text)]
                   [defns (if sort-by-name?
                              (sort
                               unsorted-defns
                               (λ (x y) (string-ci<=? (defn-name x) (defn-name y))))
                              unsorted-defns)])
              (make-object menu:can-restore-menu-item% sorting-name
                menu
                (λ (x y)
                  (change-sorting-order)))
              (make-object separator-menu-item% menu)
              (if (null? defns)
                  (send (make-object menu:can-restore-menu-item%
                          (string-constant no-definitions-found)
                          menu
                          void)
                        enable #f)
                  (let loop ([defns defns])
                    (unless (null? defns)
                      (let* ([defn (car defns)]
                             [checked? 
                              (let ([t-start (send text get-start-position)]
                                    [t-end (send text get-end-position)]
                                    [d-start (defn-start-pos defn)]
                                    [d-end (defn-end-pos defn)])
                                (or (<= t-start d-start t-end)
                                    (<= t-start d-end t-end)
                                    (<= d-start t-start t-end d-end)))]
                             [item
                              (make-object (if checked?
                                               menu:can-restore-checkable-menu-item%
                                               menu:can-restore-menu-item%)
                                (gui-utils:quote-literal-label (defn-name defn))
                                
                                menu
                                (λ (x y)
                                  (reset)
                                  (send text set-position (defn-start-pos defn) (defn-start-pos defn))
                                  (let ([canvas (send text get-canvas)])
                                    (when canvas
                                      (send canvas focus)))))])
                        (when checked?
                          (send item check #t))
                        (loop (cdr defns)))))))))
        
        (super-new (label "(define ...)") ;; this default is quickly changed
                   [string-constant-untitled (string-constant untitled)]
                   [string-constant-no-full-name-since-not-saved 
                    (string-constant no-full-name-since-not-saved)])))
    
    ;; defn = (make-defn number string number number)
    (define-struct defn (indent name start-pos end-pos) #:mutable)
    
    ;; get-definitions : boolean text -> (listof defn)
    (define (get-definitions tag-string indent? text)
      (let* ([min-indent 0]
             [defs (let loop ([pos 0])
                     (let ([defn-pos (send text find-string tag-string 'forward pos 'eof #t #f)])
                       (cond
                         [(not defn-pos) null]
                         [(in-semicolon-comment? text defn-pos)
                          (loop (+ defn-pos (string-length tag-string)))]
                         [else
                          (let ([indent (get-defn-indent text defn-pos)]
                                [name (get-defn-name text (+ defn-pos (string-length tag-string)))])
                            (set! min-indent (min indent min-indent))
                            (cons (make-defn indent name defn-pos defn-pos)
                                  (loop (+ defn-pos (string-length tag-string)))))])))])
        
        ;; update end-pos's based on the start pos of the next defn
        (unless (null? defs)
          (let loop ([first (car defs)]
                     [defs (cdr defs)])
            (cond
              [(null? defs) 
               (set-defn-end-pos! first (send text last-position))]
              [else (set-defn-end-pos! first (max (- (defn-start-pos (car defs)) 1)
                                                  (defn-start-pos first)))
                    (loop (car defs) (cdr defs))])))
        
        (when indent?
          (for-each (λ (defn)
                      (set-defn-name! defn
                                      (string-append
                                       (apply string
                                              (vector->list
                                               (make-vector 
                                                (- (defn-indent defn) min-indent) #\space)))
                                       (defn-name defn))))
                    defs))
        defs))
    
    ;; in-semicolon-comment: text number -> boolean
    ;; returns #t if `define-start-pos' is in a semicolon comment and #f otherwise
    (define (in-semicolon-comment? text define-start-pos)
      (let* ([para (send text position-paragraph define-start-pos)]
             [start (send text paragraph-start-position para)])
        (let loop ([pos start])
          (cond
            [(pos . >= . define-start-pos) #f]
            [(char=? #\; (send text get-character pos)) #t]
            [else (loop (+ pos 1))]))))
    
    ;; get-defn-indent : text number -> number
    ;; returns the amount to indent a particular definition
    (define (get-defn-indent text pos)
      (let* ([para (send text position-paragraph pos)]
             [para-start (send text paragraph-start-position para #t)])
        (let loop ([c-pos para-start]
                   [offset 0])
          (if (< c-pos pos)
              (let ([char (send text get-character c-pos)])
                (cond
                  [(char=? char #\tab)
                   (loop (+ c-pos 1) (+ offset (- 8 (modulo offset 8))))]
                  [else
                   (loop (+ c-pos 1) (+ offset 1))]))
              offset))))
    
    ;; skip-to-whitespace/paren : text number -> number
    ;; skips to the next parenthesis or whitespace after `pos', returns that position.
    (define (skip-to-whitespace/paren text pos)
      (let loop ([pos pos])
        (if (>= pos (send text last-position))
            (send text last-position)
            (let ([char (send text get-character pos)])
              (cond
                [(or (char=? #\) char)
                     (char=? #\( char)
                     (char=? #\] char)
                     (char=? #\[ char)
                     (char-whitespace? char))
                 pos]
                [else (loop (+ pos 1))])))))
    
    ;; skip-whitespace/paren : text number -> number
    ;; skips past any parenthesis or whitespace
    (define (skip-whitespace/paren text pos)
      (let loop ([pos pos])
        (if (>= pos (send text last-position))
            (send text last-position)
            (let ([char (send text get-character pos)])
              (cond
                [(or (char=? #\) char)
                     (char=? #\( char)
                     (char=? #\] char)
                     (char=? #\[ char)
                     (char-whitespace? char))
                 (loop (+ pos 1))]
                [else pos])))))
    
    ;; get-defn-name : text number -> string
    ;; returns the name of the definition starting at `define-pos'
    (define (get-defn-name text define-pos)
      (if (>= define-pos (send text last-position))
          (string-constant end-of-buffer-define)
          (let* ([start-pos (skip-whitespace/paren text (skip-to-whitespace/paren text define-pos))]
                 [end-pos (skip-to-whitespace/paren text start-pos)])
            (send text get-text start-pos end-pos))))
    
    (define (set-box/f! b v) (when (box? b) (set-box! b v)))
    
    
    
    
    

;                                        
;                                        
;                                        
;                                        
;   ;;;;                                 
;  ;;;                                   
;  ;;;; ;;; ;;;;;;;  ;;; ;; ;;;    ;;;;  
;  ;;;; ;;;;;;;;;;;; ;;;;;;;;;;;  ;; ;;; 
;  ;;;  ;;;  ;;  ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;;    ;;;;; ;;; ;;; ;;; ;;;;;;; 
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;;     
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;;;;;; 
;  ;;;  ;;;   ;;;;;; ;;; ;;; ;;;   ;;;;  
;                                        
;                                        
;                                        
;                                        

    (define dragable/def-int-mixin
      (mixin (panel:dragable<%>) ()
        (init-field unit-frame)
        (inherit get-percentages)
        (define/augment (after-percentage-change)
          (let ([percentages (get-percentages)])
            (when (and (= 1
                          (length (send unit-frame get-definitions-canvases))
                          (length (send unit-frame get-interactions-canvases)))
                       (= 2 (length percentages)))
              (preferences:set 'drscheme:unit-window-size-percentage (car percentages))))
          (inner (void) after-percentage-change))
        (super-new)))
    
    (define vertical-dragable/def-int% (dragable/def-int-mixin panel:vertical-dragable%))
    (define horizontal-dragable/def-int% (dragable/def-int-mixin panel:horizontal-dragable%))
    
    (define super-frame%
      (drscheme:frame:mixin
       (drscheme:frame:basics-mixin 
        (frame:searchable-text-mixin 
         (frame:searchable-mixin
          (frame:text-info-mixin 
           (frame:delegate-mixin
            (frame:status-line-mixin
             (frame:info-mixin
              (frame:text-mixin
               (frame:open-here-mixin
                (frame:editor-mixin
                 (frame:standard-menus-mixin
                  (frame:register-group-mixin
                   (frame:basic-mixin
                    frame%)))))))))))))))
    
    (define tab%
      (class* object% (drscheme:rep:context<%> tab<%>)
        (init-field frame
                    defs
                    i
                    defs-shown?
                    ints-shown?)
        (define enabled? #t)
        (field [ints #f]
               [visible-defs #f]
               [visible-ints #f]
               [focus-d/i 'defs])
        
        ;; only called to initialize this tab.
        ;; the interactions editor should be invariant.
        (define/public (set-ints i) (set! ints i)) 
        
        (define/public-final (get-frame) frame)
        (define/public-final (get-defs) defs)
        (define/public-final (get-ints) ints)
        (define/public-final (get-visible-defs) (values visible-defs defs-shown?))
        (define/public-final (set-visible-defs vd ds?) 
          (set! visible-defs vd)
          (set! defs-shown? ds?))
        (define/public-final (get-visible-ints) (values visible-ints ints-shown?))
        (define/public-final (set-visible-ints vi is?)
          (set! visible-ints vi)
          (set! ints-shown? is?))
        (define/public-final (set-focus-d/i di)
          (set! focus-d/i di))
        (define/public-final (get-focus-d/i) focus-d/i)
        (define/public-final (get-i) i)
        (define/public-final (set-i _i) (set! i _i))
        (define/public (disable-evaluation)
          (set! enabled? #f)
          (send defs lock #t)
          (send ints lock #t)
          (send frame disable-evaluation-in-tab this))
        (define/public (enable-evaluation)
          (set! enabled? #t)
          (send defs lock #f)
          (send ints lock #f)
          (send frame enable-evaluation-in-tab this))
        (define/public (get-enabled) enabled?)
        
        ;; current-execute-warning is a snapshot of the needs-execution-message
        ;; that is taken each time repl submission happens, and it gets reset
        ;; when "Run" is clicked.
        (define current-execute-warning #f)
        (define/pubment (repl-submit-happened)
          (set! current-execute-warning (send defs get-needs-execution-message))
          (update-execute-warning-gui))
        (define/public (get-current-execute-warning) current-execute-warning)
        (define/public (clear-execution-state) 
          (set! current-execute-warning #f)
          (update-execute-warning-gui))
        (define/public (update-execute-warning-gui)
          (when (is-current-tab?)
            (send frame show/hide-warning-message 
                  (get-current-execute-warning)
                  (λ () 
                    ;; this callback might be run with a different tab ...
                    (send (send frame get-current-tab) clear-execution-state)))))
        
        (define/public (get-directory)
          (let ([filename (send defs get-filename)])
            (if (and (path? filename)
                     (file-exists? filename))
                (let-values ([(base _1 _2) (split-path (normalize-path filename))])
                  base)
                #f)))
        
        (define/pubment (can-close?)
          (and (send defs can-close?)
               (send ints can-close?)
               (inner #t can-close?)))
        (define/pubment (on-close)
          (send defs on-close)
          (send ints on-close)
          (inner (void) on-close))
        
        ;; this should really do something local to the tab, but
        ;; for now it doesn't.
        (define/public (ensure-rep-shown rep) 
          (send frame ensure-rep-shown rep))
        
        (field [thread-to-break-box (make-weak-box #f)]
               [custodian-to-kill-box (make-weak-box #f)]
               [offer-kill? #f])
        
        ;; break-callback : -> void
        (define/public (break-callback)
          (let ([thread-to-break (weak-box-value thread-to-break-box)]
                [custodian-to-kill (weak-box-value custodian-to-kill-box)])
            (cond
              [(or (not thread-to-break)
                   (not custodian-to-kill))
               (bell)]
              [offer-kill? 
               (if (user-wants-kill?)
                   (when thread-to-break
                     (break-thread thread-to-break))
                   (when custodian-to-kill
                     (custodian-shutdown-all custodian-to-kill)))]
              [else
               (when thread-to-break
                 (break-thread thread-to-break))
               ;; only offer a kill the next time if 
               ;; something got broken.
               (set! offer-kill? #t)])))
        
        ;; user-wants-kill? : -> boolean
        ;; handles events, so be sure to check state
        ;; after calling to avoid race conditions.
        (define/private (user-wants-kill?)
          (gui-utils:get-choice
           (string-constant kill-evaluation?)
           (string-constant just-break)
           (string-constant kill)
           (string-constant kill?)
           'diallow-close
           frame))
        
        ;; reset-offer-kill
        (define/public (reset-offer-kill)
          (set! offer-kill? #f))
        
        ;; get-breakables : -> (union #f thread) (union #f cust) -> void
        (define/public (get-breakables)
          (values (weak-box-value thread-to-break-box) (weak-box-value custodian-to-kill-box)))
        
        ;; set-breakables : (union #f thread) (union #f cust) -> void
        (define/public (set-breakables thd cust)
          (set! thread-to-break-box (make-weak-box thd))
          (set! custodian-to-kill-box (make-weak-box cust)))
        
        (define/pubment (clear-annotations)
          (inner (void) clear-annotations)
          (send ints reset-highlighting))
        
        (define running? #f)
        (define/public-final (is-running?) running?)
        (define/public (update-running b?) 
          (set! running? b?)
          (send frame update-running b?))
        
        (define/public-final (is-current-tab?) (eq? this (send frame get-current-tab)))
        
        (define log-visible? #f)
        (define/public-final (toggle-log)
          (set! log-visible? (not log-visible?))
          (send frame show/hide-log log-visible?))
        (define/public-final (update-log)
          (send frame show/hide-log log-visible?))
        (define/public-final (update-logger-window command)
          (when (is-current-tab?)
            (send frame update-logger-window command)))
        
        (define current-planet-status #f)
        (define/public-final (new-planet-status a b)
          (set! current-planet-status (cons a b))
          (update-planet-status))
        (define/public-final (clear-planet-status) 
          (set! current-planet-status #f)
          (update-planet-status))
        (define/public-final (update-planet-status)
          (send frame show-planet-status 
                (and current-planet-status 
                     (car current-planet-status))
                (and current-planet-status 
                     (cdr current-planet-status))))
        
        (super-new)))
    
    ;; should only be called by the tab% object (and the class itself)
    (define-local-member-name 
      disable-evaluation-in-tab
      enable-evaluation-in-tab
      update-toolbar-visibility
      show/hide-log
      show-planet-status)
    
    (define -frame<%>
      (interface (drscheme:frame:<%> frame:searchable-text<%> frame:delegate<%> frame:open-here<%>)
        get-insert-menu
        get-special-menu
        get-interactions-text
        get-definitions-text
        get-interactions-canvas
        get-definitions-canvas
        get-button-panel
        execute-callback
        get-current-tab
        open-in-new-tab
        close-current-tab
        on-tab-change
        enable-evaluation
        disable-evaluation
        get-definitions/interactions-panel-parent
        register-capability-menu-item
        
        ensure-rep-shown
        ensure-rep-hidden
        ensure-defs-shown
        
        
        get-language-menu
        register-toolbar-button
        register-toolbar-buttons
        unregister-toolbar-button
        get-tabs))
    

    (define frame-mixin
      (mixin (drscheme:frame:<%> frame:searchable-text<%> frame:delegate<%> frame:open-here<%>)
        (-frame<%>)
        (init filename)
        (inherit set-label-prefix get-show-menu
                 get-menu%
                 get-area-container
                 update-info
                 get-file-menu
                 search-hidden?
                 unhide-search
                 hide-search
                 file-menu:get-close-item
                 file-menu:get-save-item
                 file-menu:get-save-as-item
                 file-menu:get-revert-item
                 file-menu:get-print-item)

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; execute warning
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define execute-warning-panel #f)
        (define execute-warning-parent-panel #f)
        (define execute-warning-canvas #f)
        (define/public-final (show/hide-warning-message msg hide-canvas)
          (when (and execute-warning-parent-panel
                     execute-warning-panel)
            (cond
              [msg
               (cond
                 [execute-warning-canvas
                  (send execute-warning-canvas set-message msg)]
                 [else
                  (set! execute-warning-canvas
                        (new execute-warning-canvas% 
                             [stretchable-height #t]
                             [parent execute-warning-panel]
                             [message msg]))
                  (new close-icon% 
                       [parent execute-warning-panel]
                       [bg-color "yellow"]
                       [callback (λ () (hide-canvas))])])
               (send execute-warning-parent-panel
                     change-children
                     (λ (l) (append (remq execute-warning-panel l)
                                    (list execute-warning-panel))))]
              [else
               (when execute-warning-canvas
                 (send execute-warning-parent-panel
                        change-children
                        (λ (l) (remq execute-warning-panel l)))
                 (send execute-warning-canvas set-message #f))])))

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; logging 
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define logger-panel #f)
        (define logger-parent-panel #f)
        
        ;; logger-gui-tab-panel: (or/c #f (is-a?/c tab-panel%))
        ;; this is #f when the GUI has not been built yet. After
        ;; it becomes a tab-panel, it is always a tab-panel (altho the tab panel might not always be shown)
        (define logger-gui-tab-panel #f)
        (define logger-gui-canvas #f)
        
        ;; logger-gui-text: (or/c #f (is-a?/c tab-panel%))
        ;; this is #f when the GUI has not been built or when the logging panel is hidden
        ;; in that case, the logging messages aren't begin saved in an editor anywhere
        (define logger-gui-text #f)
        
        (define logger-menu-item #f)

        (define/public-final (show/hide-log show?)
          (let ([p (preferences:get 'drscheme:logging-size-percentage)])
            (begin-container-sequence)
            (cond
              [logger-gui-tab-panel
               (send logger-parent-panel change-children
                     (λ (l)
                       (cond
                         [(or (and show? (member logger-panel l))
                              (and (not show?)
                                   (not (member logger-panel l))))
                          ;; if things are already up to date, only update the logger text
                          (when show?
                            (update-logger-window #f))
                          l]
                         [show? 
                          (new-logger-text)
                          (send logger-gui-canvas set-editor logger-gui-text)
                          (update-logger-window #f)
                          (send logger-menu-item set-label (string-constant hide-log))
                          (append (remq logger-panel l) (list logger-panel))]
                         [else
                          (send logger-menu-item set-label (string-constant show-log))
                          (set! logger-gui-text #f)
                          (send logger-gui-canvas set-editor #f)
                          (remq logger-panel l)])))]
              [else
               (when show? ;; if we want to hide and it isn't built yet, do nothing
                 (set! logger-gui-tab-panel
                       (new tab-panel% 
                            [choices (list (string-constant logging-all)
                                           "fatal" "error" "warning" "info" "debug")]
                            [parent logger-panel]
                            [callback
                             (λ (tp evt)
                               (preferences:set 'drscheme:logger-gui-tab-panel-level (send logger-gui-tab-panel get-selection))
                               (update-logger-window #f))]))
                 (send logger-gui-tab-panel set-selection (preferences:get 'drscheme:logger-gui-tab-panel-level))
                 (new-logger-text)
                 (set! logger-gui-canvas 
                       (new editor-canvas% [parent logger-gui-tab-panel] [editor logger-gui-text]))
                 (send logger-menu-item set-label (string-constant hide-log))
                 (update-logger-window #f)
                 (send logger-parent-panel change-children (lambda (l) (append l (list logger-panel)))))])
            (with-handlers ([exn:fail? void])
              (send logger-parent-panel set-percentages (list p (- 1 p))))
            (update-logger-button-label)
            (end-container-sequence)))
        
        (define/private (log-shown?)
          (and logger-gui-tab-panel
               (member logger-panel (send logger-parent-panel get-children))))
        
        (define/private (new-logger-text)
          (set! logger-gui-text (new (text:hide-caret/selection-mixin text:basic%)))
          (send logger-gui-text lock #t))
        
        (define/public (update-logger-window command)
          (when logger-gui-text 
            (let ([admin (send logger-gui-text get-admin)]
                  [canvas (send logger-gui-text get-canvas)])
              (when (and canvas admin)
                (let ([logger-messages (send interactions-text get-logger-messages)]
                      [level (case (send logger-gui-tab-panel get-selection)
                               [(0) #f]
                               [(1) 'fatal]
                               [(2) 'error]
                               [(3) 'warning]
                               [(4) 'info]
                               [(5) 'debug])])
                  (cond
                    [(and (pair? command)
                          (pair? logger-messages)
                          ;; just flush and redraw everything if there is one (or zero) logger messages
                          (pair? (cdr logger-messages)))
                     (let ([msg (cdr command)])
                       (when (or (not level) 
                                 (eq? (vector-ref msg 0) level))
                         (send logger-gui-text begin-edit-sequence)
                         (send logger-gui-text lock #f)
                         (case (car command)
                           [(add-line) (void)]
                           [(clear-last-and-add-line)
                            (send logger-gui-text delete
                                  0
                                  (send logger-gui-text paragraph-start-position 1))]) 
                         (send logger-gui-text insert
                               "\n"
                               (send logger-gui-text last-position)
                               (send logger-gui-text last-position))
                         (send logger-gui-text insert 
                               (vector-ref msg 1) 
                               (send logger-gui-text last-position)
                               (send logger-gui-text last-position))
                         (send logger-gui-text end-edit-sequence)
                         (send logger-gui-text lock #t)))]
                    [else
                     (send logger-gui-text begin-edit-sequence)
                     (send logger-gui-text lock #f)
                     (send logger-gui-text erase)
                     
                     (let ([insert-one
                            (λ (x newline?)
                              (when (or (not level)
                                        (eq? level (vector-ref x 0)))
                                (when newline? (send logger-gui-text insert "\n" 0 0))
                                (send logger-gui-text insert (vector-ref x 1) 0 0)))]) 
                       
                       (unless (null? logger-messages)
                         ;; skip the last newline in the buffer
                         (insert-one (car logger-messages) #f)
                         (for-each
                          (λ (x) (insert-one x #t))
                          (cdr (send interactions-text get-logger-messages)))))
                       
                     (send logger-gui-text lock #t)
                     (send logger-gui-text end-edit-sequence)]))))))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; planet status 
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
        (define planet-status-parent-panel #f)
        (define planet-status-panel #f)
        (define planet-message #f)
        (define planet-logger-button #f)
        ;; local-member-name
        (define/public (show-planet-status tag package)
          (cond
            [(and (not tag)
                  (not package)
                  (or (not planet-status-parent-panel)
                      (not (member planet-status-panel (send planet-status-parent-panel get-children)))))
             ;; if there is no information and there is no GUI there, don't do anything
             (void)]
            [else
             (when planet-status-panel
               (unless planet-message
                 (new message% 
                      [parent planet-status-panel]
                      [label drscheme:debug:small-planet-bitmap])
                 (set! planet-message (new message% [parent planet-status-panel] [label ""] [stretchable-width #t]))
                 (set! planet-logger-button
                       (new button%
                            [font small-control-font]
                            [parent planet-status-panel]
                            [label (string-constant show-log)]
                            [callback (λ (a b) (send current-tab toggle-log))]))
                 (update-logger-button-label)
                 (new close-icon%
                      [parent planet-status-panel]
                      [callback (λ () 
                                  (send planet-status-parent-panel change-children
                                        (λ (l)
                                          (remq planet-status-panel l)))
                                  (send current-tab clear-planet-status))]))
               (send planet-message set-label 
                     (case tag
                       [(download)
                        (format (string-constant planet-downloading) package)]
                       [(install)
                        (format (string-constant planet-installing) package)]
                       [(finish)
                        (format (string-constant planet-finished) package)]
                       [else
                        (string-constant planet-no-status)]))
               (send planet-status-parent-panel change-children
                     (λ (l)
                       (if (memq planet-status-panel l)
                           l
                           (append (remq planet-status-panel l) (list planet-status-panel))))))]))
            
        (define/private (update-logger-button-label)
          (when planet-logger-button
            (send planet-logger-button set-label
                  (if (and logger-gui-text
                           (member logger-panel (send logger-parent-panel get-children)))
                      (string-constant hide-log)
                      (string-constant show-log)))))
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;
        ;; transcript
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
        
        ;; transcript : (union #f string[directory-name])
        (field [transcript #f]
               [definitions-transcript-counter 0]  ;; number
               [interactions-transcript-counter 0] ;; number
               [transcript-parent-panel #f]    ;; panel (unitialized short time only)
               [transcript-panel #f]           ;; panel (unitialized short time only)
               [transcript-menu-item #f])      ;; menu-item (unitialized short time only)
        ;; record-definitions : -> void
        (define/private (record-definitions)
          (when transcript
            (set! definitions-transcript-counter (+ definitions-transcript-counter 1))
            (send definitions-text save-file 
                  (build-path transcript (format "~a-definitions" (pad-two definitions-transcript-counter)))
                  'copy)))
        
        ;; record-ineractions : -> void
        (define/private (record-interactions)
          (when transcript
            (set! interactions-transcript-counter (+ interactions-transcript-counter 1))
            (send interactions-text save-file 
                  (build-path transcript (format "~a-interactions" (pad-two interactions-transcript-counter)))
                  'copy)))
        
        ;; pad-two : number -> string
        ;; pads a number to two digits?
        (define/private (pad-two n)
          (cond
            [(<= 0 n 9) (format "0~a" n)]
            [else (format "~a" n)]))
        
        ;; start-transcript : -> void
        ;; turns on the transcript and shows the transcript gui
        (define/private (start-transcript)
          (let ([transcript-directory (mred:get-directory
                                       (string-constant please-choose-a-log-directory)
                                       this)])
            (when (and transcript-directory
                       (ensure-empty transcript-directory))
              (send transcript-menu-item set-label (string-constant stop-logging))
              (set! transcript transcript-directory)
              (set! definitions-transcript-counter 0)
              (set! interactions-transcript-counter 0)
              (build-transcript-panel)
              (record-definitions))))
        
        ;; stop-transcript : -> void
        ;; turns off the transcript procedure
        (define/private (stop-transcript)
          (record-interactions)
          (send transcript-menu-item set-label (string-constant log-definitions-and-interactions))
          (set! transcript #f)
          (send transcript-panel change-children (λ (l) null)))
        
        ;; build-transcript-panel : -> void
        ;; builds the contents of the transcript panel
        (define/private (build-transcript-panel)
          (define hp (make-object horizontal-panel% transcript-panel '(border)))
          (make-object message% (string-constant logging-to) hp)
          (send (make-object message% (path->string transcript) hp) stretchable-width #t)
          (make-object button% (string-constant stop-logging) hp (λ (x y) (stop-transcript))))
        
        ;; ensure-empty : string[directory] -> boolean
        ;; if the transcript-directory is empty, just return #t
        ;; if not, ask the user about emptying it. 
        ;;   if they say yes, try to empty it.
        ;;     if that fails, report the error and return #f.
        ;;     if it succeeds, return #t.
        ;;   if they say no, return #f.
        (define/private (ensure-empty transcript-directory)
          (let ([dir-list (directory-list transcript-directory)])
            (or (null? dir-list)
                (let ([query (message-box 
                              (string-constant drscheme)
                              (gui-utils:format-literal-label (string-constant erase-log-directory-contents)
                                                              transcript-directory)
                              this
                              '(yes-no))])
                  (cond
                    [(eq? query 'no) 
                     #f]
                    [(eq? query 'yes)
                     (with-handlers ([exn:fail:filesystem?
                                      (λ (exn)
                                        (message-box 
                                         (string-constant drscheme)
                                         (gui-utils:format-literal-label (string-constant error-erasing-log-directory)
                                                 (if (exn? exn)
                                                     (format "~a" (exn-message exn))
                                                     (format "~s" exn)))
                                         this)
                                        #f)])
                       (for-each (λ (file) (delete-file (build-path transcript-directory file)))
                                 dir-list)
                       #t)])))))
        
        (define/override (make-root-area-container cls parent)
          (let* ([saved-p (preferences:get 'drscheme:module-browser-size-percentage)]
                 [saved-p2 (preferences:get 'drscheme:logging-size-percentage)]
                 [_module-browser-parent-panel
                  (super make-root-area-container 
                         (make-two-way-prefs-dragable-panel% panel:horizontal-dragable%
                                                             'drscheme:module-browser-size-percentage)
                         parent)]
                 [_module-browser-panel (new vertical-panel%
                                             (parent _module-browser-parent-panel)
                                             (alignment '(left center))
                                             (stretchable-width #f))]
                 [planet-status-outer-panel (new vertical-panel% [parent _module-browser-parent-panel])]
                 [execute-warning-outer-panel (new vertical-panel% [parent planet-status-outer-panel])]
                 [logger-outer-panel (new (make-two-way-prefs-dragable-panel% panel:vertical-dragable%
                                                                              'drscheme:logging-size-percentage)
                                          [parent execute-warning-outer-panel])]
                 [trans-outer-panel (new vertical-panel% [parent logger-outer-panel])]
                 [root (make-object cls trans-outer-panel)])
            (set! module-browser-parent-panel _module-browser-parent-panel)
            (set! module-browser-panel _module-browser-panel)
            (send module-browser-parent-panel change-children (λ (l) (remq module-browser-panel l)))
            (set! logger-parent-panel logger-outer-panel)
            (set! logger-panel (new vertical-panel% [parent logger-parent-panel]))
            (send logger-parent-panel change-children (lambda (x) (remq logger-panel x)))
            
            (set! execute-warning-parent-panel execute-warning-outer-panel)
            (set! execute-warning-panel (new horizontal-panel% 
                                             [parent execute-warning-parent-panel]
                                             [stretchable-height #f]))
            (send execute-warning-parent-panel change-children (λ (l) (remq execute-warning-panel l)))
            
            (set! transcript-parent-panel (new horizontal-panel%
                                               (parent trans-outer-panel)
                                               (stretchable-height #f)))
            (set! transcript-panel (make-object horizontal-panel% transcript-parent-panel))
            (set! planet-status-parent-panel (new vertical-panel% 
                                                  [parent planet-status-outer-panel]
                                                  [stretchable-height #f]))
            (set! planet-status-panel (new horizontal-panel% 
                                           [parent planet-status-parent-panel]))
            (send planet-status-parent-panel change-children (λ (l) (remq planet-status-panel l)))
            (unless (toolbar-shown?)
              (send transcript-parent-panel change-children (λ (l) '())))
            (preferences:set 'drscheme:module-browser-size-percentage saved-p)
            (preferences:set 'drscheme:logging-size-percentage saved-p2)
            
            root))
        
        (inherit show-info hide-info is-info-hidden?)
        (field [toolbar-state (preferences:get 'drscheme:toolbar-state)]
               [toolbar-top-menu-item #f]
               [toolbar-left-menu-item #f]
               [toolbar-right-menu-item #f]
               [toolbar-hidden-menu-item #f]
               [toolbar-menu #f])
        
        ;; returns #t if the toolbar is visible, #f otherwise
        (define/private (toolbar-shown?) (car toolbar-state))
        
        (define/private (change-toolbar-state new-state)
          (set! toolbar-state new-state)
          (preferences:set 'drscheme:toolbar-state new-state)
          (update-toolbar-visibility))
        
        (define/override (on-toolbar-button-click) (change-toolbar-state (cons (not (car toolbar-state)) (cdr toolbar-state))))
        (define/private (set-toolbar-left) (change-toolbar-state (cons #f 'left)))
        (define/private (set-toolbar-right) (change-toolbar-state (cons #f 'right)))
        (define/private (set-toolbar-top) (change-toolbar-state (cons #f 'top)))
        (define/private (set-toolbar-hidden) (change-toolbar-state (cons #t (cdr toolbar-state))))
        
        (define/public (update-toolbar-visibility)
          (let* ([hidden? (toolbar-is-hidden?)]
                 [left? (toolbar-is-left?)]
                 [right? (toolbar-is-right?)]
                 [top? (toolbar-is-top?)])
            
            (send toolbar-left-menu-item check left?)
            (send toolbar-right-menu-item check right?)
            (send toolbar-top-menu-item check top?)
            (send toolbar-hidden-menu-item check hidden?)
            
            (cond
              [hidden?
               (hide-info)
               (send top-outer-panel change-children (λ (l) '()))
               (send transcript-parent-panel change-children (λ (l) '()))]
              [top? (orient/show #t)]
              [left? (orient/show #t)]
              [right? (orient/show #f)]))
          (update-defs/ints-resize-corner))
        
        (define/private (toolbar-is-hidden?)
          (car (preferences:get 'drscheme:toolbar-state)))
        (define/private (toolbar-is-top?)
          (and (not (toolbar-is-hidden?))
               (eq? (cdr (preferences:get 'drscheme:toolbar-state))
                    'top)))
        (define/private (toolbar-is-right?)
          (and (not (toolbar-is-hidden?))
               (eq? (cdr (preferences:get 'drscheme:toolbar-state))
                    'right)))
        (define/private (toolbar-is-left?)
          (and (not (toolbar-is-hidden?))
               (eq? (cdr (preferences:get 'drscheme:toolbar-state))
                    'left)))

        (define/private (orient/show bar-at-beginning?)
          (let ([vertical? (or (toolbar-is-left?) (toolbar-is-right?))])
            (begin-container-sequence)
            (show-info)
            
            ;; orient the button panel and all panels inside it.
            (let loop ([obj button-panel])
              (when (is-a? obj area-container<%>)
                (when (or (is-a? obj vertical-panel%)
                          (is-a? obj horizontal-panel%))
                  (unless (equal? (send obj get-orientation) (not vertical?))
                    (send obj set-orientation (not vertical?))
                    ;; have to be careful to avoid reversing the list when the orientation is already proper
                    (send obj change-children reverse)))
                (for-each loop (send obj get-children))))
                    
            (orient)
            
            (send top-outer-panel stretchable-height vertical?)
            (send top-outer-panel stretchable-width (not vertical?))
            (send top-panel set-orientation (not vertical?))
            (send toolbar/rest-panel set-orientation vertical?)
            (send toolbar/rest-panel change-children
                  (λ (l)
                    (if bar-at-beginning?
                        (cons top-outer-panel (remq top-outer-panel l))
                        (append (remq top-outer-panel l) (list top-outer-panel)))))
            (send top-outer-panel change-children (λ (l) (list top-panel)))
            (send transcript-parent-panel change-children (λ (l) (list transcript-panel)))

            (let* ([settings (send definitions-text get-next-settings)]
                   [language (drscheme:language-configuration:language-settings-language settings)]
                   [name (get-drscheme:define-popup-name (send language capability-value 'drscheme:define-popup)
                                                         vertical?)])
              (when name
                (send func-defs-canvas set-message #f name)))
            (send name-message set-short-title vertical?)
            (send name-panel set-orientation (not vertical?))
            (if vertical?
                (send name-panel set-alignment 'right 'top)
                (send name-panel set-alignment 'left 'center))
            (end-container-sequence)))
        
        (define toolbar-buttons '())
        (define/public (register-toolbar-button b)
          (set! toolbar-buttons (cons b toolbar-buttons))
          (orient))
        
        (define/public (register-toolbar-buttons bs)
          (set! toolbar-buttons (append bs toolbar-buttons))
          (orient))
        
        (define/public (unregister-toolbar-button b)
          (set! toolbar-buttons (remq b toolbar-buttons))
          (void))
        
        (define/private (orient)
          (let ([vertical? (or (toolbar-is-left?) (toolbar-is-right?))])
            (for-each
             (λ (obj) (send obj set-label-visible (not vertical?)))
             toolbar-buttons))
          
          (let loop ([obj button-panel])
            (cond
              [(is-a? obj area-container<%>)
               (for-each loop (send obj get-children))]
              [(is-a? obj switchable-button%)
               (unless (memq obj toolbar-buttons)
                 (error 'register-toolbar-button "found a switchable-button% that is not registered, label ~s"
                        (send obj get-label)))]
              [else (void)])))
        
        (field [remove-show-status-line-callback
                (preferences:add-callback
                 'framework:show-status-line
                 (λ (p v)
                   (update-defs/ints-resize-corner/pref v)))])
        
        (define/private (update-defs/ints-resize-corner)
          (update-defs/ints-resize-corner/pref
           (preferences:get 'framework:show-status-line)))
        
        (define/private (update-defs/ints-resize-corner/pref si-pref)
          (let ([bottom-material? (and (not (car toolbar-state))
                                       si-pref)])
            (let loop ([cs definitions-canvases])
              (cond
                [(null? cs) (void)]
                [(null? (cdr cs))
                 (send (car cs) set-resize-corner (and (not bottom-material?)
                                                       (not interactions-shown?)))]
                [else
                 (send (car cs) set-resize-corner #f)
                 (loop (cdr cs))]))
            (let loop ([cs interactions-canvases])
              (cond
                [(null? cs) (void)]
                [(null? (cdr cs))
                 (send (car cs) set-resize-corner (and (not bottom-material?) 
                                                       interactions-shown?))]
                [else
                 (send (car cs) set-resize-corner #f)
                 (loop (cdr cs))]))))
        
        [define definitions-item #f]
        [define interactions-item #f]
        [define name-message #f]
        [define save-button #f]
        [define save-init-shown? #f]
        
        [define/private set-save-init-shown? (λ (x) (set! save-init-shown? x))]
        
        [define canvas-show-mode #f]
        [define allow-split? #f]
        [define forced-quit? #f]
        [define search-canvas #f]
        
        (define/public (make-searchable canvas)
          (update-info)
          (set! search-canvas canvas))
        
        (define was-locked? #f)
        
        (define/public-final (disable-evaluation-in-tab tab)
          (when (eq? tab current-tab)
            (disable-evaluation)))
        
        (define/pubment (disable-evaluation)
          (when execute-menu-item
            (send execute-menu-item enable #f))
          (send execute-button enable #f)
          (inner (void) disable-evaluation))
        
        (define/public-final (enable-evaluation-in-tab tab)
          (when (eq? tab current-tab)
            (enable-evaluation)))
        
        (define/pubment (enable-evaluation)
          (when execute-menu-item
            (send execute-menu-item enable #t))
          (send execute-button enable #t)
          (inner (void) enable-evaluation))
        
        (inherit set-label)
        (inherit modified)
        (define/public (update-save-button)
          (let ([mod? (send definitions-text is-modified?)])
            (modified mod?)
            (if save-button
                (unless (eq? mod? (send save-button is-shown?))
                  (send save-button show mod?))
                (set! save-init-shown? mod?))
            (update-tab-label current-tab)))
        
        (define/public (language-changed)
          (let* ([settings (send definitions-text get-next-settings)]
                 [language (drscheme:language-configuration:language-settings-language settings)])
            (send func-defs-canvas language-changed language (or (toolbar-is-left?) (toolbar-is-right?)))
            (send language-message set-yellow/lang
                  (not (send definitions-text this-and-next-language-the-same?))
                  (string-append (send language get-language-name)
                                 (if (send language default-settings? 
                                           (drscheme:language-configuration:language-settings-settings settings))
                                     ""
                                     (string-append " " (string-constant custom)))))
            (when (is-a? scheme-menu menu%)
              (let ([label (send scheme-menu get-label)]
                    [new-label (send language capability-value 'drscheme:language-menu-title)])
                (unless (equal? label new-label)
                  (send scheme-menu set-label new-label))))))
        
        (define/public (get-language-menu) scheme-menu)
        
        ;; update-save-message : -> void
        ;; sets the save message. If input is #f, uses the frame's
        ;; title.
        (define/public (update-save-message)
          (when name-message
            (let ([filename (send definitions-text get-filename)])
              (send name-message set-message 
                    (if filename #t #f)
                    (send definitions-text get-filename/untitled-name))))
          (update-tabs-labels))
        
        (define/private (update-tabs-labels)
          (for-each (λ (tab) (update-tab-label tab)) tabs)
          (send tabs-panel set-selection (send current-tab get-i))
          (send (send tabs-panel get-parent)
                change-children
                (λ (l)
                  (cond
                    [(= (send tabs-panel get-number) 1)
                     (remq tabs-panel l)]
                    [else
                     (if (memq tabs-panel l)
                         l
                         (cons tabs-panel l))]))))
        
        (define/private (update-tab-label tab)
          (let ([label (gui-utils:trim-string (get-defs-tab-label (send tab get-defs) tab) 200)])
            (unless (equal? label (send tabs-panel get-item-label (send tab get-i)))
              (send tabs-panel set-item-label (send tab get-i) label))))
        
        (define/public (get-tab-filename i)
          (get-defs-tab-filename (send (list-ref tabs i) get-defs)))
        
        (define/private (get-defs-tab-label defs tab)
          (let ([fn (send defs get-filename)]
                [i-prefix (or (for/or ([i (in-list tabs)]
                                       [n (in-naturals 1)]
                                       #:when (<= n 9))
                                (and (eq? i tab)
                                     (format "~a: " n)))
                              "")])
            (add-modified-flag
             defs
             (string-append
              i-prefix
              (get-defs-tab-filename defs)))))
        
        (define/private (get-defs-tab-filename defs)
          (let ([fn (send defs get-filename)])
            (if fn
                (get-tab-label-from-filename fn)
                (send defs get-filename/untitled-name))))
        
        (define/private (get-tab-label-from-filename fn)
          (let* ([take-n
                  (λ (n lst)
                    (let loop ([n n]
                               [lst lst])
                      (cond
                        [(zero? n) null]
                        [(null? lst) null]
                        [else (cons (car lst) (loop (- n 1) (cdr lst)))])))]
                 [find-exp-diff
                  (λ (p1 p2)
                    (let loop ([p1 p1]
                               [p2 p2]
                               [i 1])
                      (cond
                        [(or (null? p1) (null? p2)) i]
                        [else (let ([f1 (car p1)]
                                    [f2 (car p2)])
                                (if (equal? f1 f2)
                                    (loop (cdr p1) (cdr p2) (+ i 1))
                                    i))])))]
                 [exp (reverse (explode-path (normalize-path/exists fn)))]
                 [other-exps
                  (filter
                   (λ (x) (and x 
                               (not (equal? exp x))))
                   (map (λ (other-tab) 
                          (let ([fn (send (send other-tab get-defs) get-filename)])
                            (and fn 
                                 (reverse (explode-path (normalize-path/exists fn))))))
                        tabs))]
                 [size
                  (let loop ([other-exps other-exps]
                             [size 1])
                    (cond
                      [(null? other-exps) size]
                      [else (let ([new-size (find-exp-diff (car other-exps) exp)])
                              (loop (cdr other-exps)
                                    (max new-size size)))]))])
            (path->string (apply build-path (reverse (take-n size exp))))))
        
        (define/private (normalize-path/exists fn)
          (if (file-exists? fn)
              (normalize-path fn)
              fn))
        
        (define/private (add-modified-flag text string)
          (if (send text is-modified?)
              (let ([prefix (get-save-diamond-prefix)])
                (if prefix
                    (string-append prefix string)
                    string))
              string))
        
        (define/private (get-save-diamond-prefix)
          (let ([candidate-prefixes 
                 ;; be sure asterisk is at the end of each list,
                 ;; since that's a relatively safe character
                 (case (system-type)
                   [(windows) '("• " "★ " "◆ " "* ")]
                   [(unix) '("★ " "◆ " "* ")]
                   [else '("◆ " "★ " "* ")])])
            (ormap
             (lambda (candidate)
               (and (andmap (λ (x) (send normal-control-font screen-glyph-exists? x #t))
                            (string->list candidate))
                    candidate))
             candidate-prefixes)))
        
        [define/override get-canvas% (λ () (drscheme:get/extend:get-definitions-canvas))]
        
        (define/public (update-running running?)
          (send running-canvas set-running running?))
        (define/public (ensure-defs-shown)
          (unless definitions-shown?
            (toggle-show/hide-definitions)
            (update-shown)))
        (define/public (ensure-rep-shown rep)
          (unless (eq? rep interactions-text)
            (let loop ([tabs tabs])
              (unless (null? tabs)
                (let ([tab (car tabs)])
                  (if (eq? (send tab get-ints) rep)
                      (change-to-tab tab)
                      (loop (cdr tabs)))))))
          (unless interactions-shown?
            (toggle-show/hide-interactions)
            (update-shown)))
        (define/public (ensure-rep-hidden)
          (when interactions-shown?
            (toggle-show/hide-interactions)
            (update-shown)))
        
        (define/override (get-editor%) (drscheme:get/extend:get-definitions-text))
        (define/public (still-untouched?)
          (and (send definitions-text still-untouched?)
               (let* ([prompt (send interactions-text get-prompt)]
                      [first-prompt-para
                       (let loop ([n 0])
                         (cond
                           [(n . <= . (send interactions-text last-paragraph))
                            (if (string=?
                                 (send interactions-text get-text 
                                       (send interactions-text paragraph-start-position n)
                                       (+ (send interactions-text paragraph-start-position n)
                                          (string-length prompt)))
                                 prompt)
                                n
                                (loop (+ n 1)))]
                           [else #f]))])
                 (and first-prompt-para
                      (= first-prompt-para (send interactions-text last-paragraph))
                      (equal? 
                       (send interactions-text get-text
                             (send interactions-text paragraph-start-position first-prompt-para)
                             (send interactions-text paragraph-end-position first-prompt-para))
                       (send interactions-text get-prompt))))))
        (define/public (change-to-file name)
          (cond
            [(and name (file-exists? name))
             (ensure-rep-hidden)
             (send definitions-text begin-edit-sequence)
             (send definitions-text load-file/gui-error name)
             (send definitions-text end-edit-sequence)
             (send language-message set-yellow #f)]
            [name
             (send definitions-text set-filename name)]
            [else (send definitions-text clear)])
          (send definitions-canvas focus))
        
     
        
        
        
        
        ;                                            
        ;                                            
        ;                                            
        ;                           ;                
        ;                           ;                
        ;                           ;                
        ;   ; ;;  ;;     ;;;     ;; ;    ;;;    ;;;  
        ;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;  ;     
        ;   ;   ;   ;  ;     ; ;    ;  ;    ;  ;;    
        ;   ;   ;   ;  ;     ; ;    ;  ;;;;;;   ;;   
        ;   ;   ;   ;  ;     ; ;    ;  ;          ;  
        ;   ;   ;   ;   ;   ;   ;  ;;   ;         ;  
        ;   ;   ;   ;    ;;;     ;; ;    ;;;;  ;;;   
        ;                                            
        ;                                            
        ;                                            
        
        
        (define/private (add-modes-submenu edit-menu)
          (new menu%
               (parent edit-menu)
               (label (string-constant mode-submenu-label))
               (demand-callback
                (λ (menu)
                  (for-each (λ (item) (send item delete))
                            (send menu get-items))
                  (for-each (λ (mode) 
                              (let* ([item
                                      (new checkable-menu-item%
                                           (label (drscheme:modes:mode-name mode))
                                           (parent menu)
                                           (callback 
                                            (λ (_1 _2) (send definitions-text set-current-mode mode))))])
                                (when (send definitions-text is-current-mode? mode)
                                  (send item check #t))))
                            (drscheme:modes:get-modes))))))
        
        
        
        
        ;                                                                                         
        ;                                                                                         
        ;                                                                                         
        ;                  ;   ;           ;                  ;   ;                               
        ;                  ;               ;                  ;   ;                               
        ;                  ;       ;      ;                   ;   ;                               
        ;    ;;;   ; ;;    ;   ;  ;;;;    ;     ;;;    ;;;    ;   ;   ;;;    ; ;;     ;;;    ;;;  
        ;   ;      ;;  ;   ;   ;   ;      ;    ;   ;  ;   ;   ;   ;  ;   ;   ;;  ;   ;      ;   ; 
        ;   ;;     ;    ;  ;   ;   ;      ;   ;      ;     ;  ;   ;      ;   ;    ;  ;;    ;    ; 
        ;    ;;    ;    ;  ;   ;   ;     ;    ;      ;     ;  ;   ;   ;;;;   ;    ;   ;;   ;;;;;; 
        ;      ;   ;    ;  ;   ;   ;     ;    ;      ;     ;  ;   ;  ;   ;   ;    ;     ;  ;      
        ;      ;   ;;  ;   ;   ;   ;     ;     ;   ;  ;   ;   ;   ;  ;   ;   ;;  ;      ;   ;     
        ;   ;;;    ; ;;    ;   ;    ;;  ;       ;;;    ;;;    ;   ;   ;;;;;  ; ;;    ;;;     ;;;; 
        ;          ;                    ;                                    ;                    
        ;          ;                    ;                                    ;                    
        ;          ;                                                         ;                    
        
        
        (inherit get-edit-target-window)
        
        (define/public (split)
          (let ([canvas-to-be-split (get-edit-target-window)])
            (cond
              [(memq canvas-to-be-split definitions-canvases)
               (split-definitions canvas-to-be-split)]
              [(memq canvas-to-be-split interactions-canvases)
               (split-interactions canvas-to-be-split)]
              [else (bell)])))
        
        (define/private (split-definitions canvas-to-be-split)
          (handle-split canvas-to-be-split
                        (λ (x) (set! definitions-canvases x))
                        definitions-canvases
                        (drscheme:get/extend:get-definitions-canvas)
                        definitions-text))
        
        (define/private (split-interactions canvas-to-be-split)
          (handle-split canvas-to-be-split
                        (λ (x) (set! interactions-canvases x))
                        interactions-canvases
                        (drscheme:get/extend:get-interactions-canvas)
                        interactions-text))
        
        (define/private (handle-split canvas-to-be-split set-canvases! canvases canvas% text)
          (let-values ([(ox oy ow oh cursor-y)
                        (get-visible-region canvas-to-be-split)])
            (let ([orig-percentages (send resizable-panel get-percentages)]
                  [orig-canvases (send resizable-panel get-children)]
                  [new-canvas (new canvas% 
                                   (parent resizable-panel)
                                   (editor text)
                                   (style '()))])
              
              (set-canvases!
               (let loop ([canvases canvases])
                 (cond
                   [(null? canvases) (error 'split "couldn't split; didn't find canvas")]
                   [else
                    (let ([canvas (car canvases)])
                      (if (eq? canvas canvas-to-be-split)
                          (list* new-canvas
                                 canvas
                                 (cdr canvases))
                          (cons canvas (loop (cdr canvases)))))])))
              
              (update-shown)
              
              ;; with-handlers prevents bad calls to set-percentages
              ;; might still leave GUI in bad state, however.
              (with-handlers ([exn:fail? (λ (x) (void))])
                (send resizable-panel set-percentages
                      (let loop ([canvases orig-canvases]
                                 [percentages orig-percentages])
                        (cond
                          [(null? canvases)
                           (error 'split "couldn't split; didn't find canvas")]
                          [(null? percentages)
                           (error 'split "wrong number of percentages: ~s ~s"
                                  orig-percentages
                                  (send resizable-panel get-children))]
                          [else (let ([canvas (car canvases)])
                                  (if (eq? canvas-to-be-split canvas)
                                      (list* (/ (car percentages) 2)
                                             (/ (car percentages) 2)
                                             (cdr percentages))
                                      (cons
                                       (car percentages)
                                       (loop (cdr canvases)
                                             (cdr percentages)))))]))))
              
              (set-visible-region new-canvas ox oy ow oh cursor-y)
              (set-visible-region canvas-to-be-split ox oy ow oh cursor-y)
              
              (send new-canvas focus))))
        
        ;; split-demand : menu-item -> void
        ;; enables the menu-item if splitting is allowed, disables otherwise
        (define/private (split-demand item)
          (let ([canvas-to-be-split (get-edit-target-window)])
            (send item enable
                  (or (memq canvas-to-be-split definitions-canvases)
                      (memq canvas-to-be-split interactions-canvases))))) 
        
        ;; collapse-demand : menu-item -> void
        ;; enables the menu-item if collapsing is allowed, disables otherwise
        (define/private (collapse-demand item)
          (let ([canvas-to-be-split (get-edit-target-window)])
            (cond
              [(memq canvas-to-be-split definitions-canvases)
               (send item enable (2 . <= . (length definitions-canvases)))]
              [(memq canvas-to-be-split interactions-canvases)
               (send item enable (2 . <= . (length interactions-canvases)))]
              [else
               (send item enable #f)])))
        
        ;; get-visible-region : editor-canvas -> number number number number (union #f number)
        ;; calculates the visible region of the editor in this editor-canvas, returning
        ;; four numbers for the x, y, width and height of the visible region
        ;; also, the last two booleans indiciate if the beginning and the end
        ;; of the selection was visible before the split, respectively.
        (define/private (get-visible-region canvas)
          (send canvas call-as-primary-owner
                (λ ()
                  (let* ([text (send canvas get-editor)]
                         [admin (send text get-admin)]
                         [start (send text get-start-position)]
                         [end (send text get-end-position)])
                    (let-values ([(x y w h) (get-visible-area admin)])
                      (let ([ysb (box 0)])
                        (send text position-location (send text get-start-position) #f ysb)
                        (values x y w h
                                (and (= start end)
                                     (<= y (unbox ysb) (+ y h))
                                     (unbox ysb)))))))))
        
        ;; set-visible-region : editor-canvas number number number number (union #f number) -> void
        ;; sets the visible region of the text displayed by the editor canvas
        ;; to be the middle of the region (vertically) specified by x, y, w, and h.
        ;; if start-visible? and/or end-visible? are true, some special handling
        ;; is done to try to keep the start and end visible, with precendence
        ;; given to start if both are #t.
        (define/private (set-visible-region canvas x y w h cursor-y)
          (send canvas call-as-primary-owner
                (λ ()
                  (let* ([text (send canvas get-editor)]
                         [admin (send text get-admin)]
                         [nwb (box 0)]
                         [nhb (box 0)])
                    (send admin get-view #f #f nwb nhb)
                    (let* ([nw (unbox nwb)]
                           [nh (unbox nhb)]
                           
                           [nx x]
                           [raw-y (- (+ y (/ h 2)) (/ nh 2))]
                           [ny (if (and cursor-y 
                                        (not (<= raw-y cursor-y (+ raw-y nh))))
                                   (- cursor-y (/ nh 2))
                                   raw-y)])
                      (send canvas scroll-to nx ny nw nh #t)
                      (void))))))
        
        ;; get-visible-area : admin -> number number number number
        ;; returns the visible area for this admin
        (define/private (get-visible-area admin)
          (let ([bx (box 0)]
                [by (box 0)]
                [bw (box 0)]
                [bh (box 0)])
            (send admin get-view bx by bw bh)
            (values (unbox bx)
                    (unbox by)
                    (unbox bw)
                    (unbox bh))))
        
        (define/public (collapse)
          (let* ([target (get-edit-target-window)])
            (cond
              [(memq target definitions-canvases)
               (collapse-definitions target)]
              [(memq target interactions-canvases)
               (collapse-interactions target)]
              [else (bell)])))
        
        (define/private (collapse-definitions target)
          (handle-collapse
           target
           (λ () definitions-canvases)
           (λ (c) (set! definitions-canvases c))))
        
        (define/private (collapse-interactions target)
          (handle-collapse
           target
           (λ () interactions-canvases)
           (λ (c) (set! interactions-canvases c))))
        
        (define/private (handle-collapse target get-canvases set-canvases!)
          (if (= 1 (length (get-canvases)))
              (bell)
              (let* ([old-percentages (send resizable-panel get-percentages)]
                     [soon-to-be-bigger-canvas #f]
                     [percentages
                      (if (eq? (car (get-canvases)) target)
                          (begin
                            (set! soon-to-be-bigger-canvas (cadr (get-canvases)))
                            (cons (+ (car old-percentages)
                                     (cadr old-percentages))
                                  (cddr old-percentages)))
                          (let loop ([canvases (cdr (get-canvases))]
                                     [prev-canvas (car (get-canvases))]
                                     [percentages (cdr old-percentages)]
                                     [prev-percentage (car old-percentages)])
                            (cond
                              [(null? canvases)
                               (error 'collapse "internal error.1")]
                              [(null? percentages)
                               (error 'collapse "internal error.2")]
                              [else
                               (if (eq? (car canvases) target)
                                   (begin
                                     (set! soon-to-be-bigger-canvas prev-canvas)
                                     (cons (+ (car percentages)
                                              prev-percentage)
                                           (cdr percentages)))
                                   (cons prev-percentage
                                         (loop (cdr canvases)
                                               (car canvases)
                                               (cdr percentages)
                                               (car percentages))))])))])
                (unless soon-to-be-bigger-canvas
                  (error 'collapse "internal error.3"))
                (set-canvases! (remq target (get-canvases)))
                (update-shown)
                
                (let ([target-admin 
                       (send target call-as-primary-owner
                             (λ ()
                               (send (send target get-editor) get-admin)))]
                      [to-be-bigger-admin 
                       (send soon-to-be-bigger-canvas call-as-primary-owner
                             (λ ()
                               (send (send soon-to-be-bigger-canvas get-editor) get-admin)))])
                  (let-values ([(bx by bw bh) (get-visible-area target-admin)])
                    
                    ;; this line makes the soon-to-be-bigger-canvas bigger
                    ;; if it fails, we're out of luck, but at least we don't crash.
                    (with-handlers ([exn:fail? (λ (x) (void))])
                      (send resizable-panel set-percentages percentages))
                    
                    (let-values ([(ax ay aw ah) (get-visible-area to-be-bigger-admin)])
                      (send soon-to-be-bigger-canvas scroll-to
                            bx
                            (- by (/ (- ah bh) 2))
                            aw
                            ah
                            #t))))
                
                (send target set-editor #f)
                (send soon-to-be-bigger-canvas focus))))
        ;                                                                          
        ;                                                                          
        ;                                                                          
        ;          ;                                                               
        ;          ;                                                               
        ;          ;                                                               
        ;    ;;;   ; ;;     ;;;   ;   ;   ;      ; ;;  ;;     ;;;   ; ;;    ;   ;  
        ;   ;      ;;  ;   ;   ;  ;   ;   ;      ;;  ;;  ;   ;   ;  ;;  ;   ;   ;  
        ;   ;;     ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;    ;  ;   ;   ;   ;  
        ;    ;;    ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;;;;;;  ;   ;   ;   ;  
        ;      ;   ;   ;  ;     ;  ; ; ; ;       ;   ;   ;  ;       ;   ;   ;   ;  
        ;      ;   ;   ;   ;   ;    ;   ;        ;   ;   ;   ;      ;   ;   ;  ;;  
        ;   ;;;    ;   ;    ;;;     ;   ;        ;   ;   ;    ;;;;  ;   ;    ;; ;  
        ;                                                                          
        ;                                                                          
        ;                                                                          
        
        
        (define interactions-shown? #t)
        (define definitions-shown? #t)
        
        (define/private (toggle-show/hide-definitions)
          (set! definitions-shown? (not definitions-shown?))
          (unless definitions-shown?
            (set! interactions-shown? #t)))
        (define/private (toggle-show/hide-interactions)
          (set! interactions-shown? (not interactions-shown?))
          (unless  interactions-shown?
            (set! definitions-shown? #t)))
        
        (define/override (update-shown)
          (super update-shown)
          (let ([new-children
                 (foldl
                  (λ (shown? children sofar)
                    (if shown?
                        (append children sofar)
                        sofar))
                  null
                  (list interactions-shown?
                        definitions-shown?)
                  (list interactions-canvases
                        definitions-canvases))]
                [old-children (send resizable-panel get-children)]
                [p (preferences:get 'drscheme:unit-window-size-percentage)])
            (update-defs/ints-resize-corner)
            (send definitions-item set-label 
                  (if definitions-shown?
                      (string-constant hide-definitions-menu-item-label)
                      (string-constant show-definitions-menu-item-label)))
            (send interactions-item set-label 
                  (if interactions-shown?
                      (string-constant hide-interactions-menu-item-label)
                      (string-constant show-interactions-menu-item-label)))
            (send resizable-panel begin-container-sequence)
            
            ;; this might change the unit-window-size-percentage, so save/restore it
            (send resizable-panel change-children (λ (l) new-children))
            
            (preferences:set 'drscheme:unit-window-size-percentage p)
            ;; restore preferred interactions/definitions sizes
            (when (and (= 1 (length definitions-canvases))
                       (= 1 (length interactions-canvases))
                       (= 2 (length new-children)))
              (with-handlers ([exn:fail? (λ (x) (void))])
                (send resizable-panel set-percentages
                      (list p (- 1 p)))))
            
            (send resizable-panel end-container-sequence)
            (when (ormap (λ (child)
                           (and (is-a? child editor-canvas%)
                                (not (send child has-focus?))))
                         (send resizable-panel get-children))
              (let ([new-focus
                     (let loop ([children (send resizable-panel get-children)])
                       (cond
                         [(null? children) (void)]
                         [else (let ([child (car children)])
                                 (if (is-a? child editor-canvas%)
                                     child
                                     (loop (cdr children))))]))]
                    [old-focus
                     (ormap (λ (x) (and (is-a? x editor-canvas%) (send x has-focus?) x))
                            old-children)])
                
                ;; conservatively, only scroll when the focus stays in the same place.
                (when old-focus
                  (when (eq? old-focus new-focus)
                    (let ([ed (send old-focus get-editor)])
                      (when ed
                        (send ed scroll-to-position 
                              (send ed get-start-position)
                              #f
                              (send ed get-end-position))))))
                
                (send new-focus focus)))
            
            (for-each
             (λ (get-item)
               (let ([item (get-item)])
                 (when item
                   (send item enable definitions-shown?))))
             (list (λ () (file-menu:get-revert-item))
                   (λ () (file-menu:get-save-item))
                   (λ () (file-menu:get-save-as-item))
                   ;(λ () (file-menu:save-as-text-item)) ; Save As Text...
                   (λ () (file-menu:get-print-item))))
            (send file-menu:print-interactions-item enable interactions-shown?)))
        
        (define/augment (can-close?)
          (and (andmap (lambda (tab)
                         (or (eq? tab current-tab)
                             (and (send (send tab get-defs) can-close?)
                                  (send (send tab get-ints) can-close?))))
                       tabs)
               (send interactions-text can-close?)
               (inner #t can-close?)))
        (define/augment (on-close)
          (inner (void) on-close)
          (for-each (lambda (tab)
                      (unless (eq? tab current-tab)
                        (send (send tab get-defs) on-close)
                        (send (send tab get-ints) on-close)))
                    tabs)
          (when (eq? this newest-frame)
            (set! newest-frame #f))
          (when transcript
            (stop-transcript))
          (remove-show-status-line-callback)
          (remove-bug-icon-callback)
          (send interactions-text on-close))
        
        ;; execute-callback : -> void
        ;; uses the state of the button to determine if an execution is
        ;; already running. This function is called from many places, not
        ;; just the execute button.
        (define/public (execute-callback)
          (when (send execute-button is-enabled?)
            
            ;; if the language is not-a-language, and the buffer looks like a module,
            ;; automatically make the switch to the module language
            (let ([next-settings (send definitions-text get-next-settings)])
              (when (is-a? (drscheme:language-configuration:language-settings-language next-settings) 
                           drscheme:language-configuration:not-a-language-language<%>)
                (when (looks-like-module? definitions-text)
                  (let-values ([(module-language module-language-settings) (get-module-language/settings)])
                    (when (and module-language module-language-settings)
                      (send definitions-text set-next-settings 
                            (drscheme:language-configuration:make-language-settings
                             module-language
                             module-language-settings)))))))
            
            (check-if-save-file-up-to-date)
            (when (preferences:get 'drscheme:show-interactions-on-execute)
              (ensure-rep-shown interactions-text))
            (when transcript
              (record-definitions)
              (record-interactions))
            (send definitions-text just-executed)
            (send language-message set-yellow #f)
            (send interactions-canvas focus)
            (send interactions-text reset-console)
            (send interactions-text clear-undos)
            
            (let ([start 0])
              (send definitions-text split-snip start)
              (let* ([name (send definitions-text get-port-name)]
                     [text-port (open-input-text-editor definitions-text start 'end values name #t)])
                (port-count-lines! text-port)
                (let* ([line (send definitions-text position-paragraph start)]
                       [column (- start (send definitions-text paragraph-start-position line))]
                       [relocated-port (relocate-input-port text-port 
                                                            (+ line 1)
                                                            column
                                                            (+ start 1))])
                  (port-count-lines! relocated-port)
                  (send interactions-text evaluate-from-port
                        relocated-port
                        #t
                        (λ ()
                          (send interactions-text clear-undos))))))))
        
        (inherit revert save)
        (define/private (check-if-save-file-up-to-date)
          (when (send definitions-text save-file-out-of-date?)
            (let ([user-choice 
                   (message-box/custom
                    (string-constant drscheme)
                    (string-constant definitions-modified)
                    (string-constant ignore)
                    (string-constant revert)
                    #f
                    this
                    '(caution default=2 number-order)
                    1)])
              (case user-choice
                [(1) (void)]
                [(2) (revert)]))))
        
        (inherit get-menu-bar get-focus-object get-edit-target-object)
        
        (inherit is-maximized?)
        (define/override (on-size w h)
          (unless (is-maximized?)
            (preferences:set 'drscheme:unit-window-width w)
            (preferences:set 'drscheme:unit-window-height h))
          (preferences:set 'drscheme:unit-window-max? (is-maximized?))
          (super on-size w h))
        
        (define on-move-timer-args #f)
        (define on-move-timer #f)
        (define/override (on-move x y)
          (cond
            [on-move-timer
             (set! on-move-timer-args (cons x y))]
            [else
             (set! on-move-timer-args (cons x y))
             (set! on-move-timer 
                   (new timer% 
                        [notify-callback
                         (λ () 
                           (set! on-move-timer #f)
                           (set! on-move-timer-args #f)
                           (preferences:set 'drscheme:frame:initial-position on-move-timer-args))]
                        [interval 1000]
                        [just-once? #t]))]))
        
        (define/override (get-editor) definitions-text)
        (define/override (get-canvas)
          (initialize-definitions-canvas)
          definitions-canvas)
        (define/private (initialize-definitions-canvas)
          (unless definitions-canvas
            (set! definitions-canvas
                  (new (drscheme:get/extend:get-definitions-canvas)
                       (parent resizable-panel)
                       (editor definitions-text)))))
        
        (define/override (get-delegated-text) definitions-text)
        (define/override (get-open-here-editor) definitions-text)
        
        ;; wire the definitions text to the interactions text and initialize it.
        (define/private (init-definitions-text tab)
          (let ([defs (send tab get-defs)]
                [ints (send tab get-ints)])
            (send defs set-interactions-text ints)
            (send defs set-tab tab)
            (send ints set-definitions-text defs)
            (send defs change-mode-to-match)))
        
        
        ;                              
        ;                              
        ;                @@            
        ;    @            @            
        ;   @@@@@   $@$:  @-@$   :@@+@ 
        ;    @        -@  @+ *$  @$ -@ 
        ;    @     -$@$@  @   @  :@@$- 
        ;    @     $*  @  @   @     *@ 
        ;    @: :$ @- *@  @  +$  @  :@ 
        ;    :@@$- -$$-@@@@+@$   $+@@: 
        ;                              
        ;                              
        ;                              
        ;                              
        
        (define/public (get-current-tab) current-tab)
        
        ;; create-new-tab : -> void
        ;; creates a new tab and updates the GUI for that new tab
        (define/private create-new-tab
          (lambda ([filename #f])
            (let* ([defs (new (drscheme:get/extend:get-definitions-text))]
                   [tab-count (length tabs)]
                   [new-tab (new (drscheme:get/extend:get-tab)
                                 (defs defs)
                                 (i tab-count)
                                 (frame this)
                                 (defs-shown? #t)
                                 (ints-shown? (not filename)))]
                   [ints (make-object (drscheme:get/extend:get-interactions-text) new-tab)])
              (send new-tab set-ints ints)
              (set! tabs (append tabs (list new-tab)))
              (send tabs-panel append 
                    (gui-utils:trim-string
                     (if filename
                         (get-tab-label-from-filename filename)
                         (get-defs-tab-label defs #f))
                     200))
              (init-definitions-text new-tab)
              (when filename (send defs load-file filename))
              (change-to-nth-tab (- (send tabs-panel get-number) 1))
              (send ints initialize-console)
              (send tabs-panel set-selection (- (send tabs-panel get-number) 1))
              (set! newest-frame this)
              (update-menu-bindings))))
        
        ;; change-to-tab : tab -> void
        ;; updates current-tab, definitions-text, and interactactions-text
        ;; to be the nth tab. Also updates the GUI to show the new tab
        (inherit begin-container-sequence end-container-sequence)
        (define/private (change-to-tab tab)
          (let ([old-delegate (send definitions-text get-delegate)]
                [old-tab current-tab])
            (save-visible-tab-regions)
            (set! current-tab tab)
            (set! definitions-text (send current-tab get-defs))
            (set! interactions-text (send current-tab get-ints))
            
            
            (begin-container-sequence)
            (for-each (λ (defs-canvas) (send defs-canvas set-editor definitions-text #f))
                      definitions-canvases)
            (for-each (λ (ints-canvas) (send ints-canvas set-editor interactions-text #f))
                      interactions-canvases)
            
            (update-save-message)
            (update-save-button)
            (language-changed)
            
            (send definitions-text update-frame-filename)
            (send definitions-text set-delegate old-delegate)
            (update-running (send current-tab is-running?))
            (on-tab-change old-tab current-tab)
            (send tab update-log)
            (send tab update-planet-status)
            (send tab update-execute-warning-gui)
            (restore-visible-tab-regions)
            (for-each (λ (defs-canvas) (send defs-canvas refresh))
                      definitions-canvases)
            (for-each (λ (ints-canvas) (send ints-canvas refresh))
                      interactions-canvases)
            (end-container-sequence)))
        
        (define/pubment (on-tab-change from-tab to-tab)
          (let ([old-enabled (send from-tab get-enabled)]
                [new-enabled (send to-tab get-enabled)])
            (unless (eq? old-enabled new-enabled)
              (if new-enabled
                  (enable-evaluation)
                  (disable-evaluation))))
          
          (let ([from-defs (send from-tab get-defs)]
                [to-defs (send to-tab get-defs)])
            (let ([delegate (send from-defs get-delegate)])
              (send from-defs set-delegate #f)
              (send to-defs set-delegate delegate)))
                    
          (inner (void) on-tab-change from-tab to-tab))
        
        (define/public (next-tab) (change-to-delta-tab +1))
        (define/public (prev-tab) (change-to-delta-tab -1))
        
        (define/private (change-to-delta-tab dt)
          (change-to-nth-tab (modulo (+ (send current-tab get-i) dt) (length tabs))))
        
        (define/public-final (close-current-tab)
          (cond
            [(null? tabs) (void)]
            [(null? (cdr tabs)) (void)]
            [else
             (let loop ([l-tabs tabs])
               (cond
                 [(null? l-tabs) (error 'close-current-tab "uh oh.3")]
                 [else
                  (let ([tab (car l-tabs)])
                    (if (eq? tab current-tab)
                        (when (close-tab tab)
                          (for-each (lambda (t) (send t set-i (- (send t get-i) 1)))
                                    (cdr l-tabs))
                          (set! tabs (remq tab tabs))
                          (send tabs-panel delete (send tab get-i))
                          (update-menu-bindings) 
                          (change-to-tab (cond
                                           [(< (send tab get-i) (length tabs))
                                            (list-ref tabs (send tab get-i))]
                                           [else (last tabs)])))
                        (loop (cdr l-tabs))))]))]))
        
        ;; a helper private method for close-current-tab -- doesn't close an arbitrary tab.
        (define/private (close-tab tab)
          (cond
            [(send tab can-close?)
             (send tab on-close)
             #t]
            [else #f]))
        
        (define/public (open-in-new-tab filename)
          (create-new-tab filename))
        
        (define/public (get-tab-count) (length tabs))
        (define/public (change-to-nth-tab n)
          (unless (< n (length tabs))
            (error 'change-to-nth-tab "number too big ~s" n))
          (change-to-tab (list-ref tabs n)))
        
        (define/private (save-visible-tab-regions)
          (send current-tab set-visible-ints
                (get-tab-visible-regions interactions-text)
                interactions-shown?)
          (send current-tab set-visible-defs 
                (get-tab-visible-regions definitions-text)
                definitions-shown?)
          (send current-tab set-focus-d/i
                (if (ormap (λ (x) (send x has-focus?)) interactions-canvases)
                    'ints
                    'defs)))
        
        (define/private (get-tab-visible-regions txt)
          (map (λ (canvas) 
                 (let-values ([(x y w h _) (get-visible-region canvas)])
                   (list x y w h)))
               (send txt get-canvases)))
        
        (inherit set-text-to-search reflow-container)
        (define/private (restore-visible-tab-regions)
          (define (fix-up-canvas-numbers txt regions ints?)
            (when regions
              (let* ([canvases (send txt get-canvases)]
                     [canvases-count (length canvases)]
                     [regions-count (length regions)])
                (cond
                  [(> canvases-count regions-count)
                   (let loop ([i (- canvases-count regions-count)]
                              [canvases canvases])
                     (unless (zero? i)
                       (if ints?
                           (collapse-interactions (car canvases))
                           (collapse-definitions (car canvases)))
                       (loop (- i 1)
                             (cdr canvases))))]
                  [(= canvases-count regions-count)
                   (void)]
                  [(< canvases-count regions-count)
                   (let loop ([i (- regions-count canvases-count)]
                              [canvases canvases])
                     (unless (zero? i)
                       (if ints?
                           (split-interactions (car canvases))
                           (split-definitions (car canvases)))
                       (loop (- i 1) 
                             (cdr canvases))))]))))
          
          (define (set-visible-regions txt regions)
            (when regions
              (for-each (λ (canvas region) 
                          (let ([admin (send txt get-admin)])
                            (send admin scroll-to 
                                  (first region)
                                  (second region)
                                  (third region)
                                  (fourth region)))) 
                        (send txt get-canvases)
                        regions)))
          
          (let-values ([(vi is?) (send current-tab get-visible-ints)]
                       [(vd ds?) (send current-tab get-visible-defs)])
            (set! interactions-shown? is?)
            (set! definitions-shown? ds?)
            (update-shown)
            (reflow-container) ;; without this one, the percentages in the resizable-panel are not up to date with the children
            (fix-up-canvas-numbers definitions-text vd #f)
            (fix-up-canvas-numbers interactions-text vi #t)
            (reflow-container)
            (set-visible-regions definitions-text vd)
            (set-visible-regions interactions-text vi))
          (case (send current-tab get-focus-d/i)
            [(defs) 
             (send (car definitions-canvases) focus)
             (set-text-to-search (send (car definitions-canvases) get-editor))]
            [(ints)
             (send (car interactions-canvases) focus)
             (set-text-to-search (send (car interactions-canvases) get-editor))]))
        
        (define/private (pathname-equal? p1 p2)
          (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
            (string=? (path->string (normal-case-path (normalize-path p1)))
                      (path->string (normal-case-path (normalize-path p2))))))
        
        (define/override (make-visible filename)
          (let ([tab (find-matching-tab filename)])
            (when tab
              (change-to-tab tab))))

        (define/private (find-matching-tab filename)
          (let loop ([tabs tabs])
            (cond
              [(null? tabs) #f]
              [else
               (let* ([tab (car tabs)]
                      [tab-filename (send (send tab get-defs) get-filename)])
                 (if (and tab-filename
                          (pathname-equal? filename tab-filename))
                     tab
                     (loop (cdr tabs))))])))
        
        (define/override (editing-this-file? filename)
          (ormap (λ (tab)
                   (let ([fn (send (send tab get-defs) get-filename)])
                     (and fn
                          (pathname-equal? fn filename))))
                 tabs))
        
        (define/override (get-menu-item%)
          (class (super get-menu-item%)
            (inherit get-label get-plain-label)
            (define/override (restore-keybinding)
              (cond
                [(equal? (get-plain-label) (string-constant close))
                 (update-close-menu-item-shortcut this)]
                [(equal? (get-plain-label) (string-constant close-tab))
                 (update-close-tab-menu-item-shortcut this)]
                [else (super restore-keybinding)]))
            (super-new)))
        
        (define/private (update-menu-bindings)
          (when (preferences:get 'framework:menu-bindings)
            (when close-tab-menu-item
              (update-close-tab-menu-item-shortcut close-tab-menu-item))
            (update-close-menu-item-shortcut (file-menu:get-close-item))))
        
        (define/private (update-close-tab-menu-item-shortcut item)
          (let ([just-one? (and (pair? tabs) (null? (cdr tabs)))])
            (send item set-label (if just-one? 
                                     (string-constant close-tab)
                                     (string-constant close-tab-amp)))
            (send item set-shortcut (if just-one? #f #\w))))
        
        (define/private (update-close-menu-item-shortcut item)
          (let ([just-one? (and (pair? tabs) (null? (cdr tabs)))])
            (send item set-label (if just-one? 
                                     (string-constant close-menu-item)
                                     (string-constant close)))
            (send item set-shortcut (if just-one? #\w #f))))
        
        ;; offer-to-save-file : path -> void
        ;; bring the tab that edits the file named by `path' to the front
        ;; and opens a dialog asking if it should be saved.
        (define/public (offer-to-save-file path)
          (let ([original-tab current-tab]
                [tab-to-save (find-matching-tab path)])
            (when tab-to-save
              (let ([defs-to-save (send tab-to-save get-defs)])
                (when (send defs-to-save is-modified?)
                  (unless (eq? tab-to-save original-tab)
                    (change-to-tab tab-to-save))
                  (send defs-to-save user-saves-or-not-modified? #f)
                  (unless (eq? tab-to-save original-tab)
                    (change-to-tab original-tab)))))))
                  
              
        ;;
        ;; end tabs
        ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        (define/public (get-definitions-text) definitions-text)
        (define/public (get-interactions-text) interactions-text)
        
        (define/public (get-definitions/interactions-panel-parent)
          toolbar/rest-panel)
        
        (inherit delegated-text-shown? hide-delegated-text show-delegated-text)
        (define/override (add-show-menu-items show-menu)
          (super add-show-menu-items show-menu)
          (set! definitions-item
                (make-object menu:can-restore-menu-item%
                  (string-constant hide-definitions-menu-item-label)
                  (get-show-menu)
                  (λ (_1 _2) 
                    (toggle-show/hide-definitions)
                    (update-shown))
                  #\d
                  (string-constant definitions-menu-item-help-string)))
          (set! interactions-item
                (make-object menu:can-restore-menu-item%
                  (string-constant show-interactions-menu-item-label)
                  (get-show-menu)
                  (λ (_1 _2) 
                    (toggle-show/hide-interactions)
                    (update-shown))
                  #\e
                  (string-constant interactions-menu-item-help-string)))
          
          (new menu:can-restore-menu-item%
               (shortcut #\u)
               (label 
                (if (delegated-text-shown?)
                    (string-constant hide-overview)
                    (string-constant show-overview)))
               (parent (get-show-menu))
               (callback
                (λ (menu evt)
                  (if (delegated-text-shown?)
                      (begin
                        (send menu set-label (string-constant show-overview))
                        (preferences:set 'framework:show-delegate? #f)
                        (hide-delegated-text))
                      (begin
                        (send menu set-label (string-constant hide-overview))
                        (preferences:set 'framework:show-delegate? #t)
                        (show-delegated-text))))))
          
          (set! module-browser-menu-item
                (new menu:can-restore-menu-item%
                     (label (if module-browser-shown?
                                (string-constant hide-module-browser)
                                (string-constant show-module-browser)))
                     (parent (get-show-menu))
                     (callback
                      (λ (menu evt)
                        (if module-browser-shown?
                            (hide-module-browser)
                            (show-module-browser))))))
          
          (set! toolbar-menu (new menu% 
                                  [parent show-menu]
                                  [label (string-constant toolbar)]))
          (set! toolbar-left-menu-item
                (new checkable-menu-item%
                     [label (string-constant toolbar-on-left)]
                     [parent toolbar-menu]
                     [callback (λ (x y) (set-toolbar-left))]
                     [checked #f]))
          (set! toolbar-top-menu-item
                (new checkable-menu-item%
                     [label (string-constant toolbar-on-top)]
                     [parent toolbar-menu]
                     [callback (λ (x y) (set-toolbar-top))]
                     [checked #f]))
          (set! toolbar-right-menu-item
                (new checkable-menu-item%
                     [label (string-constant toolbar-on-right)]
                     [parent toolbar-menu]
                     [callback (λ (x y) (set-toolbar-right))]
                     [checked #f]))
          (set! toolbar-hidden-menu-item
                (new checkable-menu-item%
                     [label (string-constant toolbar-hidden)]
                     [parent toolbar-menu]
                     [callback (λ (x y) (set-toolbar-hidden))]
                     [checked #f]))
          
          (set! logger-menu-item
                (new menu-item%
                     [label (string-constant show-log)]
                     [parent show-menu]
                     [callback
                      (λ (x y) (send current-tab toggle-log))]))
          )
        
        
        ;                                                                                                       
        ;                                                                                                       
        ;                                                                                                       
        ;                           ;           ;              ;                                                
        ;                           ;           ;              ;                                                
        ;                           ;           ;              ;                                                
        ;   ; ;;  ;;     ;;;     ;; ;   ;   ;   ;    ;;;       ; ;;    ; ;   ;;;   ;   ;   ;   ;;;    ;;;   ; ; 
        ;   ;;  ;;  ;   ;   ;   ;  ;;   ;   ;   ;   ;   ;      ;;  ;   ;;   ;   ;  ;   ;   ;  ;      ;   ;  ;;  
        ;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;    ;      ;    ;  ;   ;     ;  ; ; ; ;   ;;    ;    ;  ;   
        ;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;;;;;;      ;    ;  ;   ;     ;  ; ; ; ;    ;;   ;;;;;;  ;   
        ;   ;   ;   ;  ;     ; ;    ;   ;   ;   ;  ;           ;    ;  ;   ;     ;  ; ; ; ;      ;  ;       ;   
        ;   ;   ;   ;   ;   ;   ;  ;;   ;  ;;   ;   ;          ;;  ;   ;    ;   ;    ;   ;       ;   ;      ;   
        ;   ;   ;   ;    ;;;     ;; ;    ;; ;   ;    ;;;;      ; ;;    ;     ;;;     ;   ;    ;;;     ;;;;  ;   
        ;                                                                                                       
        ;                                                                                                       
        ;                                                                                                       
        
        
        (field [module-browser-shown? #f]
               [module-browser-parent-panel #f]
               [module-browser-panel #f]
               [module-browser-ec #f]
               [module-browser-button #f]
               [module-browser-lib-path-check-box #f]
               [module-browser-planet-path-check-box #f]
               [module-browser-name-length-choice #f]
               [module-browser-pb #f]
               [module-browser-menu-item 'module-browser-menu-item-unset])
        
        (inherit open-status-line close-status-line update-status-line)
        
        (define/private (show-module-browser)
          (when module-browser-panel
            (when (can-browse-language?)
              (set! module-browser-shown? #t)
              (send module-browser-menu-item set-label (string-constant hide-module-browser))
              (update-module-browser-pane))))
        
        (define/private (hide-module-browser)
          (when module-browser-panel
            (set! module-browser-shown? #f)
            (send module-browser-menu-item set-label (string-constant show-module-browser))
            (close-status-line 'plt:module-browser:mouse-over)
            (send module-browser-parent-panel change-children
                  (λ (l)
                    (remq module-browser-panel l)))))
        
        (define/private (can-browse-language?)
          (let* ([lang/config (send (get-definitions-text) get-next-settings)]
                 [lang (drscheme:language-configuration:language-settings-language lang/config)]
                 [strs (send lang get-language-position)]
                 [can-browse?
                  (or (is-a? lang drscheme:module-language:module-language<%>)
                      (ormap (λ (x) (regexp-match #rx"PLT" x))
                             strs))])
            (unless can-browse?
              (message-box (string-constant drscheme)
                           (string-constant module-browser-only-in-plt-and-module-langs)))
            can-browse?))
        
        (define/private (update-module-browser-pane)
          (open-status-line 'plt:module-browser:mouse-over)
          (send module-browser-panel begin-container-sequence)
          (unless module-browser-ec 
            (set! module-browser-pb 
                  (drscheme:module-overview:make-module-overview-pasteboard
                   #t
                   (λ (x) (mouse-currently-over x))))
            (set! module-browser-ec (make-object editor-canvas%
                                      module-browser-panel
                                      module-browser-pb))
            
            (let* ([show-callback
                    (λ (cb key)
                      (if (send cb get-value)
                          (send module-browser-pb show-visible-paths key)
                          (send module-browser-pb remove-visible-paths key))
                      (preferences:set 'drscheme:module-browser:hide-paths (send module-browser-pb get-hidden-paths)))]
                   [mk-checkbox
                    (λ (key label)
                      (new check-box%
                           (parent module-browser-panel)
                           (label label)
                           (value (not (memq key (preferences:get 'drscheme:module-browser:hide-paths))))
                           (callback 
                            (λ (cb _) 
                              (show-callback cb key)))))])
              (set! module-browser-lib-path-check-box (mk-checkbox 'lib show-lib-paths))
              (set! module-browser-planet-path-check-box (mk-checkbox 'planet show-planet-paths)))
            
            (set! module-browser-name-length-choice
                  (new choice%
                       (parent module-browser-panel)
                       (label (string-constant module-browser-name-length))
                       (choices (list (string-constant module-browser-name-short)
                                      (string-constant module-browser-name-medium)
                                      (string-constant module-browser-name-long)
                                      (string-constant module-browser-name-very-long)))
                       (selection (preferences:get 'drscheme:module-browser:name-length))
                       (callback
                        (λ (x y)
                          (let ([selection (send module-browser-name-length-choice get-selection)])
                            (preferences:set 'drscheme:module-browser:name-length selection)
                            (update-module-browser-name-length selection))))))
            (update-module-browser-name-length 
             (preferences:get 'drscheme:module-browser:name-length))
            
            (set! module-browser-button 
                  (new button%
                       (parent module-browser-panel)
                       (label refresh)
                       (callback (λ (x y) (update-module-browser-pane)))
                       (stretchable-width #t))))
          
          (let ([p (preferences:get 'drscheme:module-browser-size-percentage)])
            (send module-browser-parent-panel change-children
                  (λ (l)
                    (cons module-browser-panel
                          (remq module-browser-panel l))))
            (with-handlers ([exn:fail? void])
              (send module-browser-parent-panel set-percentages (list p (- 1 p))))
            (send module-browser-parent-panel end-container-sequence)
            (calculate-module-browser)))
        
        (define/private (update-module-browser-name-length i)
          (send module-browser-pb set-name-length 
                (case i
                  [(0) 'short]
                  [(1) 'medium]
                  [(2) 'long]
                  [(3) 'very-long])))
        
        (define/private (mouse-currently-over snips)
          (if (null? snips)
              (update-status-line 'plt:module-browser:mouse-over #f)
              (let* ([snip (car snips)]
                     [lines (send snip get-lines)]
                     [name (or (send snip get-filename)
                               (send snip get-word))]
                     [str (if lines
                              (format (string-constant module-browser-filename-format) name lines)
                              name)])
                (update-status-line 'plt:module-browser:mouse-over str))))
        
        (define/private (calculate-module-browser)
          (let ([mod-tab current-tab])
            (let-values ([(old-break-thread old-custodian) (send mod-tab get-breakables)])
              (open-status-line 'plt:module-browser)
              (update-status-line 'plt:module-browser status-compiling-definitions)
              (send module-browser-button enable #f)
              (send module-browser-lib-path-check-box enable #f)
              (send module-browser-planet-path-check-box enable #f)
              (send module-browser-name-length-choice enable #f)
              (disable-evaluation-in-tab current-tab)
              (drscheme:module-overview:fill-pasteboard 
               module-browser-pb
               (drscheme:language:make-text/pos
                definitions-text
                0
                (send definitions-text last-position))
               (λ (str) (update-status-line 
                         'plt:module-browser 
                         (format module-browser-progress-constant str)))
               (λ (user-thread user-custodian)
                 (send mod-tab set-breakables user-thread user-custodian)))
              (send mod-tab set-breakables old-break-thread old-custodian)
              (send mod-tab enable-evaluation)
              (send module-browser-button enable #t)
              (send module-browser-lib-path-check-box enable #t)
              (send module-browser-planet-path-check-box enable #t)
              (send module-browser-name-length-choice enable #t)
              (close-status-line 'plt:module-browser))))
        
        ;; set-directory : text -> void
        ;; sets the current-directory and current-load-relative-directory
        ;; based on the file saved in the definitions-text
        (define/private (set-directory definitions-text)
          (let* ([tmp-b (box #f)]
                 [fn (send definitions-text get-filename tmp-b)])
            (unless (unbox tmp-b)
              (when fn
                (let-values ([(base name dir?) (split-path fn)])
                  (current-directory base)
                  (current-load-relative-directory base))))))
        
        
        ;                                            
        ;                                            
        ;                                            
        ;                                            
        ;                                            
        ;                                            
        ;   ; ;;  ;;     ;;;   ; ;;    ;   ;    ;;;  
        ;   ;;  ;;  ;   ;   ;  ;;  ;   ;   ;   ;     
        ;   ;   ;   ;  ;    ;  ;   ;   ;   ;   ;;    
        ;   ;   ;   ;  ;;;;;;  ;   ;   ;   ;    ;;   
        ;   ;   ;   ;  ;       ;   ;   ;   ;      ;  
        ;   ;   ;   ;   ;      ;   ;   ;  ;;      ;  
        ;   ;   ;   ;    ;;;;  ;   ;    ;; ;   ;;;   
        ;                                            
        ;                                            
        ;                                            
        
        (define execute-menu-item #f)
        (define file-menu:print-interactions-item #f)
        (define file-menu:create-new-tab-item #f)
        
        (define/override (file-menu:between-new-and-open file-menu)
          (set! file-menu:create-new-tab-item
                (new menu:can-restore-menu-item%
                     (label (string-constant new-tab))
                     (shortcut #\=)
                     (parent file-menu)
                     (callback
                      (λ (x y)
                        (create-new-tab))))))
        [define/override file-menu:between-open-and-revert
          (lambda (file-menu)
            (super file-menu:between-open-and-revert file-menu)
            (make-object separator-menu-item% file-menu))]
        (define close-tab-menu-item #f)
        (define/override (file-menu:between-close-and-quit file-menu)
          (set! close-tab-menu-item
                (new (get-menu-item%)
                     (label (string-constant close-tab))
                     (demand-callback
                      (λ (item)
                        (send item enable (1 . < . (send tabs-panel get-number)))))
                     (parent file-menu)
                     (callback
                      (λ (x y)
                        (close-current-tab)))))
          (super file-menu:between-close-and-quit file-menu))
        
        (define/override (file-menu:save-string) (string-constant save-definitions))
        (define/override (file-menu:save-as-string) (string-constant save-definitions-as))
        (define/override (file-menu:between-save-as-and-print file-menu)
          (let ([sub-menu (make-object menu% (string-constant save-other) file-menu)])
            (make-object menu:can-restore-menu-item%
              (string-constant save-definitions-as-text)
              sub-menu
              (λ (_1 _2)
                (let ([filename (send definitions-text put-file #f #f)])
                  (when filename
                    (send definitions-text save-file/gui-error filename 'text)))))
            (make-object menu:can-restore-menu-item%
              (string-constant save-interactions)
              sub-menu
              (λ (_1 _2) 
                (send interactions-text save-file/gui-error)))
            (make-object menu:can-restore-menu-item%
              (string-constant save-interactions-as)
              sub-menu
              (λ (_1 _2)
                (let ([filename (send interactions-text put-file #f #f)])
                  (when filename
                    (send interactions-text save-file/gui-error filename 'standard)))))
            (make-object menu:can-restore-menu-item%
              (string-constant save-interactions-as-text)
              sub-menu
              (λ (_1 _2)
                (let ([filename (send interactions-text put-file #f #f)])
                  (when filename
                    (send interactions-text save-file/gui-error filename 'text)))))
            (make-object separator-menu-item% file-menu)
            (set! transcript-menu-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant log-definitions-and-interactions)
                    file-menu
                    (λ (x y)
                      (if transcript
                          (stop-transcript)
                          (start-transcript)))))
            (make-object separator-menu-item% file-menu)
            (super file-menu:between-save-as-and-print file-menu)))
        
        [define/override file-menu:print-string (λ () (string-constant print-definitions))]
        (define/override (file-menu:between-print-and-close file-menu)
          (set! file-menu:print-interactions-item
                (make-object menu:can-restore-menu-item%
                  (string-constant print-interactions)
                  file-menu
                  (λ (_1 _2)
                    (send interactions-text print
                          #t 
                          #t
                          (preferences:get 'framework:print-output-mode)))))
          (super file-menu:between-print-and-close file-menu))
        
        (define/override (edit-menu:between-find-and-preferences edit-menu)
          (super edit-menu:between-find-and-preferences edit-menu)
          (new menu:can-restore-menu-item%
               [label (string-constant complete-word)]
               [shortcut #\/]
               [parent edit-menu]
               [demand-callback
                (λ (mi)
                  (send mi enable
                        (let ([ed (get-edit-target-object)])
                          (and ed
                               (is-a? ed text:autocomplete<%>)))))]
               [callback (λ (x y)
                           (send (get-edit-target-object) auto-complete))])
          (add-modes-submenu edit-menu))
        
        (define/override (edit-menu:between-select-all-and-find edit-menu)
          (new menu:can-restore-checkable-menu-item%
               [label (string-constant overwrite-mode)]
               [parent edit-menu]
               [demand-callback
                (λ (mi)
                  (let ([target (get-edit-target-object)])
                    (send mi enable (is-a? target text%))
		    (when (is-a? target text%)
                      (send mi check (and target (send target get-overwrite-mode))))))]
               [callback (λ (x y)
                           (let ([target (get-edit-target-object)])
                             (send target set-overwrite-mode
                                   (not (send target get-overwrite-mode)))))])
          (super edit-menu:between-select-all-and-find edit-menu))
        
        ;; capability-menu-items : hash-table[menu -o> (listof (list menu-item number key)))
        (define capability-menu-items (make-hasheq))
        (define/public (register-capability-menu-item key menu)
          (let ([items (send menu get-items)])
            (when (null? items)
              (error 'register-capability-menu-item "menu ~e has no items" menu))
            (let* ([menu-item (last items)]
                   [this-one (list menu-item (- (length items) 1) key)]
                   [old-ones (hash-ref capability-menu-items menu (λ () '()))])
              (hash-set! capability-menu-items menu (cons this-one old-ones)))))
        
        (define/private (update-items/capability menu)
          (let ([new-items (begin '(get-items/capability menu)
                                  (send menu get-items))])
            (for-each (λ (i) (send i delete)) (send menu get-items))
            (for-each (λ (i) (send i restore)) new-items)))
        (define/private (get-items/capability menu)
          (let loop ([capability-items (reverse (hash-ref capability-menu-items menu '()))]
                     [all-items (send menu get-items)]
                     [i 0])
            (cond
              [(null? capability-items) all-items]
              [else
               (let* ([cap-item-list (car capability-items)]
                      [cap-item (list-ref cap-item-list 0)]
                      [cap-num (list-ref cap-item-list 1)]
                      [cap-key (list-ref cap-item-list 2)])
                 (cond
                   [(= cap-num i)
                    (let ([is-on? (get-current-capability-value cap-key)])
                      (cond
                        [is-on?
                         (cond
                           [(null? all-items)
                            (cons cap-item (loop (cdr capability-items) null (+ i 1)))]
                           [(eq? (car all-items) cap-item)
                            (cons cap-item (loop (cdr capability-items) (cdr all-items) (+ i 1)))]
                           [else
                            (cons cap-item (loop (cdr capability-items) all-items (+ i 1)))])]
                        [else
                         (cond
                           [(null? all-items)
                            (loop (cdr capability-items) null (+ i 1))]
                           [(eq? (car all-items) cap-item)
                            (loop (cdr capability-items) (cdr all-items) (+ i 1))]
                           [else
                            (loop (cdr capability-items) all-items (+ i 1))])]))]
                   [else (cons (car all-items)
                               (loop capability-items
                                     (cdr all-items)
                                     (+ i 1)))]))])))
        
        (define/private (get-current-capability-value key)
          (let* ([language-settings (send (get-definitions-text) get-next-settings)]
                 [new-language (drscheme:language-configuration:language-settings-language language-settings)])
            (send new-language capability-value key)))
        
        (define language-menu 'uninited-language-menu)
        (define scheme-menu 'scheme-menu-not-yet-init)
        (define insert-menu 'insert-menu-not-yet-init)
        (define/public (get-insert-menu) insert-menu)
        (define/public (get-special-menu) insert-menu)
        
        (define/public (choose-language-callback)
          (let ([new-settings (drscheme:language-configuration:language-dialog
                               #f
                               (send definitions-text get-next-settings)
                               this)])
            (when new-settings
              (send definitions-text set-next-settings new-settings))))
        
        ;; must be called from on-demand (on each menu click), or the state won't be handled properly
        (define/private (update-teachpack-menu)
          (for-each (λ (item) (send item delete)) teachpack-items)
          (let ([tp-callbacks (get-current-capability-value 'drscheme:teachpack-menu-items)])
            (cond
              [tp-callbacks
               (let* ([language (drscheme:language-configuration:language-settings-language
                                 (send (get-definitions-text) get-next-settings))]
                      [settings (drscheme:language-configuration:language-settings-settings
                                 (send (get-definitions-text) get-next-settings))]
                      [tp-names ((teachpack-callbacks-get-names tp-callbacks) settings)]
                      [update-settings
                       (λ (settings)
                         (send (get-definitions-text) set-next-settings 
                               (drscheme:language-configuration:make-language-settings language settings))
                         (send (get-definitions-text) teachpack-changed))])
                 (set! teachpack-items
                       (list*
                        (make-object separator-menu-item% language-menu)
                        (new menu:can-restore-menu-item%
                             [label (string-constant add-teachpack-menu-item-label)]
                             [parent language-menu]
                             [callback
                              (λ (_1 _2)
                                (update-settings ((teachpack-callbacks-add tp-callbacks) settings this)))])
                        (let ([mi (new menu:can-restore-menu-item% 
                                       [label (string-constant clear-all-teachpacks-menu-item-label)]
                                       [parent language-menu]
                                       [callback
                                        (λ (_1 _2) 
                                          (update-settings ((teachpack-callbacks-remove-all tp-callbacks) settings)))])])
                          
                          (send mi enable (not (null? tp-names)))
                          mi)
                        (map (λ (name)
                               (new menu:can-restore-menu-item%
                                    [label (gui-utils:format-literal-label (string-constant clear-teachpack) name)]
                                    [parent language-menu]
                                    [callback
                                     (λ (item evt)
                                       (update-settings ((teachpack-callbacks-remove tp-callbacks) settings name)))]))
                             tp-names))))]
              [else 
               (set! teachpack-items 
                     (list
                      (new menu:can-restore-menu-item%
                           [label (string-constant add-teachpack-menu-item-label)]
                           [parent language-menu]
                           [callback
                            (λ (_1 _2)
                              (message-box (string-constant drscheme)
                                           (gui-utils:format-literal-label (string-constant teachpacks-only-in-languages)
                                                   (apply 
                                                    string-append
                                                    (reverse
                                                     (filter
                                                      values
                                                      (map (λ (l) 
                                                             (and 
                                                              (send l capability-value 'drscheme:teachpack-menu-items)
                                                              (format "\n  ~a" (send l get-language-name))))
                                                           (drscheme:language-configuration:get-languages))))))
                                           this))])))])))
        
        (define/private (initialize-menus)
          (let* ([mb (get-menu-bar)]
                 [language-menu-on-demand (λ (menu-item) (update-teachpack-menu))]
                 [_ (set! language-menu (make-object (get-menu%) 
                                          (string-constant language-menu-name)
                                          mb
                                          #f
                                          language-menu-on-demand))]
                 [_ (set! scheme-menu (new (get-menu%) 
                                           [label (drscheme:language:get-capability-default
                                                   'drscheme:language-menu-title)]
                                           [parent mb]))]
                 [send-method
                  (λ (method)
                    (λ (_1 _2)
                      (let ([text (get-focus-object)])
                        (when (is-a? text scheme:text<%>)
                          (method text)))))]
                 [show/hide-capability-menus
                  (λ ()
                    (for-each (λ (menu) (update-items/capability menu))
                              (send (get-menu-bar) get-items)))])
            
            (make-object menu:can-restore-menu-item%
              (string-constant choose-language-menu-item-label)
              language-menu
              (λ (_1 _2) (choose-language-callback))
              #\l)
            
            (set! execute-menu-item
                  (make-object menu:can-restore-menu-item%
                    (string-constant execute-menu-item-label)
                    scheme-menu
                    (λ (_1 _2) (execute-callback))
                    #\t
                    (string-constant execute-menu-item-help-string)))
            (make-object menu:can-restore-menu-item%
              (string-constant ask-quit-menu-item-label)
              scheme-menu
              (λ (_1 _2) (send current-tab break-callback))
              #\b
              (string-constant ask-quit-menu-item-help-string))
            (make-object menu:can-restore-menu-item%
              (string-constant force-quit-menu-item-label)
              scheme-menu
              (λ (_1 _2) (send interactions-text kill-evaluation))
              #\k
              (string-constant force-quit-menu-item-help-string))
            (when (custodian-memory-accounting-available?)
              (new menu-item%
                   [label (string-constant limit-memory-menu-item-label)]
                   [parent scheme-menu]
                   [callback
                    (λ (item b)
                      (let ([num (get-mbytes this 
                                             (let ([limit (send interactions-text get-custodian-limit)])
                                               (and limit
                                                    (floor (/ limit 1024 1024)))))])
                        (when num
                          (cond
                            [(eq? num #t)
                             (preferences:set 'drscheme:child-only-memory-limit #f)
                             (send interactions-text set-custodian-limit #f)]
                            [else
                             (preferences:set 'drscheme:child-only-memory-limit 
                                              (* 1024 1024 num))
                             (send interactions-text set-custodian-limit
                                   (* 1024 1024 num))]))))]))
            (new menu:can-restore-menu-item%
                 (label (string-constant clear-error-highlight-menu-item-label))
                 (parent scheme-menu)
                 (callback
                  (λ (_1 _2) 
                    (let ([ints (send (get-current-tab) get-ints)])
                      (send ints reset-error-ranges))))
                 (help-string (string-constant clear-error-highlight-item-help-string))
                 (demand-callback
                  (λ (item)
                    (let ([ints (send (get-current-tab) get-ints)])
                      (send item enable (send ints get-error-ranges))))))
            (make-object separator-menu-item% scheme-menu)
            (make-object menu:can-restore-menu-item%
              (string-constant create-executable-menu-item-label)
              scheme-menu
              (λ (x y) (create-executable this)))
            (make-object menu:can-restore-menu-item%
              (string-constant module-browser...)
              scheme-menu
              (λ (x y) (drscheme:module-overview:module-overview this)))
            (make-object separator-menu-item% scheme-menu)
            
            (let ([cap-val
                   (λ ()
                     (let* ([tab (get-current-tab)]
                            [defs (send tab get-defs)]
                            [settings (send defs get-next-settings)]
                            [language (drscheme:language-configuration:language-settings-language settings)])
                       (send language capability-value 'drscheme:tabify-menu-callback)))])
              (new menu:can-restore-menu-item%
                   [label (string-constant reindent-menu-item-label)]
                   [parent scheme-menu]
                   [demand-callback (λ (m) (send m enable (cap-val)))]
                   [callback (send-method 
                              (λ (x)
                                (let ([f (cap-val)])
                                  (when f
                                    (f x
                                       (send x get-start-position)
                                       (send x get-end-position))))))])
              
              (new menu:can-restore-menu-item%
                   [label (string-constant reindent-all-menu-item-label)]
                   [parent scheme-menu]
                   [callback 
                    (send-method 
                     (λ (x)
                       (let ([f (cap-val)])
                         (when f
                           (f x 0 (send x last-position))))))]
                   [shortcut #\i]
                   [demand-callback (λ (m) (send m enable (cap-val)))]))
            
            (make-object menu:can-restore-menu-item%
              (string-constant box-comment-out-menu-item-label)
              scheme-menu
              (send-method (λ (x) (send x box-comment-out-selection))))
            (make-object menu:can-restore-menu-item%
              (string-constant semicolon-comment-out-menu-item-label)
              scheme-menu
              (send-method (λ (x) (send x comment-out-selection))))
            (make-object menu:can-restore-menu-item%
              (string-constant uncomment-menu-item-label)
              scheme-menu
              (λ (x y)
                (let ([text (get-focus-object)])
                  (when (is-a? text text%)
                    (let ([admin (send text get-admin)])
                      (cond
                        [(is-a? admin editor-snip-editor-admin<%>)
                         (let ([es (send admin get-snip)])
                           (cond
                             [(is-a? es comment-box:snip%)
                              (let ([es-admin (send es get-admin)])
                                (when es-admin
                                  (let ([ed (send es-admin get-editor)])
                                    (when (is-a? ed scheme:text<%>)
                                      (send ed uncomment-box/selection)))))]
                             [else (send text uncomment-selection)]))]
                        [else (send text uncomment-selection)]))))))
            
            (set! insert-menu
                  (new (get-menu%)
                       [label (string-constant insert-menu)]
                       [parent mb]
                       [demand-callback
                        (λ (insert-menu)
                          ;; just here for convience -- it actually works on all menus, not just the special menu
                          (show/hide-capability-menus))]))
            
            (let ([has-editor-on-demand
                   (λ (menu-item)
                     (let ([edit (get-edit-target-object)])
                       (send menu-item enable (and edit (is-a? edit editor<%>)))))]
                  [callback
                   (λ (menu evt)
                     (let ([edit (get-edit-target-object)])
                       (when (and edit
                                  (is-a? edit editor<%>))
                         (let ([number (get-fraction-from-user this)])
                           (when number
                             (send edit insert
                                   (number-snip:make-fraction-snip number #f)))))
                       #t))]
                  [insert-lambda
                   (λ ()
                     (let ([edit (get-edit-target-object)])
                       (when (and edit
                                  (is-a? edit editor<%>))
                         (send edit insert "\u03BB")))
                     #t)]
                  [insert-large-semicolon-letters
                   (λ ()
                     (let ([edit (get-edit-target-object)])
                       (when edit
                         (let ([language-settings (send definitions-text get-next-settings)])
                           (let-values ([(comment-prefix comment-character)
                                         (if language-settings
                                             (send (drscheme:language-configuration:language-settings-language
                                                    language-settings)
                                                   get-comment-character)
                                             (values ";" #\;))])
                             (insert-large-letters comment-prefix comment-character edit this))))))]
                  [c% (get-menu-item%)])
              
              (frame:add-snip-menu-items 
               insert-menu 
               c%
               (λ (item)
                 (let ([label (send item get-label)])
                   (cond
                     [(equal? label (string-constant insert-comment-box-menu-item-label))
                      (register-capability-menu-item 'drscheme:special:insert-comment-box insert-menu)]
                     [(equal? label (string-constant insert-image-item))
                      (register-capability-menu-item 'drscheme:special:insert-image insert-menu)]))))
              
              (make-object c% (string-constant insert-fraction-menu-item-label)
                insert-menu callback 
                #f #f
                has-editor-on-demand)
              (register-capability-menu-item 'drscheme:special:insert-fraction insert-menu)
              
              (make-object c% (string-constant insert-large-letters...)
                insert-menu
                (λ (x y) (insert-large-semicolon-letters))
                #f #f
                has-editor-on-demand)
              (register-capability-menu-item 'drscheme:special:insert-large-letters insert-menu)
              
              (make-object c% (string-constant insert-lambda)
                insert-menu
                (λ (x y) (insert-lambda))
                #\\
                #f
                has-editor-on-demand)
              (register-capability-menu-item 'drscheme:special:insert-lambda insert-menu))
            
            (make-object separator-menu-item% (get-show-menu))
            
            (new menu:can-restore-menu-item%
                 (shortcut (if (eq? (system-type) 'macosx) #f #\m))
                 (label (string-constant split-menu-item-label))
                 (parent (get-show-menu))
                 (callback (λ (x y) (split)))
                 (demand-callback (λ (item) (split-demand item))))
            (new menu:can-restore-menu-item% 
                 (shortcut (if (eq? (system-type) 'macosx) #f #\m))
                 (shortcut-prefix (if (eq? (system-type) 'macosx) 
                                      (get-default-shortcut-prefix)
                                      (cons 'shift (get-default-shortcut-prefix))))
                 (label (string-constant collapse-menu-item-label))
                 (parent (get-show-menu))
                 (callback (λ (x y) (collapse)))
                 (demand-callback (λ (item) (collapse-demand item))))
            
            (frame:reorder-menus this)))
        
        ;                          
        ;                          
        ;                          
        ;                          
        ;   ++-@@-   -+@+- +++: :++
        ;   +@@-+@  -@-:-@--@-   -@
        ;   :@:  @: @+   ++ @::@::@
        ;   :@   @: @@@@@@@ +--@--*
        ;   :@   @: @-      -@+*+@:
        ;   -@: :@- +@:::+@ :@@:@@ 
        ;   @@@ +@@: +@@@+:  ++ ++ 
        ;                          
        ;                          
        ;                          
        
        (define definitions-text (new (drscheme:get/extend:get-definitions-text)))
        
        ;; tabs : (listof tab)
        (define tabs (list (new (drscheme:get/extend:get-tab)
                                (defs definitions-text)
                                (frame this)
                                (i 0)
                                (defs-shown? #t)
                                (ints-shown? #t))))
        (define/public-final (get-tabs) tabs)
        
        ;; current-tab : tab
        ;; corresponds to the tabs-panel's active button.
        (define current-tab (car tabs))
        
        (define interactions-text (new (drscheme:get/extend:get-interactions-text) 
                                       (context (car tabs))))
        (send (car tabs) set-ints interactions-text)
        
        (init-definitions-text (car tabs))
        
        (super-new
         (filename filename)
         (style '(toolbar-button))
         (width (preferences:get 'drscheme:unit-window-width))
         (height (preferences:get 'drscheme:unit-window-height)))
        (inherit maximize)
        (when (preferences:get 'drscheme:unit-window-max?)
          (maximize #t))
        
        (initialize-menus)
        
        
        ;                                                                               
        ;                                                                               
        ;                                                                               
        ;                                 ;       ;                                     
        ;                                 ;       ;                                     
        ;                                 ;       ;                                 ;   
        ;   ; ;;    ;;;    ; ;;     ;;;   ;       ;   ;;;   ;     ;  ;;;    ;   ;  ;;;; 
        ;   ;;  ;  ;   ;   ;;  ;   ;   ;  ;       ;  ;   ;  ;     ; ;   ;   ;   ;   ;   
        ;   ;    ;     ;   ;   ;  ;    ;  ;       ;      ;   ;   ; ;     ;  ;   ;   ;   
        ;   ;    ;  ;;;;   ;   ;  ;;;;;;  ;       ;   ;;;;   ;   ; ;     ;  ;   ;   ;   
        ;   ;    ; ;   ;   ;   ;  ;       ;       ;  ;   ;    ; ;  ;     ;  ;   ;   ;   
        ;   ;;  ;  ;   ;   ;   ;   ;      ;       ;  ;   ;    ; ;   ;   ;   ;  ;;   ;   
        ;   ; ;;    ;;;;;  ;   ;    ;;;;  ;       ;   ;;;;;    ;     ;;;     ;; ;    ;; 
        ;   ;                                                  ;                        
        ;   ;                                                  ;                        
        ;   ;                                                 ;                         
        
        
        (define toolbar/rest-panel (new vertical-panel% [parent (get-area-container)]))
        
        ;; most contain only top-panel (or nothing)
        (define top-outer-panel (new horizontal-panel% 
                                     [parent toolbar/rest-panel]
                                     [alignment '(right top)]
                                     [stretchable-height #f]))
        
        [define top-panel (make-object horizontal-panel% top-outer-panel)]
        [define name-panel (new horizontal-panel%
                                (parent top-panel)
                                (alignment '(left center))
                                (stretchable-width #f)
                                (stretchable-height #f))]
        (define panel-with-tabs (new vertical-panel%
                                     (parent (get-definitions/interactions-panel-parent))))
        (define tabs-panel (new tab-panel% 
                                (font small-control-font)
                                (parent panel-with-tabs)
                                (stretchable-height #f)
                                (style '(deleted no-border))
                                (choices '("first name"))
                                (callback (λ (x y)
                                            (let ([sel (send tabs-panel get-selection)])
                                              (when sel
                                                (change-to-nth-tab sel)))))))
        [define resizable-panel (new (if (preferences:get 'drscheme:defs/ints-horizontal)
                                         horizontal-dragable/def-int%
                                         vertical-dragable/def-int%)
                                     (unit-frame this)
                                     (parent panel-with-tabs))]
        
        [define definitions-canvas #f]
        (initialize-definitions-canvas)
        [define definitions-canvases (list definitions-canvas)]
        [define interactions-canvas (new (drscheme:get/extend:get-interactions-canvas)
                                         (parent resizable-panel)
                                         (editor interactions-text))]
        [define interactions-canvases (list interactions-canvas)]
        
        
        (define/public (get-definitions-canvases) 
          ;; before definition, just return null
          (if (pair? definitions-canvases)
              definitions-canvases
              null))
        (define/public (get-interactions-canvases)
          ;; before definition, just return null
          (if (pair? interactions-canvases)
              interactions-canvases
              null))
        
        (public get-definitions-canvas get-interactions-canvas)
        [define get-definitions-canvas (λ () definitions-canvas)]
        [define get-interactions-canvas (λ () interactions-canvas)]
        
        (set! save-button
              (new switchable-button%
                   [parent top-panel]
                   [callback (λ (x) (when definitions-text
                                      (save)
                                      (send definitions-canvas focus)))]
                   [bitmap save-bitmap]
                   [label (string-constant save-button-label)]))
        (register-toolbar-button save-button)
        
        (set! name-message (new drs-name-message% [parent name-panel]))
        (send name-message stretchable-width #t)
        (send name-message set-allow-shrinking 200)
        [define teachpack-items null]
        [define break-button (void)]
        [define execute-button (void)]
        [define button-panel (new horizontal-panel% [parent top-panel] [spacing 2])]
        (define/public (get-execute-button) execute-button)
        (define/public (get-break-button) break-button)
        (define/public (get-button-panel) button-panel)
        
        (inherit get-info-panel)
        (define running-canvas
          (new running-canvas% [parent (get-info-panel)]))
        
        (define bug-icon
          (let* ([info-panel (get-info-panel)]
                 [btn 
                  (new switchable-button%
                       [parent info-panel]
                       [callback (λ (x) (show-saved-bug-reports-window))]
                       [bitmap very-small-planet-bitmap]
                       [vertical-tight? #t]
                       [label (string-constant show-planet-contract-violations)])])
            (send btn set-label-visible #f)
            (send info-panel change-children 
                  (λ (l)
                    (cons btn (remq* (list btn) l))))
            btn))
        (define/private (set-bug-label v)
          (if (null? v)
              (send bug-icon show #f)
              (send bug-icon show #t)))
        (set-bug-label (preferences:get 'drscheme:saved-bug-reports))
        (define remove-bug-icon-callback
          (preferences:add-callback
           'drscheme:saved-bug-reports
           (λ (p v)
             (set-bug-label v))))
        
        [define func-defs-canvas (new func-defs-canvas% 
                                      (parent name-panel)
                                      (frame this))]
        
        (set! execute-button
              (new switchable-button%
                   [parent button-panel]
                   [callback (λ (x) (execute-callback))]
                   [bitmap execute-bitmap]
                   [label (string-constant execute-button-label)]))
        (register-toolbar-button execute-button)
        
        (set! break-button
              (new switchable-button%
                   [parent button-panel]
                   [callback (λ (x) (send current-tab break-callback))]
                   [bitmap break-bitmap]
                   [label (string-constant break-button-label)]))
        (register-toolbar-button break-button)

        (send button-panel stretchable-height #f)
        (send button-panel stretchable-width #f) 
        
        (send top-panel change-children
              (λ (l)
                (list name-panel save-button
                      (make-object vertical-panel% top-panel) ;; spacer
                      button-panel)))
        
        (send top-panel stretchable-height #f)
        (inherit get-label)
        (let ([m (send definitions-canvas get-editor)])
          (set-save-init-shown?
           (and m (send m is-modified?))))
        
        (define language-message
          (let* ([info-panel (get-info-panel)]
                 [p (new vertical-panel% 
                         [parent info-panel]
                         [alignment '(left center)])]
                 [language-message (new language-label-message% [parent p] [frame this])])
            (send info-panel change-children 
                  (λ (l)
                    (list* p
                           (remq* (list p)
                                  l))))
            language-message))
        
        (update-save-message)
        (update-save-button)
        (language-changed)
        
        (cond
          [filename
           (set! definitions-shown? #t)
           (set! interactions-shown? #f)]
          [else
           (set! definitions-shown? #t)
           (set! interactions-shown? #t)])
        
        (update-shown)
        
        (when (= 2 (length (send resizable-panel get-children)))
          (send resizable-panel set-percentages
                (let ([p (preferences:get 'drscheme:unit-window-size-percentage)])
                  (list p (- 1 p)))))
        
        (set-label-prefix (string-constant drscheme))
        (set! newest-frame this)
        (send definitions-canvas focus)))
    
    ;; get-drscheme:define-popup-name : (or/c #f (cons/c string? string?) (list/c string? string? string)) boolean -> (or/c #f string?)
    (define (get-drscheme:define-popup-name info vertical?)
      (and info
           (if vertical?
               (if (pair? (cdr info))
                   (list-ref info 2)
                   "δ")
               (if (pair? (cdr info))
                   (list-ref info 1)
                   (cdr info)))))
                  
    
    (define execute-warning-canvas%
      (class canvas%
        (inherit stretchable-height get-dc get-client-size min-height)
        (init-field message)
        (define/public (set-message _msg) (set! message _msg))
        
        (define/override (on-paint)
          (let ([dc (get-dc)])
            (let-values ([(w h) (get-client-size)])
              (send dc set-pen "yellow" 1 'solid)
              (send dc set-brush "yellow" 'solid)
              (send dc draw-rectangle 0 0 w h)
              (when message
                (let* ([base normal-control-font]
                       [face (send base get-face)])
                  (if face
                      (send dc set-font (send the-font-list find-or-create-font
                                              (send base get-point-size)
                                              face
                                              (send base get-family)
                                              (send base get-style)
                                              'bold))
                      (send dc set-font (send the-font-list find-or-create-font
                                              (send base get-point-size)
                                              (send base get-family)
                                              (send base get-style)
                                              'bold))))
                (let-values ([(tw th _1 _2) (send dc get-text-extent message)])
                  (send dc draw-text message 
                        (floor (- (/ w 2) (/ tw 2)))
                        (floor (- (/ h 2) (/ th 2)))))))))
        (super-new)
        (let-values ([(w h d a) (send (get-dc) get-text-extent "Xy")])
          (min-height (+ 4 (floor (inexact->exact h)))))))
    
    
;                                                   
;                                                   
;                                                   
;                                                   
;                               ;;;                 
;                                                   
;  ;;; ;;;; ;;; ;;; ;;  ;;; ;;  ;;; ;;; ;;   ;; ;;; 
;  ;;;;;;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;;;;;; ;;;;;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;;;;; 
;  ;;;   ;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;; ;;; 
;                                               ;;; 
;                                           ;;;;;;  
;                                                   
;                                                   

    
    (define running-bitmap (include-bitmap (lib "icons/b-run.png")))
    (define waiting-bitmap (include-bitmap (lib "icons/b-wait.png")))
    (define waiting2-bitmap (include-bitmap (lib "icons/b-wait2.png")))
    (define running/waiting-bitmaps (list running-bitmap waiting-bitmap waiting2-bitmap))
    (define running-canvas%
      (class canvas%
        (inherit get-dc refresh get-client-size)
        (define/public (set-running r?) 
          (unless (eq? r? is-running?)
            (set! is-running? r?)
            (refresh)))
        (define is-running? #f)
        (define toggle? #t)
        (define timer #f)
        (define inside? #f)
        
        (define/override (on-event evt)
          (let-values ([(w h) (get-client-size)])
            (let ([new-inside?
                   (and (<= 0 (send evt get-x) w)
                        (<= 0 (send evt get-y) h))]
                  [old-inside? inside?])
              (set! inside? new-inside?)
              (cond
                [(and new-inside? (not old-inside?))
                 (unless is-running?
                   (set! timer
                         (new timer%
                              [notify-callback 
                               (λ ()
                                 (set! toggle? (not toggle?))
                                 (refresh))]
                              [interval 200])))]
                [(and (not new-inside?) old-inside? timer)
                 (send timer stop)
                 (set! timer #f)]))))
        
        (define/override (on-paint) 
          (let ([dc (get-dc)]
                [bm 
                 (if is-running?
                     running-bitmap
                     (if toggle?
                         waiting-bitmap
                         waiting2-bitmap))])
            (let-values ([(cw ch) (get-client-size)])
              (send dc draw-bitmap bm 
                    (- (/ cw 2) (/ (send bm get-width) 2))
                    (- (/ ch 2) (/ (send bm get-height) 2))
                    'solid
                    (send the-color-database find-color "black")
                    (send bm get-loaded-mask)))))
        
        (super-new [stretchable-width #f]
                   [stretchable-height #f]
                   [style '(transparent)])
        (inherit min-width min-height)
        (min-width (apply max (map (λ (x) (send x get-width)) running/waiting-bitmaps)))
        (min-height (apply max (map (λ (x) (send x get-height)) running/waiting-bitmaps)))))
    
    ;; get-mbytes : top-level-window -> (union #f  ;; cancel
    ;;                                         integer[>=100] ;; a limit
    ;;                                         #t) ;; no limit
    (define (get-mbytes parent current-limit)
      (define d (new dialog%
                     [label (string-constant drscheme)]
                     [parent parent]))
      (define msg1 (new message%
                        [parent d]
                        [label (string-constant limit-memory-msg-1)]))
      (define msg1.5 (new message%
                          [parent d]
                          [label (string-constant limit-memory-msg-2)]))
      
      (define outer-hp (new horizontal-panel% [parent d] [alignment '(center bottom)]))
      (define rb (new radio-box%
                      [label #f]
                      [choices (list (string-constant limit-memory-unlimited) (string-constant limit-memory-limited))]
                      [callback (λ (a b) (grayizie))]
                      [parent outer-hp]))
      
      (define (grayizie)
        (case (send rb get-selection)
          [(0) 
           (send tb enable #f)
           (send msg2 enable #f)
           (background gray-foreground-sd)]
          [(1)
           (send tb enable #t)
           (send msg2 enable #t)
           (background black-foreground-sd)
           (let ([e (send tb get-editor)])
             (send e set-position 0 (send e last-position)))
           (send tb focus)])
        (update-ok-button-state))
      
      (define hp (new horizontal-panel% 
                      [parent outer-hp] 
                      [stretchable-height #f]
                      [stretchable-width #f]))
      
      (define tb
        (new text-field%
             [label #f]
             [parent hp]
             [init-value (if current-limit
                             (format "~a" current-limit)
                             "64")]
             [stretchable-width #f]
             [min-width 100]
             [callback
              (λ (tf e)
                (let ([ed (send tf get-editor)])
                  (cond 
                    [(is-valid-number? ed)
                     (background clear-sd)]
                    [else
                     (background yellow-sd)]))
                (update-ok-button-state))]))
      
      (define (update-ok-button-state)
        (case (send rb get-selection)
          [(0) (send ok-button enable #t)]
          [(1) (send ok-button enable (is-valid-number? (send tb get-editor)))]))
      
      (define msg2 (new message% [parent hp] [label (string-constant limit-memory-megabytes)]))
      (define bp (new horizontal-panel% [parent d]))
      (define-values (ok-button cancel-button)
        (gui-utils:ok/cancel-buttons
         bp 
         (λ (a b) 
           (case (send rb get-selection)
             [(0) (set! result #t)]
             [(1) (set! result (string->number (send (send tb get-editor) get-text)))])
           (send d show #f))
         (λ (a b) (send d show #f))))
      
      (define result #f)
      
      (define clear-sd (make-object style-delta%))
      (define yellow-sd (make-object style-delta%))
      
      (define black-foreground-sd (make-object style-delta%))
      (define gray-foreground-sd (make-object style-delta%))
      
      (define (is-valid-number? txt)
        (let* ([n (string->number (send txt get-text))])
          (and n
               (integer? n)
               (1 . <= . n))))
      
      (define (background sd)
        (let ([txt (send tb get-editor)])
          (send txt change-style sd 0 (send txt last-position))))
      
      (send clear-sd set-delta-background "white")
      (send yellow-sd set-delta-background "yellow")
      (send black-foreground-sd set-delta-foreground "black")
      (send gray-foreground-sd set-delta-foreground "gray")
      (send d set-alignment 'left 'center)
      (send bp set-alignment 'right 'center)
      (when current-limit
        (send rb set-selection 1))
      (update-ok-button-state)
      (grayizie)
      (send tb focus)
      (let ([e (send tb get-editor)])
        (send e set-position 0 (send e last-position)))
      (send d show #t)
      result)
    
    
    
    (define (limit-length l n)
      (let loop ([l l]
                 [n n])
        (cond
          [(or (null? l) (zero? n))  null]
          [else (cons (car l) (loop (cdr l) (- n 1)))])))
    (define (remove-duplicate-languages l)
      (reverse
       (let loop ([l (reverse l)])
         (cond
           [(null? l) l]
           [else
            (if (member (car (car l)) (map car (cdr l)))
                (loop (cdr l))
                (cons (car l) (loop (cdr l))))]))))
    
    (define language-label-message%
      (class name-message%
        (init-field frame)
        (inherit refresh)
        
        (inherit set-message)
        (define yellow? #f)
        (define/override (get-background-color) (and yellow? "yellow"))
        (define/public (set-yellow y?) 
          (set! yellow? y?)
          (refresh))
        (define/public (set-yellow/lang y? lang) 
          (set-message #f lang)
          (set-yellow y?))
        
        (define/override (fill-popup menu reset)
          (let ([added-one? #f])
            (send (new menu-item%
                       [label (string-constant recent-languages)]
                       [callback void]
                       [parent menu])
                  enable #f)
            (for-each
             (λ (name/settings)
               (let* ([name (car name/settings)]
                      [marshalled-settings (cdr name/settings)]
                      [lang (ormap
                             (λ (l) (and (equal? (send l get-language-name) name) l))
                             (drscheme:language-configuration:get-languages))])
                 (when lang
                   ;; this test can fail when a language has been added wrongly via the tools interface
                   ;; just ignore that menu item, in that case.
                   (let ([settings (or (send lang unmarshall-settings marshalled-settings)
                                       (send lang default-settings))])
                     (when lang
                       (set! added-one? #t)
                       (new menu-item%
                            [parent menu]
                            [label (send lang get-language-name)]
                            [callback
                             (λ (x y)
                               (send (send frame get-definitions-text)
                                     set-next-settings
                                     (drscheme:language-configuration:make-language-settings
                                      lang
                                      settings)))]))))))
             (preferences:get 'drscheme:recent-language-names))
            (unless added-one?
              (send (new menu-item% 
                         [label (string-append
                                 "  << "
                                 (string-constant no-recently-chosen-languages)
                                 " >>")]
                         [parent menu]
                         [callback void])
                    enable #f))
            (new separator-menu-item% [parent menu]))
          (new menu-item%
               [label (string-constant choose-language-menu-item-label)]
               [parent menu]
               [callback 
                (λ (x y)
                  (send frame choose-language-callback))]))
        
        (super-new [label ""]
                   [font small-control-font]
                   [string-constant-untitled (string-constant untitled)]
                   [string-constant-no-full-name-since-not-saved 
                    (string-constant no-full-name-since-not-saved)])
                
        (inherit set-allow-shrinking)
        (set-allow-shrinking 100)))
    
    
    
;                                                                            
;                                                                            
;                                                                            
;                                                                            
;  ;;;                                                             ;         
;  ;;;                                                           ;;;         
;  ;;; ;;  ;;; ;;;  ;; ;;;     ;;; ;; ;;;;  ;;; ;;    ;;;   ;;; ;;;;;  ;;;;  
;  ;;;;;;; ;;; ;;; ;;;;;;;     ;;;;; ;; ;;; ;;;;;;;  ;;;;;  ;;;;;;;;; ;;; ;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;  ;;;    
;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;;  ;;;   ;;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;;     ;;;  ;;;     ;;; ;;; ;;; ;;; ;;;  ;;;     ;;; 
;  ;;;;;;; ;;;;;;; ;;;;;;;     ;;;   ;;;;;; ;;;;;;;  ;;;;;  ;;;  ;;;; ;; ;;; 
;  ;;; ;;   ;; ;;;  ;; ;;;     ;;;    ;;;;  ;;; ;;    ;;;   ;;;   ;;;  ;;;;  
;                      ;;;                  ;;;                              
;                  ;;;;;;                   ;;;                              
;                                                                            
;                                                                            

    
    ;; record-saved-bug-report : (listof (cons symbol string)) -> void
    ;; =Kernel= =Handler=
    (define (record-saved-bug-report table)
      (let ([recorded (preferences:get 'drscheme:saved-bug-reports)])
        (unless (member table recorded)
          (preferences:set 'drscheme:saved-bug-reports (shorten-to (cons table recorded) 15)))))
    
    ;; shorten-to : (listof X) number -> (listof X)
    ;; drops items from the end of the list to bring it back down to `n' items
    (define (shorten-to l n)
      (let loop ([l l]
                 [n n])
        (cond
          [(zero? n) '()]
          [(null? l) '()]
          [else (cons (car l) (loop (cdr l) (- n 1)))])))

    (define very-small-planet-bitmap (include-bitmap (lib "icons/very-small-planet.png") 'png/mask))
    
    (define saved-bug-reports-window #f)
    (define saved-bug-reports-panel #f)
    (define (init-saved-bug-reports-window)
      (unless saved-bug-reports-window
        (let ()
          (define stupid-internal-define-syntax1
            (set! saved-bug-reports-window (new frame:basic% [label (string-constant drscheme)] [width 600])))
          (define stupid-internal-define-syntax2
            (set! saved-bug-reports-panel
                  (new vertical-panel% [parent (send saved-bug-reports-window get-area-container)])))
          (define hp (new horizontal-panel% 
                          [parent (send saved-bug-reports-window get-area-container)] 
                          [stretchable-width #f] 
                          [alignment '(right center)]))
          (define forget-all (new button% 
                                  [label (string-constant bug-track-forget-all)] 
                                  [callback 
                                   (λ (_1 _2)
                                     (send saved-bug-reports-window show #f)
                                     (preferences:set 'drscheme:saved-bug-reports '()))]
                                  [parent hp]))
          (void))))
    
    (preferences:add-callback
     'drscheme:saved-bug-reports
     (λ (p v)
       (when saved-bug-reports-window
         (when (send saved-bug-reports-window is-shown?)
           (cond
             [(null? v)
              (send saved-bug-reports-window show #f)]
             [else
              (refresh-saved-bug-reports-window v)])))))
    
    (define (refresh-saved-bug-reports-window pref)
      (send saved-bug-reports-window begin-container-sequence)
      (send saved-bug-reports-panel change-children (λ (l) '()))
      (for-each
       (λ (item)
         (let ()
           (define (lookup k [default ""])
             (let loop ([item item])
               (cond
                 [(null? item) default]
                 [else (let ([rib (car item)])
                         (if (eq? (car rib) k)
                             (cdr rib)
                             (loop (cdr item))))])))
           (define vp
             (new vertical-panel% 
                  [style '(border)]
                  [parent saved-bug-reports-panel]
                  [stretchable-height #f]))
           (define hp
             (new horizontal-panel% 
                  [parent vp]
                  [stretchable-height #f]))
           (define first-line-msg 
             (let ([desc (lookup 'description #f)])
               (and desc
                    (new message%
                         [label (read-line (open-input-string desc))]
                         [parent vp]
                         [stretchable-width #t]
                         [font (send (send (editor:get-standard-style-list) find-named-style "Standard") get-font)]))))
           (define msg (new message% 
                            [stretchable-width #t]
                            [label (string-append (lookup 'component "<<unknown component>>")
                                                  (let ([v (lookup 'version #f)])
                                                    (if v
                                                        (string-append " " v)
                                                        "")))]
                            [parent hp]))
           (define forget (new button% 
                                [parent hp] 
                                [callback (λ (x y) (forget-saved-bug-report item))]
                                [label (string-constant bug-track-forget)]))
           (define report (new button% 
                               [parent hp] 
                               [callback (λ (x y) 
                                           (forget-saved-bug-report item)
                                           (send-url
                                            (url->string
                                             (drscheme:debug:bug-info->ticket-url item))))]
                               [label (string-constant bug-track-report)]))
           (void)))
       pref) ;; reverse list so first elements end up on top of list
      (send saved-bug-reports-window reflow-container)
      (send saved-bug-reports-window end-container-sequence))
    
    (define (forget-saved-bug-report item)
      (preferences:set 'drscheme:saved-bug-reports (remove item (preferences:get 'drscheme:saved-bug-reports))))
    
    (define (show-saved-bug-reports-window)
      (init-saved-bug-reports-window)
      (unless (send saved-bug-reports-window is-shown?)
        (refresh-saved-bug-reports-window (preferences:get 'drscheme:saved-bug-reports)))
      (send saved-bug-reports-window show #t))
    
    
    
;                                                    
;                                                    
;                                                    
;                                                    
;   ;;;;                                   ;;    ;   
;  ;;;                                    ;  ;  ;    
;  ;;;; ;;; ;;;;;;;  ;;; ;; ;;;    ;;;;   ;  ;  ;    
;  ;;;; ;;;;;;;;;;;; ;;;;;;;;;;;  ;; ;;;  ;  ; ;     
;  ;;;  ;;;  ;;  ;;; ;;; ;;; ;;; ;;; ;;;   ;; ;; ;;  
;  ;;;  ;;;    ;;;;; ;;; ;;; ;;; ;;;;;;;      ; ;  ; 
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;;         ;  ;  ; 
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;  ;;;;;;     ;  ;  ; 
;  ;;;  ;;;   ;;;;;; ;;; ;;; ;;;   ;;;;     ;    ;;  
;                                                    
;                                                    
;                                                    
;                                                    

    
    (define -frame% (frame-mixin super-frame%))
    
    (define (make-two-way-prefs-dragable-panel% % pref-key)
      (class %
        (inherit get-percentages)
        (define/augment (after-percentage-change)
          (let ([percentages (get-percentages)])
            (when (and (pair? percentages)
                       (pair? (cdr percentages))
                       (null? (cddr percentages)))
              (preferences:set pref-key (car percentages))))
          (inner (void) after-percentage-change))
        (super-new)))
    
    (define drs-name-message%
      (class name-message%
        (define/override (on-choose-directory dir)
          (let ([file (finder:get-file dir
                                       (string-constant select-file)
                                       #f
                                       ""
                                       (send this get-top-level-window))])
            (when file
              (handler:edit-file file))))
        (super-new 
         [string-constant-untitled (string-constant untitled)]
         [string-constant-no-full-name-since-not-saved 
          (string-constant no-full-name-since-not-saved)])))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; lambda-snipclass is for backwards compatibility
    ;;
    (define lambda-snipclass
      (make-object (class snip-class%
                     (define/override (read p) (make-object string-snip% "λ"))
                     (super-new))))
    (send lambda-snipclass set-version 1)
    (send lambda-snipclass set-classname "drscheme:lambda-snip%")
    (send (get-the-snip-class-list) add lambda-snipclass)
    
    (define newest-frame 'nothing-yet)
    
    (define open-drscheme-window
      (case-lambda
        [() (open-drscheme-window #f)]
        [(name)
         (cond
           [(and newest-frame
                 name
                 (not (eq? newest-frame 'nothing-yet)) 
                 (send newest-frame still-untouched?))
            (send newest-frame change-to-file name)
            (send newest-frame show #t)
            (begin0 newest-frame
                    (set! newest-frame #f))]
           [(and name ;; only open a tab if we have a filename
                 (preferences:get 'drscheme:open-in-tabs))
            (let ([fr (let loop ([frs (cons (send (group:get-the-frame-group) get-active-frame)
                                            (send (group:get-the-frame-group) get-frames))])
                        (cond
                          [(null? frs) #f]
                          [else (let ([fr (car frs)])
                                  (or (and (is-a? fr -frame<%>)
                                           fr)
                                      (loop (cdr frs))))]))])
              (if fr
                  (begin (send fr open-in-new-tab name)
                         (send fr show #t)
                         fr)
                  (create-new-drscheme-frame name)))]
           [else
            (create-new-drscheme-frame name)])]))

    (define first-frame? #t)
    (define (create-new-drscheme-frame filename)
      (let* ([drs-frame% (drscheme:get/extend:get-unit-frame)]
             [frame (new drs-frame% (filename filename))])
        (send (send frame get-interactions-text) initialize-console)
        (when first-frame?
          (let ([pos (preferences:get 'drscheme:frame:initial-position)])
            (when pos
              (send frame move (car pos) (cdr pos)))))
        (send frame update-toolbar-visibility)
        (send frame show #t)
        (set! first-frame? #f)
        frame)))
