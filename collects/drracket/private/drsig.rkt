#lang racket/base
(require racket/unit)

(provide drracket:eval^
         drracket:debug^
         drracket:module-language^
         drracket:module-language-tools^
         drracket:get-collection^
         drracket:main^
         drracket:init^
         drracket:language-configuration^
         drracket:language-configuration/internal^
         drracket:tools^
         drracket:tools-drs^
         drracket:get/extend^
         drracket:unit^
         drracket:frame^
         drracket:program^
         drracket:text^
         drracket:rep^
         drracket:app^
         drracket:draw-arrow^
         drracket:help-desk^
         drracket:language^
         drracket:multi-file-search^
         drracket:module-overview^
         drracket:font^
         drracket:modes^
         drracket:tracing^
         drracket:tool-exports^
         drracket:tool^
         drracket:tool-cm^
         drscheme:tool^
         drscheme:tool-cm^)

(define-signature drracket:modes-cm^
  ())
(define-signature drracket:modes^ extends drracket:modes-cm^
  (add-mode
   get-modes
   add-initial-modes
   make-mode
   (struct mode (name surrogate repl-submit matches-language) 
           #:omit-constructor)))

(define-signature drracket:font-cm^
  ())
(define-signature drracket:font^ extends drracket:font-cm^
  (setup-preferences))

(define-signature drracket:debug-cm^
  (profile-definitions-text-mixin
   profile-tab-mixin
   profile-unit-frame-mixin
   test-coverage-interactions-text-mixin
   test-coverage-definitions-text-mixin
   test-coverage-tab-mixin))
(define-signature drracket:debug^ extends drracket:debug-cm^
  (make-debug-error-display-handler
   make-debug-eval-handler
   error-display-handler/stacktrace
   bug-info->ticket-url
   test-coverage-enabled
   profiling-enabled
   
   add-prefs-panel
   
   get-error-color
   
   hide-backtrace-window
   show-backtrace-window
   show-backtrace-window/edition-pairs
   open-and-highlight-in-file
   
   small-planet-bitmap

   srcloc->edition/pair
   
   
   ;show-error-and-highlight
   ;print-bug-to-stderr
   ;display-srclocs-in-error
   ;show-syntax-error-context
   ))

(define-signature drracket:module-language-cm^
  (module-language<%>))
(define-signature drracket:module-language^ extends drracket:module-language-cm^
  (add-module-language
   module-language-put-file-mixin))

(define-signature drracket:module-language-tools-cm^
  (frame-mixin
   frame<%>
   tab-mixin
   tab<%>
   definitions-text-mixin
   definitions-text<%>))
(define-signature drracket:module-language-tools^ extends drracket:module-language-tools-cm^
  (add-opt-out-toolbar-button))

(define-signature drracket:get-collection-cm^ ())
(define-signature drracket:get-collection^ extends drracket:get-collection-cm^
  (get-file/collection))

(define-signature drracket:main-cm^ ())
(define-signature drracket:main^ extends drracket:main-cm^ ())

(define-signature drracket:init-cm^
  ())
(define-signature drracket:init^ extends drracket:init-cm^
  (original-output-port
   original-error-port
   original-error-display-handler
   primitive-eval
   primitive-load
   error-display-handler-message-box-title
   system-logger
   system-custodian
   system-eventspace
   system-namespace
   system-security-guard
   first-dir))

(define-signature drracket:language-configuration-cm^
  ())
(define-signature drracket:language-configuration^ extends drracket:language-configuration-cm^
  (add-language
   get-languages
   (struct language-settings (language settings))
   make-language-settings
   get-settings-preferences-symbol
   language-dialog
   fill-language-dialog))

(define-signature drracket:language-configuration/internal^ extends drracket:language-configuration^
  (add-info-specified-languages
   get-default-language-settings
   settings-preferences-symbol
   get-all-scheme-manual-keywords
   get-all-manual-keywords
   add-built-in-languages
   not-a-language-language<%>))

(define-signature drracket:tools-cm^
  ())
(define-signature drracket:tools^ extends drracket:tools-cm^
  ((struct successful-tool (spec bitmap name url))
   make-successful-tool
   get-successful-tools
   only-in-phase
   load/invoke-all-tools
   add-prefs-panel))

(define-signature drracket:tools-drs-cm^
  ())
(define-signature drracket:tools-drs^ extends drracket:tools-drs-cm^
  (invoke-drs-tool))

(define-signature drracket:get/extend-cm^
  ())
(define-signature drracket:get/extend^ extends drracket:get/extend-cm^
  (extend-tab
   extend-interactions-text
   extend-definitions-text
   extend-interactions-canvas
   extend-definitions-canvas
   extend-unit-frame
   get-tab
   get-interactions-text
   get-definitions-text
   get-interactions-canvas
   get-definitions-canvas
   get-unit-frame))

(define-signature drracket:unit-cm^
  (tab%
   tab<%>
   frame% 
   frame<%>
   definitions-canvas%
   get-definitions-text%
   definitions-text<%>
   interactions-canvas%))
(define-signature drracket:unit^ extends drracket:unit-cm^
  (open-drscheme-window
   find-symbol
   get-program-editor-mixin
   add-to-program-editor-mixin
   forget-saved-bug-report
   record-saved-bug-report
   (struct teachpack-callbacks (get-names remove add))
   make-teachpack-callbacks
   add-search-help-desk-menu-item))

(define-signature drracket:frame-cm^
  (<%>
   mixin
   basics-mixin
   basics<%>))
(define-signature drracket:frame^ extends drracket:frame-cm^
  (create-root-menubar
   add-keybindings-item
   planet-spec?))

(define-signature drracket:program-cm^
  (frame%))
(define-signature drracket:program^ extends drracket:program-cm^
  ())

(define-signature drracket:eval-cm^
  ())
(define-signature drracket:eval^ extends drracket:eval-cm^
  (expand-program
   expand-program/multiple
   traverse-program/multiple
   build-user-eventspace/custodian
   set-basic-parameters
   get-snip-classes))

(define-signature drracket:text-cm^
  (text<%>
   text%))
(define-signature drracket:text^ extends drracket:text-cm^
  ())

(define-signature drracket:setup-cm^
  ())
(define-signature drracket:setup^ extends drracket:setup-cm^ 
  (do-setup))

(define-signature drracket:rep-cm^
  (drs-bindings-keymap-mixin
   text%
   text<%>
   context<%>))
(define-signature drracket:rep^ extends drracket:rep-cm^
  (current-rep
   current-language-settings
   current-value-port
   get-drs-bindings-keymap
   error-delta
   get-welcome-delta 
   get-dark-green-delta
   drs-autocomplete-mixin))

(define-signature drracket:app-cm^
  ())
(define-signature drracket:app^ extends drracket:app-cm^
  (about-drscheme
   add-language-items-to-help-menu
   add-important-urls-to-help-menu
   switch-language-to))

(define-signature drracket:draw-arrow-cm^
  ())
(define-signature drracket:draw-arrow^ extends drracket:draw-arrow-cm^
  (draw-arrow))

(define-signature drracket:help-desk-cm^
  ())
(define-signature drracket:help-desk^ extends drracket:help-desk-cm^
  (help-desk
   goto-plt-license
   get-docs))

(define-signature drracket:language-cm^
  (language<%>
   module-based-language<%>
   simple-module-based-language<%>
   simple-module-based-language%
   simple-module-based-language->module-based-language-mixin
   module-based-language->language-mixin))
(define-signature drracket:language^ extends drracket:language-cm^
  (get-default-mixin
   extend-language-interface
   get-language-extensions
   
   create-module-based-launcher
   create-module-based-stand-alone-executable
   create-module-based-distribution
   
   create-distribution-for-executable
   
   create-executable-gui
   put-executable
   
   ;(struct loc (source position line column span))
   make-text/pos
   (struct text/pos (text start end))
   make-simple-settings
   (struct simple-settings (case-sensitive 
                            printing-style 
                            fraction-style
                            show-sharing
                            insert-newlines
                            annotations))
   simple-settings->vector
   
   simple-module-based-language-config-panel
   simple-module-based-language-convert-value
   setup-printing-parameters
   
   add-snip-value
   setup-setup-values
   
   register-capability
   capability-registered?
   get-capability-default
   get-capability-contract))

(define-signature drracket:multi-file-search-cm^
  ())
(define-signature drracket:multi-file-search^ extends drracket:multi-file-search-cm^
  (multi-file-search
   search-type-params
   search-types))

(define-signature drracket:module-overview-cm^
  ())
(define-signature drracket:module-overview^ extends drracket:module-overview-cm^
  (module-overview
   make-module-overview-pasteboard
   fill-pasteboard))

(define-signature drracket:tracing-cm^
  (tab-mixin
   frame-mixin))
(define-signature drracket:tracing^ extends drracket:tracing-cm^
  (annotate))

(define-signature drracket:tool-exports-cm^
  ())
(define-signature drracket:tool-exports^ extends drracket:tool-exports-cm^
  (phase1 
   phase2))

(define-signature no-prefix:tool-cm^
  ((open (prefix debug: drracket:debug-cm^))
   (open (prefix unit: drracket:unit-cm^))
   (open (prefix rep: drracket:rep-cm^))
   (open (prefix frame: drracket:frame-cm^))
   (open (prefix get/extend: drracket:get/extend-cm^))
   (open (prefix language-configuration: drracket:language-configuration-cm^))
   (open (prefix language: drracket:language-cm^))
   (open (prefix help-desk: drracket:help-desk-cm^))
   (open (prefix eval: drracket:eval-cm^))
   (open (prefix font: drracket:font-cm^))
   (open (prefix modes: drracket:modes-cm^))
   (open (prefix tracing: drracket:tracing-cm^))
   (open (prefix module-language: drracket:module-language-cm^))
   (open (prefix module-language-tools: drracket:module-language-tools-cm^))))

(define-signature drracket:tool-cm^
  ((open (prefix drracket: no-prefix:tool-cm^))))
(define-signature drscheme:tool-cm^
  ((open (prefix drscheme: no-prefix:tool-cm^))))

(define-signature no-prefix:tool^ 
  ((open (prefix debug: drracket:debug^))
   (open (prefix unit: drracket:unit^))
   (open (prefix rep: drracket:rep^))
   (open (prefix frame: drracket:frame^))
   (open (prefix get/extend: drracket:get/extend^))
   (open (prefix language-configuration: drracket:language-configuration^))
   (open (prefix language: drracket:language^))
   (open (prefix help-desk: drracket:help-desk^))
   (open (prefix eval: drracket:eval^))
   (open (prefix modes: drracket:modes^))
   (open (prefix tracing: drracket:tracing^))
   (open (prefix module-language: drracket:module-language^))
   (open (prefix module-language-tools: drracket:module-language-tools^))))

(define-signature drracket:tool^
  ((open (prefix drracket: no-prefix:tool^))))
(define-signature drscheme:tool^
  ((open (prefix drscheme: no-prefix:tool^))))
