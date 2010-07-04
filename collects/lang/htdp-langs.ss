#lang scheme
(require string-constants
         framework
         (prefix-in et: errortrace/stacktrace)
         mzlib/pretty
         (prefix-in pc: mzlib/pconvert)
         mzlib/file
         mzlib/unit
         mzlib/class
         mzlib/list
         mzlib/struct
         mzlib/compile
         mzlib/struct
         drscheme/tool
         mred
         framework/private/bday
         syntax/moddep
         mrlib/cache-image-snip
         compiler/embed
         wxme/wxme
         setup/dirs
         
         ;; this module is shared between the drscheme's namespace (so loaded here) 
         ;; and the user's namespace in the teaching languages
         "private/set-result.ss"
         
         "stepper-language-interface.ss"           
         "debugger-language-interface.ss"
         "run-teaching-program.ss"
         stepper/private/shared
         
         (only-in test-engine/scheme-gui make-formatter)
         (only-in test-engine/scheme-tests scheme-test-data error-handler test-format test-execute)
         (lib "test-engine/test-display.scm")
         )
  
  
  (provide tool@)
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define user-installed-teachpacks-collection "installed-teachpacks")
  (define teachpack-installation-dir (build-path (find-user-collects-dir) user-installed-teachpacks-collection))
  
  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      
      (define drs-eventspace (current-eventspace))
      
      ;; tracing? : boolean
      ;; teachpacks : (listof require-spec)
      (define-struct (htdp-lang-settings drscheme:language:simple-settings) (tracing? teachpacks))
      (define htdp-lang-settings->vector (make-->vector htdp-lang-settings))
      
      (define image-string "<image>")
      
      (define htdp-language<%>
        (interface ()
          get-module
          get-language-position
          get-sharing-printing
          get-abbreviate-cons-as-list
          get-allow-sharing?
          get-use-function-output-syntax?
          get-accept-quasiquote?
          get-read-accept-dot))
      
      ;; module-based-language-extension :    (implements drscheme:language:module-based-language<%>) 
      ;;                                   -> (implements drscheme:language:module-based-language<%>)
      ;; changes the default settings and sets a few more paramters during `on-execute'
      (define (module-based-language-extension super%)
        (class* super% ()
          
          (inherit get-sharing-printing get-abbreviate-cons-as-list)
          
          (define/override (default-settings)
            (make-htdp-lang-settings 
             #t
             'constructor
             'repeating-decimal
             (get-sharing-printing)
             #t
             'none
             #f 
             (preferences:get 'drscheme:htdp:last-set-teachpacks)))
          
          (define/override (default-settings? s)
            (and (super default-settings? s)
                 (not (htdp-lang-settings-tracing? s))
                 (null? (htdp-lang-settings-teachpacks s))))
          
          (define/override (marshall-settings x)
            (list (super marshall-settings x)
                  (htdp-lang-settings-tracing? x)
                  (htdp-lang-settings-teachpacks x)))
          
          (define/override (unmarshall-settings x)
            (if (and (list? x)
                     (= (length x) 3)
                     (boolean? (list-ref x 1))
                     (list-of-require-specs? (list-ref x 2)))
                (let ([drs-settings (super unmarshall-settings (first x))])
                  (make-htdp-lang-settings
                   (drscheme:language:simple-settings-case-sensitive drs-settings)
                   (drscheme:language:simple-settings-printing-style  drs-settings)
                   (drscheme:language:simple-settings-fraction-style  drs-settings)
                   (drscheme:language:simple-settings-show-sharing  drs-settings)
                   (drscheme:language:simple-settings-insert-newlines  drs-settings)
                   (drscheme:language:simple-settings-annotations drs-settings)
                   (cadr x)
                   (caddr x)))
                (default-settings)))
          
          (define/private (list-of-require-specs? l)
            (and (list? l)
                 (andmap (λ (x)
                           (and (list? x)
                                (andmap (λ (x) (or (string? x) (symbol? x))) x)))
                         l)))
          
          (inherit get-allow-sharing? get-use-function-output-syntax? 
                   get-accept-quasiquote? get-read-accept-dot)
          (define/override (config-panel parent)
            (sharing/not-config-panel (get-allow-sharing?) (get-accept-quasiquote?) parent))
          
          (define/override (on-execute settings run-in-user-thread)
            (let ([drs-namespace (current-namespace)]
                  [set-result-module-name 
                   ((current-module-name-resolver) '(lib "lang/private/set-result.ss") #f #f)]
                  [scheme-test-module-name
                   ((current-module-name-resolver) '(lib "test-engine/scheme-tests.ss") #f #f)])
              (run-in-user-thread
               (lambda ()
                 (read-accept-quasiquote (get-accept-quasiquote?))
                 (namespace-attach-module drs-namespace ''drscheme-secrets)
                 (namespace-attach-module drs-namespace set-result-module-name)                 
                 (error-display-handler teaching-languages-error-display-handler)
                 (error-value->string-handler (λ (x y) (teaching-languages-error-value->string settings x y)))
                 (current-eval (add-annotation (htdp-lang-settings-tracing? settings) (current-eval)))
                 (error-print-source-location #f)
                 (read-decimal-as-inexact #f)
                 (read-accept-dot (get-read-accept-dot))
                 (namespace-attach-module drs-namespace scheme-test-module-name)
                 (namespace-require scheme-test-module-name)
                 (scheme-test-data (list (drscheme:rep:current-rep) drs-eventspace test-display%))
                 (test-execute (get-preference 'tests:enable? (lambda () #t)))
                 (test-format (make-formatter (lambda (v o) (render-value/format v settings o 40)))))))
            (super on-execute settings run-in-user-thread))
          
          (define/private (teaching-languages-error-value->string settings v len)
            (let ([sp (open-output-string)])
              (set-printing-parameters settings (λ () (print v sp)))
              (flush-output sp)
              (let ([s (get-output-string sp)])
                (cond
                  [(<= (string-length s) len) s]
                  [else (string-append (substring s 0 (- len 3)) "...")]))))

	  ;; set-printing-parameters : settings ( -> TST) -> TST
	  ;; is implicitly exposed to the stepper.  watch out!  --  john
          (define/public (set-printing-parameters settings thunk)
            (parameterize ([pc:booleans-as-true/false #t]
                           [pc:abbreviate-cons-as-list (get-abbreviate-cons-as-list)]
                           [pc:current-print-convert-hook
                            (let ([ph (pc:current-print-convert-hook)])
                              (lambda (val basic sub)
                                (cond
                                  [(equal? val set!-result) '(void)]
                                  [else (ph val basic sub)])))]
                           [pretty-print-show-inexactness #t]
                           [pretty-print-exact-as-decimal #t]
                           [pc:use-named/undefined-handler
                            (lambda (x)
                              (and (get-use-function-output-syntax?)
                                   (procedure? x)
                                   (object-name x)))]
                           [pc:named/undefined-handler
                            (lambda (x)
                              (string->symbol
                               (format "function:~a" (object-name x))))])
              (thunk)))
          
          (define/override (render-value/format value settings port width)
            (set-printing-parameters
             settings
             (lambda ()
               (super render-value/format value settings port width))))
          
          (define/override (render-value value settings port)
            (set-printing-parameters
             settings
             (lambda ()
               (super render-value value settings port))))
          
          (super-new)))
      
      ;; sharing/not-config-panel :  boolean boolean parent -> (case-> (-> settings) (settings -> void))
      ;; constructs the config-panel for a language without a sharing option.
      (define (sharing/not-config-panel allow-sharing-config? accept-quasiquote? _parent)
        (let* ([parent (make-object vertical-panel% _parent)]
               
               [input-panel (instantiate group-box-panel% ()
                              (parent parent)
                              (label (string-constant input-syntax))
                              (alignment '(left center)))]
               
               [output-panel (instantiate group-box-panel% ()
                               (parent parent)
                               (label (string-constant output-syntax))
                               (alignment '(left center)))]
               
               [tp-group-box (instantiate group-box-panel% ()
                               (label (string-constant teachpacks))
                               (parent parent)
                               (alignment '(center top)))]
               [tp-panel (new vertical-panel%
                              [parent tp-group-box]
                              [alignment '(center center)]
                              [stretchable-width #f]
                              [stretchable-height #f])]
               
               [case-sensitive (make-object check-box%
                                 (string-constant case-sensitive-label)
                                 input-panel
                                 void)]
               [output-style (make-object radio-box%
                               (string-constant output-style-label)
                               (if accept-quasiquote?
                                   (list (string-constant constructor-printing-style)
                                         (string-constant quasiquote-printing-style)
                                         (string-constant write-printing-style))
                                   (list (string-constant constructor-printing-style)
                                         (string-constant write-printing-style)))
                               output-panel
                               void)]
               [fraction-style
                (make-object radio-box% (string-constant fraction-style)
                  (list (string-constant use-mixed-fractions)
                        (string-constant use-repeating-decimals))
                  output-panel
                  void)]
               [show-sharing #f]
               [insert-newlines (make-object check-box%
                                  (string-constant use-pretty-printer-label)
                                  output-panel
                                  void)]
               [tracing (new check-box%
                             (parent output-panel)
                             (label (string-constant tracing-enable-tracing))
                             (callback void))]
               
               [tps '()])
          
          (when allow-sharing-config?
            (set! show-sharing
                  (instantiate check-box% ()
                    (parent output-panel)
                    (label (string-constant sharing-printing-label))
                    (callback void))))
          
          ;; set the characteristics of the GUI
          (send _parent set-alignment 'center 'center)
          (send parent stretchable-height #f)
          (send parent stretchable-width #f)
          (send parent set-alignment 'center 'center)
          
          (case-lambda
            [()
             (make-htdp-lang-settings
              (send case-sensitive get-value)
              (if accept-quasiquote?
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'quasiquote]
                    [(2) 'write])
                  (case (send output-style get-selection)
                    [(0) 'constructor]
                    [(1) 'write]))
              (case (send fraction-style get-selection)
                [(0) 'mixed-fraction]
                [(1) 'repeating-decimal])
              (and allow-sharing-config? (send show-sharing get-value))
              (send insert-newlines get-value)
              'none
              (send tracing get-value)
              tps)]
            [(settings)
             (send case-sensitive set-value (drscheme:language:simple-settings-case-sensitive settings))
             (send output-style set-selection
                   (if accept-quasiquote?
                       (case (drscheme:language:simple-settings-printing-style settings)
                         [(constructor) 0]
                         [(quasiquote) 1]
                         [(write) 2]
                         [(print) 2])
                       (case (drscheme:language:simple-settings-printing-style settings)
                         [(constructor) 0]
                         [(quasiquote) 0]
                         [(write) 1]
                         [(print) 1])))
             (send fraction-style set-selection
                   (case (drscheme:language:simple-settings-fraction-style settings)
                     [(mixed-fraction) 0]
                     [(repeating-decimal) 1]))
             (when allow-sharing-config?
               (send show-sharing set-value (drscheme:language:simple-settings-show-sharing settings)))
             (send insert-newlines set-value 
                   (drscheme:language:simple-settings-insert-newlines settings))
             (set! tps (htdp-lang-settings-teachpacks settings))
             (send tp-panel change-children (λ (l) '()))
             (if (null? tps)
                 (new message%
                      [parent tp-panel]
                      [label (string-constant teachpacks-none)])
                 (for-each
                  (λ (tp) (new message% 
                               [parent tp-panel]
                               [label (format "~s" tp)]))
                  tps))
             (send tracing set-value (htdp-lang-settings-tracing? settings))
             (void)])))
      
      (define simple-htdp-language%
        (class* drscheme:language:simple-module-based-language% (htdp-language<%>)
          (init-field sharing-printing
                      abbreviate-cons-as-list
                      allow-sharing?
                      manual
                      reader-module
                      (use-function-output-syntax? #f)
                      (accept-quasiquote? #t)
                      (read-accept-dot #f)
                      (style-delta #f))
          (define/public (get-sharing-printing) sharing-printing)
          (define/public (get-abbreviate-cons-as-list) abbreviate-cons-as-list)
          (define/public (get-allow-sharing?) allow-sharing?)
          (define/public (get-manual) manual)
          (define/public (get-use-function-output-syntax?) use-function-output-syntax?)
          (define/public (get-accept-quasiquote?) accept-quasiquote?)
          (define/public (get-read-accept-dot) read-accept-dot)
          ;(define/override (get-one-line-summary) one-line-summary)
          (define/public (get-htdp-style-delta) style-delta)
          
          (super-new [language-url "http://www.htdp.org/"])))
      
      (define (language-extension %)
        (class %
          (inherit get-manual)
          
          (define/override (extra-repl-information settings port) 
            (define welcome (drscheme:rep:get-welcome-delta))
            (define (go str sd)
              (let* ([s (make-object string-snip% str)]
                     [sl (editor:get-standard-style-list)]
                     [std (send sl find-named-style "Standard")]
                     [style (send sl find-or-create-style std sd)])
                (send s set-style style)
                (write-special s port)))
            
            (define tps (htdp-lang-settings-teachpacks settings))
            
            (unless (null? tps)
              (go "Teachpack" welcome)
              (cond
                [(= 1 (length tps))
                 (go ": " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))]
                [(= 2 (length tps))
                 (go "s: " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (go " and " welcome)
                 (go (cadr (cadr tps)) (drscheme:rep:get-dark-green-delta))]
                [else
                 (go "s: " welcome)
                 (go (cadr (car tps)) (drscheme:rep:get-dark-green-delta))
                 (let loop ([these-tps (cdr tps)])
                   (cond
                     [(null? (cdr these-tps))
                      (go ", and " welcome)
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))]
                     [else
                      (go ", " welcome)
                      (go (cadr (car these-tps)) (drscheme:rep:get-dark-green-delta))
                      (loop (cdr these-tps))]))])
              (go "." welcome)
              (newline port)))
          
          (inherit get-module get-transformer-module get-init-code
                   use-namespace-require/copy?)
          (define/override (create-executable setting parent program-filename)
            (let ([dist-filename
		   (drscheme:language:put-executable
		    parent program-filename
		    'distribution
		    #t 
		    (string-constant save-a-mred-distribution))])
              (when dist-filename
                (drscheme:language:create-distribution-for-executable 
                 dist-filename
                 #t
                 (λ (exe-name)
                   (create-embedding-executable 
                    exe-name
                    #:modules `((#f ,program-filename))
                    #:cmdline `("-l" 
                                "scheme/base"
                                "-e"
                                ,(format "~s" `(#%require ',(filename->require-symbol program-filename))))
                    #:src-filter
                    (λ (path) (cannot-compile? path))
                    #:get-extra-imports
                    (λ (path cm)
                      (call-with-input-file path
                        (λ (port)
                          (cond
                            [(is-wxme-stream? port)
                             (append
                              ;; Extract snip-related modules:
                              (let-values ([(snip-class-names data-class-names)
                                            (extract-used-classes port)])
                                (list*
                                 '(lib "wxme/read.ss")
                                 '(lib "mred/mred.ss")
                                 reader-module
                                 (filter
                                  values
                                  (map (λ (x) (string->lib-path x #t))
                                       (append
                                        snip-class-names
                                        data-class-names)))))
                              ;; Extract reader-related modules:
                              (begin
                                (file-position port 0)
                                (let ([mods null])
                                  (parameterize ([current-reader-guard
                                                  (let ([g (current-reader-guard)])
                                                    (lambda (p)
                                                      (set! mods (cons p mods))
                                                      (g p)))])
                                    (read-language (wxme-port->port port) (lambda () #f)))
                                  mods)))]
                            [else
                             '()]))))
                    #:mred? #t))))))

          (define/private (filename->require-symbol fn)
            (let-values ([(base name dir) (split-path fn)])
              (string->symbol
               (path->string
                (path-replace-suffix name #"")))))
          
          (define/private (get-export-names sexp)
            (let* ([sym-name ((current-module-name-resolver) sexp #f #f)]
                   [no-ext-name (substring (symbol->string sym-name)
                                           1
                                           (string-length (symbol->string sym-name)))]
                   [full-name
                    (cond
                      [(file-exists? (string-append no-ext-name ".ss"))
                       (string-append no-ext-name ".ss")]
                      [(file-exists? (string-append no-ext-name ".scm"))
                       (string-append no-ext-name ".scm")]
                      [(file-exists? no-ext-name)
                       no-ext-name]
                      [else (error 'htdp-lang.ss "could not find language filename ~s" no-ext-name)])]
                   [base-dir (let-values ([(base _1 _2) (split-path full-name)]) base)]
                   [stx
                    (call-with-input-file full-name
                      (lambda (port)
                        (read-syntax full-name port)))]
                   [code
                    (parameterize ([current-load-relative-directory base-dir]
                                   [current-directory base-dir])
                      (expand stx))]
                   [find-name
                    (lambda (p)
                      (cond
                        [(symbol? p) p]
                        [(and (pair? p) (pair? (cdr p)))
                         (cadr p)]
                        [else (car p)]))])
              (append
               (map find-name (syntax-property code 'module-variable-provides))
               (map find-name (syntax-property code 'module-syntax-provides)))))

          (define/private (symbol-append x y)
            (string->symbol
             (string-append
              (symbol->string x)
              (symbol->string y))))
          
          (inherit get-htdp-style-delta)
          (define/override (get-style-delta)
            (get-htdp-style-delta))
          
          (inherit get-reader set-printing-parameters)
          
          (define/override (front-end/complete-program port settings)
            (expand-teaching-program port  
                                     (get-reader)
                                     (get-module)
                                     (htdp-lang-settings-teachpacks settings)
                                     (drscheme:rep:current-rep)))

          (define keywords #f)
          (define/augment (capability-value key)
            (case key
              [(drscheme:autocomplete-words)
               (unless keywords 
                 (set! keywords (text:get-completions/manuals #f))) ;; complete with everything, which is wrong ..
               keywords]
              [(drscheme:teachpack-menu-items) htdp-teachpack-callbacks]
              [(drscheme:special:insert-lambda) #f]
              #;
              ;; FIXME: disable context for now, re-enable when it is possible
              ;; to have the context search the teachpack manual too.
              [(drscheme:help-context-term)
               (let* ([m (get-module)]
                      [m (and m (pair? m) (pair? (cdr m)) (cadr m))]
                      [m (and m (regexp-match #rx"^(lang/[^/.]+).ss$" m))]
                      [m (and m (cadr m))])
                 (if m
                   (format "L:~a" m)
                   (error 'drscheme:help-context-term
                          "internal error: unexpected module spec")))]
              [(tests:test-menu tests:dock-menu) #t]
              [else (inner (drscheme:language:get-capability-default key) 
                           capability-value
                           key)]))
          
          (define htdp-teachpack-callbacks
            (drscheme:unit:make-teachpack-callbacks
             (λ (settings) 
               (map cadr (htdp-lang-settings-teachpacks settings)))
             (λ (settings parent) 
               (let ([teachpack (get-teachpack-from-user parent)])
                 (if teachpack
                     (let ([old-tps (htdp-lang-settings-teachpacks settings)])
                       (if (member teachpack old-tps)
                           (begin
                             (message-box (string-constant drscheme)
                                          (format (string-constant already-added-teachpack)
                                                  (cadr teachpack)))
                             settings)
                           
                           (let ([new-tps (append old-tps (list teachpack))])
                             (preferences:set 'drscheme:htdp:last-set-teachpacks new-tps)
                             (make-htdp-lang-settings
                              (drscheme:language:simple-settings-case-sensitive settings)
                              (drscheme:language:simple-settings-printing-style settings)
                              (drscheme:language:simple-settings-fraction-style settings)
                              (drscheme:language:simple-settings-show-sharing settings)
                              (drscheme:language:simple-settings-insert-newlines settings)
                              (drscheme:language:simple-settings-annotations settings)
                              (htdp-lang-settings-tracing? settings)
                              new-tps))))
                     settings)))
             (λ (settings name) 
               (let ([new-tps (filter (λ (x) (not (equal? (cadr x) name)))
                                      (htdp-lang-settings-teachpacks settings))])
                 (preferences:set 'drscheme:htdp:last-set-teachpacks new-tps)
                 (make-htdp-lang-settings
                  (drscheme:language:simple-settings-case-sensitive settings)
                  (drscheme:language:simple-settings-printing-style settings)
                  (drscheme:language:simple-settings-fraction-style settings)
                  (drscheme:language:simple-settings-show-sharing settings)
                  (drscheme:language:simple-settings-insert-newlines settings)
                  (drscheme:language:simple-settings-annotations settings)
                  (htdp-lang-settings-tracing? settings)
                  new-tps)))
             (λ (settings) 
               (preferences:set 'drscheme:htdp:last-set-teachpacks '())
               (make-htdp-lang-settings
                (drscheme:language:simple-settings-case-sensitive settings)
                (drscheme:language:simple-settings-printing-style settings)
                (drscheme:language:simple-settings-fraction-style settings)
                (drscheme:language:simple-settings-show-sharing settings)
                (drscheme:language:simple-settings-insert-newlines settings)
                (drscheme:language:simple-settings-annotations settings)
                (htdp-lang-settings-tracing? settings)
                '()))))
        
          (inherit-field reader-module)
          (define/override (get-reader-module) reader-module)
          (define/override (get-metadata modname settings)
            (string-append
             ";; The first three lines of this file were inserted by DrScheme. They record metadata\n"
             ";; about the language level of this file in a form that our tools can easily process.\n"
             (format "#reader~s~s\n"
                     reader-module
                     `((modname ,modname)
                       (read-case-sensitive ,(drscheme:language:simple-settings-case-sensitive settings))
                       (teachpacks ,(htdp-lang-settings-teachpacks settings))
                       (htdp-settings ,(htdp-lang-settings->vector settings))))))
          
          (inherit default-settings)
          (define/override (metadata->settings metadata)
            (let* ([table (metadata->table metadata)] ;; extract the table
                   [ssv (assoc 'htdp-settings table)])
              (if ssv
                  (let ([settings-list (vector->list (cadr ssv))])
                    (if (equal? (length settings-list)
                                (procedure-arity make-htdp-lang-settings))
                        (apply make-htdp-lang-settings settings-list)
                        (default-settings)))
                  (default-settings))))
          
          (define/private (metadata->table metadata)
            (let ([p (open-input-string metadata)])
              (regexp-match #rx"\n#reader" p) ;; skip to reader
              (read p) ;; skip module
              (read p)))
          
          (define/override (get-metadata-lines) 3)
          
          (super-new)))
      
      ;; cannot-compile? : path -> boolean
      ;; returns #t if the file cannot be compiled, #f otherwise
      (define (cannot-compile? path)
        (call-with-input-file path
          (λ (port) 
            (let ([ok-to-compile-names 
                   (map (λ (x) (format "~s" x))
                        '(wxtext
                          (lib "comment-snip.ss" "framework")
                          (lib "xml-snipclass.ss" "xml")
                          (lib "scheme-snipclass.ss" "xml")))])
              (and (is-wxme-stream? port)
                   (let-values ([(snip-class-names data-class-names)
                                 (extract-used-classes port)])
                     (not (and (andmap
                                (λ (used-name) (member used-name ok-to-compile-names))
                                snip-class-names)
                               (andmap
                                (λ (used-name) (member used-name ok-to-compile-names))
                                data-class-names)))))))))
      
      (define (get-teachpack-from-user parent)
        (define tp-dirs (list (collection-path "teachpack" "htdp")
                              (collection-path "teachpack" "2htdp")))
        (define columns 2)
        (define tps (apply
                     append
                     (map (λ (tp-dir)
                            (filter
                             (λ (x) (file-exists? (build-path tp-dir x)))
                             (directory-list tp-dir)))
                          tp-dirs)))
        (define sort-order (λ (x y) (string<=? (path->string x) (path->string y))))
        (define pre-installed-tps (sort tps sort-order))
        (define dlg (new dialog% [parent parent] [label (string-constant drscheme)]))
        (define hp (new horizontal-panel% [parent dlg]))
        (define answer #f)
        (define compiling? #f)
        
        (define pre-installed-gb (new group-box-panel%
                                      [label (string-constant teachpack-pre-installed)]
                                      [parent hp]))
        (define user-installed-gb (new group-box-panel%
                                       [label (string-constant teachpack-user-installed)]
                                       [parent hp]))
        
        (define pre-installed-lb
          (new list-box%
               [label #f]
               [choices (map path->string pre-installed-tps)]
               [stretchable-height #t]
               [min-height 300]
               [min-width 200]
               [callback
                (λ (x evt)
                  (case (send evt get-event-type)
                    [(list-box-dclick) (selected pre-installed-lb)]
                    [else
                     (clear-selection user-installed-lb)
                     (update-button)]))]
               [parent pre-installed-gb]))
        
        (define user-installed-lb
          (new list-box%
               [label #f]
               [choices '()]
               [stretchable-height #t]
               [min-width 200]
               [callback
                (λ (x evt)
                  (case (send evt get-event-type)
                    [(list-box-dclick) (selected user-installed-lb)]
                    [else
                     (clear-selection pre-installed-lb)
                     (update-button)]))]
               [parent user-installed-gb]))
        
        (define (selected lb)
          (unless compiling?
            (set! answer (figure-out-answer))
            (send dlg show #f)))
        
        (define (clear-selection lb)
          (for-each
           (λ (x) (send lb select x #f))
           (send lb get-selections)))
        
        (define add-button (new button%
                                [parent user-installed-gb]
                                [label (string-constant add-teachpack-to-list...)]
                                [callback (λ (x y) (install-teachpack))]))
        
        (define (install-teachpack)
          (let ([file (get-file (string-constant select-a-teachpack) dlg)])
            (when file
              (let-values ([(base name dir) (split-path file)])
                (let ([dest-file (build-path teachpack-installation-dir name)])
                  (when (or (not (file-exists? dest-file))
                            (equal? 1
                                    (message-box/custom
                                     (string-constant drscheme)
                                     (format
                                      (string-constant teachpack-already-installed)
                                      (path->string name))
                                     (string-constant overwrite)
                                     (string-constant cancel)
                                     #f
                                     dlg
                                     '(default=2 caution))))
                    (make-directory* teachpack-installation-dir)
                    (when (file-exists? dest-file)
                      (delete-file dest-file))
                    (copy-file file dest-file)
                    
                    ;; compiling the teachpack should be the last thing in this GUI callback
                    (compile-new-teachpack dest-file)))))))
        
        (define (compile-new-teachpack filename)
          (let-values ([(_1 short-name _2) (split-path filename)])
            (cond
              [(cannot-compile? filename)
               (post-compilation-gui-cleanup short-name)]
              [else
               (send compiling-message set-label
                     (format (string-constant compiling-teachpack) 
                             (path->string short-name)))
               (starting-compilation)
               (let ([nc (make-custodian)]
                     [exn #f])
                 (let ([t 
                        (parameterize ([current-custodian nc])
                          (thread (λ () 
                                    (with-handlers ((exn? (λ (x) (set! exn x))))
                                      (parameterize ([read-accept-reader #t]
                                                     [current-namespace (make-base-namespace)])
                                        (compile-file filename))))))])
                   (thread
                    (λ ()
                      (thread-wait t)
                      (queue-callback
                       (λ () 
                         (cond
                           [exn
                            (message-box (string-constant drscheme)
                                         (exn-message exn))
                            (delete-file filename)
                            (update-user-installed-lb)]
                           [else
                            (post-compilation-gui-cleanup short-name)])
                         (done-compilation)
                         (send compiling-message set-label "")))))))])))
        
        (define (post-compilation-gui-cleanup short-name)
          (update-user-installed-lb)
          (clear-selection pre-installed-lb)
          (send user-installed-lb set-string-selection (path->string short-name)))
        
        (define (starting-compilation)
          (set! compiling? #t)
          (update-button)
          (send cancel-button enable #f))
        
        (define (done-compilation)
          (set! compiling? #f)
          (update-button)
          (send cancel-button enable #t))
        
        (define (update-user-installed-lb)
          (let ([files
                 (if (directory-exists? teachpack-installation-dir)
                     (map path->string 
                          (filter 
                           (λ (x) (file-exists? (build-path teachpack-installation-dir x)))
                           (directory-list teachpack-installation-dir)))
                     '())])
            (send user-installed-lb set (sort files string<=?))))
        
        
        (define (update-button)
          (send ok-button enable 
                (and (not compiling?)
                     (or (pair? (send user-installed-lb get-selections))
                         (pair? (send pre-installed-lb get-selections))))))
        
        (define button-panel (new horizontal-panel% 
                                  [parent dlg]
                                  [alignment '(right center)]
                                  [stretchable-height #f]))
        (define compiling-message (new message% [parent button-panel] [label ""] [stretchable-width #t]))
        (define-values (ok-button cancel-button)
          (gui-utils:ok/cancel-buttons button-panel
                                       (λ (b e)
                                         (set! answer (figure-out-answer))
                                         (send dlg show #f))
                                       (λ (b e) 
                                         (send dlg show #f))
                                       (string-constant ok) (string-constant cancel)))
        
        (define (figure-out-answer)
          (cond
            [(send pre-installed-lb get-selection)
             =>
             (λ (i)
               (define f (send pre-installed-lb get-string i))
               (cond
                 [(file-exists? (build-path (collection-path "teachpack" "htdp") f))
                  `(lib ,f "teachpack" "htdp")]
                 [(file-exists? (build-path (collection-path "teachpack" "2htdp") f))
                  `(lib ,f "teachpack" "2htdp")]
                 [else (error 'figuer-out-answer "argh: ~a ~a" 
                              (collection-path "teachpack" "htdp") f)]))]
            [(send user-installed-lb get-selection)
             =>
             (λ (i) `(lib ,(send user-installed-lb get-string i)
                          ,user-installed-teachpacks-collection))]
            [else (error 'figure-out-answer "no selection!")]))
        
        
        (send ok-button enable #f)
        (update-user-installed-lb)
        
        (send dlg show #t)
        answer)

      (define (stepper-settings-language %)
        (if (implementation? % stepper-language<%>)
            (class* % (stepper-language<%>)
              (init-field stepper:supported)
              (init-field stepper:enable-let-lifting)
	      (init-field stepper:show-lambdas-as-lambdas)
              (define/override (stepper:supported?) stepper:supported)
              (define/override (stepper:enable-let-lifting?) stepper:enable-let-lifting)
              (define/override (stepper:show-lambdas-as-lambdas?) stepper:show-lambdas-as-lambdas)
              (super-new))
            (class* % ()
              (init stepper:supported)
              (init stepper:enable-let-lifting)
              (init stepper:show-lambdas-as-lambdas)
              (super-new))))

      (define (debugger-settings-language %)
        (if (implementation? % debugger-language<%>)
            (class* % (debugger-language<%>)
              (init-field [debugger:supported #f])
              (define/override (debugger:supported?) debugger:supported)
              (super-new))
            (class %
              (init [debugger:supported #f])
              (super-new))))

      ;; filter/hide-ids : syntax[list] -> listof syntax
      (define (filter/hide-ids ids)
        ;; When a `define-values' or `define-syntax' declaration
        ;; is macro-generated, if the defined name also originates
        ;; from a macro, then the name is hidden to anything
        ;; that wasn't generated by the same macro invocation. This
        ;; hiding relies on renaming at the symbol level, and it's
        ;; exposed by the fact that `syntax-e' of the identifier 
        ;; returns a different name than `identifier-binding'.
        (filter
         (lambda (id)
           (let ([ib (identifier-binding id)])
             ;; ib should always be a 4-elem list, but
             ;; check, just in case:
             (or (not (pair? ib)) 
                 (eq? (syntax-e id)
                      (cadr ib)))))
         (syntax->list ids)))
                 
      
      ;                                                                                              
      ;                                                                                              
      ;                                                                                              
      ;   ;                                                             ;                     ;      
      ;   ;                                                             ;                     ;      
      ;   ;                                                             ;                     ;      
      ;   ; ;;    ;;;    ; ;;     ;; ;   ; ;;  ;;    ;;;    ; ;;        ; ;;    ;;;     ;;;   ;   ;  
      ;   ;;  ;  ;   ;   ;;  ;   ;  ;;   ;;  ;;  ;  ;   ;   ;;  ;       ;;  ;  ;   ;   ;   ;  ;  ;   
      ;   ;   ;      ;   ;   ;  ;    ;   ;   ;   ;      ;   ;   ;       ;   ;      ;  ;       ; ;    
      ;   ;   ;   ;;;;   ;   ;  ;    ;   ;   ;   ;   ;;;;   ;   ;       ;   ;   ;;;;  ;       ;;;    
      ;   ;   ;  ;   ;   ;   ;  ;    ;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;  ;       ;  ;   
      ;   ;   ;  ;   ;   ;   ;   ;  ;;   ;   ;   ;  ;   ;   ;   ;       ;   ;  ;   ;   ;   ;  ;   ;  
      ;   ;   ;   ;;;;;  ;   ;    ;; ;   ;   ;   ;   ;;;;;  ;   ;       ;   ;   ;;;;;   ;;;   ;    ; 
      ;                              ;                                                               
      ;                         ;    ;                                                               
      ;                          ;;;;                                                                
      

      ;; this inspector should be powerful enough to see
      ;; any structure defined in the user's namespace
      (define drscheme-inspector (current-inspector))
      (eval `(,#'module drscheme-secrets mzscheme
                        (provide drscheme-inspector)
                        (define drscheme-inspector ,drscheme-inspector)))
      (namespace-require ''drscheme-secrets)
      
      
      
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                                               
      ;                                 ;                             
      ;    ;;;   ; ;  ; ;   ;;;    ; ; ;;;;  ; ;  ;;;     ;;;    ;;;  
      ;   ;   ;  ;;   ;;   ;   ;   ;;   ;    ;;  ;   ;   ;   ;  ;   ; 
      ;  ;    ;  ;    ;   ;     ;  ;    ;    ;       ;  ;      ;    ; 
      ;  ;;;;;;  ;    ;   ;     ;  ;    ;    ;    ;;;;  ;      ;;;;;; 
      ;  ;       ;    ;   ;     ;  ;    ;    ;   ;   ;  ;      ;      
      ;   ;      ;    ;    ;   ;   ;    ;    ;   ;   ;   ;   ;  ;     
      ;    ;;;;  ;    ;     ;;;    ;     ;;  ;    ;;;;;   ;;;    ;;;; 
      ;                                                               
      ;                                                               
      ;                                                               
      
      
      
      
      ;; cm-key : symbol
      ;; the key used to put information on the continuation
      (define cm-key (gensym 'teaching-languages-continuation-mark-key))
      
      (define mf-note
        (let ([bitmap
               (make-object bitmap%
                 (build-path (collection-path "icons") "mf.gif"))])
          (and (send bitmap ok?)
               (make-object image-snip% bitmap))))
      
      ;; teaching-languages-error-display-handler : 
      ;;    (string (union TST exn) -> void) -> string exn -> void
      ;; adds in the bug icon, if there are contexts to display
      (define (teaching-languages-error-display-handler msg exn)
          
          (if (exn? exn)
              (display (exn-message exn) (current-error-port))
              (fprintf (current-error-port) "uncaught exception: ~e" exn))
          (fprintf (current-error-port) "\n")
          
          ;; need to flush here so that error annotations inserted in next line
          ;; don't get erased if this output were to happen after the insertion
          (flush-output (current-error-port))
          
          (let ([rep (drscheme:rep:current-rep)])
            (when (and (is-a? rep drscheme:rep:text<%>)
                       (eq? (send rep get-err-port) (current-error-port)))
              (let ([to-highlight 
                     (cond
                       [(exn:srclocs? exn) 
                        ((exn:srclocs-accessor exn) exn)]
                       [(exn? exn) 
                        (let ([cms (continuation-mark-set->list (exn-continuation-marks exn) cm-key)])
                          (if cms
                              (let loop ([cms cms])
                                (cond
                                  [(null? cms) '()]
                                  [else (let* ([cms (car cms)]
                                               [source (car cms)]
                                               [pos (cadr cms)]
                                               [span (cddr cms)])
                                          (if (or (path? source)
                                                  (symbol? source))
                                              (list (make-srcloc source #f #f pos span))
                                              (loop (cdr cms))))]))
                              '()))]
                       [else '()])])
                
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (lambda ()
                     ;; need to make sure that the user's eventspace is still the same
                     ;; and still running here?
                     (send rep highlight-errors to-highlight #f))))))))
      
      ;; with-mark : syntax syntax -> syntax
      ;; a member of stacktrace-imports^
      ;; guarantees that the continuation marks associated with cm-key are
      ;; members of the debug-source type
      (define (with-mark source-stx expr)
        (let ([source (syntax-source source-stx)]
              [start-position (syntax-position source-stx)]
              [span (syntax-span source-stx)])
          (if (and (or (symbol? source) (path? source))
                   (number? start-position)
                   (number? span))
              (with-syntax ([expr expr]
                            [mark (list* source start-position span)]
                            [cm-key cm-key])
                #`(with-continuation-mark 'cm-key
                    'mark
                    expr))
              expr)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  profiling infrastructure. Not used.
      ;;
      
      (define profile-key (gensym))
      (define (profiling-enabled) #f)
      (define (initialize-profile-point . x) (void))
      (define (register-profile-start . x) #f)
      (define (register-profile-done . x) (void))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  test coverage
      ;;
      
      (define test-coverage-enabled (make-parameter #t))
      (define current-test-coverage-info (make-thread-cell #f))
      
      (define (initialize-test-coverage-point key expr)
        (unless (thread-cell-ref current-test-coverage-info)
          (let ([ht (make-hasheq)])
            (thread-cell-set! current-test-coverage-info ht)
            (let ([rep (drscheme:rep:current-rep)])
              (when rep
                (parameterize ([current-eventspace drs-eventspace])
                  (queue-callback
                   (λ ()
                     (let ([on-sd (make-object style-delta%)]
                           [off-sd (make-object style-delta%)])
                       (cond
                         [(preferences:get 'framework:white-on-black?)
                          (send on-sd set-delta-foreground "white")
                          (send off-sd set-delta-foreground "indianred")]
                         [else
                          ;; picture 1.png
                          #;
                          (begin
                            (send on-sd set-delta-foreground "black")
                            (send off-sd set-delta-foreground "lightgray")
                            (send off-sd set-delta-background "firebrick"))
                          
                          ;; picture 2.png
                          #;
                          (begin
                            (send on-sd set-delta-foreground "darkgreen")
                            (send off-sd set-delta-foreground "firebrick")
                            (send off-sd set-delta-background "Khaki"))
                          
                          ;; picture 3.png
                          #;
                          (begin
                            (send on-sd set-delta-foreground "darkgreen")
                            (send off-sd set-delta-foreground "Khaki")
                            (send off-sd set-delta-background "black"))
                          
                          ;; picture 4.png
                          #;
                          (begin
                            (send on-sd set-delta-foreground "black")
                            (send off-sd set-delta-foreground "Khaki")
                            (send off-sd set-delta-background "darkblue"))
                          
                          ;; picture 5.png
                          #;
                          (begin
                            (send on-sd set-delta-foreground (make-object color% 0 80 0))
                            (send off-sd set-delta-foreground "orange")
                            (send off-sd set-delta-background "black"))
                          
                          ;; variation on 5.
                          (begin
                            (send on-sd set-delta-foreground "black")
                            (send on-sd set-transparent-text-backing-off #f)
                            (send on-sd set-transparent-text-backing-on #t)
                            (send off-sd set-delta-foreground "orange")
                            (send off-sd set-delta-background "black"))
                          
                          ;; mike's preferred color scheme, but looks just like the selection
                          #;
                          (begin
                            (send on-sd set-delta-foreground "black")
                            (send off-sd set-delta-background "lightblue")
                            (send off-sd set-delta-foreground "black"))])
                       (send rep set-test-coverage-info ht on-sd off-sd #f)))))))))
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (hash-set! ht key (mcons #f expr)))))
      
      (define (test-covered key)
        (let ([ht (thread-cell-ref current-test-coverage-info)])
          (when ht
            (let ([v (hash-ref ht key)])
              (set-mcar! v #t)))))
      
      (define-values/invoke-unit et:stacktrace@
        (import et:stacktrace-imports^) (export (prefix et: et:stacktrace^)))
     
      ;; add-annotation : boolean (sexp -> value) -> sexp -> value
      ;; adds debugging and test coverage information to `sexp' and calls `oe'
      (define (add-annotation tracing? oe)
        (let ([teaching-language-eval-handler
               (lambda (exp)
                 (let* ([is-compiled? (compiled-expression? (if (syntax? exp) (syntax-e exp) exp))]
                        [annotated
                         (if is-compiled? 
                             exp
                             (let* ([et-annotated (et:annotate-top (expand exp) 
                                                                   (namespace-base-phase))]
                                    [tr-annotated
                                     (if tracing?
                                         (drscheme:tracing:annotate (expand et-annotated))
                                         et-annotated)])
                               tr-annotated))])
                   (oe annotated)))])
          teaching-language-eval-handler))
      
     
      
;                                                                                                                
;                                                                                                                
;                                                                                                                
;                            ;                   ;   ;                                        ;                  
;                                                ;   ;                                        ;                  
;                   ;            ;               ;   ;       ;                           ;    ;                  
;   ; ;;    ;   ;  ;;;;      ;  ;;;;      ;;;    ;   ;      ;;;;   ;;;     ;; ;    ;;;  ;;;;  ; ;;     ;;;   ; ; 
;   ;;  ;   ;   ;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;   ;  ;    ;;  ;   ;   ;  ;;  
;   ;    ;  ;   ;   ;        ;   ;           ;   ;   ;       ;   ;     ; ;    ;  ;    ;  ;    ;   ;  ;    ;  ;   
;   ;    ;  ;   ;   ;        ;   ;        ;;;;   ;   ;       ;   ;     ; ;    ;  ;;;;;;  ;    ;   ;  ;;;;;;  ;   
;   ;    ;  ;   ;   ;        ;   ;       ;   ;   ;   ;       ;   ;     ; ;    ;  ;       ;    ;   ;  ;       ;   
;   ;;  ;   ;  ;;   ;        ;   ;       ;   ;   ;   ;       ;    ;   ;   ;  ;;   ;      ;    ;   ;   ;      ;   
;   ; ;;     ;; ;    ;;      ;    ;;      ;;;;;  ;   ;        ;;   ;;;     ;; ;    ;;;;   ;;  ;   ;    ;;;;  ;   
;   ;                                                                         ;                                  
;   ;                                                                    ;    ;                                  
;   ;                                                                     ;;;;                                   
      
      
      ;; add-htdp-language : (instanceof htdp-language<%>) -> void
      (define (add-htdp-language o)
        (drscheme:language-configuration:add-language o))
      
      (define (phase1) (void))

      ;; phase2 : -> void
      (define (phase2)
        (define htdp-language%
          (stepper-settings-language
           (debugger-settings-language
            ((drscheme:language:get-default-mixin)
             (language-extension
              (drscheme:language:module-based-language->language-mixin
               (module-based-language-extension
                (drscheme:language:simple-module-based-language->module-based-language-mixin
                 simple-htdp-language%))))))))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant advanced-one-line-summary))
           (module '(lib "lang/htdp-advanced.ss"))
           (manual #"advanced")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant advanced-student)))
           (language-id "plt:advanced-student")
           (language-numbers '(-500 -500 5))
           (sharing-printing #t)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #t)
           (reader-module '(lib "htdp-advanced-reader.ss" "lang"))
           (debugger:supported #t)
	   (stepper:supported #f)
	   (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #t)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate/lambda-one-line-summary))
           (module '(lib "lang/htdp-intermediate-lambda.ss"))
           (manual #"intermediate-lambda")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student/lambda)))
           (language-id "plt:intermediate-student/lambda")
           (style-delta (let ([match (regexp-match-positions
                                      "lambda"
                                      (string-constant intermediate-student/lambda))])
                          (if match
                              (let ([pos (car match)])
                                (list (list (make-object style-delta% 'change-family 'modern)
                                            (car pos)
                                            (cdr pos))))
                              #f)))
           (language-numbers '(-500 -500 4))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (reader-module '(lib "htdp-intermediate-lambda-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #t)))
        
	(add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant intermediate-one-line-summary))
           (module '(lib "lang/htdp-intermediate.ss"))
           (manual #"intermediate")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant intermediate-student)))
           (language-id "plt:intermediate-student")
           (language-numbers '(-500 -500 3))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (use-function-output-syntax? #t)
           (reader-module '(lib "htdp-intermediate-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning/abbrev-one-line-summary))
           (module '(lib "lang/htdp-beginner-abbr.ss"))
           (manual #"beginning-abbr")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student/abbrev)))
           (language-id "plt:beginning-student/abbrev")
           (language-numbers '(-500 -500 2))
           (sharing-printing #f)
           (abbreviate-cons-as-list #t)
           (allow-sharing? #f)
           (reader-module '(lib "htdp-beginner-abbr-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f)))
        
        (add-htdp-language
         (instantiate htdp-language% ()
           (one-line-summary (string-constant beginning-one-line-summary))
           (module '(lib "lang/htdp-beginner.ss"))
           (manual #"beginning")
           (language-position
            (list (string-constant teaching-languages)
                  (string-constant how-to-design-programs)
                  (string-constant beginning-student)))
           (language-numbers '(-500 -500 1))
           (language-id "plt:beginning-student")
           (sharing-printing #f)
           (abbreviate-cons-as-list #f)
           (allow-sharing? #f)
           (accept-quasiquote? #f)
           (reader-module '(lib "htdp-beginner-reader.ss" "lang"))
	   (stepper:supported #t)
           (stepper:enable-let-lifting #t)
	   (stepper:show-lambdas-as-lambdas #f))))))
