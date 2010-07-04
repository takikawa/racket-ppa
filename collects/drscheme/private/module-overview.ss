
(module module-overview mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
	   (lib "list.ss")
           (lib "string.ss")
           (lib "moddep.ss" "syntax")
           (lib "toplevel.ss" "syntax")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           (lib "graph.ss" "mrlib")
           "drsig.ss"
           (lib "unitsig.ss")
           (lib "unit.ss")
           (lib "async-channel.ss"))
  
  (define-struct req (filename key))
  ;; type req = (make-req string[filename] (union symbol #f))
      
  (provide module-overview@
           process-program-unit
           (struct req (filename key)))

  (define adding-file (string-constant module-browser-adding-file))
  (define unknown-module-name "? unknown module name")
      
  (define module-overview@
    (unit/sig drscheme:module-overview^
      (import [drscheme:frame : drscheme:frame^]
              [drscheme:eval : drscheme:eval^]
              [drscheme:language-configuration : drscheme:language-configuration/internal^]
              [drscheme:language : drscheme:language^])
      
      (define filename-constant (string-constant module-browser-filename-format))
      (define font-size-gauge-label (string-constant module-browser-font-size-gauge-label))
      (define progress-label (string-constant module-browser-progress-label))
      (define laying-out-graph-label (string-constant module-browser-laying-out-graph-label))
      (define open-file-format (string-constant module-browser-open-file-format))
      (define lib-paths-checkbox-constant (string-constant module-browser-show-lib-paths))
      
      (preferences:set-default 'drscheme:module-overview:label-font-size 12 number?)
      (preferences:set-default 'drscheme:module-overview:window-height 500 number?)
      (preferences:set-default 'drscheme:module-overview:window-width 500 number?)
      (preferences:set-default 'drscheme:module-browser:hide-paths '(lib)
                               (λ (x)
                                 (and (list? x)
                                      (andmap symbol? x))))
      
      (define (set-box/f b v) (when (box? b) (set-box! b v)))
      
      (define (module-overview parent)
        (let ([filename (get-file #f parent)])
          (when filename
            (module-overview/file filename parent))))

      (define (find-label-font size)
        (send the-font-list find-or-create-font size 'decorative 'normal 'normal #f))
      
      (define module-overview-pasteboard<%>
        (interface ()
          set-label-font-size
          get-label-font-size
          get-hidden-paths
          show-visible-paths
          remove-visible-paths
          set-name-length
          get-name-length))
      
      (define boxed-word-snip<%>
        (interface ()
          get-filename
          get-word
          get-lines
          is-special-key-child?
          add-special-key-child))
      
      ;; make-module-overview-pasteboard : boolean
      ;;                                   ((union #f snip) -> void)
      ;;                                -> (union string pasteboard)
      ;; string as result indicates an error message
      ;; pasteboard as result is the pasteboard to show
      (define (make-module-overview-pasteboard vertical? mouse-currently-over)
        
        (define level-ht (make-hash-table))

        ;; snip-table : hash-table[sym -o> snip]
        (define snip-table (make-hash-table 'equal))
        (define label-font (find-label-font (preferences:get 'drscheme:module-overview:label-font-size)))
        (define text-color (make-object color% "blue"))
        
        (define dark-syntax-pen (send the-pen-list find-or-create-pen "darkorchid" 1 'solid))
        (define dark-syntax-brush (send the-brush-list find-or-create-brush "darkorchid" 'solid))
        (define light-syntax-pen (send the-pen-list find-or-create-pen "plum" 1 'solid))
        (define light-syntax-brush (send the-brush-list find-or-create-brush "plum" 'solid))

        (define dark-template-pen (send the-pen-list find-or-create-pen "seagreen" 1 'solid))
        (define dark-template-brush (send the-brush-list find-or-create-brush "seagreen" 'solid))
        (define light-template-pen (send the-pen-list find-or-create-pen "springgreen" 1 'solid))
        (define light-template-brush (send the-brush-list find-or-create-brush "springgreen" 'solid))
        
        (define dark-pen (send the-pen-list find-or-create-pen "blue" 1 'solid))
        (define dark-brush (send the-brush-list find-or-create-brush "blue" 'solid))
        (define light-pen (send the-pen-list find-or-create-pen "light blue" 1 'solid))
        (define light-brush (send the-brush-list find-or-create-brush "light blue" 'solid))
        
        (define (module-overview-pasteboard-mixin %)
          (class* % (module-overview-pasteboard<%>)
            
            (inherit get-snip-location
                     begin-edit-sequence
                     end-edit-sequence
                     insert
                     move-to
                     find-first-snip
                     dc-location-to-editor-location
                     find-snip
                     get-canvas)
          
            (define name-length 'long)
            (define/public (set-name-length nl)
              (unless (eq? name-length nl)
                (set! name-length nl)
                (re-add-snips)
                (render-snips)))
            (define/public (get-name-length) name-length)
            
            (field [max-lines #f])
            
            ;; controls if the snips should be moved
            ;; around when the font size is changed.
            ;; set to #f if the user ever moves a
            ;; snip themselves.
            (define dont-move-snips #f)
            
            (field (label-font-size (preferences:get 'drscheme:module-overview:label-font-size)))
            (define/public (get-label-font-size) label-font-size)
            (define/private (get-snip-hspace) (if vertical?
                                                  2
                                                  (* 2 label-font-size)))
            (define/private (get-snip-vspace) (if vertical?
                                                  30
                                                  2))
            (define snip-height #f)
            
            (define font-label-size-callback-running? #f)
            (define new-font-size #f)
            (define/public (set-label-font-size size-to-set)
              (set! new-font-size size-to-set)
              (unless font-label-size-callback-running?
                (set! font-label-size-callback-running? #t)
                (queue-callback
                 (λ ()
                   (set! label-font-size new-font-size)
                   (preferences:set 'drscheme:module-overview:label-font-size 
                                    new-font-size)
                   (set! label-font (find-label-font label-font-size))
                   (begin-edit-sequence)
                   (let loop ([snip (find-first-snip)])
                     (when snip
                       (let ([admin (send snip get-admin)])
                         (when admin
                           (send admin resized snip #t)))
                       (loop (send snip next))))
                   (unless dont-move-snips
                     (render-snips))
                   (end-edit-sequence)
                   (set! new-font-size #f)
                   (set! font-label-size-callback-running? #f))
                 #f)))
            
            (define/public (begin-adding-connections)
              (when max-lines
                (error 'begin-adding-connections "already in begin-adding-connections/end-adding-connections sequence"))
              (set! max-lines 0)
              (begin-edit-sequence)
              (let loop ()
                (let ([s (find-first-snip)])
                  (when s
                    (send s release-from-owner)
                    (loop))))
              (set! level-ht (make-hash-table))
              (set! snip-table (make-hash-table 'equal)))

            (define/public (end-adding-connections)
              (unless max-lines
                (error 'end-adding-connections "not in begin-adding-connections/end-adding-connections sequence"))
              
              (unless (zero? max-lines)
                (let loop ([snip (find-first-snip)])
                  (when snip
                    (when (is-a? snip word-snip/lines%)
                      (send snip normalize-lines max-lines))
                    (loop (send snip next)))))
              
              
              (set! max-lines #f)
              
              (remove-specially-linked)
              (render-snips)
              (end-edit-sequence))
              
            ;; add-connection : string string boolean symbol -> void
            ;; name-original and name-require and the identifiers for those paths and
            ;; original-filename? and require-filename? are booleans indicating if the names
            ;; are filenames.
            (define/public (add-connection name-original name-require path-key require-type)
              (unless max-lines
                (error 'add-connection "not in begin-adding-connections/end-adding-connections sequence"))
              (let* ([original-filename? (file-exists? name-original)]
                     [require-filename? (file-exists? name-require)]
                     [original-snip (find/create-snip name-original original-filename?)]
                     [require-snip (find/create-snip name-require require-filename?)]
                     [original-level (send original-snip get-level)]
                     [require-level (send require-snip get-level)])
                (case require-type 
                  [(require)
                   (add-links original-snip require-snip
                              dark-pen light-pen
                              dark-brush light-brush)]
                  [(require-for-syntax)
                   (add-links original-snip require-snip 
                              dark-syntax-pen light-syntax-pen
                              dark-syntax-brush light-syntax-brush)]
                  [(require-for-template)
                   (add-links original-snip require-snip
                              dark-template-pen light-template-pen
                              dark-template-brush light-template-brush)]
                  [else (error 'add-connection "unknown require-type ~s" require-type)])
                (when path-key
                  (send original-snip add-special-key-child path-key require-snip))
                (if (send original-snip get-level)
                    (fix-snip-level require-snip (+ original-level 1))
                    (fix-snip-level original-snip 0))))
            
            ;; fix-snip-level : snip number -> void
            ;; moves the snip (and any children) to at least `new-level'
            ;; doesn't move them if they are already past that level
            (define/private (fix-snip-level snip new-min-level)
              (let loop ([snip snip]
                         [new-min-level new-min-level])
                (let ([current-level (send snip get-level)])
                  (when (or (not current-level)
                            (new-min-level . > . current-level))
                    (send snip set-level new-min-level)
                    (for-each
                     (λ (child) (loop child (+ new-min-level 1)))
                     (send snip get-children))))))
            
            ;; find/create-snip : (union path string) boolean? -> word-snip/lines
            ;; finds the snip with this key, or creates a new
            ;; ones. For the same key, always returns the same snip.
            ;; uses snip-table as a cache for this purpose.
            (define/private (find/create-snip name is-filename?)
              (hash-table-get
               snip-table
               name
               (λ () 
                 (let* ([snip (instantiate word-snip/lines% ()
                                (lines (if is-filename? (count-lines name) #f))
                                (word (let-values ([(_1 name _2) (split-path name)])
                                        (path->string name)))
                                (pb this)
                                (filename (if is-filename? name #f)))])
                   (insert snip)
                   (hash-table-put! snip-table name snip)
                   snip))))
            
            ;; count-lines : string[filename] -> (union #f number)
            ;; effect: updates max-lines
            (define/private (count-lines filename)
              (let ([lines
                     (call-with-input-file filename
                       (λ (port)
                         (let loop ([n 0])
                           (let ([l (read-line port)])
                             (if (eof-object? l)
                                 n
                                 (loop (+ n 1))))))
                       'text)])
                (set! max-lines (max lines max-lines))
                lines))
            
            ;; get-snip-width : snip -> number
            ;; exracts the width of a snip
            (define/private (get-snip-width snip)
              (let ([lb (box 0)]
                    [rb (box 0)])
                (get-snip-location snip lb #f #f)
                (get-snip-location snip rb #f #t)
                (- (unbox rb)
                   (unbox lb))))
            
            ;; get-snip-height : snip -> number
            ;; exracts the width of a snip
            (define/private (get-snip-height snip)
              (let ([tb (box 0)]
                    [bb (box 0)])
                (get-snip-location snip #f tb #f)
                (get-snip-location snip #f bb #t)
                (- (unbox bb)
                   (unbox tb))))
            
            (field [hidden-paths (preferences:get 'drscheme:module-browser:hide-paths)])
            (define/public (remove-visible-paths symbol)
              (unless (memq symbol hidden-paths)
                (set! hidden-paths (cons symbol hidden-paths))
                (refresh-visible-paths)))
            (define/public (show-visible-paths symbol)
              (when (memq symbol hidden-paths)
                (set! hidden-paths (remq symbol hidden-paths))
                (refresh-visible-paths)))
            (define/public (get-hidden-paths) hidden-paths)
            
            (define/private (refresh-visible-paths)
              (begin-edit-sequence)
              (re-add-snips)
              (render-snips)
              (end-edit-sequence))
            
            (define/private (re-add-snips)
              (begin-edit-sequence)
              (remove-specially-linked)
              (end-edit-sequence))
            
            (define/private (remove-specially-linked)
              (remove-currrently-inserted)
              (cond
                [(null? hidden-paths)
                 (add-all)]
                [else
                 (let ([ht (make-hash-table)])
                   (for-each
                    (λ (snip)
                      (insert snip)
                      (let loop ([snip snip])
                        (unless (hash-table-get ht snip (λ () #f))
                          (hash-table-put! ht snip #t)
                          (for-each
                           (λ (child)
                             (unless (ormap (λ (key) (send snip is-special-key-child? key child))
                                            hidden-paths)
                               (insert child)
                               (loop child)))
                           (send snip get-children)))))
                    (get-top-most-snips)))]))
                
            (define/private (remove-currrently-inserted)
              (let loop ()
                (let ([snip (find-first-snip)])
                  (when snip
                    (send snip release-from-owner)
                    (loop)))))
            
            (define/private (add-all)
              (let ([ht (make-hash-table)])
                (for-each
                 (λ (snip)
                   (let loop ([snip snip])
                     (unless (hash-table-get ht snip (λ () #f))
                       (hash-table-put! ht snip #t)
                       (insert snip)
                       (for-each loop (send snip get-children)))))
                 (get-top-most-snips))))

            (define/private (get-top-most-snips) (hash-table-get level-ht 0 (λ () null)))
              
            ;; render-snips : -> void
            (define/public (render-snips)
              (begin-edit-sequence)
              (let ([max-minor 0])
                
                ;; major-dim is the dimension that new levels extend along
                ;; minor-dim is the dimension that snips inside a level extend along
                
                (hash-table-for-each
                 level-ht
                 (λ (n v)
                   (set! max-minor (max max-minor (apply + (map (if vertical?
                                                                    (λ (x) (get-snip-width x))
                                                                    (λ (x) (get-snip-height x)))
                                                                v))))))
                
                (let ([levels (sort (hash-table-map level-ht list)
                                    (λ (x y) (<= (car x) (car y))))])
                  (let loop ([levels levels]
                             [major-dim 0])
                    (cond
                      [(null? levels) (void)]
                      [else
                       (let* ([level (car levels)]
                              [n (car level)]
                              [this-level-snips (cadr level)]
                              [this-minor (apply + (map (if vertical? 
                                                            (λ (x) (get-snip-width x))
                                                            (λ (x) (get-snip-height x)))
                                                        this-level-snips))]
                              [this-major (apply max (map (if vertical? 
                                                              (λ (x) (get-snip-height x))
                                                              (λ (x) (get-snip-width x)))
                                                          this-level-snips))])
                         (let loop ([snips this-level-snips]
                                    [minor-dim (/ (- max-minor this-minor) 2)])
                           (unless (null? snips)
                             (let* ([snip (car snips)]
                                    [new-major-coord
                                     (+ major-dim
                                        (floor
                                         (- (/ this-major 2) 
                                            (/ (if vertical? 
                                                   (get-snip-height snip)
                                                   (get-snip-width snip))
                                               2))))])
                               (if vertical?
                                   (move-to snip minor-dim new-major-coord)
                                   (move-to snip new-major-coord minor-dim))
                               (loop (cdr snips)
                                     (+ minor-dim
                                        (if vertical?
                                            (get-snip-hspace)
                                            (get-snip-vspace))
                                        (if vertical?
                                            (get-snip-width snip)
                                            (get-snip-height snip)))))))
                         (loop (cdr levels)
                               (+ major-dim 
                                  (if vertical? 
                                      (get-snip-vspace)
                                      (get-snip-hspace))
                                  this-major)))]))))
              (end-edit-sequence))
            
            (define/override (on-mouse-over-snips snips)
              (mouse-currently-over snips))
            
            (define/override (on-double-click snip event)
              (cond
                [(is-a? snip boxed-word-snip<%>) 
                 (let ([fn (send snip get-filename)])
                   (when fn
                     (handler:edit-file fn)))]
                [else (super on-double-click snip event)]))
              
            (define/override (on-event evt)
              (cond
                [(send evt button-down? 'right)
                 (let ([ex (send evt get-x)]
                       [ey (send evt get-y)])
                   (let-values ([(x y) (dc-location-to-editor-location ex ey)])
                     (let ([snip (find-snip x y)]
                           [canvas (get-canvas)])
                       (let ([right-button-menu (make-object popup-menu%)])
                         (when (and snip
                                    (is-a? snip boxed-word-snip<%>)
                                    canvas
                                    (send snip get-filename))
                           (instantiate menu-item% ()
                             (label 
                              (trim-string
                               (format open-file-format
                                       (path->string (send snip get-filename)))
                               200))
                             (parent right-button-menu)
                             (callback
                              (λ (x y)
                                (handler:edit-file
                                 (send snip get-filename))))))
                         (instantiate menu-item% ()
                           (label (string-constant module-browser-open-all))
                           (parent right-button-menu)
                           (callback
                            (λ (x y)
                              (let loop ([snip (find-first-snip)])
                                (when snip
                                  (when (is-a? snip boxed-word-snip<%>)
                                    (let ([filename (send snip get-filename)])
                                      (handler:edit-file filename)))
                                  (loop (send snip next)))))))
                         (send canvas popup-menu
                               right-button-menu
                               (+ (send evt get-x) 1)
                               (+ (send evt get-y) 1))))))]
                [else (super on-event evt)]))
            
            (super-new)))
        
        (define (trim-string str len)
          (cond
            [(<= (string-length str) len) str]
            [else (substring str (- (string-length str) len) (string-length str))]))
        
        (define (level-mixin %)
          (class %
            (field (level #f))
            (define/public (get-level) level)
            (define/public (set-level _l) 
              (when level
                (hash-table-put! level-ht level
                                 (remq this (hash-table-get level-ht level))))
              (set! level _l)
              (hash-table-put! level-ht level 
                               (cons this (hash-table-get level-ht level (λ () null)))))
            
            (super-instantiate ())))
        
        (define (boxed-word-snip-mixin %)
          (class* % (boxed-word-snip<%>)
            (init-field word
                        filename
                        lines
                        pb)
            
            (field [special-children (make-hash-table)])
            (define/public (is-special-key-child? key child)
              (let ([ht (hash-table-get special-children key (λ () #f))])
                (and ht
                     (hash-table-get ht child (λ () #f)))))
            (define/public (add-special-key-child key child)
              (let ([ht (hash-table-get special-children key (λ () #f))])
                (unless ht
                  (set! ht (make-hash-table))
                  (hash-table-put! special-children key ht))
                (hash-table-put! ht child #t)))
            
            (define/public (get-filename) filename)
            (define/public (get-word) word)
            (define/public (get-lines) lines)
            
            (field (lines-brush #f))
            (define/public (normalize-lines n)
              (if lines
                  (let* ([grey (inexact->exact (floor (- 255 (* 255 (sqrt (/ lines n))))))])
                    (set! lines-brush (send the-brush-list find-or-create-brush
                                            (make-object color% grey grey grey)
                                            'solid)))
                  (set! lines-brush (send the-brush-list find-or-create-brush
                                          "salmon"
                                          'solid))))
            
            (field (snip-width 0)
                   (snip-height 0))
            
            (define/override (get-extent dc x y wb hb descent space lspace rspace)
              (cond
                [(equal? (name->label) "")
                 (set! snip-width 15)
                 (set! snip-height 15)]
                [else
                 (let-values ([(w h a d) (send dc get-text-extent (name->label) label-font)])
                   (set! snip-width (+ w 4))
                   (set! snip-height (+ h 4)))])
              (set-box/f wb snip-width)
              (set-box/f hb snip-height)
              (set-box/f descent 0)
              (set-box/f space 0)
              (set-box/f lspace 0)
              (set-box/f rspace 0))
            
            (define/override (draw dc x y left top right bottom dx dy draw-caret)
              (let ([old-font (send dc get-font)]
                    [old-text-foreground (send dc get-text-foreground)]
                    [old-brush (send dc get-brush)])
                (send dc set-font label-font)
                (when lines-brush
                  (send dc set-brush lines-brush))
                (when (and (or (<= left x right)
                               (<= left (+ x snip-width) right))
                           (or (<= top y bottom)
                               (<= top (+ y snip-height) bottom)))
                  (send dc draw-rectangle x y snip-width snip-height)
                  (send dc set-text-foreground text-color)
                  (send dc draw-text (name->label) (+ x 2) (+ y 2)))
                (send dc set-brush old-brush)
                (send dc set-text-foreground old-text-foreground)
                (send dc set-font old-font)))
                          
            ;; name->label : path -> string
            ;; constructs a label for the little boxes in terms
            ;; of the filename.
            
            (define last-name #f)
            (define last-size #f)
            
            (define/private (name->label)
              (let ([this-size (send pb get-name-length)])
                (cond
                  [(eq? this-size last-size) last-name]
                  [else
                   (set! last-size this-size)
                   (set! last-name
                         (case last-size
                           [(short)
                            (if (string=? word "")
                                ""
                                (string (string-ref word 0)))]
                           [(medium)
                            (let ([m (regexp-match #rx"^(.*)\\.[^.]*$" word)])
                              (let ([short-name (if m (cadr m) word)])
                                (if (string=? short-name "")
                                    ""
                                    (let ([ms (regexp-match* #rx"-[^-]*" short-name)])
                                      (cond
                                        [(null? ms)
                                         (substring short-name 0 (min 2 (string-length short-name)))]
                                        [else
                                         (apply string-append
                                                (cons (substring short-name 0 1)
                                                      (map (λ (x) (substring x 1 2))
                                                           ms)))])))))]
                           [(long) word]))
                   last-name])))

            (super-new)))
        
        (define word-snip/lines% (level-mixin (boxed-word-snip-mixin (graph-snip-mixin snip%))))
        
        (define draw-lines-pasteboard% (module-overview-pasteboard-mixin
                                        (graph-pasteboard-mixin
                                         pasteboard:basic%)))
        (make-object draw-lines-pasteboard%))
      
      
;                                                                
;                                                                
;                                                                
;    ;;;                                      ;;;;   ;     ;  ;  
;   ;                                        ;    ;  ;     ;  ;  
;   ;                                       ;        ;     ;  ;  
;  ;;;;  ; ;  ;;;    ; ;;  ;;     ;;;       ;        ;     ;  ;  
;   ;    ;;  ;   ;   ;;  ;;  ;   ;   ;      ;        ;     ;  ;  
;   ;    ;       ;   ;   ;   ;  ;    ;      ;        ;     ;  ;  
;   ;    ;    ;;;;   ;   ;   ;  ;;;;;;      ;     ;  ;     ;  ;  
;   ;    ;   ;   ;   ;   ;   ;  ;           ;     ;  ;     ;  ;  
;   ;    ;   ;   ;   ;   ;   ;   ;           ;    ;  ;     ;  ;  
;   ;    ;    ;;;;;  ;   ;   ;    ;;;;        ;;;;;   ;;;;;   ;  
;                                                                
;                                                                
;                                                                

      
      (define (module-overview/file filename parent)
        (define progress-frame (parameterize ([current-eventspace (make-eventspace)])
                                 (instantiate frame% ()
                                   (parent parent)
                                   (label progress-label)
                                   (width 600))))
        (define progress-message (instantiate message% ()
                                   (label "")
                                   (stretchable-width #t)
                                   (parent progress-frame)))
        
        (define thd 
          (thread
           (λ ()
             (sleep 3)
             (send progress-frame show #t))))
        
        (define text/pos 
          (let ([t (make-object text%)])
            (send t load-file filename)
            (drscheme:language:make-text/pos
             t
             0
             (send t last-position))))
        
        (define update-label void)
        
        (define (show-status str)
          (send progress-message set-label str))
        
        (define pasteboard (make-module-overview-pasteboard 
                            #f
                            (λ (x) (update-label x))))
        
        (let ([success? (fill-pasteboard pasteboard text/pos show-status void)])
          (kill-thread thd)
          (send progress-frame show #f)
          (when success?
            (let ()
              (define frame (instantiate overview-frame% ()
                              (label (string-constant module-browser))
                              (width (preferences:get 'drscheme:module-overview:window-width))
                              (height (preferences:get 'drscheme:module-overview:window-height))
                              (alignment '(left center))))
              (define vp (instantiate vertical-panel% ()
                           (parent (send frame get-area-container))
                           (alignment '(left center))))
              (define root-message (instantiate message% ()
                                     (label 
                                      (format (string-constant module-browser-root-filename)
                                              filename))
                                     (parent vp)
                                     (stretchable-width #t)))
              (define label-message (instantiate message% ()
                                      (label "")
                                      (parent vp)
                                      (stretchable-width #t)))
              (define font-size-gauge
                (instantiate slider% ()
                  (label font-size-gauge-label)
                  (min-value 1)
                  (max-value 72)
                  (init-value (preferences:get 'drscheme:module-overview:label-font-size))
                  (parent vp)
                  (callback
                   (λ (x y)
                     (send pasteboard set-label-font-size (send font-size-gauge get-value))))))
              (define lib-paths-checkbox
                (instantiate check-box% ()
                  (label lib-paths-checkbox-constant)
                  (parent vp)
                  (callback
                   (λ (x y)
                     (if (send lib-paths-checkbox get-value)
                         (send pasteboard show-visible-paths 'lib)
                         (send pasteboard remove-visible-paths 'lib))))))
              
              (define ec (make-object canvas:basic% vp pasteboard))
              
              (send lib-paths-checkbox set-value (not (memq 'lib (preferences:get 'drscheme:module-browser:hide-paths))))
              (set! update-label
                    (λ (s)
                      (if (and s (not (null? s)))
                          (let* ([currently-over (car s)]
                                 [fn (send currently-over get-filename)]
                                 [lines (send currently-over get-lines)])
                            (when (and fn lines)
                              (send label-message set-label
                                    (format filename-constant fn lines))))
                          (send label-message set-label ""))))
              
              ;; shouldn't be necessary here -- need to find callback on editor
              (send pasteboard render-snips)
              
              (send frame show #t)))))
      
      (define (fill-pasteboard pasteboard text/pos show-status send-user-thread/eventspace)
        
        (define progress-channel (make-async-channel))
        (define connection-channel (make-async-channel))
        
        (define-values/invoke-unit (add-connections) process-program-unit 
          #f
          progress-channel
          connection-channel)
        
        ;; =user thread=
        (define (iter sexp continue)
          (cond
            [(eof-object? sexp) 
             (custodian-shutdown-all user-custodian)]
            [else
             (add-connections sexp)
             (continue)]))
        (define init-complete (make-semaphore 0))
        
        (define user-custodian #f)
        (define user-thread #f)
        (define error-str #f)
        
        (define init-dir
          (let* ([bx (box #f)]
                 [filename (send (drscheme:language:text/pos-text text/pos) get-filename bx)])
            (if (and filename
                     (not (unbox bx)))
                (let-values ([(base name dir) (split-path filename)])
                  base)
                (current-directory))))
        
        (define (init)
          (set! user-custodian (current-custodian))
          (set! user-thread (current-thread))
          (moddep-current-open-input-file
           (λ (filename)
             (let* ([p (open-input-file filename)]
                    [wxme? (regexp-match-peek #rx#"^WXME" p)])
               (if wxme?
                   (let ([t (new text%)])
                     (close-input-port p)
                     (send t load-file filename)
                     (let ([prt (open-input-text-editor t)])
                       (port-count-lines! prt)
                       prt))
                   p))))
          (current-load-relative-directory init-dir)
          (current-directory init-dir)
          (error-display-handler 
           (λ (str exn)
             (set! error-str str)
             (custodian-shutdown-all user-custodian)))
          (semaphore-post init-complete))
        (define (kill-termination) (void))
        (define complete-program? #t)
        
        (define stupid-internal-define-syntax1
          ((drscheme:eval:traverse-program/multiple
            (preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))
            init
            kill-termination)
           text/pos
           iter
           complete-program?))
        
        (semaphore-wait init-complete)
        (send-user-thread/eventspace user-thread user-custodian)
        
        ;; this thread puts a "cap" on the end of the connection-channel
        ;; so that we know when we've gotten to the end.
        ;; this ensures that we can completely flush out the
        ;; connection-channel.
        (thread
         (λ ()
           (sync (thread-dead-evt user-thread))
           (async-channel-put connection-channel 'done)))
        
        (send pasteboard begin-adding-connections)
        (let ([evt
               (choice-evt
                (handle-evt progress-channel (λ (x) (cons 'progress x)))
                (handle-evt connection-channel (λ (x) (cons 'connect x))))])
          (let loop ()
            (let* ([evt-value (yield evt)]
                   [key (car evt-value)]
                   [val (cdr evt-value)])
              (case key
                [(progress) 
                 (show-status val)
                 (loop)]
                [(connect)
                 (unless (eq? val 'done)
                   (let ([name-original (first val)]
                         [name-require (second val)]
                         [path-key (third val)]
                         [require-type (fourth val)])
                     (send pasteboard add-connection name-original name-require path-key require-type))
                   (loop))]))))
        (send pasteboard end-adding-connections)
        
        (custodian-shutdown-all user-custodian)
        
        (cond
          [error-str
           (message-box 
            (string-constant module-browser)
            (format (string-constant module-browser-error-expanding)
                    error-str))
           #f]
          [else
           #t]))
      
      (define overview-frame%
        (class (drscheme:frame:basics-mixin
                frame:standard-menus%)
          (define/override (edit-menu:between-select-all-and-find menu) (void))
          (define/override (edit-menu:between-redo-and-cut menu) (void))
          (define/override (edit-menu:between-find-and-preferences menu) (void))
          
          (define/override (edit-menu:create-cut?) #f)
          (define/override (edit-menu:create-copy?) #f)
          (define/override (edit-menu:create-paste?) #f)
          (define/override (edit-menu:create-clear?) #f)
          (define/override (edit-menu:create-select-all?) #f)
          
          (define/override (on-size w h)
            (preferences:set 'drscheme:module-overview:window-width w)
            (preferences:set 'drscheme:module-overview:window-height h)
            (super on-size w h))
          (super-instantiate ())))))
  
  
        
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;                                                                                    
;   ; ;;    ; ;   ;;;     ;;;    ;;;    ;;;    ;;;       ; ;;    ; ;   ;;;     ;; ;  
;   ;;  ;   ;;   ;   ;   ;   ;  ;   ;  ;      ;          ;;  ;   ;;   ;   ;   ;  ;;  
;   ;    ;  ;   ;     ; ;      ;    ;  ;;     ;;         ;    ;  ;   ;     ; ;    ;  
;   ;    ;  ;   ;     ; ;      ;;;;;;   ;;     ;;        ;    ;  ;   ;     ; ;    ;  
;   ;    ;  ;   ;     ; ;      ;          ;      ;       ;    ;  ;   ;     ; ;    ;  
;   ;;  ;   ;    ;   ;   ;   ;  ;         ;      ;       ;;  ;   ;    ;   ;   ;  ;;  
;   ; ;;    ;     ;;;     ;;;    ;;;;  ;;;    ;;;        ; ;;    ;     ;;;     ;; ;  
;   ;                                                    ;                        ;  
;   ;                                                    ;                   ;    ;  
;   ;                                                    ;                    ;;;;   


  (define process-program-unit
    (unit 
      (import progress-channel
              connection-channel)
      (export add-connections)
      
      (define visited-hash-table (make-hash-table 'equal))
      
      ;; add-connections : (union syntax string[filename]) -> (union #f string)
      ;; recursively adds a connections from this file and
      ;; all files it requires
      ;; returns a string error message if there was an error compiling
      ;; the program
      (define (add-connections filename/stx)
        (cond
          [(string? filename/stx)
           (add-filename-connections filename/stx)]
          [(syntax? filename/stx)
           (add-syntax-connections filename/stx)]))
      
      ;; add-syntax-connections : syntax -> void
      (define (add-syntax-connections stx)
        (let ([module-codes (map compile (expand-syntax-top-level-with-compile-time-evals/flatten stx))])
          (for-each
           (λ (module-code)
             (when (compiled-module-expression? module-code)
               (let* ([name (extract-module-name stx)]
                      [base 
                       (build-module-filename
                        (if (regexp-match #rx"^," name)
                            (substring name 1 (string-length name))
                            (build-path (current-load-relative-directory) name)))])
                 (add-module-code-connections base module-code))))
           module-codes)))
      
      (define (build-module-filename str)
        (let ([try (λ (ext)
                     (let ([tst (bytes->path (bytes-append (path->bytes str) ext))])
                       (and (file-exists? tst)
                            tst)))])
          (or (try #".ss")
              (try #".scm")
              (try #"")
              str)))
      
      ;; add-filename-connections : string -> void
      (define (add-filename-connections filename)
        (add-module-code-connections filename (get-module-code filename)))
      
      (define (add-module-code-connections module-name module-code)
        (unless (hash-table-get visited-hash-table module-name (λ () #f))
          (async-channel-put progress-channel (format adding-file module-name))
          (hash-table-put! visited-hash-table module-name #t)
          (let-values ([(imports fs-imports ft-imports) (module-compiled-imports module-code)])
            (let ([requires (extract-filenames imports module-name)]
                  [syntax-requires (extract-filenames fs-imports module-name)]
                  [template-requires (extract-filenames ft-imports module-name)])
              (for-each (λ (require)
                          (add-connection module-name
                                          (req-filename require)
                                          (req-key require)
                                          'require)
                          (add-filename-connections (req-filename require)))
                        requires)
              (for-each (λ (syntax-require)
                          (add-connection module-name 
                                          (req-filename syntax-require)
                                          (req-key syntax-require)
                                          'require-for-syntax)
                          (add-filename-connections (req-filename syntax-require)))
                        syntax-requires)
              (for-each (λ (require)
                          (add-connection module-name
                                          (req-filename require)
                                          (req-key require)
                                          'require-for-template)
                          (add-filename-connections (req-filename require)))
                        template-requires)))))
      
      ;; add-connection : string string boolean symbol -> void
      ;; name-original and name-require and the identifiers for those paths and
      ;; original-filename? and require-filename? are booleans indicating if the names
      ;; are filenames.
      (define (add-connection name-original name-require req-sym require-type)
        (async-channel-put connection-channel (list name-original 
                                                    name-require  
                                                    req-sym
                                                    require-type)))
      
      (define (extract-module-name stx)
        (syntax-case stx ()
          [(module m-name rest ...)
           (and (eq? (syntax-e (syntax module)) 'module)
                (identifier? (syntax m-name)))
           (format "~a" (syntax-object->datum (syntax m-name)))]
          [else unknown-module-name]))
      
      ;; extract-filenames : (listof (union symbol module-path-index)) string[module-name] ->
      ;;                     (listof req)
      (define (extract-filenames direct-requires base)
        (let loop ([direct-requires direct-requires])
          (cond
            [(null? direct-requires) null]
            [else (let ([dr (car direct-requires)])
                    (if (module-path-index? dr)
                        (begin
                          ;(printf ">> ~s ~s\n" base (collapse-module-path-index dr base))
                          (cons (make-req (simplify-path (expand-path (resolve-module-path-index dr base)))
                                          (get-key dr))
                                (loop (cdr direct-requires))))
                        (loop (cdr direct-requires))))])))
      
      (define (get-key dr)
        (and (module-path-index? dr)
             (let-values ([(a b) (module-path-index-split dr)])
               (and (pair? a)
                    (symbol? (car a))
                    (car a))))))))
