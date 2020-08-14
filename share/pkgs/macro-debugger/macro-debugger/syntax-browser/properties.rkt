#lang racket/base
(require (only-in racket/list [range l:range])
         racket/class
         racket/match
         racket/pretty
         racket/gui/base
         framework
         mrlib/interactive-value-port
         racket/class/iop
         macro-debugger/syntax-browser/interfaces
         macro-debugger/model/stx-util
         "hrule-snip.rkt"
         "util.rkt"
         macro-debugger/util/mpi)
(provide properties-view%
         properties-snip%)

(define color-text-default-style-name
  "macro-debugger/syntax-browser/properties color-text% basic")

(define color-text%
  (class (editor:standard-style-list-mixin text:basic%)
    (inherit get-style-list)
    (define/override (default-style-name)
      color-text-default-style-name)
    (super-new)
    (let* ([sl (get-style-list)]
           [standard
            (send sl find-named-style (editor:get-default-color-style-name))]
           [basic
            (send sl find-or-create-style standard
                  (make-object style-delta% 'change-family 'default))])
      (send sl new-named-style color-text-default-style-name basic))))

;; properties-view-base-mixin
(define properties-view-base-mixin
  (mixin () ()
    ;; controller : controller<%>
    (init-field controller)

    ;; selected-syntax : syntax
    (field (selected-syntax #f))

    ;; text : text%
    (field (text (new color-text%)))
    (field (pdisplayer (new properties-displayer% (text text) (view this) (controller controller))))

    (send/i controller selection-manager<%> listen-selected-syntax
            (lambda (stx)
              (set! selected-syntax stx)
              (refresh)))
    (super-new)

    ;; refresh : -> void
    (define/public (refresh)
      (with-unlock text
        (send text erase)
        (if (syntax? selected-syntax)
            (refresh/mode #t)
            (refresh/mode #f)))
      (send text scroll-to-position 0))

    ;; refresh/mode : symbol -> void
    (define/public (refresh/mode mode)
      (case mode
        ((#t) (send pdisplayer display-info selected-syntax))
        ((#f) (send pdisplayer display-null-info))
        (else (error 'properties-view-base:refresh
                     "internal error: no such mode: ~s" mode))))

    (send text set-styles-sticky #f)
    (send text auto-wrap #t)
    (send text hide-caret #t)
    (send text lock #t)
    (refresh)))


;; properties-snip%
(define properties-snip%
  (class (properties-view-base-mixin editor-snip%)
    (inherit-field text)
    (inherit-field pdisplayer)

    (define/private outer:insert
      (case-lambda
       [(obj)
        (outer:insert obj style:normal)]
       [(text style)
        (outer:insert text style #f)]
       [(text style clickback)
        (let ([start (send outer-text last-position)])
          (send outer-text insert text)
          (let ([end (send outer-text last-position)])
            (send outer-text change-style style start end #f)
            (when clickback
                  (send outer-text set-clickback start end clickback))))]))

    (define outer-text (new text%))
    (super-new (editor outer-text))
    (outer:insert "Information\n")
    (outer:insert (new editor-snip% (editor text)))
    (send outer-text hide-caret #t)
    (send outer-text lock #t)))

;; properties-view%
(define properties-view%
  (class* (properties-view-base-mixin object%) ()
    (init parent)
    (inherit-field text)
    (inherit-field pdisplayer)
    (super-new)

    (define ecanvas (new canvas:color% (editor text) (parent parent)))))

;; properties-displayer%
(define properties-displayer%
  (class* object% ()
    (init-field text
                view
                controller)

    (define/private (refresh-view) (send view refresh))

    ;; Mapping of keys to toggle boxes (per stepper view, not persistent)
    (define toggles (make-hash))
    (define (toggle key [default #f])
      (hash-ref! toggles key (lambda () (box default))))
    (define (toggled? key)
      (cond [(hash-ref toggles key #f) => unbox] [else #f]))

    ;; ----

    ;; display-null-info : -> Void
    (define/public (display-null-info)
      (display "No syntax selected\n" n/a-sd))

    ;; display-info : Syntax -> Void
    (define/public (display-info stx)
      (display-meaning-info stx)
      (display (new hrule-snip%))
      (display "\n")
      (display-stxobj-info stx))

    ;; display-meaning-info : syntax -> void
    (define/public (display-meaning-info stx)
      (when (identifier? stx)
        (define sym (syntax-e stx))
        (cond [(symbol-interned? sym) (void)]
              [(symbol-unreadable? sym)
               (display "The identifier contains an unreadable symbol.\n\n")]
              [else
               (display "The identifier contains an uninterned symbol.\n\n")]))
      ;; Binding info
      (display (list (toggle 'binding #t) "Apparent identifier binding\n") key-sd)
      (when (toggled? 'binding)
        (display-bindings stx))
      ;; Indirect binding info
      (display (list (toggle '#%top) "Binding if used for #%top\n") key-sd)
      (when (toggled? '#%top)
        (display-bindings (datum->syntax stx '#%top)))
      (display (list (toggle '#%app) "Binding if used for #%app\n") key-sd)
      (when (toggled? '#%app)
        (display-bindings (datum->syntax stx '#%app)))
      (display (list (toggle '#%datum) "Binding if used for #%datum\n") key-sd)
      (when (toggled? '#%datum)
        (display-bindings (datum->syntax stx '#%datum))))

    ;; display-bindings : Syntax -> Void
    (define/private (display-bindings stx)
      (define phases-to-search '(0 1 -1 #f 2 3 4 5 -2 -3 -4 -5))
      (unless (identifier? stx)
        (display "Not applicable\n\n" n/a-sd))
      (when (identifier? stx)
        (let ([bindings (for/hash ([phase (in-list phases-to-search)])
                          (values phase (identifier-binding stx phase #t)))])
          (cond [(for/or ([(p b) (in-hash bindings)]) b)
                 (for ([phase (in-list phases-to-search)])
                   (cond [(hash-ref bindings phase #f)
                          => (lambda (b) (display-binding-kvs phase b stx))]
                         [else (void)]))]
                [else (display "none\n" #f)]))
        (display "\n" #f)))

    ;; display-binding-kvs : phase bindinginfo identifier -> void
    (define/private (display-binding-kvs phase v stx)
      (define (phase-label)
        (case phase
          [(1) " (transformer phase)"]
          [(-1) " (template phase)"]
          [(#f) " (label phase)"]
          [else ""]))
      (display (format "in phase ~a~a: " phase (phase-label)) sub-key-sd)
      (match v
        [(list* def-mpi def-sym imp-mpi imp-sym defined-at-phase _)
         (display "\n" #f)
         (display-subkv/mpi "  defined in" def-mpi)
         (unless (eq? def-sym (syntax-e stx))
           (display-subkv "    as" def-sym))
         (display-subkv/mpi "  imported from" imp-mpi)
         (unless (eq? imp-sym (syntax-e stx))
           (display-subkv "    provided as" (list-ref v 3)))
         (unless (zero? defined-at-phase)
           (display-subkv "  defined at phase" defined-at-phase))]
        ['lexical
         (display "lexical\n")]
        [(list top-level-sym)
         (display "at top-level\n")
         (display-subkv "  as" top-level-sym)]
        [_ (display "\n")]))

    ;; display-stxobj-info : syntax -> void
    (define/public (display-stxobj-info stx)
      (display-source-info stx)
      (display "\n")
      (display-scopes stx)
      (display "\n")
      (display-builtin-properties stx)
      (display "\n")
      (display-symbol-property-info stx)
      (display "\n")
      (display-artificial stx))

    ;; display-source-info : syntax -> void
    (define/private (display-source-info stx)
      (define s-source (syntax-source stx))
      (define s-line (syntax-line stx))
      (define s-column (syntax-column stx))
      (define s-position (syntax-position stx))
      (define s-span (syntax-span stx))
      (define s-span-known? (not (memv s-span '(0 #f))))
      (display (list (toggle 'srcloc #t) "Source location\n") key-sd)
      (when (toggled? 'srcloc)
        (if (or s-source s-line s-column s-position s-span-known?)
            (begin
              (display-subkv "source" (prettify-source s-source))
              (display-subkv "line" s-line)
              (display-subkv "column" s-column)
              (display-subkv "position" s-position)
              (display-subkv "span" s-span))
            (display "No source location available\n" n/a-sd))))

    ;; display-builtin-properties : syntax -> void
    (define/private (display-builtin-properties stx)
      (define (syntax-armed? stx)
        (syntax-tainted? (datum->syntax stx 'dummy)))
      (display (list (toggle 'props #t) "Built-in properties\n") key-sd)
      (when (toggled? 'props)
        (display-subkv "tamper status"
                       (cond [(syntax-tainted? stx) "tainted (💥)"]
                             [(syntax-armed? stx) "armed (🔒)"]
                             [else "unarmed"]))
        (display-subkv/mpi "source module" (syntax-source-module stx))
        (display-subkv "original?" (syntax-original? stx))))

    ;; display-symbol-property-info : syntax -> void
    (define/private (display-symbol-property-info stx)
      (display (list (toggle 'more-props #t) "Additional properties\n") key-sd)
      (when (toggled? 'more-props)
        (define keys (syntax-property-symbol-keys stx))
        (when (null? keys)
          (display "No additional properties available.\n" n/a-sd))
        (when (pair? keys)
          (for ([k (in-list keys)]) (display-subkv/value k (syntax-property stx k))))))

    (define marks-phase 0)

    ;; display-scopes : syntax -> void
    (define/private (display-scopes stx)
      (display (list (toggle 'scopes #t) "Scopes\n") key-sd)
      (when (toggled? 'scopes)
        (for ([phase (append (l:range (add1 marks-phase))
                             (reverse (l:range (- marks-phase) 0)))])
          (define info (syntax-debug-info stx phase))
          (define ctx (hash-ref info 'context null))
          (when (pair? ctx)
            (display (format "scopes at phase ~s:\n" phase) sub-key-sd)
            (for ([scope (in-list ctx)])
              (display (format "~s\n" scope) #f))
            (display "\n" #f)))
        (display "Show scopes at ")
        (display "more phases"
                 link-sd
                 (lambda _
                   (set! marks-phase (add1 marks-phase))
                   (refresh-view)))
        (when (positive? marks-phase)
          (display " | ")
          (display "fewer phases"
                   link-sd
                   (lambda _
                     (set! marks-phase (max 0 (sub1 marks-phase)))
                     (refresh-view))))
        (display ".\n")))

    ;; display-artificial : syntax -> void
    (define/private (display-artificial stx)
      (when (syntax-artificial? stx)
        (display "This syntax is artificial.\n\n" n/a-sd)))

    ;; display-kv : any any -> void
    (define/private (display-kv key value)
      (display (format "~a\n" key) key-sd)
      (display (format "~s\n\n" value) #f))

    ;; display-subkv : any any -> void
    (define/public (display-subkv k v)
      (display (format "~a: " k) sub-key-sd)
      (display (format "~a\n" v) #f))

    ;; FIXME: add option to show mpi path instead of collapsed?
    (define (display-subkv/mpi key mpi)
      (display-subkv key (collapse-mpi->string mpi)))

    (define/public (display-subkv/value k v)
      (display-subkv k (format "~v" v))
      #;
      (begin
        (display (format "~a:\n" k) sub-key-sd)
        (let* ([value-text (new text:standard-style-list% (auto-wrap #t))]
               [value-snip (new editor-snip% (editor value-text))]
               [value-port
                (open-output-text-editor value-text)
                #; (make-text-port value-text)])
          (parameterize ((pretty-print-size-hook
                          (lambda (v mode p) (if (syntax? v) 1 #f)))
                         (pretty-print-print-hook
                          (lambda (v mode p)
                            (if (syntax? v)
                                (write-special (make-syntax-snip v) p)
                                (write v p))))
                         (pretty-print-columns 'infinity))
            (pretty-print v value-port))
          (send value-text lock #t)
          (send text insert value-snip)
          (send text insert "\n")
          #;(send ecanvas add-wide-snip value-snip))))

    ;; display : (U String ...) StyleDelta -> Void
    (define/private (display item [sd #f] [clickback #f])
      (define p0 (send text last-position))
      (let loop ([item item])
        (cond [(list? item) (for-each loop item)]
              [(box? item)
               (display (if (unbox item) "▽ " "▶ ")
                        #f
                        (lambda _ (set-box! item (not (unbox item))) (refresh-view)))]
              [else (send text insert item)]))
      (define p1 (send text last-position))
      (when sd (send text change-style sd p0 p1))
      (when clickback (send text set-clickback p0 p1 clickback)))

    (super-new)))

;;(require racket/lazy-require)
;;(lazy-require ["snip-decorated.rkt" (make-syntax-snip)])

(define (uninterned? s)
  (not (eq? s (string->symbol (symbol->string s)))))

(define (prettify-source s)
  (cond [(is-a? s editor<%>)
         'editor]
        [else s]))

;; Styles
  
(define key-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "blue")
    (send sd set-weight-on 'bold)
    sd))

(define sub-key-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "blue")
    sd))

(define link-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "blue")
    (send sd set-underlined-on #t)
    sd))

(define n/a-sd
  (let ([sd (new style-delta%)])
    (send sd set-delta-foreground "gray")
    sd))

(define style:normal (make-object style-delta% 'change-normal))

(define style:hyper
  (let ([s (make-object style-delta% 'change-normal)])
    (send s set-delta 'change-toggle-underline)
    (send s set-delta-foreground "blue")
    s))
