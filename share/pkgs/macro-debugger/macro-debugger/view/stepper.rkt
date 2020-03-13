#lang racket/base
(require racket/class
         racket/match
         racket/gui/base
         racket/class/iop
         "interfaces.rkt"
         "extensions.rkt"
         "hiding-panel.rkt"
         "term-record.rkt"
         "step-display.rkt"
	 (prefix-in sb: macro-debugger/syntax-browser/interfaces)
         macro-debugger/model/deriv
         macro-debugger/model/deriv-util
         "cursor.rkt"
         "../syntax-browser/util.rkt"
         framework/notify
         images/compile-time
         images/gui
         (for-syntax racket/base
                     images/icons/arrow images/icons/control images/logos
                     images/icons/style))
(provide macro-stepper-widget%
         macro-stepper-widget/process-mixin)

;; Compiled-in assets (button icons)

(define navigate-up-icon
  (compiled-bitmap (up-arrow-icon #:color syntax-icon-color #:height (toolbar-icon-height))))
(define navigate-to-start-icon
  (compiled-bitmap (search-backward-icon #:color syntax-icon-color #:height (toolbar-icon-height))))
(define navigate-previous-icon
  (compiled-bitmap (step-back-icon #:color syntax-icon-color #:height (toolbar-icon-height))))
(define navigate-next-icon
  (compiled-bitmap (step-icon #:color syntax-icon-color #:height (toolbar-icon-height))))
(define navigate-to-end-icon
  (compiled-bitmap (search-forward-icon #:color syntax-icon-color #:height (toolbar-icon-height))))
(define navigate-down-icon
  (compiled-bitmap (down-arrow-icon #:color syntax-icon-color #:height (toolbar-icon-height))))

(define small-logo (compiled-bitmap (macro-stepper-logo #:height 32)))
(define large-logo (compiled-bitmap (macro-stepper-logo)))

(define (show-about-dialog parent)
  (define dlg
    (new logo-about-dialog%
         (label "About the Macro Stepper")
         (parent parent)
         (bitmap large-logo)
         (messages '("The Macro Stepper is formalized and proved correct in\n"
                     "\n"
                     "    Ryan Culpepper and Matthias Felleisen\n"
                     "    Debugging Hygienic Macros\n"
                     "    Science of Computer Programming, July 2010\n"))))
  (send dlg show #t))

;; Macro Stepper

;; macro-stepper-widget%
(define macro-stepper-widget%
  (class* object% (widget<%>)
    (init-field parent)
    (init-field config)
    (init-field/i (director director<%>))

    (define frame (send parent get-top-level-window))
    (define eventspace (send frame get-eventspace))

    (define-syntax-rule (with-eventspace . body)
      (parameterize ((current-eventspace eventspace))
        (queue-callback (lambda () . body))))

    ;; Terms

    ;; all-terms : (list-of TermRecord)
    ;; (Reversed)
    (define all-terms null)

    ;; terms : (Cursor-of TermRecord)
    ;; Contains visible terms of all-terms
    (define terms (cursor:new null))

    ;; focused-term : -> TermRecord or #f
    (define (focused-term)
      (cursor:next terms))

    ;; current-step-index : notify of number/#f
    (notify:define-notify current-step-index (new notify:notify-box% (value #f)))

    ;; add-deriv : Deriv [(Listof Event)]-> void
    (define/public (add-deriv d [events #f])
      (let ([trec (new term-record% (stepper this) (raw-deriv d) (events events))])
        (add trec)))

    ;; add-trace : (list-of event) -> void
    (define/public (add-trace events)
      (let ([trec (new term-record% (stepper this) (events events))])
        (add trec)))

    ;; add : TermRecord -> void
    (define/private (add trec)
      (with-eventspace
       (set! all-terms (cons trec all-terms))
       (let ([display-new-term? (cursor:at-end? terms)]
             [invisible? (send/i trec term-record<%> get-deriv-hidden?)])
         (unless invisible?
           (cursor:add-to-end! terms (list trec))
           (trim-navigator)
           (if display-new-term?
               (refresh)
               (update))))))

    ;; remove-current-term : -> void
    (define/public (remove-current-term)
      (when (cursor:has-next? terms)
        (cursor:remove-current! terms)
        (trim-navigator)
        (refresh)))

    ;; show-in-new-frame : -> void
    (define/public (show-in-new-frame)
      (let ([term (focused-term)])
        (when term
          (let ([new-stepper (send/i director director<%> new-stepper '(no-new-traces))])
            (send/i new-stepper widget<%> add-deriv
                    (send/i term term-record<%> get-raw-deriv)
                    (send/i term term-record<%> get-events))
            (void)))))

    ;; duplicate-stepper : -> void
    (define/public (duplicate-stepper)
      (let ([new-stepper (send/i director director<%> new-stepper)])
        (for ([term (cursor->list terms)])
          (send/i new-stepper widget<%> add-deriv
                  (send/i term term-record<%> get-raw-deriv)
                  (send/i term term-record<%> get-events)))))

    (define/public (get-config) config)
    (define/public (get-controller) sbc)
    (define/public (get-view) sbview)
    (define/public (get-step-displayer) step-displayer)
    (define/public (get-macro-hiding-prefs) macro-hiding-prefs)

    (define/public (reset-primary-partition)
      (send/i sbc sb:controller<%> reset-primary-partition)
      (update/preserve-view))

    (define superarea (new vertical-pane% (parent parent)))
    (define area
      (new vertical-panel%
           (parent superarea)
           (enabled #f)))
    (define top-panel
      (new horizontal-panel%
           (parent area)
           (horiz-margin 5)
           (stretchable-height #f)))
    (define supernavigator
      (new horizontal-panel%
           (parent top-panel)
           (stretchable-height #f)
           (alignment '(center center))))
    (define navigator
      (new horizontal-panel%
           (parent supernavigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (alignment '(left center))))
    (define extra-navigator
      (new horizontal-panel%
           (parent supernavigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (alignment '(left center))
           (style '(deleted))))
    
    (define logo-canvas
      (new (class bitmap-canvas%
             (super-new (parent top-panel) (bitmap small-logo))
             (define/override (on-event evt)
               (when (eq? (send evt get-event-type) 'left-up)
                 (show-about-dialog frame))))))
    
    (define/i sbview sb:syntax-browser<%>
      (new stepper-syntax-widget% 
           (parent area)
           (macro-stepper this)))
    (define/i step-displayer step-display<%>
      (new step-display%
           (config config)
           (syntax-widget sbview)))
    (define/i sbc sb:controller<%>
      (send/i sbview sb:syntax-browser<%> get-controller))
    (define control-pane
      (new vertical-panel% (parent area) (stretchable-height #f)))

    (define/i macro-hiding-prefs hiding-prefs<%>
      (new macro-hiding-prefs-widget%
           (parent control-pane)
           (stepper this)
           (config config)))

    (send/i sbc sb:controller<%> listen-selected-syntax
            (lambda (stx) (send/i macro-hiding-prefs hiding-prefs<%> set-syntax stx)))
    (send/i sbc sb:controller<%> listen-primary-partition
            (lambda (_p) (update/preserve-view)))
    (send config listen-pretty-abbrev?
          (lambda (_) (update/preserve-view)))
    (send*/i config config<%>
      (listen-show-hiding-panel?
       (lambda (show?) (show-macro-hiding-panel show?)))
      (listen-split-context?
       (lambda (_) (update/preserve-view)))
      (listen-highlight-foci?
       (lambda (_) (update/preserve-view)))
      (listen-highlight-frontier?
       (lambda (_) (update/preserve-view)))
      (listen-show-rename-steps?
       (lambda (_) (refresh/re-reduce)))
      (listen-one-by-one?
       (lambda (_) (refresh/re-reduce)))
      (listen-extra-navigation?
       (lambda (show?) (show-extra-navigation show?))))
    (send config listen-pretty-styles
          (lambda (_) (update/preserve-view)))

    (define nav:up
      (new button% (label (list navigate-up-icon "Previous term" 'left)) (parent navigator)
           (callback (lambda (b e) (navigate-up)))))
    (define nav:start
      (new button% (label (list navigate-to-start-icon "Start" 'left)) (parent navigator)
           (callback (lambda (b e) (navigate-to-start)))))
    (define nav:previous
      (new button% (label (list navigate-previous-icon "Step" 'left)) (parent navigator)
           (callback (lambda (b e) (navigate-previous)))))
    (define nav:next
      (new button% (label (list navigate-next-icon "Step" 'right)) (parent navigator)
           (callback (lambda (b e) (navigate-next)))))
    (define nav:end
      (new button% (label (list navigate-to-end-icon "End" 'right)) (parent navigator)
           (callback (lambda (b e) (navigate-to-end)))))
    (define nav:down
      (new button% (label (list navigate-down-icon "Next term" 'right)) (parent navigator)
           (callback (lambda (b e) (navigate-down)))))

    (define nav:text
      (new text-field%
           (label "Step#")
           (init-value "00000")
           (parent extra-navigator)
           (stretchable-width #f)
           (stretchable-height #f)
           (callback
            (lambda (b e)
              (when (eq? (send e get-event-type) 'text-field-enter)
                (let* ([value (send b get-value)]
                       [step (string->number value)])
                  (cond [(exact-positive-integer? step)
                         (navigate-to (sub1 step))]
                        [(equal? value "end")
                         (navigate-to-end)])))))))

    (define nav:step-count
      (new message%
           (label "")
           (parent extra-navigator)
           (auto-resize #t)
           (stretchable-width #f)
           (stretchable-height #f)))
    (send nav:text set-value "")

    (listen-current-step-index
     (lambda (n)
       (send nav:text set-value
             (if (number? n) (number->string (add1 n)) ""))))

    (define/private (trim-navigator)
      (if (> (length (cursor->list terms)) 1)
          (send navigator change-children
                (lambda _
                  (list nav:up
                        nav:start
                        nav:previous
                        nav:next
                        nav:end
                        nav:down)))
          (send navigator change-children
                (lambda _
                  (list nav:start
                        nav:previous
                        nav:next
                        nav:end)))))

    (define/public (show-macro-hiding-panel show?)
      (send area change-children
            (lambda (children)
              (if show?
                  (append (remq control-pane children) (list control-pane))
                  (remq control-pane children)))))

    (define/private (show-extra-navigation show?)
      (send supernavigator change-children
            (lambda (children)
              (if show?
                  (list navigator extra-navigator)
                  (list navigator)))))

    ;; Navigation
    (define/public-final (navigate-to-start)
      (send/i (focused-term) term-record<%> navigate-to-start)
      (update/preserve-lines-view))
    (define/public-final (navigate-to-end)
      (send/i (focused-term) term-record<%> navigate-to-end)
      (update/preserve-lines-view))
    (define/public-final (navigate-previous)
      (send/i (focused-term) term-record<%> navigate-previous)
      (update/preserve-lines-view))
    (define/public-final (navigate-next)
      (send/i (focused-term) term-record<%> navigate-next)
      (update/preserve-lines-view))
    (define/public-final (navigate-to n)
      (send/i (focused-term) term-record<%> navigate-to n)
      (update/preserve-lines-view))

    (define/public-final (navigate-up)
      (when (focused-term)
        (send/i (focused-term) term-record<%> on-lose-focus))
      (cursor:move-prev terms)
      (refresh/move))
    (define/public-final (navigate-down)
      (when (focused-term)
        (send/i (focused-term) term-record<%> on-lose-focus))
      (cursor:move-next terms)
      (refresh/move))

    ;; enable/disable-buttons : -> void
    (define/private (enable/disable-buttons [? #t])
      (define term (and ? (focused-term)))
      (send area enable ?)
      (send (send frame get-menu-bar) enable ?)
      (send nav:start enable (and ? term (send/i term term-record<%> has-prev?)))
      (send nav:previous enable (and ? term (send/i term term-record<%> has-prev?)))
      (send nav:next enable (and ? term (send/i term term-record<%> has-next?)))
      (send nav:end enable (and ? term (send/i term term-record<%> has-next?)))
      (send nav:text enable (and ? term #t))
      (send nav:up enable (and ? (cursor:has-prev? terms)))
      (send nav:down enable (and ? (cursor:has-next? terms))))

    ;; Update

    ;; update/preserve-lines-view : -> void
    (define/public (update/preserve-lines-view)
      (begin
       (define text (send/i sbview sb:syntax-browser<%> get-text))
       (define start-box (box 0))
       (define end-box (box 0))
       (send text get-visible-line-range start-box end-box)
       (update*)
       (send text scroll-to-position
             (send text line-start-position (unbox start-box))
             #f
             (send text line-start-position (unbox end-box))
             'start)))

    ;; update/preserve-view : -> void
    (define/public (update/preserve-view)
      (begin
       (define text (send/i sbview sb:syntax-browser<%> get-text))
       (define start-box (box 0))
       (define end-box (box 0))
       (send text get-visible-position-range start-box end-box)
       (update*)
       (send text scroll-to-position (unbox start-box) #f (unbox end-box) 'start)))

    ;; update : -> void
    ;; Updates the terms in the syntax browser to the current step
    (define/private (update)
      (begin
       (update*)))

    (define/private (update*)
      ;; update:show-prefix : -> void
      (define (update:show-prefix)
        ;; Show the final terms from the cached synth'd derivs
        (for ([trec (in-list (cursor:prefix->list terms))])
          (send/i trec term-record<%> display-final-term)))
      ;; update:show-current-step : -> void
      (define (update:show-current-step)
        (when (focused-term)
          (send/i (focused-term) term-record<%> display-step)))
      ;; update:show-suffix : -> void
      (define (update:show-suffix)
        (let ([suffix0 (cursor:suffix->list terms)])
          (when (pair? suffix0)
            (for ([trec (in-list (cdr suffix0))])
              (send/i trec term-record<%> display-initial-term)))))
      ;; update-nav-index : -> void
      (define (update-nav-index)
        (define term (focused-term))
        (set-current-step-index
         (and term (send/i term term-record<%> get-step-index))))

      (define text (send/i sbview sb:syntax-browser<%> get-text))
      (define position-of-interest 0)
      (define multiple-terms? (> (length (cursor->list terms)) 1))

      (with-unlock text
        (send/i sbview sb:syntax-browser<%> erase-all)
        (update:show-prefix)
        (when multiple-terms? (send/i sbview sb:syntax-browser<%> add-separator))
        (set! position-of-interest (send text last-position))
        (update:show-current-step)
        (when multiple-terms? (send/i sbview sb:syntax-browser<%> add-separator))
        (update:show-suffix))

      (send text scroll-to-position
            position-of-interest
            #f
            (send text last-position)
            'start)
      (update-nav-index)
      (enable/disable-buttons #t))

    ;; --

    ;; refresh/resynth : -> void
    ;; Macro hiding policy has changed; invalidate cached parts of trec
    (define/public (refresh/resynth)
      (for ([trec (in-list (cursor->list terms))])
        (send/i trec term-record<%> invalidate-synth!))
      (refresh))

    ;; refresh/re-reduce : -> void
    ;; Reduction config has changed; invalidate cached parts of trec
    (define/private (refresh/re-reduce)
      (for ([trec (in-list (cursor->list terms))])
        (send/i trec term-record<%> invalidate-steps!))
      (refresh))

    ;; refresh/move : -> void
    ;; Moving between terms; clear the saved position
    (define/private (refresh/move)
      (refresh))

    ;; refresh : -> void
    (define/public (refresh)
      (begin
       (when (focused-term)
         (send/i (focused-term) term-record<%> on-get-focus))
       (send nav:step-count set-label "")
       (let ([term (focused-term)])
         (when term
           (let ([step-count (send/i term term-record<%> get-step-count)])
             (when step-count
               ;; +1 for end of expansion "step"
               (send nav:step-count set-label (format "of ~s" (add1 step-count)))))))
       (update*)))

    ;; Hiding policy
    
    (define/public (get-show-macro?)
      (send/i macro-hiding-prefs hiding-prefs<%> get-policy))

    ;; Derivation pre-processing

    ;; get-preprocess-deriv : -> (Deriv -> Deriv/#f)
    (define/public (get-preprocess-deriv)
      (send director get-preprocess-deriv))

    ;; Initialization

    (super-new)
    (show-macro-hiding-panel (send/i config config<%> get-show-hiding-panel?))
    (show-extra-navigation (send/i config config<%> get-extra-navigation?))
    ))

;; Obsolete: for backwards compatibility w/ old DrRacket tool only
(define macro-stepper-widget/process-mixin (lambda (%) %))
