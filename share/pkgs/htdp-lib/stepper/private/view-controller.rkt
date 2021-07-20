#lang racket/unit

;; this module implements the UI side of the stepper; it
;; opens a window, starts the stepper thread running,
;; and handles the resulting calls to 'break'.

;; this module lies outside of the "testing boundary"
;; of through-tests; it is not tested automatically at all.

(require racket/class
         racket/match
         racket/list
         drracket/tool
         mred
         string-constants
         racket/async-channel
         (prefix-in model: "model.rkt")
         (prefix-in x: "mred-extensions.rkt")
         "shared.rkt"
         "shared-typed.rkt"
         "syntax-hider.rkt"
         "model-settings.rkt"
         "view-controller-typed.rkt"
         images/compile-time
         images/gui
         mrlib/switchable-button
         mrlib/panel-wob
         framework
         (for-syntax racket/base
                     images/icons/control
                     images/icons/symbol
                     images/icons/style
                     images/logos
                     pict))


(import  stepper-frame^)
(export view-controller^)

(define drracket-eventspace (current-eventspace))

(define (definitions-text->settings definitions-text)
  (send definitions-text get-next-settings))

(define (show-about-dialog parent)
  (define dlg
    (new logo-about-dialog%
         [label "About the Stepper"]
         [parent parent]
         [bitmap (compiled-bitmap (stepper-logo))]
         [messages '("The Algebraic Stepper is formalized and proved correct in\n"
                     "\n"
                     "    John Clements, Matthew Flatt, Matthias Felleisen\n"
                     "    Modeling an Algebraic Stepper\n"
                     "    European Symposium on Programming, 2001\n")]))
  (send dlg show #t))

;; create a new view-controller, start the model
(define (vc-go drracket-tab program-expander dynamic-requirer
               language-level simple-settings
               selection-start selection-end)

  ;; VALUE CONVERSION CODE:

  ;; render-to-string : TST -> string
  (define (render-to-string val)
    (let ([string-port (open-output-string)])
      (send language-level render-value
            val simple-settings string-port)
      (get-output-string string-port)))

  ;; render-to-sexp : TST -> sexp
  (define (render-to-sexp val)
    (send language-level stepper:render-to-sexp
          val simple-settings language-level))

  ;; channel for incoming views
  (define view-channel (make-async-channel))

  ;; the first-step semaphore
  (define first-step-sema (make-semaphore 0))

  (define step-repository (new-step-repository))
  (define add-step! (make-add-step! step-repository))
  (define num-steps (make-num-steps step-repository))
  (define step-ref (make-step-ref step-repository))

  ;; the view in the stepper window. All buttons should
  ;; be disabled until the first step is added, so any
  ;; code triggered by a button should be able to assume
  ;; that view is a number
  #;(: view (U False Natural))
  (define view #f)

  ;; wait for steps to show up on the channel.
  ;; When they do, add them to the list.
  (define (start-listener-thread stepper-frame-eventspace)
    (thread
     (lambda ()
       (let loop ()
         (define new-result (async-channel-get view-channel))
         (receive-result-from-target new-result)
         (loop)))))

  ;; handles an incoming result. Either adds it to the list of
  ;; steps, or prompts user to see whether to continue running.
  (define (receive-result-from-target result)
    (cond [(Runaway-Process? result)
           (parameterize ([current-eventspace stepper-frame-eventspace])
             (queue-callback
              (lambda ()
                (when (confirm-running)
                  (semaphore-post (Runaway-Process-sema result)))
                (void))))]
          [else
           (define new-step (format-result result))
           (parameterize ([current-eventspace stepper-frame-eventspace])
             (queue-callback
              (lambda ()
                (add-step! new-step)
                ;; this is only necessary the first time, but it's cheap:
                (semaphore-post first-step-sema)
                (update-status-bar))))]))


  ;; STEP PREDICATES:

  ;; 2 moved to typed...

  ;; is this step on the selected expression?
  (define (selected-exp-step? history-entry)
    (ormap (span-overlap selection-start selection-end) (Step-posns history-entry)))

  ;; build gui object:


  ;; next-of-specified-kind : starting at the current view, search forward for the
  ;; desired step or wait for it if not found
  (define (next-of-specified-kind right-kind? msg)
    (maybe-update-view (find-later-step right-kind? view (num-steps) step-ref) msg))

  ;; first-of-specified-kind : similar to next-of-specified-kind, but
  ;; always start at zero
  (define (first-of-specified-kind right-kind? msg)
    (maybe-update-view (find-first-step right-kind? (num-steps) step-ref) msg))

  ;; previous-of-specified-kind: if the desired step is already in the list, display
  ;; it; otherwise, put up a dialog
  (define (previous-of-specified-kind right-kind? msg)
    (maybe-update-view (find-earlier-step right-kind? view step-ref) msg))

  ;; next-of-specified-kind/helper : if the desired step
  ;; is already in the list, display it; otherwise, give up.
  (define (maybe-update-view maybe-step msg)
    (match maybe-step
      [(? number? n)
       (update-view/existing n)]
      ['nomatch
       (message-box (string-constant stepper-no-such-step/title) msg)]))

  ;; BUTTON/CHOICE BOX PROCEDURES

  ;; respond to a click on the "next" button
  (define (next)
    (next-of-specified-kind (lambda (x) #t)
                            (string-constant stepper-no-later-step)))

  ;; previous : the action of the 'previous' button
  (define (previous)
    (previous-of-specified-kind (lambda (x) #t)
                             (string-constant stepper-no-earlier-step)))

  ;; jump-to-beginning : the action of the choice menu entry
  (define (jump-to-beginning)
    (first-of-specified-kind (lambda (x) #t)
                             ;; I don't believe this can fail...
                             "internal error 2010-01-10 21:48"))

  ;; jump-to-end : the action of the jump-to-end choice box option
  (define (jump-to-end)
    (first-of-specified-kind finished-stepping-step?
                             (string-constant stepper-no-last-step)))

  ;; jump-to-selected : the action of the jump to selected choice box option
  (define (jump-to-selected)
    (first-of-specified-kind selected-exp-step?
                             (string-constant stepper-no-selected-step)))

  ;; jump-to-next-application : the action of the jump to next application
  ;; choice box option
  (define (jump-to-next-application)
    (next-of-specified-kind application-step?
                            (string-constant stepper-no-later-application-step)))

  ;; jump-to-previous-application : the action of the "jump to previous application"
  ;; choice box option
  (define (jump-to-previous-application)
    (previous-of-specified-kind application-step?
                             (string-constant
                              stepper-no-earlier-application-step)))

  ;; GUI ELEMENTS:
  (define s-frame
    (make-object stepper-frame% drracket-tab))

  (define top-panel
    (new horizontal-panel% [parent (send s-frame get-area-container)] [horiz-margin 5]
         ;[style '(border)]  ; for layout testing only
         [stretchable-width #t]
         [stretchable-height #f]))

  (define button-panel
    (new panel:horizontal-discrete-sizes% [parent top-panel] [alignment '(center top)]
         ;[style '(border)]  ; for layout testing only
         [stretchable-width #t]
         [stretchable-height #f]))

  ;; this text box counts the steps for the user
  (define status-text
    (new text:hide-caret/selection%))

  (define status-canvas
    (new editor-canvas%
         [parent top-panel]
         [editor status-text]
         [stretchable-width #f]
         [style '(transparent no-border no-hscroll no-vscroll)]
         ;; some way to get the height of a line of text?
         [min-width 100]))

  (define logo-canvas
    (new (class bitmap-canvas%
           (super-new [parent top-panel] [bitmap (compiled-bitmap (stepper-logo #:height 32))])
           (define/override (on-event evt)
             (when (eq? (send evt get-event-type) 'left-up)
               (show-about-dialog s-frame))))))


  ;; a button-making abstraction
  (define (stepper-button label bitmap callback)
      (new switchable-button%
           [parent button-panel]
           [label label]
           [bitmap bitmap]
           [callback (λ (_1) (callback))]
           [enabled #f]))

  (define beginning-button
    (stepper-button "Beginning" to-beginning-img jump-to-beginning))
  (define previous-app-button
    (stepper-button "Previous Call" prev-app-img jump-to-previous-application))
  (define previous-button
    (stepper-button "Previous" prev-img previous))
  (define selected-button
    (stepper-button "Selected" to-selected-img jump-to-selected))
  (define next-button
    (stepper-button "Next" next-img next))
  (define next-app-button
    (stepper-button "Next Call" next-app-img jump-to-next-application))
  (define end-button
    (stepper-button "End" to-end-img jump-to-end))

  (define all-buttons
    (list selected-button
          beginning-button previous-app-button previous-button
          end-button       next-app-button     next-button))

  (define canvas
    (make-object x:stepper-canvas% (send s-frame get-area-container)))

  ;; update-view/existing : set an existing step as the one shown in the
  ;; frame
  (define (update-view/existing new-view)
    (set! view new-view)
    (let ([e (Step-text (step-ref view))])
      (send e begin-edit-sequence)
      (send canvas set-editor e)
      (send e reset-width canvas)
      (send e end-edit-sequence))
    (update-status-bar))

  ;; set the status bar to the correct m/n text.
  (define (update-status-bar)
    (send status-text begin-edit-sequence)
    (send status-text lock #f)
    (send status-text delete 0 (send status-text last-position))
    ;; updated to yield 1-based step numbering rather than 0-based numbering.
    (send status-text insert
          (format "~a/~a" (if view (+ 1 view) "none") (num-steps)))
    (when (white-on-black-panel-scheme?)
      (define sd (new style-delta%))
      (send sd set-delta-foreground "white")
      (send status-text change-style sd 0 (send status-text last-position)))
    (send status-text lock #t)
    (send status-text end-edit-sequence))

  (define update-status-bar-semaphore (make-semaphore 1))

  (define (enable-all-buttons)
    (for ([b (in-list all-buttons)])
      (send b enable #t)))

  (define (print-current-view item evt)
    (send (send canvas get-editor) print))

  ;; code for dealing with runaway processes:

  (define runaway-counter-limit 500)
  (define disable-runaway-counter #f)
  (define runaway-counter 0)

  ;; runs on the stepped-process side.
  ;; checks to see if the process has taken too
  ;; many steps. If so, send a message and block
  ;; for a response, then send the result. Otherwise,
  ;; just send the result.
  (define (deliver-result-to-gui result)
    (when (not disable-runaway-counter)
      (set! runaway-counter (+ runaway-counter 1)))
    (when (= runaway-counter runaway-counter-limit)
      (define runaway-semaphore (make-semaphore 0))
      (async-channel-put view-channel
                         (Runaway-Process runaway-semaphore))
      ;; wait for a signal to continue running:
      (semaphore-wait runaway-semaphore))
    (async-channel-put view-channel result))

  (define keep-running-message
    (string-append
     "The program running in the stepper has taken a whole bunch of steps. "
     "Do you want to continue running it for now, halt, or let it run "
     "without asking again?"))

  (define (confirm-running)
    (define message-box-result
      (message-box/custom
       "Keep Running Program?"
       keep-running-message
       "Continue for now"
       "Halt"
       "Continue uninterrupted"
       #f ;; use the stepper window instead?
       '(stop disallow-close default=1)
       ))
    (match message-box-result
      ;; continue-for-now:
      [1 (set! runaway-counter 0)
         #t]
      ;; halt:
      [2 #f]
      ;; continue-forever:
      [3 (set! runaway-counter 0)
         (set! disable-runaway-counter #t)
         #t]))


  ;; translates a result into a step
  ;; format-result : step-result -> step?
  (define (format-result result)
    (match result
      [(Before-After-Result pre-exps post-exps kind pre-src post-src)
       (Step (new x:stepper-text%
                  [left-side (map sstx-s pre-exps)]
                  [right-side (map sstx-s post-exps)]
                  [show-inexactness?
                   (send language-level stepper:show-inexactness?)]
                  [print-boolean-long-form?
                   (send language-level stepper:print-boolean-long-form?)])
             kind
             (list pre-src post-src))]
      [(Before-Error-Result pre-exps err-msg pre-src)
       (Step (new x:stepper-text%
                  [left-side (map sstx-s pre-exps)]
                  [right-side err-msg]
                  [show-inexactness?
                   (send language-level stepper:show-inexactness?)]
                  [print-boolean-long-form?
                   (send language-level stepper:print-boolean-long-form?)])
             'finished-or-error
             (list pre-src))]
      [(Error-Result err-msg)
       (Step (new x:stepper-text%
                  [left-side null]
                  [right-side err-msg]
                  [show-inexactness?
                   (send language-level stepper:show-inexactness?)]
                  [print-boolean-long-form?
                   (send language-level stepper:print-boolean-long-form?)])
             'finished-or-error
             (list))]
      ['finished-stepping
       (Step x:finished-text 'finished-or-error (list))]))

  ;; program-expander-prime : wrap the program-expander for a couple of reasons:
  ;; 1) we need to capture the custodian as the thread starts up:
  ;; ok, it was just one.
  ;;
  (define (program-expander-prime init iter)
    (program-expander
     (lambda args
       (send s-frame set-custodian! (current-custodian))
       (apply init args))
     iter))

  ;; CONFIGURE GUI ELEMENTS
  (send s-frame set-printing-proc print-current-view)
  (send canvas stretchable-height #t)
  (send (send s-frame edit-menu:get-undo-item) enable #f)
  (send (send s-frame edit-menu:get-redo-item) enable #f)

  (define stepper-frame-eventspace (send s-frame get-eventspace))
  ;; START THE MODEL
  (start-listener-thread stepper-frame-eventspace)
  (model:go
   program-expander-prime
   dynamic-requirer
   ;; what do do with the results:
   deliver-result-to-gui
   (get-render-settings render-to-string
                        render-to-sexp
                        (send language-level stepper:enable-let-lifting?)
			(send language-level stepper:show-consumed-and/or-clauses?)
                        (send language-level stepper:show-lambdas-as-lambdas?)))

  (send s-frame show #t)

  ;; turn on the buttons and display the first step when it shows up:
  (thread
   (lambda ()
     (semaphore-wait first-step-sema)
     (parameterize
         ([current-eventspace stepper-frame-eventspace])
       (queue-callback
        (lambda ()
          (update-view/existing 0)
          (enable-all-buttons))))))

  s-frame)



;; the left-arrow image
(define prev-img (compiled-bitmap (step-back-icon #:color run-icon-color
                                                  #:height (toolbar-icon-height))))
(define to-beginning-img (compiled-bitmap (search-backward-icon #:color run-icon-color
                                                          #:height (toolbar-icon-height))))
(define prev-app-img (compiled-bitmap
                      (pict->bitmap
                       (let ([little-lambda
                              (bitmap
                               (lambda-icon #:height (* 4/5 (toolbar-icon-height))))])
                         (lc-superimpose
                          little-lambda
                          (hc-append
                           (blank (* 6/7 (pict-width little-lambda)) 0)
                           (bitmap
                            (back-icon #:color "green"
                                       #:height (toolbar-icon-height)))))))))


(define to-selected-img
  (compiled-bitmap (check-icon #:height (toolbar-icon-height))))

;; the right-arrow-image
(define next-img (compiled-bitmap (step-icon #:color run-icon-color
                                             #:height (toolbar-icon-height))))
(define to-end-img (compiled-bitmap (search-forward-icon #:color run-icon-color
                                               #:height (toolbar-icon-height))))
(define next-app-img (compiled-bitmap
                      (pict->bitmap
                       (let ([little-lambda
                              (bitmap
                               (lambda-icon #:height (* 4/5 (toolbar-icon-height))))])
                         (rc-superimpose
                          little-lambda
                          (hc-append
                           (bitmap
                            (play-icon #:color "green"
                                       #:height (toolbar-icon-height)))
                           (blank (* 6/7 (pict-width little-lambda)) 0)))))))

;; UTILITY FUNCTIONS:

;; span-overlap : number number -> posn-info -> boolean
;; return true if the selection is of zero length and precedes a char of the
;; stepping expression, *or* if the selection has positive overlap with the
;; stepping expression.
(define ((span-overlap selection-start selection-end) source-posn-info)
  (match source-posn-info
    [#f #f]
    [(Posn-Info posn span)
     (let ([end (+ posn span)])
       (and posn
            ;; you can *almost* combine these two, but not quite.
            (cond [(= selection-start selection-end)
                   (and (<= posn selection-start) (< selection-start end))]
                  [else
                   (let ([overlap-begin (max selection-start posn)]
                         ;; nb: we don't want zero-length overlaps at the end.
                         ;; compensate by subtracting one from the end of the
                         ;; current expression.
                         [overlap-end (min selection-end end)])
                     ;; #t if there's positive overlap:
                     (< overlap-begin overlap-end))])))]))

;; a few unit tests.  Use them if changing span-overlap.
;; ...oops, can't use module+ inside of a unit.
#;(module+ test
  (require rackunit)
  ;; zero-length selection cases:
  (check-equal? ((span-overlap 13 13) (model:make-posn-info 14 4)) #f)
  (check-equal? ((span-overlap 14 14) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 18 18) (model:make-posn-info 14 4)) #f)
  ;; nonzero-length selection cases:
  (check-equal? ((span-overlap 13 14) (model:make-posn-info 14 4)) #f)
  (check-equal? ((span-overlap 13 15) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 13 23) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 16 17) (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 16 24)  (model:make-posn-info 14 4)) #t)
  (check-equal? ((span-overlap 18 21)  (model:make-posn-info 14 4)) #f))
