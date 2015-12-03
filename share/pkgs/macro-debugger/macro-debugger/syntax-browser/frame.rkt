#lang racket/base
(require racket/class
         racket/gui/base
         racket/class/iop
         macro-debugger/syntax-browser/interfaces
         macro-debugger/syntax-browser/partition
         "prefs.rkt"
         "widget.rkt")
(provide browse-syntax
         browse-syntaxes
         make-syntax-browser
         syntax-browser-frame%
         syntax-widget/controls%)

;; browse-syntax : syntax -> void
(define (browse-syntax stx)
  (browse-syntaxes (list stx)))

;; browse-syntaxes : (list-of syntax) -> void
(define (browse-syntaxes stxs)
  (let ((w (make-syntax-browser)))
    (for ([stx stxs])
      (send*/i w syntax-browser<%>
        (add-syntax stx)
        (add-separator)))))

;; make-syntax-browser : -> syntax-browser<%>
(define (make-syntax-browser)
  (let* ([view (new syntax-browser-frame%)])
    (send view show #t)
    (send view get-widget)))

;; syntax-browser-frame%
(define syntax-browser-frame%
  (class* frame% ()
    (inherit get-width
             get-height)
    (init-field/i [config config<%> (new syntax-prefs%)])
    (super-new (label "Syntax Browser")
               (width (send/i config config<%> get-width))
               (height (send/i config config<%> get-height)))
    (define/i widget syntax-browser<%>
      (new syntax-widget/controls%
           (parent this)
           (config config)))
    (define/public (get-widget) widget)
    (define/augment (on-close)
      (send*/i config config<%>
        (set-width (get-width))
        (set-height (get-height)))
      (send widget shutdown)
      (inner (void) on-close))))

;; syntax-widget/controls%
(define syntax-widget/controls%
  (class* widget% ()
    (inherit get-main-panel
             get-controller)
    (super-new)
    (inherit-field config)

    (define -control-panel 
      (new horizontal-pane%
           (parent (get-main-panel))
           (stretchable-height #f)))

    ;; Put the control panel up front
    (send (get-main-panel) change-children
          (lambda (children)
            (cons -control-panel (remq -control-panel children))))

    (define -identifier=-choices (identifier=-choices))
    (define -choice
      (new choice% (label "identifier=?") (parent -control-panel)
           (choices (map car -identifier=-choices))
           (callback 
            (lambda (c e)
              (cond [(assoc (send c get-string-selection)
                            -identifier=-choices)
                     => (lambda (p) (send/i (get-controller) controller<%>
                                       set-identifier=? (cdr p)))])))))
    (new button% 
         (label "Clear")
         (parent -control-panel)
         (callback (lambda _ (send/i (get-controller) controller<%> set-selected-syntax #f))))
    (new button%
         (label "Properties")
         (parent -control-panel)
         (callback
          (lambda _ 
            (send/i config config<%> set-props-shown? 
                   (not (send/i config config<%> get-props-shown?))))))

    (send/i (get-controller) controller<%> listen-identifier=?
            (lambda (func)
              (send -choice set-string-selection
                    (for/or ([name+func (in-list -identifier=-choices)])
                      (and (eq? (cdr name+func) func)
                           (car name+func))))))
    ))
