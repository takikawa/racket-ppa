#lang scheme/unit

(require scheme/gui/base
         browser/external
         framework
         scheme/class
         net/url
         setup/dirs
         help/search
         help/private/buginfo
         "drsig.ss")

(import [prefix drscheme:frame: drscheme:frame^]
        [prefix drscheme:language-configuration: drscheme:language-configuration/internal^])
(export drscheme:help-desk^)

(define (-add-help-desk-font-prefs b) '(add-help-desk-font-prefs b))

;; : -> string
(define (get-computer-language-info)
  (let* ([language/settings (preferences:get 
                             drscheme:language-configuration:settings-preferences-symbol)]
         [language (drscheme:language-configuration:language-settings-language
                    language/settings)]
         [settings (drscheme:language-configuration:language-settings-settings
                    language/settings)])
    (format
     "~s"
     (list
      (send language get-language-position)
      (send language marshall-settings settings)))))

(set-bug-report-info! "Computer Language" get-computer-language-info)

(define lang-message%
  (class canvas%
    (init-field button-release font)
    (define/override (on-event evt)
      (when (send evt button-up?)
        (button-release)))
    (field [msg ""])
    (define/public (set-msg l) (set! msg l) (on-paint))
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (let ([dc (get-dc)]
            [dots "..."])
        (let-values ([(tw th _1 _2) (send dc get-text-extent msg)]
                     [(dw dh _3 _4) (send dc get-text-extent dots)]
                     [(cw ch) (get-client-size)])
          (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
          (send dc set-pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
          (send dc set-font font)
          (send dc draw-rectangle 0 0 cw ch)
          (cond
            [(tw . <= . cw)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)))]
            [(cw . <= . dw)  ;; just give up if there's not enough room to draw the dots
             (void)]
            [else
             (send dc set-clipping-rect 0 0 (- cw dw 2) ch)
             (send dc draw-text msg 0 (- (/ ch 2) (/ th 2)))
             (send dc set-clipping-region #f)
             (send dc draw-text dots (- cw dw) (- (/ ch 2) (/ th 2)))]))))
    (super-new)))

(define (goto-plt-license)
  (send-main-page #:sub "license/index.html"))

(define (help-desk [key #f] [context #f])
  (if key (perform-search key context) (send-main-page)))

;; here for legacy code that should be removed
(define (get-docs) '())
