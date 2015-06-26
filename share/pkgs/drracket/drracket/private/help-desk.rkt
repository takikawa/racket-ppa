#lang racket/unit

(require racket/gui/base
         browser/external
         framework
         racket/class
         net/url
         setup/dirs
         setup/materialize-user-docs
         help/search
         help/private/buginfo
         drracket/private/drsig
         string-constants
         "local-member-names.rkt")

(import [prefix drracket:frame: drracket:frame^]
        [prefix drracket:language-configuration: drracket:language-configuration/internal^]
        [prefix drracket:init: drracket:init^])
(export drracket:help-desk^)

(define (-add-help-desk-font-prefs b) '(add-help-desk-font-prefs b))

;; : -> string
(define (get-computer-language-info)
  (let* ([language/settings (preferences:get 
                             drracket:language-configuration:settings-preferences-symbol)]
         [language (drracket:language-configuration:language-settings-language
                    language/settings)]
         [settings (drracket:language-configuration:language-settings-settings
                    language/settings)])
    (format
     "~s"
     (list
      (send language get-language-position)
      (send language marshall-settings settings)))))

(set-bug-report-info! "Computer Language" get-computer-language-info)
(set-bug-report-info! "Recent Internal Errors" 
                      (λ ()
                        (define errs (drracket:init:get-last-N-errors))
                        (define sp (open-output-string))
                        (unless (null? errs) 
                          (fprintf sp "Saved ~a internal error~a:\n\n"
                                   (length errs)
                                   (if (= 1 (length errs)) "" "s")))
                        (parameterize ([current-error-port sp])
                          (define first? #t)
                          (for ([err (in-list errs)])
                            (if first? 
                                (set! first? #f)
                                (eprintf "\n\n"))
                            (drracket:init:original-error-display-handler
                             (list-ref err 0)
                             (list-ref err 1))))
                        (get-output-string sp))
                      100)

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

(define (maybe-try-to-materialize-docs)
  ;; under mac os x, the basic installation is put into a 'com.apple.quarantine'
  ;; which has the strange effect of making osascript drop the query parameters
  ;; for urls when they are sent to the browser. To work around this,
  ;; we materialize the documentation indicies in a user-specific place the
  ;; first time someone tries to read the docs with a specific query
  ;; the 'drracket:tried-materialize-user-docs pref is initialized to #t
  ;; on non-mac os x platforms so that we don't try at all there.
  (unless (preferences:get 'drracket:tried-materialize-user-docs)
    (preferences:set 'drracket:tried-materialize-user-docs #t)
    (define sp (open-output-string))
    (define succeeded? #t)
    (materialize-user-docs (λ (go)
                             (set! succeeded? #f)
                             (parameterize ([current-output-port sp]
                                            [current-error-port sp])
                               (set! succeeded? (go)))))
    (unless succeeded?
      (message-box (string-constant drracket)
                   (string-append
                    "Attempting to materialize user docs failed:\n\n"
                    (get-output-string sp))))))

(define (help-desk [key #f] [context #f])
  (when key (maybe-try-to-materialize-docs))
  (if key (perform-search key context) (send-main-page)))
