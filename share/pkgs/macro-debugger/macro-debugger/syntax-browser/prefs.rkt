#lang racket/base
(require racket/class
         framework/preferences
         macro-debugger/syntax-browser/interfaces
         framework/notify)
(provide prefs-base%
         syntax-prefs-base%
         syntax-prefs%
         syntax-prefs/readonly%

         pref:invert-colors?)

(preferences:set-default 'SyntaxBrowser:Width 700 number?)
(preferences:set-default 'SyntaxBrowser:Height 600 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelPercentage 1/3 number?)
(preferences:set-default 'SyntaxBrowser:PropertiesPanelShown #t boolean?)
(preferences:set-default 'SyntaxBrowser:DrawArrows? #t boolean?)

(define pref:width (preferences:get/set 'SyntaxBrowser:Width))
(define pref:height (preferences:get/set 'SyntaxBrowser:Height))
(define pref:props-percentage (preferences:get/set 'SyntaxBrowser:PropertiesPanelPercentage))
(define pref:props-shown? (preferences:get/set 'SyntaxBrowser:PropertiesPanelShown))
(define pref:draw-arrows? (preferences:get/set 'SyntaxBrowser:DrawArrows?))

(define pref:invert-colors? (preferences:get/set 'framework:white-on-black?))

(define prefs-base%
  (class object%
    ;; suffix-option : SuffixOption
    (notify:define-notify suffix-option (new notify:notify-box% (value 'over-limit)))

    ;; pretty-abbrev? : boolean
    (notify:define-notify pretty-abbrev? (new notify:notify-box% (value #t)))

    ;; pretty-styles : ImmutableHash[symbol -> symbol]
    (notify:define-notify pretty-styles
      (new notify:notify-box% (value (make-immutable-hasheq null))))

    ;; syntax-font-size : number/#f
    ;; When non-false, overrides the default font size
    (notify:define-notify syntax-font-size (new notify:notify-box% (value #f)))

    ;; colors : (listof string)
    (notify:define-notify colors
      (new notify:notify-box% (value the-colors)))

    (super-new)))

(define alt-colors
  '("black"
    "red"       "blue"           "forestgreen" "purple"       "brown"
    "firebrick" "darkblue"       "seagreen"    "violetred"    "chocolate"
    "darkred"   "cornflowerblue" "darkgreen"   "indigo"       "sandybrown"
    "orange"    "cadetblue"      "olive"       "mediumpurple" "goldenrod"))

(define the-colors
  '("black" "red" "blue"
    "mediumforestgreen" "darkgreen" 
    "darkred"
    "cornflowerblue" "royalblue" "steelblue" "darkslategray" "darkblue"
    "indigo" "purple" 
    "orange" "salmon" "darkgoldenrod" "olive"))

(define syntax-prefs-base%
  (class* prefs-base% (config<%>)
    (init readonly?)

    (define-syntax-rule (define-pref-notify* (name pref) ...)
      (begin (notify:define-notify name (notify:notify-box/pref pref #:readonly? readonly?)) ...))

    (define-pref-notify*
      (width pref:width)
      (height pref:height)
      (props-percentage pref:props-percentage)
      (props-shown? pref:props-shown?)
      (draw-arrows? pref:draw-arrows?))

    (super-new)))

(define syntax-prefs%
  (class syntax-prefs-base%
    (super-new (readonly? #f))))

(define syntax-prefs/readonly%
  (class syntax-prefs-base%
    (super-new (readonly? #t))))
