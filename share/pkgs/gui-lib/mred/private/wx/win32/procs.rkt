#lang racket/base
(require ffi/unsafe
         racket/class
	 "../../syntax.rkt"
	 "theme.rkt"
         "types.rkt"
         "utils.rkt"
         "const.rkt"
         "menu-item.rkt"
         "frame.rkt"
         "window.rkt"
         "dc.rkt"
         "printer-dc.rkt"
         (except-in "../common/default-procs.rkt"
                    get-panel-background
                    any-control+alt-is-altgr)
         "filedialog.rkt"
         "colordialog.rkt"
         "sound.rkt"
         "key.rkt"
	 racket/draw)

(provide
 (protect-out 
  color-from-user-platform-mode
  get-font-from-user
  font-from-user-platform-mode
  get-panel-background
  find-graphical-system-path
  register-collecting-blit
  unregister-collecting-blit
  shortcut-visible-in-label?
  get-double-click-time
  get-control-font-face
  get-control-font-size
  get-control-font-size-in-pixels?
  cancel-quit
  bell
  hide-cursor
  get-display-depth
  is-color-display?
  can-show-print-setup?
  get-highlight-background-color
  get-highlight-text-color
  check-for-break)
 flush-display
 get-current-mouse-state
 fill-private-color
 play-sound
 location->window
 file-selector
 show-print-setup
 id-to-menu-item
 file-creator-and-type
 display-origin
 display-size
 display-bitmap-resolution
 make-screen-bitmap
 make-gl-bitmap
 special-control-key
 special-option-key
 get-color-from-user
 key-symbol-to-menu-key
 needs-grow-box-spacer?
 graphical-system-type)

(define (find-graphical-system-path what)
  #f)

(define (cancel-quit) (void))

(define (color-from-user-platform-mode) 'dialog)

(define (font-from-user-platform-mode) #f)
(define-unimplemented get-font-from-user)

(define (get-panel-background)
  (let ([c (GetSysColor COLOR_BTNFACE)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))

(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [? #f]) #t)

(define (get-double-click-time) 500)
(define (get-control-font-face) (get-theme-font-face))
(define (get-control-font-size) (->normal (get-theme-font-size)))
(define (get-control-font-size-in-pixels?) #t)

(define-user32 MessageBeep (_wfun _UINT -> _BOOL))
(define (bell)
  (void (MessageBeep MB_OK)))

(define (hide-cursor) (void))

(define (get-display-depth) 32)

(define (is-color-display?) #t)

(define (can-show-print-setup?) #t)

(define (get-highlight-background-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))
(define (get-highlight-text-color)
  (let ([c (GetSysColor COLOR_HIGHLIGHTTEXT)])
    (make-object color% (GetRValue c) (GetGValue c) (GetBValue c))))

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (make-object win32-bitmap% w h #f))

(define/top (make-gl-bitmap [exact-positive-integer? w]
                            [exact-positive-integer? h]
                            [gl-config% c])
  (make-object win32-bitmap% w h #f c))

(define (check-for-break) #f)

(define (needs-grow-box-spacer?) #f)

(define (graphical-system-type) 'win32)

(define-user32 GetCursorPos (_wfun (p : (_ptr o _POINT)) -> (r : _BOOL)
                                   ;; GetCursorPos can fail if permission
                                   ;; to access the display goes away, which
                                   ;; can happen temporarily when Windows is
                                   ;; put to sleep
                                   -> (and r p)))
(define-user32 GetAsyncKeyState (_wfun _int -> _SHORT))
(define-user32 GetSystemMetrics (_wfun _int -> _int))
(define SM_SWAPBUTTON 23)
(define (get-current-mouse-state)
  (define p (GetCursorPos))
  (cond
    [p
     (define (maybe vk sym)
       (if (negative? (GetAsyncKeyState vk))
           (list sym)
           null))
     (define swapped? (not (zero? (GetSystemMetrics SM_SWAPBUTTON))))
     (values (make-object point% (->normal (POINT-x p)) (->normal (POINT-y p)))
             (append
              (maybe (if swapped? VK_RBUTTON VK_LBUTTON) 'left)
              (maybe (if swapped? VK_LBUTTON VK_RBUTTON) 'right)
              (maybe VK_LSHIFT 'shift)
              (maybe VK_CONTROL 'control)
              (maybe VK_MENU 'alt)
              (maybe VK_CAPITAL 'caps)))]
    [else
     ;; Since `get-current-mouse-state` doesn't have a notion of
     ;; failure, make up the obvious result:
     (values (make-object point% 0 0)
             '())]))
