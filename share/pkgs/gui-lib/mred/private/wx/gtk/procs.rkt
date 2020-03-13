#lang racket/base
(require ffi/unsafe
         "../../syntax.rkt"
         "../../lock.rkt"
         racket/class
         racket/draw
         "filedialog.rkt"
         "colordialog.rkt"
         "types.rkt"
         "utils.rkt"
         "style.rkt"
         "widget.rkt"
         "window.rkt"
         "frame.rkt"
         "dc.rkt"
         "queue.rkt"
         "printer-dc.rkt"
         "gl-context.rkt"
         "keycode.rkt"
         "../common/default-procs.rkt"
         "../common/handlers.rkt")

(provide
 (protect-out
  color-from-user-platform-mode
  get-font-from-user
  font-from-user-platform-mode
  play-sound
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
  id-to-menu-item
  can-show-print-setup?
  get-highlight-background-color
  get-highlight-text-color
  get-label-background-color
  get-label-foreground-color
  check-for-break)
 file-selector
 show-print-setup
 display-origin
 display-size
 display-bitmap-resolution
 flush-display
 location->window
 make-screen-bitmap
 make-gl-bitmap
 file-creator-and-type
 special-control-key
 special-option-key
 any-control+alt-is-altgr
 get-panel-background
 fill-private-color
 get-color-from-user
 key-symbol-to-menu-key
 needs-grow-box-spacer?
 graphical-system-type)

(define (find-graphical-system-path what)
  (case what
    [(x-display) (string->path x11-display)]
    [else #f]))

(define (cancel-quit) (void))

(define-unimplemented play-sound)

(define (color-from-user-platform-mode) 
  (and (color-dialog-works?)
       'dialog))

(define (font-from-user-platform-mode) #f)
(define-unimplemented get-font-from-user)

(define (register-collecting-blit canvas x y w h on off on-x on-y off-x off-y)
  (send canvas register-collecting-blit x y w h on off on-x on-y off-x off-y))
(define (unregister-collecting-blit canvas)
  (send canvas unregister-collecting-blits))
(define (shortcut-visible-in-label? [mbar? #f]) #t)

(define _GtkSettings (_cpointer 'GtkSettings))
(define-gtk gtk_settings_get_default (_fun -> _GtkSettings))
(define-gobj g_object_get/int (_fun _GtkSettings _string (r : (_ptr o _int)) (_pointer = #f) 
				    -> _void
				    -> r)
  #:c-id g_object_get)
(define-gobj g_object_get/string (_fun _GtkSettings _string (r : (_ptr o _pointer)) (_pointer = #f)
				       -> _void
				       -> r)
  #:c-id g_object_get)

(define (get-double-click-time)
  (let ([s (gtk_settings_get_default)])
    (if s
	(g_object_get/int s "gtk-double-click-time")
	250)))
(define (get-control-font proc default)
  (or
   (let ([s (gtk_settings_get_default)])
     (and s
	  (let ([f (g_object_get/string s "gtk-font-name")])
	    (and f
		 (begin0
		  (cond
		   [(regexp-match #rx"^(.*) ([0-9]+)$" (cast f _pointer _string))
		    => (lambda (m) (proc (cdr m)))]
		   [else #f])
		  (g_free f))))))
   default))
(define (get-control-font-size)
  (define s (get-control-font (lambda (m) (string->number (cadr m)))
                              10))
  (cond
   [(and gtk3?
         ((gtk_get_minor_version) . >= . 22))
    ;; As of version 3.22, a size in points ends up rounded
    ;; to an integral absolute size for 96 DPI; see also
    ;; `install-control-font`
    (* (round (* s (/ 96.0 72.0))) (/ 72.0 96.0))]
   [else s]))

(define (get-control-font-face)
  (get-control-font (lambda (m) (car m))
		    "Sans"))
(define (get-control-font-size-in-pixels?) #f)

(define (get-display-depth) 32)

(define-gdk gdk_display_beep (_fun _GdkDisplay -> _void))
(define (bell) (gdk_display_beep (gdk_display_get_default)))

(define (hide-cursor) (void))

(define (is-color-display?) #t)

(define (id-to-menu-item i) i)
(define (can-show-print-setup?) #t)

(define (get-highlight-background-color)
  (let-values ([(r g b) (get-selected-background-color)])
    (make-object color% r g b)))

(define (get-highlight-text-color)
  (let-values ([(r g b) (get-selected-text-color)])
    (if (and (zero? r) (zero? g) (zero? b))
	#f
	(make-object color% r g b))))

(define (get-label-background-color)
  (let-values ([(r g b) (get-label-bg-color)])
    (make-object color% r g b)))

(define (get-label-foreground-color)
  (let-values ([(r g b) (get-label-fg-color)])
    (make-object color% r g b)))

(define/top (make-screen-bitmap [exact-positive-integer? w]
                                [exact-positive-integer? h])
  (if (and (eq? 'unix (system-type))
           (not wayland?))
      (make-object x11-bitmap% w h #f)
      (make-object bitmap% w h #f #t (display-bitmap-resolution 0 (lambda () 1.0)))))

(define/top (make-gl-bitmap [exact-positive-integer? w]
                            [exact-positive-integer? h]
                            [gl-config% c])
  (let ([bm (make-object x11-bitmap% w h #f)])
    (create-and-install-gl-context bm c)
    bm))

(define (check-for-break) #f)

(define (needs-grow-box-spacer?) #f)

(define (graphical-system-type)
  (cond
   [gtk3? 'gtk3]
   [else 'gtk2]))
