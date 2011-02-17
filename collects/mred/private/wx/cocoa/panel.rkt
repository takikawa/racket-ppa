#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/objc
          "../../syntax.rkt"
          "types.rkt"
          "utils.rkt"
         "window.rkt")

(provide 
 (protect-out panel%
              panel-mixin))

(import-class NSView)

(define-objc-class MyPanelView NSView
  #:mixins (KeyMouseTextResponder CursorDisplayer)
  [wxb])

(define (panel-mixin %)
  (class %
    (inherit register-as-child on-new-child
             is-window-enabled? get-cocoa)

    (define lbl-pos 'horizontal)
    (define children null)

    (super-new)
    
    (define/public (get-label-position) lbl-pos)
    (define/public (set-label-position pos) (set! lbl-pos pos))

    (define/public (adopt-child p)
      ;; in atomic mode
      (send p set-parent this))

    (define/override (fix-dc)
      (for ([child (in-list children)])
        (send child fix-dc)))

    (define/override (hide-children)
      (for ([child (in-list children)])
        (send child hide-children)))

    (define/override (show-children)
      (for ([child (in-list children)])
        (send child show-children)))

    (define/override (fixup-locations-children)
      (for ([child (in-list children)])
        (send child fixup-locations-children)))
    
    (define/override (paint-children)
      (for ([child (in-list children)])
        (send child paint-children)))

    (define/override (children-accept-drag on?)
      (for ([child (in-list children)])
        (send child child-accept-drag on?)))

    (define/override (enable-window on?)
      (let ([on? (and on? (is-window-enabled?))])
        (for ([child (in-list children)])
          (send child enable-window on?))))
    
    (define/override (set-size x y w h)
      (super set-size x y w h)
      (fix-dc))
    
    (define/override (maybe-register-as-child parent on?)
      (register-as-child parent on?))
    
    (define/override (register-child child on?)
      (let ([now-on? (and (memq child children) #t)])
        (unless (eq? on? now-on?)
          (set! children 
                (if on?
                    (cons child children)
                    (remq child children)))
          (on-new-child child on?))))

    (define/override (show on?)
      (super show on?)
      (fix-dc))
    
    (define/public (set-item-cursor x y) (void))))

(defclass panel% (panel-mixin window%)
  (init parent
        x y w h
        style
        label)
  (super-new [parent parent]
             [cocoa
              (as-objc-allocation
               (tell (tell MyPanelView alloc)
                     initWithFrame: #:type _NSRect (make-NSRect (make-init-point x y)
                                                                (make-NSSize (max 1 w) (max 1 h)))))]
             [no-show? (memq 'deleted style)]))
