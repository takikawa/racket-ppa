#lang racket/base
(require racket/class
         racket/class/iop
         macro-debugger/syntax-browser/interfaces
         macro-debugger/syntax-browser/partition
         framework/notify)
(provide controller%)

;; displays-manager-mixin
(define displays-manager-mixin
  (mixin () (displays-manager<%>)
    (super-new)
    ;; displays : (list-of display<%>)
    (field [displays null])

    ;; add-syntax-display : display<%> -> void
    (define/public (add-syntax-display c)
      (set! displays (cons c displays)))

    ;; remove-all-syntax-displays : -> void
    (define/public (remove-all-syntax-displays)
      (set! displays null))

    ;; refresh-all-displays : -> void
    (define/public (refresh-all-displays)
      (for ([d (in-list displays)]) (send/i d display<%> refresh)))))

;; selection-manager-mixin
(define selection-manager-mixin
  (mixin (displays-manager<%>) (selection-manager<%>)
    (inherit refresh-all-displays)
    (super-new)

    (notify:define-notify selected-syntax (new notify:notify-box% (value #f)))

    (listen-selected-syntax
     (lambda (new-value) (refresh-all-displays)))))

;; relation-mixin
(define relation-mixin
  (mixin (displays-manager<%>) (relation<%>)
    (inherit refresh-all-displays)
    (super-new)

    (notify:define-notify primary-partition-factory
      (new notify:notify-box% (value new-macro-scopes-partition)))
    (notify:define-notify primary-partition
      (new notify:notify-box% (value ((get-primary-partition-factory)))))
    (notify:define-notify identifier=?
      (new notify:notify-box% (value #f)))

    (listen-primary-partition-factory
     (lambda (f) (set-primary-partition (f))))

    ;; (listen-primary-partition ...)
    ;; When primary-partition changes, can't just refresh displays (doesn't
    ;; change fg colors / suffixes); need to instead re-render entire contents.
    ;; So the stepper handles that.

    (listen-identifier=?
     (lambda (proc) (refresh-all-displays)))

    (define/public (reset-primary-partition)
      (set-primary-partition ((get-primary-partition-factory))))
    ))

(define controller%
  (class* (relation-mixin
           (selection-manager-mixin
            (displays-manager-mixin
             object%)))
    (controller<%>)
    (super-new)))
