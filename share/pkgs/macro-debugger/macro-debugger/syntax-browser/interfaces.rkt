#lang racket/base
(require racket/class/iop
         (only-in macro-debugger/syntax-browser/partition partition<%>)
         (for-syntax racket/base racket/syntax))
(provide partition<%>
         (all-defined-out))

(define-interface-expander methods:notify
  (lambda (stx)
    (syntax-case stx ()
      [(_ name ...)
       (with-syntax ([((method-name ...) ...)
                      (for/list ([name (syntax->list #'(name ...))])
                        (list (format-id name "get-~a" name)
                              (format-id name "set-~a" name)
                              (format-id name "listen-~a" name)))])
         #'(method-name ... ...))])))

;; Interfaces

;; config<%>
(define-interface config<%> ()
  ((methods:notify suffix-option
                   taint-icons
                   syntax-font-size
                   colors
                   width
                   height
                   props-percentage
                   props-shown?)))

;; displays-manager<%>
(define-interface displays-manager<%> ()
  (;; add-syntax-display : display<%> -> void
   add-syntax-display

   ;; remove-all-syntax-displays : -> void
   remove-all-syntax-displays

   ;; refresh-all-displays : -> void
   refresh-all-displays))

;; selection-manager<%>
(define-interface selection-manager<%> ()
  (;; selected-syntax : notify-box of syntax/#f
   (methods:notify selected-syntax)))

;; relation<%>
(define-interface relation<%> ()
  (;; identifier=? : notify-box of (U #f (id id -> bool))
   (methods:notify identifier=?)
   ;; primary-partition-factory : notify-box of (-> partition%)
   ;; primary-partition : notify-box of partition%
   (methods:notify primary-partition-factory)
   (methods:notify primary-partition)
   reset-primary-partition))

;; controller<%>
(define-interface controller<%> (displays-manager<%>
                                 selection-manager<%>
                                 relation<%>)
  ())


;; host<%>
(define-interface host<%> ()
  (;; get-controller : -> controller<%>
   get-controller

   ;; add-keymap : text snip
   add-keymap))

;; keymap/popup<%>
(define-interface keymap/popup<%> ()
  (;; add-context-menu-items : popup-menu -> void
   add-context-menu-items))

;; display<%>
(define-interface display<%> ()
  (;; refresh : -> void
   refresh

   ;; highlight-syntaxes : (list-of syntax) color -> void
   highlight-syntaxes

   ;; underline-syntaxes : (listof syntax) -> void
   underline-syntaxes

   ;; get-start-position : -> number
   get-start-position

   ;; get-end-position : -> number
   get-end-position

   ;; get-range : -> range<%>
   get-range))

;; range<%>
(define-interface range<%> ()
  (;; get-ranges : Syntax -> (listof Range)
   get-ranges

   ;; get-treeranges : -> (listof TreeRange)
   get-treeranges

   ;; all-ranges : (list-of Range)
   ;; Sorted outermost-first
   all-ranges

   ;; get-identifier-list : (list-of identifier)
   get-identifier-list))


;; A Range is (range Syntax Nat Nat Nat)
(struct range (stx start pstart end))

;; A TreeRange is (treerange Syntax Nat Nat (Listof TreeRange))
;; where subs are disjoint, in order, and all contained within [start, end]
(struct treerange (stx start end subs))

;; syntax-prefs<%>
(define-interface syntax-prefs<%> ()
  (pref:width
   pref:height
   pref:props-percentage
   pref:props-shown?))

;; widget-hooks<%>
(define-interface widget-hooks<%> ()
  (;; setup-keymap : -> void
   setup-keymap

   ;; shutdown : -> void
   shutdown))

;; keymap-hooks<%>
(define-interface keymap-hooks<%> ()
  (;; make-context-menu : -> context-menu<%>
   make-context-menu

   ;; get-context-menu% : -> class
   get-context-menu%))

;; context-menu-hooks<%>
(define-interface context-menu-hooks<%> ()
  (add-edit-items
   after-edit-items
   add-selection-items
   after-selection-items
   add-partition-items
   after-partition-items))


;;----------

;; Convenience widget, specialized for displaying stx and not much else
(define-interface syntax-browser<%> ()
  (add-syntax
   add-text
   add-error-text
   add-clickback
   add-separator
   erase-all
   get-controller
   get-text))
