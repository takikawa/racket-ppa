(module mrtextfield mzscheme
  (require mzlib/class
	   mzlib/class100
	   mzlib/list
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "check.ss"
	   "helper.ss"
	   "wx.ss"
	   "kw.ss"
	   "wxtextfield.ss"
	   "mrcontainer.ss"
	   "mritem.ss"
	   "mrmenu.ss"
	   "mrpopup.ss")

  (provide text-field%
	   combo-field%)

  (define combo-flag (gensym))

  (define (check-text-field-args cwho
				 label
				 choices? choices
				 parent
				 callback
				 init-value
				 style req-styles
				 font)
    (check-label-string/false cwho label)
    (when choices?
      (unless (and (list? choices) (andmap label-string? choices))
	(raise-type-error (who->name cwho) "list of strings (up to 200 characters)" choices)))
    (check-container-parent cwho parent)
    (check-callback cwho callback)
    (check-string cwho init-value)
    (check-style cwho 
		 req-styles 
		 (append
		  (if choices? null '(hscroll password))
		  '(vertical-label horizontal-label deleted))
		 (remq combo-flag style))
    (check-font cwho font))

  (define text-field%
    (class100*/kw basic-control% () 
		  [(label parent [callback (lambda (b e) (void))] [init-value ""] [style '(single)])
		   control%-keywords]
      (sequence 
	(check-text-field-args '(constructor text-field)
			       label 
			       #f #f
			       parent callback init-value
			       style '(single multiple)
			       font))
      (private-field
       [wx #f])
      (public
        [set-field-background (lambda (c)
                                (check-instance '(method text-field% set-field-color) 
                                                wx:color% 'color% #f c)
                                (send wx set-field-background c))]
        [get-field-background (lambda () (send wx get-field-background))]
	[get-editor (entry-point (lambda () (send wx get-editor)))]
	[get-value (lambda () (send wx get-value))] ; note: wx method doesn't expect as-entry
	[set-value (entry-point 
		    (lambda (v) 
		      (check-string '(method text-field% set-value) v)
		      (send wx set-value v)))])
      (sequence
	;; Technically a bad way to change margin defaults, since it's
	;;  implemented with an update after creation:
	(when (eq? horiz-margin no-val) (set! horiz-margin 2))
	(when (eq? vert-margin no-val) (set! vert-margin 2))
	(as-entry
	 (lambda ()
	   (super-init (lambda () 
			 (set! wx (make-object wx-text-field% this this
					       (mred->wx-container parent) (wrap-callback callback)
					       label init-value 
					       (if (memq combo-flag style)
						   (cons 'combo (remq combo-flag style))
						   style)
					       (no-val->#f font)))
			 wx)
		       (lambda ()
			 (let ([cwho '(constructor text-field)])
			   (check-container-ready cwho parent)))
		       label parent callback ibeam))))))

  (define combo-field%
    (class100*/kw text-field% () 
		  [(label choices parent [callback (lambda (b e) (void))] [init-value ""] [style '()])
		   control%-keywords]
      (inherit set-value popup-menu get-size focus get-editor)
      (sequence 
	(check-text-field-args '(constructor combo-field)
			       label 
			       #f choices
			       parent callback init-value
			       style #f 
			       font))
      (private
        [prep-popup
         (lambda ()
           (send menu on-demand)
           (let ([items (send menu get-items)]
                 [wx (mred->wx this)])
             (send wx clear-combo-items)
             (for-each 
              (lambda (item)
                (unless (item . is-a? . separator-menu-item%)
                  (send wx append-combo-item
                        (send item get-plain-label)
                        (lambda ()
                          (send item command 
                                (make-object wx:control-event% 'menu-popdown))))))
              items)))])
      (public
	[on-popup (lambda (e) (void))]
	[get-menu (lambda () menu)]
	[append (lambda (item)
		  (check-label-string '(method combo-field% append) item)
                  (make-object menu-item% item menu 
                               (lambda (i e)
                                 (handle-selected item))))])
      (private
        [handle-selected (lambda (item)
                           (focus)
                           (set-value item)
                           (let ([e (get-editor)])
                             (send e set-position 0 (send e last-position)))
                           (send (as-entry (lambda () (mred->wx this)))
                                 command
                                 (make-object wx:control-event% 'text-field)))])
      (private-field
       [menu (new popup-menu% [font font])])
      (sequence
	(super-init label parent callback init-value (list* combo-flag 'single style))
        (send (mred->wx this) 
              set-on-popup
              (lambda ()
                (on-popup (make-object wx:control-event% 'menu-popdown))
                (prep-popup)))
	(for-each (lambda (item) (append item))
		  choices)))))
