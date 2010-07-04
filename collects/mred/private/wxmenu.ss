(module wxmenu mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "helper.ss"
	   "wx.ss")
  
  (provide (protect wx-menu-item%
		    wx-menu-bar%
		    wx-menu%))

  (define wx-menu-item%
    (class100* wx:menu-item% (wx<%>) (mr mn-dat can-enable?)
      (private-field 
       [menu-data mn-dat]
       [mred mr]
       [keymap #f]
       [wx-menu #f]
       [enabled? #t]
       [ever-enabled? can-enable?])
      (public
	[get-keymap (lambda () keymap)]
	[set-keymap (lambda (k) (set! keymap k))]
	[swap-keymap (lambda (parent k) 
		       (send (send (mred->wx parent) get-container) swap-item-keymap keymap k) 
		       (set-keymap k))]
	[get-mred (lambda () mred)]
	[get-menu-data (lambda () menu-data)]  ; for meta-shortcuts
	[get-container (lambda () wx-menu)]
	[set-wx-menu (lambda (wx) (set! wx-menu wx))]
	[is-enabled? (lambda () enabled?)]
	[set-enabled (lambda (on?) (set! enabled? on?))]
	[ignore-enabled? (lambda () (not ever-enabled?))])
      (sequence
	(super-init))))

  (define wx-menu-bar%
    (class100* wx:menu-bar% (wx<%>) (mr)
      (inherit delete)
      (rename [super-append append]
	      [super-enable-top enable-top])
      (private-field
       [mred mr]
       [items null]
       [disabled null]
       [disabled? #f]
       [keymap (make-object wx:keymap%)])
      (public
	[get-container (lambda () this)]
	[handle-key (lambda (event) 
		      (as-exit 
		       (lambda () 
			 (or (send keymap handle-key-event this event)
			     (and (menu-shortcut-in-label?)
				  (send event get-meta-down)
				  (char? (send event get-key-code))
				  (let ([c (send event get-key-code)])
				    (and (or (char-alphabetic? c)
					     (char-numeric? c))
					 (let ([re (key-regexp c)])
					   (ormap
					    (lambda (i)
					      (let* ([data (send (mred->wx i) get-menu-data)]
						     [label (car data)]
						     [menu (cdr data)])
						(if (regexp-match re label)
						    (begin
						      (send menu select this)
						      #t)
						    #f)))
					    items)))))))))]
	[on-demand (lambda () (as-exit (lambda () (send mred on-demand))))]
	[get-mred (lambda () mred)]
	[get-items (lambda () items)]
	[append-item (lambda (item menu title)
		       (super-append menu title)
		       (when disabled?
			 (super-enable-top (length items) #f))
		       (set! items (append items (list item)))
		       (send keymap chain-to-keymap (send (mred->wx item) get-keymap) #f))]
	[all-enabled? (lambda () (not disabled?))]
	[enable-all (lambda (on?)
		      (set! disabled? (not on?))
		      (let loop ([n (sub1 (length items))])
			(unless (negative? n)
			  (if on?
			      (unless (memq (list-ref items n) disabled)
				(super-enable-top n #t))
			      (super-enable-top n #f))
			  (loop (sub1 n)))))]
	[delete-item (lambda (i)
		       (let ([p (position-of i)])
			 (set! items (remq i items))
			 (set! disabled (remq i disabled))
			 (delete #f p)
			 (send keymap remove-chained-keymap (send (mred->wx i) get-keymap))))]
	[position-of (lambda (i) (find-pos items i eq?))])
      (override
	[enable-top (lambda (p on?)
		      (let ([i (list-ref items p)])
			(if on?
			    (when (memq i disabled)
			      (set! disabled (remq i disabled))
			      (unless disabled?
				(super-enable-top p #t)))
			    (unless (memq i disabled)
			      (set! disabled (cons i disabled))
			      (super-enable-top p #f)))))])
      (sequence
	(super-init))))

  (define wx-menu%
    (class100* wx:menu% (wx<%>) (mr popup-label popup-callback font)
      (private-field
       [mred mr]
       [items null]
       [keymap (make-object wx:keymap%)]
       [popup-grabber #f])
      (inherit delete-by-position)
      (rename [super-delete delete]
	      [super-enable enable])
      (public
	[get-container (lambda () this)]
	[get-keymap (lambda () keymap)]
	[get-mred (lambda () mred)]
	[get-items (lambda () items)]
	[append-item (lambda (i iwx) 
		       (set! items (append items (list i)))
		       (unless (or (send iwx ignore-enabled?)
				   (not (send iwx is-enabled?)))
			 (let ([k (send iwx get-keymap)])
			   (when k
			     (send keymap chain-to-keymap k #f)))))]
	[delete-sep (lambda (i iwx)
		      (delete-by-position (find-pos items i eq?))
		      (set! items (remq i items)))]
	[swap-item-keymap (lambda (old-k new-k)
			    (when old-k (send keymap remove-chained-keymap old-k))
			    (when new-k (send keymap chain-to-keymap new-k #f)))]

	[popup-grab (lambda (c)
		      (if popup-grabber
			  #f
			  (begin
			    (set! popup-grabber c)
			    #t)))]
	[popup-release (lambda () (set! popup-grabber #f))]
	[get-popup-grabber (lambda () popup-grabber)])
      (override
	[delete (lambda (id i) 
		  (super-delete id) 
		  (set! items (remq i items))
		  (let ([k (send (mred->wx i) get-keymap)])
		    (when k
		      (send keymap remove-chained-keymap k))))]
	[enable (lambda (iwx id on?)
		  ;; Only called if the item is not deleted
		  (unless (eq? (send iwx is-enabled?) (and on? #t))
		    (send iwx set-enabled (and on? #t))
		    (super-enable id on?)))])
      (sequence
	(super-init popup-label popup-callback font)))))
