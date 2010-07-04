
(module option-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "option-sig.ss")

  (provide setup:option@)

  (define setup:option@
    (unit/sig setup-option^
      (import)

      (define verbose (make-parameter #f))
      (define make-verbose (make-parameter #f))
      (define compiler-verbose (make-parameter #f))
      (define clean (make-parameter #f))
      (define compile-mode (make-parameter #f))
      (define make-zo (make-parameter #t))
      (define make-so (make-parameter #f))
      (define make-launchers (make-parameter #t))
      (define make-info-domain (make-parameter #t))
      (define call-install (make-parameter #t))
      (define pause-on-errors (make-parameter #f))
      (define force-unpacks (make-parameter #f))

      (define specific-collections (make-parameter null))
      (define specific-planet-dirs (make-parameter null))
      
      (define archives (make-parameter null))

      (define current-target-directory-getter (make-parameter current-directory))
      (define current-target-plt-directory-getter 
	(make-parameter 
	 (lambda (preferred main-collects-parent-dir choices) preferred))))))
