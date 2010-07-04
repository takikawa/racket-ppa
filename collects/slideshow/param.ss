
(module param mzscheme
  (require (lib "unit.ss")
	   "sig.ss"
	   "cmdline.ss"
	   "viewer.ss")

  (provide current-slideshow-linker)

  (define current-slideshow-linker 
    (make-parameter 
     (lambda (core@)
       (compound-unit
	(import)
	(export CORE CMDLINE VIEWER)
	(link [((CONFIG : config^) (CMDLINE : cmdline^)) cmdline@]
	      [((CORE : core^)) core@ CMDLINE VIEWER]
	      [((VIEWER : viewer^)) viewer@ CMDLINE CORE]))))))
