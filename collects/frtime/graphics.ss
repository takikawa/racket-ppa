(module graphics mzscheme
  (require mzlib/unit
	   mred/mred-sig
	   mred
	   "graphics-sig.ss"
	   "graphics-unit.ss")
  (provide-signature-elements graphics:posn^ graphics:posn-less^)

  (define-values/invoke-unit/infer graphics@))
