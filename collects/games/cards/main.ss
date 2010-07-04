
(module main mzscheme
  (require (lib "class.ss")
	   (lib "etc.ss")
	   "make-cards.ss"
	   "classes.ss"
           "card-class.ss")

  (provide make-table make-deck make-card
           table<%> card<%>)

  (define table<%> (class->interface table%))
  (define card<%> (class->interface card%))
  
  (define make-table
    (opt-lambda ([title "Cards"][w 7][h 3])
      (make-object table% title w h)))

  (define (make-deck)
    (map (lambda (l) (send l copy)) deck-of-cards)))
