#lang setup/infotab

(define scribblings
  '(("start.scrbl"
     (main-doc-root always-run depends-all-main no-depend-on) (omit))
    ("search.scrbl"       (depends-all-main no-depend-on) (omit))
    ("master-index.scrbl" (depends-all-main no-depend-on) (omit))
    ("getting-started.scrbl" () (omit))
    ("license.scrbl" () (omit))
    ("acks.scrbl"    () (omit))
    ("release.scrbl" () (omit))))
