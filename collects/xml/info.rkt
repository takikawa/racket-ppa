#lang setup/infotab

;; the XML tool has been moved to the stepper collection, so that the
;; stepper can create xml snips.  See collects/stepper/tool.ss for (a
;; bit) more information
(define tools '(("text-box-tool.rkt")))
(define tool-names '("Text Box"))

(define scribblings '(("xml.scrbl" () (parsing-library))))
