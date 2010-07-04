#lang scheme
(provide itemize)
(define (itemize . items)
  (add-between (map (lambda (item)
                      (list "* " item))
                    items)
               "\n"))