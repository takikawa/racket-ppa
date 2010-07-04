#lang at-exp scheme/base
(require scheme/list)
(provide (all-defined-out))
(define (itemize . items)
  (add-between (map (lambda (item)
                      @list{* @item})
                    items)
               "\n"))
(define summary
  @list{If that's not enough,
        I don't know what is.})