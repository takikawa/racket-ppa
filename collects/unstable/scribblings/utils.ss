#lang at-exp scheme/base
(require scribble/base scribble/manual scribble/core)
(provide unstable
         unstable-header
         addition)

(define (unstable . authors)
  (make-compound-paragraph 
   plain
   (list (apply author authors)
         (unstable-header))))

(define (unstable-header)
  @para{This library is @emph{unstable};
        compatibility will not be maintained.
        See @secref{unstable} for more information.})

(define (addition name)
  @margin-note{The subsequent bindings were added by @|name|.})
