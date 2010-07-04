#lang scribble/manual
@(require scribble/eval
          (for-label unstable/struct
                     scheme/contract
                     scheme/base))

@title[#:tag "struct"]{Structs}

@(define the-eval (make-base-eval))
@(the-eval '(require unstable/struct))

@defmodule[unstable/struct]

@defform[(make struct-id expr ...)]{

Creates an instance of @scheme[struct-id], which must be bound as a
struct name. The number of @scheme[expr]s is statically checked
against the number of fields associated with @scheme[struct-id]. If
they are different, or if the number of fields is not known, an error
is raised at compile time.

@examples[#:eval the-eval
  (define-struct triple (a b c))
  (make triple 3 4 5)
  (make triple 2 4)
]
}

@defproc[(struct->list [v any/c]
                       [#:on-opaque on-opaque (or/c 'error 'return-false 'skip) 'error])
         (or/c list? #f)]{

Returns a list containing the struct instance @scheme[v]'s
fields. Unlike @scheme[struct->vector], the struct name itself is not
included.

If any fields of @scheme[v] are inaccessible via the current inspector
the behavior of @scheme[struct->list] is determined by
@scheme[on-opaque]. If @scheme[on-opaque] is @scheme['error] (the
default), an error is raised. If it is @scheme['return-false],
@scheme[struct->list] returns @scheme[#f]. If it is @scheme['skip],
the inaccessible fields are omitted from the list.

@examples[#:eval the-eval
(define-struct open (u v) #:transparent)
(struct->list (make-open 'a 'b))
(struct->list #s(pre 1 2 3))
(define-struct (secret open) (x y))
(struct->list (make-secret 0 1 17 22))
(struct->list (make-secret 0 1 17 22) #:on-opaque 'return-false)
(struct->list (make-secret 0 1 17 22) #:on-opaque 'skip)
(struct->list 'not-a-struct #:on-opaque 'return-false)
(struct->list 'not-a-struct #:on-opaque 'skip)
]
}
