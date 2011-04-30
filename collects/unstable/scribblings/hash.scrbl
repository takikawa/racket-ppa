#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label scheme unstable/hash))

@title{Hash Tables}

@defmodule[unstable/hash]

@unstable[@author+email["Carl Eastlund" "cce@racket-lang.org"]]

This module provides tools for manipulating hash tables.

@defproc[(hash-union [h0 (and/c hash? hash-can-functional-set?)]
                     [h hash?] ...
                     [#:combine combine
                                (-> any/c any/c any/c)
                                (lambda _ (error 'hash-union ...))]
                     [#:combine/key combine/key
                                    (-> any/c any/c any/c any/c)
                                    (lambda (k a b) (combine a b))])
         (and/c hash? hash-can-functional-set?)]{

Computes the union of @scheme[h0] with each hash table @scheme[h] by functional
update, adding each element of each @scheme[h] to @scheme[h0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'unstable/hash)
(hash-union (make-immutable-hash '([1 . one])) (make-immutable-hash '([2 . two])) (make-immutable-hash '([3 . three])))
(hash-union (make-immutable-hash '([1 . (one uno)] [2 . (two dos)]))
            (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
            #:combine/key (lambda (k v1 v2) (append v1 v2)))
]

}

@defproc[(hash-union! [h0 (and/c hash? hash-mutable?)]
                      [h hash?] ...
                      [#:combine combine
                                 (-> any/c any/c any/c)
                                 (lambda _ (error 'hash-union ...))]
                      [#:combine/key combine/key
                                     (-> any/c any/c any/c any/c)
                                     (lambda (k a b) (combine a b))])
         void?]{

Computes the union of @scheme[h0] with each hash table @scheme[h] by mutable
update, adding each element of each @scheme[h] to @scheme[h0] in turn.  For each
key @scheme[k] and value @scheme[v], if a mapping from @scheme[k] to some value
@scheme[v0] already exists, it is replaced with a mapping from @scheme[k] to
@scheme[(combine/key k v0 v)].

@defexamples[
#:eval (eval/require 'unstable/hash)
(define h (make-hash))
h
(hash-union! h (make-immutable-hash '([1 . (one uno)] [2 . (two dos)])))
h
(hash-union! h
             (make-immutable-hash '([1 . (ein une)] [2 . (zwei deux)]))
             #:combine/key (lambda (k v1 v2) (append v1 v2)))
h
]

}
