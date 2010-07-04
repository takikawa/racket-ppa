#lang scribble/doc
@(require "mz.ss")

@title[#:tag "vectors"]{Vectors}

@guideintro["vectors"]{vectors}

A @deftech{vector} is a fixed-length array with constant-time access
and update of the vector slots, which are numbered from @scheme[0] to
one less than the number of slots in the vector.

Two vectors are @scheme[equal?] if they have the same length, and if
the values in corresponding slots of the vectors are
@scheme[equal?].

A vector can be @defterm{mutable} or @defterm{immutable}. When an
immutable vector is provided to a procedure like @scheme[vector-set!],
the @exnraise[exn:fail:contract]. Vectors generated by the default
reader (see @secref["parse-string"]) are immutable.

A vector can be used as a single-valued sequence (see
@secref["sequences"]). The elements of the vector serve as elements
of the sequence. See also @scheme[in-vector].

@defproc[(vector? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a vector, @scheme[#f] otherwise.}


@defproc[(make-vector [size exact-nonnegative-integer?]
                      [v any/c 0]) vector?]{

Returns a mutable vector with @scheme[size] slots, where all slots are
initialized to contain @scheme[v].}


@defproc[(vector [v any/c] ...) vector?]{

Returns a mutable vector with as many slots as provided @scheme[v]s,
where the slots are initialized to contain the given @scheme[v]s in
order.}


@defproc[(vector-immutable [v any/c] ...) (and/c vector?
                                                 immutable?)]{

Returns an immutable vector with as many slots as provided
@scheme[v]s, where the slots are contain the given @scheme[v]s in
order.}



@defproc[(vector-length [vec vector?]) exact-nonnegative-integer?]{

Returns the length of @scheme[vec] (i.e., the number of slots in the
vector).}

@defproc[(vector-ref [vec vector?][pos exact-nonnegative-integer?]) any/c]{

Returns the element in slot @scheme[pos] of @scheme[vec]. The first
slot is position @scheme[0], and the last slot is one less than
@scheme[(vector-length vec)].}

@defproc[(vector-set! [vec (and/c vector? (not/c immutable?))]
                      [pos exact-nonnegative-integer?]
                      [v any/c])
         void?]{

Updates the slot @scheme[pos] of @scheme[vec] to contain @scheme[v].}


@defproc[(vector->list [vec vector?])
         list?]{

Returns a list with the same length and elements as @scheme[vec].}


@defproc[(list->vector [lst list?])
         vector?]{

Returns a mutable vector with the same length and elements as
@scheme[lst].}


@defproc[(vector->immutable-vector [vec vector?])
         (and/c vector? immutable?)]{

Returns an immutable vector with the same length and elements as @scheme[vec].
If @scheme[vec] is itself immutable, then it is returned as the result.}


@defproc[(vector-fill! [vec (and/c vector? (not/c immutable?))]
                       [v any/c])
         void?]{

Changes all slots of @scheme[vec] to contain @scheme[v].}


@defproc[(vector-copy! [dest (and/c vector? (not/c immutable?))]
                       [dest-start exact-nonnegative-integer?]
                       [src vector?]
                       [src-start exact-nonnegative-integer? 0]
                       [src-end exact-nonnegative-integer? (vector-length src)])
         void?]{

 Changes the elements of @scheme[dest] starting at position
 @scheme[dest-start] to match the elements in @scheme[src] from
 @scheme[src-start] (inclusive) to @scheme[src-end] (exclusive). The
 vectors @scheme[dest] and @scheme[src] can be the same vector, and in
 that case the destination region can overlap with the source region;
 the destination elements after the copy match the source elements
 from before the copy. If any of @scheme[dest-start],
 @scheme[src-start], or @scheme[src-end] are out of range (taking into
 account the sizes of the vectors and the source and destination
 regions), the @exnraise[exn:fail:contract].

@examples[(define v (vector 'A 'p 'p 'l 'e))
          (vector-copy! v 4 #(y))
          (vector-copy! v 0 v 3 4)
          v]}


@defproc[(vector->values [vec vector?]
                         [start-pos exact-nonnegative-integer? 0]
                         [end-pos exact-nonnegative-integer? (vector-length vec)])
         any]{

Returns @math{@scheme[end-pos] - @scheme[start-pos]} values, which are
the elements of @scheme[vec] from @scheme[start-pos] (inclusive) to
@scheme[end-pos] (exclusive). If @scheme[start-pos] or
@scheme[end-pos] are greater than @scheme[(vector-length vec)], or if
@scheme[end-pos] is less than @scheme[start-pos], the
@exnraise[exn:fail:contract].}

@defproc[(build-vector [n exact-nonnegative-integer?]
                       [proc (exact-nonnegative-integer? . -> . any/c)])
         vector?]{

Creates a vector of @scheme[n] elements by applying @scheme[proc] to
the integers from @scheme[0] to @scheme[(sub1 n)] in order. If
@scheme[_vec] is the resulting vector, then @scheme[(vector-ref _vec
_i)] is the value produced by @scheme[(proc _i)].

@examples[
(build-vector 5 add1)
]}

