#reader(lib "docreader.ss" "scribble")
@require[(lib "struct.ss" "scribble")]
@require["mz.ss"]

@define[(cont n) (make-element "schemevariable"
                                (list "C" (make-element 'subscript (list (format "~a" n)))))]

@title[#:tag "contmarks"]{Continuation Marks}

See @secref["mark-model"] and @secref["prompt-model"] for
general information about continuation marks.

The list of continuation marks for a key @scheme[_k] and a continuation
@scheme[_C] that extends @cont[0] is defined as follows:

@itemize{

 @item{If @scheme[_C] is an empty continuation, then the mark list is
 @scheme[null].}

 @item{If @scheme[_C]'s first frame contains a mark @scheme[_m] for @scheme[_k],
 then the mark list for @scheme[_C] is @scheme[(cons @scheme[_m] _lst)],
 where @scheme[_lst] is the mark list for @scheme[_k] in @cont[0].}

 @item{If @scheme[_C]'s first frame does not contain a mark keyed by
 @scheme[_k], then the mark list for @scheme[_C] is the mark list for
 @cont[0].}

}

The @scheme[with-continuation-mark] form installs a mark on the first
frame of the current continuation (see @secref["wcm"]).  Procedures
such as @scheme[current-continuation-marks] allow inspection of marks.

Whenever Scheme creates an exception record for a primitive exception,
it fills the @scheme[continuation-marks] field with the value of
@scheme[(current-continuation-marks)], thus providing a snapshot of
the continuation marks at the time of the exception.

When a continuation procedure returned by
@scheme[call-with-current-continuation] or
@scheme[call-with-composable-continuation] is invoked, it restores the
captured continuation, and also restores the marks in the
continuation's frames to the marks that were present when
@scheme[call-with-current-continuation] or
@scheme[call-with-composable-continuation] was invoked.

@defproc[(continuation-marks [cont continuation?]
                             [prompt-tag prompt-tag? (default-continuation-prompt-tag)])
         continuation-mark-set?]{

Returns an opaque value containing the set of continuation marks for
all keys in the continuation @scheme[cont] up to the prompt tagged by
@scheme[prompt-tag].  If @scheme[cont] is an escape continuation (see
@secref["prompt-model"]), then the current continuation must extend
@scheme[cont], or the @exnraise[exn:fail:contract]. If @scheme[cont]
was not captured with respect to @scheme[prompt-tag] and does not
include a prompt for @scheme[prompt-tag], the
@exnraise[exn:fail:contract].}

@defproc[(current-continuation-marks [prompt-tag prompt-tag? (default-continuation-prompt-tag)])
         continuation-mark-set?]{

Returns an opaque value containing the set of continuation marks for
all keys in the current continuation up to @scheme[prompt-tag]. In
other words, it produces the same value as

@schemeblock[
(call-with-current-continuation
  (lambda (k) 
    (continuation-marks k prompt-tag))
  prompt-tag)
]}

@defproc[(continuation-mark-set->list 
          [mark-set continuation-mark-set?]
          [key-v any/c]
          [prompt-tag prompt-tag? (default-continuation-prompt-tag)])
         list?]{
Returns a newly-created list containing the marks for @scheme[key-v]
in @scheme[mark-set], which is a set of marks returned by
@scheme[current-continuation-marks]. The result list is truncated at
the first point, if any, where continuation frames were originally
separated by a prompt tagged with @scheme[prompt-tag]..}

@defproc[(continuation-mark-set->list* 
          [mark-set continuation-mark-set?]
          [key-v any/c]
          [none-v any/c #f]
          [prompt-tag prompt-tag? (default-continuation-prompt-tag)])
         (listof vector?)]{
Returns a newly-created list containing vectors of marks in
@scheme[mark-set] for the keys in @scheme[key-list], up to
@scheme[prompt-tag]. The length of each vector in the result list is
the same as the length of @scheme[key-list], and a value in a
particular vector position is the value for the corresponding key in
@scheme[key-list]. Values for multiple keys appear in a single vector
only when the marks are for the same continuation frame in
@scheme[mark-set]. The @scheme[none-v] argument is used for vector
elements to indicate the lack of a value.}

@defproc[(continuation-mark-set-first 
          [mark-set (or/c continuation-mark-set? false/c)]
          [key-v any/c]
          [prompt-tag prompt-tag? (default-continuation-prompt-tag)])
         any]{
Returns the first element of the list that would be returned by
@scheme[(continuation-mark-set->list (or mark-set
(current-continuation-marks prompt-tag)) key-v prompt-tag)], or
@scheme[#f] if the result would be the empty list. Typically, this
result can be computed more quickly using
@scheme[continuation-mark-set-first].}

@defproc[(continuation-mark-set? [v any/c]) boolean?]{
Returns @scheme[#t] if @scheme[v] is a mark set created by
@scheme[continuation-marks] or @scheme[current-continuation-marks],
@scheme[#f] otherwise.}

@defproc[(continuation-mark-set->context
          [mark-set continuation-mark-set?])
          list?]{

Returns a list representing a ``@index["stack dump"]{@as-index{stack
trace}}'' for @scheme[mark-set]'s continuation. The list contains
pairs, where the @scheme[car] of each pair contains either @scheme[#f]
or a symbol for a procedure name, and the @scheme[cdr] of each pair
contains either @scheme[#f] or a @scheme[srcloc] value for the
procedure's source location (see @secref["linecol"]); the
@scheme[car] and @scheme[cdr] are never both @scheme[#f].

The stack-trace list is the result of
@scheme[continuation-mark-set->list] with @scheme[mark-set] and
Scheme's private key for procedure-call marks. A stack trace is
extracted from an exception and displayed by the default error display
handler (see @scheme[current-error-display-handler]) for exceptions other than
@scheme[exn:fail:user] (see @scheme[raise-user-error] in
@secref["errorproc"]).}

@examples[
(define (extract-current-continuation-marks key) 
   (continuation-mark-set->list 
    (current-continuation-marks) 
    key)) 

(with-continuation-mark 'key 'mark 
  (extract-current-continuation-marks 'key))

(with-continuation-mark 'key1 'mark1 
  (with-continuation-mark 'key2 'mark2 
    (list 
     (extract-current-continuation-marks 'key1) 
     (extract-current-continuation-marks 'key2))))

(with-continuation-mark 'key 'mark1 
  (with-continuation-mark 'key 'mark2 (code:comment @t{replaces the previous mark})
    (extract-current-continuation-marks 'key)))

(with-continuation-mark 'key 'mark1 
  (list (code:comment @t{continuation extended to evaluate the argument})
   (with-continuation-mark 'key 'mark2 
      (extract-current-continuation-marks 'key))))

(let loop ([n 1000])
  (if (zero? n) 
      (extract-current-continuation-marks 'key) 
      (with-continuation-mark 'key n
        (loop (sub1 n)))))
]
