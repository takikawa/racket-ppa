#lang scribble/doc
@(require scribblings/htdp-langs/common "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/sdp/vanilla
                     (only-in deinprogramm/sdp/beginner define)))

@title[#:style 'toc #:tag "sdp-vanilla"]{Schreibe Dein Programm!}

This is documentation for the language level @italic{Schreibe Dein Programm!}
to go with the German textbooks
@italic{Schreibe Dein Programm!}.

@declare-exporting[deinprogramm/sdp/vanilla #:use-sources (deinprogramm/sdp/private/primitives)]

@racketgrammar*-sdp[
#:literals ()
() ()
(
  @#,racket[(let ((id expr) (... ...)) expr)]
  @#,racket[(letrec ((id expr) (... ...)) expr)]
  @#,racket[(let* ((id expr) (... ...)) expr) ]
)
(
  @#,racket[(list-of sig)]
  @#,racket[(nonempty-list-of sig)]
)
(
  @#,racket[empty]
  @#,racket[(make-pair pattern pattern)]
  @#,racket[(list pattern ...)]
)
]

@|prim-nonterms|

@prim-ops['(lib "vanilla.rkt" "deinprogramm" "sdp") #'here]

@section[#:tag "signatures-vanilla"]{Signaturen}

@defidform[empty-list]{
Signatur für die leere Liste.
}

@defform[(list-of sig)]{
Diese Signatur ist dann für einen Wert gültig, wenn dieser eine Liste ist,
für dessen Elemente @racket[sig] gültig ist.
}

@defform[(nonempty-list-of sig)]{
Diese Signatur ist dann für einen Wert gültig, wenn dieser eine nichtleere Liste ist,
für dessen Elemente @racket[sig] gültig ist.
}

@section{@racket[let], @racket[letrec] und @racket[let*]}

@defform[(let ((id expr) ...) expr)]{

Bei einem @racket[let]-Ausdruck werden zunächst die @racket[expr]s aus
den @racket[(id expr)]-Paaren ausgewertet. Ihre Werte werden dann im
Rumpf-@racket[expr] für die Namen @racket[id] eingesetzt. Dabei können
sich die Ausdrücke nicht auf die Namen beziehen.

@racketblock[
(define a 3)
(let ((a 16)
      (b a))
  (+ b a))
=> 19]

Das Vorkommen von @racket[a] in der Bindung von @racket[b] bezieht
sich also auf das @racket[a] aus der Definition, nicht das @racket[a]
aus dem @racket[let]-Ausdruck.
}

@defform[(letrec ((id expr) ...) expr)]{
Ein @racket[letrec]-Ausdruck ist
ähnlich zum entsprechenden @racket[let]-Ausdruck, mit dem Unterschied, daß sich
die @racket[expr]s aus den Bindungen auf die gebundenen Namen beziehen
dürfen.}

@defform[(let* ((id expr) ...) expr)]{
Ein @racket[let*]-Ausdruck ist ähnlich zum entsprechenden
@racket[let]-Ausdruck, mit dem Unterschied, daß sich die @racket[expr]s
aus den Bindungen auf die Namen beziehen dürfen, die jeweils vor dem
@racket[expr] gebunden wurden. Beispiel:

@racketblock[
(define a 3)
(let* ((a 16)
       (b a))
  (+ b a))
=> 32]

Das Vorkommen von @racket[a] in der Bindung von @racket[b] bezieht
sich also auf das @racket[a] aus dem @racket[let*]-Ausdruck, nicht das
@racket[a] aus der globalen Definition.
}

@section[#:tag "pattern-matching-vanilla"]{Pattern-Matching}

@defform/none[(match expr (pattern expr) ...)
		#:grammar [(pattern
		                ...
				empty
				(make-pair pattern pattern)
				(list pattern ...)
				)]]{
Zu den Patterns aus der "Anfänger"-Sprache kommen noch drei neue hinzu:

@itemlist[
@item{Das Pattern @racket[empty] paßt auf die leere Liste.}

@item{Das Pattern @racket[(make-pair pattern pattern)] paßt auf Paare, bei
  denen die beiden inneren Patterns auf @racket[first] bzw. @racket[rest] passen.}

@item{Das Pattern [(list pattern ...)] paßt auf Listen, die genauso
viele Elemente haben, wie Teil-Patterns im @racket[list]-Pattern
stehen und bei denen die inneren Patterns auf die Listenelemente
passen.
}
]
}

@section[#:tag "vanilla-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "vanilla.rkt" "deinprogramm" "sdp") #'here '()]
