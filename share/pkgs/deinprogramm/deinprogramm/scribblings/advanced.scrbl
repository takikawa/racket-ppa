#lang scribble/doc
@(require scribblings/htdp-langs/common "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/sdp/advanced))

@title[#:style 'toc #:tag "sdp-advanced"]{Schreibe Dein Programm! - fortgeschritten}

This is documentation for the language level @italic{Schreibe Dein Programm
- fortgeschritten} that goes with the German textbook
@italic{Schreibe Dein Programm!}.

@declare-exporting[deinprogramm/sdp/advanced]

@racketgrammar*-sdp[
#:literals ()
(
)
(
  [field-spec id (id id)]
  [quoted id
          number
          string
          character
	  symbol
          (quoted ...)
          @#,elem{@racketvalfont{'}@racket[quoted]}]
)
(
  @#,racket[(let ((id expr) (... ...)) expr)]
  @#,racket[(letrec ((id expr) (... ...)) expr)]
  @#,racket[(let* ((id expr) (... ...)) expr) ]
  quoted
  (code:line @#,elem{@racketvalfont{'}@racket[quoted]} (code:comment @#,seclink["advanced-quote"]{Quote-Literal}))
)
(
  @#,racket[(list-of sig)]
  @#,racket[(nonempty-list-of sig)]
)
(
 @#,racket[(make-pair pattern pattern)]
 @#,racket[(list pattern ...)]
 @#,elem{@racketvalfont{'}@racket[quoted]}
)
]

@|prim-nonterms|

@prim-ops['(lib "advanced.rkt" "deinprogramm" "sdp") #'here]

@section[#:tag "advanced-quote"]{Quote-Literal}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[quoted]})]
@defform[(quote quoted)]
)]{
Der Wert eines Quote-Literals hat die gleiche externe Repräsentation wie @racket[quoted].
}

@section[#:tag "advanced-signatures"]{Signaturen}

@defidform[symbol]{
Signatur für Symbole.
}

@section[#:tag "pattern-matching-advanced"]{Pattern-Matching}

@defform/none[(match expr (pattern expr) ...)
		#:grammar [(pattern
		                ...
				@#,elem{@racketvalfont{'}@racket[quoted]}
				)]]{
Zu den Patterns kommt noch eins hinzu:

@itemlist[
@item{Das Pattern @racketvalfont{'}@racket[quoted] paßt auf genau auf Werte, welche
die gleiche externe Repräsentation wie @racket[quoted] haben.}
]
}

@section[#:tag "advanced-definitions"]{Definitionen}
@declare-exporting[deinprogramm/sdp/deflam]

@defform[(define id expr)]{Diese Form ist wie in den unteren
Sprachebenen.}

@section[#:tag "advanced-lambda"]{@racket[lambda] / @racket[λ]}
@declare-exporting[deinprogramm/sdp/deflam]

@defform[(lambda (id id ... . id) expr)]{
Bei @racket[lambda] ist in
dieser Sprachebene in einer Form zulässig, die es erlaubt, eine
Prozedur mit einer variablen Anzahl von Paramern zu erzeugen: Alle
Parameter vor dem Punkt funktionieren wie gewohnt und werden jeweils
an die entsprechenden Argumente gebunden.  Alle restlichen Argumente
werden in eine Liste verpackt und an den Parameter nach dem Punkt
gebunden.}

@defform[(λ (id id ... . id) expr)]{
@racket[λ] ist ein anderer Name für @racket[lambda].
}

@section[#:tag "advanced-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "advanced.rkt" "deinprogramm" "sdp") #'here '()]
