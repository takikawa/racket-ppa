#lang scribble/doc
@(require scribblings/htdp-langs/common scribble/struct
          "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/sdp/beginner))

@title[#:style 'toc #:tag "sdp-beginner"]{Schreibe Dein Programm! - Anfänger}

This is documentation for the language level @italic{Schreibe Dein Programm!
- Anfänger} to go with the German textbook
@italic{Schreibe Dein Programm!}.

@declare-exporting[deinprogramm/sdp/beginner #:use-sources (deinprogramm/sdp/private/primitives deinprogramm/sdp/record)]

@racketgrammar*-sdp[
#:literals ()
() () () () ()
]

@|prim-nonterms|

@prim-ops['(lib "beginner.rkt" "deinprogramm" "sdp") #'here]

@; ----------------------------------------------------------------------

@section{Definitionen}

@defform[(define id expr)]{
Diese Form ist eine Definition, und bindet @racket[id] als
Namen an den Wert von @racket[expr].
Eine Definition kann ganz außen vorkommen, dann ist sie global und
kann überall verwendet werden.  Eine Definition kann aber auch innerhalb eines
@racket[lambda]-Ausdrucks oder innerhalb von @racket[cond]- und @racket[match]-Zweigen
vorkommen, dann ist sie lokal und nur dort gültig.}


@section{Record-Typ-Definitionen}

@defform*[((define-record type
             constructor
	     (selector signature) ...)
           (define-record type
	     constructor
	     predicate?
	     (selector signature) ...))]{

Die @racket[define-record]-Form ist eine Definition
für einen neuen Record-Typ.  Dabei ist @racket[type] der Name der Record-Signatur,
@racket[constructor] der Name des Konstruktors und @racket[predicate?]
der (optionale) Name des Prädikats.

Jedes @racket[(selector signature)] beschreibt ein @italic{Feld} des
Record-Typs, wobei  @racket[selector]
der Name des Selektors für das Feld und @racket[signature] die Signatur des Feldes ist.
}

@section[#:tag "define-record-with-parameters"]{Record-Typ-Definitionen mit Signatur-Parametern}

@defform*[#:link-target? #f
          ((define-record (type-constructor signature-parameter ...)
              constructor
	      (selector signature) ...)
           (define-record (type-constructor signature-parameter ...)
	      constructor
	      predicate?
	      (selector signature) ...))]{
Diese Variante von @racket[define-record] erlaubt die
Verwendung von Signatur-Parametern: Statt einer konkreten Signatur
@racket[type] wie oben definiert die Form einen Signatur-Konstruktor
@racket[type-constructor], also eine Funktion, die Signaturen als
Argumente akzeptiert, entsprechend den Signatur-Parametern
@racket[signature-parameter].  Diese Signatur-Parameter können in den
Signaturen @racket[signature] der Felder verwendet werden.
}

@section[#:tag "application"]{Funktionsapplikation}

@defform/none[(expr expr ...)]{
Dies ist eine Funktionsanwendung oder -applikation.
Alle @racket[expr]s werden ausgewertet:
Der Operator (also der erste Ausdruck) muss eine
Funktion ergeben, die genauso viele Argumente
akzeptieren kann, wie es Operanden, also weitere @racket[expr]s gibt.
Die Anwendung wird dann ausgewertet, indem der Rumpf
der Applikation ausgewertet wird, nachdem die Parameter
der Funktion durch die Argumente, also die Werte der
Operanden ersetzt wurden.}

@; @defform[(#%app id expr expr ...)]{
@; 
@; Eine Funktionsapplikation kann auch mit @racket[#%app] geschrieben
@; werden, aber das macht eigentlich niemand.}

@section{@racket[#t] and @racket[#f]}

@as-index{@litchar{#t}} ist das Literal für den booleschen Wert "wahr",
@as-index{@litchar{#f}} das Literal für den booleschen Wert "falsch".

@section{@racket[lambda] / @racket[λ]}

@defform[(lambda (id ...) definition ... expr)]{
Ein Lambda-Ausdruck ergibt bei der Auswertung eine Funktion.

Im Rumpf können interne Definitionen vorkommen, die aber nur in
@racket[expr] gelten.}

@defform[(λ (id ...) definition ... expr)]{
@racket[λ] ist ein anderer Name für @racket[lambda].
}

@section[#:tag "id"]{Bezeichner}

@defform/none[id]{
Eine Variable bezieht sich auf die, von innen nach
außen suchend, nächstgelegene Bindung durch @racket[lambda], @racket[let], @racket[letrec], oder
@racket[let*]. Falls es keine solche lokale Bindung gibt, muss es eine
Definition oder eine eingebaute Bindung mit dem entsprechenden Namen
geben. Die Auswertung des Namens ergibt dann den entsprechenden
Wert. }

@section{@racket[cond]}

@defform[(cond (expr definition ... expr) ... (expr definition ... expr))]{
Ein @racket[cond]-Ausdruck bildet eine Verzweigung, die aus mehreren
Zweigen besteht. Jeder Zweig besteht
aus einem Test und einem Ausdruck. Bei der Auswertung werden die
Zweige nacheinander abgearbeitet. Dabei wird jeweils zunächst der Test
ausgewertet, der jeweils einen booleschen Wert ergeben müssen. Beim
ersten Test, der @racket[#t] ergibt, wird der Wert des Ausdrucks des Zweigs zum
Wert der gesamten Verzweigung. Wenn kein Test @racket[#t] ergibt, wird das
Programm mit einer Fehlermeldung abgebrochen.

In einem @racket[cond]-Zweig können lokale Definitionen mit
@racket[define] vorkommen.
}

@defform/none[#:literals (cond else)
              (cond (expr definition ... expr) ... (else definition expr))]{
 Die Form des @racket[cond]-Ausdrucks ist ähnlich zur vorigen, mit der
 Ausnahme, dass in dem Fall, in dem kein Test @racket[#t] ergibt, der Wert des
 letzten Ausdruck zum Wert der @racket[cond]-Form wird.
}

@defidform[else]{

Das Schlüsselwort @racket[else] kann nur in @racket[cond] benutzt werden.}

@; ----------------------------------------------------------------------

@section{@racket[if]}

@defform[(if expr expr expr)]{
Eine @racket[if]-Form ist eine binäre Verzweigung. Bei der Auswertung wird
zunächst der erste Operand ausgewertet (der Test), der einen
booleschen Wert ergeben muss. Ergibt er @racket[#t], wird der Wert des zweiten
Operanden (die Konsequente) zum Wert der @racket[if]-Form, bei @racket[#f] der Wert des
dritten Operanden (die Alternative).
}

@; ----------------------------------------------------------------------

@section{@racket[and]}

@defform[(and expr ...)]{
Bei der Auswertung eines @racket[and]-Ausdrucks werden nacheinander die
Operanden (die boolesche Werte ergeben müssen) ausgewertet. Ergibt
einer @racket[#f], ergibt auch der and-Ausdruck @racket[#f]; wenn alle
Operanden @racket[#t] ergeben, ergibt auch der @racket[and]-Ausdruck
@racket[#t].
}

@; ----------------------------------------------------------------------

@section{@racket[or]}

@defform[(or expr ...)]{
Bei der Auswertung eines @racket[or]-Ausdrucks werden nacheinander die
Operanden (die boolesche Werte ergeben müssen) ausgewertet. Ergibt
einer @racket[#t], ergibt auch der or-Ausdruck @racket[#t]; wenn alle Operanden @racket[#f]
ergeben, ergibt auch der @racket[or]-Ausdruck @racket[#f].
}

@section{Signaturen}

Signaturen können statt der Verträge aus dem Buch geschrieben werden:
Während Verträge reine Kommentare sind, überprüft DrRacket Signaturen 
und meldet etwaige Verletzungen.

@subsection{@racket[signature]}
@defform[(signature sig)]{
Diese Form liefert die Signatur mit der Notation @racket[sig].
}

@subsection{Signaturdeklaration}
@defform[(: id sig)]{
Diese Form erklärt @racket[sig] zur gültigen Signatur für @racket[id].
}

@subsection{Eingebaute Signaturen}

@defidform[number]{
Signatur für beliebige Zahlen.
}

@defidform[real]{
Signatur für reelle Zahlen.
}

@defidform[rational]{
Signatur für rationale Zahlen.
}

@defidform[integer]{
Signatur für ganze Zahlen.
}

@defproc[(integer-from-to [low integer?] [high integer?]) signature?]{
Signatur für ganze Zahlen zwischen @racket[low] und @racket[high].
}

@defidform[natural]{
Signatur für ganze, nichtnegative Zahlen.
}

@defidform[boolean]{
Signatur für boolesche Werte.
}

@defidform[true]{
Signatur für @racket[#t].
}

@defidform[false]{
Signatur für @racket[#f].
}

@defidform[string]{
Signatur für Zeichenketten.
}

@defidform[any]{
Signatur, die auf alle Werte gültig ist.}

@defform/none[signature]{
Signatur für Signaturen.}

@defidform[property]{
Signatur für Eigenschaften.}

@subsection{@racket[predicate]}
@defform[(predicate expr)]{
Bei dieser Signatur muss @racket[expr] als Wert ein Prädikat haben, also
eine Funktion, die einen beliebigen Wert akzeptiert und entweder @racket[#t]
oder @racket[#f] zurückgibt.
Die Signatur ist dann für einen Wert gültig, wenn das Prädikat, darauf angewendet,
@racket[#t] ergibt.
}

@subsection{@racket[enum]}
@defform[(enum expr ...)]{
Diese Signatur ist für einen Wert gültig, wenn er gleich dem Wert eines
der @racket[expr] ist.
}

@subsection{@racket[mixed]}
@defform[(mixed sig ...)]{
Diese Signatur ist für einen Wert gültig, wenn er für eine der Signaturen
@racket[sig] gültig ist.
}

@subsection[#:tag "proc-signature"]{Funktions-Signatur}
@defidform[->]{
@defform/none[(sig ... -> sig)]{
Diese Signatur ist dann für einen Wert gültig, wenn dieser eine
Funktion ist.  Er erklärt außerdem, dass die Signaturen vor dem @racket[->]
für die Argumente der Funktion gelten und die Signatur nach dem @racket[->]
für den Rückgabewert.
}}
}

@subsection[#:tag "signature-variable"]{Signatur-Variablen} 
@defform/none[%a]
@defform/none[%b]
@defform/none[%c]
@defform/none[...]{
Dies ist eine Signaturvariable: sie steht für eine Signatur, die für jeden Wert gültig ist.
}

@subsection{@racket[combined]}
@defform[(combined sig ...)]{
Diese Signatur ist für einen Wert gültig, wenn sie für alle der Signaturen
@racket[sig] gültig ist.
}

@section{Testfälle}

@defform[(check-expect expr expr)]{

Dieser Testfall überprüft, ob der erste @racket[expr] den gleichen
Wert hat wie der zweite @racket[expr], wobei das zweite @racket[expr]
meist ein Literal ist.}

@defform[(check-within expr expr expr)]{

Wie @racket[check-expect], aber mit einem weiteren Ausdruck, 
der als Wert eine Zahl @racket[_delta] hat. Der Testfall überprüft, dass jede Zahl im Resultat
des ersten @racket[expr] maximal um @racket[_delta] 
von der entsprechenden Zahl im zweiten @racket[expr] abweicht.}

@defform[(check-member-of expr expr ...)]{

Ähnlich wie @racket[check-expect]: Der Testfall überprüft, dass das Resultat
des ersten Operanden gleich dem Wert eines der folgenden Operanden ist.}

@defform[(check-satisfied expr pred)]{
Ähnlich wie @racket[check-expect]: Der Testfall überprüft, ob der Wert
des Ausdrucks @racket[expr] vom Prädikat @racket[pred] erfüllt wird -
das bedeutet, dass die Funktion @racket[pred] den Wert @racket[#t]
liefert, wenn sie auf den Wert von @racket[expr] angewendet wird.

Der folgende Test wird also bestanden:
@racketblock[(check-satisfied 1 odd?)]

Der folgende Test hingegen wird hingegen nicht bestanden:

@racketblock[(check-satisfied 1 even?)]
}

@defform[(check-range expr expr expr)]{

Ähnlich wie @racket[check-expect]: Alle drei Operanden müssen 
Zahlen sein.  Der Testfall überprüft, ob die erste Zahl zwischen der
zweiten und der dritten liegt (inklusive).}

@defform[(check-error expr expr)]{

Dieser Testfall überprüft, ob der erste @racket[expr] einen Fehler produziert,
wobei die Fehlermeldung der Zeichenkette entspricht, die der Wert des zweiten
@racket[expr] ist.}

@defform[(check-property expr)]{

Dieser Testfall überprüft experimentell, ob die @tech{Eigenschaft}
@racket[expr] erfüllt ist.  Dazu werden zufällige Werte für die mit
@racket[for-all] quantifizierten Variablen eingesetzt: Damit wird
überprüft, ob die Bedingung gilt.

@emph{Wichtig:} @racket[check-property] funktioniert nur für
Eigenschaften, bei denen aus den Signaturen sinnvoll Werte generiert
werden können.  Dies ist für viele Signaturen der
Fall, aber nicht für solche mit Signaturvariablen.}

@section{Pattern-Matching}

@defform[(match expr (pattern definition ... expr) ...)
		#:grammar [(pattern
		                id
				#t
				#f
				string
				number
				...
				(constructor pattern ...))]
					
]{ Ein @racket[match]- Ausdruck führt eine Verzweigung durch, ähnlich
wie @racket[cond].  Dazu wertet match zunächst einmal den Ausdruck
@racket[expr] nach dem match zum Wert @italic{v} aus.  Es prüft dann
nacheinander jeden Zweig der Form @racket[(pattern expr)] dahingehend,
ob das Pattern @racket[pattern] darin auf den Wert @italic{v} passt
(``matcht'').  Beim ersten passenden Zweig @racket[(pattern expr)]
macht @racket[match] dann mit der Auswertung voh @racket[expr] weiter.

Ob ein Wert @italic{v} passt, hängt von @racket[pattern] ab:

@itemlist[
@item{Ein Pattern, das ein Literal ist (@racket[#t], @racket[#f],
Zeichenketten @racket[string], Zahlen @racket[number]) passt nur dann,
wenn der Wert @italic{v} gleich dem Pattern ist.}

@item{Ein Pattern, das ein Bezeichner @racket[id] ist, passt auf
@emph{jeden} Wert.  Der Bezeichner wird dann an diesen Wert gebunden
und kann in dem Ausdruck des Zweigs benutzt werden.
}

@item{Das Pattern @racketkeywordfont{...} passt auf jeden Wert, ohne dass ein
Bezeichner gebunden wird.}

@item{Ein Pattern @racket[(constructor pattern @#,racketmetafont{...})], bei dem
@racket[constructor] ein Record-Konstruktor ist (ein
@italic{Konstruktor-Pattern}), passt auf @italic{v}, falls @italic{v}
ein passender Record ist, und dessen Felder auf die entsprechenden
Patterns passen, die noch im Konstruktor-Pattern stehen.}
]
}

@; ----------------------------------------------------------------------

@; @section{@racket[require]}
@; 
@; @defform[(require string)]{
@; 
@; Diese Form macht die Definitionen des durch @racket[string] spezifizierten Moduls
@; verfügbar.  Dabei bezieht sich @racket[string] auf eine Datei relativ zu der Datei,
@; in der die @racket[require]-Form steht.
@; 
@; Dabei ist @racket[string] leicht eingeschränkt, um Portabilitätsprobleme zu vermeiden:
@; @litchar{/} ist der Separator für Unterverzeichnisse,, @litchar{.} bedeutet das aktuelle
@; Verzeichnis, @litchar{..} meint das übergeordnete Verzeichnis, Pfadelemente 
@; können nur @litchar{a} bis @litchar{z} (groß oder klein),
@; @litchar{0} bis @litchar{9}, @litchar{-}, @litchar{_}
@; und @litchar{.} enthalten, und die Zeichenkette kann nicht leer sein oder
@; ein @litchar{/} am Anfang oder Ende enthalten.}
@; 
@; 
@; @defform/none[#:literals (require)
@;               (require module-id)]{
@; 
@; Diese Form macht eine eingebaute Library mit dem Namen @racket[module-id] verfügbar.}
@;
@; @defform/none[#:literals (require lib)
@;               (require (lib string string ...))]{
@; 
@; Diese Form macht die Definitionen eines Moduls in einer installierten Bibliothek
@; verfügbar.
@; Der erste
@; @racket[string] ist der Name der Datei des Moduls, und die restlichen
@; @racket[string]s bezeichnen die Collection (und Sub-Collection undsoweiter),
@; in der die Datei installiert ist. Jede @racket[string] ist ebenso eingeschränkt
@; wie bei @racket[(require string)].}
@; 
@; 
@; @defform/none[#:literals (require planet)
@;               (require (planet string (string string number number)))]{
@; 
@; Diese Form macht ein Modul einer Bibliothek verfügbar, die aus PLaneT
@; kommt.}

@; ----------------------------------------

@section{Eigenschaften}

Eine @deftech{Eigenschaft} definiert eine Aussage über einen
Scheme-Ausdruck, die experimentell überprüft werden kann.  Der
einfachste Fall einer Eigenschaft ist ein boolescher Ausdruck.  Die
folgende Eigenschaft gilt immer:

@racketblock[
(= 1 1)
]

Es ist auch möglich, in einer Eigenschaft Variablen zu verwenden, für
die verschiedene Werte eingesetzt werden.  Dafür müssen die Variablen
gebunden und @deftech{quantifiziert} werden, d.h. es muss festgelegt
werden, welche Signatur die Werte der Variable erfüllen sollen.
Eigenschaften mit Variablen werden mit der @racket[for-all]-Form erzeugt:

@defform[(for-all ((id sig) ...) expr)]{
Dies bindet die Variablen @racket[id] in der Eigenschaft
@racket[expr].  Zu jeder Variable gehört eine Signatur
@racket[sig], der von den Werten der Variable erfüllt werden
muss.

Beispiel:

@racketblock[
(for-all ((x integer))
  (= x (/ (* x 2) 2)))
]
}

@defform[(expect expr expr)]{

Ein @racket[expect]-Ausdruck ergibt  eine Eigenschaft,  die dann gilt,
wenn   die Werte von @racket[expr] und   @racket[expr] gleich sind, im
gleichen Sinne wie bei @racket[check-expect].}


@defform[(expect-within expr expr expr)]{

Wie @racket[expect], aber entsprechend @racket[check-within] mit einem
weiteren Ausdruck, der als Wert eine Zahl @racket[_delta] hat. Die
resultierende Eigenschaft gilt, wenn jede Zahl im Resultat des ersten
@racket[expr] maximal um @racket[_delta] von der entsprechenden Zahl
im zweiten @racket[expr] abweicht.}

@defform[(expect-member-of expr expr ...)]{

Wie @racket[expect], aber entsprechend @racket[check-member-of] mit
weiteren Ausdrücken, die mit dem ersten verglichen werden.  Die
resultierende Eigenschaft gilt, wenn das erste Argument gleich 
einem der anderen Argumente ist.}

@defform[(expect-range expr expr expr)]{

Wie @racket[expect], aber entsprechend @racket[check-range]: Die
Argumente müssen Zahlen sein.  Die Eigenschaft gilt, wenn die erste Zahl
zwischen der zweiten und dritten Zahl liegt (inklusive).}


@defform[(==> expr expr)]{
Der erste Operand ist ein boolescher Ausdruck, der zweite Operand eine
Eigenschaft: @racket[(==> c p)] legt fest, dass die Eigenschaft
@racket[p] nur erfüllt sein muss, wenn @racket[c] (die
@emph{Bedingung}) @racket[#t] ergibt, also erfüllt ist.}
 
@racketblock[
(for-all ((x integer))
  (==> (even? x)
       (= x (* 2 (/ x 2)))))
]

@section[#:tag "beginner-prim-ops"]{Primitive Operationen}

@declare-exporting[deinprogramm/sdp/beginner]
@prim-op-defns['(lib "beginner.rkt" "deinprogramm" "sdp") #'here '()]
