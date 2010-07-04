#lang scribble/doc
@(require scribblings/htdp-langs/common
	  scribble/struct
	  "std-grammar.ss"
	  "prim-ops.ss"
          (for-label deinprogramm/DMdA-beginner))

@title[#:style 'toc #:tag "DMdA-beginner"]{Die Macht der Abstraktion - Anfänger}

This is documentation for the language level @italic{Die Macht der
Abstraktion - Anfänger} to go with the German textbook @italic{Die
Macht der Abstraktion}.

@declare-exporting[deinprogramm/DMdA-beginner]

@schemegrammar*-DMdA[
#:literals ()
() () ()
]

@|prim-nonterms|

@prim-ops['(lib "DMdA-beginner.ss" "deinprogramm") #'here]

@; ----------------------------------------------------------------------

@section{Definitionen}

@defform[(define id expr)]{
Diese Form ist eine Definition, und bindet @scheme[id] als
globalen Namen an den Wert von @scheme[exp].}

@section{Record-Typ-Definitionen}

@defform[(define-record-procedures t c p (s1 ...))]{

Die @scheme[define-record-procedures]-Form ist eine Definition
für einen neuen Record-Typ.  Dabei ist @scheme[t] der Name des Record-Vertrags,
@scheme[c] der Name des Konstruktors, @scheme[p]
der Name des Prädikats, und die @scheme[si] sind die 
Namen der Selektoren.}

@section[#:tag "application"]{Prozedurapplikation}

@defform/none[(expr expr ...)]{
Dies ist eine Prozeduranwendung oder Applikation.
Alle @scheme[expr]s werden ausgewertet:
Der Operator (also der erste Ausdruck) muß eine
Prozedur ergeben, die genauso viele Argumente
akzeptieren kann, wie es Operanden, also weitere @scheme[expr]s gibt.
Die Anwendung wird dann ausgewertet, indem der Rumpf
der Applikation ausgewertet wird, nachdem die Parameter
der Prozedur durch die Argumente, also die Werte der
Operanden ersetzt wurden.}

@; @defform[(#%app id expr expr ...)]{
@; 
@; Eine Prozedurapplikation kann auch mit @scheme[#%app] geschrieben
@; werden, aber das macht eigentlich niemand.}

@section{@scheme[#t] and @scheme[#f]}

@as-index{@litchar{#t}} ist das Literal für den booleschen Wert "wahr",
@as-index{@litchar{#f}} das Literal für den booleschen Wert "falsch".

@section{@scheme[lambda]}

@defform[(lambda (id ...) expr)]{
Ein Lambda-Ausdruck ergibt bei der Auswertung eine neue Prozedur.}

@section[#:tag "id"]{Bezeichner}

@defform/none[id]{
Eine Variable bezieht sich auf die, von innen nach
außen suchend, nächstgelegene Bindung durch @scheme[lambda], @scheme[let], @scheme[letrec], oder
@scheme[let*]. Falls es keine solche lokale Bindung gibt, muß es eine
Definition oder eine eingebaute Bindung mit dem entsprechenden Namen
geben. Die Auswertung des Namens ergibt dann den entsprechenden
Wert. }

@section{@scheme[cond]}

@defform[(cond (expr expr) ... (expr expr))]{
Ein @scheme[cond]-Ausdruck bildet eine Verzweigung, die aus mehreren
Zweigen besteht. Jeder Zweig besteht
aus einem Test und einem Ausdruck. Bei der Auswertung werden die
Zweige nacheinander abgearbeitet. Dabei wird jeweils zunächst der Test
ausgewertet, der jeweils einen booleschen Wert ergeben müssen. Beim
ersten Test, der @scheme[#t] ergibt, wird der Wert des Ausdrucks des Zweigs zum
Wert der gesamten Verzweigung. Wenn kein Test @scheme[#t] ergibt, wird das
Programm mit einer Fehlermeldung abgebrochen.
}

@defform/none[#:literals (cond else)
              (cond (expr expr) ... (else expr))]{
 Die Form des cond-Ausdrucks ist ähnlich zur vorigen, mit der
 Ausnahme, daß in dem Fall, in dem kein Test @scheme[#t] ergibt, der Wert des
 letzten Ausdruck zum Wert der @scheme[cond]-Form wird.
}

@defidform[else]{

Das Schlüsselwort @scheme[else] kann nur in @scheme[cond] benutzt werden.}

@; ----------------------------------------------------------------------

@section{@scheme[if]}

@defform[(if expr expr expr)]{
Eine @scheme[if]-Form ist eine binäre Verzweigung. Bei der Auswertung wird
zunächst der erste Operand ausgewertet (der Test), der einen
booleschen Wert ergeben muß. Ergibt er @scheme[#t], wird der Wert des zweiten
Operanden (die Konsequente) zum Wert der @scheme[if]-Form, bei @scheme[#f] der Wert des
dritten Operanden (die Alternative).
}

@; ----------------------------------------------------------------------

@section{@scheme[and]}

@defform[(and expr ...)]{
Bei der Auswertung eines @scheme[and]-Ausdrucks werden nacheinander die
Operanden (die boolesche Werte ergeben müssen) ausgewertet. Ergibt
einer @scheme[#f], ergibt auch der and-Ausdruck @scheme[#f]; wenn alle
Operanden @scheme[#t] ergeben, ergibt auch der @scheme[and]-Ausdruck
@scheme[#t].
}

@; ----------------------------------------------------------------------

@section{@scheme[or]}

@defform[(or expr ...)]{
Bei der Auswertung eines @scheme[or]-Ausdrucks werden nacheinander die
Operanden (die boolesche Werte ergeben müssen) ausgewertet. Ergibt
einer @scheme[#t], ergibt auch der or-Ausdruck @scheme[#t]; wenn alle Operanden @scheme[#f]
ergeben, ergibt auch der @scheme[or]-Ausdruck @scheme[#f].
}

@section{@scheme[let], @scheme[letrec] und @scheme[let*]}

@defform[(let ((id expr) ...) expr)]{

Bei einem @scheme[let]-Ausdruck werden zunächst die @scheme[expr]s aus
den @scheme[(id expr)]-Paaren ausgewertet. Ihre Werte werden dann im
Rumpf-@scheme[expr] für die Namen @scheme[id] eingesetzt. Dabei können
sich die Ausdrücke nicht auf die Namen beziehen.

@schemeblock[
(define a 3)
(let ((a 16)
      (b a))
  (+ b a))
=> 19]

Das Vorkommen von @scheme[a] in der Bindung von @scheme[b] bezieht
sich also auf das @scheme[a] aus der Definition, nicht das @scheme[a]
aus dem @scheme[let]-Ausdruck.
}

@defform[(letrec ((id expr) ...) expr)]{
Ein @scheme[letrec]-Ausdruck ist
ähnlich zum entsprechenden @scheme[let]-Ausdruck, mit dem Unterschied, daß sich
die @scheme[expr]s aus den Bindungen auf die gebundenen Namen beziehen
dürfen.}

@defform[(let* ((id expr) ...) expr)]{
Ein @scheme[let*]-Ausdruck ist ähnlich zum entsprechenden
@scheme[let]-Ausdruck, mit dem Unterschied, daß sich die @scheme[expr]s
aus den Bindungen auf die Namen beziehen dürfen, die jeweils vor dem
@scheme[expr] gebunden wurden. Beispiel:

@schemeblock[
(define a 3)
(let* ((a 16)
       (b a))
  (+ b a))
=> 32]

Das Vorkommen von @scheme[a] in der Bindung von @scheme[b] bezieht
sich also auf das @scheme[a] aus dem @scheme[let*]-Ausdruck, nicht das
@scheme[a] aus der globalen Definition.
}

@section{@scheme[begin]}

@defform[(begin expr expr ...)]{
Bei der Auswertung eines @scheme[begin]-Ausdrucks werden nacheinander
die Operanden ausgewertet. Der Wert des letzten Ausdrucks wird der
Wert des @scheme[begin]-Ausdrucks.
}

@section{Verträge}

@subsection{@scheme[contract]}
@defform[(contract contr)]{
Diese Form liefert den Vertrag mit der Notation @scheme[contr].
}

@subsection{Vertragserklärung}
@defform[(: id contr)]{
Diese Form erklärt @scheme[contr] zum gültigen Vertrag für @scheme[id].
}

@defidform[number]{
Vertrag für beliebige Zahlen.
}

@defidform[real]{
Vertrag für reelle Zahlen.
}

@defidform[rational]{
Vertrag für rationale Zahlen.
}

@defidform[integer]{
Vertrag für ganze Zahlen.
}

@defidform[natural]{
Vertrag für ganze, nichtnegative Zahlen.
}

@defidform[boolean]{
Vertrag für boolesche Werte.
}

@defidform[true]{
Vertrag für \scheme[#t].
}

@defidform[false]{
Vertrag für \scheme[#f].
}

@defidform[string]{
Vertrag für Zeichenketten.
}

@defidform[empty-list]{
Vertrag für die leere Liste.
}

@subsection{@scheme[predicate]}
@defform[(predicate expr)]{
Bei diesem Vertrag muß @scheme[expr] als Wert ein Prädikat haben, also
eine Prozedur, die einen beliebigen Wert akzeptiert und entweder @scheme[#t]
oder @scheme[#f] zurückgibt.
Der Vertrag ist dann für einen Wert gültig, wenn das Prädikat, darauf angewendet,
@scheme[#t] ergibt.
}

@subsection{@scheme[one-of]}
@defform[(one-of expr ...)]{
Dieser Vertrag ist für einen Wert gültig, wenn er gleich dem Wert eines
der @scheme[expr] ist.
}

@subsection{@scheme[mixed]}
@defform[(mixed contr ...)]{
Dieser Vertrag ist für einen Wert gültig, wenn er für einen der Verträge
@scheme[contr] gültig ist.
}

@subsection[#:tag "proc-contract"]{Prozedur-Vertrag}
@defidform[->]{
@defform/none[(contr ... -> contr)]{
Dieser Vertrag ist dann für einen Wert gültig, wenn dieser eine
Prozedur ist.  Er erklärt außerdem, daß die Verträge vor dem @scheme[->]
für die Argumente der Prozedur gelten und der Vertrag nach dem @scheme[->]
für den Rückgabewert.
}}
}

@subsection{@scheme[property]}
@defform[(property expr contr)]{
Dieser Vertrag ist für ein Objekt @scheme[obj] gültig, wenn der
Vertrag @scheme[contr] für @scheme[(expr obj)] gültig ist.

(In der Regel ist @scheme[expr] ein Record-Selektor @scheme[s].  In
dem Fall ist der Vertrag @scheme[(property s c)] für alle Records
gültig, bei denen der Wert des zu @scheme[s] gehörigen Felds den
Vertrag @scheme[c] erfüllt.)
}

@subsection{@scheme[list]} 
@defform[(list contr)]{
Dieser Vertrag ist dann für einen Wert gültig, wenn dieser eine Liste ist,
für dessen Elemente @scheme[contr] gültig ist.
}

@subsection[#:tag "contract-variable"]{Vertrags-Variablen} 
@defform/none[%a]
@defform/none[%b]
@defform/none[%c]
@defform/none[...]{
Dies ist eine Vertragsvariable: sie steht für einen Vertrag, der für jeden Wert gültig ist.
}

@subsection{@scheme[combined]}
@defform[(combined contr ...)]{
Dieser Vertrag ist für einen Wert gültig, wenn er für alle der Verträge
@scheme[contr] gültig ist.
}

@section{Testfälle}

@defform[(check-expect expr expr)]{

Dieser Testfall überprüft, ob der erste @scheme[expr] den gleichen
Wert hat wie der zweite @scheme[expr], wobei das zweite @scheme[expr]
meist ein Literal ist.}

@defform[(check-within expr expr expr)]{

Wie @scheme[check-expect], aber mit einem weiteren Ausdruck, 
der als Wert eine Zahl @scheme[_delta] hat. Der Testfall überprüft, daß jede Zahl im Resultat
des ersten @scheme[expr] maximal um @scheme[_delta] 
von der entsprechenden Zahl im zweiten @scheme[expr] abweicht.}

@defform[(check-error expr expr)]{

Dieser Testfall überprüft, ob der erste @scheme[expr] einen Fehler produziert,
wobei die Fehlermeldung der Zeichenkette entspricht, die der Wert des zweiten
@scheme[expr] ist.}

@defform[(check-property expr)]{

Dieser Testfall überprüft experimentell, ob die Eigenschaft
@scheme[expr] erfüllt ist.  Dazu werden zufällige Werte für die mit
@scheme[for-all] quantifizierten Variablen eingesetzt: Damit wird
überprüft, ob die Bedingung gilt.

@emph{Wichtig:} @scheme[check-property] funktioniert nur für
Eigenschaften, bei denen aus den Verträgen sinnvoll Werte generiert
werden können.  Dies ist für die meisten eingebauten Verträge der
Fall, aber nicht für Verträge, die mit @scheme[predicate],
@scheme[property] oder @scheme[define-record-procedures] definiert
wurden.  In diesen Fällen erzeugt @scheme[check-property] eine Fehlermeldung.
}

@section{Parametrische Record-Typ-Definitionen}

@defform[(define-record-procedures-parametric t cc c p (s1 ...))]{

Die @scheme[define-record-procedures-parametric] ist wie
@scheme[define-record-procedures].  Zusäzlich wird der Bezeichner
@scheme[cc] an einen Vertragskonstruktor gebunden: Dieser akzeptiert
für jedes Feld einen Feld-Vertrag und liefert einen Vertrag, den nur
Records des Record-Typs @scheme[t] erfüllen, bei dem die Feldinhalte
die Feld-Verträge erfüllen.

Beispiel:

@schemeblock[
(define-record-procedures-parametric pare pare-of
  make-pare pare?
  (pare-one pare-two))
]

Dann ist @scheme[(pare-of integer string)] der Vertrag für
@scheme[pare]-Records, bei dem die Feldinhalte die Verträge
@scheme[integer] bzw. @scheme[string] erfüllen müssen.
}

@; ----------------------------------------------------------------------

@; @section{@scheme[require]}
@; 
@; @defform[(require string)]{
@; 
@; Diese Form macht die Definitionen des durch @scheme[string] spezifizierten Moduls
@; verfügbar.  Dabei bezieht sich @scheme[string] auf eine Datei relativ zu der Datei,
@; in der die @scheme[require]-Form steht.
@; 
@; Dabei ist @scheme[string] leicht eingeschränkt, um Portabilitätsprobleme zu vermeiden:
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
@; Diese Form macht eine eingebaute Library mit dem Namen @scheme[module-id] verfügbar.}
@;
@; @defform/none[#:literals (require lib)
@;               (require (lib string string ...))]{
@; 
@; Diese Form macht die Definitionen eines Moduls in einer installierten Bibliothek
@; verfügbar.
@; Der erste
@; @scheme[string] ist der Name der Datei des Moduls, und die restlichen
@; @scheme[string]s bezeichnen die Collection (und Sub-Collection undsoweiter),
@; in der die Datei installiert ist. Jede @scheme[string] ist ebenso eingeschränkt
@; wie bei @scheme[(require string)].}
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

@schemeblock[
(= 1 1)
]

Es ist auch möglich, in einer Eigenschaft Variablen zu verwenden, für
die verschiedene Werte eingesetzt werden.  Dafür müssen die Variablen
gebunden und @deftech{quantifiziert} werden, d.h. es muß festgelegt
werden, welchen Vertrag die Werte der Variable erfüllen sollen.
Eigenschaften mit Variablen werden mit der @scheme[for-all]-Form erzeugt:

@defform[(for-all ((id contr) ...) expr)]{
Dies bindet die Variablen @scheme[id] in der Eigenschaft
@scheme[expr].  Zu jeder Variable gehört ein Vertrag
@scheme[contr], der von den Werten der Variable erfüllt werden
muß.

Beispiel:

@schemeblock[
(for-all ((x integer))
  (= x (/ (* x 2) 2)))
]
}

@defform[(expect expr expr)]{

Ein @scheme[expect]-Ausdruck ergibt  eine Eigenschaft,  die dann gilt,
wenn   die Werte von @scheme[expr] und   @scheme[expr] gleich sind, im
gleichen Sinne wie bei @scheme[check-expect].}


@defform[(expect-within expr expr expr)]{

Wie @scheme[expect], aber entsprechend @scheme[check-within] mit einem
weiteren Ausdruck, der als Wert eine Zahl @scheme[_delta] hat. Die
resultierende Eigenschaft gilt, wenn jede Zahl im Resultat des ersten
@scheme[expr] maximal um @scheme[_delta] von der entsprechenden Zahl
im zweiten @scheme[expr] abweicht.}

@defform[(==> expr expr)]{
Der erste Operand ist ein boolescher Ausdruck, der zweite Operand eine
Eigenschaft: @scheme[(==> c p)] legt fest, daß die Eigenschaft
@scheme[p] nur erfüllt sein muß, wenn @scheme[c] (die
@emph{Bedingung}) @scheme[#t] ergibt, also erfüllt ist.}
 
@schemeblock[
(for-all ((x integer))
  (==> (even? x)
       (= x (* 2 (/ x 2)))))
]

@section[#:tag "beginner-prim-ops"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-beginner.ss" "deinprogramm") #'here '()]
