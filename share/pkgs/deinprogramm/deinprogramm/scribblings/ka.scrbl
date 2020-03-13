#lang scribble/doc

@(require scribble/manual
         scribble/basic
         scribble/extract
         scheme/class
         scheme/contract)

@title{Konstruktionsanleitungen}

This documents the design recipes of the German textbook @italic{Schreibe
Dein Programm!}.

@table-of-contents[]

@section{Ablauf} Gehe bei der Konstruktion einer Funktion in folgender
  Reihenfolge vor:
@itemize[
  @item{Kurzbeschreibung}
  @item{Datenanalyse}
  @item{Signatur}
  @item{Testfälle}
  @item{Gerüst}
  @item{Schablonen}
  @item{Rumpf}
]

@section{Kurzbeschreibung} Schreibe für die Funktion zunächst einen
  Kommentar, der ihren Zweck kurz beschreibt.  Ein Satz, der auf eine
  Zeile passen sollte, reicht.  Beispiel:

@racketblock[
(code:comment @#,t{monatlichen Rechnungsbetrag für Tarif Billig-Strom berechnen})
]

@section{Signatur-Deklaration} Schreibe für die Funktion direkt unter
die Kurzbeschreibung eine Signatur-Deklaration.  Dazu denke Dir
zunächst einen möglichst aussagekräftigen Namen aus.  Überlege dann,
welche Sorten die Ein- und Ausgaben haben und schreibe dann eine
Signatur, welche die Ein- und Ausgaben der Funktion möglichst präzise
beschreiben.  Beispiel:

@racketblock[
(: billig-strom (natural -> rational))
]

Achte bei den Zahlen-Signaturen besonders auf eine möglichst präzise
Signatur.  Bei @racket[billig-strom] wäre auch die Signatur
@racket[(number -> number)] korrekt, aber nicht so genau.

@section{Tests} Schreibe unter die Signatur Tests für die Funktion.
Denke Dir dafür möglichst möglichst einfache, aber auch möglichst
interessante Beispiele für Aufrufe der Funktion auf und lege fest, was
dabei herauskommen soll.  Mache aus den Beispielen Tests mit
@racket[check-expect].  Beispiel:

@racketblock[
(check-expect (billig-strom 0) 4.9)
(check-expect (billig-strom 10) 6.8)
(check-expect (billig-strom 20) 8.7)
(check-expect (billig-strom 30) 10.6)
]

Achte darauf, dass die Tests dafür sorgen, dass der Code Deiner
Funktion durch die Tests vollständig abgedeckt wird.

@section{Gerüst}
  Schreibe unter die Tests ein Gerüst für die Funktion: Dazu
  übernimmst Du den Namen aus der Signatur-Deklaration in eine
  Funktionsdefinition wie zum Beispiel:

@racketblock[
(define billig-strom
  (lambda (...)
    ...))
]

Denke Dir Namen für die Eingaben der Funktion aus.  Das müssen
  genauso viele sein, wie die Signatur Eingaben hat.  Schreibe dann
  diese Namen als Eingaben in die @racket[lambda]-Abstraktion.
  Beispiel:

@racketblock[
(define billig-strom
  (lambda (kWh)
    ...))
]

@section{Rumpf}
  Als letzten Schritt fülle mit Hilfe des Wissens über das Problem
  den Rumpf der Funktion aus.

@racketblock[
(define billig-strom
  (lambda (kWh)
    (+ 4.90 (* 0.19 kWh))))
]

@section{Datenanalyse}
Suche in der Aufgabenstellung nach problemrelevanten Größen;
Kandidaten sind immer die Substantive. Schreibe für jede dieser
Größen eine Datendefinition, es sei denn, diese ist aus dem Kontext
offensichtlich.

Wenn es für die Datendefinition noch keine Signatur gibt, schreibe
eine Signaturdefinition dazu.  Schreibe außerdem Beispiele auf und
schreibe jeweils einen Kommentar, der die Beziehung zwischen Daten
und Information beschreibt.

@section{Fallunterscheidung: Datenanalyse} Versuche, für die
  Datendefinition eine Formulierung “...  ist eins der
  folgenden” zu finden. Wenn da möglich ist, beschreibt Deine
  Datendefinition eine @italic{Fallunterscheidung}.  Schreibe dann
  eine Auflistung aller Fälle, jeder Fall auf eine separate Zeile:
  
@racketblock[
(code:comment @#,t{Ein X ist eins der folgenden:})
(code:comment @#,t{- Fall 1})
(code:comment @#,t{- Fall 2})
(code:comment @#,t{- ...})
(code:comment @#,t{- Fall n})
]

@section{Aufzählung: Datenanalyse}
Falls Deine Datendefinition eine Fallunterscheidung beschriebt und
jeder der Fälle nur aus einem einzelnen Wert besteht, handelt es
sich um eine @italic{Aufzählung}.

Schreibe für jede Aufzählung eine Signaturdefinition der Form:

@racketblock[
(define s (signature (one-of ...)))
]

Achte darauf, dass die Anzahl der Fälle der Signaturdefinition der
Anzahl der Fälle der Datendefinition entspricht.

@section{Schablone}
Wenn Du das Gerüst fertiggestellt hast, benutze die Signatur und die
dazugehörigen Datendefinitionen, um Konstruktionsanleitungen mit ein
oder mehreren Schablonen auszuwählen und übertrage die Elemente der
Schablonen in den Rumpf der Funktion.

@section{Fallunterscheidung: Schablone}
Wenn Du eine Funktion schreibst, die eine Fallunterscheidung als
Eingabe verarbeitet, schreibe als Schablone in den Rumpf eine
Verzweigung mit sovielen Zweigen, wie es in der Fallunterscheidung
Fälle gibt, nach folgendem Muster:

@racketblock[
(define f
  (lambda a
    (cond
      (... ....)
      ...
      (... ...))))
]

Schreibe danach Bedingungen in die Zweige, welche die einzelnen
Fälle voneinander unterscheiden.

@section{boolesche Fallunterscheidung: Schablone}
Wenn sich das Ergebnis einer Funktion nach einer booleschen Größe
richtet, welche die Funktion mit Hilfe der Eingaben berechnen kann,
benutze als Schablone im Rumpf eine binäre Verzweigung:

@racketblock[
(define f
  (lambda (e)
    (if ... (code:comment @#,t{hier wird die boolesche Größe berechnet})
        ...
        ....)))
]

@section{Zusammengesetzte Daten: Datenanalyse}
Zusammengesetzte Daten kannst Du an Formulierungen wie “ein @italic{X}
besteht aus ...”, “ein @italic{X} ist charakterisiert durch ...”
oder “ein @italic{X} hat ...” erkennen.  Manchmal lautet die
Formulierung etwas anders.  Die daraus resultierende Datendefinition
ist ein Kommentar im Programm in folgender Form:

@racketblock[
(code:comment @#,t{Ein X hat / besteht aus / ist charakterisiert durch:})
(code:comment @#,t{- Bestandteil / Eigenschaft 1})
(code:comment @#,t{- Bestandteil / Eigenschaft 2})
(code:comment @#,t{...})
(code:comment @#,t{- Bestandteil / Eigenschaft n})
]

Auf die Datendefinition folgt eine entsprechende Record-Definition.
Dafür überlege Dir Namen für den Record-Typ @racket[T] und für die
Felder, @racket[f]@subscript{1} ... @racket[f]@subscript{n}.  Für
jedes Feld solltest Du außerdem die dazu passende Signatur
@racket[sig]@subscript{i} angeben.  Die Record-Definition hat dann
folgende Form:

@racketblock[
(define-record-functions T
  make-T
  (#,(elem @racket[T-f]@subscript{1}) #,(elem @racket[sig]@subscript{1}))
  ...
  (#,(elem @racket[T-f]@subscript{n}) #,(elem @racket[sig]@subscript{n})))
]

Der Name des Record-Typs @racket[T] ist die Record-Signatur,
@racket[make-T] ist der Konstruktor und
@racket[T-f]@subscript{i} sind die Selektoren.

Dass der Konstruktorname mit @racket[make-] anfängt und dass die
Selektornamen sich aus dem Namen des Typs und der Felder
zusammensetzt, ist reine Konvention.  Von ihr solltest Du aber nur aus
guten Gründen abweichen.

Unter die Record-Definition gehören die Signaturen für den Konstruktor
und die Selektoren:

@racketblock[
(: make-T (#,(elem @racket[sig]@subscript{1}) ... #,(elem @racket[sig]@subscript{n})) -> T)
(: #,(elem @racket[T-f]@subscript{1}) (T -> #,(elem @racket[sig]@subscript{1})))
...
(: #,(elem @racket[T-f]@subscript{n}) (T -> #,(elem @racket[sig]@subscript{n})))
]

@section{Zusammengesetzte Daten als Eingabe: Schablone}

Wenn Deine Funktion zusammengesetzte Daten als Eingabe akzeptiert
(das ergibt sich aus der Signatur), gehe nach Schreiben des Gerüstes
folgendermaßen vor:

@itemize[
@item{Für jede dieser Komponenten, schreibe
  @racket[(sel r)] in die
  Schablone, wobei @racket[sel] der Selektor der Komponente und @racket[r] der Name
  des Record-Parameters ist, also zum Beispiel:

@racketblock[(wallclock-time-hour wt)]}

@item{Vervollständige die Schablone, indem Du einen Ausdruck
  konstruieren, in dem die Selektor-Anwendungen vorkommen.}
@item{Es ist möglich, dass nicht alle Selektor-Anwendungen im Rumpf
  verwendet werden: In diesem Fall lösche die Selektor-Anwendung
  wieder.}
  ]

@section{Zusammengesetzte Daten als Ausgabe: Schablone}
Wenn Deine Funktion zusammengesetzte Daten als Ausgabe hat, schreibe
einen Aufruf des passenden Record-Konstruktors in den Rumpf,
zunächst mit einer Ellipse für jedes Feld des Records, also zum
Beispiel:

@racketblock[(make-wallclock-time ... ...)]

@section{Gemischte Daten: Datenanalyse}
Gemischte Daten sind Fallunterscheidungen, bei denen jeder Fall eine
eigene Klasse von Daten mit eigener Signatur ist.
Schreibe bei gemischten Daten eine Signaturdefinition der folgenden Form unter die
Datendefinition:

@racketblock[
(define sig
  (signature
    (mixed #,(elem @racket[sig]@subscript{1}) ... #,(elem @racket[sig]@subscript{n}))))
]
@racket[Sig] ist die Signatur für die neue Datensorte; @racket[sig]@subscript{1} bis @racket[sig]@subscript{sn}
sind die Signaturen, aus denen die neue
Datensorte zusammengemischt ist.

@section{Gemischte Daten als Eingabe: Schablone}
Eine Schablone für eine Funktion und deren Testfälle, die gemischte
Daten akzeptiert, kannst Du folgendermaßen konstruieren:

@itemize[
@item{Schreibe Tests für jeden der Fälle.}
@item{Schreibe eine @racket[cond]-Verzweigung als Rumpf in die
  Schablone, die genau @italic{n} Zweige hat - also genau soviele Zweige,
  wie es Fälle in der Datendefinition beziehungsweise der Signatur gibt.}
@item{Schreibe für jeden Zweig eine Bedingung, der den entsprechenden
  Fall identifiziert.}
@item{Vervollständige die Zweige, indem Du eine Datenanalyse für
  jeden einzelnen Fall vornimmst und entsprechende Hilfsfunktionen
  und Konstruktionsanleitungen benutzt.
  Die übersichtlichsten Programme entstehen meist, wenn für jeden Fall
  separate Hilfsfunktionen definiert sind.}
]

@section{Selbstbezüge als Eingabe: Schablone}
  Wenn Du eine Funktion schreibst, die Daten konsumiert, in denen
  Selbstbezüge enthalten sind, dann schreibe an die Stellen der
  Selbstbezüge jeweils einen rekursiven Aufruf.

@section{Listen als Eingabe: Schablone}

Eine Funktion, die eine Liste konsumiert, hat die folgende
Schablone:
  
@racketblock[
(: func ((list-of elem) -> ...))

(define func
  (lambda (lis)
    (cond
      ((empty? lis) ...)
      ((cons? lis)
       ... (first lis)
       ... (func (rest lis)) ...))))
]

Dabei ist @racket[elem] die Signatur für die Elemente der Liste.  Dies
kann eine Signaturvariable (@racket[%a], @racket[%b], ...) sein, falls
die Funktion unabhängig von der Signatur der Listenelemente ist.

Füllen Sie in der Schablone zuerst den @racket[empty?]-Zweig aus.
Vervollständigen Sie dann den anderen Zweig unter der Annahme, daß
der rekursive Aufruf @racket[(func (rest lis))] das gewünschte
Ergebnis für den Rest der Liste liefert.

Beispiel:

@racketblock[
(: list-sum ((list-of number) -> number))

(define list-sum
  (lambda (lis)
    (cond
      ((empty? lis) 0)
      ((cons? lis)
       (+ (first lis)
          (list-sum (rest lis)))))))
]

@section{Natürliche Zahlen als Eingabe: Schablone}

Eine Funktion, die natürliche Zahlen konsumiert, hat die folgende
Schablone:

@racketblock[
(: func (natural -> ...))

(define func
  (lambda (n)
    (if (= n 0)
        ...
        ... (func (- n 1)) ...)))
]

Füllen Sie in der Schablone zuerst den 0-Zweig aus.  Vervollständigen
Sie dann den anderen Zweig unter der Annahme, daß der rekursive Aufruf
@racket[(func (- n 1))] das gewünschte Ergebnis für @racket[n]-1
liefert.

Beispiel:

@racketblock[
(: factorial (natural -> natural))

(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))
]

@section{Funktionen mit Akkumulatoren: Schablone}

Eine Funktion mit Akkumulator, die Listen konsumiert, hat die
folgende Schablone:

@racketblock[
(: func ((list-of elem) -> ...))

(define func
  (lambda (lis)
    (func-helper lis z)))

(: func ((list-of elem) sig -> ...))

(define func-helper
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((cons? lis)
       (func-helper (rest lis)
                    (... (first lis) ... acc ...))))))
]

Hier ist @racket[func] der Name der zu definierenden Funktion und
@racket[func-helper] der Name der Hilfsfunktion mit Akkumulator.  Der
Anfangswert für den Akkumulator ist der Wert von @racket[z].  Die Signatur @racket[sig]
ist die Signatur für den Akkumulator.  Der
Ausdruck @racket[(... (first lis) ... acc ...)] 
macht aus dem alten Zwischenergebnis @racket[acc] das neue
Zwischenergebnis.

Beispiel:

@racketblock[
(: invert ((list-of %a) -> (list-of %a)))

(define invert
  (lambda (lis)
    (invert-helper lis empty)))

(: invert ((list-of %a) (list-of %a) -> (list-of %a)))

(define invert-helper
  (lambda (lis acc)
    (cond
      ((empty? lis) acc)
      ((cons? lis)
       (invert-helper (rest lis)
                      (cons? (first lis) acc))))))
]

Eine Funktion mit Akkumulator, die natürliche Zahlen konsumiert, hat die
folgende Schablone:

@racketblock[
(: func (natural -> ...))

(define func
  (lambda (n)
    (func-helper n z)))

(define func-helper
  (lambda (n acc)
    (if (= n 0)
        acc
        (func-helper (- n 1) (... acc ...)))))
]

Dabei ist @racket[z] das gewünschte Ergebnis für @racket[n] = 0.  Der
Ausdruck @racket[(... acc ...)] muß den neuen Wert für den
Akkumulator berechnen.

Beispiel:

@racketblock[
(: ! (natural -> natural))

(define !
  (lambda (n)
    (!-helper n 1)))

(define !-helper
  (lambda (n acc)
    (if (= n 0)
        acc
        (!-helper (- n 1) (* n acc)))))
]

