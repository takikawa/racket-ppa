#lang scribble/doc
@(require scribble/manual
	  scribble/eval
          (for-label scheme/base
                     scheme/contract
                     scheme/class
                     scheme/gui/base
                     lang/posn
                     lang/imageeq
                     lang/prim))

@(define sdp @italic{Schreibe Dein Programm!})
@(define (sdp-ref s) @secref[#:doc '(lib "deinprogramm/scribblings/deinprogramm.scrbl") s])

Note: This is documentation for the language levels that go with the
German textbook @italic{@link["http://www.deinprogramm.de/sdp/"]{Schreibe
Dein Programm!}}.

@title{@bold{sdp}: Sprachen als Libraries}

@; ------------------------------------------------------------
@section{@italic{Schreibe Dein Programm} - Anfänger}

@defmodule[deinprogramm/sdp/beginner]

Das Modul @racketmodname[deinprogramm/sdp/beginner] implementiert die
Anfängersprache für @|sdp|; siehe @sdp-ref["sdp-beginner"].

@; ------------------------------------------------------------
@section{@italic{Schreibe Dein Programm!}}

@defmodule[deinprogramm/sdp/vanilla]

Das Modul @racketmodname[deinprogramm/sdp/vanilla] implementiert die
Standardsprache für @|sdp|; siehe @sdp-ref["sdp-vanilla"].

@; ------------------------------------------------------------
@section{@italic{Schreibe Dein Programm!} - fortgeschritten}

@defmodule[deinprogramm/sdp/advanced]

Das Modul @racketmodname[deinprogramm/sdp/advanced] implementiert
die fortgeschittene Sprachebene für @|sdp|; siehe
@sdp-ref["sdp-advanced"].
