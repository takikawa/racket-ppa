#lang s-exp "type-env-lang.ss"

[Number -Number]
[Integer -Integer]
[Void -Void]
[Boolean -Boolean]
[Symbol -Symbol]
[String -String]
[Any Univ]
[Port -Port]
[Path -Path]
[Regexp -Regexp]
[PRegexp -PRegexp]
[Char -Char]
[Namespace -Namespace]
[Input-Port -Input-Port]
[Output-Port -Output-Port]
[Bytes -Bytes]
[EOF (-val eof)]
[Sexpof (-poly (a) (-Sexpof a))]   ;; recursive union of sexps with a
[Syntaxof (-poly (a) (-Syntax a))] ;; syntax-e yields a
[Syntax-E In-Syntax] ;; possible results of syntax-e on "2D" syntax
[Syntax Any-Syntax]  ;; (Syntaxof Syntax-E): "2D" syntax
[Datum Syntax-Sexp]  ;; (Sexpof Syntax), datum->syntax yields "2D" syntax
[Sexp -Sexp]         ;; (Sexpof (U)), syntax->datum of "2D" syntax
[Identifier Ident]
[Procedure top-func]
[Keyword -Keyword]
[Listof -Listof]
[Vectorof (-poly (a) (make-Vector a))]
[Option (-poly (a) (-opt a))]
[HashTable (-poly (a b) (-HT a b))]
[Promise (-poly (a) (-Promise a))]
[Pair (-poly (a b) (-pair a b))]
[Boxof (-poly (a) (make-Box a))]
[Continuation-Mark-Set -Cont-Mark-Set]
