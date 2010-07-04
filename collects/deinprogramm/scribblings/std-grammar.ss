#reader scribble/reader
#lang scheme/base
(require scribblings/htdp-langs/common
         scribble/decode
         (for-label deinprogramm/DMdA-beginner))

(provide prim-nonterms
         schemegrammar*-DMdA)

(define ex-str "Dies ist eine Zeichenkette, die \" enthält.")

(define-syntax-rule (schemegrammar*-DMdA
                     #:literals (lit ...)
		     (def-rule ...)
		     (prod ...)
                     (expr-rule ...))
  (schemegrammar*
   #:literals (define define-record-procedures lambda cond if and or let letrec let* begin 
		#;require lib planet
		check-expect check-within check-error
		define-contract :
		predicate one-of mixed list %a %b %c
		lit ...)
   (... [program (code:line def-or-expr ...)])
   [def-or-expr definition
     expr
     test-case             
     #;library-require]
   [definition #, @scheme[(define id expr)]
     #, @scheme[(define-record-procedures id id id (id (... ...)))]
     #, @scheme[(define-record-procedures-parametric (id id (... ...)) id id (id (... ...)))]
     #, @scheme[(define-contract id contract)]
     #, @scheme[(: id contract)]
     def-rule ...]
   prod ...
   [expr #, @scheme[(code:line (expr expr (... ...)) (code:comment #, @seclink["application"]{Prozedurapplikation}))]
	 #, @scheme[#t]
	 #, @scheme[#f]
	 #, @scheme[number]
	 #, @scheme[string]
	 #, @scheme[(lambda (id (... ...)) expr)]
	 #, @scheme[(code:line id (code:comment #, @seclink["id"]{Bezeichner}))]
	 #, @scheme[(cond (expr expr) (expr expr) (... ...))]
	 #, @scheme[(cond (expr expr) (... ...) (else expr))]
	 #, @scheme[(if expr expr)]
	 #, @scheme[(and expr (... ...))]
	 #, @scheme[(or expr (... ...))]
	 #, @scheme[(let ((id expr) (... ...)) expr)]
	 #, @scheme[(letrec ((id expr) (... ...)) expr)]
	 #, @scheme[(let* ((id expr) (... ...)) expr) ]
	 #, @scheme[(begin expr expr (... ...))]
	 expr-rule ...]
   [contract  id
	      #, @scheme[(predicate expr)]
	      #, @scheme[(one-of expr (... ...))]
	      #, @scheme[(mixed contract (... ...))]
	      #, @scheme[(code:line (contract (... ...) -> contract) (code:comment #, @seclink["proc-contract"]{Prozedur-Vertrag}))]
	      #, @scheme[(list contract)]
	      #, @scheme[(code:line %a %b %c (code:comment #, @seclink["contract-variable"]{Vertrags-Variable}))]
	      #, @scheme[(combined contract (... ...))]
	      #, @scheme[(property expr contract)]
   ]
   [test-case #, @scheme[(check-expect expr expr)]
              #, @scheme[(check-within expr expr expr)]
              #, @scheme[(check-error expr expr)]]
   #;(...
    [library-require #, @scheme[(require string)]
                     #, @scheme[(require (lib string string ...))]
                     #, @scheme[(require (planet string package))]])
   (...
    [package #, @scheme[(string string number number)]])))

(define prim-nonterms
  (make-splice
   (list

@t{Ein @scheme[_id] ist eine Folge von Zeichen, die weder Leerzeichen
noch eins der folgenden Zeichen enthält:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}

@t{Ein @scheme[_number] ist eine Zahl wie z.B. @scheme[123], @scheme[3/2] oder
@scheme[5.5].}

@t{Ein @scheme[_string] ist eine Zeichenkette, und durch ein Paar von @litchar{"} umschlossen. 
So sind z.B. @scheme["abcdef"],
@scheme["This is a string"] und @scheme[#,ex-str] Zeichenketten.}
)))
