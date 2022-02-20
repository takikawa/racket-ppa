#lang scheme/base

(require syntax/docprovide)

(require (only-in test-engine/test-engine
                  add-failed-check! failed-check
                  property-error property-fail)
         (rename-in scheme/base (cons racket-cons))
	 test-engine/racket-tests
	 test-engine/syntax
         test-engine/srcloc
	 scheme/class)

(require deinprogramm/sdp/private/module-begin
	 (except-in deinprogramm/signature/signature signature-violation)
	 (except-in deinprogramm/signature/signature-syntax property))

(require (for-syntax scheme/base)
	 (for-syntax stepper/private/syntax-property)
	 (for-syntax syntax/parse)
	 (for-syntax racket/struct-info)
	 syntax/parse)

(require deinprogramm/sdp/record)

(require (only-in lang/private/teachprims define-teach teach-equal? beginner-equal~?))

(require (for-syntax deinprogramm/private/syntax-checkers))

(require (for-syntax "rewrite-error-message.rkt"))
(require "rewrite-error-message.rkt")
	  
(require (rename-in deinprogramm/quickcheck/quickcheck
		    (property quickcheck:property)))

(provide provide lib planet rename-out require #%datum #%module-begin #%top-interaction) ; so we can use this as a language

(provide (all-from-out deinprogramm/sdp/record))
(provide (rename-out (define-record define-record-functions)))
(provide (all-from-out test-engine/racket-tests))
(provide signature define-contract :
	 contract ; legacy
	 one-of ; deprecated
	 -> mixed predicate enum combined list-of nonempty-list-of)
(provide (rename-out (nonempty-list-of cons-list-of)))

(provide number real rational integer integer-from-to natural
	 boolean true false
	 string symbol
	 empty-list
	 unspecific
	 any
	 property)

(provide match)

(define-syntax provide/rename
  (syntax-rules ()
    ((provide/rename (here there) ...)
     (begin
       (provide (rename-out (here there))) ...))))

(provide/rename
 (sdp-define define)
 (sdp-let let)
 (sdp-let* let*)
 (sdp-letrec letrec)
 (sdp-lambda lambda)
 (sdp-lambda λ)
 (sdp-cond cond)
 (sdp-if if)
 (sdp-else else)
 (sdp-begin begin)
 (sdp-and and)
 (sdp-or or)
 (sdp-dots ..)
 (sdp-dots ...)
 (sdp-dots ....)
 (sdp-dots .....)
 (sdp-dots ......)
 (sdp-app #%app)
 (sdp-top #%top)
 (sdp-set! set!))

(provide sdp-advanced-lambda
	 sdp-advanced-define)

(provide for-all ==>
	 check-property
	 expect expect-within expect-member-of expect-range)

(provide quote)

(provide-and-document
 procedures
 ("Zahlen"
  (number? (any -> boolean)
	   "feststellen, ob ein Wert eine Zahl ist")

  (= (number number number ... -> boolean)
     "Zahlen auf Gleichheit testen")
  (< (real real real ... -> boolean)
     "Zahlen auf kleiner-als testen")
  (> (real real real ... -> boolean)
     "Zahlen auf größer-als testen")
  (<= (real real real ... -> boolean)
      "Zahlen auf kleiner-gleich testen")
  (>= (real real real ... -> boolean)
      "Zahlen auf größer-gleich testen")
  
  (+ (number number number ... -> number)
     "Summe berechnen")
  (- (number number ... -> number)
     "bei mehr als einem Argument Differenz zwischen der ersten und der Summe aller weiteren Argumente berechnen; bei einem Argument Zahl negieren")
  (* (number number number ... -> number)
     "Produkt berechnen")
  (/ (number number number ... -> number)
     "das erste Argument durch das Produkt aller weiteren Argumente berechnen")
  (max (real real ... -> real)
       "Maximum berechnen")
  (min (real real ... -> real)
       "Minimum berechnen")
  (quotient (integer integer -> integer)
	    "ganzzahlig dividieren")
  (remainder (integer integer -> integer)
	     "Divisionsrest berechnen")
  (modulo (integer integer -> integer)
	  "Divisionsmodulo berechnen")
  (sqrt (number -> number)
	"Quadratwurzel berechnen")
  (expt (number number -> number)
	"Potenz berechnen (erstes Argument hoch zweites Argument)")
  (abs (real -> real)
       "Absolutwert berechnen")
  
  ;; fancy numeric 
  (exp (number -> number)
       "Exponentialfunktion berechnen (e hoch Argument)")
  (log (number -> number)
       "natürlichen Logarithmus (Basis e) berechnen")
  
  ;; trigonometry
  (sin (number -> number)
       "Sinus berechnen (Argument in Radian)")
  (cos (number -> number)
       "Cosinus berechnen (Argument in Radian)")
  (tan (number -> number)
       "Tangens berechnen (Argument in Radian)")
  (asin (number -> number)
	"Arcussinus berechnen (in Radian)")
  (acos (number -> number)
	"Arcuscosinus berechnen (in Radian)")
  (atan (number -> number)
	"Arcustangens berechnen (in Radian)")
  
  (exact? (number -> boolean)
	  "feststellen, ob eine Zahl exakt ist")
  
  (integer? (any -> boolean)
	    "feststellen, ob ein Wert eine ganze Zahl ist")
  (natural? (any -> boolean)
	    "feststellen, ob ein Wert eine natürliche Zahl (inkl. 0) ist")
  
  (zero? (number -> boolean)
	 "feststellen, ob eine Zahl Null ist") 
  (positive? (number -> boolean)
	     "feststellen, ob eine Zahl positiv ist")
  (negative? (number -> boolean)
	     "feststellen, ob eine Zahl negativ ist")
  (odd? (integer -> boolean)
	"feststellen, ob eine Zahl ungerade ist")
  (even? (integer -> boolean)
	 "feststellen, ob eine Zahl gerade ist")

  (lcm (integer integer ... -> natural)
       "kleinstes gemeinsames Vielfaches berechnen")
  
  (gcd (integer integer ... -> natural)
       "größten gemeinsamen Teiler berechnen")
  
  (rational? (any -> boolean)
	     "feststellen, ob eine Zahl rational ist")
  
  (numerator (rational -> integer)
	     "Zähler eines Bruchs berechnen")
  
  (denominator (rational -> natural)
	       "Nenner eines Bruchs berechnen")
  
  (inexact? (number -> boolean)
	    "feststellen, ob eine Zahl inexakt ist")
  
  (real? (any -> boolean)
	 "feststellen, ob ein Wert eine reelle Zahl ist")
  
  (floor (real -> integer)
	 "nächste ganze Zahl unterhalb einer rellen Zahlen berechnen")
  
  (ceiling (real -> integer)
	   "nächste ganze Zahl oberhalb einer rellen Zahlen berechnen")
  
  (round (real -> integer)
	 "relle Zahl auf eine ganze Zahl runden")
  
  (complex? (any -> boolean)
	    "feststellen, ob ein Wert eine komplexe Zahl ist")
  
  (make-polar (real real -> number)
	      "komplexe Zahl aus Abstand zum Ursprung und Winkel berechnen")
  
  (real-part (number -> real)
	     "reellen Anteil einer komplexen Zahl extrahieren")
  
  (imag-part (number -> real)
	     "imaginären Anteil einer komplexen Zahl extrahieren")
  
  (magnitude (number -> real)
	     "Abstand zum Ursprung einer komplexen Zahl berechnen")
  
  (angle (number -> real)
	 "Winkel einer komplexen Zahl berechnen")
  
  (exact->inexact (number -> number)
		  "eine Zahl durch eine inexakte Zahl annähern")
  
  (inexact->exact (number -> number)
		  "eine Zahl durch eine exakte Zahl annähern")
  
  ;;    "Odds and ends"
  
  (number->string (number -> string)
		  "Zahl in Zeichenkette umwandeln")

  (string->number (string -> (mixed number false))
		  "Zeichenkette in Zahl umwandeln, falls möglich")
  
  (random (natural -> natural)
	  "eine natürliche Zufallszahl berechnen, die kleiner als das Argument ist")
  
  (current-seconds (-> natural)
		   "aktuelle Zeit in Sekunden seit einem unspezifizierten Startzeitpunkt berechnen"))

 ("boolesche Werte" 
  (boolean? (any -> boolean)
	    "feststellen, ob ein Wert ein boolescher Wert ist")
  
  ((sdp-not not) (boolean -> boolean)
   "booleschen Wert negieren")

  (boolean=? (boolean boolean -> boolean)
	     "Booleans auf Gleichheit testen")

  (true? (any -> boolean)
	 "feststellen, ob ein Wert #t ist")
  (false? (any -> boolean)
	  "feststellen, ob ein Wert #f ist"))

 ("Listen"
  (empty list "die leere Liste")
  ((sdp-cons cons) (%a (list-of %a) -> (list-of %a))
	     "erzeuge ein Cons aus Element und Liste")
  (cons? (any -> boolean)
	 "feststellen, ob ein Wert ein Cons ist")
  (empty? (any -> boolean)
	  "feststellen, ob ein Wert die leere Liste ist")
  
  (first ((list-of %a) -> %a)
	 "erstes Element eines Cons extrahieren")
  (rest ((list-of %a) -> (list-of %a))
	"Rest eines Cons extrahieren")

  (list (%a ... -> (list-of %a))
	"Liste aus den Argumenten konstruieren")

  (length ((list-of %a) -> natural)
	  "Länge einer Liste berechnen")

  (filter ((%a -> boolean) (list-of %a) -> (list-of %a))
	 "Alle Elemente einer Liste extrahieren, für welche die Funktion #t liefert.")

  (fold (%b (%a %b -> %b) (list-of %a) -> %b)
	 "Liste einfalten.")


  ((sdp-append append) ((list-of %a) ... -> (list-of %a))
   "mehrere Listen aneinanderhängen")

  (list-ref ((list-of %a) natural -> %a)
	    "das Listenelement an der gegebenen Position extrahieren")
  
  (reverse ((list-of %a)  -> (list-of %a))
	   "Liste in umgekehrte Reihenfolge bringen"))

 ;; #### Zeichen sollten noch dazu, Vektoren wahrscheinlich auch

 ("Zeichenketten"
  (string? (any -> boolean)
	   "feststellen, ob ein Wert eine Zeichenkette ist")

  (string=? (string string string ... -> boolean)
	    "Zeichenketten auf Gleichheit testen")
  (string<? (string string string ... -> boolean)
	    "Zeichenketten lexikografisch auf kleiner-als testen")
  (string>? (string string string ... -> boolean)
	    "Zeichenketten lexikografisch auf größer-als testen")
  (string<=? (string string string ... -> boolean)
	     "Zeichenketten lexikografisch auf kleiner-gleich testen")
  (string>=? (string string string ... -> boolean)
	     "Zeichenketten lexikografisch auf größer-gleich testen")

  (string-append (string string ... -> string)
		 "Hängt Zeichenketten zu einer Zeichenkette zusammen")

  (strings-list->string ((list-of string) -> string)
			"Eine Liste von Zeichenketten in eine Zeichenkette umwandeln")

  (string->strings-list (string -> (list-of string))
			"Eine Zeichenkette in eine Liste von Zeichenketten mit einzelnen Zeichen umwandeln")

  (string-length (string -> natural)
		 "Liefert Länge einer Zeichenkette"))

 ("Symbole"
  (symbol? (any -> boolean)
	   "feststellen, ob ein Wert ein Symbol ist")
  (symbol=? (symbol symbol -> boolean)
	    "Sind zwei Symbole gleich?")
  (symbol->string (symbol -> string)
		  "Symbol in Zeichenkette umwandeln")
  (string->symbol (string -> symbol)
		  "Zeichenkette in Symbol umwandeln"))
 
 ("Verschiedenes"
  (signature? (any -> boolean)
	      "feststellen, ob ein Wert eine Signatur ist")
  (equal? (%a %b -> boolean)
	  "zwei Werte auf Gleichheit testen")
  (eq? (%a %b -> boolean)
       "zwei Werte auf Selbheit testen")
  ((sdp-write-string write-string) (string -> unspecific)
   "Zeichenkette in REPL ausgeben")
  (write-newline (-> unspecific)
		 "Zeilenumbruch ausgeben")
  (violation (string -> unspecific)
	     "Programmm mit Fehlermeldung abbrechen")

  (map ((%a -> %b) (list-of %a) -> (list-of %b))
       "Funktion auf alle Elemente einer Liste anwenden, Liste der Resultate berechnen")
  (for-each ((%a -> %b) (list-of %a) -> unspecific)
	    "Funktion von vorn nach hinten auf alle Elemente einer Liste anwenden")
  (apply (function (list-of %a) -> %b)
	 "Funktion auf Liste ihrer Argumente anwenden")
  (read (-> any)
	"Externe Repräsentation eines Werts in der REPL einlesen und den zugehörigen Wert liefern")))

(define cons
  (lambda (f r)
    (when (and (not (null? r))
               (not (pair? r)))
      (raise
       (make-exn:fail:contract
        (string->immutable-string
         (format "Zweites Argument zu cons ist keine Liste, sondern ~e" r))
        (current-continuation-marks))))
    (racket-cons f r)))

(define-syntax sdp-cons
  (let ()
    ;; make it work with match
    (define-struct cons-info ()
      #:super struct:struct-info
      #:property
      prop:procedure
      (lambda (_ stx)
	(syntax-case stx ()
	  ((self . args) (syntax/loc stx (cons . args)))
	  (else (syntax/loc stx cons)))))
    (make-cons-info (lambda ()
		      (list #f
			    #'cons
			    #'cons?
			    (list #'cdr #'car)
			    '(#f #f)
			    #f)))))

(define (first l)
  (when (not (pair? l))
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "Argument zu first kein Cons, sondern ~e" l))
      (current-continuation-marks))))
  (car l))

(define (rest l)
  (when (not (pair? l))
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "Argument zu rest kein Cons, sondern ~e" l))
      (current-continuation-marks))))
  (cdr l))

(define empty '())

(define (empty? obj)
  (null? obj))

(define (cons? obj)
  (pair? obj))

(define-teach sdp append
  (lambda args
    (let loop ((args args)
	       (seen-rev '()))
      (when (not (null? args))
	(let ((arg (car args)))
	  (when (and (not (null? arg))
		     (not (pair? arg)))
	    (raise
	     (make-exn:fail:contract
	      (string->immutable-string
	       (format "Erstes Argument zu append keine Liste, sondern ~e; restliche Argumente:~a"
		       arg
		       (apply string-append
			      (map (lambda (arg)
				     (format " ~e" arg))
				   (append (reverse seen-rev)
					   (list '<...>)
					   (cdr args))))))
	      (current-continuation-marks))))
	  (loop (cdr args)
		(racket-cons arg seen-rev)))))
  

    (apply append args)))

(define fold
  (lambda (unit combine lis)
    (cond
      ((empty? lis) unit)
      ((pair? lis) 
       (combine (first lis)
                (fold unit combine (rest lis))))
      (else
       (raise
	(make-exn:fail:contract
	 (string->immutable-string
	  (format "Drittes Argument zu fold keine Liste, sondern ~e; andere Argumente: ~e ~e"
		  lis
		  unit combine))
	 (current-continuation-marks)))))))

(define filter
  (lambda (p? lis)
    (when (not (procedure? p?))
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "Erstes Argument zu filter keine Funktion, sondern ~e" p?))
	(current-continuation-marks))))
    (cond
     ((empty? lis) '())
     ((pair? lis)
      (if (p? (first lis))
	  (racket-cons (first lis)
                       (filter p? (rest lis)))
	  (filter p? (rest lis))))
     (else
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "Zweites Argument zu filter keine Liste, sondern ~e"
		 lis))
	(current-continuation-marks)))))))

;; This is copied from collects/lang/private/beginner-funs.rkt
;; Test-suite support (require is really an effect
;;  to make sure that it's loaded)
(require deinprogramm/test-suite)


(define-for-syntax (raise-sdp-syntax-error form msg . exprs)

  (define (expr->form expr)
    (let ((sexpr (syntax->datum expr)))
      (cond
       ((identifier? expr) sexpr)
       ((syntax->list expr)
	=> (lambda (lis)
	     (expr->form (car lis))))
       (else #f))))
  
  (let ((form
	 (or form
	     (if (pair? exprs)
		 (expr->form (car exprs))
		 #f))))
    (raise
     (exn:fail:syntax (if form
			  (string-append (format "~a" form) ": " msg)
			  msg)
		      (current-continuation-marks)
		      exprs))))
  
(define-for-syntax (binding-in-this-module? b)
  (and (list? b)
       (module-path-index? (car b))
       (let-values (((path base) (module-path-index-split (car b))))
	 (and (not path) (not base)))))

(define-for-syntax (transform-sdp-define stx mutable?)
  (syntax-case stx ()
    ((sdp-define)
     (raise-sdp-syntax-error
      #f "Definition ohne Operanden" stx))
    ((sdp-define v)
     (raise-sdp-syntax-error
      #f "Definition erwartet zwei Operanden, nicht einen" stx))
    ((sdp-define var expr)
     (begin
       (check-for-id!
	(syntax var)
	"Der erste Operand der Definition ist kein Name")
       
       (let ((binding (identifier-binding (syntax var))))
	 (when binding
	   (if (binding-in-this-module? binding)
	       (raise-sdp-syntax-error
		#f
		"Zweite Definition für denselben Namen"
		stx)
	       (raise-sdp-syntax-error
		#f
		"Dieser Name gehört einer eingebauten Funktion und kann nicht erneut definiert werden" (syntax var)))))
       (if mutable?
	   (with-syntax
	       ((dummy-def (stepper-syntax-property
			    (syntax (define dummy (lambda () (set! var 'dummy))))
			    'stepper-skip-completely
			    #t)))
	     (syntax/loc stx
			 (begin
			   dummy-def
			   (define var expr))))
	   (syntax/loc stx (define var expr)))))
    ((sdp-define v e1 e2 e3 ...)
     (raise-sdp-syntax-error
      #f "Definition mit mehr als zwei Operanden" stx))))

(define-syntax (sdp-define stx)
  (transform-sdp-define stx #f))

(define-syntax (sdp-advanced-define stx)
  (transform-sdp-define stx #t))

(define-for-syntax (check-body-definitions bodies)
  (let ((pairs
	 (map (lambda (stx)
		;; want to be able to shadow global definitions
		(syntax-case stx (sdp-define)
		  ((sdp-define)
		   (raise-sdp-syntax-error
		    #f "Definition ohne Operanden" stx))
		  ((sdp-define v)
		   (raise-sdp-syntax-error
		    #f "Definition erwartet zwei Operanden, nicht einen" stx))
		  ((sdp-define var expr)
		   (begin
		     (check-for-id!
		      (syntax var)
		      "Der erste Operand der Definition ist kein Name")
		     (cons #'var (syntax/loc stx (define var expr)))))
		  ((sdp-define v e1 e2 e3 ...)
		   (raise-sdp-syntax-error
		    #f "Definition mit mehr als zwei Operanden" stx))
		  (else
		   (raise-sdp-syntax-error
		    #f "Hier muss Definition stehen" stx))))
	      bodies)))
    (let loop ((pairs pairs))
      (when (pair? pairs)
	(let ((id (caar pairs)))
	  (cond
	   ((memf (lambda (p)
		    (bound-identifier=? id (car p)))
		  (cdr pairs))
	    => (lambda (rest)
		 (raise-sdp-syntax-error
		  #f
		  "Zweite Definition für denselben Namen"
		  (cdar rest)))))
	  (loop (cdr pairs)))))
    (map cdr pairs)))
	
(define-syntax (sdp-let stx)
  (syntax-case stx ()
    ((sdp-let ((var expr) ...) body0 ... body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Name in `let-Bindung")
       (with-syntax (((body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))))
	 (syntax/loc stx ((lambda (var ...) body0 ... body) expr ...)))))
    ((sdp-let expr ...)
     (raise-sdp-syntax-error
      #f "`let'-Ausdruck erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

(define-syntax (sdp-let* stx)
  (syntax-case stx ()
    ((sdp-let* () body0 ... body)
     (syntax/loc stx (let () body0 ... body)))
    ((sdp-let* ((var1 expr1) (var2 expr2) ...) body0 ... body)
     (begin
       (check-for-id!
	(syntax var1)
	"Kein Name in `let*'-Bindung")
       (with-syntax (((body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))))
	 (syntax/loc stx ((lambda (var1)
			    (sdp-let* ((var2 expr2) ...) body0 ... body))
			  expr1)))))
    ((sdp-let* expr ...)
     (raise-sdp-syntax-error
      #f "`let*'-Ausdruck erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))

(define-syntax (sdp-letrec stx)
  (syntax-case stx ()
    ((sdp-letrec ((var expr) ...) body0 ... body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Name in letrec-Bindung")
       (with-syntax (((body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))))
	 (syntax/loc stx (letrec ((var expr) ...) body0 ... body)))))
    ((sdp-letrec expr ...)
     (raise-sdp-syntax-error
      #f "`letrec''-Ausdruck erwartet eine Liste von Bindungen (Paare aus Name und Ausdruck) und einen Rumpf" stx))))
	    
(define-syntax (sdp-lambda stx)
  (syntax-case stx ()
    ((sdp-lambda (var ...) body0 ... body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Name als Parameter der Abstraktion")
       (with-syntax (((body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))))
	 (syntax/loc stx (lambda (var ...) body0 ... body)))))
    ((sdp-lambda var body ...)
     (identifier? (syntax var))
     (raise-sdp-syntax-error
      #f "Um die Parameter einer Abstraktion gehören Klammern" (syntax var)))
    ((sdp-lambda var ...)
     (raise-sdp-syntax-error
      #f "Fehlerhafte Abstraktion" stx))))

(define-syntax (sdp-advanced-lambda stx)
  (syntax-case stx ()
    ((sdp-lambda (var ...) body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Name als Parameter der Abstraktion")
       (syntax/loc stx (lambda (var ...) body))))
    ((sdp-lambda (var ... . rest) body0 ... body)
     (begin
       (check-for-id-list!
	(syntax->list (syntax (var ...)))
	"Kein Name als Parameter der Abstraktion")
       (unless (null? (syntax->datum #'rest))
	 (check-for-id! 
	  (syntax rest)
	  "Kein Name als Restlisten-Parameter der Abstraktion"))
       (with-syntax (((body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))))
	 (syntax/loc stx (lambda (var ... . rest) body0 ... body)))))
    ((sdp-lambda var ...)
     (raise-sdp-syntax-error
      #f "Fehlerhafte Abstraktion" stx))))

(define-syntax (sdp-begin stx)
  (syntax-case stx ()
    ((sdp-begin)
     (raise-sdp-syntax-error
      #f "`begin`-Ausdruck braucht mindestens einen Operanden" stx))
    ((sdp-begin expr1 expr2 ...)
     (syntax/loc stx (begin expr1 expr2 ...)))))

(define-for-syntax (local-expand-for-error stx ctx stops)
  ;; This function should only be called in an 'expression
  ;;  context. In case we mess up, avoid bogus error messages.
  (when (memq (syntax-local-context) '(expression))
    (local-expand stx ctx stops)))

(define-for-syntax (ensure-expression stx k)
  (if (memq (syntax-local-context) '(expression))
      (k)
      (stepper-syntax-property #`(begin0 #,stx) 'stepper-skipto skipto/second)))

;; A consistent pattern for stepper-skipto:
(define-for-syntax (stepper-ignore-checker stx)
  (stepper-syntax-property stx 'stepper-skipto '(syntax-e cdr syntax-e cdr car)))

;; Raise a syntax error:
(define-for-syntax (teach-syntax-error form stx detail msg . args)
  (let ([form (if (eq? form '|function call|) ; ####
		  form
		  #f)] ; extract name from stx
	[msg (apply format msg args)])
    (if detail
	(raise-sdp-syntax-error form msg stx detail)
	(raise-sdp-syntax-error form msg stx))))

;; The syntax error when a form's name doesn't follow a "("
(define-for-syntax (bad-use-error name stx)
  (teach-syntax-error
   name
   stx
   #f
   "`~a' wurde an einer Stelle gefunden, die keiner offenen Klammer folgt"
   name))

;; Use for messages "expected ..., found <something else>"
(define-for-syntax (something-else v)
  (let ([v (syntax-e v)])
    (cond
     [(number? v) "eine Zahl"]
     [(string? v) "eine Zeichenkette"]
     [else "etwas anderes"])))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (sdp-cond stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_)
	(teach-syntax-error
	 'cond
	 stx
	 #f
	 "Bedingung und ein Ausdruck nach `cond' erwartet, aber da ist nichts")]
       [(_ clause ...)
	(let* ([clauses (syntax->list (syntax (clause ...)))]
	       [check-preceding-exprs
		(lambda (stop-before)
		  (let/ec k
		    (for-each (lambda (clause)
				(if (eq? clause stop-before)
				    (k #t)
				    (syntax-case clause ()
				      [(question body0 ... answer)
				       (begin
					 (unless (and (identifier? (syntax question))
						      (free-identifier=? (syntax question) #'sdp-else))
					   (local-expand-for-error (syntax question) 'expression null))
					 (local-expand-for-error #'(let () body0 ... answer) 'expression null))])))
			      clauses)))])
	  (let ([checked-clauses
		 (map
		  (lambda (clause)
		    (syntax-case clause (sdp-else)
		      [(sdp-else body0 ... answer)
		       (let ([lpos (memq clause clauses)])
			 (when (not (null? (cdr lpos)))
			   (teach-syntax-error
			    'cond
			    stx
			    clause
			    "`else'-Bedingung gefunden, die nicht am Ende des `cond'-Ausdrucks steht"))
			 (with-syntax ([(body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))]
				       [new-test (stepper-syntax-property (syntax #t) 'stepper-else #t)])
			   (syntax/loc clause (new-test body0 ... answer))))]
		      [(question body0 ... answer)
		       (begin
			 (with-syntax ([(body0 ...) (check-body-definitions (syntax->list #'(body0 ...)))]
				       [verified (stepper-ignore-checker (syntax (verify-boolean question 'cond)))])
			   (syntax/loc clause (verified body0 ... answer))))]
		      [()
		       (check-preceding-exprs clause)
		       (teach-syntax-error
			'cond
			stx
			clause
			"Bedingung und Ausdruck in Zweig erwartet, aber Zweig leer")]
		      [(question?)
		       (check-preceding-exprs clause)
		       (teach-syntax-error
			'cond
			stx
			clause
			"Zweig mit Bedingung und Ausdruck erwartet, aber Zweig enthält nur eine Form")]
		      [_else
		       (teach-syntax-error
			'cond
			stx
			clause
			"Zweig mit Bedingung und Ausdruck erwartet, aber ~a gefunden"
			(something-else clause))]))
		  clauses)])
	    ;; Add `else' clause for error (always):
	    (let ([clauses (append checked-clauses 
				   (list 
				    (with-syntax ([error-call (syntax/loc stx (error 'cond "alle Bedingungen ergaben #f"))])
				      (syntax [else error-call]))))])
	      (with-syntax ([clauses clauses])
		(syntax/loc stx (cond . clauses))))))]
       [_else (bad-use-error 'cond stx)]))))

(define-syntax sdp-else
  (make-set!-transformer
   (lambda (stx)
     (define (bad expr)
       (teach-syntax-error
	'else
	expr
	#f
	"hier nicht erlaubt, weil kein Bedingung in `cond'-Zweig"))
     (syntax-case stx (set! x)
       [(set! e expr) (bad #'e)]
       [(e . expr) (bad #'e)]
       [e (bad stx)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (sdp-if stx)
  (ensure-expression
   stx
   (lambda ()
     (syntax-case stx ()
       [(_ test then else)
	(with-syntax ([new-test (stepper-ignore-checker (syntax (verify-boolean test 'if)))])
	  (syntax/loc stx
		      (if new-test
			  then
			  else)))]
       [(_ . rest)
	(let ([n (length (syntax->list (syntax rest)))])
	  (teach-syntax-error
	   'if
	   stx
	   #f
	   "Bedingung und zwei Ausdrücke erwartet, aber ~a Form~a gefunden"
	   (if (zero? n) "keine" n)
	   (if (= n 1) "" "en")))]
       [_else (bad-use-error 'if stx)]))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; or, and
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntaxes (sdp-or sdp-and)
  (let ([mk
	 (lambda (where)
	   (let ([stepper-tag (case where
				[(or) 'comes-from-or]
				[(and) 'comes-from-and])])
	     (with-syntax ([swhere where])
	       (lambda (stx)
		 (ensure-expression
		  stx
		  (lambda ()
		    (syntax-case stx ()
		      [(_ . clauses)
		       (let ([n (length (syntax->list (syntax clauses)))])
			 (let loop ([clauses-consumed 0]
				    [remaining (syntax->list #`clauses)])
			   (if (null? remaining)
			       (case where
				 [(or) #`#f]
				 [(and) #`#t])
			       (stepper-syntax-property
				(stepper-syntax-property
				 (quasisyntax/loc 
				  stx
				  (if #,(stepper-ignore-checker (quasisyntax/loc stx (verify-boolean #,(car remaining) 'swhere)))
				      #,@(case where
					   [(or) #`(#t
						    #,(loop (+ clauses-consumed 1) (cdr remaining)))]
					   [(and) #`(#,(loop (+ clauses-consumed 1) (cdr remaining))
						     #f)])))
				 'stepper-hint
				 stepper-tag)
				'stepper-and/or-clauses-consumed
				clauses-consumed))))]
		      [_else (bad-use-error where stx)])))))))])
    (values (mk 'or) (mk 'and))))

;; verify-boolean is inserted to check for boolean results:
(define (verify-boolean b where)
  (if (or (eq? b #t) (eq? b #f))
      b
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "~a: Testresultat ist nicht boolesch: ~e" where b))
	(current-continuation-marks)))))

(define-teach sdp not
  (lambda (b)
    (verify-boolean b 'not)
    (not b)))

(define (boolean=? a b)
  (verify-boolean a 'boolean=?)
  (verify-boolean b 'boolean=?)
  (eq? a b))

(define (verify-symbol b where)
  (if (symbol? b)
      b
      (raise
       (make-exn:fail:contract
	(string->immutable-string
	 (format "~a: Wert ist kein Symbol: ~e" where b))
	(current-continuation-marks)))))

(define (symbol=? a b)
  (verify-symbol a 'symbol=?)
  (verify-symbol b 'symbol=?)
  (eq? a b))

(define-syntax (sdp-app stx)
  (define (raise-operator-error no-op expr)
    (raise-sdp-syntax-error #f
			    (format "Operator darf ~a sein, ist aber ~s" no-op (syntax->datum expr))
			    expr))
  (syntax-case stx ()
    ((_)
     (raise-sdp-syntax-error
      #f "Zusammengesetzte Form ohne Operator" (syntax/loc stx ())))
    ((_ datum1 datum2 ...)
     (number? (syntax->datum #'datum1))
     (raise-operator-error "keine Zahl" #'datum1))
    ((_ datum1 datum2 ...)
     (boolean? (syntax->datum #'datum1))
     (raise-operator-error "kein boolesches Literal" #'datum1))
    ((_ datum1 datum2 ...)
     (string? (syntax->datum #'datum1))
     (raise-operator-error "keine Zeichenkette" #'datum1))
    ((_ datum1 datum2 ...)
     (char? (syntax->datum #'datum1))
     (raise-operator-error "kein Zeichen" #'datum1))
    ((_ datum1 datum2 ...)
     (syntax/loc stx (#%app datum1 datum2 ...)))))

(define (top/check-defined id)
  (namespace-variable-value (syntax-e id) #t (lambda () (raise-not-bound-error id))))

(define-syntax (sdp-top stx)
  (syntax-case stx ()
    ((_ . id)
     ;; If we're in a module, we'll need to check that the name
     ;;  is bound....
     (if (not (identifier-binding #'id))
	 (if (syntax-source-module #'id)
	     ;; ... but it might be defined later in the module, so
	     ;; delay the check.
	     (stepper-ignore-checker 
	      (syntax/loc stx (#%app values (sdp-top-continue id))))
             ;; identifier-finding only returns useful information when inside a module. 
             ;; At the top-level we need to  do the check at runtime. Also, note that at 
             ;; the top level there is no need for stepper annotations
             (syntax/loc stx (#%app top/check-defined #'id)))

	 (syntax/loc stx (#%top . id))))))

(define-syntax (sdp-top-continue stx)
  (syntax-case stx ()
    [(_ id)
     ;; If there's still no binding, it's an "unknown name" error.
     (if (not (identifier-binding #'id))
           ;; If there's still no binding, it's an "unknown name" error.
           (raise-not-bound-error #'id)

	 ;; Don't use #%top here; id might have become bound to something
	 ;;  that isn't a value.
	 #'id)]))

(define-teach sdp write-string
  (lambda (s)
    (when (not (string? s))
      (error "Argument von write-string ist keine Zeichenkette"))
    (display s)))

(define (write-newline)
  (newline))

(define (violation text)
  (error text))

(define (string->strings-list s)
  (map (lambda (c) (make-string 1 c)) (string->list s)))

(define (strings-list->string l)
  (if (null? l)
      ""
      (string-append (car l) (strings-list->string (cdr l)))))

(define integer (signature/arbitrary arbitrary-integer integer (predicate integer?)))
(define (integer-from-to lo hi)
  (unless (integer? lo)
    (error "Erstes Argument von integer-from-to ist keine ganze Zahl."))
  (unless (integer? hi)
    (error "Zweites Argument von integer-from-to ist keine ganze Zahl."))
  (unless (<= lo hi)
    (error "Das erste Argument von integer-from-to ist größer als das zweite."))
  (signature/arbitrary (arbitrary-integer-from-to lo hi) integer-from-to
                       (predicate (lambda (n)
                                    (and (integer? n)
                                         (<= lo n hi))))))
(define number (signature/arbitrary arbitrary-real number (predicate number?)))
(define rational (signature/arbitrary arbitrary-rational rational (predicate rational?)))
(define real (signature/arbitrary arbitrary-real real (predicate real?)))

(define (natural? x)
  (and (integer? x)
       (not (negative? x))))

(define natural (signature/arbitrary arbitrary-natural natural (predicate natural?)))

(define boolean (signature/arbitrary arbitrary-boolean boolean (predicate boolean?)))

(define (true? x)
  (eq? x #t))

(define (false? x)
  (eq? x #f))

(define true (signature true (enum #t)))
(define false (signature false (enum #f)))

(define string (signature/arbitrary arbitrary-printable-ascii-string string (predicate string?)))
(define symbol (signature/arbitrary arbitrary-symbol symbol (predicate symbol?)))
(define empty-list (signature empty-list (enum empty)))

(define unspecific (signature unspecific %unspecific))
(define any (signature any %any))

;; aus collects/lang/private/teach.rkt

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dots (.. and ... and .... and ..... and ......)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntax Identifier -> Expression
;; Produces an expression which raises an error reporting unfinished code.
(define-for-syntax (dots-error stx name)
  (quasisyntax/loc stx
		   (error (quote (unsyntax name))
			  "Fertiger Ausdruck erwartet, aber da sind noch Ellipsen")))

;; Expression -> Expression
;; Transforms unfinished code (... and the like) to code
;; raising an appropriate error.
(define-syntax sdp-dots
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [(set! form expr) (dots-error stx (syntax form))]
       [(form . rest) (dots-error stx (syntax form))]
       [form (dots-error stx stx)]))))

(define-syntaxes (sdp-set! sdp-set!-continue)
  (let ((proc
	 (lambda (continuing?)
	   (lambda (stx)
	     (ensure-expression
	      stx
	      (lambda ()
		(syntax-case stx ()
		  ((_ id expr)
		   (identifier? (syntax id))
		   (begin
		     ;; Check that id isn't syntax, and not lexical.
		     ((with-handlers ((exn:fail? (lambda (exn) void)))
			;; First try syntax:
			;; If it's a transformer binding, then it can take care of itself...
			(if (set!-transformer? (syntax-local-value (syntax id)))
			    void  ;; no lex check wanted
			    (lambda ()
			      (raise-sdp-syntax-error
			       #f
			       "Nach set! wird eine gebundene Variable erwartet, aber da ist ein Schlüsselwort."
			       stx)))))
		     ;; If we're in a module, we'd like to check here whether
		     ;;  the identier is bound, but we need to delay that check
		     ;;  in case the id is defined later in the module. So only
		     ;;  do this in continuing mode:
		     (when continuing?
		       (when (and (not (identifier-binding #'id))
				  (syntax-source-module #'id))
			 (raise-sdp-syntax-error #f "Ungebundene Variable" #'id)))
		     (if continuing?
			 (syntax/loc stx (set! id expr))
			 (stepper-ignore-checker (syntax/loc stx (#%app values (sdp-set!-continue id expr)))))))
		  ((_ id expr)
		   (raise-sdp-syntax-error
		    #f
		    "Nach set! wird eine Variable aber da ist etwas anderes."
		    #'id))
		  ((_ id)
		   (raise-sdp-syntax-error
		    #f
		    "Nach set! wird eine Variable und ein Ausdruck erwartet - der Ausdruck fehlt."
		    stx))
		  ((_)
		   (raise-sdp-syntax-error
		    #f
		    "Nach set! wird eine Variable und ein Ausdruck erwartet, aber da ist nichts."
		    stx))
		  (_else 
		   (raise-sdp-syntax-error
		    #f
		    "Inkorrekter set!-Ausdruck."
		    stx)))))))))
    (values (proc #f)
	    (proc #t))))

; QuickCheck

(define-syntax (for-all stx)
  (syntax-case stx ()
    ((_ (?clause ...) ?body0 ?body ...)
     (with-syntax ((((?id ?arb) ...)
		    (map (lambda (pr)
			   (syntax-case pr ()
			     ((?id ?signature)
			      (identifier? #'?id)
			      (with-syntax ((?error-call
					     (syntax/loc #'?signature (error "Signatur hat keinen Generator"))))
				#'(?id
				   (or (signature-arbitrary (signature ?signature))
				       ?error-call))))
			     (_
			      (raise-sdp-syntax-error #f "inkorrekte `for-all'-Klausel - sollte die Form (id signature) haben"
						      pr))))
			 (syntax->list #'(?clause ...)))))

       (stepper-syntax-property #'(quickcheck:property 
				   ((?id ?arb) ...) ?body0 ?body ...)
				'stepper-skip-completely
				#t)))
    ((_ ?something ?body0 ?body ...)
     (raise-sdp-syntax-error #f "keine Klauseln der Form (id contr)"
			     stx))
    ((_ ?something)
     (raise-sdp-syntax-error #f "Rumpf fehlt" stx))))


(define-syntax (check-property stx)
  (unless (memq (syntax-local-context) '(module top-level))
    (raise-sdp-syntax-error
     #f "`check-property' muss ganz außen stehen" stx))
  (syntax-case stx ()
    ((_ ?prop)
     (stepper-syntax-property
      (check-expect-maker stx #'check-property-error #'?prop '() 
			  'comes-from-check-property)
      'stepper-replace
      #'#t))
    (_ (raise-sdp-syntax-error #f "`check-property' erwartet einen einzelnen Operanden"
			       stx))))

(define quickcheck-config
  (make-config 100
               2000
               (lambda (n)
                 (+ 3 (* n 2)))
               values))

(define (check-property-error test srcloc)
  (with-handlers ((exn:fail?
                   (lambda (e)
                     (add-failed-check! (failed-check (property-error srcloc e)
                                                      (exn-srcloc e))))))
    (call-with-values
     (lambda ()
       (with-handlers
           ((exn:assertion-violation?
             (lambda (e)
               ;; minor kludge to produce comprehensible error message
               (if (eq? (exn:assertion-violation-who e) 'coerce->result-generator)
                   (raise (make-exn:fail (string-append "Wert muss Eigenschaft oder boolesch sein: "
                                                        ((error-value->string-handler)
                                                         (car (exn:assertion-violation-irritants e))
                                                         100))
                                         (exn-continuation-marks e)))
                   (raise e)))))
         (check-results quickcheck-config (test))))
     (lambda (ntest stamps result)
       (if (check-result? result)
           (begin
             (add-failed-check! (failed-check (property-fail srcloc result) #f))
             #f)
           #t)))))

(define (expect v1 v2)
  (quickcheck:property () (teach-equal? v1 v2)))

(define (ensure-real who n val)
  (unless (real? val)
    (raise
     (make-exn:fail:contract
      (string->immutable-string
       (format "~a Argument ~e zu `~a' keine reelle Zahl." n val who))
      (current-continuation-marks)))))

(define (expect-within v1 v2 epsilon)
  (ensure-real 'expect-within "Drittes" epsilon)
  (quickcheck:property () (beginner-equal~? v1 v2 epsilon)))

(define (expect-range val min max)
  (ensure-real 'expect-range "Erstes" val)
  (ensure-real 'expect-range "Zweites" min)
  (ensure-real 'expect-range "Drittes" max)
  (quickcheck:property ()
		       (and (<= min val)
			    (<= val max))))

(define (expect-member-of val . candidates)
  (quickcheck:property () 
		       (ormap (lambda (cand)
				(teach-equal? val cand))
			      candidates)))

(define property (signature (predicate (lambda (x)
					(or (boolean? x)
					    (property? x))))))


(define-syntax (match stx)
  (syntax-parse stx
    ((_ ?case:expr (?pattern0 ?body0:expr) (?pattern ?body:expr) ...)
     (let ()
       (define (pattern-variables pat)
	 (syntax-case pat (empty sdp-cons list quote ...)
	   ((... ...) '())
	   (empty '())
	   (?var (identifier? #'?var)
	     (if (eq? (syntax->datum #'?var) '_)
		 '()
		 (list #'?var)))
	   (?lit (let ((d (syntax->datum #'?lit)))
		   (or (string? d) (number? d) (boolean? d)))
		 '())
	   ('?lit '())
	   ((sdp-cons ?pat1 ?pat2)
	    (append (pattern-variables #'?pat1) (pattern-variables #'?pat2)))
	   ((list) '())
	   ((list ?pat0 ?pat ...)
	    (apply append (map pattern-variables (syntax->list #'(?pat0 ?pat ...)))))
	   ((?const ?pat ...)
	    (apply append (map pattern-variables (syntax->list #'(?pat ...)))))))
       (define (check pat)
	 (let loop ((vars (pattern-variables pat)))
	   (when (pair? vars)
	     (let ((var (car vars)))
	       (when (memf (lambda (other-var)
			     (free-identifier=? var other-var))
			   (cdr vars))
		 (raise-sdp-syntax-error #f "Variable in match-Zweig kommt doppelt vor"
					 var))
	       (loop (cdr vars))))))
       (for-each check (syntax->list #'(?pattern0 ?pattern ...)))
       #'(let* ((val ?case)
		(nomatch (lambda () (match val (?pattern ?body) ...))))
	   (match-helper val ?pattern0 ?body0 (nomatch)))))
    ((_ ?case:expr)
     (syntax/loc stx (error 'match "keiner der Zweige passte")))))


(define (list-length=? lis n)
  (cond
   ((zero? n) (null? lis))
   ((null? lis) #f)
   (else
    (list-length=? (cdr lis) (- n 1)))))

(define-syntax (match-helper stx)
  (syntax-case stx ()
    ((_ ?id ?pattern0 ?body0 ?nomatch)
     (syntax-case #'?pattern0 (empty cons list quote ...)
       (empty
	#'(if (null? ?id)
	      ?body0
	      ?nomatch))
       ((... ...)
	#'?body0)
       (?var (identifier? #'?var)
	     (if (eq? (syntax->datum #'?var) '_) ; _ is magic
		 #'?body0
		 #'(let ((?var ?id))
		     ?body0)))
       (?lit (let ((d (syntax->datum #'?lit)))
	       (or (string? d) (number? d) (boolean? d)))
	     #'(if (equal? ?id ?lit)
		   ?body0
		   ?nomatch))
       ('?lit
	#'(if (equal? ?id '?lit)
	      ?body0
	      ?nomatch))
       ((cons ?pat1 ?pat2)
	#'(if (pair? ?id)
	      (let ((f (first ?id))
		    (r (rest ?id)))
		(match-helper f ?pat1
			      (match-helper r ?pat2 ?body0 ?nomatch)
			      ?nomatch))
	      ?nomatch))
       ((list)
	#'(if (null? ?id)
	      ?body0
	      ?nomatch))
       ((list ?pat0 ?pat ...)
	(let* ((pats (syntax->list #'(?pat0 ?pat ...)))
	       (cars (generate-temporaries pats))
	       (cdrs (generate-temporaries pats)))
	#`(if (and (pair? ?id)
		   (list-length=? ?id #,(length pats)))
	      #,(let recur ((ccdr #'?id)
			    (pats pats)
			    (cars cars) (cdrs cdrs))
		  (if (null? pats)
		      #'?body0
		      #`(let ((#,(car cars) (car #,ccdr))
			      (#,(car cdrs) (cdr #,ccdr)))
			  (match-helper #,(car cars) #,(car pats)
					#,(recur (car cdrs) (cdr pats) (cdr cars) (cdr cdrs))
					?nomatch))))
	      ?nomatch)))
       ((?const ?pat ...)
	(identifier? #'?const)
	(let* ((fail (lambda ()
		       (raise-sdp-syntax-error #f "Operator in match muss ein Record-Konstruktor sein"
					       #'?const)))
	       (v (syntax-local-value #'?const fail)))
	  (unless (struct-info? v)
	    (fail))

	  (apply
	   (lambda (_ _cons pred rev-selectors _mutators ?)
	     (let* ((pats (syntax->list #'(?pat ...)))
		    (selectors (reverse rev-selectors))
		    (field-ids (generate-temporaries pats)))
	       (unless (= (length rev-selectors) (length pats))
		 (raise-sdp-syntax-error #f "Die Anzahl der Felder im match stimmt nicht" #'?pattern0))
	       #`(if (#,pred ?id)
		     #,(let recur ((pats pats)
				   (selectors selectors)
				   (field-ids field-ids))
			 (if (null? pats)
			     #'?body0
			     #`(let ((#,(car field-ids) (#,(car selectors) ?id)))
				 (match-helper #,(car field-ids) #,(car pats)
					       #,(recur (cdr pats) (cdr selectors) (cdr field-ids))
					       ?nomatch))))
		     ?nomatch)))
	   (extract-struct-info v))))))))
