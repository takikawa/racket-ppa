#lang scheme/base

;; Copied and adapted from htdp-lib

(require mzlib/etc
         mzlib/list
         (for-syntax lang/private/firstorder
                     scheme/base))

(provide rewrite-contract-error-message
         reraise-rewriten-lookup-error-message
         get-rewriten-error-message
         raise-not-bound-error
         argcount-error-message)

(define (reraise-rewriten-lookup-error-message e id was-in-app-position)
  (let ([var-or-function (if was-in-app-position "Funktion" "Variable")])
    (raise-syntax-error
     #f
     (format "~a ist nicht definiert" var-or-function)
     id)))

(define (exn-needs-rewriting? exn)
  (exn:fail:contract? exn))

(define (ensure-number n-or-str)
  (cond
    ((number? n-or-str)
     n-or-str)
    ((string=? n-or-str "no") 0)
    ((string=? n-or-str "none") 0)
    (else (string->number n-or-str))))

(define (plural-e n)
  (if (> (ensure-number n) 1) "e" ""))

(define (raise-not-bound-error id)
  (if (syntax-property id 'was-in-app-position)
      (raise-syntax-error
       #f
       "Funktion ist nicht definiert"
       id)
      (raise-syntax-error
       #f
       "Variable ist nicht definiert"
       id)))

(define (argcount-error-message name arity found [at-least #f])
  (define arity:n (ensure-number arity))
  (define found:n (ensure-number found))
  (define fn-is-large (> arity:n found:n))
  (format "~a~a ~a~a~a Argument~a erwartet, aber ~a~a gefunden"
          (or name "") (if name ":" "")
          (if at-least "mindestens " "")
          (if (or (= arity:n 0) fn-is-large) "" "nur ")
          (if (= arity:n 0) "kein" arity:n) (plural-e arity:n)
          (if (and (not (= found:n 0)) fn-is-large) "nur " "")
          (if (= found:n 0) "keins" found:n)))

(define (format-enum conj l)
  (if (= (length l) 2)
      (format "~a ~a ~a" (car l) conj (cadr l))
      (apply string-append
             (let loop ([l l])
               (cond
                [(null? (cdr l)) l]
                [(null? (cddr l))
                 (list* (car l) ", " conj " " (loop (cdr l)))]
                [else
                 (list* (car l) ", " (loop (cdr l)))])))))

(define (contract-to-desc ctc)
  (with-handlers ([exn:fail:read? (lambda (exn) ctc)])
    (define s (read (open-input-string ctc)))
    (let loop ([s s])
      (cond
       [(not s) "#f"]
       [(and (symbol? s) (regexp-match? #rx"[?]$" (symbol->string s)))
	(case s
	  ((number?) "Zahl")
	  ((string?) "Zeichenkette")
	  (else
           (define str (symbol->string s))
	   (substring str 0 (sub1 (string-length str)))))]
       [(null? s) "einen unmöglichen Wert"]
       [(not (list? s)) ctc] ;; ???
       [(eq? 'or/c (car s))
        (format-enum "oder" (map loop (cdr s)))]
       [(eq? 'and/c (car s))
        (string-append "einen Wert der " (format-enum "und" (map loop (cdr s))))]
       [(eq? 'not/c (car s))
        (format "ein Wert der nicht ~a" (loop (cadr s)))]
       [(and (eq? '>/c (car s)) (zero? (cadr s)))
        "eine positive Zahl"]
       [(and (eq? '</c (car s)) (zero? (cadr s)))
        "eine negative Zahl"]
       [(and (eq? '>=/c (car s)) (zero? (cadr s)))
        "eine nicht-negative Zahl"]
       [else ctc]))))

(define (translate-pos pos)
  (cond
   ((string=? pos "1st") "erstes")
   ((string=? pos "2nd") "zweites")
   ((string=? pos "3rd") "drittes")
   ((string=? pos "4th") "viertes")
   ((string=? pos "5th") "fünftes")
   ((string=? pos "6th") "sechstes")
   ((string=? pos "7th") "siebtes")
   ((string=? pos "8th") "achtes")
   ((string=? pos "9th") "neuntes")
   ((string=? pos "10th") "zehntes")
   ((string=? pos "11th") "zehntes")
   ((regexp-match #rx"^([0-9]+)th" pos)
    => (lambda (lis)
	 (string-append (cadr lis) ".")))
   (else pos)))
   
(define (contract-error-message ctc given pos)
  (define d (contract-to-desc ctc))
  (format "~a~a~a~a erwartet, ~a bekommen"
          d
          (if pos " als " "")
          (if  pos
	       (translate-pos pos)
	       "")
          (if pos " Argument" "")
          given))

(define (expects-a all one two)
  (format "~a erwartet" two))

(define (rewrite-contract-error-message msg)
  (define replacements
    (list (list #rx"application: not a procedure;\n [^\n]*?\n  given: ([^\n]*)(?:\n  arguments[.][.][.]:(?: [[]none[]]|(?:\n   [^\n]*)*))?"
                (lambda (all one)
                  (format "Applikation: Funktion nach der öffnenden Klammer erwartet, aber ~a bekommen" one)))
          (list #rx"([^\n]*): undefined;\n cannot reference an identifier before its definition"
                (lambda (all one) (format "~a ist vor der Definition benutzt worden" one)))
          (list #rx"expects argument of type (<([^>]+)>)" expects-a)
          (list #rx"expected argument of type (<([^>]+)>)" expects-a)
          (list #rx"expects type (<([^>]+)>)" expects-a)
          (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: at least (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
                (lambda (all one two three) (argcount-error-message one two three #t)))
          (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
                (lambda (all one two three) (argcount-error-message one two three)))
          ;; see argcount-error-message/stx in racket-tests
          (list #px"([^\n]*): expects (only )?(at least )?(\\d+|no) arguments?, but found (only )?(none|\\d+)"
                (lambda (all name _only at-least expects _only2 found)
                  (argcount-error-message name expects found (and at-least #t))))
	  (list #px"([^\n]*): expects( at least)? (\\d+) arguments, but found( only)? (\\d+)"
                (lambda (all one two three four five) (argcount-error-message one three five two)))
	  (list #px"contract violation\n  expected: (.*?)\n  given: ([^\n]*)(?:\n  argument position: ([^\n]*))?"
                (lambda (all ctc given pos) (contract-error-message ctc given pos)))
          (list #rx"^procedure "
                (lambda (all) ""))
          (list #rx", given: "
                (lambda (all) ", bekommen "))
          (list #rx"; other arguments were:.*"
                (lambda (all) ""))
          (list #px"(?:\n  other arguments[.][.][.]:(?:\n   [^\n]*)*)"
                (lambda (all) ""))
          (list #rx"expects a (struct:)"
                (lambda (all one) "erwartet "))
          (list #rx"list or cyclic list"
                (lambda (all) "Liste"))
          (list #rx"assignment disallowed;\n cannot set variable before its definition\n  variable:"
                (lambda (all) "Kann Variable nicht vor der Definition setzen:"))
          (list #rx"^(.*): undefined;\n cannot use before initialization"
                (λ (all one) (format "Lokale Variable vor ihrer Definition benutzt: ~a" one)))
	  (list #rx"division by zero"
		(lambda (all) "durch 0 geteilt"))
          ;; When do these show up? I see only `#<image>' errors, currently.
          (list (regexp-quote "#(struct:object:image% ...)")
                (lambda (all) "ein Bild"))
          (list (regexp-quote "#(struct:object:image-snip% ...)")
                (lambda (all) "ein Bild"))
          (list (regexp-quote "#(struct:object:cache-image-snip% ...)")
                (lambda (all) "ein Bild"))))
  (for/fold ([msg msg]) ([repl. replacements])
    (regexp-replace* (first repl.) msg (second repl.))))

(define (rewrite-misc-error-message msg)
  (define replacements
    (list
     (list #rx"expected a `\\)` to close `\\(`"
	   (lambda (all) "zur offenen Klammer fehlt die geschlossene"))
     (list #rx"no expression after a sequence of internal definitions"
	   (lambda (all) "nach den internen Definitionen muss noch ein Ausdruck kommen"))))
  (for/fold ([msg msg]) ([repl. replacements])
    (regexp-replace* (first repl.) msg (second repl.))))

(define (get-rewriten-error-message exn)
  (if (exn:fail:contract? exn)
      (rewrite-contract-error-message (exn-message exn))
      (rewrite-misc-error-message (exn-message exn))))

(module+ test
  (require rackunit)

  (check-equal? (rewrite-contract-error-message "foo: expects only 3 arguments, but found 4")
                "foo: nur 3 Argumente erwartet, aber 4 gefunden")
  (check-equal? (rewrite-contract-error-message "foo: expects 5 arguments, but found only 4")
                "foo: 5 Argumente erwartet, aber nur 4 gefunden")
  (check-equal? (rewrite-contract-error-message "foo: expects at least 5 arguments, but found only 4")
                "foo: mindestens 5 Argumente erwartet, aber nur 4 gefunden")
  (check-equal? (rewrite-contract-error-message "foo: expects no argument, but found 4")
                "foo: kein Argument erwartet, aber 4 gefunden")
  (check-equal? (rewrite-contract-error-message "foo: expects 1 argument, but found none")
                "foo: 1 Argument erwartet, aber keins gefunden")
  (check-equal? (rewrite-contract-error-message "foo: expects at least 1 argument, but found none")
                "foo: mindestens 1 Argument erwartet, aber keins gefunden"))

  
           
