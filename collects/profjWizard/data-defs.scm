#| Data Defs

 Class          = (list Name SuperClass Fields [Comment])
 ;; the name of the class, the name of the supertype ("" if none), and 
 ;; the class's fields

 DataType       = (make-union TypeName Methods VariantClasses Comment)
 ;; the name of the type and its variants
 
 VariantClasses = (Listof VariantClass)
 VariantClass   = (list Name Fields [Comment])

 Name           = String 
 TypeName       = String 
 SuperClass     = String 
 Methods        = (Listof Method)
 Method         = (cons String (cons String (listof String)))
 Fields         = (Listof Field)
 Field          = (list String String)
|#

#cs
(module data-defs mzscheme 
  
  (require (lib "string-constant.ss" "string-constants"))
  
  ;; Those languages for which methods that satisfy an interface 
  ;; don't have to be decorated with public --- 
  (define BEGINNER (string-constant profj-beginner-lang))
  (define INTERMEDIATE (string-constant profj-intermediate-lang))
  (define PROFESSIONAL (string-constant profj-full-lang))
  (define *languages* (list BEGINNER INTERMEDIATE PROFESSIONAL))
  (define Language 
    (flat-named-contract "<Language>" (lambda (x) (member x *languages*))))
  
  (provide BEGINNER INTERMEDIATE PROFESSIONAL Language)

  (define-struct dt (type methods variants purpose))

  (define (dt-fields . x) (error 'dt-fields "not implemented yet: ~a\n" x))
  (provide dt-fields)
  
  ;; Examples
  (define method1 '("int" "x"))
  (define method2 '("int" "y" "boolean"))
  (define method3 '("boolean" "b" "Foo" "Bar"))
  (define methods (list method1 method2 method3))
  (define vc1     (list "Leaf"))
  (define vc2     (list "Node" '(("ATree" "left") ("ATree" "right"))))
  (define datat1  (make-dt "ATree" methods (list vc1 vc2) "a tree for ints"))
  
  (require (file "aux-contract.scm"))
  (require (lib "contract.ss"))
  
  (provide 
   Class   ;; flat-contract
   Union   ;; flat-contract 
   Variant ;; flat-contract 
   Fields  ;; flat-contract
   Method  ;; flat-contract
   java-id? ;; Any -> Boolean
   class-purpose ;; Class -> String
   variant-purpose ;; Variant -> String
   )
  
  ;; DataType -> String
  ;; (define (union-purpose dt) (if (null? (cddr dt)) "" (caddr dt)))
  
  ;; Class -> String
  (define (class-purpose c) (if (null? (cdddr c)) "" (cadddr c)))
  
  ;; Variant -> String 
  (define (variant-purpose c) (if (null? (cddr c)) "" (caddr c)))
  
  ;; Any -> Boolean
  ;; the string isn't empty and contains no spaces 
  ;; I should really import this from Kathy's parser or whatever
  ;; so I get qualified names and whatever right
  (define (java-id? s)
    (and (string? s) (not (string=? "" s)) (not (regexp-match "[ |\t|\n]" s))))
  
  (define-as-contract "<Class>" (class c)
    (and (pair? c) (pair? (cdr c)) (pair? (cddr c)) 
         (or (null? (cdddr c))
             (and (pair? (cdddr c))
                  (null? (cddddr c))
                  (string? (cadddr c))))
         ; (list? c) (= (length c) 3)
         (java-id? (car c))
         (let ([super (cadr c)])
           (or (java-id? super) (string=? super "")))
         (is-fields? (caddr c))))
  
  (define-as-contract "<Methods>" (methods l)
    (and (list? l) (andmap is-method? l)))
  
  (define-as-contract "<Method>" (method l)
    (and (list? l) (>= (length l) 2) (andmap java-id? l)))
  
  (define-as-contract "<Fields>" (fields l)
    (and (list? l) (andmap is-field? l)))
  
  (define-as-contract "<Field in Class>" (field l)
    (and (list? l) (= (length l) 2) (andmap java-id? l)))
  
  (define-as-contract "<Union>" (union l) (dt? l)) 
  
  (define (is-variants? l) (andmap is-variant? l))
  
  (define-as-contract "<Variant>" (variant c) 
    (and (pair? c) (pair? (cdr c)) 
         (or
          (null? (cddr c))
          (and
           (pair? (cddr c))
           (null? (cdddr c))
           (string? (caddr c)))
          ; (list? c) (= (length c) 2)
          (java-id? (car c))
          (is-fields? (cadr c)))))
  
  (provide/contract 
   (struct dt ((type java-id?)
               (methods (listof is-method?))
               (variants (listof is-variant?))
               (purpose string?))))
  
  
  #| Tests: 
  (require (lib "testing.scm" "testing"))
  
  (test== (java-id? "oops no") #f)
  (test== (java-id? " oops 2") #f)
  (test== (java-id? " oops2 ") #f)
  (test== (java-id? "") #f)
  (test== (java-id? (string #\tab)) #f)
  (test== (java-id? (string #\newline)) #f)
  
  (test== (is-class? '("Foo" "" ())) #t)
  (test== (is-class? '("Foo" "" () "hello world")) #t)
  (test== (is-class? '("Foo" "Moo" (("int" "x") ("int" "y")) "hello world")) #t)
  
  (test== (is-class? '("Foo" "Moo")) #f "no fields")
  (test== (is-class? '("Foo" "Moo Oops" ())) #f "space in super name")
  
  (test== (class-purpose '("a" "b" ())) "")
  (test== (class-purpose '("a" "b" () "hello world")) "hello world")
  
  (test== (is-variant? (list "B" '())) #t "variant class")
  (test== (andmap is-variant?  (list (list "B" '()) (list "C" '()))) #t "variants")
  (test== (java-id? "A") #t)
  (test== (is-union? (make-dt "A" '() (list (list "B" '()) (list "C" '())) "")) #t)
  
  (test== (is-method? method1) #t)
  (test== (is-method? method2) #t)
  (test== (is-method? method3) #t)
  (test== (is-methods? methods) #t)
|#
  )
