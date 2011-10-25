(module term-test scheme
  (require "../private/term.rkt"
           "../private/matcher.rkt"
           "test-util.rkt")
  
  (reset-count)
  (test (term 1) 1)
  (test (term (1 2)) (list 1 2))
  (test (term (1 ,(+ 1 1))) (list 1 2))
  (test (term-let ([x 1]) (term (x x))) (list 1 1))
  (test (term-let ([(x ...) (list 1 2 3)]) (term ((y x) ...))) '((y 1) (y 2) (y 3)))
  
  (test (term (in-hole (1 hole) 2)) (term (1 2)))
  (test (term (in-hole (1 hole (hole x)) 2)) (term (1 2 (hole x))))
  
  (test (equal? (term hole) (term hole)) #t)
  (test (hole? (term hole)) #t)
  (test (hole? (term (hole #f))) #f)
  (test (hole? (term (hole the-name))) #f)
  
  (test (term-let-fn ((f (lambda (q) q)))
                     (term (f 1 2 3)))
        (term (1 2 3)))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term (f (zzzz))))
        (term (y (zzzz))))
  
  (test (term-let-fn ((f (λ (x) (add1 (car x)))))
                     (term (f 2)))
        (term 3))
  
  (test (term-let ([((x ...) ...) (list (list 1 1) (list 2 2) (list 3 3))])
          (term-let-fn ((f (λ (x) (car x))))
                       (term ((qq (f x) ...) ...))))
        (term ((qq 1 1) (qq 2 2) (qq 3 3))))
  
  (test (term-let-fn ((f (lambda (x) (car x))))
                     (term (f hole)))
        (term hole))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term-let-fn ((g (lambda (x) `(ff ,(car x)))))
                                  (term (g (f (zzzz))))))
        (term (ff (y (zzzz)))))
  
  (test (term-let-fn ((f (lambda (q) `(y ,(car q)))))
                     (term-let-fn ((g (lambda (x) `(ff ,(car x)))))
                                  (term (f (g (f (zzzz)))))))
        (term (y (ff (y (zzzz))))))
  
  (test (term-let ([x 1])
          (term (x . y)))
        (term (1 . y)))
  
  (test (term-let ([(x ...) (list 3 2 1)])
          (term (x ... . y)))
        (term (3 2 1 . y)))
  
  (test (term-let ([(x . y) (cons 1 2)])
          (term (x y)))
        (term (1 2)))
  
  ;; test that the implicit `plug' inserted by `in-hole' 
  ;; deals with ellipses properly
  (test (term-let ([(E ...) '(1 2 3)])
          (term ((in-hole E x) ...)))
        (term (1 2 3)))
  
  (test (term-let-fn ((metafun car))
                     (term-let ((x 'whatever)
                                ((y ...) '(4 5 6)))
                       (term (((metafun x) y) ...))))
        '((whatever 4) (whatever 5) (whatever 6)))
  
  (test (term-let-fn ((metafun (λ (x) (car x))))
                     (term-let (((y ...) '(4 5 6)))
                       (term ((y (metafun 1)) ...))))
        '((4 1) (5 1) (6 1)))
  
  (test (term-let-fn ((f (compose add1 car)))
                     (term-let (((x ...) '(1 2 3))
                                ((y ...) '(a b c)))
                               (term (((f x) y) ...))))
        '((2 a) (3 b) (4 c)))
  
  (test (term-let-fn ((f (curry foldl + 0)))
                     (term-let (((x ...) '(1 2 3)))
                               (term (f x ...))))
        6)
  
  (test (term-let-fn ((f (compose add1 car)))
                     (term-let (((x ...) '(1 2 3))
                                (((y ...) ...) '((a b c) (d e f) (g h i))))
                               (term ((((f x) y) ...) ...))))
        '(((2 a) (3 b) (4 c)) ((2 d) (3 e) (4 f)) ((2 g) (3 h) (4 i))))
  
  (test (term-let-fn ((f (curry foldl + 0)))
                     (term-let ((((x ...) ...) '((1 2) (3 4 5) (6))))
                               (term ((f x ...) ...))))
        '(3 12 6))

  (define-namespace-anchor here)
  (define ns (namespace-anchor->namespace here))
  
  (let ([src 'term-template])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source
        '(term-let ([(x ...) '(a b c)]
                    [((y ...) ...) '((1 2) (4 5 6) (7 8 9))])
                   (term (((x y) ...) ...)))
        src))
     src))
  
  (let ([src 'term-template-metafunc])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source 
        '(term-let-fn ((f car))
                      (term-let ([(x ...) '(a b c)]
                                 [((y ...) ...) '((1 2) (4 5 6) (7 8 9))])
                                (term ((((f x) y) ...) ...))))
        src))
     src))
  
  (let ([src 'ellipsis-args])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source 
        '(term-let-fn ((f car))
                      (term-let ([(x ...) '(a b)]
                                 [(y ...) '(c d e)])
                                (term (f ((x y) ...)))))
        src))
     src))
  
  (let ([src 'ellipsis-args/map])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source 
        '(term-let-fn ((f car))
                      (term-let ([(x ...) '(a b)]
                                 [(y ...) '(c d e)])
                                (term ((f (x y)) ...))))
        src))
     src))
  
  (let ([src 'ellipsis-args/in-hole])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source 
        '(term-let ([(x ...) '(a b)]
                    [(y ...) '(c d e)])
                   (term ((in-hole hole (x y)) ...)))
        src))
     src))
  
  (let ([src 'term-let-rhs])
    (test
     (parameterize ([current-namespace ns])
       (runtime-error-source
        '(term-let ([(x ...) 'a])
                   3)
        src))
     src))
  
  (test-syn-err (term-let ([(x ...) '(a b c)]) (term x))
                #rx"missing ellipses")

  (test (parameterize ([current-namespace syn-err-test-namespace])
          (with-handlers ([exn:fail:syntax?
                           (λ (exn)
                             (match (exn:fail:syntax-exprs exn)
                               [(list e) (syntax->datum e)]
                               [_ (gensym 'wrong)]))])
            (expand
             '(term-let ([((label ...) ...) '()])
                        (term (label ...))))
            (gensym 'wrong)))
        'label)
  
  (print-tests-passed 'term-test.rkt))
