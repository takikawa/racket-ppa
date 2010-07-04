(module core-layout-test mzscheme
  (require "core-layout.ss"
           "loc-wrapper.ss"
           "lw-test-util.ss"
           "test-util.ss"
           (lib "struct.ss"))
  
  (require (lib "mrpict.ss" "texpict")
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  (dc-for-text-size (make-object bitmap-dc% (make-object bitmap% 1 1)))
  
  (reset-count)
  
  (let ([content
         (list (make-lw 'x 15 1 35 0 #f #f)
               (make-lw (list
                         (make-lw "(" 15 0 35 1 #f #f)
                         (make-lw 'a 15 0 36 1 #f #f)
                         (make-lw 'b 16 0 36 1 #f #f)
                         (make-lw ")" 16 0 37 1 #f #f))
                        15 1 35 3 #f #f))])
    (test (find-enclosing-loc-wrapper content)
          (build-lw content
                    15 1 35 3)))
  
  (define (replace-pict-tokens x)
    (let loop ([x x])
      (cond
        [(pair? x) (cons (loop (car x))
                         (loop (cdr x)))]
        [(pict-token? x)
         (copy-struct pict-token x [pict-token-pict 'pict])]
        [else x])))
  
  (test (replace-pict-tokens
         (build-lines
          '()
          (normalize-lw
           (to-lw
            ,(term
              (a b c))))))
        (list (list (make-spacer-token 0 2)
                    (make-string-token 2 1 "(" 'roman)
                    (make-string-token 3 1 "a" 'swiss)
                    (make-string-token 4 1 " " 'roman)
                    (make-string-token 5 1 "b" 'swiss)
                    (make-string-token 6 1 " " 'roman)
                    (make-string-token 7 1 "c" 'swiss)
                    (make-string-token 8 1 ")" 'roman))
              (list (make-string-token 0 0 "" 'roman) 
                    (make-pict-token 0 1 'pict)
                    (make-pict-token 1 0 'pict))))

  (print-tests-passed "core-layout.ss"))
