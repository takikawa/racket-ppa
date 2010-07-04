#lang scheme/base
(require (planet schematics/schemeunit:3)
         web-server/private/mod-map
         mzlib/serialize
         "../util.ss")
(provide mod-map-tests)

(define (simplify-unsimplify v)
  (decompress-serial
   (compress-serial
    v)))
(define (cidentity v)
  (deserialize 
   (simplify-unsimplify 
    (serialize v))))

(define the-dispatch
  `(lambda (k*v)
     (lambda (k*v)
       ((car k*v) k*v))))  

(define m00 '(lib "mm00.ss" "web-server" "default-web-root" "htdocs" "lang-servlets")) 
(define m01 '(lib "mm01.ss" "web-server" "default-web-root" "htdocs" "lang-servlets")) 

(define mod-map-tests
  (test-suite
   "Module Map"
   
   (test-suite
    "(compose decompress-serial compress-serial) is identity"
    (test-case "Integers" (check-equal? (cidentity 3) 3))
    (test-case "Symbols" (check-equal? (cidentity 'foo) 'foo))
    (test-case "Strings" (check-equal? (cidentity "Bar") "Bar"))
    (test-case "Vectors" (check-equal? (cidentity (vector 3 1 4)) (vector 3 1 4))))
   
   (test-case
    "Use compress-serial and decompress-serial with lang.ss (1)"
    (let-values ([(ev) (make-eval/mod-path m00)])
      (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start start 'foo))))]
             [k1 (simplify-unsimplify (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k0) 1)))))]
             [k2 (simplify-unsimplify (ev `(serialize (dispatch ,the-dispatch (list (deserialize ',k1) 2)))))])
        (check-true (= 6 (ev `(dispatch ,the-dispatch (list (deserialize ',k2) 3))))))))
   
   (test-case
    "Use compress-serial and decompress-serial with lang.ss (2)"
    (let-values ([(ev) (make-eval/mod-path m01)])
      (let* ([k0 (simplify-unsimplify (ev '(serialize (dispatch-start start 'foo))))])
        (check-true (= 7 (ev `(dispatch ,the-dispatch (list (deserialize ',k0) 7))))))))))
