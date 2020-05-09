#lang racket
(require rackunit string-constants)

(check-true (string-constant-language? 'english))
(check-true (string-constant-language? 'german))
(check-false (string-constant-language? "english"))
(check-false (string-constant-language? 'this-is-not-a-real-languages-name-i-hope))

(check-equal?
 (call-with-current-language
  'english
  (λ () (dynamic-string-constant 'cancel)))
 (call-with-current-language
  'english
  (λ () (string-constant cancel))))

(check-false
 (equal? (call-with-current-language
          'english
          (λ () (string-constant cancel)))
         (call-with-current-language
          'german
          (λ () (string-constant cancel)))))
