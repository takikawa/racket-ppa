#lang racket/base

(require typed-racket/utils/utils
         (prefix-in ce: test-engine/racket-tests)
         (for-syntax
          racket/base syntax/parse
          (utils tc-utils)
          (env init-envs)
          (rep filter-rep object-rep type-rep)
          (types abbrev numeric-tower union)))

(define-for-syntax ce-env
  (make-env
   ;; test*
   [(syntax-parse (local-expand #'(ce:test) 'expression null)
      #:context #'ce:test
      [(_ ce-t:id) #'ce-t])
    (-> -Void)]
   ;; insert-test
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
       #'insert-test])
    (Univ (-> Univ) . -> . -Void)]
   ;; builder
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ (nvv _ _ builder _)))
           _))
       #'builder])
    (-> Univ)]
   ;; check-values-expected
   [(syntax-parse (local-expand #'(ce:check-expect 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-values-expected _ _ _ _))))))
       #'check-values-expected])
    ((-> Univ) Univ Univ Univ . -> . -Boolean)]
   ;; check-values-within
   [(syntax-parse (local-expand #'(ce:check-within 1 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-values-within _ _ _ _ _))))))
       #'check-values-within])
    ((-> Univ) Univ -Real Univ Univ . -> . -Boolean)]
   ;; check-values-error
   [(syntax-parse (local-expand #'(ce:check-error 1 "foo") 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-values-error _ _ _ _))))))
       #'check-values-error])
    ((-> Univ) -String Univ Univ . -> . -Boolean)]
   ;; check-range-values-expected
   [(syntax-parse (local-expand #'(ce:check-range 1 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-range-values-expected _ _ _ _ _))))))
       #'check-range-values-expected])
    ((-> -Real) -Real -Real Univ Univ . -> . -Boolean)]
   ;; check-member-of-values-expected
   [(syntax-parse (local-expand #'(ce:check-member-of 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-member-of-values-expected _ _ _ _ _))))))
       #'check-member-of-values-expected])
    ((-> Univ) Univ (-lst Univ) Univ Univ . -> . -Boolean)]
   ;; check-random-values
   [(syntax-parse (local-expand #'(ce:check-random 1 1) 'module #f)
      #:literals (let* when define-values)
      [(define-values _
         (let* ((_ _) (_ _))
           (when _
             (insert-test _ (lambda () (check-random-values _ _ _ _))))))
       #'check-random-values])
    ((-> Univ) (-> Univ) (-lst Univ) Univ . -> . -Boolean)]))

(begin-for-syntax (initialize-type-env ce-env))
