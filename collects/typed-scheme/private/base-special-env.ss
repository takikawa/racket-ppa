#lang scheme/base

;; these are libraries providing functions we add types to that are not in scheme/base
(require
 "extra-procs.ss"
 "../utils/utils.ss"
 (only-in scheme/list cons? take drop add-between last filter-map)
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 (only-in scheme/match/runtime match:error)
 scheme/promise
 string-constants/string-constant)



;; these are all for constructing the types given to variables
(require (for-syntax
          scheme/base
          (env init-envs)          
          (except-in (rep effect-rep type-rep) make-arr)
          "type-effect-convenience.ss"
          (only-in "type-effect-convenience.ss" [make-arr* make-arr])
          "union.ss"
          (typecheck tc-structs)))

(define-for-syntax (initialize-others) 
  (d-s date 
       ([second : N] [minute : N] [hour : N] [day : N] [month : N] 
        [year : N] [weekday : N] [year-day : N] [dst? : B] [time-zone-offset : N])
       ())
  (d-s exn ([message : -String] [continuation-marks : Univ]) ())
  (d-s (exn:fail exn) () (-String Univ))
  (d-s (exn:fail:read exn:fail) ([srclocs : (-lst Univ)]) (-String Univ))
  )

(provide (for-syntax initial-env/special-case initialize-others initialize-type-env)
         define-initial-env)

(define-syntax (define-initial-env stx)
    (syntax-case stx ()
      [(_ initial-env make-promise-ty language-ty qq-append-ty [id ty] ...)
       (with-syntax ([(_ make-promise . _)
                      (local-expand #'(delay 3)
                                    'expression
                                    null)]
                     [language
                      (local-expand #'(this-language)
                                    'expression
                                    null)]
                     [(_ qq-append . _)
                      (local-expand #'`(,@'() 1)
                                    'expression
                                    null)])
         #`(define-for-syntax initial-env
             (make-env
              [make-promise make-promise-ty]
              [language language-ty]
              [qq-append qq-append-ty]
              [id ty] ...)))]))




(define-initial-env initial-env/special-case
  ;; make-promise
  (-poly (a) (-> (-> a) (-Promise a)))
  ;; language
  Sym
  ;; qq-append
  (-poly (a b) 
         (cl->*
          (-> (-lst a) (-val '()) (-lst a))
          (-> (-lst a) (-lst b) (-lst (*Un a b))))))
     
     
     

(begin-for-syntax   
  (initialize-type-env initial-env/special-case)
  (initialize-others))



