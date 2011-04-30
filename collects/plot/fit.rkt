(module fit mzscheme
  (require plot/math)
  (require plot/fit-low-level)

  ; a structure contain a the results of a curve-fit
  (define-struct fit-result (
                             rms
                             variance
                             names
                             final-params
                             std-error
                             std-error-percent
                             function
                             ) (make-inspector))

  ; fit-int : (number* -> number) (list-of (symbol number)) (list-of (vector number [number] number number)) -> fit-result
  (define (fit-int function guesses data)
    (let* ((independent-vars (- (procedure-arity function) (length guesses)))
           (f-of-x-y (cond
                       [(= 1 independent-vars)
                        (lambda (x y . rest)
                          (apply function x rest))]
                       [(= 2 independent-vars)
                        function]
                       [else
                        (error "Function provided is either not of one or two independent variables or the number of
                        guesses given is incorrect")]))
           (x-vals (map vector-x data))
           (y-vals (if (= 1 independent-vars)
                       x-vals
                        (map vector-y data)))
           (z-vals (if (= 1 independent-vars)
                       (map vector-y data)
                       (map vector-z data)))
           (err-vals (if (= 1 independent-vars)
                         (map vector-z data)
                         (map (lambda (vec) (vector-ref vec 4)) data)))
           (result (fit-internal f-of-x-y x-vals y-vals z-vals err-vals (map cadr guesses))))
      (if (null? result)
          null
          (begin
            ;(display result)
            (make-fit-result
             (list-ref result 3)
             (list-ref result 4)
             (map car guesses)
             (car result)
             (cadr result)
             (caddr result)
             (lambda args (apply function(append args (car result)))))))))

   (provide fit-int
            (struct fit-result (rms variance names final-params
                                std-error std-error-percent function))))
