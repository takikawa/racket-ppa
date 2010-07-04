#lang scheme/base

(require scheme/class
	 "print.ss")

(provide (all-defined-out))

;; (make-failed-check check-fail (U #f exn)
(define-struct failed-check (reason exn?))

(define-struct check-fail (src format))

;; (make-unexpected-error src format string exn)
(define-struct (unexpected-error check-fail) (expected message exn))
;; (make-unequal src format scheme-val scheme-val)
(define-struct (unequal check-fail) (test actual))
;; (make-outofrange src format scheme-val scheme-val inexact)
(define-struct (outofrange check-fail) (test actual range))
;; (make-incorrect-error src format string exn)
(define-struct (incorrect-error check-fail) (expected message exn))
;; (make-expected-error src format string scheme-val)
(define-struct (expected-error check-fail) (message value))

;; (make-message-error src format (listof string))
(define-struct (message-error check-fail) (strings))

(define test-info-base%
  (class* object% ()
    (super-instantiate ())

    (init-field (style 'check-base))
    (field [analyses null])

    (define total-tsts 0)
    (define failed-tsts 0)
    (define total-cks 0)
    (define failed-cks 0)

    (define failures null)

    (define/public (test-style) style)
    (define/public (tests-run) total-tsts)
    (define/public (tests-failed) failed-tsts)
    (define/public (checks-run) total-cks)
    (define/public (checks-failed) failed-cks)
    (define/public (summarize-results)
      (cond [(and (zero? total-tsts) (zero? total-cks)) 'no-tests]
            [(and (zero? failed-cks) (zero? failed-tsts)) 'all-passed]
            [else 'mixed-results]))

    (define/public (failed-checks) failures)

    (define/pubment (add-check)
      (set! total-cks (add1 total-cks))
      (inner (void) add-check))

    (define/pubment (add-test)
      (set! total-tsts (add1 total-tsts))
      (inner (void) add-test))

    ;; check-failed: (U check-fail (list (U string snip%))) src (U exn false) -> void
    (define/pubment (check-failed msg src exn?)
      (set! failed-cks (add1 failed-cks))
      (let ((fail 
	     ;; We'd like every caller to make a check-fail object,
	     ;; but some (such as ProfessorJ's run time) cannot because
	     ;; of phase problems.  Therefore, do the coercion here.
	     (if (check-fail? msg)
		 msg
		 (make-message-error src #f msg))))
	(set! failures (cons (make-failed-check fail exn?) failures))
	(inner (void) check-failed fail src exn?)))

    (define/pubment (test-failed failed-info)
      (set! failed-tsts (add1 failed-tsts))
      (inner (void) test-failed failed-info))

    (define/public (add-analysis a) (set! analyses (cons a analyses)))

    (define/public (analyze-position src . vals)
      (for ([a analyses]) (send a analyze src vals)))
    (define/public (extract-info pred?)
      (filter pred? (map (lambda (a) (send a provide-info)) analyses)))))

; helper for printing error messages
(define (print-reason print-string print-formatted fail)
  (let ((print
	 (lambda (fstring . vals)
	   (apply print-with-values fstring print-string print-formatted vals)))
	(formatter (check-fail-format fail)))
    (cond
     [(unexpected-error? fail)
      (print "check-expect encountered the following error instead of the expected value, ~F. ~n   :: ~a"
	     (formatter (unexpected-error-expected fail))
	     (unexpected-error-message fail))]
     [(unequal? fail)
      (print "Actual value ~F differs from ~F, the expected value."
	     (formatter (unequal-test fail))
	     (formatter (unequal-actual fail)))]
     [(outofrange? fail)
      (print "Actual value ~F is not within ~a of expected value ~F."
	     (formatter (outofrange-test fail))
	     (formatter  (outofrange-range fail))
	     (formatter (outofrange-actual fail)))]
     [(incorrect-error? fail)
      (print "check-error encountered the following error instead of the expected ~a~n   :: ~a"
	     (incorrect-error-expected fail)
	     (incorrect-error-message fail))]
     [(expected-error? fail)
      (print "check-error expected the following error, but instead received the value ~F.~n ~a"
	     (formatter (expected-error-value fail))
	     (expected-error-message fail))]
     [(message-error? fail)
      (for-each print-formatted (message-error-strings fail))])
    (print-string "\n")))

