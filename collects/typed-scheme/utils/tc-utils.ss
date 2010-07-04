#lang scheme/base

#|
This file is for utilities that are only useful for Typed Scheme, but
don't depend on any other portion of the system
|#

(provide (all-defined-out))
(require "syntax-traversal.ss" syntax/parse (for-syntax scheme/base syntax/parse) scheme/match)

;; a parameter representing the original location of the syntax being currently checked
(define current-orig-stx (make-parameter #'here))
(define orig-module-stx (make-parameter #f))
(define expanded-module-stx (make-parameter #f))

(define (stringify l [between " "])
  (define (intersperse v l)
    (cond [(null? l) null]
          [(null? (cdr l)) l]
          [else (cons (car l) (cons v (intersperse v (cdr l))))]))
  (apply string-append (intersperse between (map (lambda (s) (format "~a" s)) l))))

;; helper function, not currently used
(define (find-origin stx)
  (cond [(syntax-property stx 'origin) => (lambda (orig)
                                            (let ([r (reverse orig)])
                                              (let loop ([r (reverse orig)])
                                                (if (null? r) #f
                                                    (if (syntax-source (car r)) (car r)
                                                        (loop (cdr r)))))))]
        [else #f]))

;; do we print the fully-expanded syntax in error messages?
(define print-syntax? (make-parameter #f))


(define check-unreachable-code? (make-parameter #f))

(define warn-unreachable? (make-parameter #t))

(define (warn-unreachable e)
  (let ([l (current-logger)])
    (when (and (warn-unreachable?)
               (log-level? l 'warning)
               (and (orig-module-stx) (eq? (syntax-source-module e) (syntax-source-module (orig-module-stx))))
	       (syntax-source-module e))
      (log-message l 'warning (format "Typed Scheme has detected unreachable code: ~e" (syntax->datum (locate-stx e)))
                   e))))

(define (locate-stx stx)
  (define omodule (orig-module-stx))
  (define emodule (expanded-module-stx))
  ;(printf "orig: ~a~n" (syntax-object->datum omodule))
  ;(printf "exp: ~a~n" (syntax-object->datum emodule))
  ;(printf "stx (locate): ~a~n" (syntax-object->datum stx))
  (if (and (not (print-syntax?)) omodule emodule stx)
      (or (look-for-in-orig omodule emodule stx) stx)
      stx))

(define (raise-typecheck-error msg stxs)
  (raise (make-exn:fail:syntax (string-append "typecheck: " msg)
                               (current-continuation-marks)
                               stxs)))

(define delayed-errors null)

(define-struct err (msg stx) #:prefab)

(define-values (save-errors! restore-errors!)
  (let ([v (box #f)])
    (values (lambda () (set-box! v delayed-errors))
            (lambda () (set! delayed-errors (unbox v))))))

(define (report-all-errors)
  (define (reset!) (set! delayed-errors null))
  (match (reverse delayed-errors)
    [(list) (void)]
    [(list (struct err (msg stx)))
     (reset!)
     (raise-typecheck-error msg stx)]
    [l
     (let ([stxs
            (for/list ([e l])
              (sync (thread (lambda () (raise-typecheck-error (err-msg e) (err-stx e)))))
              (err-stx e))])
       (reset!)
       (unless (null? stxs)
         (raise-typecheck-error (format "Summary: ~a errors encountered" (length stxs)) (apply append stxs))))]))

(define delay-errors? (make-parameter #f))

(define (tc-error/delayed msg #:stx [stx* (current-orig-stx)] . rest)
  (let ([stx (locate-stx stx*)])
    (unless (syntax? stx)
      (int-err "erroneous syntax was not a syntax object: ~a ~a" stx (syntax->datum stx*)))
    (if (delay-errors?)
        (set! delayed-errors (cons (make-err (apply format msg rest) (list stx)) delayed-errors))
        (raise-typecheck-error (apply format msg rest) (list stx)))))

;; produce a type error, using the current syntax
(define (tc-error msg . rest)  
  (let ([stx (locate-stx (current-orig-stx))])
    ;; If this isn't original syntax, then we can get some pretty bogus error messages.  Note
    ;; that this is from a macro expansion, so that introduced vars and such don't confuse the user.
    (cond
      [(not (orig-module-stx))
       (raise-typecheck-error (apply format msg rest) (list stx))]
      [(eq? (syntax-source (current-orig-stx)) (syntax-source (orig-module-stx)))
       (raise-typecheck-error (apply format msg rest) (list stx))]
      [else (raise-typecheck-error (apply format (string-append "Error in macro expansion -- " msg) rest) (list stx))])))

;; produce a type error, given a particular syntax
(define (tc-error/stx stx msg . rest)
  (parameterize ([current-orig-stx stx])
    (apply tc-error msg rest)))

;; check two identifiers to see if they have the same name
(define (symbolic-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

;; parameter for currently-defined type aliases
;; this is used only for printing type names
(define current-type-names (make-parameter (lambda () '())))

;; for reporting internal errors in the type checker
(define-struct (exn:fail:tc exn:fail) ())

;; raise an internal error - typechecker bug!
(define (int-err msg . args)  
  (raise (make-exn:fail:tc (string-append "Internal Typechecker Error: "
                                          (apply format msg args)
                                          (format "\nwhile typechecking\n~a" (syntax->datum (current-orig-stx))))
                           (current-continuation-marks))))

(define-syntax (nyi stx)
  (syntax-case stx ()
    [(_ str)
     (quasisyntax/loc stx (int-err "~a: not yet implemented: ~a" str #,(syntax/loc stx (this-expression-file-name))))]
    [(_) (syntax/loc stx (nyi ""))]))


;; are we currently expanding in a typed module (or top-level form)?
(define typed-context? (box #f))

;; what type names have been referred to in this module?
(define type-name-references (make-parameter '()))

(define (add-type-name-reference t)
  (type-name-references (cons t (type-name-references))))

;; environment constructor
(define-syntax (make-env stx)
  (define-syntax-class spec
    #:transparent
    #:attributes (ty id)
    (pattern [nm:identifier ty]
             #:with id #'#'nm)
    (pattern [e:expr ty extra-mods ...]
             #:with id #'(let ([new-ns
                                (let* ([ns (make-empty-namespace)])
                                  (namespace-attach-module (current-namespace)
                                                           'scheme/base
                                                           ns)
                                  ns)])
                           (parameterize ([current-namespace new-ns])
                             (namespace-require 'scheme/base)
                             (namespace-require 'extra-mods) ...
                             e))))
  (syntax-parse stx
    [(_ e:spec ...)
     #'(list (list e.id e.ty) ...)]))

;; id: identifier
;; sym: a symbol
;; mod: a quoted require spec like 'scheme/base
;; is id the name sym defined in mod?
(define (id-from? id sym mod)
  (and (eq? (syntax-e id) sym)
       (eq? (module-path-index-resolve (syntax-source-module id))
            ((current-module-name-resolver) mod #f #f #f))))

(define-syntax-class (id-from sym mod)
  (pattern i:id
           #:fail-unless (id-from? #'i sym mod) #f))
