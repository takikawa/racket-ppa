#lang racket/base

;; This file needs to be deleted -- it is now superseded by
;; `setup/path-relativize'.

;; Unstable library by: Carl Eastlund <cce@ccs.neu.edu>
;; intended for use in racket/contract, so don't try to add contracts!

(require setup/path-relativize
         setup/dirs
         (only-in planet/config [CACHE-DIR find-planet-dir]))

(provide path->directory-relative-string
         library-relative-directories
         setup-relative-directories)

(define library-relative-directories
  (list (cons find-collects-dir 'collects)
        (cons find-user-collects-dir 'user-collects)
        (cons find-planet-dir 'planet)))

(define setup-relative-directories
  (list (cons find-collects-dir #f)
        (cons find-user-collects-dir 'user)
        (cons find-planet-dir 'planet)))

(define (path->directory-relative-string
         path
         #:default [default (if (path? path) (path->string path) path)]
         #:dirs [dirs library-relative-directories])

  (unless (path-string? path)
    (error 'path->directory-relative-string
      "expected a path or a string (first argument); got: ~e" path))

  (unless (and (list? dirs) (andmap pair? dirs))
    (error 'path->directory-relative-string
      "expected an association list (#:dirs keyword argument); got: ~e" dirs))

  (let/ec return

    (when (complete-path? path)
      (for ([dir-entry (in-list dirs)])
        (define find-dir (car dir-entry))
        (define dir-name (cdr dir-entry))

        (unless (and (procedure? find-dir)
                     (procedure-arity-includes? find-dir 0))
          (error 'path->directory-relative-string
            "expected keys in association list to be thunks (~a); got: ~e"
            "#:dirs keyword argument"
            find-dir))

        (let ()

          (define-values [ path->relative relative->path ]
            (make-relativize find-dir
                             'relative
                             'path->relative
                             'relative->path))
          (define exploded
            (with-handlers ([exn:fail? (lambda (e) #f)])
              (path->relative path)))
          (when (list? exploded)
            (let ([relative (path->string
                             (apply build-path
                               (map bytes->path-element (cdr exploded))))])
              (return
               (if dir-name
                 (format "<~a>/~a" dir-name relative)
                 relative)))))))
    default))
