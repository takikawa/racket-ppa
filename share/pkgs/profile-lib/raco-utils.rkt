#lang racket/base

(provide module-to-profile)

(define (module-to-profile file)
  ;; check if there's a main submodule
  (define file-path `(file ,file))
  (define main-path `(submod ,file-path main))
  (dynamic-require file-path (void)) ; visit the module, but don't run it
  (if (module-declared? main-path #f)
      main-path
      file-path))
