#lang racket/base
(require racket/match
         macro-debugger/model/deriv
         macro-debugger/model/deriv-util
         (only-in mzscheme [#%top-interaction mz-top-interaction]))
(provide strip-top-interaction)

;; strip-top-interaction : Deriv -> Deriv
;; Strip off {racket,mzscheme}'s #%top-interaction
;; Careful: the #%top-interaction node may be inside of a lift-deriv
(define (strip-top-interaction deriv)
  ;; adjust-deriv/lift : Deriv -> Deriv/#f
  (define (adjust-deriv/lift deriv)
    (match deriv
      [(Wrap lift-deriv (e1 e2 first lifted-stx second))
       (let ([first (adjust-deriv/lift first)])
         (and first
              (let ([e1 (wderiv-e1 first)])
                (make-lift-deriv e1 e2 first lifted-stx second))))]
      [(Wrap ecte (e1 e2 '() first second locals2))
       ;; Only adjust if no locals...
       (let ([first (adjust-deriv/lift first)])
         (and first
              (let ([e1 (wderiv-e1 first)])
                (make ecte e1 e2 '() first second locals2))))]
      [else (adjust-deriv/top deriv)]))

  ;; adjust-deriv/top : Derivation -> Derivation
  (define (adjust-deriv/top deriv)
    (if (or (not (base? deriv))
            (syntax-original? (wderiv-e1 deriv))
            (p:module? deriv))
        deriv
        ;; It's not original...
        ;; Strip out top-interactions
        ;; Keep anything that is a non-{racket,mzscheme} top-interaction
        (cond [(for/or ([x (base-resolves deriv)]) (top-interaction-kw? x))
               (adjust-deriv/top (mrule-next deriv))]
              [else deriv])))

  (define (top-interaction-kw? x)
    (or (free-identifier=? x #'#%top-interaction)
        (free-identifier=? x #'mz-top-interaction)))

  (adjust-deriv/lift deriv))
