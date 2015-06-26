#lang racket/base
(require (prefix-in racket: racket/base))
(require (for-syntax (except-in mzscheme if case cond)
                     (only-in racket/base if else case cond))
         (except-in mzscheme if case cond))
(racket:provide (all-from-out mzscheme) if else case cond)
(racket:provide (for-syntax (all-from-out mzscheme) if else case cond when))



