;; This module initializes readline unconditionally, "rep.rkt" uses it if we're
;; using a `terminal-port?' for input.

#lang racket/base

(provide pre-readline-input-port)
(require "pread.rkt")

(define pre-readline-input-port (current-input-port))
;; Change the input port and readline-prompt hook
(current-input-port readline-input)
(current-prompt-read read-cmdline-syntax)
