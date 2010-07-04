#lang scheme/base

(require "private/honu.ss"
         "private/macro.ss")

(provide int real bool obj 
         function var const
         string
         -> >->
         \;
         ? :
         && \|\|
         /
         < > <= >=
         !=
         cons list
         true false
         display write newline
         #%datum
         #%top
         #%parens #%brackets #%braces #%angles
         #%prefix #%postfix
         ;; define-honu-syntax
         (for-syntax ...)

         (rename-out (set! =)
                     (honu-return return)
                     (honu-if if)
                     (honu-macro macro)
                     (honu-time time)
                     (honu-class class)
                     (honu+ +)
                     (honu- -)
                     (honu* *) 
                     (do do)
                     (honu-end end)
                     (modulo %)
                     (equal? ==)
                     (string->number stringToNumber)
                     (number->string numberToString)
                     (car first) 
                     (cdr rest)
                     (null empty)
                     (null? isEmpty)
                     (pair? isCons)
                     (#%dynamic-honu-module-begin #%module-begin)
                     (honu-#%app #%app)
                     (honu-top #%top-interaction)
                     (honu-provide provide)
                     (honu-require require)))
