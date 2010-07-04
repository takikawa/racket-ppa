
#lang mzscheme

(require mzlib/unit)

(provide compiler:option^
         compiler^
         compiler:inner^)

;; Compiler options
(define-signature compiler:option^
  (somewhat-verbose ; default = #f
   verbose ; default = #f
   

   setup-prefix ; string to embed in public names;
                ; used mainly for compiling extensions
                ;  with the collection name so that 
                ;  cross-extension conflicts are less
                ;  likely in architectures that expose
                ;  the public names of loaded extensions
                ; default = ""

   clean-intermediate-files ; #t => keep intermediate .c/.o files
                            ; default = #f

   3m ; #t => build for 3m
      ; default = #f

   compile-subcollections   ; #t => compile collection subdirectories
                            ; default = #t

   compile-for-embedded     ; #f => make objects to be linked
                            ; directly with Racket, not dynamically
                            ; loaded; default = #f

   max-inline-size          ; max size of inlined procedures

   disable-interrupts       ; #t => UNSAFE: turn off breaking, stack
                            ; overflow, and thread switching;
                            ; default = #f
   unsafe                   ; #t => UNSAFE: omit some type checks
                            ; default = #f
   fixnum-arithmetic        ; #t => UNSAFE: don't check for overflow or
                            ; underflow for fixnum arithmetic;
                            ; default = #f

   propagate-constants      ; default = #t
   assume-primitives        ; #t => car = #%car; default = #f
   stupid                   ; allow obvious non-syntactic errors;
                            ;  e.g.: ((lambda () 0) 1 2 3)

   vehicles                 ; Controls how closures are compiled:
                            ;  'vehicles:automatic,
                            ;  'vehicles:functions,
                            ;  'vechicles:units, or
                            ;  'vehicles:monolithic.
                            ; default = 'vehicles:automatic
   vehicles:monoliths       ; Size for 'vehicles:monolithic
   seed                     ; Randomizer seed for 'vehicles:monolithic

   max-exprs-per-top-level-set ; Number of top-level Scheme expressions
                               ; crammed into one C function; default = 25

   unpack-environments      ; default = #t
   		           ; Maybe #f helps for register-poor architectures?

   debug ; #t => creates debug.txt debugging file
   test  ; #t => ignores top-level expressions with syntax errors
   ))

;; Compiler procedures
(define-signature compiler^
  (compile-extensions
   compile-extensions-to-c
   compile-c-extensions

   compile-zos

   compile-collection-zos
   compile-directory-zos

   current-compiler-dynamic-require-wrapper
   compile-notify-handler))

;; Low-level extension compiler interface
(define-signature compiler:inner^
  (compile-extension
   compile-extension-to-c
   compile-c-extension
   eval-compile-prefix))
