#lang setup/infotab

(define raco-commands
  '(("make" compiler/commands/make "compile source to bytecode" 100)
    ("exe" compiler/commands/exe "create executable" 20)
    ("pack" compiler/commands/pack "pack files/collections into a .plt archive" 10)
    ("decompile" compiler/commands/decompile "decompile bytecode" #f)
    ("expand" compiler/commands/expand "macro-expand source" #f)
    ("distribute" compiler/commands/exe-dir "prepare executable(s) in a directory for distribution" #f)
    ("ctool" compiler/commands/ctool "compile and link C-based extensions" #f)
    ("demodularize" compiler/demodularizer/batch "produce a whole program from a single module" #f)))
