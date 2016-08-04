#lang info

(define raco-commands
  '(("check-requires"
     (submod macro-debugger/analysis/check-requires main)
     "check for useless requires"
     #f)
    ("show-dependencies"
     (submod macro-debugger/analysis/show-dependencies main)
     "show module dependencies"
     #f)
    ("macro-profiler"
     (submod macro-debugger/analysis/profile main)
     "profile macro expansion (code size)"
     #f)))
