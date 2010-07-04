(module installer mzscheme
  (require (prefix lang: (lib "installer.ss" "profj" "libs" "java" "lang"))
           (prefix io: (lib "installer.ss" "profj" "libs" "java" "io"))
           (prefix test: (lib "installer.ss" "profj" "libs" "java" "tester"))
           (prefix util: (lib "installer.ss" "profj" "libs" "java" "util"))
           )
  (provide installer)

  (define (installer plthome)
    (io:installer plthome)
    (lang:installer plthome)
    (test:installer plthome)
    (util:installer plthome)))
