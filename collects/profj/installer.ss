(module installer mzscheme
  (require (prefix lang: profj/libs/java/lang/installer)
           (prefix io: profj/libs/java/io/installer)
           (prefix test: profj/libs/java/tester/installer)
           (prefix util: profj/libs/java/util/installer)
           )
  (provide installer)

  (define (installer plthome)
    (io:installer plthome)
    (lang:installer plthome)
    (test:installer plthome)
    (util:installer plthome)))
