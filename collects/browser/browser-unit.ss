(module browser-unit mzscheme
  (require mzlib/unit
           (lib "mred-sig.ss" "mred")
           (lib "plt-installer-sig.ss" "setup")
           net/tcp-sig
           net/url-sig
           net/url-unit
           "browser-sig.ss"
           "private/sig.ss"
           "private/bullet.ss"
           "private/html.ss"
           "private/hyper.ss")
  
  (provide browser@)
  
  (define-unit-from-context bullet@ bullet-export^)
  
  (define-compound-unit/infer pre-browser@
    (import setup:plt-installer^
            mred^
            url^)
    (export hyper^ html-export^ bullet-export^)
    (link html@ hyper@ bullet@))

  (define-unit/new-import-export browser@
    (import setup:plt-installer^
            mred^
            url^)
    (export browser^)
    ((hyper^ html-export^ bullet-export^) 
     pre-browser@ 
     setup:plt-installer^
     mred^
     url^)))


  
  
