(require
 "utils.ss"
 mzlib/etc
 mzlib/class
 mred
 (lib "private/aligned-pasteboard/snip-lib.ss" "mrlib")
 (lib "private/aligned-pasteboard/aligned-pasteboard.ss" "mrlib")
 (lib "private/aligned-pasteboard/aligned-editor-container.ss" "mrlib"))

(printf "running tests for snip-lib.ss~n")
  
;;snip-min-width: ((is-a?/c snip%) . -> . number?)
;;the width of a snip in the given pasteboard
(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [es1 (instantiate editor-snip% () (editor pb1))]
     [pb2 (instantiate vertical-pasteboard% ())]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb2))])
  (send frame show true)
  
  (send pb2 insert es1)
  (send es1 resize 20 20)
  (sleep/yield 1)
  (test
   =
   (snip-min-width es1)
   20)
  
  (send es1 resize 200 90)
  (sleep/yield 1)
  (test
   =
   (snip-min-width es1)
   200)
  
  (send frame show false)
  )

;;snip-min-height: ((is-a?/c snip%) . -> . number?)
;;the height of a snip in the given pasteboard
(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [es1 (instantiate editor-snip% () (editor pb1))]
     [pb2 (instantiate vertical-pasteboard% ())]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb2))])
  (send frame show true)
  
  (send pb2 insert es1)
  (send es1 resize 20 20)
  (sleep/yield 1)
  (test
   =
   (snip-min-height es1)
   20)
  
  (send es1 resize 200 90)
  (sleep/yield 1)
  (test
   =
   (snip-min-height es1)
   90)
  
  (send frame show false)
  )

;;snip-parent: ((is-a?/c snip%) . -> . (is-a?/c editor<%>))
;;the pasteboard that contains the snip
(let*
    ([pb1 (instantiate pasteboard% ())]
     [es1 (instantiate editor-snip% () (editor pb1))]
     [pb2 (instantiate pasteboard% ())]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate editor-canvas% () (parent frame) (editor pb2))])
  (send frame show true)
  
  (send pb2 insert es1)
  
  (test
   equal?
   (snip-parent es1)
   pb2)
  
  (send frame show false)
  )

(let*
    ([pb1 (instantiate horizontal-pasteboard% ())]
     [pb2 (instantiate horizontal-pasteboard% ())]
     [pb3 (instantiate horizontal-pasteboard% ())]
     [pb4 (instantiate horizontal-pasteboard% ())]
     [pb5 (instantiate horizontal-pasteboard% ())]
     [es2 (instantiate aligned-editor-snip% () (editor pb2))]
     [es3 (instantiate aligned-editor-snip% () (editor pb3))]
     [es4 (instantiate aligned-editor-snip% () (editor pb4))]
     [es5 (instantiate aligned-editor-snip% () (editor pb5))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))])
  (send frame show true)
  (send pb1 insert es2)
  (send pb2 insert es3)
  (send pb3 insert es4)
  (send pb4 insert es5)
  
  (test
   equal?
   (snip-parent es2)
   pb1)
  
  (test
   equal?
   (snip-parent es3)
   pb2)
  
  (test
   equal?
   (snip-parent es4)
   pb3)
  
  (test
   equal?
   (snip-parent es5)
   pb4)
  
  (send frame show false)
  )

;;fold-snip: (lambda (b?) ((any? b? . -> . b?) b? (is-a?/c snip%) . -> . b?))
;;the application of f on all snips from snip to the end in a foldl foldr mannor
(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [es1 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es2 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es3 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es4 (instantiate editor-snip% () (editor (instantiate text% ())))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))])
  (send frame show true)
  
  (send pb1 insert es1)
  (send pb1 insert es2)
  (send pb1 insert es3)
  (send pb1 insert es4)
  
  (send es1 resize 100 100)
  (send es2 resize 100 100)
  (send es3 resize 100 100)
  (send es4 resize 100 100)
  
  (test
   =
   (fold-snip
    (lambda (snip total-height)
      (+ (snip-min-height snip)
         total-height))
    0
    es4)
   400)
  
  (send frame show false)
  )

  
;;for-each-snip: (((is-a?/c snip%) . -> . (void)) (is-a/c? snip%) . -> . (void))
;;applies the function to all the snips
(let*
    ([pb1 (instantiate vertical-pasteboard% ())]
     [es1 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es2 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es3 (instantiate editor-snip% () (editor (instantiate text% ())))]
     [es4 (instantiate editor-snip% () (editor (instantiate text% ())))]
     
     [frame (instantiate frame% () (label "l") (width 10) (height 10))]
     [canvas (instantiate aligned-editor-canvas% () (parent frame) (editor pb1))]
     [count 0])
  (send frame show true)
  
  (send pb1 insert es1)
  (send pb1 insert es2)
  (send pb1 insert es3)
  (send pb1 insert es4)
  
  (for-each-snip
   (lambda (snip)
     (set! count (add1 count)))
   es4)
  
  (test
   =
   count
   4)
  
  (send frame show false)
  )
(tests-done)
