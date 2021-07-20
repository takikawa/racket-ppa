(module universe mzscheme
  (provide (all-from 2htdp/universe)
           mouse-event pad-event)
  (require 2htdp/universe)

  (require deinprogramm/signature/signature)
  (require deinprogramm/signature/signature-syntax)
  (require deinprogramm/signature/signature-german)
  
  (define mouse-event
    (signature mouse-event
      (enum "button-down" "button-up" "drag" "move" "enter" "leave")))
  (define pad-event
    (signature pad-event
      (enum "left" "right" "up" "down" "w" "s" "a" "d" " " "shift" "rshift"))))
