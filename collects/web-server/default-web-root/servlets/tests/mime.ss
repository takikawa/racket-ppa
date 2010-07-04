(module mime mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    `("text/uber-format"
      "uber uber uber"
      "-de-doo")))