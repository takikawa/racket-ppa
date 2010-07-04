(module web-server mzscheme
  (require (lib "tcp-sig.ss" "net")
           (lib "unitsig.ss")
           (lib "contract.ss")
           "sig.ss"
           "web-server-unit.ss"
	   "configuration.ss"
           "private/configuration-structures.ss")
  (provide/contract
   [serve (case-> [configuration? . -> . (-> void?)]
                  [configuration? natural-number/c . -> . (-> void?)]
                  [configuration? natural-number/c string? . -> . (-> void?)])])
  
  ; : configuration [nat] [(U str #f)] -> -> void
  (define serve
    (case-lambda
      [(config)
       (run-the-server config)]
      [(config port)
       (run-the-server (update-configuration config `((port . ,port))))]
      [(config port listen-ip)
       (run-the-server (update-configuration config `((port . ,port) (ip-address . ,listen-ip))))]))
  
  ; : configuration -> -> void
  (define (run-the-server config)
    (invoke-unit/sig
     (compound-unit/sig
       (import (t : net:tcp^))
       (link
        [c : web-config^ (config)]
        [s : web-server^ (web-server@ t c)]
        [m : () ((unit/sig ()
                   (import web-server^)
                   (serve))
                 s)])
       (export))
     net:tcp^)))