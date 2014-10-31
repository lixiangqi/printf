#lang medic

(layer layer1
       (in #:file "src6.rkt"
           (: fact "function fact takes @x and returns @,ret")
           [on-exit '(log (#%plain-app fact (quote 5)))]
           [(fact) [on-entry 
                    (printf "[debug]layer1 entered.\n")]]))