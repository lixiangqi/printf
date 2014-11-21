#lang medic

(layer layer1
       (in #:file "src6.rkt"
           (with-behavior fact "function fact takes @x and returns @,ret")
           [(fact) [on-entry 
                    (log x)
                    (printf "[debug]layer1 entered.\n")]]
           [on-exit (log (fact 3))]))