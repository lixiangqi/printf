#lang medic

(layer layer1
       (in #:file "factorial.rkt"
           [(fact) [on-entry 
                    (printf "[debug]layer1 entered.\n")
                    (printf "[debug]layer1: x=~v\n" x)]
                   [on-exit
                    (printf "[debug]layer1 entered.\n")
                    (printf "[debug]layer1: x=~v\n" x)]]))

(layer layer2
       (in #:file "factorial.rkt"
           [(fg) [(at (+ x 1))
                  [on-entry
                   (printf "[debug]layer2 entered.\n")
                  (printf "[debug]layer2: x=~v\n" x)]]]))

