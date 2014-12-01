#lang medic

(layer layer1
       (in #:file "src.rkt"
           [(fact) [on-entry
                    (log x)]]
           [on-exit 
            (assert (> x 0))
            (log x)]))