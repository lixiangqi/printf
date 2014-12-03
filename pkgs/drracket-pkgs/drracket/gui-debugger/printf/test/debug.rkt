#lang medic

(layer layer1
       (in #:file "src.rkt"
           [(fact) [on-entry
                    (log (cons 1 x))
                    (log "string")
                    (aggregate x (cons 1 x))
                    (timeline (cons 1 x))
                    (assert (> x 0))
                    ]]
           [on-exit 
            (assert (> x 0))
            (log x)]))