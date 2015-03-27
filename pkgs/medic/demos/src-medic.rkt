#lang medic

(layer layer1
       (in #:module "src.rkt"
           [(fact) [on-entry (timeline x)]]
           [on-exit
;            (define y 2)
;            (sleep 2)
;            (timeline y)
;            ;(fact 100000 1)
;            (sleep 4)
           ; (timeline y)
            (fact 5 1)
            ]))