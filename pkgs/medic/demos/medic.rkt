#lang medic

(layer layer1 
       (in #:module "src.rkt"
           [(test%)
            [(get-a get-b) [on-entry (printf "get-a: a=~v\n" a)]]]
           [(get-a) [on-entry (printf "get-a\n")]]
           [(get-b) [on-entry (printf "get-b\n")]]
           #;[on-entry  (log 'entry)]))

