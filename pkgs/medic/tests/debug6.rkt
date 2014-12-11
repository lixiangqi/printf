#lang medic

(layer layer1
       (in #:file "src6.rkt"
           [(f) [on-entry 
                 (+ x 10)
                 #;(printf "in f: x =~v" x)]
                [on-exit (printf "exited.\n")]]))

