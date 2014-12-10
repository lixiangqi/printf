#lang medic

(layer layer1
       (in #:file "src6.rkt"
           [(f) [on-entry (printf "in f: x ")]
                [on-exit (printf "exited.\n")]]))

