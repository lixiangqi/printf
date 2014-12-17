#lang medic

(layer layer1
       (in #:file "src.rkt"
           [on-exit (log 'aha)]))