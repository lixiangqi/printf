#lang medic

(layer layer1
       (in #:file "fact.rkt"
           [(fact) [on-entry (assert (> x 0))]]))