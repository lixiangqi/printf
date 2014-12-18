#lang medic

(layer layer1
       (in #:file "fact-iter.rkt"
           [(fact) [on-entry (aggregate x a)]]))