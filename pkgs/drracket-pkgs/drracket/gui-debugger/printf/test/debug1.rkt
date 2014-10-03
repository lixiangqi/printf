#lang medic

(layer layer1
       (in #:file "factorial.rkt"
           [(fact) [on-entry (printf "x=~v\n" x)]]))
