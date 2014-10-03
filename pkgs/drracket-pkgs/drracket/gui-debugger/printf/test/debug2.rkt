#lang medic

(layer layer2
       (in #:file "factorial.rkt"
           [on-exit (printf "end: x=~v\n" x)]
           [on-entry (define y 1)]
           [(at (define xxx 1) #:after (define e 1)) [on-entry (define t 1)]]
           [(fact) 
            [on-entry (define z 3)]
            [(at (define x 1) #:before (define e 1)) [on-entry (define t 1)]]]))