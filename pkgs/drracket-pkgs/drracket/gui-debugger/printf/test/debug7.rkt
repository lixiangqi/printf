#lang medic


(layer layer1
       (in #:file "src7.rkt"
           [on-exit (timeline r)
                    (timeline y)]
           [(fact) 
            [(at (with-start |(* x (fact (sub1 x|))
             [on-entry (timeline t)]]
            [on-entry (timeline x)
                      (assert (> x 0))]]))