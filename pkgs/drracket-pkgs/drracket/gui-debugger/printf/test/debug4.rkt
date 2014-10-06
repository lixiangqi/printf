#lang medic

(layer layer1 
       (in #:file "src4.rkt"
           [(at (define y 3)) [on-entry (printf "x = ~v\n" x)
                                        (printf "y ~v \n" y)]]
           [(g) [(at (+ x 1)) [on-exit (define n 22)
                                            (printf "n=~v\n" n)]]]))
