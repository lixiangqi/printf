#lang medic

(layer layer1 
       (in #:file "src4.rkt"
           [(at (define y 3)) [on-exit
                               (define z 5)
                               (printf "debug: x = ~v\n" x)
                               (printf "debug: y = ~v \n" y)
                               (printf "debug: z = ~v \n" z)]]
           [(g) [(at (+ x 1)) [on-exit (define n 22)
                                            (printf "n=~v\n" n)]]]))
