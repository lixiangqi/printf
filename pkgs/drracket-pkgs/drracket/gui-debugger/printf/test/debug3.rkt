#lang medic

(layer layer1 
       (in #:file "src3.rkt"
           [(at (define y 3)) [on-entry (printf "x = ~v\n" x)]]
           [(at (define y 3) #:after (define z 5)) [on-exit (define t 8)
                                                            (printf "t=~v\n" t)]]
           [(at (define y 3) #:before (with-start |(define (g x)|)) [on-entry (define m 11)
                                                                              (printf "m=~v\n" m)]]
           [(g) [(at (define y 3)) [on-exit (define n 22)
                                            (printf "n=~v\n" n)]]]))

             
             