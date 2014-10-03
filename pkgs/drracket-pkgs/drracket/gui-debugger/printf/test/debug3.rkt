#lang medic

(layer layer1 
       (in #:file "src3.rkt"
           [(at (define y 3)) [on-entry (printf "x = ~v\n" x)]]
           [(g) [(at (define y 3) #:after (define z 5)
                                     (+ x 1)) [on-exit (define t 8)
                                                            (printf "t=~v\n" t)]]]
           [(at (define y 3) #:after (define z 5)
                                     (+ x 1)) [on-exit (define t 8)
                                                            (printf "t=~v\n" t)]]
           [(at (define y 3) #:before (with-start |(define (g x)|)) [on-entry (define m 11)
                                                                              (printf "m=~v\n" m)]]
           [(g) [(at (define y 3)) [on-exit (define n 22)
                                            (printf "n=~v\n" n)]]]))

             
; test different (g, f) while f is not true
; test different before and after expression, which some are not true

;; we have to use annotators to decide whether the at patten is in the function closure