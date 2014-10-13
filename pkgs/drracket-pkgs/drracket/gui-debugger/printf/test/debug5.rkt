#lang medic

(layer layer1 
       (in #:file "src5.rkt"
           [(at (define y 3)) [on-exit
                               (define z 5)
                               (define counter 0)
                               (printf "[debug]x = ~v\n" x)
                               (printf "[debug]y = ~v \n" y)
                               (printf "[debug]z = ~v \n" z)]]
           [(at (set! x (add1 x))) [on-exit
                                    (printf "[debug]before call (incr y): y = ~v\n" y)
                                    (incr y)
                                    (printf "[debug]after set x: x = ~v\n" x)
                                    (printf "[debug]after call (incr y): y = ~v\n" y)
                                    (printf "[debug]after call (incr y): counter = ~v\n" counter)]]
           [(at (+ 3 x)) [on-exit (define (incr x) 
                                    (printf "[debug]in incr before: arg = ~v\n" x)
                                    (set! x (add1 x))
                                    (set! counter (add1 counter))
                                    (printf "[debug]in incr after: arg = ~v\n" x))
                                  (incr 1)
                                  ]]
           [(g) [(at (+ x 1)) [on-exit (define n 22)
                                       (printf "[debug]in g (refer to debugging definition): z = ~v \n" z)
                                       (printf "[debug]in g (refer to debugging definition): n=~v\n" n)]]]))